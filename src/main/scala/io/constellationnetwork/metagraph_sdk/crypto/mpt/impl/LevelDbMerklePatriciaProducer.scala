package io.constellationnetwork.metagraph_sdk.crypto.mpt.impl

import java.nio.file.Path

import cats.effect.{Ref, Resource, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.metagraph_sdk.storage.Collection
import io.constellationnetwork.metagraph_sdk.storage.impl.LevelDbCollection
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax._
import io.circe.{Encoder, Json}

class LevelDbMerklePatriciaProducer[F[_]: Sync: JsonBinaryHasher](
  val entriesStore: Collection[F, Hash, Json],
  metadataStore: Collection[F, String, Json],
  stateRef: Ref[F, LevelDbMerklePatriciaProducer.ProducerState]
) extends StatefulMerklePatriciaProducer[F] {


  /**
   * Get a prover using the current built trie if available.
   * Falls back to rebuilding from storage if no trie is cached.
   */
  def getProver: F[MerklePatriciaProver[F]] =
    stateRef.get.flatMap { state =>
      state.currentTrie match {
        case Some(trie) =>
          // Use cached trie for prover
          MerklePatriciaProver.make[F](trie).pure[F]
        case None =>
          // Build trie, cache it, then create prover
          for {
            allEntries <- entries
            trie <- MerklePatriciaTrie.make[F, Json](allEntries)
            _ <- stateRef.update(_.copy(currentTrie = Some(trie)))
          } yield MerklePatriciaProver.make[F](trie)
      }
    }

  def entries: F[Map[Hash, Json]] =
    entriesStore.dump.map(_.toMap)

  def build: F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    stateRef.get.flatMap { state =>
      if (state.entryCount == 0) {
        OperationError("Cannot build trie with no entries").asLeft[MerklePatriciaTrie].pure[F].widen
      } else {
        state.currentTrie match {
          case Some(trie) if state.dirtyKeys.isEmpty =>
            // Return cached trie if no changes
            trie.asRight[MerklePatriciaError].pure[F]
          case _ =>
            // Rebuild trie and update cache
            entries.flatMap { allEntries =>
              MerklePatriciaTrie.make[F, Json](allEntries).attempt.flatMap {
                case Right(trie) =>
                  // Atomically update metadata and state
                  for {
                    _ <- metadataStore.put("root", trie.rootNode.digest.asJson)
                    _ <- metadataStore.put("version", state.version.asJson)
                    _ <- stateRef.update(_.copy(
                      currentTrie = Some(trie),
                      dirtyKeys = Set.empty,
                      version = state.version + 1
                    ))
                  } yield trie.asRight[MerklePatriciaError]
                case Left(e) =>
                  OperationError(e.getMessage).asLeft[MerklePatriciaTrie].pure[F].widen
              }
            }
        }
      }
    }

  def insert[A: Encoder](data: Map[Hash, A]): F[Either[MerklePatriciaError, Unit]] =
    if (data.isEmpty) {
      ().asRight[MerklePatriciaError].pure[F]
    } else {
      val entries = data.map { case (k, v) => (k, v.asJson) }.toList

      // Atomic batch operation
      (for {
        _ <- entriesStore.putBatch(entries)
        _ <- stateRef.update { s =>
          val newKeys = data.keySet
          val actualNewKeys = newKeys.diff(s.dirtyKeys)
          s.copy(
            entryCount = s.entryCount + actualNewKeys.size,
            dirtyKeys = s.dirtyKeys ++ newKeys,
            currentTrie = None // Invalidate cache
          )
        }
      } yield ().asRight[MerklePatriciaError]).handleError { e =>
        OperationError(s"Insert failed: ${e.getMessage}").asLeft[Unit]
      }
    }

  def update[A: Encoder](key: Hash, value: A): F[Either[MerklePatriciaError, Unit]] =
    for {
      exists <- entriesStore.contains(key)
      result <- if (!exists) {
        OperationError(s"Key not found for update: $key").asLeft[Unit].pure[F]
      } else {
        (for {
          _ <- entriesStore.put(key, value.asJson)
          _ <- stateRef.update { s =>
            s.copy(
              dirtyKeys = s.dirtyKeys + key,
              currentTrie = None
            )
          }
        } yield ().asRight[MerklePatriciaError]).handleError { e =>
          OperationError(s"Update failed: ${e.getMessage}").asLeft[Unit]
        }
      }
    } yield result

  def remove(keys: List[Hash]): F[Either[MerklePatriciaError, Unit]] =
    if (keys.isEmpty) {
      ().asRight[MerklePatriciaError].pure[F]
    } else {
      (for {
        // Check which keys exist
        existing <- keys.traverseFilter { key =>
          entriesStore.contains(key).map(exists => if (exists) Some(key) else None)
        }

        _ <- if (existing.isEmpty) {
          ().pure[F]
        } else {
          // Atomic batch removal
          entriesStore.removeBatch(existing) >>
          stateRef.update { s =>
            s.copy(
              entryCount = s.entryCount - existing.size,
              dirtyKeys = s.dirtyKeys ++ existing.toSet,
              currentTrie = None
            )
          }
        }
      } yield ().asRight[MerklePatriciaError]).handleError { e =>
        OperationError(s"Remove failed: ${e.getMessage}").asLeft[Unit]
      }
    }


  def clear: F[Unit] =
    for {
      allKeys <- entriesStore.dump.map(_.map(_._1))
      _ <- entriesStore.removeBatch(allKeys)
      _ <- metadataStore.remove("root")
      _ <- metadataStore.remove("version")
      _ <- stateRef.update(_.copy(
        entryCount = 0,
        currentTrie = None,
        dirtyKeys = Set.empty,
        version = 0L
      ))
    } yield ()

  def create[A: Encoder](data: Map[Hash, A]): F[MerklePatriciaTrie] =
    for {
      _ <- clear
      _ <- insert(data)
      result <- build
      trie <- result.liftTo[F]
    } yield trie

  def insert[A: Encoder](
    current: MerklePatriciaTrie,
    data:    Map[Hash, A]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    for {
      _ <- insert(data)
      result <- build
    } yield result

  def remove(
    current: MerklePatriciaTrie,
    keys:    List[Hash]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    for {
      _ <- remove(keys)
      result <- build
    } yield result

  def getProver(trie: MerklePatriciaTrie): F[MerklePatriciaProver[F]] =
    MerklePatriciaProver.make[F](trie).pure[F]
}

object LevelDbMerklePatriciaProducer {

  case class ProducerState(
    entryCount: Int,
    currentTrie: Option[MerklePatriciaTrie],
    dirtyKeys: Set[Hash],
    version: Long
  )

  def make[F[_]: Sync: JsonBinaryHasher](
    dbPath: Path,
    initial: Map[Hash, Json] = Map.empty
  ): Resource[F, LevelDbMerklePatriciaProducer[F]] = for {
    entriesStore <- LevelDbCollection.make[F, Hash, Json](dbPath.resolve("entries"))
    metadataStore <- LevelDbCollection.make[F, String, Json](dbPath.resolve("metadata"))

    producer <- Resource.eval {
      for {
        // Check if database exists
        existingEntries <- entriesStore.dump
        existingCount = existingEntries.size

        // Initialize atomically if empty
        _ <- (existingCount == 0 && initial.nonEmpty).pure[F].ifM(
          ifTrue = entriesStore.putBatch(initial.toList),
          ifFalse = ().pure[F]
        )

        stateRef <- Ref.of[F, ProducerState](
          ProducerState(
            entryCount = if (existingCount > 0) existingCount else initial.size,
            currentTrie = None,
            dirtyKeys = Set.empty,
            version = 0L
          )
        )
      } yield new LevelDbMerklePatriciaProducer[F](entriesStore, metadataStore, stateRef)
    }
  } yield producer

  /**
   * Load an existing LevelDB database without initializing
   * Fails if the database doesn't exist or is empty
   */
  def load[F[_]: Sync: JsonBinaryHasher](
    dbPath: Path
  ): Resource[F, LevelDbMerklePatriciaProducer[F]] = for {
    entriesStore <- LevelDbCollection.make[F, Hash, Json](dbPath.resolve("entries"))
    metadataStore <- LevelDbCollection.make[F, String, Json](dbPath.resolve("metadata"))

    producer <- Resource.eval {
      for {
        existingCount <- entriesStore.dump.map(_.size)
        _ <- (existingCount == 0).pure[F].ifM(
          ifTrue = Sync[F].raiseError[Unit](new IllegalStateException(s"No existing data found at $dbPath")),
          ifFalse = ().pure[F]
        )

        stateRef <- Ref.of[F, ProducerState](
          ProducerState(
            entryCount = existingCount,
            currentTrie = None,
            dirtyKeys = Set.empty,
            version = 0L
          )
        )
      } yield new LevelDbMerklePatriciaProducer[F](entriesStore, metadataStore, stateRef)
    }
  } yield producer
}