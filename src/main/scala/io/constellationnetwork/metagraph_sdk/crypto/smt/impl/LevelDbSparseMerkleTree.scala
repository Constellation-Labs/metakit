package io.constellationnetwork.metagraph_sdk.crypto.smt.impl

import java.nio.file.Path
import java.util.Base64

import cats.effect.{Async, Ref, Resource, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.smt._
import io.constellationnetwork.metagraph_sdk.crypto.smt.api.SparseMerkleProver
import io.constellationnetwork.metagraph_sdk.crypto.smt.node.{SparseMerkleNode, SparseMerkleNodeOps}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.metagraph_sdk.storage.Collection
import io.constellationnetwork.metagraph_sdk.storage.impl.LevelDbCollection
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax._
import io.circe.{Decoder, Json}

/**
 * A LevelDB-backed, STATEFUL persistent sparse Merkle tree -- the persistent sibling of
 * [[InMemorySparseMerkleTree]], mirroring `LevelDbMerklePatriciaProducer`'s store-plus-`Ref` discipline.
 *
 * Deliberately NOT a [[SparseMerkleTree]]: that trait's mutators must return a NEW tree (structural sharing, receiver
 * untouched), which is incompatible with a mutable persistent store. Instead this exposes a stateful API whose mutators
 * advance THIS store in place (returning the resulting [[SparseMerkleRoot]]), exactly as the MPT LevelDB producer advances its
 * own state.
 *
 * Two backing stores are kept in lock-step on every mutation:
 *   - `entriesStore: Collection[F, Hex, Json]` -- the authoritative persisted `key -> value` map. SMT values are
 *     `Array[Byte]`, which has no circe `Encoder`; each is stored as a BASE64 Json STRING and decoded back on load. The
 *     round-trip is byte-EXACT (`Base64` is a lossless bijection on byte arrays), so the rebuilt `valueDigest`
 *     (`Hash.fromBytes(value)`) and therefore the [[root]] reproduce after a restart.
 *   - `rootRef: Ref[F, SparseMerkleNode]` -- the live in-memory tree used to answer [[get]] / [[root]] / [[prover]], rebuilt from
 *     the persisted entries at construction (the SMT is order-independent, so any iteration order of the persisted map
 *     yields the same node and the same root).
 *
 * A `metadataStore` records the latest root digest and a monotonically increasing version on each mutation, matching the
 * MPT producer's metadata bookkeeping.
 */
final class LevelDbSparseMerkleTree[F[_]: Sync: JsonBinaryHasher] private (
  val entriesStore: Collection[F, Hex, Json],
  metadataStore: Collection[F, String, Json],
  rootRef: Ref[F, SparseMerkleNode],
  versionRef: Ref[F, Long]
) {

  /** The value bytes bound to `key`, or `None` if `key` is absent. `key` is hashed to its position internally. */
  def get(key: Hex): F[Option[Array[Byte]]] =
    for {
      pos  <- SparseMerkleHashing.position[F](key)
      node <- rootRef.get
    } yield SparseMerkleNodeOps.get(node, pos, 0)

  /** The current root commitment (digest of the live root node; [[SparseMerkleRoot.empty]] for the empty tree). */
  def root: F[SparseMerkleRoot] =
    rootRef.get.map(node => SparseMerkleRoot(node.digest))

  /** Every persisted `key -> value` binding (values decoded byte-exactly from their base64 Json form). */
  def entries: F[Map[Hex, Array[Byte]]] =
    entriesStore.dump.flatMap { pairs =>
      pairs.traverse {
        case (k, json) => LevelDbSparseMerkleTree.decodeValue[F](json).map(k -> _)
      }
    }.map(_.toMap)

  /**
   * Upsert `key -> value`: persist the value and advance the live root. Returns the new [[SparseMerkleRoot]]. The entriesStore and
   * the `Ref[SparseMerkleNode]` are updated together (and the metadata root/version bumped) so the persisted map and the live
   * tree never diverge.
   */
  def insert(key: Hex, value: Array[Byte]): F[SparseMerkleRoot] =
    for {
      _       <- entriesStore.put(key, LevelDbSparseMerkleTree.encodeValue(value))
      node    <- rootRef.get
      pos     <- SparseMerkleHashing.position[F](key)
      vd      <- SparseMerkleHashing.valueDigest[F](value)
      updated <- SparseMerkleNodeOps.insert[F](node, key, pos, vd, value, 0)
      _       <- rootRef.set(updated)
      result  <- commit(updated)
    } yield result

  /** Remove `key` (no-op if absent): drop it from the store and re-collapse the live root. Returns the new [[SparseMerkleRoot]]. */
  def remove(key: Hex): F[SparseMerkleRoot] =
    for {
      _       <- entriesStore.remove(key)
      node    <- rootRef.get
      pos     <- SparseMerkleHashing.position[F](key)
      updated <- SparseMerkleNodeOps.remove[F](node, pos, 0)
      _       <- rootRef.set(updated)
      result  <- commit(updated)
    } yield result

  /**
   * Apply `removes` then `upserts` (removals first, upsert-wins) atomically against both backing stores, advancing the
   * live root once. Independent of the order entries appear in the inputs.
   */
  def withChanges(upserts: Map[Hex, Array[Byte]], removes: Set[Hex]): F[SparseMerkleRoot] =
    for {
      _    <- entriesStore.removeBatch(removes.toList)
      _    <- entriesStore.putBatch(upserts.toList.map { case (k, v) => k -> LevelDbSparseMerkleTree.encodeValue(v) })
      node <- rootRef.get
      afterRemoves <- removes.toList.foldLeftM(node) { (acc, key) =>
        SparseMerkleHashing.position[F](key).flatMap(pos => SparseMerkleNodeOps.remove[F](acc, pos, 0))
      }
      afterUpserts <- upserts.toList.foldLeftM(afterRemoves) {
        case (acc, (key, value)) =>
          for {
            pos  <- SparseMerkleHashing.position[F](key)
            vd   <- SparseMerkleHashing.valueDigest[F](value)
            next <- SparseMerkleNodeOps.insert[F](acc, key, pos, vd, value, 0)
          } yield next
      }
      _      <- rootRef.set(afterUpserts)
      result <- commit(afterUpserts)
    } yield result

  /** An [[SparseMerkleProver]] bound to THIS tree's current root node (snapshot at call time). */
  def prover: F[SparseMerkleProver[F]] =
    rootRef.get.map { node =>
      new SparseMerkleProver[F] {
        def prove(key: Hex): F[Either[SparseMerkleProofError, SparseMerkleProof]] =
          SparseMerkleHashing.position[F](key).flatMap(pos => SparseMerkleNodeOps.prove[F](node, key, pos))
      }
    }

  /** Record the new root digest and bump the version in the metadata store; return the new [[SparseMerkleRoot]]. */
  private def commit(node: SparseMerkleNode): F[SparseMerkleRoot] =
    for {
      version <- versionRef.updateAndGet(_ + 1)
      _       <- metadataStore.put("root", node.digest.asJson)
      _       <- metadataStore.put("version", version.asJson)
    } yield SparseMerkleRoot(node.digest)
}

object LevelDbSparseMerkleTree {

  /** Byte-exact value encoding: `Array[Byte]` -> base64 Json string. */
  private def encodeValue(value: Array[Byte]): Json =
    Base64.getEncoder.encodeToString(value).asJson

  /** Inverse of [[encodeValue]]: base64 Json string -> the exact original bytes. */
  private def decodeValue[F[_]: Sync](json: Json): F[Array[Byte]] =
    Sync[F].fromEither(
      json.as[String](Decoder[String]).map(Base64.getDecoder.decode)
    )

  /** Rebuild the live `SparseMerkleNode` from a persisted `key -> base64-json-value` map (order-independent). */
  private def rebuildNode[F[_]: Sync: JsonBinaryHasher](persisted: List[(Hex, Json)]): F[SparseMerkleNode] =
    persisted.foldLeftM(SparseMerkleNode.Empty: SparseMerkleNode) {
      case (acc, (key, json)) =>
        for {
          value <- decodeValue[F](json)
          pos   <- SparseMerkleHashing.position[F](key)
          vd    <- SparseMerkleHashing.valueDigest[F](value)
          next  <- SparseMerkleNodeOps.insert[F](acc, key, pos, vd, value, 0)
        } yield next
    }

  /**
   * Open (creating if missing) a LevelDB-backed SMT at `dbPath`. If the store is empty and `initial` is non-empty, it is
   * persisted; existing persisted entries always take precedence (mirroring `LevelDbMerklePatriciaProducer.make`). The
   * live `SparseMerkleNode` is rebuilt from whatever entries end up persisted and seeded into the `Ref`.
   */
  def make[F[_]: Async: JsonBinaryHasher](
    dbPath: Path,
    initial: Map[Hex, Array[Byte]] = Map.empty
  ): Resource[F, LevelDbSparseMerkleTree[F]] = for {
    entriesStore  <- LevelDbCollection.make[F, Hex, Json](dbPath.resolve("entries"))
    metadataStore <- LevelDbCollection.make[F, String, Json](dbPath.resolve("metadata"))

    tree <- Resource.eval {
      for {
        existing <- entriesStore.dump
        _ <- (existing.isEmpty && initial.nonEmpty)
          .pure[F]
          .ifM(
            ifTrue = entriesStore.putBatch(initial.toList.map { case (k, v) => k -> encodeValue(v) }),
            ifFalse = ().pure[F]
          )
        persisted  <- if (existing.isEmpty && initial.nonEmpty) entriesStore.dump else existing.pure[F]
        node       <- rebuildNode[F](persisted)
        rootRef    <- Ref.of[F, SparseMerkleNode](node)
        versionRef <- Ref.of[F, Long](0L)
      } yield new LevelDbSparseMerkleTree[F](entriesStore, metadataStore, rootRef, versionRef)
    }
  } yield tree

  /**
   * Load an existing LevelDB-backed SMT without seeding. Fails if the store is empty or absent (mirroring
   * `LevelDbMerklePatriciaProducer.load`). The live `SparseMerkleNode` is rebuilt from the persisted entries.
   */
  def load[F[_]: Async: JsonBinaryHasher](
    dbPath: Path
  ): Resource[F, LevelDbSparseMerkleTree[F]] = for {
    entriesStore  <- LevelDbCollection.make[F, Hex, Json](dbPath.resolve("entries"))
    metadataStore <- LevelDbCollection.make[F, String, Json](dbPath.resolve("metadata"))

    tree <- Resource.eval {
      for {
        persisted <- entriesStore.dump
        _ <- persisted.isEmpty
          .pure[F]
          .ifM(
            ifTrue = Sync[F].raiseError[Unit](new IllegalStateException(s"No existing data found at $dbPath")),
            ifFalse = ().pure[F]
          )
        node       <- rebuildNode[F](persisted)
        rootRef    <- Ref.of[F, SparseMerkleNode](node)
        versionRef <- Ref.of[F, Long](0L)
      } yield new LevelDbSparseMerkleTree[F](entriesStore, metadataStore, rootRef, versionRef)
    }
  } yield tree
}
