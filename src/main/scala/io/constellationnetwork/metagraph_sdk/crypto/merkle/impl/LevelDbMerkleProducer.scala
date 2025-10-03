package io.constellationnetwork.metagraph_sdk.crypto.merkle.impl

import java.nio.file.Path

import cats.effect.{Async, Ref, Resource, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api._
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.metagraph_sdk.storage.Collection
import io.constellationnetwork.metagraph_sdk.storage.impl.LevelDbCollection

class LevelDbMerkleProducer[F[_]: Sync: JsonBinaryHasher](
  val leavesStore: Collection[F, Int, MerkleNode.Leaf],
  stateRef: Ref[F, LevelDbMerkleProducer.ProducerState]
) extends StatefulMerkleProducer[F] {

  /**
   * Get a prover using the current built tree if available.
   * Falls back to rebuilding from storage if no tree is cached.
   */
  def getProver: F[MerkleProver[F]] =
    stateRef.get.flatMap { state =>
      state.currentRoot match {
        case Some(tree) =>
          MerkleProver.make[F](tree).pure[F]
        case None =>
          for {
            leafList <- leaves
            tree     <- MerkleTree.fromLeaves(leafList)
            _        <- stateRef.update(_.copy(currentRoot = Some(tree)))
          } yield MerkleProver.make[F](tree)
      }
    }

  def leaves: F[List[MerkleNode.Leaf]] =
    stateRef.get.flatMap { state =>
      val indices = (0 until state.leafCount).toList
      leavesStore
        .getBatch(indices)
        .map(_.collect {
          case (_, Some(leaf)) =>
            leaf
        })
    }

  def build: F[Either[TreeBuildError, MerkleTree]] =
    stateRef.get.flatMap { state =>
      if (state.leafCount == 0) {
        TreeBuildError("Cannot build tree with no leaves").asLeft[MerkleTree].pure[F]
      } else {
        state.currentRoot match {
          case Some(root) if state.dirtyIndices.isEmpty => root.asRight[TreeBuildError].pure[F]
          case _ =>
            leaves.flatMap { leafList =>
              MerkleTree.fromLeaves(leafList).flatMap { tree =>
                stateRef
                  .update(
                    _.copy(
                      currentRoot = Some(tree),
                      dirtyIndices = Set.empty
                    )
                  )
                  .as(tree.asRight[TreeBuildError])
              }
            }
        }
      }
    }

  def update(index: Int, leaf: MerkleNode.Leaf): F[Either[MerkleProducerError, Unit]] =
    stateRef.get.flatMap { state =>
      if (index < 0 || index >= state.leafCount) {
        InvalidIndex(index, state.leafCount).asLeft[Unit].pure[F].widen
      } else {
        for {
          _ <- leavesStore.put(index, leaf)
          _ <- stateRef.update { s =>
            s.copy(
              dirtyIndices = s.dirtyIndices + index,
              currentRoot = None
            )
          }
        } yield ().asRight[MerkleProducerError]
      }
    }

  def append(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
    stateRef.get.flatMap { state =>
      val startIdx = state.leafCount
      val updates = newLeaves.zipWithIndex.map { case (leaf, i) => (startIdx + i, leaf) }

      for {
        _ <- leavesStore.putBatch(updates)
        _ <- stateRef.update { s =>
          s.copy(
            leafCount = s.leafCount + newLeaves.size,
            dirtyIndices = s.dirtyIndices ++ (startIdx until startIdx + newLeaves.size),
            currentRoot = None
          )
        }
      } yield ()
    }

  def prepend(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
    for {
      existingLeaves <- leaves
      allLeaves = newLeaves ++ existingLeaves
      _ <- clearAllLeaves
      _ <- append(allLeaves)
    } yield ()

  def remove(index: Int): F[Either[MerkleProducerError, Unit]] =
    stateRef.get.flatMap { state =>
      if (index < 0 || index >= state.leafCount) {
        InvalidIndex(index, state.leafCount).asLeft[Unit].pure[F].widen
      } else {
        val indicesToShift = ((index + 1) until state.leafCount).toList
        for {
          // Get all leaves after the removed index in a single batch
          leavesAfter <- leavesStore.getBatch(indicesToShift)
          // Prepare shifted leaves with new indices
          shiftedLeaves = leavesAfter.collect {
            case (oldIdx, Some(leaf)) =>
              (oldIdx - 1, leaf)
          }
          // Remove all old positions (including the target and positions that will shift)
          _ <- leavesStore.removeBatch(index :: indicesToShift)
          // Put all shifted leaves in their new positions
          _ <- leavesStore.putBatch(shiftedLeaves)
          _ <- stateRef.update { s =>
            s.copy(
              leafCount = s.leafCount - 1,
              dirtyIndices = Set.empty,
              currentRoot = None
            )
          }
        } yield ().asRight[MerkleProducerError]
      }
    }

  private def clearAllLeaves: F[Unit] =
    stateRef.get.flatMap { state =>
      val indices = (0 until state.leafCount).toList
      leavesStore.removeBatch(indices) >>
      stateRef.update(_.copy(leafCount = 0, dirtyIndices = Set.empty, currentRoot = None))
    }
}

object LevelDbMerkleProducer {

  case class ProducerState(
    leafCount: Int,
    dirtyIndices: Set[Int],
    currentRoot: Option[MerkleTree]
  )

  def make[F[_]: Async: JsonBinaryHasher](
    dbPath: Path,
    initial: List[MerkleNode.Leaf] = List.empty
  ): Resource[F, LevelDbMerkleProducer[F]] = for {
    leavesStore <- LevelDbCollection.make[F, Int, MerkleNode.Leaf](dbPath.resolve("leaves"))

    producer <- Resource.eval {
      for {
        // Initialize with existing leaves or provided initial leaves
        existingCount <- leavesStore.dump.map(_.size)
        initialCount = if (existingCount > 0) existingCount else initial.size

        _ <- (existingCount == 0)
          .pure[F]
          .ifM(
            ifTrue = leavesStore.putBatch(initial.zipWithIndex.map { case (leaf, i) => (i, leaf) }),
            ifFalse = ().pure[F]
          )

        stateRef <- Ref.of[F, ProducerState](
          ProducerState(
            leafCount = initialCount,
            dirtyIndices = Set.empty,
            currentRoot = None
          )
        )
      } yield new LevelDbMerkleProducer[F](leavesStore, stateRef)
    }
  } yield producer

  /**
   * Load an existing LevelDB database without initializing
   * Fails if the database doesn't exist or is empty
   */
  def load[F[_]: Async: JsonBinaryHasher](
    dbPath: Path
  ): Resource[F, LevelDbMerkleProducer[F]] = for {
    leavesStore <- LevelDbCollection.make[F, Int, MerkleNode.Leaf](dbPath.resolve("leaves"))

    producer <- Resource.eval {
      for {
        existingCount <- leavesStore.dump.map(_.size)
        _ <- (existingCount == 0)
          .pure[F]
          .ifM(
            ifTrue = Sync[F].raiseError[Unit](new IllegalStateException(s"No existing data found at $dbPath")),
            ifFalse = ().pure[F]
          )

        stateRef <- Ref.of[F, ProducerState](
          ProducerState(
            leafCount = existingCount,
            dirtyIndices = Set.empty,
            currentRoot = None
          )
        )
      } yield new LevelDbMerkleProducer[F](leavesStore, stateRef)
    }
  } yield producer
}
