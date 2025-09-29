package io.constellationnetwork.metagraph_sdk.crypto.merkle.impl

import cats.effect.{Ref, Sync}
import cats.syntax.all._

import scala.collection.mutable

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api._
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

/**
 * Memory-efficient Merkle tree producer with proper index management and caching strategies
 */
class InMemoryMerkleProducer[F[_]: Sync: JsonBinaryHasher](
  stateRef: Ref[F, InMemoryMerkleProducer.ProducerState],
  config:   InMemoryMerkleProducer.Config
) extends MerkleProducer[F] {

  def leaves: F[List[MerkleNode.Leaf]] =
    stateRef.get.map(_.leaves.toList)

  def build: F[Either[TreeBuildError, MerkleTree]] =
    stateRef.get.flatMap { state =>
      if (state.leaves.isEmpty) {
        TreeBuildError("Cannot build tree with no leaves").asLeft[MerkleTree].pure[F]
      } else {
        state.cachedTree match {
          case Some((version, tree)) if version == state.version =>
            tree.asRight[TreeBuildError].pure[F]

          case _ =>
            MerkleTree.fromLeaves(state.leaves.toList).flatMap { tree =>
              val newPathCahce =
                if (config.enablePathCache) buildPathCache(tree, config.maxCachedPaths)
                else state.pathCache

              val newState =
                if (state.leaves.size >= config.maxTreeSizeToCache) state.copy(cachedTree = None)
                else {
                  state.copy(
                    cachedTree = Some((state.version, tree)),
                    pathCache = newPathCahce
                  )
                }

              stateRef.update(_ => newState).as(tree.asRight[TreeBuildError])
            }
        }
      }
    }

  def update(index: Int, leaf: MerkleNode.Leaf): F[Either[MerkleProducerError, Unit]] =
    stateRef.get.flatMap { state =>
      if (index < 0 || index >= state.leaves.size) {
        InvalidIndex(index, state.leaves.size).asLeft[Unit].pure[F].widen
      } else {
        stateRef
          .update { s =>
            s.copy(
              leaves = s.leaves.updated(index, leaf),
              version = s.version + 1,
              cachedTree = None, // Invalidate cache
              dirtyIndices = s.dirtyIndices + index
            )
          }
          .map(_.asRight[MerkleProducerError])
      }
    }

  def append(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
    stateRef.update { state =>
      val startIdx = state.leaves.size
      val newIndices = (startIdx until startIdx + newLeaves.size).toSet

      state.copy(
        leaves = state.leaves ++ newLeaves,
        version = state.version + 1,
        cachedTree = None,
        dirtyIndices = if (config.trackDirtyIndices) {
          state.dirtyIndices ++ newIndices
        } else {
          Set.empty // Clear dirty tracking for large appends
        }
      )
    }

  def prepend(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
    stateRef.update { state =>
      // Prepend invalidates all indices, so clear caches
      state.copy(
        leaves = Vector.from(newLeaves) ++ state.leaves,
        version = state.version + 1,
        cachedTree = None,
        dirtyIndices = Set.empty, // All indices are effectively dirty
        pathCache = mutable.LinkedHashMap.empty
      )
    }

  def remove(index: Int): F[Either[MerkleProducerError, Unit]] =
    stateRef.get.flatMap { state =>
      if (index < 0 || index >= state.leaves.size) {
        InvalidIndex(index, state.leaves.size).asLeft[Unit].pure[F].widen
      } else {
        stateRef
          .update { s =>
            // Remove shifts all subsequent indices
            val affectedIndices = (index until s.leaves.size).toSet

            s.copy(
              leaves = s.leaves.patch(index, Vector(), 1),
              version = s.version + 1,
              cachedTree = None,
              dirtyIndices = if (config.trackDirtyIndices && affectedIndices.size < config.dirtyThreshold) {
                affectedIndices
              } else {
                Set.empty // Too many affected indices, clear tracking
              },
              pathCache = {
                // Remove invalidated paths from cache
                val newCache = mutable.LinkedHashMap.empty[Int, InMemoryMerkleProducer.PathInfo]
                s.pathCache.foreach { case (idx, path) =>
                  if (idx < index) {
                    newCache(idx) = path // Keep paths before removed index
                  } else if (idx > index) {
                    newCache(idx - 1) = path // Shift paths after removed index
                  }
                }
                newCache
              }
            )
          }
          .map(_.asRight[MerkleProducerError])
      }
    }

  def getProver: F[MerkleProver[F]] =
    build.flatMap {
      case Right(tree) => MerkleProver.make[F](tree).pure[F]
      case Left(error) => Sync[F].raiseError(new RuntimeException(s"Failed to build tree: ${error.message}"))
    }

  private def buildPathCache(
    tree:     MerkleTree,
    maxPaths: Int
  ): mutable.LinkedHashMap[Int, InMemoryMerkleProducer.PathInfo] = {
    val cache = mutable.LinkedHashMap.empty[Int, InMemoryMerkleProducer.PathInfo]

    // Cache paths for first N leaves (could be made smarter with access patterns)
    tree.leafDigestIndex.take(maxPaths).foreach { case (digest, index) =>
      // In a real implementation, we'd extract the actual path here
      cache(index) = InMemoryMerkleProducer.PathInfo(
        leafIndex = index,
        leafDigest = digest,
        pathToRoot = List.empty // Would be populated with actual path
      )
    }

    cache
  }
}

object InMemoryMerkleProducer {

  case class Config(
    maxTreeSizeToCache: Int = 10000, // Don't cache trees larger than this
    maxCachedPaths:     Int = 100, // Maximum number of paths to cache
    enablePathCache:    Boolean = true, // Enable path caching
    trackDirtyIndices:  Boolean = true, // Track which indices are dirty
    dirtyThreshold:     Int = 1000 // Stop tracking if too many indices are dirty
  )

  case class PathInfo(
    leafIndex:  Int,
    leafDigest: Hash,
    pathToRoot: List[Hash]
  )

  case class ProducerState(
    leaves:       Vector[MerkleNode.Leaf],
    version:      Long, // Incremented on each modification
    cachedTree:   Option[(Long, MerkleTree)], // (version, tree) tuple
    dirtyIndices: Set[Int], // Indices modified since last build
    pathCache:    mutable.LinkedHashMap[Int, PathInfo] // LRU cache of paths
  )

  def make[F[_]: Sync: JsonBinaryHasher](
    initialLeaves: List[MerkleNode.Leaf] = List.empty,
    config:        Config = Config()
  ): F[InMemoryMerkleProducer[F]] = {
    val initialState = ProducerState(
      leaves = Vector.from(initialLeaves),
      version = 0L,
      cachedTree = None,
      dirtyIndices = Set.empty,
      pathCache = mutable.LinkedHashMap.empty
    )

    Ref.of[F, ProducerState](initialState).map(new InMemoryMerkleProducer(_, config))
  }
}
