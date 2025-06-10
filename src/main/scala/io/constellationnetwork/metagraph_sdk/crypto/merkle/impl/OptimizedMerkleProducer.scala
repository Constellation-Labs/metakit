package io.constellationnetwork.metagraph_sdk.crypto.merkle.impl

import cats.effect.{Ref, Sync}
import cats.syntax.all._

import scala.annotation.tailrec

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.{
  InvalidIndex,
  MerkleProducer,
  MerkleProducerError,
  TreeBuildError
}
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

class OptimizedMerkleProducer[F[_]: Sync: JsonBinaryHasher](
  stateRef: Ref[F, OptimizedMerkleProducer.ProducerState]
) extends MerkleProducer[F] {

  def leaves: F[List[MerkleNode.Leaf]] =
    stateRef.get.map(_.leaves.toList)

  def build: F[Either[TreeBuildError, MerkleTree]] =
    stateRef.get.flatMap { state =>
      if (state.leaves.isEmpty) {
        TreeBuildError("Cannot build tree with no leaves").asLeft[MerkleTree].pure[F]
      } else {
        state.currentRoot match {
          case Some(root) if state.dirtyNodes.isEmpty => root.asRight[TreeBuildError].pure[F]
          case _ =>
            rebuildTree(state).flatMap { tree =>
              stateRef
                .update(
                  _.copy(
                    currentRoot = Some(tree),
                    dirtyNodes = Set.empty
                  )
                )
                .as(tree.asRight[TreeBuildError])
            }
        }
      }
    }

  private def rebuildTree(state: OptimizedMerkleProducer.ProducerState): F[MerkleTree] = {
    def getOrBuildNode(left: MerkleNode, rightOpt: Option[MerkleNode]): F[MerkleNode] =
      state.nodeCache.get(left.digest) match {
        case Some(cached) if !state.dirtyNodes.contains(left.digest) => cached.pure[F]
        case _ =>
          MerkleNode
            .Internal(left, rightOpt)
            .flatTap { node =>
              stateRef.update { s =>
                s.copy(nodeCache = s.nodeCache + (left.digest -> node))
              }
            }
            .widen
      }

    def buildLevel(nodes: List[MerkleNode]): F[MerkleNode] =
      Sync[F].tailRecM(nodes) { currentLevel =>
        if (currentLevel.length <= 1) currentLevel.head.asRight[List[MerkleNode]].pure[F]
        else {
          currentLevel
            .grouped(2)
            .toList
            .traverse[F, MerkleNode] {
              case left :: right :: Nil => getOrBuildNode(left, Some(right))
              case left :: Nil          => getOrBuildNode(left, None)
              case _                    => new RuntimeException("Unexpected grouping").raiseError
            }
            .map(_.asLeft[MerkleNode])
        }
      }

    buildLevel(state.leaves.toList).map { rootNode =>
      MerkleTree(
        rootNode,
        state.leaves.zipWithIndex.map { case (leaf, idx) =>
          (leaf.digest, idx)
        }.toMap
      )
    }
  }

  def update(index: Int, leaf: MerkleNode.Leaf): F[Either[MerkleProducerError, Unit]] =
    stateRef.get.flatMap { state =>
      if (index < 0 || index >= state.leaves.size) {
        InvalidIndex(index, state.leaves.size).asLeft[Unit].pure[F].widen
      } else {
        val dirtyPath = getPathToRoot(state, index)
        stateRef
          .update { s =>
            s.copy(
              leaves = s.leaves.updated(index, leaf),
              dirtyNodes = s.dirtyNodes ++ dirtyPath,
              currentRoot = None
            )
          }
          .map(_.asRight[MerkleProducerError])
      }
    }

  def append(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
    stateRef.update { state =>
      val startIdx = state.leaves.size
      val dirtyPath = (startIdx until startIdx + newLeaves.size).flatMap(getPathToRoot(state, _))
      state.copy(
        leaves = state.leaves ++ newLeaves,
        dirtyNodes = state.dirtyNodes ++ dirtyPath,
        currentRoot = None
      )
    }

  def prepend(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
    stateRef.update { state =>
      state.copy(
        leaves = Vector.from(newLeaves) ++ state.leaves,
        nodeCache = Map.empty,
        dirtyNodes = Set.empty,
        currentRoot = None
      )
    }

  def remove(index: Int): F[Either[MerkleProducerError, Unit]] =
    stateRef.get.flatMap { state =>
      if (index < 0 || index >= state.leaves.size) {
        InvalidIndex(index, state.leaves.size).asLeft[Unit].pure[F].widen
      } else {
        val dirtyPath = getPathToRoot(state, index)
        stateRef
          .update { s =>
            s.copy(
              leaves = s.leaves.patch(index, Vector(), 1),
              dirtyNodes = s.dirtyNodes ++ dirtyPath,
              currentRoot = None
            )
          }
          .map(_.asRight[MerkleProducerError])
      }
    }

  private def getPathToRoot(state: OptimizedMerkleProducer.ProducerState, index: Int): Set[Hash] = {
    @tailrec
    def loop(idx: Int, acc: Set[Hash]): Set[Hash] =
      if (idx == 0) acc
      else {
        val parentIdx = (idx - 1) / 2
        state.nodeCache.get(state.leaves(parentIdx).digest) match {
          case Some(parent) => loop(parentIdx, acc + parent.digest)
          case None         => acc
        }
      }

    loop(index, Set.empty)
  }
}

object OptimizedMerkleProducer {

  case class ProducerState(
    leaves:      Vector[MerkleNode.Leaf],
    nodeCache:   Map[Hash, MerkleNode],
    dirtyNodes:  Set[Hash],
    currentRoot: Option[MerkleTree]
  )
}
