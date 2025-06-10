package io.constellationnetwork.metagraph_sdk.crypto.merkle.impl

import cats.effect.{Ref, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api._
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher

class SimpleMerkleProducer[F[_]: Sync: JsonBinaryHasher](
  stateRef: Ref[F, Vector[MerkleNode.Leaf]]
) extends MerkleProducer[F] {

  override def leaves: F[List[MerkleNode.Leaf]] =
    stateRef.get.map(_.toList)

  override def build: F[Either[TreeBuildError, MerkleTree]] =
    stateRef.get.flatMap { leaves =>
      if (leaves.isEmpty) TreeBuildError("Cannot build tree with no leaves").asLeft[MerkleTree].pure[F]
      else MerkleTree.create(leaves.toList).map(_.asRight[TreeBuildError])
    }

  def update(index: Int, leaf: MerkleNode.Leaf): F[Either[MerkleProducerError, Unit]] =
    stateRef.get.flatMap { leaves =>
      if (index < 0 || index >= leaves.size) InvalidIndex(index, leaves.size).asLeft[Unit].pure[F].widen
      else stateRef.update(_.updated(index, leaf)).map(_.asRight[MerkleProducerError])
    }

  def append(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
    stateRef.update { leaves =>
      leaves.appendedAll(newLeaves)
    }

  def prepend(newLeaves: List[MerkleNode.Leaf]): F[Unit] =
    stateRef.update { leaves =>
      leaves.prependedAll(newLeaves)
    }

  def remove(index: Int): F[Either[MerkleProducerError, Unit]] =
    stateRef.get.flatMap { leaves =>
      if (index < 0 || index >= leaves.size) InvalidIndex(index, leaves.size).asLeft[Unit].pure[F].widen
      else stateRef.update(_.patch(index, Vector(), 1)).map(_.asRight[MerkleProducerError])
    }
}
