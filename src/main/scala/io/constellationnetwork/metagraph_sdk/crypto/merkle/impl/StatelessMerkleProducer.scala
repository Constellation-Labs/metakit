package io.constellationnetwork.metagraph_sdk.crypto.merkle.impl

import cats.MonadThrow
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api._
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher

class StatelessMerkleProducer[F[_]: MonadThrow: JsonBinaryHasher] extends MerkleProducer[F] {

  def create(leaves: List[MerkleNode.Leaf]): F[MerkleTree] =
    MerkleTree.fromLeaves(leaves)

  def append(
    current: MerkleTree,
    leaves: List[MerkleNode.Leaf]
  ): F[MerkleTree] =
    MerkleTree.fromLeaves(current.leaves ++ leaves)

  def prepend(
    current: MerkleTree,
    leaves: List[MerkleNode.Leaf]
  ): F[MerkleTree] =
    MerkleTree.fromLeaves(leaves ++ current.leaves)

  def update(
    current: MerkleTree,
    index: Int,
    leaf: MerkleNode.Leaf
  ): F[Either[MerkleProducerError, MerkleTree]] = {
    val currentLeaves: List[MerkleNode.Leaf] = current.leaves
    if (index < 0 || index >= currentLeaves.size) {
      InvalidIndex(index, currentLeaves.size).asLeft[MerkleTree].pure[F].widen
    } else {
      val updatedLeaves = currentLeaves.updated(index, leaf)
      MerkleTree.fromLeaves(updatedLeaves).map(_.asRight[MerkleProducerError])
    }
  }

  def remove(
    current: MerkleTree,
    index: Int
  ): F[Either[MerkleProducerError, MerkleTree]] = {
    val currentLeaves = current.leaves
    if (index < 0 || index >= currentLeaves.size) {
      InvalidIndex(index, currentLeaves.size).asLeft[MerkleTree].pure[F].widen
    } else {
      val updatedLeaves = currentLeaves.patch(index, Nil, 1)
      MerkleTree.fromLeaves(updatedLeaves).map(_.asRight[MerkleProducerError])
    }
  }

  def getProver(tree: MerkleTree): F[MerkleProver[F]] =
    MerkleProver.make[F](tree).pure[F]
}

object StatelessMerkleProducer {

  def apply[F[_]: JsonBinaryHasher: MonadThrow]: StatelessMerkleProducer[F] =
    new StatelessMerkleProducer[F]
}
