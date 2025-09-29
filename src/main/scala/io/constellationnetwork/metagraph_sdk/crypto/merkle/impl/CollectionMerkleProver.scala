package io.constellationnetwork.metagraph_sdk.crypto.merkle.impl

import cats.MonadThrow
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api._
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleInclusionProof, MerkleNode, MerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.metagraph_sdk.storage.CollectionReader
import io.constellationnetwork.security.hash.Hash

/**
 * A prover that generates proofs directly from storage, independent of any producer.
 * This is read-only and rebuilds the tree from leaves for each proof generation.
 *
 * Use this when you need a completely independent prover that doesn't share state
 * with a producer (e.g., a separate read-only service).
 */
class CollectionMerkleProver[F[_]: MonadThrow: JsonBinaryHasher](
  leavesStore: CollectionReader[F, Int, MerkleNode.Leaf]
) extends MerkleProver[F] {

  def attestLeaf(leaf: MerkleNode.Leaf): F[Either[MerkleProofError, MerkleInclusionProof]] =
    attestDigest(leaf.digest)

  def attestDigest(digest: Hash): F[Either[MerkleProofError, MerkleInclusionProof]] =
    buildTreeAndProve(tree => MerkleProver.make[F](tree).attestDigest(digest))

  def attestIndex(index: Int): F[Either[MerkleProofError, MerkleInclusionProof]] =
    buildTreeAndProve(tree => MerkleProver.make[F](tree).attestIndex(index))

  private def buildTreeAndProve(
    prove: MerkleTree => F[Either[MerkleProofError, MerkleInclusionProof]]
  ): F[Either[MerkleProofError, MerkleInclusionProof]] =
    for {
      leaves <- getAllLeaves
      result <-
        if (leaves.isEmpty) {
          InvalidLeafIndex(0, 0)
            .asLeft[MerkleInclusionProof]
            .pure[F]
            .widen[Either[MerkleProofError, MerkleInclusionProof]]
        } else {
          MerkleTree.fromLeaves(leaves).flatMap(prove)
        }
    } yield result

  private def getAllLeaves: F[List[MerkleNode.Leaf]] =
    leavesStore.dump.map(_.sortBy(_._1).map(_._2))
}

object CollectionMerkleProver {

  /**
   * Create a prover that reads from storage and rebuilds the tree for each proof.
   */
  def make[F[_]: MonadThrow: JsonBinaryHasher](
    leavesStore: CollectionReader[F, Int, MerkleNode.Leaf]
  ): MerkleProver[F] =
    new CollectionMerkleProver[F](leavesStore)
}
