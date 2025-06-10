package io.constellationnetwork.metagraph_sdk.crypto.merkle.api

import cats.MonadThrow
import cats.implicits.toTraverseOps
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.functor._

import scala.annotation.tailrec

import io.constellationnetwork.metagraph_sdk.crypto.merkle.MerkleInclusionProof.Side
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleInclusionProof, MerkleNode, MerkleTree}
import io.constellationnetwork.security.hash.Hash

/**
 * Type class for generating Merkle inclusion proofs
 */
trait MerkleProver[F[_]] {

  /**
   * Generate a proof that a leaf exists in the tree
   *
   * @param leaf The leaf node to prove inclusion for
   * @return A proof of inclusion if the leaf exists
   */
  def attestLeaf(leaf: MerkleNode.Leaf): F[Either[MerkleProofError, MerkleInclusionProof]]

  /**
   * Generate a proof that a digest exists as a leaf in the tree
   *
   * @param digest The digest to prove inclusion for
   * @return A proof of inclusion if the digest exists as a leaf
   */
  def attestDigest(digest: Hash): F[Either[MerkleProofError, MerkleInclusionProof]]

  /**
   * Generate a proof for a leaf at a specific index
   *
   * @param index The index of the leaf
   * @return A proof of inclusion if the index is valid
   */
  def attestIndex(index: Int): F[Either[MerkleProofError, MerkleInclusionProof]]
}

object MerkleProver {
  def apply[F[_]](implicit prover: MerkleProver[F]): MerkleProver[F] = prover

  /**
   * Create a prover instance from a Merkle tree
   */
  def make[F[_]: MonadThrow](tree: MerkleTree): MerkleProver[F] =
    new MerkleProver[F] {

      def attestLeaf(leaf: MerkleNode.Leaf): F[Either[MerkleProofError, MerkleInclusionProof]] =
        attestDigest(leaf.digest)

      def attestDigest(digest: Hash): F[Either[MerkleProofError, MerkleInclusionProof]] =
        tree.leafDigestIndex
          .get(digest)
          .map(proofByIndex)
          .getOrElse(LeafNotFound(digest).asLeft.pure[F].widen)

      def attestIndex(index: Int): F[Either[MerkleProofError, MerkleInclusionProof]] =
        proofByIndex(index)

      private def proofByIndex(index: Int): F[Either[MerkleProofError, MerkleInclusionProof]] = {
        // bitwise shift operation to calculate log2 by counting number of divisions by 2
        @tailrec
        def log2(n: Int, acc: Int = 0): Int =
          if (n <= 1) acc
          else log2(n >> 1, acc + 1)

        val maxDepth = log2(tree.leafDigestIndex.size)

        @tailrec
        def loop(
          node:  MerkleNode,
          acc:   Seq[Option[(Hash, Side)]],
          depth: Int
        ): Option[(MerkleNode.Leaf, Seq[Option[(Hash, Side)]])] =
          node match {
            case MerkleNode.Internal(left, right, _) =>
              if (((index >> (maxDepth - depth)) & 1) == 0) {
                loop(left, right.map(_.digest).map((_, MerkleInclusionProof.RightSide)) +: acc, depth + 1)
              } else {
                loop(right.get, Some((left.digest, MerkleInclusionProof.LeftSide)) +: acc, depth + 1)
              }

            case n: MerkleNode.Leaf =>
              Some((n, acc))
          }

        if (index < 0 || index >= tree.leafDigestIndex.size)
          InvalidLeafIndex(index, tree.leafDigestIndex.size).asLeft[MerkleInclusionProof].pure[F].widen
        else
          loop(tree.rootNode, Seq(), 0)
            .flatMap { case (leaf, witness) =>
              witness.sequence.map(MerkleInclusionProof(leaf.digest, _))
            }
            .toRight[MerkleProofError](InvalidLeafIndex(index, tree.leafDigestIndex.size))
            .pure[F]
      }
    }

  /**
   * Provides syntax extensions for more ergonomic proof generation
   *
   * Import xyz.kd5ujc.accumulators.merkle.api.MerkleProver.syntax._ to use these extensions
   */
  object syntax {

    implicit class MerkleLeafOps(val leaf: MerkleNode.Leaf) extends AnyVal {

      /**
       * Generate a proof that this leaf exists in the tree
       *
       * @return A proof of inclusion if the leaf exists
       */
      def attestInclusion[F[_]](implicit P: MerkleProver[F]): F[Either[MerkleProofError, MerkleInclusionProof]] =
        P.attestLeaf(leaf)
    }

    implicit class MerkleDigestOps(val digest: Hash) extends AnyVal {

      /**
       * Generate a proof that this digest exists as a leaf in the tree
       *
       * @return A proof of inclusion if the digest exists as a leaf
       */
      def attestInclusion[F[_]](implicit P: MerkleProver[F]): F[Either[MerkleProofError, MerkleInclusionProof]] =
        P.attestDigest(digest)
    }
  }
}

sealed trait MerkleProofError extends Throwable

case class LeafNotFound(digest: Hash) extends MerkleProofError {
  override def getMessage: String = s"Leaf with digest ${digest.value} not found"
}

case class InvalidLeafIndex(index: Int, size: Int) extends MerkleProofError {
  override def getMessage: String = s"Invalid leaf index $index, tree size is $size"
}
