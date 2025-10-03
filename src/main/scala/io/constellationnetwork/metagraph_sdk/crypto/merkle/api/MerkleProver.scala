package io.constellationnetwork.metagraph_sdk.crypto.merkle.api

import cats.MonadThrow
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

        @tailrec
        def countLeaves(nodes: List[MerkleNode], acc: Int = 0): Int =
          nodes match {
            case Nil => acc
            case (_: MerkleNode.Leaf) :: tail =>
              countLeaves(tail, acc + 1)
            case MerkleNode.Internal(left, rightOpt, _) :: tail =>
              rightOpt match {
                case Some(right) => countLeaves(left :: right :: tail, acc)
                case None        => countLeaves(left :: tail, acc)
              }
          }

        @tailrec
        def findPath(
          node: MerkleNode,
          targetIdx: Int,
          currentIdx: Int,
          acc: List[(Hash, Side)]
        ): Option[(MerkleNode.Leaf, List[(Hash, Side)])] =
          node match {
            case n: MerkleNode.Leaf =>
              if (currentIdx == targetIdx) Some((n, acc))
              else None

            case MerkleNode.Internal(left, rightOpt, _) =>
              val leftCount = countLeaves(List(left))

              rightOpt match {
                case Some(right) =>
                  if (targetIdx < currentIdx + leftCount) {
                    // Target is in left subtree
                    findPath(
                      left,
                      targetIdx,
                      currentIdx,
                      (right.digest, MerkleInclusionProof.RightSide) :: acc
                    )
                  } else {
                    // Target is in right subtree
                    findPath(
                      right,
                      targetIdx,
                      currentIdx + leftCount,
                      (left.digest, MerkleInclusionProof.LeftSide) :: acc
                    )
                  }
                case None =>
                  // Unbalanced tree indicates corruption - return None to signal error
                  None
              }
          }

        if (index < 0 || index >= tree.leafDigestIndex.size) {
          InvalidLeafIndex(index, tree.leafDigestIndex.size).asLeft[MerkleInclusionProof].pure[F].widen
        } else {
          findPath(tree.rootNode, index, 0, List.empty) match {
            case Some((leaf, witness)) =>
              MerkleInclusionProof(leaf.digest, witness).asRight[MerkleProofError].pure[F]
            case None =>
              // Could be invalid index or malformed tree structure
              MalformedTreeStructure(s"Failed to generate proof for index $index. Tree may be corrupted or unbalanced.")
                .asLeft[MerkleInclusionProof]
                .pure[F]
                .widen[Either[MerkleProofError, MerkleInclusionProof]]
          }
        }
      }
    }

  /**
   * Provides syntax extensions for more ergonomic proof generation*
   */
  object syntax {

    implicit class MerkleLeafOps(private val leaf: MerkleNode.Leaf) extends AnyVal {

      /**
       * Generate a proof that this leaf exists in the tree
       *
       * @return A proof of inclusion if the leaf exists
       */
      def attestInclusion[F[_]](implicit P: MerkleProver[F]): F[Either[MerkleProofError, MerkleInclusionProof]] =
        P.attestLeaf(leaf)
    }

    implicit class MerkleDigestOps(private val digest: Hash) extends AnyVal {

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

case class MalformedTreeStructure(message: String) extends MerkleProofError {
  override def getMessage: String = message
}
