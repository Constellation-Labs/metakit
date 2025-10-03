package io.constellationnetwork.metagraph_sdk.crypto.merkle.api

import cats.syntax.applicative._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.{Applicative, MonadThrow}

import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleCommitment, MerkleInclusionProof, MerkleNode}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash

/**
 * Type class for verifying Merkle inclusion proofs
 */
trait MerkleVerifier[F[_]] {

  /**
   * Confirm that a Merkle inclusion proof is valid
   *
   * @param proof The inclusion proof to verify
   * @return True if the proof is valid for this tree's root
   */
  def confirm(proof: MerkleInclusionProof): F[Boolean]

  /**
   * Confirm that a leaf exists in the tree
   *
   * @param leaf The leaf node to verify
   * @param proof The inclusion proof for this leaf
   * @return True if the leaf exists and the proof is valid
   */
  def confirmLeaf(leaf: MerkleNode.Leaf, proof: MerkleInclusionProof)(implicit app: Applicative[F]): F[Boolean] =
    if (leaf.digest == proof.leafDigest) confirm(proof)
    else false.pure[F]
}

object MerkleVerifier {
  def apply[F[_]](implicit verifier: MerkleVerifier[F]): MerkleVerifier[F] = verifier

  /**
   * Create a verifier for a specific root digest
   */
  private def fromRoot[F[_]: MonadThrow](root: Hash): MerkleVerifier[F] =
    new MerkleVerifier[F] {

      def confirm(proof: MerkleInclusionProof): F[Boolean] = {
        def combine(a: Hash, b: Hash): F[Hash] =
          MerkleCommitment
            .Internal(a, b)
            .computePrefixDigest(MerkleNode.InternalPrefix)

        proof.witness
          .foldLeftM(proof.leafDigest) {
            case (acc, (digest, MerkleInclusionProof.LeftSide))  => combine(digest, acc)
            case (acc, (digest, MerkleInclusionProof.RightSide)) => combine(acc, digest)
          }
          .map(_ == root)
      }
    }

  /**
   * Create a verifier instance from a root
   */
  def make[F[_]: MonadThrow](root: Hash): MerkleVerifier[F] =
    fromRoot(root)

  /**
   * Provides syntax extensions for more ergonomic Merkle verification
   *
   * Import xyz.kd5ujc.accumulators.merkle.api.MerkleVerifier.syntax._ to use these extensions
   */
  object syntax {

    implicit class MerkleVerifierOps(private val proof: MerkleInclusionProof) extends AnyVal {

      /**
       * Confirm this proof is valid
       *
       * @return True if the proof is valid for the tree's root
       */
      def confirm[F[_]](implicit V: MerkleVerifier[F]): F[Boolean] =
        V.confirm(proof)
    }

    implicit class MerkleLeafOps(private val leaf: MerkleNode.Leaf) extends AnyVal {

      /**
       * Confirm this leaf exists in the tree
       *
       * @param proof The inclusion proof for this leaf
       * @return True if the leaf exists and the proof is valid
       */
      def confirmInclusion[F[_]](
        proof: MerkleInclusionProof
      )(implicit V: MerkleVerifier[F], app: Applicative[F]): F[Boolean] =
        V.confirmLeaf(leaf, proof)
    }
  }
}
