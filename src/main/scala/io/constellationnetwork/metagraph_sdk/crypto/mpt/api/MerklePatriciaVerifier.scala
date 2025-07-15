package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import cats.MonadThrow
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax.EncoderOps

trait MerklePatriciaVerifier[F[_]] {

  /**
   * Confirm that a Merkle Patricia inclusion proof is valid
   *
   * @param proof The inclusion proof to verify
   * @return Success if the proof is valid, error otherwise
   */
  def confirm(proof: MerklePatriciaInclusionProof): F[Either[MerklePatriciaVerificationError, Unit]]
}

object MerklePatriciaVerifier {
  def apply[F[_]](implicit verifier: MerklePatriciaVerifier[F]): MerklePatriciaVerifier[F] = verifier

  def make[F[_]: MonadThrow](root: Hash)(implicit producer: JsonBinaryHasher[F]): MerklePatriciaVerifier[F] =
    new MerklePatriciaVerifier[F] {

      def confirm(proof: MerklePatriciaInclusionProof): F[Either[MerklePatriciaVerificationError, Unit]] = {
        type Continue = (List[MerklePatriciaCommitment], Hash, Seq[Nibble])
        type Return = Either[MerklePatriciaVerificationError, Unit]

        def verifyLeaf(
          nodeCommit:    MerklePatriciaCommitment.Leaf,
          currentDigest: Hash,
          remainingPath: Seq[Nibble]
        ): F[Either[Continue, Return]] =
          JsonBinaryHasher[F]
            .computeDigest(nodeCommit.asJson, MerklePatriciaNode.LeafPrefix)
            .map { digest =>
              if (digest == currentDigest && remainingPath == nodeCommit.remaining)
                ().asRight[MerklePatriciaVerificationError].asRight[Continue]
              else InvalidNodeCommitment("Invalid leaf commitment or path mismatch").asLeft[Unit].asRight[Continue]
            }
            .handleError(e =>
              InvalidNodeCommitment(s"Hash computation error: ${e.getMessage}").asLeft[Unit].asRight[Continue]
            )

        def verifyExtension(
          nodeCommit:    MerklePatriciaCommitment.Extension,
          tail:          List[MerklePatriciaCommitment],
          currentDigest: Hash,
          remainingPath: Seq[Nibble]
        ): F[Either[Continue, Return]] =
          JsonBinaryHasher[F]
            .computeDigest(nodeCommit.asJson, MerklePatriciaNode.ExtensionPrefix)
            .map { digest =>
              if (digest == currentDigest)
                (tail, nodeCommit.childDigest, remainingPath.drop(nodeCommit.shared.length)).asLeft[Return]
              else InvalidNodeCommitment("Invalid extension commitment").asLeft[Unit].asRight[Continue]
            }
            .handleError(e =>
              InvalidNodeCommitment(s"Hash computation error: ${e.getMessage}").asLeft[Unit].asRight[Continue]
            )

        def verifyBranch(
          nodeCommit:    MerklePatriciaCommitment.Branch,
          tail:          List[MerklePatriciaCommitment],
          currentDigest: Hash,
          remainingPath: Seq[Nibble]
        ): F[Either[Continue, Return]] =
          nodeCommit.pathsDigest.get(remainingPath.head) match {
            case Some(childDigest) =>
              JsonBinaryHasher[F]
                .computeDigest(nodeCommit.asJson, MerklePatriciaNode.BranchPrefix)
                .map { digest =>
                  if (digest == currentDigest)
                    (tail, childDigest, remainingPath.tail).asLeft[Return]
                  else
                    InvalidNodeCommitment("Invalid branch commitment").asLeft[Unit].asRight[Continue]
                }
                .handleError(e =>
                  InvalidNodeCommitment(s"Hash computation error: ${e.getMessage}").asLeft[Unit].asRight[Continue]
                )

            case None =>
              MonadThrow[F].pure(
                InvalidPath(s"Path not found in branch: ${remainingPath.head}").asLeft[Unit].asRight[Continue]
              )
          }

        MonadThrow[F]
          .tailRecM[Continue, Return]((proof.witness.reverse, root, Nibble(proof.path))) {
            case (commitments, currentDigest, remainingPath) =>
              commitments match {
                case (nodeCommit: MerklePatriciaCommitment.Leaf) :: Nil =>
                  verifyLeaf(nodeCommit, currentDigest, remainingPath)

                case (nodeCommit: MerklePatriciaCommitment.Extension) :: tail =>
                  verifyExtension(nodeCommit, tail, currentDigest, remainingPath)

                case (nodeCommit: MerklePatriciaCommitment.Branch) :: tail =>
                  verifyBranch(nodeCommit, tail, currentDigest, remainingPath)

                case _ =>
                  MonadThrow[F].pure(
                    InvalidWitness("Invalid witness structure").asLeft[Unit].asRight[Continue]
                  )
              }
          }
          .handleError(e => InvalidWitness(s"Verification failed with error: ${e.getMessage}").asLeft[Unit])
      }
    }

  /**
   * Provides syntax extensions for more ergonomic Merkle Patricia verification
   *
   * Import xyz.kd5ujc.accumulators.mpt.api.MerklePatriciaVerifier.syntax._ to use these extensions
   */
  object syntax {

    implicit class MerklePatriciaProofOps(val proof: MerklePatriciaInclusionProof) extends AnyVal {

      /**
       * Confirm this proof is valid
       *
       * @return Success if the proof is valid
       */
      def confirm[F[_]](implicit V: MerklePatriciaVerifier[F]): F[Either[MerklePatriciaVerificationError, Unit]] =
        V.confirm(proof)
    }
  }
}

sealed trait MerklePatriciaVerificationError extends Throwable

case class InvalidWitness(message: String) extends MerklePatriciaVerificationError {
  override def getMessage: String = message
}

case class InvalidPath(message: String) extends MerklePatriciaVerificationError {
  override def getMessage: String = message
}

case class InvalidNodeCommitment(message: String) extends MerklePatriciaVerificationError {
  override def getMessage: String = message
}
