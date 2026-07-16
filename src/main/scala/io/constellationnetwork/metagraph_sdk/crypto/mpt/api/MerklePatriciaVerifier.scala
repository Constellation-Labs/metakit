package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import cats.MonadThrow
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps

trait MerklePatriciaVerifier[F[_]] {

  /**
   * Confirm that a Merkle Patricia inclusion proof is valid
   *
   * @param proof The inclusion proof to verify
   * @return Success if the proof is valid, error otherwise
   */
  def confirm(proof: MerklePatriciaInclusionProof): F[Either[MerklePatriciaVerificationError, Unit]]

  /**
   * Confirm a sealed [[MerklePatriciaProof]] -- inclusion OR absence -- against the trusted root.
   *
   * Both arms are the SAME fold (digest recomputation per commitment with the node-type prefix
   * byte, child-digest threading, nibble consumption from the queried path); they differ only in
   * the terminal assertion. Inclusion requires the fold to end at a Leaf whose `remaining` equals
   * the un-consumed path. Absence requires the terminal commitment to (a) hash to the digest
   * reached by the fold, and (b) structurally refuse the next step: a Branch lacking the next
   * nibble (or with the path exhausted -- branches carry no value slot), an Extension whose
   * `shared` diverges from the remaining path, or a Leaf whose `remaining` differs. The fold
   * itself guarantees (c): every consumed nibble is bound to the query path (branch child lookup
   * by nibble, extension `shared` prefix check).
   *
   * @param proof The sealed proof to verify
   * @return Success if the proof is valid, error otherwise
   */
  def confirm(proof: MerklePatriciaProof): F[Either[MerklePatriciaVerificationError, Unit]]
}

object MerklePatriciaVerifier {
  def apply[F[_]](implicit verifier: MerklePatriciaVerifier[F]): MerklePatriciaVerifier[F] = verifier

  def make[F[_]: MonadThrow](root: Hash)(implicit producer: JsonBinaryHasher[F]): MerklePatriciaVerifier[F] =
    new MerklePatriciaVerifier[F] {
      private type Continue = (List[MerklePatriciaCommitment], Hash, Seq[Nibble])
      private type Return = Either[MerklePatriciaVerificationError, Unit]

      def confirm(proof: MerklePatriciaInclusionProof): F[Either[MerklePatriciaVerificationError, Unit]] =
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

      def confirm(proof: MerklePatriciaProof): F[Either[MerklePatriciaVerificationError, Unit]] =
        proof match {
          case MerklePatriciaProof.Inclusion(inclusion)   => confirm(inclusion)
          case MerklePatriciaProof.Absence(path, witness) => confirmAbsence(path, witness)
        }

      /**
       * Absence arm: replay the fold root-first over the witness; the last (deepest) commitment
       * is the terminal. It must hash to the digest the fold reached AND structurally lack a
       * continuation for the remaining path -- see [[MerklePatriciaProof]] for the three terminal
       * shapes. A terminal that could continue (or a Leaf that MATCHES) proves nothing: invalid.
       */
      private def confirmAbsence(
        path: Hex,
        witness: List[MerklePatriciaCommitment]
      ): F[Either[MerklePatriciaVerificationError, Unit]] =
        MonadThrow[F]
          .tailRecM[Continue, Return]((witness.reverse, root, Nibble(path))) {
            case (commitments, currentDigest, remainingPath) =>
              commitments match {
                case terminal :: Nil =>
                  verifyAbsenceTerminal(terminal, currentDigest, remainingPath)

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

      private def verifyAbsenceTerminal(
        terminal: MerklePatriciaCommitment,
        currentDigest: Hash,
        remainingPath: Seq[Nibble]
      ): F[Either[Continue, Return]] =
        MerklePatriciaCommitment
          .commitmentDigest[F](terminal)
          .map { digest =>
            if (digest != currentDigest)
              InvalidNodeCommitment("Invalid terminal commitment").asLeft[Unit].asRight[Continue]
            else {
              val witnessesAbsence = terminal match {
                case MerklePatriciaCommitment.Branch(pathsDigest) =>
                  remainingPath.isEmpty || !pathsDigest.contains(remainingPath.head)
                case MerklePatriciaCommitment.Extension(shared, _) =>
                  !remainingPath.startsWith(shared)
                case MerklePatriciaCommitment.Leaf(remaining, _) =>
                  remaining != remainingPath
              }
              if (witnessesAbsence) ().asRight[MerklePatriciaVerificationError].asRight[Continue]
              else InvalidPath("Terminal commitment does not witness absence of the path").asLeft[Unit].asRight[Continue]
            }
          }
          .handleError(e => InvalidNodeCommitment(s"Hash computation error: ${e.getMessage}").asLeft[Unit].asRight[Continue])

      private def verifyLeaf(
        nodeCommit: MerklePatriciaCommitment.Leaf,
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
          .handleError(e => InvalidNodeCommitment(s"Hash computation error: ${e.getMessage}").asLeft[Unit].asRight[Continue])

      private def verifyExtension(
        nodeCommit: MerklePatriciaCommitment.Extension,
        tail: List[MerklePatriciaCommitment],
        currentDigest: Hash,
        remainingPath: Seq[Nibble]
      ): F[Either[Continue, Return]] =
        // The shared-prefix check binds the consumed nibbles to the queried path (the batch
        // verifier has always enforced this); without it a witness from one path could be
        // replayed for another path of equal shape.
        if (!remainingPath.startsWith(nodeCommit.shared))
          MonadThrow[F].pure(
            InvalidPath("Extension shared nibbles diverge from the proof path").asLeft[Unit].asRight[Continue]
          )
        else
          JsonBinaryHasher[F]
            .computeDigest(nodeCommit.asJson, MerklePatriciaNode.ExtensionPrefix)
            .map { digest =>
              if (digest == currentDigest)
                (tail, nodeCommit.childDigest, remainingPath.drop(nodeCommit.shared.length)).asLeft[Return]
              else InvalidNodeCommitment("Invalid extension commitment").asLeft[Unit].asRight[Continue]
            }
            .handleError(e => InvalidNodeCommitment(s"Hash computation error: ${e.getMessage}").asLeft[Unit].asRight[Continue])

      private def verifyBranch(
        nodeCommit: MerklePatriciaCommitment.Branch,
        tail: List[MerklePatriciaCommitment],
        currentDigest: Hash,
        remainingPath: Seq[Nibble]
      ): F[Either[Continue, Return]] =
        remainingPath.headOption.flatMap(nodeCommit.pathsDigest.get) match {
          case Some(childDigest) =>
            JsonBinaryHasher[F]
              .computeDigest(nodeCommit.asJson, MerklePatriciaNode.BranchPrefix)
              .map { digest =>
                if (digest == currentDigest)
                  (tail, childDigest, remainingPath.tail).asLeft[Return]
                else
                  InvalidNodeCommitment("Invalid branch commitment").asLeft[Unit].asRight[Continue]
              }
              .handleError(e => InvalidNodeCommitment(s"Hash computation error: ${e.getMessage}").asLeft[Unit].asRight[Continue])

          case None =>
            val where = remainingPath.headOption.map(_.toString).getOrElse("<path exhausted>")
            MonadThrow[F].pure(
              InvalidPath(s"Path not found in branch: $where").asLeft[Unit].asRight[Continue]
            )
        }
    }

  /**
   * Provides syntax extensions for more ergonomic Merkle Patricia verification
   *
   * Import xyz.kd5ujc.accumulators.mpt.api.MerklePatriciaVerifier.syntax._ to use these extensions
   */
  object syntax {

    implicit class MerklePatriciaProofOps(private val proof: MerklePatriciaInclusionProof) extends AnyVal {

      /**
       * Confirm this proof is valid
       *
       * @return Success if the proof is valid
       */
      def confirm[F[_]](implicit V: MerklePatriciaVerifier[F]): F[Either[MerklePatriciaVerificationError, Unit]] =
        V.confirm(proof)
    }

    implicit class MerklePatriciaSealedProofOps(private val proof: MerklePatriciaProof) extends AnyVal {

      /**
       * Confirm this sealed (inclusion-or-absence) proof is valid
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
