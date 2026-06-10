package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import cats.MonadThrow
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps

trait MerklePatriciaBatchInclusionVerifier[F[_]] {

  /**
   * Confirm that a batch inclusion proof is valid for every path it attests.
   *
   * @param proof The batch proof to verify
   * @return Success if every path verifies against the root, error otherwise
   */
  def confirm(proof: MerklePatriciaBatchInclusionProof): F[Either[MerklePatriciaVerificationError, Unit]]
}

object MerklePatriciaBatchInclusionVerifier {

  def apply[F[_]](implicit verifier: MerklePatriciaBatchInclusionVerifier[F]): MerklePatriciaBatchInclusionVerifier[F] = verifier

  def make[F[_]: MonadThrow: JsonBinaryHasher](root: Hash): MerklePatriciaBatchInclusionVerifier[F] =
    new MerklePatriciaBatchInclusionVerifier[F] {

      private val singleVerifier = MerklePatriciaVerifier.make[F](root)

      def confirm(proof: MerklePatriciaBatchInclusionProof): F[Either[MerklePatriciaVerificationError, Unit]] = {

        // Reconstruct the per-path witness from the shared, de-duplicated witness by walking from
        // the root: at each step, find the commitment whose prefixed digest matches the expected
        // child digest and whose path is consistent with the remaining nibbles. The reconstructed
        // (leaf-first) witness is then handed to the standard single-path verifier.
        def reconstructProof(
          path: Hex,
          sharedWitness: List[MerklePatriciaCommitment]
        ): F[Either[MerklePatriciaVerificationError, Unit]] = {
          type Continue = (Seq[Nibble], Hash, List[MerklePatriciaCommitment])
          type Return = Either[MerklePatriciaVerificationError, List[MerklePatriciaCommitment]]

          val pathNibbles = Nibble(path)

          def checkCommitment(
            commitment: MerklePatriciaCommitment,
            expectedDigest: Hash,
            remainingPath: Seq[Nibble]
          ): F[Option[(MerklePatriciaCommitment, Hash, Seq[Nibble])]] =
            commitment match {
              case leaf: MerklePatriciaCommitment.Leaf =>
                JsonBinaryHasher[F]
                  .computeDigest(leaf.asJson, MerklePatriciaNode.LeafPrefix)
                  .map { digest =>
                    if (digest == expectedDigest && remainingPath == leaf.remaining)
                      Some((leaf, expectedDigest, Seq.empty[Nibble]))
                    else None
                  }

              case ext: MerklePatriciaCommitment.Extension =>
                JsonBinaryHasher[F]
                  .computeDigest(ext.asJson, MerklePatriciaNode.ExtensionPrefix)
                  .map { digest =>
                    if (digest == expectedDigest && remainingPath.startsWith(ext.shared))
                      Some((ext, ext.childDigest, remainingPath.drop(ext.shared.length)))
                    else None
                  }

              case branch: MerklePatriciaCommitment.Branch =>
                JsonBinaryHasher[F]
                  .computeDigest(branch.asJson, MerklePatriciaNode.BranchPrefix)
                  .map { digest =>
                    if (digest == expectedDigest && remainingPath.nonEmpty && branch.pathsDigest.contains(remainingPath.head))
                      Some((branch, branch.pathsDigest(remainingPath.head), remainingPath.tail))
                    else None
                  }
            }

          def findMatchingCommitment(
            expectedDigest: Hash,
            remainingPath: Seq[Nibble]
          ): F[Option[(MerklePatriciaCommitment, Hash, Seq[Nibble])]] =
            sharedWitness
              .traverse(checkCommitment(_, expectedDigest, remainingPath))
              .map(_.collectFirst { case Some(result) => result })

          MonadThrow[F]
            .tailRecM[Continue, Return]((pathNibbles, root, List.empty[MerklePatriciaCommitment])) {
              case (remainingPath, expectedDigest, acc) =>
                if (remainingPath.isEmpty)
                  acc.asRight[MerklePatriciaVerificationError].asRight[Continue].pure[F]
                else
                  findMatchingCommitment(expectedDigest, remainingPath).map {
                    case Some((commitment: MerklePatriciaCommitment.Leaf, _, _)) =>
                      (commitment :: acc).asRight[MerklePatriciaVerificationError].asRight[Continue]

                    case Some((commitment, nextDigest, nextPath)) =>
                      (nextPath, nextDigest, commitment :: acc).asLeft[Return]

                    case None =>
                      (InvalidWitness(
                        s"No matching commitment for digest ${expectedDigest.value} at path ${path.value} " +
                        s"(position ${pathNibbles.length - remainingPath.length}/${pathNibbles.length})"
                      ): MerklePatriciaVerificationError)
                        .asLeft[List[MerklePatriciaCommitment]]
                        .asRight[Continue]
                  }
            }
            .flatMap {
              case Right(relevantCommitments) =>
                singleVerifier.confirm(MerklePatriciaInclusionProof(path, relevantCommitments))
              case Left(error) =>
                error.asLeft[Unit].pure[F]
            }
        }

        if (proof.paths.isEmpty)
          (InvalidWitness("Batch proof cannot have empty paths list"): MerklePatriciaVerificationError)
            .asLeft[Unit]
            .pure[F]
        else
          proof.paths
            .traverse(path => reconstructProof(path, proof.witness))
            .map(_.sequence.map(_ => ()))
            .handleError(e => InvalidWitness(s"Batch verification failed: ${e.getMessage}").asLeft[Unit])
      }
    }

  /**
   * Provides syntax extensions for more ergonomic batch verification
   */
  object syntax {

    implicit class MerklePatriciaBatchInclusionProofOps(private val proof: MerklePatriciaBatchInclusionProof) extends AnyVal {

      def confirm[F[_]](implicit V: MerklePatriciaBatchInclusionVerifier[F]): F[Either[MerklePatriciaVerificationError, Unit]] =
        V.confirm(proof)
    }
  }
}
