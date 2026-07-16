package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import cats.MonadThrow
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hex.Hex

trait MerklePatriciaProver[F[_]] {

  /**
   * Generate a proof that a path exists in the trie.
   *
   * INCLUSION-ONLY, kept for backward compatibility: an absent path is an error (`PathNotFound`
   * when a branch lacks the next nibble, `InvalidNodeType` for any other divergence), exactly as
   * before absence proofs existed. Prefer [[provePath]], which returns a first-class
   * [[MerklePatriciaProof.Absence]] instead of an error. (Deprecated in spirit; not annotated so
   * the many existing call sites stay warning-free.)
   *
   * @param path The path to prove inclusion for
   * @return A proof of inclusion if the path exists, or an error
   */
  def attestPath(path: Hex): F[Either[MerklePatriciaProofError, MerklePatriciaInclusionProof]]

  /**
   * Prove the STATUS of a path: [[MerklePatriciaProof.Inclusion]] when present,
   * [[MerklePatriciaProof.Absence]] (the root-to-divergence witness chain, terminal commitment
   * deepest) when not. Mirrors the SMT api's `SparseMerkleProver.prove`, which also emits
   * inclusion or absence from the same walk.
   *
   * @param path The path whose presence or absence is to be proven
   * @return The sealed proof; the error channel only reports hashing/traversal failures
   */
  def provePath(path: Hex): F[Either[MerklePatriciaProofError, MerklePatriciaProof]]
}

object MerklePatriciaProver {
  def apply[F[_]](implicit prover: MerklePatriciaProver[F]): MerklePatriciaProver[F] = prover

  /**
   * How the trie walk terminated. `AbsentMissingChild` (a branch lacking the next nibble) is kept
   * distinct from `AbsentDiverged` (leaf/extension divergence or a path exhausted at a branch)
   * only so `attestPath` can reproduce its legacy `PathNotFound` vs `InvalidNodeType` errors.
   */
  sealed private trait WalkOutcome { def witness: List[MerklePatriciaCommitment] }
  private case class Included(witness: List[MerklePatriciaCommitment]) extends WalkOutcome
  private case class AbsentMissingChild(witness: List[MerklePatriciaCommitment]) extends WalkOutcome
  private case class AbsentDiverged(witness: List[MerklePatriciaCommitment]) extends WalkOutcome

  /**
   * Create a prover instance from a Merkle Patricia Trie
   */
  def make[F[_]: MonadThrow](
    trie: MerklePatriciaTrie
  )(implicit producer: JsonBinaryHasher[F]): MerklePatriciaProver[F] =
    new MerklePatriciaProver[F] {

      def attestPath(path: Hex): F[Either[MerklePatriciaProofError, MerklePatriciaInclusionProof]] =
        walk(path).map(_.flatMap {
          case Included(witness) =>
            MerklePatriciaInclusionProof(path, witness).asRight[MerklePatriciaProofError]
          case AbsentMissingChild(_) =>
            PathNotFound(s"Path not found: ${path.value}").asLeft[MerklePatriciaInclusionProof]
          case AbsentDiverged(_) =>
            InvalidNodeType(s"Unexpected node type encountered for path: ${path.value}").asLeft[MerklePatriciaInclusionProof]
        })

      def provePath(path: Hex): F[Either[MerklePatriciaProofError, MerklePatriciaProof]] =
        walk(path).map(_.map {
          case Included(witness)    => MerklePatriciaProof.Inclusion(MerklePatriciaInclusionProof(path, witness))
          case outcome: WalkOutcome => MerklePatriciaProof.Absence(path, outcome.witness)
        })

      /**
       * Descend from the root along `path`, accumulating the commitment chain (deepest-first).
       * Terminates with `Included` at a leaf whose `remaining` matches, and with an absence
       * outcome wherever the path cannot continue; the terminal commitment is always recorded so
       * absence witnesses bind the divergence point to the root.
       */
      private def walk(path: Hex): F[Either[MerklePatriciaProofError, WalkOutcome]] = {
        type Continue = (MerklePatriciaNode, Seq[Nibble], List[MerklePatriciaCommitment])
        type Return = Either[MerklePatriciaProofError, WalkOutcome]

        MonadThrow[F]
          .tailRecM[Continue, Return]((trie.rootNode, Nibble(path), List.empty[MerklePatriciaCommitment])) {
            case (currentNode, remainingPath: Seq[Nibble], acc) =>
              currentNode match {
                case leaf: MerklePatriciaNode.Leaf =>
                  JsonBinaryHasher[F]
                    .computeDigest(leaf.data)
                    .map { dataDigest =>
                      val witness = MerklePatriciaCommitment.Leaf(leaf.remaining, dataDigest) :: acc
                      val outcome: WalkOutcome =
                        if (leaf.remaining == remainingPath) Included(witness) else AbsentDiverged(witness)
                      outcome.asRight[MerklePatriciaProofError].asRight[Continue]
                    }
                    .handleError(e => ProofGenerationError(e.getMessage).asLeft[WalkOutcome].asRight[Continue])

                case extension: MerklePatriciaNode.Extension if remainingPath.startsWith(extension.shared) =>
                  MonadThrow[F].pure(
                    (
                      extension.child,
                      remainingPath.drop(extension.shared.length),
                      MerklePatriciaCommitment.Extension(extension.shared, extension.child.digest) :: acc
                    ).asLeft[Return]
                  )

                case extension: MerklePatriciaNode.Extension =>
                  MonadThrow[F].pure(
                    (AbsentDiverged(
                      MerklePatriciaCommitment.Extension(extension.shared, extension.child.digest) :: acc
                    ): WalkOutcome).asRight[MerklePatriciaProofError].asRight[Continue]
                  )

                case branch: MerklePatriciaNode.Branch =>
                  val commitment = MerklePatriciaCommitment.Branch(
                    branch.paths.toSeq.sortBy(_._1.value).map { case (k, v) => k -> v.digest }.toMap
                  )

                  remainingPath.headOption.flatMap(n => branch.paths.get(n).map(n -> _)) match {
                    case Some((_, child)) =>
                      MonadThrow[F].pure((child, remainingPath.tail, commitment :: acc).asLeft[Return])

                    case None =>
                      val outcome: WalkOutcome =
                        if (remainingPath.nonEmpty) AbsentMissingChild(commitment :: acc)
                        else AbsentDiverged(commitment :: acc)

                      MonadThrow[F].pure(outcome.asRight[MerklePatriciaProofError].asRight[Continue])
                  }
              }
          }
          .handleError(e => ProofGenerationError(e.getMessage).asLeft[WalkOutcome])
      }
    }

  /**
   * Provides syntax extensions for more ergonomic proof generation
   */
  object syntax {

    implicit class MerklePatriciaPathOps(private val path: Hex) extends AnyVal {

      /**
       * Generate a proof that this path exists in the trie
       *
       * @return A proof of inclusion if the path exists
       */
      def attestInclusion[F[_]](implicit P: MerklePatriciaProver[F]): F[Either[MerklePatriciaProofError, MerklePatriciaInclusionProof]] =
        P.attestPath(path)

      /**
       * Prove this path's status (inclusion or absence) in the trie
       *
       * @return A sealed proof of inclusion or absence
       */
      def attestStatus[F[_]](implicit P: MerklePatriciaProver[F]): F[Either[MerklePatriciaProofError, MerklePatriciaProof]] =
        P.provePath(path)
    }
  }
}

sealed trait MerklePatriciaProofError extends Throwable

case class PathNotFound(path: String) extends MerklePatriciaProofError {
  override def getMessage: String = s"Path not found: $path"
}

case class InvalidNodeType(message: String) extends MerklePatriciaProofError {
  override def getMessage: String = message
}

case class ProofGenerationError(message: String) extends MerklePatriciaProofError {
  override def getMessage: String = message
}
