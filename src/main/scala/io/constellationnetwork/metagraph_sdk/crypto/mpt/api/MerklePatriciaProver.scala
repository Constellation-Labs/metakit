package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import cats.MonadThrow
import cats.syntax.applicativeError._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

trait MerklePatriciaProver[F[_]] {

  /**
   * Generate a proof that a digest exists in the trie
   *
   * @param digest The digest to prove inclusion for
   * @return A proof of inclusion if the digest exists, or an error
   */
  def attestDigest(digest: Hash): F[Either[MerklePatriciaProofError, MerklePatriciaInclusionProof]]
}

object MerklePatriciaProver {
  def apply[F[_]](implicit prover: MerklePatriciaProver[F]): MerklePatriciaProver[F] = prover

  /**
   * Create a prover instance from a Merkle Patricia Trie
   */
  def make[F[_]: MonadThrow](
    trie: MerklePatriciaTrie
  )(implicit producer: JsonBinaryHasher[F]): MerklePatriciaProver[F] =
    new MerklePatriciaProver[F] {

      def attestDigest(digest: Hash): F[Either[MerklePatriciaProofError, MerklePatriciaInclusionProof]] = {
        type Continue = (MerklePatriciaNode, Seq[Nibble], List[MerklePatriciaCommitment])
        type Return = Either[MerklePatriciaProofError, List[MerklePatriciaCommitment]]

        MonadThrow[F]
          .tailRecM[Continue, Return]((trie.rootNode, Nibble(digest), List.empty[MerklePatriciaCommitment])) {
            case (currentNode, remainingPath, acc) =>
              currentNode match {
                case leaf: MerklePatriciaNode.Leaf if leaf.remaining == remainingPath =>
                  JsonBinaryHasher[F]
                    .computeDigest(leaf.data)
                    .map(dataDigest => MerklePatriciaCommitment.Leaf(leaf.remaining, dataDigest) :: acc)
                    .map(commitments => commitments.asRight[MerklePatriciaProofError])
                    .map(_.asRight[Continue])
                    .handleError(e =>
                      ProofGenerationError(e.getMessage).asLeft[List[MerklePatriciaCommitment]].asRight[Continue]
                    )

                case extension: MerklePatriciaNode.Extension if remainingPath.startsWith(extension.shared) =>
                  MonadThrow[F].pure(
                    (
                      extension.child,
                      remainingPath.drop(extension.shared.length),
                      MerklePatriciaCommitment.Extension(extension.shared, extension.child.digest) :: acc
                    ).asLeft[Return]
                  )

                case branch: MerklePatriciaNode.Branch if remainingPath.nonEmpty =>
                  branch.paths.get(remainingPath.head) match {
                    case Some(child) =>
                      MonadThrow[F].pure(
                        (
                          child,
                          remainingPath.tail,
                          MerklePatriciaCommitment.Branch(
                            branch.paths.toSeq.sortBy(_._1.value).map { case (k, v) => k -> v.digest }.toMap
                          ) :: acc
                        ).asLeft[Return]
                      )

                    case None =>
                      MonadThrow[F].pure(
                        PathNotFound(s"Path not found: ${digest.toString}")
                          .asLeft[List[MerklePatriciaCommitment]]
                          .asRight[Continue]
                      )
                  }

                case _ =>
                  MonadThrow[F].pure(
                    InvalidNodeType(s"Unexpected node type encountered for path: ${digest.toString}")
                      .asLeft[List[MerklePatriciaCommitment]]
                      .asRight[Continue]
                  )
              }
          }
          .map(_.map(commitments => MerklePatriciaInclusionProof(digest, commitments)))
          .handleError(e => ProofGenerationError(e.getMessage).asLeft[MerklePatriciaInclusionProof])
      }
    }

  /**
   * Provides syntax extensions for more ergonomic proof generation
   *
   * Import xyz.kd5ujc.accumulators.mpt.api.MerklePatriciaProver.syntax._ to use these extensions
   */
  object syntax {

    implicit class MerklePatriciaDigestOps(val digest: Hash) extends AnyVal {

      /**
       * Generate a proof that this digest exists in the trie
       *
       * @return A proof of inclusion if the digest exists
       */
      def attestInclusion[F[_]](implicit
        P: MerklePatriciaProver[F]
      ): F[Either[MerklePatriciaProofError, MerklePatriciaInclusionProof]] =
        P.attestDigest(digest)
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
