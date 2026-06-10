package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import cats.MonadThrow
import cats.syntax.all._

import scala.annotation.tailrec

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hex.Hex

trait MerklePatriciaBatchInclusionProver[F[_]] {

  /**
   * Generate a single proof attesting that all of the given paths are present in the trie.
   *
   * @param paths The paths to prove inclusion for
   * @return A batch proof with a de-duplicated shared witness, or an error
   */
  def attestPaths(paths: List[Hex]): F[Either[MerklePatriciaProofError, MerklePatriciaBatchInclusionProof]]
}

object MerklePatriciaBatchInclusionProver {

  def apply[F[_]](implicit prover: MerklePatriciaBatchInclusionProver[F]): MerklePatriciaBatchInclusionProver[F] = prover

  /**
   * Create a batch prover from a Merkle Patricia Trie.
   *
   * Each path is attested independently via the single-path prover; the resulting per-path
   * witnesses are concatenated and de-duplicated into one shared witness. Paths are sorted so the
   * proof is deterministic regardless of the caller's input order.
   */
  def make[F[_]: MonadThrow: JsonBinaryHasher](
    trie: MerklePatriciaTrie
  ): MerklePatriciaBatchInclusionProver[F] =
    new MerklePatriciaBatchInclusionProver[F] {

      def attestPaths(paths: List[Hex]): F[Either[MerklePatriciaProofError, MerklePatriciaBatchInclusionProof]] = {

        val singleProver = MerklePatriciaProver.make[F](trie)

        def commitmentKey(commitment: MerklePatriciaCommitment): String = commitment match {
          case MerklePatriciaCommitment.Leaf(remaining, dataDigest) =>
            s"leaf:${remaining.mkString}:${dataDigest.value}"
          case MerklePatriciaCommitment.Branch(pathsDigest) =>
            s"branch:${pathsDigest.toSeq.sortBy(_._1.value).map { case (k, v) => s"${k.value}:${v.value}" }.mkString(",")}"
          case MerklePatriciaCommitment.Extension(shared, childDigest) =>
            s"extension:${shared.mkString}:${childDigest.value}"
        }

        def deduplicate(commitments: List[MerklePatriciaCommitment]): List[MerklePatriciaCommitment] = {
          @tailrec
          def loop(
            remaining: List[MerklePatriciaCommitment],
            seen: Set[String],
            acc: List[MerklePatriciaCommitment]
          ): List[MerklePatriciaCommitment] = remaining match {
            case Nil => acc.reverse
            case head :: tail =>
              val key = commitmentKey(head)
              if (seen.contains(key)) loop(tail, seen, acc)
              else loop(tail, seen + key, head :: acc)
          }

          loop(commitments, Set.empty, List.empty)
        }

        if (paths.isEmpty)
          (ProofGenerationError("Cannot create batch proof for empty path list"): MerklePatriciaProofError)
            .asLeft[MerklePatriciaBatchInclusionProof]
            .pure[F]
        else {
          val sortedPaths = paths.sorted(Ordering.by[Hex, String](_.value))

          sortedPaths
            .traverse(singleProver.attestPath)
            .map { results =>
              results.sequence.map { proofs =>
                val deduplicated = deduplicate(proofs.flatMap(_.witness))
                MerklePatriciaBatchInclusionProof(sortedPaths, deduplicated)
              }
            }
            .handleError(e => ProofGenerationError(e.getMessage).asLeft[MerklePatriciaBatchInclusionProof])
        }
      }
    }

  /**
   * Provides syntax extensions for more ergonomic batch proof generation
   */
  object syntax {

    implicit class MerklePatriciaPathListOps(private val paths: List[Hex]) extends AnyVal {

      def attestBatchInclusion[F[_]](
        implicit P: MerklePatriciaBatchInclusionProver[F]
      ): F[Either[MerklePatriciaProofError, MerklePatriciaBatchInclusionProof]] =
        P.attestPaths(paths)
    }
  }
}
