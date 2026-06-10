package io.constellationnetwork.metagraph_sdk.crypto.smt.api

import cats.MonadThrow
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.smt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

/**
 * Verifies a native [[SparseMerkleProof]] against a trusted [[SparseMerkleRoot]], returning a [[Verified]]-gated [[SparseMerkleEntry]] (so the
 * result cannot be consumed unverified).
 *
 * Verification is the SAME authentication-path fold for inclusion and absence -- they differ only in what sits at the
 * terminating slot:
 *   - INCLUSION: assert `Hash.fromBytes(value) == proof.valueDigest` (mandatory value-binding =>
 *     [[SparseMerkleProofError.ValueBindingFailed]] on mismatch), recompute the leaf digest from `(position, valueDigest)`, fold
 *     the siblings up, compare to the trusted root.
 *   - ABSENCE/Default: fold the EMPTY placeholder (`Hash.empty`) up through the siblings, compare to the trusted root.
 *   - ABSENCE/OtherLeaf: recompute the occupying leaf's position; assert it differs from the queried position (else it
 *     is not a genuine other leaf => [[SparseMerkleProofError.MalformedProof]]); recompute its leaf digest from
 *     `(occupyingPosition, occupyingDataDigest)`, fold up, compare to the trusted root. The root match also enforces the
 *     shared-prefix relationship.
 *
 * Any path longer than the 256-bit position space is [[SparseMerkleProofError.MalformedProof]]; a fold that does not reproduce
 * the trusted root is [[SparseMerkleProofError.RootMismatch]].
 */
trait SparseMerkleVerifier[F[_]] {
  def verify(root: SparseMerkleRoot, proof: SparseMerkleProof): F[Either[SparseMerkleProofError, Verified[SparseMerkleEntry]]]
}

object SparseMerkleVerifier {

  def apply[F[_]](implicit verifier: SparseMerkleVerifier[F]): SparseMerkleVerifier[F] = verifier

  def make[F[_]: MonadThrow: JsonBinaryHasher]: SparseMerkleVerifier[F] = new SparseMerkleVerifier[F] {

    def verify(root: SparseMerkleRoot, proof: SparseMerkleProof): F[Either[SparseMerkleProofError, Verified[SparseMerkleEntry]]] =
      if (proof.siblings.length > SparseMerkleHashing.PositionBits)
        (SparseMerkleProofError.MalformedProof(proof.key, SparseMerkleProofError.MalformedReason.PathTooDeep): SparseMerkleProofError)
          .asLeft[Verified[SparseMerkleEntry]]
          .pure[F]
      else
        proof match {
          case SparseMerkleProof.Inclusion(key, value, valueDigest, siblings) =>
            SparseMerkleHashing.valueDigest[F](value).flatMap { computed =>
              if (computed != valueDigest)
                (SparseMerkleProofError.ValueBindingFailed(key): SparseMerkleProofError).asLeft[Verified[SparseMerkleEntry]].pure[F]
              else
                SparseMerkleHashing.position[F](key).flatMap { pos =>
                  SparseMerkleHashing
                    .leafDigest[F](pos, valueDigest)
                    .flatMap(leaf => foldUp(pos, leaf, siblings))
                    .map(recomputed => bindRoot(root, recomputed, SparseMerkleEntry.Present(key, value)))
                }
            }

          case SparseMerkleProof.Absence(key, AbsenceWitness.Default, siblings) =>
            SparseMerkleHashing.position[F](key).flatMap { pos =>
              foldUp(pos, SparseMerkleHashing.empty, siblings)
                .map(recomputed => bindRoot(root, recomputed, SparseMerkleEntry.Absent(key)))
            }

          case SparseMerkleProof.Absence(key, AbsenceWitness.OtherLeaf(occupyingKey, occupyingDataDigest), siblings) =>
            (SparseMerkleHashing.position[F](key), SparseMerkleHashing.position[F](occupyingKey)).tupled.flatMap {
              case (pos, occPos) =>
                if (occPos == pos)
                  (SparseMerkleProofError.MalformedProof(
                    key,
                    SparseMerkleProofError.MalformedReason.OtherLeafCollidesWithKey
                  ): SparseMerkleProofError)
                    .asLeft[Verified[SparseMerkleEntry]]
                    .pure[F]
                else
                  SparseMerkleHashing
                    .leafDigest[F](occPos, occupyingDataDigest)
                    .flatMap(leaf => foldUp(pos, leaf, siblings))
                    .map(recomputed => bindRoot(root, recomputed, SparseMerkleEntry.Absent(key)))
            }
        }

    /**
     * Fold a terminating digest `start` (sitting at depth = `siblings.length`) up to the root, choosing left/right at
     * each level by the corresponding bit of `position`. `siblings` is top-down (root-first); we consume it
     * deepest-first.
     */
    private def foldUp(position: Hash, start: Hash, siblings: List[SparseMerkleSibling]): F[Hash] = {
      val depth = siblings.length
      // (level, sibling) pairs, deepest level first: level d-1 down to 0.
      val indexed = siblings.reverse.zipWithIndex.map { case (sib, i) => (depth - 1 - i, sib.digest) }
      indexed.foldLeft(start.pure[F]) {
        case (acc, (level, sibling)) =>
          acc.flatMap(cur => SparseMerkleHashing.combine[F](SparseMerkleHashing.bit(position, level), cur, sibling))
      }
    }

    private def bindRoot(
      root: SparseMerkleRoot,
      recomputed: Hash,
      entry: SparseMerkleEntry
    ): Either[SparseMerkleProofError, Verified[SparseMerkleEntry]] =
      if (recomputed == root.value) Verified.makeInternal(entry).asRight[SparseMerkleProofError]
      else (SparseMerkleProofError.RootMismatch(root.value, recomputed): SparseMerkleProofError).asLeft[Verified[SparseMerkleEntry]]
  }

  /** Provides syntax extensions for more ergonomic verification. */
  object syntax {

    implicit class SparseMerkleProofOps(private val proof: SparseMerkleProof) extends AnyVal {

      /** Verify this proof against the trusted `root`. */
      def verifyAgainst[F[_]](root: SparseMerkleRoot)(
        implicit V: SparseMerkleVerifier[F]
      ): F[Either[SparseMerkleProofError, Verified[SparseMerkleEntry]]] =
        V.verify(root, proof)
    }
  }
}
