package io.constellationnetwork.metagraph_sdk.crypto.smt

import cats.Eq
import cats.syntax.eq._

import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

/**
 * Failure modes of [[api.SparseMerkleProver.prove]] / [[api.SparseMerkleVerifier.verify]]. Sealed, exhaustive, no string-typed control
 * flow -- the structural cause is carried, never re-derived from a message string. Extends `Throwable` so it can be
 * raised through the `F[_]` error channel like the MPT's `MerklePatriciaProofError`.
 *
 *   - [[SparseMerkleProofError.ValueBindingFailed]] -- an [[SparseMerkleProof.Inclusion]]'s `Hash.fromBytes(value)` did not equal the
 *     leaf's committed value digest (the value bytes do not bind to the committed leaf). Value-binding is MANDATORY
 *     inside verify; there is no verify variant that skips it.
 *   - [[SparseMerkleProofError.RootMismatch]] -- the root recomputed by folding the proof's authentication path did not equal
 *     the trusted [[SparseMerkleRoot]]. Carries both `expected` (trusted) and `got` (recomputed).
 *   - [[SparseMerkleProofError.MalformedProof]] -- the proof is structurally inconsistent before any root fold can be trusted:
 *     e.g. an [[AbsenceWitness.OtherLeaf]] whose recomputed position equals the queried position (so it is not a
 *     genuine OTHER leaf), or an authentication path deeper than the 256-bit position space. Carries the offending
 *     `key` and a structural `reason`.
 */
sealed trait SparseMerkleProofError extends Throwable with Product with Serializable

object SparseMerkleProofError {

  final case class ValueBindingFailed(key: Hex) extends SparseMerkleProofError {
    override def getMessage: String = s"Value bytes do not bind to the committed leaf for key: ${key.value}"
  }

  final case class RootMismatch(expected: Hash, got: Hash) extends SparseMerkleProofError {
    override def getMessage: String =
      s"Authentication-path fold did not reproduce the trusted root (expected ${expected.value}, got ${got.value})"
  }

  final case class MalformedProof(key: Hex, reason: MalformedReason) extends SparseMerkleProofError {
    override def getMessage: String = s"Malformed proof for key ${key.value}: $reason"
  }

  /** Structural (non-string) reasons a proof is malformed. Closed set so control flow stays pattern-matched. */
  sealed trait MalformedReason extends Product with Serializable
  object MalformedReason {

    /**
     * An [[AbsenceWitness.OtherLeaf]] whose recomputed occupying position equals the queried position -- i.e. the key
     * IS present, so the proof cannot soundly claim absence.
     */
    case object OtherLeafCollidesWithKey extends MalformedReason

    /** The authentication path (siblings list) is longer than the 256-bit position space allows. */
    case object PathTooDeep extends MalformedReason

    implicit val eq: Eq[MalformedReason] = Eq.fromUniversalEquals
  }

  // Local Eq for test assertions / dedup, never control flow.
  implicit val eq: Eq[SparseMerkleProofError] = Eq.instance {
    case (ValueBindingFailed(k1), ValueBindingFailed(k2)) => k1 === k2
    case (RootMismatch(e1, g1), RootMismatch(e2, g2))     => e1 === e2 && g1 === g2
    case (MalformedProof(k1, r1), MalformedProof(k2, r2)) => k1 === k2 && r1 === r2
    case _                                                => false
  }
}
