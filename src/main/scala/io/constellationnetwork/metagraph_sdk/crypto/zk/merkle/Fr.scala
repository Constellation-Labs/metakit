package io.constellationnetwork.metagraph_sdk.crypto.zk.merkle

import io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon

/**
 * Canonical-encoding helpers for the BN254 / alt_bn128 scalar field `Fr` used by the Poseidon Merkle tree.
 *
 * An `Fr` element is just a `BigInt` in the canonical range `[0, R)`, where `R` is the field modulus shared with
 * [[Poseidon]] (and with the Groth16 machinery in the wider `crypto.zk` package):
 *
 *   R = 21888242871839275222246405745257275088548364400416034343698204186575808495617
 *
 * Encoding discipline (be exact):
 *   - Leaves, positions, sibling digests and roots are ALL canonical field elements in `[0, R)`.
 *   - [[reduce]] maps any `BigInt` into that range with Euclidean (always-non-negative) reduction, so a negative
 *     input does not wrap to a negative residue. This matches what the in-circuit arithmetic computes.
 *   - The tree itself never silently reduces caller-supplied leaves; it [[require]]s canonicality and reports the
 *     offending value. [[reduce]] is provided for callers that want to normalise before handing values in, and is
 *     used internally only where a value is already known to be a Poseidon output (hence already canonical).
 */
object Fr {

  /** The BN254 scalar field modulus, shared with [[Poseidon.R]]. */
  val R: BigInt = Poseidon.R

  /** The additive identity, used as the canonical "empty leaf" of the tree. */
  val Zero: BigInt = BigInt(0)

  /** True iff `x` is already a canonical field element, i.e. `0 <= x < R`. */
  def isCanonical(x: BigInt): Boolean = x >= 0 && x < R

  /**
   * Euclidean reduction of `x` into `[0, R)`. `mod` on Scala's `BigInt` already returns a non-negative result for a
   * positive modulus, so this is exactly `x mod R`; named [[reduce]] for intent at call sites.
   */
  def reduce(x: BigInt): BigInt = x.mod(R)

  /** Reject a non-canonical element, naming the role for a useful error; returns the element unchanged. */
  def requireCanonical(x: BigInt, role: => String): BigInt = {
    require(isCanonical(x), s"$role is not a canonical BN254 field element (must be in [0, R)): $x")
    x
  }
}
