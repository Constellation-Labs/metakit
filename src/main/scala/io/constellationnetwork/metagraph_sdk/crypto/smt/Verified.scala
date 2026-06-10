package io.constellationnetwork.metagraph_sdk.crypto.smt

/**
 * Proof that `value: A` was produced by an [[api.SparseMerkleVerifier]] and only an [[api.SparseMerkleVerifier]]. The constructor is
 * private and the only builder ([[Verified.makeInternal]]) is `private[smt]`, so a `Verified[A]` cannot be forged
 * outside this package.
 *
 * A consumer that wants verified state must hold a `Verified[SparseMerkleEntry]`, and there is no path to obtain one except
 * through `verify` -- so an absence/inclusion proof can never be consumed unverified.
 */
sealed abstract case class Verified[A] private (value: A)

object Verified {

  /** Sole builder. `private[smt]` -- callable only by the verifier in this package. */
  private[smt] def makeInternal[A](value: A): Verified[A] = new Verified[A](value) {}
}
