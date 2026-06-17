package io.constellationnetwork.metagraph_sdk.crypto.smt

import cats.Eq
import cats.syntax.eq._

import io.constellationnetwork.security.hex.Hex

/**
 * The verified outcome a consumer obtains from [[api.SparseMerkleVerifier.verify]], wrapped in [[Verified]] so it cannot be
 * forged. Either the key is present with its (value-bound) bytes, or the key is proven absent -- both established
 * against the trusted [[SparseMerkleRoot]].
 */
sealed trait SparseMerkleEntry extends Product with Serializable {
  def key: Hex
}

object SparseMerkleEntry {

  /**
   * The key is present and `value` cryptographically binds to the committed leaf (`Hash.fromBytes(value)` matched the
   * leaf's value digest during verify).
   */
  final case class Present(key: Hex, value: Hex) extends SparseMerkleEntry

  /** The key is absent under the trusted root (verified via [[AbsenceWitness]]). */
  final case class Absent(key: Hex) extends SparseMerkleEntry

  // Structural Eq (value compared by content). For test assertions, never control flow.
  implicit val eq: Eq[SparseMerkleEntry] = Eq.instance {
    case (Present(k1, v1), Present(k2, v2)) => k1 === k2 && v1 === v2
    case (Absent(k1), Absent(k2))           => k1 === k2
    case _                                  => false
  }
}
