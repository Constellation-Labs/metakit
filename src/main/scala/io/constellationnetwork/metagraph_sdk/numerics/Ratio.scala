/*
 * Adapted from Bifrost (https://github.com/Topl/Bifrost), originally
 * licensed under the Mozilla Public License 2.0, by way of the Tessellation
 * project's `io.constellationnetwork.numerics.Ratio`. This file remains under
 * MPL-2.0 even though metakit is otherwise Apache-2.0.
 *
 * Source: models/src/main/scala/co/topl/models/utility/Ratio.scala
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * Changes from the Tessellation copy:
 *  - The smart constructor normalizes to a strictly positive denominator, so
 *    the gcd-reduced form is canonical for every value (incl. negatives). This
 *    is required for the JSON Logic VM, whose `/` op can otherwise produce
 *    negative denominators that break the cross-multiplication comparisons in
 *    RatioOps. With denom > 0, `equals`/`hashCode`/compare are all well-defined.
 *  - Added `fromBigDecimal` (exact: BigDecimal is a terminating decimal).
 */
package io.constellationnetwork.metagraph_sdk.numerics

import scala.annotation.tailrec

/**
 * Exact rational `numerator / denominator`, gcd-reduced with a strictly positive denominator at construction time.
 *
 * Used as the JSON Logic VM's numeric backbone so that the Scala, Rust, and WASM evaluators compute byte-identical results
 * regardless of JVM/CPU/JIT — eliminating the IEEE-754 nondeterminism that Double/BigDecimal-with-MathContext would introduce.
 * All arithmetic is exact; the only rounding happens at canonical serialization (RFC 8785 shortest-double), which is itself
 * deterministic.
 */
case class Ratio(numerator: BigInt, denominator: BigInt, greatestCommonDenominator: BigInt) {

  override def toString(): String =
    numerator.toString + (if (denominator != 1) "/" + denominator else "")

  override def equals(that: Any): Boolean =
    that match {
      case that: Ratio => numerator == that.numerator && denominator == that.denominator
      case _           => false
    }

  override def hashCode: Int =
    41 * numerator.hashCode() + denominator.hashCode()
}

object Ratio {

  val One: Ratio = Ratio(1)
  val Zero: Ratio = Ratio(0)
  val NegativeOne: Ratio = Ratio(-1)

  def apply(n: BigInt): Ratio = apply(n, BigInt(1))

  def apply(n: BigInt, d: BigInt): Ratio = {
    if (d == 0) throw new ArithmeticException("Ratio denominator cannot be zero")
    val g = gcd(n, d).abs
    val nn = n / g
    val dd = d / g
    // Canonicalize the sign onto the numerator so the denominator is always > 0.
    if (dd < 0) new Ratio(-nn, -dd, g) else new Ratio(nn, dd, g)
  }

  def apply(i: Int): Ratio = apply(BigInt(i), BigInt(1))

  def apply(n: Int, d: Int): Ratio = apply(BigInt(n), BigInt(d))

  /** Exact conversion from a terminating decimal: BigDecimal `v` == unscaledValue * 10^(-scale). No precision loss. */
  def fromBigDecimal(v: BigDecimal): Ratio = {
    val unscaled = BigInt(v.bigDecimal.unscaledValue)
    val scale = v.bigDecimal.scale
    if (scale >= 0) apply(unscaled, BigInt(10).pow(scale))
    else apply(unscaled * BigInt(10).pow(-scale), BigInt(1))
  }

  /**
   * Convert a Double to Ratio with `prec` decimal digits of precision. Lossy; intended only for parsing Double-typed
   * configuration at boot. After this point the value never touches Double again.
   */
  def apply(double: Double, prec: Int): Ratio = {
    val d = BigInt(10).pow(prec)
    val n = (BigDecimal(double).setScale(prec, BigDecimal.RoundingMode.DOWN) * BigDecimal(d)).toBigInt
    apply(n, d)
  }

  @tailrec
  private def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == 0) a else gcd(b, a % b)
}
