/*
 * Adapted from Bifrost (https://github.com/Topl/Bifrost) via Tessellation's
 * `io.constellationnetwork.numerics.RatioOps`. Remains MPL-2.0 (metakit is
 * otherwise Apache-2.0).
 *
 * Source: numerics/src/main/scala/co/topl/numerics/RatioOps.scala
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * Additions for the JSON Logic VM (all rely on the denominator > 0 invariant
 * established by Ratio.apply): floor, ceil, truncate, roundHalfUp, mod,
 * compare/min/max, signum, isInteger, toBigIntExact.
 */
package io.constellationnetwork.metagraph_sdk.numerics

object RatioOps {

  trait Implicits {

    implicit class Ops(ratio: Ratio) {

      def inverse: Ratio =
        Ratio(ratio.denominator, ratio.numerator)

      def abs: Ratio =
        Ratio(ratio.numerator.abs, ratio.denominator) // denominator already > 0

      def pow(n: Int): Ratio =
        Ratio(ratio.numerator.pow(n), ratio.denominator.pow(n))

      def unary_- : Ratio =
        Ratio(-ratio.numerator, ratio.denominator)

      def *(that: Int): Ratio = Ratio(ratio.numerator * that, ratio.denominator)
      def /(that: Int): Ratio = Ratio(ratio.numerator, ratio.denominator * that)
      def *(that: Long): Ratio = Ratio(ratio.numerator * that, ratio.denominator)
      def /(that: Long): Ratio = Ratio(ratio.numerator, ratio.denominator * that)
      def *(that: BigInt): Ratio = Ratio(ratio.numerator * that, ratio.denominator)

      def +(that: Ratio): Ratio =
        Ratio(
          ratio.numerator * that.denominator + that.numerator * ratio.denominator,
          ratio.denominator * that.denominator
        )

      def -(that: Ratio): Ratio =
        Ratio(
          ratio.numerator * that.denominator - that.numerator * ratio.denominator,
          ratio.denominator * that.denominator
        )

      def *(that: Ratio): Ratio =
        Ratio(ratio.numerator * that.numerator, ratio.denominator * that.denominator)

      def /(that: Ratio): Ratio =
        Ratio(ratio.numerator * that.denominator, ratio.denominator * that.numerator)

      // --- ordering (valid because denominator > 0 for both operands) ---

      def compare(that: Ratio): Int =
        (ratio.numerator * that.denominator).compare(that.numerator * ratio.denominator)

      def <(that: Ratio): Boolean = compare(that) < 0
      def >(that: Ratio): Boolean = compare(that) > 0
      def <=(that: Ratio): Boolean = compare(that) <= 0
      def >=(that: Ratio): Boolean = compare(that) >= 0

      def min(that: Ratio): Ratio = if (compare(that) <= 0) ratio else that
      def max(that: Ratio): Ratio = if (compare(that) >= 0) ratio else that

      def signum: Int = ratio.numerator.signum

      // --- integer views ---

      def isInteger: Boolean = ratio.denominator == 1

      def toBigIntExact: Option[BigInt] =
        if (isInteger) Some(ratio.numerator) else None

      /** Largest integer <= x. */
      def floor: BigInt = {
        val q = ratio.numerator / ratio.denominator
        val r = ratio.numerator % ratio.denominator
        if (r != 0 && ratio.numerator < 0) q - 1 else q
      }

      /** Smallest integer >= x. */
      def ceil: BigInt = {
        val q = ratio.numerator / ratio.denominator
        val r = ratio.numerator % ratio.denominator
        if (r != 0 && ratio.numerator > 0) q + 1 else q
      }

      /** Round toward zero. */
      def truncate: BigInt = ratio.numerator / ratio.denominator

      /** Round half away from zero — matches java.math BigDecimal RoundingMode.HALF_UP. */
      def roundHalfUp: BigInt = {
        val n   = ratio.numerator.abs
        val d   = ratio.denominator
        val mag = (n * 2 + d) / (d * 2) // floor(|x| + 1/2), all positive => truncation == floor
        ratio.numerator.signum * mag
      }

      /** Truncated remainder: a - b * truncate(a / b). Matches java.math BigDecimal.remainder. */
      def mod(that: Ratio): Ratio =
        ratio - (that * (ratio / that).truncate)

      def toBigInt: BigInt = truncate

      def toBigDecimal: BigDecimal =
        BigDecimal(ratio.numerator) / BigDecimal(ratio.denominator)

      /** Lossy. Diagnostics/serialization-boundary only — never compared on the deterministic path. */
      def toDouble: Double = toBigDecimal.toDouble
    }
  }

  object implicits extends Implicits
}
