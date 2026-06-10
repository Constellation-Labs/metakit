package io.constellationnetwork.metagraph_sdk.json_logic.ops

import cats.syntax.either._
import cats.syntax.traverse._

import scala.annotation.tailrec

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.numerics.Ratio
import io.constellationnetwork.metagraph_sdk.numerics.RatioOps.implicits._

/**
 * Unified numeric handling for the JSON Logic VM.
 *
 * Arithmetic is exact (rational) so the Scala, Rust, and WASM evaluators agree byte-for-byte — the cross-language
 * reproducibility that matching Java's BigDecimal DECIMAL128/setScale rounding elsewhere would not give us. The only
 * rounding is at canonical serialization (RFC 8785 shortest-double), which is deterministic. Integers and non-integers are
 * tracked separately so operator result typing (IntValue vs FloatValue) matches JSON Logic semantics.
 */
object NumericOps {

  /**
   * Maximum permitted magnitude of the effective decimal scale (fractional digits minus exponent) accepted by string ->
   * number coercion. Mirrors the Rust reference `Ratio::MAX_DECIMAL_SCALE` (rust/jlvm-core/src/ratio.rs).
   *
   * SECURITY: `Ratio.fromBigDecimal` materializes `10^|scale|` as a full BigInt, so an attacker-controlled exponent like
   * "1e-2000000000" would eagerly allocate a multi-GB integer (memory bomb). Scala's BigDecimal stores such strings
   * compactly without expansion, so without this shared bound the Scala evaluator would compute an exact tiny value where
   * Rust/TS reject — a consensus divergence. With it, programs like {"+":["1e-2000000000"]} error in ALL impls.
   */
  val MaxDecimalScale: Int = 10000

  /**
   * Parse a decimal string into an exact Ratio, rejecting any value whose effective decimal scale magnitude exceeds
   * [[MaxDecimalScale]]. Java BigDecimal's `scale` is exactly `fractionalDigits - exponent`, the same quantity Rust's
   * `Ratio::parse_decimal` bounds; it is checked BEFORE `Ratio.fromBigDecimal` materializes `10^|scale|`.
   */
  def parseDecimalBounded(s: String): Either[Throwable, Ratio] =
    Either.catchNonFatal(BigDecimal(s)).flatMap { bd =>
      val scale = bd.bigDecimal.scale
      if (math.abs(scale.toLong) > MaxDecimalScale.toLong)
        JsonLogicException(s"Decimal scale $scale exceeds maximum magnitude $MaxDecimalScale").asLeft
      else
        Ratio.fromBigDecimal(bd).asRight
    }

  /** Exact rational division (was BigDecimal DECIMAL128 on dev; now lossless). */
  def safeDivide(l: Ratio, r: Ratio): Ratio =
    l / r

  def safeToInt(bi: BigInt, name: String): Either[JsonLogicException, Int] =
    if (bi >= Int.MinValue && bi <= Int.MaxValue)
      bi.toInt.asRight
    else
      JsonLogicException(s"$name value $bi exceeds Int range").asLeft

  /**
   * BigInt -> i64 (Long) conversion matching the Rust reference `bigint_to_i64` (eval.rs): values outside the i64 range
   * are an error. The error text byte-matches Rust's (`"<name> out of range"`).
   */
  def safeToI64(bi: BigInt, name: String): Either[JsonLogicException, Long] =
    if (bi >= Long.MinValue && bi <= Long.MaxValue)
      bi.toLong.asRight
    else
      JsonLogicException(s"$name out of range").asLeft

  /**
   * Long addition saturating at the i64 bounds, mirroring Rust's `i64::saturating_add` used by `op_substr` / `op_slice`:
   * the operands are attacker-controlled i64 extremes, and saturation followed by the callers' clamps yields the same
   * indices as exact (unbounded) arithmetic would.
   */
  def saturatingAddI64(a: Long, b: Long): Long = {
    val r = a + b
    if (((a ^ r) & (b ^ r)) < 0L) {
      if (a < 0L) Long.MinValue else Long.MaxValue
    } else r
  }

  sealed trait NumericResult {

    def toRatio: Ratio = this match {
      case IntResult(i)   => Ratio(i)
      case FloatResult(r) => r
    }

    def isFloat: Boolean = this match {
      case FloatResult(_) => true
      case IntResult(_)   => false
    }

    def toBigDecimal: BigDecimal = toRatio.toBigDecimal

    def toJsonLogicValue: JsonLogicValue = this match {
      case IntResult(i)   => IntValue(i)
      case FloatResult(r) => FloatValue(r)
    }
  }

  case class IntResult(value: BigInt) extends NumericResult
  case class FloatResult(value: Ratio) extends NumericResult

  /**
   * Promotes a JsonLogicValue to a numeric type, handling coercion
   */
  @tailrec
  def promoteToNumeric(value: JsonLogicValue): Either[JsonLogicException, NumericResult] =
    value match {
      case IntValue(i)   => IntResult(i).asRight
      case FloatValue(f) => FloatResult(f).asRight
      case BoolValue(b)  => IntResult(if (b) 1 else 0).asRight
      case NullValue     => IntResult(0).asRight
      case StrValue(s) =>
        if (s.isEmpty) {
          IntResult(0).asRight
        } else {
          Either
            .catchNonFatal(BigInt(s))
            .map(IntResult(_): NumericResult)
            .orElse(parseDecimalBounded(s).map(FloatResult(_): NumericResult))
            .leftMap(_ => JsonLogicException(s"Cannot convert string '$s' to number"))
        }
      case ArrayValue(List(single)) =>
        promoteToNumeric(single)
      case ArrayValue(Nil) =>
        IntResult(0).asRight
      case ArrayValue(list) =>
        JsonLogicException(s"Cannot convert multi-element array ${list.mkString("[", ",", "]")} to number").asLeft
      case MapValue(m) if m.isEmpty =>
        IntResult(0).asRight
      case MapValue(m) if m.size == 1 =>
        promoteToNumeric(m.values.head)
      case MapValue(m) =>
        JsonLogicException(s"Cannot convert multi-key object with keys ${m.keys.mkString(",")} to number").asLeft
      case FunctionValue(_) =>
        JsonLogicException("Cannot convert function to number").asLeft
    }

  /**
   * Combines two numeric values using the given exact-rational operation.
   * Returns IntValue when neither operand was a float and the result is integral, otherwise FloatValue.
   */
  def combineNumeric(
    op: (Ratio, Ratio) => Ratio
  )(left: NumericResult, right: NumericResult): JsonLogicValue = {
    val result = op(left.toRatio, right.toRatio)
    if (!left.isFloat && !right.isFloat && result.isInteger) IntValue(result.toBigInt)
    else FloatValue(result)
  }

  /**
   * Combines a list of numeric values using the given exact-rational operation.
   */
  def reduceNumeric(
    values: List[JsonLogicValue],
    op: (Ratio, Ratio) => Ratio
  ): Either[JsonLogicException, JsonLogicValue] =
    if (values.isEmpty) JsonLogicException("Cannot reduce empty list").asLeft
    else {
      values.traverse(promoteToNumeric).map { numerics =>
        val hasFloat = numerics.exists(_.isFloat)
        val result = numerics.map(_.toRatio).reduce(op)

        if (!hasFloat && result.isInteger) IntValue(result.toBigInt)
        else FloatValue(result)
      }
    }

  /**
   * Compares two numeric values exactly.
   */
  def compareNumeric(left: NumericResult, right: NumericResult): Int =
    left.toRatio.compare(right.toRatio)

  /**
   * Plain decimal string for a rational, used by the string ops (cat / join / in). Integral values render without a
   * decimal point; non-integral values use the exact decimal expansion with trailing zeros stripped.
   */
  def floatToPlainString(r: Ratio): String =
    if (r.isInteger) r.numerator.toString
    else r.toBigDecimal.bigDecimal.stripTrailingZeros.toPlainString
}
