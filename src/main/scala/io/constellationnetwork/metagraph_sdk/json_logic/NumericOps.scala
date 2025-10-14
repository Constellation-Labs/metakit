package io.constellationnetwork.metagraph_sdk.json_logic

import cats.syntax.either._
import cats.syntax.traverse._

import scala.annotation.tailrec

/**
 * Unified numeric handling for consistent arithmetic operations across Int/Float types
 */
object NumericOps {

  sealed trait NumericResult {
    def toBigDecimal: BigDecimal = this match {
      case IntResult(i)   => BigDecimal(i)
      case FloatResult(f) => f
    }

    def toJsonLogicValue: JsonLogicValue = this match {
      case IntResult(i)   => IntValue(i)
      case FloatResult(f) => FloatValue(f)
    }
  }

  case class IntResult(value: BigInt) extends NumericResult
  case class FloatResult(value: BigDecimal) extends NumericResult

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
          Either.catchNonFatal(BigInt(s))
            .map(IntResult(_))
            .orElse(Either.catchNonFatal(BigDecimal(s)).map(FloatResult(_)))
            .leftMap(_ => JsonLogicException(s"Cannot convert string '$s' to number"))
        }
      case ArrayValue(List(single)) =>
        // Single element arrays coerce to their element
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
   * Combines two numeric values using the given operation.
   * Returns IntValue if both operands are ints and result is whole, otherwise FloatValue
   */
  def combineNumeric(
    op: (BigDecimal, BigDecimal) => BigDecimal
  )(left: NumericResult, right: NumericResult): JsonLogicValue =
    (left, right) match {
      case (IntResult(l), IntResult(r)) =>
        val result = op(BigDecimal(l), BigDecimal(r))
        if (result.isWhole && result.isValidLong) {
          IntValue(result.toBigInt)
        } else {
          FloatValue(result)
        }
      case (IntResult(l), FloatResult(r))  => FloatValue(op(BigDecimal(l), r))
      case (FloatResult(l), IntResult(r))  => FloatValue(op(l, BigDecimal(r)))
      case (FloatResult(l), FloatResult(r)) => FloatValue(op(l, r))
    }

  /**
   * Combines a list of numeric values using the given operation and identity element
   */
  def reduceNumeric(
    values: List[JsonLogicValue],
    op: (BigDecimal, BigDecimal) => BigDecimal,
    identity: BigDecimal
  ): Either[JsonLogicException, JsonLogicValue] =
    if (values.isEmpty) {
      JsonLogicException("Cannot reduce empty list").asLeft
    } else {
      values.traverse(promoteToNumeric).map { numerics =>
        val hasFloat = numerics.exists(_.isInstanceOf[FloatResult])
        val result = numerics.map(_.toBigDecimal).reduce(op)

        if (!hasFloat && result.isWhole && result.isValidLong) {
          IntValue(result.toBigInt)
        } else {
          FloatValue(result)
        }
      }
    }

  /**
   * Compares two numeric values
   */
  def compareNumeric(left: NumericResult, right: NumericResult): Int =
    left.toBigDecimal.compare(right.toBigDecimal)

  /**
   * Checks if two numeric values are equal
   */
  def numericEquals(left: NumericResult, right: NumericResult): Boolean =
    left.toBigDecimal == right.toBigDecimal
}