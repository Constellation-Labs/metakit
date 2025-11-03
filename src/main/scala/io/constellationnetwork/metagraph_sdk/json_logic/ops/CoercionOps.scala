package io.constellationnetwork.metagraph_sdk.json_logic.ops

import cats.syntax.either._

import scala.annotation.tailrec

import io.constellationnetwork.metagraph_sdk.json_logic.core._

sealed trait CoercedValue
case object CoercedNull extends CoercedValue
final case class CoercedBool(value: Boolean) extends CoercedValue
final case class CoercedInt(value: BigInt) extends CoercedValue
final case class CoercedFloat(value: BigDecimal) extends CoercedValue
final case class CoercedString(value: String) extends CoercedValue

object CoercionOps {

  @tailrec
  def coerceToPrimitive(value: JsonLogicValue): Either[JsonLogicException, CoercedValue] =
    value match {
      case NullValue     => CoercedNull.asRight
      case BoolValue(b)  => CoercedBool(b).asRight
      case IntValue(i)   => CoercedInt(i).asRight
      case FloatValue(d) => CoercedFloat(d).asRight
      case StrValue(s) =>
        if (s.isEmpty) CoercedInt(0).asRight
        else s.toIntOption.fold[CoercedValue](CoercedString(s))(i => CoercedInt(i)).asRight
      case FunctionValue(expr) => JsonLogicException(s"Cannot coerce FunctionValue($expr) to a primitive").asLeft
      case ArrayValue(elems) =>
        elems match {
          case Nil           => Right(CoercedInt(0))
          case single :: Nil => coerceToPrimitive(single)
          case _             => JsonLogicException(s"Cannot coerce multi-element array $elems to a single primitive").asLeft
        }
      case MapValue(m) =>
        m.size match {
          case 0 => Right(CoercedInt(0))
          case 1 => coerceToPrimitive(m.values.head)
          case _ => JsonLogicException(s"Cannot coerce multi-key object $m to a single primitive").asLeft
        }
    }

  private def safeParseBigInt(s: String): Option[BigInt] =
    try
      Some(BigInt(s))
    catch {
      case _: NumberFormatException => None
    }

  private def safeParseBigDecimal(s: String): Option[BigDecimal] =
    try
      Some(BigDecimal(s))
    catch {
      case _: NumberFormatException => None
    }

  def compareCoercedValues(l: CoercedValue, r: CoercedValue): Either[JsonLogicException, Boolean] =
    (l, r) match {
      case (CoercedNull, CoercedNull)             => true.asRight
      case (CoercedNull, _)                       => false.asRight
      case (_, CoercedNull)                       => false.asRight
      case (CoercedBool(lb), CoercedBool(rb))     => (lb == rb).asRight
      case (CoercedBool(lb), CoercedInt(ri))      => (if (lb) ri == 1 else ri == 0).asRight
      case (CoercedInt(li), CoercedBool(rb))      => (if (rb) li == 1 else li == 0).asRight
      case (CoercedInt(li), CoercedInt(ri))       => (li == ri).asRight
      case (CoercedInt(li), CoercedString(rs))    => safeParseBigInt(rs).contains(li).asRight
      case (CoercedString(ls), CoercedInt(ri))    => safeParseBigInt(ls).contains(ri).asRight
      case (CoercedFloat(li), CoercedString(rs))  => safeParseBigDecimal(rs).contains(li).asRight
      case (CoercedString(ls), CoercedFloat(ri))  => safeParseBigDecimal(ls).contains(ri).asRight
      case (CoercedString(ls), CoercedString(rs)) => (ls == rs).asRight
      case _                                      => JsonLogicException(s"Cannot compare coerced values $l and $r").asLeft
    }
}
