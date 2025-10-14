package io.constellationnetwork.metagraph_sdk.json_logic

import cats.syntax.either._
import cats.syntax.traverse._

import scala.annotation.tailrec

import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait JsonLogicValue {
  val tag: String
}

sealed abstract class JsonLogicPrimitive(val tag: String) extends JsonLogicValue
sealed abstract class JsonLogicCollection(val tag: String) extends JsonLogicValue

case object NullValue extends JsonLogicValue { val tag = "null" }
final case class FunctionValue(expr: JsonLogicExpression) extends JsonLogicValue { val tag = "function" }
final case class BoolValue(value: Boolean) extends JsonLogicPrimitive("bool")
final case class IntValue(value: BigInt) extends JsonLogicPrimitive("int")
final case class FloatValue(value: BigDecimal) extends JsonLogicPrimitive("float")
final case class StrValue(value: String) extends JsonLogicPrimitive("string")
final case class ArrayValue(value: List[JsonLogicValue]) extends JsonLogicCollection("array")
final case class MapValue(value: Map[String, JsonLogicValue]) extends JsonLogicCollection("map")

sealed trait CoercedValue
case object CoercedNull extends CoercedValue
final case class CoercedBool(value: Boolean) extends CoercedValue
final case class CoercedInt(value: BigInt) extends CoercedValue
final case class CoercedFloat(value: BigDecimal) extends CoercedValue
final case class CoercedString(value: String) extends CoercedValue

object JsonLogicValue {

  implicit class lookupDefaultOps(value: JsonLogicValue) {

    def getDefault: JsonLogicValue = value match {
      case NullValue        => NullValue
      case _: FunctionValue => FunctionValue(ApplyExpression(JsonLogicOp.NoOp, List.empty))
      case _: BoolValue     => BoolValue(false)
      case _: IntValue      => IntValue(0)
      case _: FloatValue    => FloatValue(0.0)
      case _: StrValue      => StrValue("")
      case _: ArrayValue    => ArrayValue(List.empty)
      case _: MapValue      => MapValue.empty
    }

    val isTruthy: Boolean = value match {
      case NullValue        => false
      case BoolValue(v)     => v
      case IntValue(i)      => i != 0
      case FloatValue(d)    => d != 0.0
      case StrValue(s)      => s.nonEmpty
      case ArrayValue(v)    => v.nonEmpty
      case MapValue(v)      => v.nonEmpty
      case FunctionValue(_) => false
    }

    def contains(key: String): Boolean = value match {
      case NullValue        => false
      case FunctionValue(_) => false
      case BoolValue(v)     => key.toBooleanOption.contains(v)
      case IntValue(v)      => Option(BigInt(key)).contains(v)
      case FloatValue(v)    => Option(BigDecimal(key)).contains(v)
      case StrValue(v)      => key.contains(v)
      case ArrayValue(list) => key.toIntOption.exists(_ <= list.length)
      case MapValue(map)    => map.contains(key)
    }
  }

  implicit lazy val encodeJsonLogicValue: Encoder[JsonLogicValue] = Encoder.instance {
    case NullValue         => Json.Null
    case BoolValue(value)  => Json.fromBoolean(value)
    case IntValue(value)   => Json.fromBigInt(value)
    case FloatValue(value) => Json.fromBigDecimal(value)
    case StrValue(value)   => Json.fromString(value)
    case ArrayValue(value) => Json.fromValues(value.map(encodeJsonLogicValue(_)))
    case MapValue(value)   => Json.obj(value.map { case (k, v) => k -> encodeJsonLogicValue(v) }.toSeq: _*)
    case FunctionValue(_)  => Json.Null
  }

  implicit lazy val decodeJsonLogicValue: Decoder[JsonLogicValue] = Decoder.instance { cursor =>
    cursor.value.fold(
      jsonNull = Right(NullValue),
      jsonBoolean = b => Right(BoolValue(b)),
      jsonNumber = num =>
        num.toBigDecimal match {
          case Some(decimal) =>
            decimal.toBigIntExact match {
              case Some(bi) => Right(IntValue(bi))
              case None     => Right(FloatValue(decimal))
            }
          case None => Left(DecodingFailure(s"Failed to decode number: $num", cursor.history))
        },
      jsonString = s => Right(StrValue(s)),
      jsonArray = _ =>
        cursor.as[List[Json]].flatMap { jsonList =>
          jsonList.traverse(_.as[JsonLogicValue](decodeJsonLogicValue)).map(ArrayValue(_))
        },
      jsonObject = _ =>
        cursor.as[Map[String, Json]].flatMap { jsonMap =>
          jsonMap.toList.traverse { case (k, v) => v.as[JsonLogicValue](decodeJsonLogicValue).map(k -> _) }
            .map(pairs => MapValue(pairs.toMap))
        }
    )
  }
}

object JsonLogicCollection {

  implicit val encodeJsonLogicCollection: Encoder[JsonLogicCollection] = Encoder.instance {
    JsonLogicValue.encodeJsonLogicValue(_)
  }

  implicit val decodeJsonLogicCollection: Decoder[JsonLogicCollection] =
    Decoder.instance[JsonLogicCollection] { c =>
      c.as[JsonLogicValue].flatMap {
        case coll: JsonLogicCollection => Right(coll)
        case other                     => Left(DecodingFailure(s"Expected a collection (map or array), but got $other", c.history))
      }
    }
}

object MapValue {
  val empty: MapValue = new MapValue(Map.empty)
}

object IntValue {

  implicit class IntValueOps(iv: IntValue) {
    def asFloatValue: FloatValue = FloatValue(BigDecimal(iv.value))
  }
}

object CoercedValue {

  @tailrec
  def coerceToPrimitive(value: JsonLogicValue): Either[JsonLogicException, CoercedValue] =
    value match {
      case NullValue     => CoercedNull.asRight
      case BoolValue(b)  => CoercedBool(b).asRight
      case IntValue(i)   => CoercedInt(i).asRight
      case FloatValue(d) => CoercedFloat(d).asRight
      case StrValue(s)   =>
        // JavaScript semantics: empty string coerces to 0, numeric strings coerce to their numeric value
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
      case (CoercedInt(li), CoercedString(rs))    => safeParseBigInt(rs).exists(_ == li).asRight
      case (CoercedString(ls), CoercedInt(ri))    => safeParseBigInt(ls).exists(_ == ri).asRight
      case (CoercedFloat(li), CoercedString(rs))  => safeParseBigDecimal(rs).exists(_ == li).asRight
      case (CoercedString(ls), CoercedFloat(ri))  => safeParseBigDecimal(ls).exists(_ == ri).asRight
      case (CoercedString(ls), CoercedString(rs)) => (ls == rs).asRight
      case _                                      => JsonLogicException(s"Cannot compare coerced values $l and $r").asLeft
    }
}
