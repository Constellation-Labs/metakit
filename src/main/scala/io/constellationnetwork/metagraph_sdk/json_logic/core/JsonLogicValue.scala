package io.constellationnetwork.metagraph_sdk.json_logic.core

import cats.Functor
import cats.syntax.traverse._

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

  implicit class JsonLogicValueOps[Result[_], A <: JsonLogicValue](r: Result[A])(implicit F: Functor[Result]) {
    def widenToJsonLogicValue: Result[JsonLogicValue] = F.map(r)(identity: A => JsonLogicValue)
  }

  implicit def widenJsonLogicValue[Result[_], A <: JsonLogicValue](r: Result[A])(implicit F: Functor[Result]): Result[JsonLogicValue] =
    F.map(r)(identity: A => JsonLogicValue)

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
