package io.constellationnetwork.metagraph_sdk.json_logic.core

import cats.syntax.traverse._
import cats.{Eq, Functor, Show}

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

  implicit lazy val eqJsonLogicValue: Eq[JsonLogicValue] = Eq.instance {
    case (NullValue, NullValue)               => true
    case (BoolValue(a), BoolValue(b))         => a == b
    case (IntValue(a), IntValue(b))           => a == b
    case (FloatValue(a), FloatValue(b))       => a == b
    case (StrValue(a), StrValue(b))           => a == b
    case (ArrayValue(a), ArrayValue(b))       => a.length == b.length && a.zip(b).forall { case (x, y) => eqJsonLogicValue.eqv(x, y) }
    case (MapValue(a), MapValue(b))           => a.keySet == b.keySet && a.keys.forall(k => eqJsonLogicValue.eqv(a(k), b(k)))
    case (FunctionValue(a), FunctionValue(b)) => a == b
    case _                                    => false
  }

  implicit lazy val showJsonLogicValue: Show[JsonLogicValue] = Show.show {
    case NullValue        => "null"
    case BoolValue(v)     => v.toString
    case IntValue(v)      => v.toString
    case FloatValue(v)    => v.toString
    case StrValue(v)      => s""""$v""""
    case ArrayValue(vs)   => vs.map(showJsonLogicValue.show).mkString("[", ", ", "]")
    case MapValue(m)      => m.map { case (k, v) => s""""$k": ${showJsonLogicValue.show(v)}""" }.mkString("{", ", ", "}")
    case FunctionValue(_) => "<function>"
  }

  // Typeclass for converting values to JsonLogicValue
  trait ToJLV[A] {
    def apply(a: A): JsonLogicValue
  }

  object ToJLV {
    def apply[A](implicit ev: ToJLV[A]): ToJLV[A] = ev

    implicit val booleanToJLV: ToJLV[Boolean] = BoolValue(_)
    implicit val intToJLV: ToJLV[Int] = i => IntValue(i)
    implicit val longToJLV: ToJLV[Long] = l => IntValue(l)
    implicit val bigIntToJLV: ToJLV[BigInt] = IntValue(_)
    implicit val doubleToJLV: ToJLV[Double] = d => FloatValue(d)
    implicit val bigDecimalToJLV: ToJLV[BigDecimal] = FloatValue(_)
    implicit val stringToJLV: ToJLV[String] = StrValue(_)
    implicit def jlvIdentity[A <: JsonLogicValue]: ToJLV[A] = a => a
  }

  // Construction syntax
  implicit class BooleanToJLV(private val b: Boolean) extends AnyVal {
    def jlv: BoolValue = BoolValue(b)
  }

  implicit class IntToJLV(private val i: Int) extends AnyVal {
    def jlv: IntValue = IntValue(i)
  }

  implicit class LongToJLV(private val l: Long) extends AnyVal {
    def jlv: IntValue = IntValue(l)
  }

  implicit class BigIntToJLV(private val bi: BigInt) extends AnyVal {
    def jlv: IntValue = IntValue(bi)
  }

  implicit class DoubleToJLV(private val d: Double) extends AnyVal {
    def jlv: FloatValue = FloatValue(d)
  }

  implicit class BigDecimalToJLV(private val bd: BigDecimal) extends AnyVal {
    def jlv: FloatValue = FloatValue(bd)
  }

  implicit class StringToJLV(private val s: String) extends AnyVal {
    def jlv: StrValue = StrValue(s)
  }

  implicit class ListToJLV(private val list: List[JsonLogicValue]) extends AnyVal {
    def jlv: ArrayValue = ArrayValue(list)
  }

  implicit class HomogeneousListToJLV[A](private val list: List[A]) extends AnyVal {
    def jlv(implicit ev: ToJLV[A]): ArrayValue = ArrayValue(list.map(ev.apply))
  }

  implicit class MapToJLV(private val map: Map[String, JsonLogicValue]) extends AnyVal {
    def jlv: MapValue = MapValue(map)
  }

  implicit class HomogeneousMapToJLV[A](private val map: Map[String, A]) extends AnyVal {
    def jlv(implicit ev: ToJLV[A]): MapValue = MapValue(map.view.mapValues(ev.apply).toMap)
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

object ArrayValue {
  val empty: ArrayValue = ArrayValue(List.empty)

  def of(values: JsonLogicValue*): ArrayValue = ArrayValue(values.toList)
}

object MapValue {
  val empty: MapValue = MapValue(Map.empty)

  def of(entries: (String, JsonLogicValue)*): MapValue = MapValue(entries.toMap)
}

object IntValue {

  implicit class IntValueOps(iv: IntValue) {
    def asFloatValue: FloatValue = FloatValue(BigDecimal(iv.value))
  }
}
