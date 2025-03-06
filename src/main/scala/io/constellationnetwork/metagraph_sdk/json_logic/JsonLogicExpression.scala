package io.constellationnetwork.metagraph_sdk.json_logic

import cats.syntax.either._
import cats.syntax.traverse._

import io.circe._
import io.circe.syntax.EncoderOps

sealed trait JsonLogicExpression

final case class ConstExpression(value: JsonLogicValue) extends JsonLogicExpression
final case class ArrayExpression(value: List[JsonLogicExpression]) extends JsonLogicExpression
final case class ApplyExpression(op: JsonLogicOp, args: List[JsonLogicExpression]) extends JsonLogicExpression

final case class VarExpression(value: Either[String, JsonLogicExpression], default: Option[JsonLogicValue] = None)
    extends JsonLogicExpression

object JsonLogicExpression {

  final case class ArgParser(args: List[JsonLogicExpression])

  implicit val encodeOperatorArgs: Encoder[ArgParser] = Encoder.instance { opArgs =>
    if (opArgs.args.size == 1) opArgs.args.head.asJson
    else opArgs.args.asJson
  }

  implicit val decodeOperatorArgs: Decoder[ArgParser] = Decoder.instance { cursor =>
    cursor.as[List[JsonLogicExpression]].map(ArgParser) orElse
    cursor.as[JsonLogicExpression].map(v => ArgParser(List(v)))
  }

  implicit val encodeJsonLogicExpr: Encoder[JsonLogicExpression] = {
    case e: VarExpression          => e.asJson
    case ConstExpression(value)    => value.asJson
    case ArrayExpression(list)     => Json.fromValues(list.map(encodeJsonLogicExpr(_)))
    case ApplyExpression(op, args) => Json.obj((op.tag, Json.fromValues(args.map(encodeJsonLogicExpr(_)))))
  }

  implicit val decodeJsonLogicExpr: Decoder[JsonLogicExpression] = (c: HCursor) =>
    c.value.fold(
      jsonNull = ConstExpression(NullValue).asRight,
      jsonBoolean = bool => ConstExpression(BoolValue(bool)).asRight,
      jsonNumber = num =>
        num.toBigDecimal match {
          case Some(num) =>
            num.toBigIntExact match {
              case Some(bigInt) => ConstExpression(IntValue(bigInt)).asRight
              case None         => ConstExpression(FloatValue(num)).asRight
            }
          case None => Left(DecodingFailure(s"Failed to decode number: $num", c.history))
        },
      jsonString = str => ConstExpression(StrValue(str)).asRight,
      jsonArray = arr =>
        arr
          .traverse(_.as[JsonLogicExpression](decodeJsonLogicExpr))
          .map(v => ArrayExpression(v.toList))
          .orElse(arr.traverse(a => a.as[JsonLogicValue]).map(v => ConstExpression(ArrayValue(v.toList)))),
      jsonObject = { obj =>
        obj.toList match {
          case Nil                  => ConstExpression(MapValue.empty).asRight
          case ("", json) :: Nil    => json.as[VarExpression]
          case ("var", json) :: Nil => json.as[VarExpression]
          case (opStr, json) :: Nil if JsonLogicOp.knownOperatorTags.contains(opStr) =>
            json.as[ArgParser].map(ap => ApplyExpression(JsonLogicOp.knownOperatorTags(opStr), ap.args))
          case _ => obj.asJson.as[JsonLogicValue].map(ConstExpression(_))
        }
      }
    )

  implicit class exprOps(expr: JsonLogicExpression) {
    def bind(input: JsonLogicValue): (JsonLogicExpression, JsonLogicValue) = expr -> input
  }

  implicit class listProgramOps(exprs: List[JsonLogicExpression]) {
    def bind(inputs: List[JsonLogicValue]): List[(JsonLogicExpression, JsonLogicValue)] = exprs.zip(inputs)
  }
}

object VarExpression {

  implicit val encodeVarExpr: Encoder[VarExpression] = (v: VarExpression) => {
    val jsonKey: Json = v.value match {
      case Left(str)   => Json.fromString(str)
      case Right(expr) => expr.asJson
    }

    val varValue: Json = v.default match {
      case None             => jsonKey
      case Some(defaultVal) => Json.arr(jsonKey, defaultVal.asJson)
    }

    Json.obj("var" -> varValue)
  }

  implicit val decodeVarExpr: Decoder[VarExpression] = (c: HCursor) => {

    def parseKey(json: Json): Decoder.Result[Either[String, JsonLogicExpression]] =
      json
        .as[String]
        .orElse(json.as[JsonNumber].map(_.toString))
        .map(_.asLeft[JsonLogicExpression])
        .orElse(json.as[JsonLogicExpression].map(_.asRight[String]))

    // Try single-argument: string, number, or expression
    val singleTry: Decoder.Result[VarExpression] =
      parseKey(c.value).map(v => VarExpression(v, None))

    // Try array form: [key/expr, default]
    val arrayTry: Decoder.Result[VarExpression] =
      c.as[List[Json]].flatMap {
        case Nil      => DecodingFailure("`var` array cannot be empty", c.history).asLeft
        case k :: Nil => parseKey(k).map(v => VarExpression(v, None))
        case k :: d :: _ =>
          for {
            v       <- parseKey(k)
            default <- d.as[JsonLogicValue]
          } yield VarExpression(v, Some(default))
      }

    arrayTry.orElse(singleTry)
  }
}
