package io.constellationnetwork.metagraph_sdk.json_logic

import enumeratum.{CirceEnum, _}

sealed abstract class JsonLogicOp(val tag: String) extends EnumEntry

object JsonLogicOp extends Enum[JsonLogicOp] with CirceEnum[JsonLogicOp] {
  val values: IndexedSeq[JsonLogicOp] = findValues
  val knownOperatorTags: Map[String, JsonLogicOp] = JsonLogicOp.values.map(op => op.tag -> op).toMap

  case object NoOp extends JsonLogicOp("noop")
  case object MissingNoneOp extends JsonLogicOp("missing")
  case object MissingSomeOp extends JsonLogicOp("missing_some")
  case object IfElseOp extends JsonLogicOp("if")
  case object EqOp extends JsonLogicOp("==")
  case object EqStrictOp extends JsonLogicOp("===")
  case object NEqOp extends JsonLogicOp("!=")
  case object NEqStrictOp extends JsonLogicOp("!==")
  case object NotOp extends JsonLogicOp("!")
  case object NOp extends JsonLogicOp("!!")
  case object OrOp extends JsonLogicOp("or")
  case object AndOp extends JsonLogicOp("and")
  case object Lt extends JsonLogicOp("<")
  case object Leq extends JsonLogicOp("<=")
  case object Gt extends JsonLogicOp(">")
  case object Geq extends JsonLogicOp(">=")
  case object ModuloOp extends JsonLogicOp("%")
  case object MaxOp extends JsonLogicOp("max")
  case object MinOp extends JsonLogicOp("min")
  case object AddOp extends JsonLogicOp("+")
  case object MinusOp extends JsonLogicOp("-")
  case object TimesOp extends JsonLogicOp("*")
  case object DivOp extends JsonLogicOp("/")
  case object MapOp extends JsonLogicOp("map")
  case object ReduceOp extends JsonLogicOp("reduce")
  case object FilterOp extends JsonLogicOp("filter")
  case object AllOp extends JsonLogicOp("all")
  case object NoneOp extends JsonLogicOp("none")
  case object SomeOp extends JsonLogicOp("some")
  case object MergeOp extends JsonLogicOp("merge")
  case object InOp extends JsonLogicOp("in")
  case object CatOp extends JsonLogicOp("cat")
  case object SubStrOp extends JsonLogicOp("substr")

  case object MapValuesOp extends JsonLogicOp("values")
  case object MapKeysOp extends JsonLogicOp("keys")
  case object GetOp extends JsonLogicOp("get")
  case object ExistsOp extends JsonLogicOp("exists")
  case object IntersectOp extends JsonLogicOp("intersect")
  case object CountOp extends JsonLogicOp("count")
}
