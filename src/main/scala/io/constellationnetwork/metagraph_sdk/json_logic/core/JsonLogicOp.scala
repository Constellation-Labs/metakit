package io.constellationnetwork.metagraph_sdk.json_logic.core

import enumeratum.{CirceEnum, _}

sealed abstract class JsonLogicOp(val tag: String) extends EnumEntry

object JsonLogicOp extends Enum[JsonLogicOp] with CirceEnum[JsonLogicOp] {
  val values: IndexedSeq[JsonLogicOp] = findValues
  val knownOperatorTags: Map[String, JsonLogicOp] = JsonLogicOp.values.map(op => op.tag -> op).toMap

  // Control Flow
  case object NoOp extends JsonLogicOp("noop")
  case object IfElseOp extends JsonLogicOp("if")
  case object DefaultOp extends JsonLogicOp("default")

  // Logical Operators
  case object NotOp extends JsonLogicOp("!")
  case object NOp extends JsonLogicOp("!!")
  case object OrOp extends JsonLogicOp("or")
  case object AndOp extends JsonLogicOp("and")

  // Comparison Operators
  case object EqOp extends JsonLogicOp("==")
  case object EqStrictOp extends JsonLogicOp("===")
  case object NEqOp extends JsonLogicOp("!=")
  case object NEqStrictOp extends JsonLogicOp("!==")
  case object Lt extends JsonLogicOp("<")
  case object Leq extends JsonLogicOp("<=")
  case object Gt extends JsonLogicOp(">")
  case object Geq extends JsonLogicOp(">=")

  // Arithmetic Operators
  case object AddOp extends JsonLogicOp("+")
  case object MinusOp extends JsonLogicOp("-")
  case object TimesOp extends JsonLogicOp("*")
  case object DivOp extends JsonLogicOp("/")
  case object ModuloOp extends JsonLogicOp("%")
  case object MaxOp extends JsonLogicOp("max")
  case object MinOp extends JsonLogicOp("min")
  case object AbsOp extends JsonLogicOp("abs")
  case object RoundOp extends JsonLogicOp("round")
  case object FloorOp extends JsonLogicOp("floor")
  case object CeilOp extends JsonLogicOp("ceil")
  case object PowOp extends JsonLogicOp("pow")

  // Array Operations
  case object MapOp extends JsonLogicOp("map")
  case object FilterOp extends JsonLogicOp("filter")
  case object ReduceOp extends JsonLogicOp("reduce")
  case object MergeOp extends JsonLogicOp("merge")
  case object AllOp extends JsonLogicOp("all")
  case object SomeOp extends JsonLogicOp("some")
  case object NoneOp extends JsonLogicOp("none")
  case object FindOp extends JsonLogicOp("find")
  case object CountOp extends JsonLogicOp("count")
  case object InOp extends JsonLogicOp("in")
  case object IntersectOp extends JsonLogicOp("intersect")
  case object UniqueOp extends JsonLogicOp("unique")
  case object SliceOp extends JsonLogicOp("slice")
  case object ReverseOp extends JsonLogicOp("reverse")
  case object FlattenOp extends JsonLogicOp("flatten")

  // String Operations
  case object CatOp extends JsonLogicOp("cat")
  case object SubStrOp extends JsonLogicOp("substr")
  case object LowerOp extends JsonLogicOp("lower")
  case object UpperOp extends JsonLogicOp("upper")
  case object JoinOp extends JsonLogicOp("join")
  case object SplitOp extends JsonLogicOp("split")
  case object TrimOp extends JsonLogicOp("trim")
  case object StartsWithOp extends JsonLogicOp("startsWith")
  case object EndsWithOp extends JsonLogicOp("endsWith")

  // Object/Map Operations
  case object MapValuesOp extends JsonLogicOp("values")
  case object MapKeysOp extends JsonLogicOp("keys")
  case object GetOp extends JsonLogicOp("get")
  case object HasOp extends JsonLogicOp("has")
  case object EntriesOp extends JsonLogicOp("entries")

  // Utility Operations
  case object LengthOp extends JsonLogicOp("length")
  case object ExistsOp extends JsonLogicOp("exists")
  case object MissingNoneOp extends JsonLogicOp("missing")
  case object MissingSomeOp extends JsonLogicOp("missing_some")
  case object TypeOfOp extends JsonLogicOp("typeof")
}
