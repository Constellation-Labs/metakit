package io.constellationnetwork.metagraph_sdk.json_logic.syntax

import io.constellationnetwork.metagraph_sdk.json_logic.core._

trait ValueSyntax {

  implicit class ValueSyntaxOps(value: JsonLogicValue) {

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

    def isTruthy: Boolean = value match {
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

  implicit class IntValueSyntaxOps(iv: IntValue) {
    def asFloatValue: FloatValue = FloatValue(BigDecimal(iv.value))
  }
}