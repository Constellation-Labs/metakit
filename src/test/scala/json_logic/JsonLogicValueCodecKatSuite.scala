package json_logic

import cats.syntax.eq._

import io.constellationnetwork.metagraph_sdk.json_logic.core._

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite

/**
 * Wire-format KAT for `JsonLogicValue` — the JLVM value type and the shape of all committed JLVM
 * state. Its codec is a JSON-IDENTITY codec: a `BoolValue` IS a bare JSON `true`, a `MapValue` IS a
 * bare JSON object, etc. — NOT a tagged/discriminated encoding like `{"BoolValue": {...}}`. That
 * identity is load-bearing (it is what makes JLVM state hash + round-trip as plain JSON), so this KAT
 * pins it: each variant encodes to its bare JSON form and `decode(encode(x)) === x`. If someone ever
 * "derives" this sealed trait (which would tag it), these assertions fail before it can change a
 * committed state hash.
 */
object JsonLogicValueCodecKatSuite extends SimpleIOSuite {

  // Encode via the sealed-trait Encoder (the concrete variants have no Encoder of their own).
  private def enc(v: JsonLogicValue): Json = v.asJson

  pureTest("JsonLogicValue encodes as bare JSON (identity codec), not a tagged ADT") {
    expect(enc(NullValue) == Json.Null)
      .and(expect(enc(BoolValue(true)) == Json.True))
      .and(expect(enc(IntValue(BigInt(5))) == Json.fromInt(5)))
      .and(expect(enc(StrValue("x")) == Json.fromString("x")))
      .and(expect(enc(ArrayValue(List(IntValue(1), BoolValue(false)))) == Json.arr(Json.fromInt(1), Json.False)))
      .and(expect(enc(MapValue(Map("k" -> BoolValue(true)))) == Json.obj("k" -> Json.True)))
  }

  pureTest("JsonLogicValue round-trips decode(encode(x)) === x for each data variant") {
    val values: List[JsonLogicValue] = List(
      NullValue,
      BoolValue(true),
      BoolValue(false),
      IntValue(BigInt(42)),
      FloatValue(BigDecimal("0.5")),
      StrValue("hello"),
      ArrayValue(List(IntValue(1), StrValue("a"), BoolValue(true))),
      MapValue(Map("a" -> IntValue(1), "b" -> ArrayValue(List(NullValue))))
    )
    values.foldLeft(success) { (acc, v) =>
      acc.and(expect(decode[JsonLogicValue](enc(v).noSpaces).exists(_ === v)))
    }
  }
}
