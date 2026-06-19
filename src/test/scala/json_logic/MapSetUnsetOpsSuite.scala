package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

/**
 * Conformance + behavior tests for the JLVM map opcodes `set` and `unset`.
 *
 *   - `set [map, key, value]` (arity 3): returns a NEW map = input with `key` -> `value`. If `key`
 *     is already present its value is REPLACED (last-wins, same spirit as merge); otherwise the
 *     entry is ADDED. The input map is never mutated.
 *   - `unset [map, key]` (arity 2): returns a NEW map = input WITHOUT `key`. An absent key is a
 *     no-op (returns the map unchanged, NOT an error). The input map is never mutated.
 *
 * 1st arg MUST be a map, 2nd MUST be a string (the key); `set`'s 3rd arg is any value. Wrong arity
 * / non-map / non-string key -> JsonLogicException. The canonical vectors below are the
 * cross-language math facts (identical across the Scala / Rust / TS evaluators). Maps canonicalize
 * by sorted keys for hashing, so output key-order does not affect consensus; expected values are
 * asserted by [[JsonLogicValue]] structural equality, which is order-independent for maps.
 *
 * Each value vector is exercised end-to-end through the evaluator; immutability and the
 * replace/no-op semantics are additionally asserted directly at the value level.
 */
object MapSetUnsetOpsSuite extends SimpleIOSuite {

  private def evalExpr(exprJson: String, dataJson: String = "{}"): IO[Either[JsonLogicException, JsonLogicValue]] =
    for {
      expr <- IO.fromEither(parser.parse(exprJson).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse(dataJson).flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield out

  private def m(entries: (String, JsonLogicValue)*): JsonLogicValue = MapValue(entries.toMap)

  // ===========================================================================
  // `set` — canonical value vectors (end-to-end through the evaluator).
  // ===========================================================================

  private val setVectors: List[(String, String, JsonLogicValue)] = List(
    ("""{"set":[{},"a",1]}""", "{}", m("a" -> IntValue(1))),
    ("""{"set":[{"a":1},"b",2]}""", "{}", m("a" -> IntValue(1), "b" -> IntValue(2))),
    // replace existing key (last-wins)
    ("""{"set":[{"a":1,"b":2},"a",9]}""", "{}", m("a" -> IntValue(9), "b" -> IntValue(2))),
    // computed key + value
    ("""{"set":[{},{"var":"k"},{"var":"v"}]}""", """{"k":"x","v":5}""", m("x" -> IntValue(5))),
    // array value
    ("""{"set":[{"a":1},"b",[1,2]]}""", "{}", m("a" -> IntValue(1), "b" -> ArrayValue(List(IntValue(1), IntValue(2))))),
    // nested map value
    ("""{"set":[{"a":1},"b",{"c":3}]}""", "{}", m("a" -> IntValue(1), "b" -> m("c" -> IntValue(3))))
  )

  setVectors.foreach {
    case (exprJson, dataJson, expected) =>
      test(s"set: $exprJson with data $dataJson == ${expected}") {
        evalExpr(exprJson, dataJson).map(r => expect(r == Right(expected)))
      }
  }

  // ===========================================================================
  // `unset` — canonical value vectors (end-to-end through the evaluator).
  // ===========================================================================

  private val unsetVectors: List[(String, String, JsonLogicValue)] = List(
    ("""{"unset":[{"a":1,"b":2},"a"]}""", "{}", m("b" -> IntValue(2))),
    // absent key == no-op (returns the map unchanged, not an error)
    ("""{"unset":[{"a":1},"z"]}""", "{}", m("a" -> IntValue(1))),
    // remove the last key -> empty map
    ("""{"unset":[{"a":1},"a"]}""", "{}", MapValue.empty),
    // computed key
    ("""{"unset":[{"a":1,"b":2},{"var":"k"}]}""", """{"k":"a"}""", m("b" -> IntValue(2)))
  )

  unsetVectors.foreach {
    case (exprJson, dataJson, expected) =>
      test(s"unset: $exprJson with data $dataJson == ${expected}") {
        evalExpr(exprJson, dataJson).map(r => expect(r == Right(expected)))
      }
  }

  // ===========================================================================
  // Integration: the motivating use — add a voter to a voters map keyed by agent address.
  // ===========================================================================

  test("set: add a voter to the voters map (motivating use)") {
    val exprJson = """{"set":[{"var":"voters"},{"var":"agent"},true]}"""
    val dataJson = """{"voters":{"0xaaa":true},"agent":"0xbbb"}"""
    val expected = m("0xaaa" -> BoolValue(true), "0xbbb" -> BoolValue(true))
    evalExpr(exprJson, dataJson).map(r => expect(r == Right(expected)))
  }

  // ===========================================================================
  // Immutability + replace/no-op semantics asserted directly at the value level.
  // (The handlers compute `MapValue(m + (k -> v))` / `MapValue(m - k)`, which never mutate input.)
  // ===========================================================================

  test("set: replaces an existing key in place, leaving the original map unmutated") {
    val original = """{"obj":{"a":1,"b":2}}"""
    // set replaces "a"; a follow-up `get` on the SAME original var still sees the old value.
    evalExpr("""{"get":[{"var":"obj"},"a"]}""", original).map(r => expect(r == Right(IntValue(1)))) *>
    evalExpr("""{"set":[{"var":"obj"},"a",9]}""", original).map(r => expect(r == Right(m("a" -> IntValue(9), "b" -> IntValue(2)))))
  }

  test("unset: absent key returns an equal map (no-op, structural equality holds)") {
    evalExpr("""{"unset":[{"a":1,"b":2},"zzz"]}""").map(r => expect(r == Right(m("a" -> IntValue(1), "b" -> IntValue(2)))))
  }

  // ===========================================================================
  // Error cases: must RAISE (a JsonLogicException / Left), not return a value.
  // ===========================================================================

  test("set: rejects a non-map 1st arg (integer 5)") {
    evalExpr("""{"set":[5,"a",1]}""").map(r => expect(r.isLeft))
  }

  test("set: rejects a non-string key (integer 5)") {
    evalExpr("""{"set":[{},5,1]}""").map(r => expect(r.isLeft))
  }

  test("set: rejects wrong arity (2 args, missing value)") {
    evalExpr("""{"set":[{},"a"]}""").map(r => expect(r.isLeft))
  }

  test("set: rejects wrong arity (1 arg)") {
    evalExpr("""{"set":[{}]}""").map(r => expect(r.isLeft))
  }

  test("unset: rejects a non-map 1st arg (integer 5)") {
    evalExpr("""{"unset":[5,"a"]}""").map(r => expect(r.isLeft))
  }

  test("unset: rejects a non-string key (integer 5)") {
    evalExpr("""{"unset":[{},5]}""").map(r => expect(r.isLeft))
  }

  test("unset: rejects wrong arity (1 arg)") {
    evalExpr("""{"unset":[{}]}""").map(r => expect(r.isLeft))
  }
}
