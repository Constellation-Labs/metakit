package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.{Expectations, SimpleIOSuite}

object LetOpSuite extends SimpleIOSuite {

  private def evaluate(jsonLogic: String, data: Option[JsonLogicValue] = None): IO[JsonLogicValue] =
    for {
      expr <- IO.fromEither(parser.parse(jsonLogic).flatMap(_.as[JsonLogicExpression]))
      dataVal = data.getOrElse(NullValue)
      result <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, dataVal, None)
      value  <- IO.fromEither(result)
    } yield value

  private def expectResult(jsonLogic: String, expected: JsonLogicValue, data: Option[JsonLogicValue] = None): IO[Expectations] =
    evaluate(jsonLogic, data).map(result => expect.same(expected, result))

  test("let with single binding") {
    expectResult(
      """{"let": [[["x", 5]], {"var": "x"}]}""",
      IntValue(5)
    )
  }

  test("let with multiple bindings") {
    expectResult(
      """{"let": [[["x", 5], ["y", 10]], {"+": [{"var": "x"}, {"var": "y"}]}]}""",
      IntValue(15)
    )
  }

  test("let with later binding referencing earlier") {
    expectResult(
      """{"let": [[["x", 5], ["y", {"+": [{"var": "x"}, 3]}]], {"var": "y"}]}""",
      IntValue(8)
    )
  }

  test("let with complex expression") {
    expectResult(
      """{
        "let": [
          [
            ["a", 10],
            ["b", 20],
            ["sum", {"+": [{"var": "a"}, {"var": "b"}]}]
          ],
          {"*": [{"var": "sum"}, 2]}
        ]
      }""",
      IntValue(60)
    )
  }

  test("let with external data access") {
    expectResult(
      """{"let": [[["doubled", {"*": [{"var": "value"}, 2]}]], {"+": [{"var": "doubled"}, 5]}]}""",
      IntValue(25),
      Some(MapValue(Map("value" -> IntValue(10))))
    )
  }

  test("let with nested let") {
    expectResult(
      """{
        "let": [
          [["x", 5]],
          {"let": [
            [["y", {"+": [{"var": "x"}, 10]}]],
            {"var": "y"}
          ]}
        ]
      }""",
      IntValue(15)
    )
  }

  test("let with no bindings") {
    expectResult(
      """{"let": [[], 42]}""",
      IntValue(42)
    )
  }

  test("let preserves original context") {
    expectResult(
      """{
        "let": [
          [["x", 100]],
          {"+": [{"var": "x"}, {"var": "original"}]}
        ]
      }""",
      IntValue(150),
      Some(MapValue(Map("original" -> IntValue(50))))
    )
  }

  // --- object form: {"let": [{name: expr, ...}, result]} -------------------
  // Mirrors Rust `eval_let` / the TS evaluator, which accept the convenience
  // object form alongside the array-of-pairs form. Used by the shared vectors.

  test("let object form with single binding") {
    expectResult(
      """{"let": [{"x": 5}, {"var": "x"}]}""",
      IntValue(5)
    )
  }

  test("let object form binding used in result expression") {
    expectResult(
      """{"let": [{"x": 5}, {"+": [{"var": "x"}, 1]}]}""",
      IntValue(6)
    )
  }

  test("let object form binding references outer scope") {
    expectResult(
      """{"let": [{"doubled": {"*": [{"var": "x"}, 2]}}, {"+": [{"var": "doubled"}, 1]}]}""",
      IntValue(11),
      Some(MapValue(Map("x" -> IntValue(5))))
    )
  }

  test("let object form preserves original context") {
    expectResult(
      """{"let": [{"x": 100}, {"+": [{"var": "x"}, {"var": "original"}]}]}""",
      IntValue(150),
      Some(MapValue(Map("original" -> IntValue(50))))
    )
  }

  // --- object form: RFC-8785 sorted-key binding order ----------------------
  // A JSON object has no inherent member order, so object-form `let` evaluates
  // bindings in RFC-8785 sorted-key order (UTF-16 code units) for crypto-determinism,
  // byte-identical with the Rust and TS impls. Each binding sees prior (sorted) ones.

  test("let object form evaluates bindings in sorted-key order (a before b)") {
    // Insertion order has `b` first referencing the not-yet-bound `a`; sorted order
    // binds `a` (=1) first, so `b` = a + 1 = 2. Insertion order would fail on unbound `a`.
    expectResult(
      """{"let": [{"b": {"+": [{"var": "a"}, 1]}, "a": 1}, {"var": "b"}]}""",
      IntValue(2)
    )
  }

  test("let object form sorts keys by UTF-16 code units (non-ASCII)") {
    // 'a' (U+0061) sorts before 'ä' (U+00E4), so a=1 then ä = a + 1 = 2.
    expectResult(
      """{"let": [{"ä": {"+": [{"var": "a"}, 1]}, "a": 1}, {"var": "ä"}]}""",
      IntValue(2)
    )
  }

  test("let array form keeps explicit insertion order (unchanged)") {
    // Array form preserves the listed order: `a` then `b`, b references a.
    expectResult(
      """{"let": [[["a", 1], ["b", {"+": [{"var": "a"}, 1]}]], {"var": "b"}]}""",
      IntValue(2)
    )
  }
}
