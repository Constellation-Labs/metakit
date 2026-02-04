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
      value <- IO.fromEither(result)
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
}
