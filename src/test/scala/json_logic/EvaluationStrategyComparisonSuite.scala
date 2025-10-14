package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic._

import io.circe.parser
import weaver.SimpleIOSuite

object EvaluationStrategyComparisonSuite extends SimpleIOSuite {

  def parseTestJson(expr: String, dataStr: String): IO[(JsonLogicExpression, JsonLogicValue)] = for {
    expr <- IO.fromEither(parser.parse(expr).flatMap(_.as[JsonLogicExpression]))
    data <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
  } yield (expr, data)

  private def testBothStrategies(
    exprStr: String,
    dataStr: String
  ): IO[Boolean] = {
    for {
      expr <- IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))

      recursiveResult <- JsonLogicEvaluator.recursive[IO].evaluate(expr, data, None).attempt
      tailRecResult   <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None).attempt
    } yield recursiveResult == tailRecResult
  }

  test("simple if/else with lazy evaluation: both strategies should produce same result") {
    val exprStr = """{"if":[{"==":[{"var":"method"},"create"]}, "created", "other"]}"""
    val dataStr = """{"method": "create"}"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("if with count in unexecuted branch: both strategies should produce same result") {
    val exprStr = """
      {"if":[
        {"==":[{"var":"method"},"create"]},
        {"and":[
          {"exists":[{"var":"title"},{"var":"options"}]},
          {">":[{"count":[{"var":"options"}]},1]}
        ]},

        {"==":[{"var":"method"},"vote"]},
        {"var":"voter"}
      ]}
    """
    val dataStr = """{"method": "vote", "voter": "Alice", "content": {"options": ["A", "B"]}}"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("nested if/else: both strategies should produce same result") {
    val exprStr = """
      {"if" : [
        {"<": [{"var":"temp"}, 0] },
        "freezing",

        {"<": [{"var":"temp"}, 100] },
        "liquid",

        "gas"
      ]}
    """
    val dataStr = """{"temp":55}"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("if without else clause: both strategies should return null") {
    val exprStr = """{"if":[false, "yes"]}"""
    val dataStr = """null"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("if/else-if without final else: both strategies should return null when no condition matches") {
    val exprStr = """{"if":[{"==":[1, 2]}, "first", {"==":[3, 4]}, "second"]}"""
    val dataStr = """null"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("complex fizzbuzz with lazy evaluation: both strategies should produce same result") {
    val exprStr = """
      {
        "map": [
          { "var": "list" },
          {
            "if": [
              { "==": [{ "%": [{ "var": "" }, 15] }, 0] },
              "fizzbuzz",

              { "==": [{ "%": [{ "var": "" }, 3] }, 0] },
              "fizz",

              { "==": [{ "%": [{ "var": "" }, 5] }, 0] },
              "buzz",

              { "var": "" }
            ]
          }
        ]
      }
    """
    val dataStr = """{"list": [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]}"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("count operation: both strategies should produce same result") {
    val exprStr = """{"count":[{"var":"options"}]}"""
    val dataStr = """{"options": ["A", "B", "C"]}"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("count with predicate: both strategies should produce same result") {
    val exprStr = """
      {"count": [
        [1, 2, 3, 4, 5],
        {">":[{"var":""}, 2]}
      ]}
    """
    val dataStr = """null"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("reduce operation: both strategies should produce same result") {
    val exprStr = """
      {"reduce":[
        {"var":"integers"},
        {"+":[{"var":"current"}, {"var":"accumulator"}]},
        0
      ]}
    """
    val dataStr = """{"integers":[1,2,3,4,5]}"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }

  test("map operation: both strategies should produce same result") {
    val exprStr = """
      {"map":[
        {"var":"integers"},
        {"*":[{"var":""},2]}
      ]}
    """
    val dataStr = """{"integers":[1,2,3,4,5]}"""

    testBothStrategies(exprStr, dataStr).map(expect(_))
  }
}