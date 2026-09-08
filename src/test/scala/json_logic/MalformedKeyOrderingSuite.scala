package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas.GasLimit
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

object MalformedKeyOrderingSuite extends SimpleIOSuite {
  // Parsed inputs from the independent review: equal maps, opposite insertion order.
  private val forward = "{\"\\ud800\":1,\"\\ud801\":2}"
  private val reverse = "{\"\\ud801\":2,\"\\ud800\":1}"

  List("keys", "values", "entries").foreach { op =>
    List("tail", "recursive", "tail-gas", "recursive-gas").foreach { mode =>
      test(s"$op $mode: accepted lone-surrogate keys retain distinct raw code units") {
        val expected: JsonLogicValue = op match {
          case "keys"   => ArrayValue(List(StrValue("\ud800"), StrValue("\ud801")))
          case "values" => ArrayValue(List(IntValue(1), IntValue(2)))
          case _ => ArrayValue(List(ArrayValue(List(StrValue("\ud800"), IntValue(1))), ArrayValue(List(StrValue("\ud801"), IntValue(2)))))
        }
        val evaluator = if (mode.startsWith("recursive")) JsonLogicEvaluator.recursive[IO] else JsonLogicEvaluator.tailRecursive[IO]
        def evaluate(expr: JsonLogicExpression, data: JsonLogicValue) =
          if (mode.endsWith("gas")) evaluator.evaluateWithGas(expr, data, None, GasLimit.Default).map(_.map(_.value))
          else evaluator.evaluate(expr, data, None)
        for {
          a       <- IO.fromEither(parser.parse(forward).flatMap(_.as[JsonLogicValue]))
          b       <- IO.fromEither(parser.parse(reverse).flatMap(_.as[JsonLogicValue]))
          expr    <- IO.fromEither(parser.parse(s"""{"$op":[{"var":""}]}""").flatMap(_.as[JsonLogicExpression]))
          aResult <- evaluate(expr, a)
          bResult <- evaluate(expr, b)
        } yield expect.same(a, b).and(expect.same(true, aResult.contains(expected))).and(expect.same(true, bResult.contains(expected)))
      }
    }
  }
}
