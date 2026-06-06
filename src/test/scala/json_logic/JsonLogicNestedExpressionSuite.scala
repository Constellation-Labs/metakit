package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

/**
 * Nested JSON Logic expression evaluation, focused on array literals that
 * contain variable references and computed sub-expressions.
 *
 * The motivating pattern (the OttoChain "market commitment" append) builds a
 * new array by concatenating an existing array (read from state via `var`)
 * with a one-element array literal whose element is a map constructed from
 * `var`s / computed expressions:
 *
 *   {"merge": [ {"var": "state.commitments"},
 *               [ {"agent": {"var": "event.agent"}, ... } ] ]}
 *
 * Notes on operator choice in this JLVM:
 *   - `cat` is STRING concatenation and errors on collections.
 *   - `merge` is the array concat/flatten op (one level), so wrapping the new
 *     record in `[ ... ]` and merging appends it as a single element.
 *
 * These cases exercise that nested `var`/expression evaluation inside array and
 * map literals happens correctly through `merge`, which the existing suites do
 * not cover (they only `merge` constant arrays).
 */
object JsonLogicNestedExpressionSuite extends SimpleIOSuite with Checkers {

  private def parseTestJson(expr: String, data: String): IO[(JsonLogicExpression, JsonLogicValue)] = for {
    expr <- IO.fromEither(parser.parse(expr).flatMap(_.as[JsonLogicExpression]))
    data <- IO.fromEither(parser.parse(data).flatMap(_.as[JsonLogicValue]))
  } yield (expr, data)

  private def testRunner(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    expected: JsonLogicValue
  ): IO[Expectations] =
    JsonLogicEvaluator
      .tailRecursive[IO]
      .evaluate(expr, data, None)
      .flatMap {
        case Right(result) => IO.pure(expect(result == expected))
        case Left(ex)      => IO.raiseError(ex)
      }

  private def expectError(
    expr: JsonLogicExpression,
    data: JsonLogicValue
  ): IO[Expectations] =
    JsonLogicEvaluator
      .tailRecursive[IO]
      .evaluate(expr, data, None)
      .map {
        case Left(_)  => success
        case Right(r) => failure(s"Expected an error but evaluation succeeded with: $r")
      }

  test("merge appends a literal element built from a var into an existing constant array") {
    val exprStr =
      """
        |{
        |  "merge": [
        |    [1, 2, 3],
        |    [{"var": "event.value"}]
        |  ]
        |}
        |""".stripMargin

    val dataStr = """{"event": {"value": 42}}"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        testRunner(expr, data, ArrayValue(List(IntValue(1), IntValue(2), IntValue(3), IntValue(42))))
    }
  }

  test("merge implements the market-commitment append pattern (existing array + new record)") {
    val exprStr =
      """
        |{
        |  "merge": [
        |    {"var": "state.commitments"},
        |    [{
        |      "agent": {"var": "event.agent"},
        |      "amount": {"var": "event.amount"}
        |    }]
        |  ]
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "state": {
        |    "commitments": [
        |      {"agent": "alice", "amount": 100},
        |      {"agent": "bob", "amount": 200}
        |    ]
        |  },
        |  "event": {"agent": "charlie", "amount": 150}
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        testRunner(
          expr,
          data,
          ArrayValue(
            List(
              MapValue(Map("agent" -> StrValue("alice"), "amount" -> IntValue(100))),
              MapValue(Map("agent" -> StrValue("bob"), "amount" -> IntValue(200))),
              MapValue(Map("agent" -> StrValue("charlie"), "amount" -> IntValue(150)))
            )
          )
        )
    }
  }

  test("merge appends a record whose fields mix var types and a computed expression") {
    val exprStr =
      """
        |{
        |  "merge": [
        |    {"var": "existing_items"},
        |    [{
        |      "id": {"var": "new_item.id"},
        |      "name": {"var": "new_item.name"},
        |      "active": {"var": "new_item.active"},
        |      "computed": {"+": [{"var": "new_item.base"}, 10]}
        |    }]
        |  ]
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "existing_items": [
        |    {"id": 1, "name": "first", "active": true, "computed": 50}
        |  ],
        |  "new_item": {"id": 2, "name": "second", "active": false, "base": 15}
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        testRunner(
          expr,
          data,
          ArrayValue(
            List(
              MapValue(
                Map(
                  "id"       -> IntValue(1),
                  "name"     -> StrValue("first"),
                  "active"   -> BoolValue(true),
                  "computed" -> IntValue(50)
                )
              ),
              MapValue(
                Map(
                  "id"       -> IntValue(2),
                  "name"     -> StrValue("second"),
                  "active"   -> BoolValue(false),
                  "computed" -> IntValue(25)
                )
              )
            )
          )
        )
    }
  }

  test("merge resolves deeply nested var paths inside a record appended to an empty array") {
    val exprStr =
      """
        |{
        |  "merge": [
        |    {"var": "results"},
        |    [{
        |      "user": {
        |        "id": {"var": "payload.user.info.id"},
        |        "profile": {
        |          "name": {"var": "payload.user.info.name"},
        |          "email": {"var": "payload.user.contact.email"}
        |        }
        |      },
        |      "metadata": {
        |        "timestamp": {"var": "payload.meta.created_at"},
        |        "source": {"var": "payload.meta.source"}
        |      }
        |    }]
        |  ]
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "results": [],
        |  "payload": {
        |    "user": {
        |      "info": {"id": "user-123", "name": "Alice Smith"},
        |      "contact": {"email": "alice@example.com"}
        |    },
        |    "meta": {"created_at": 1234567890, "source": "api"}
        |  }
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        testRunner(
          expr,
          data,
          ArrayValue(
            List(
              MapValue(
                Map(
                  "user" -> MapValue(
                    Map(
                      "id" -> StrValue("user-123"),
                      "profile" -> MapValue(
                        Map("name" -> StrValue("Alice Smith"), "email" -> StrValue("alice@example.com"))
                      )
                    )
                  ),
                  "metadata" -> MapValue(
                    Map("timestamp" -> IntValue(1234567890), "source" -> StrValue("api"))
                  )
                )
              )
            )
          )
        )
    }
  }

  test("merge appends a record using a var default for a missing field (standard array-form default)") {
    val exprStr =
      """
        |{
        |  "merge": [
        |    {"var": "state.commitments"},
        |    [{
        |      "agent": {"var": "event.agent"},
        |      "amount": {"var": ["event.missing_field", 0]}
        |    }]
        |  ]
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "state": {"commitments": []},
        |  "event": {"agent": "dave"}
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        testRunner(
          expr,
          data,
          ArrayValue(List(MapValue(Map("agent" -> StrValue("dave"), "amount" -> IntValue(0)))))
        )
    }
  }

  test("merge appends the first record when the existing-commitments array is empty") {
    val exprStr =
      """
        |{
        |  "merge": [
        |    {"var": "state.commitments"},
        |    [{
        |      "agent": {"var": "event.agent"},
        |      "amount": {"var": "event.amount"}
        |    }]
        |  ]
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "state": {"commitments": []},
        |  "event": {"agent": "first_user", "amount": 500}
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        testRunner(
          expr,
          data,
          ArrayValue(List(MapValue(Map("agent" -> StrValue("first_user"), "amount" -> IntValue(500)))))
        )
    }
  }

  test("merge keeps a leading NullValue when the existing-array var path is absent") {
    // `state` has no `commitments` key, so {"var":"state.commitments"} resolves
    // to NullValue. `merge` does not drop nulls: the null is kept as a leading
    // element, and the missing `event.amount` field becomes NullValue.
    val exprStr =
      """
        |{
        |  "merge": [
        |    {"var": "state.commitments"},
        |    [{
        |      "agent": {"var": "event.agent"},
        |      "amount": {"var": "event.amount"}
        |    }]
        |  ]
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "state": {},
        |  "event": {"agent": "user_with_missing_amount"}
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        testRunner(
          expr,
          data,
          ArrayValue(
            List(
              NullValue,
              MapValue(Map("agent" -> StrValue("user_with_missing_amount"), "amount" -> NullValue))
            )
          )
        )
    }
  }

  test("merge appends a record whose fields are computed via nested if/else and arithmetic") {
    val exprStr =
      """
        |{
        |  "merge": [
        |    {"var": "state.items"},
        |    [{
        |      "id": {"var": "new_item.id"},
        |      "status": {
        |        "if": [
        |          {">": [{"var": "new_item.amount"}, 100]},
        |          "premium",
        |          "standard"
        |        ]
        |      },
        |      "computed_fee": {
        |        "*": [
        |          {"var": "new_item.amount"},
        |          {"if": [{"==": [{"var": "new_item.type"}, "vip"]}, 0.05, 0.1]}
        |        ]
        |      }
        |    }]
        |  ]
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "state": {
        |    "items": [{"id": "item-1", "status": "active", "computed_fee": 5.0}]
        |  },
        |  "new_item": {"id": "item-2", "amount": 200, "type": "vip"}
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        testRunner(
          expr,
          data,
          ArrayValue(
            List(
              // 5.0 decodes to an integer because it is a whole number.
              MapValue(Map("id" -> StrValue("item-1"), "status" -> StrValue("active"), "computed_fee" -> IntValue(5))),
              // 200 * 0.05 => 10.00 (BigDecimal product scale).
              MapValue(
                Map(
                  "id"           -> StrValue("item-2"),
                  "status"       -> StrValue("premium"),
                  "computed_fee" -> FloatValue(BigDecimal("10.00"))
                )
              )
            )
          )
        )
    }
  }

  test("cat rejects array arguments (it is string-only), unlike merge") {
    // Documents why the array-append pattern must use `merge`, not `cat`.
    val exprStr =
      """
        |{"cat": [[1, 2, 3], [{"var": "event.value"}]]}
        |""".stripMargin

    val dataStr = """{"event": {"value": 42}}"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        expectError(expr, data)
    }
  }
}
