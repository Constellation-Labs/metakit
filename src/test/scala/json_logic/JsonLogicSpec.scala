package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object JsonLogicSpec extends SimpleIOSuite with Checkers {

  def parseTestJson(expr: String, data: String): IO[(JsonLogicExpression, JsonLogicValue)] = for {
    expr <- IO.fromEither(parser.parse(expr).flatMap(_.as[JsonLogicExpression]))
    data <- IO.fromEither(parser.parse(data).flatMap(_.as[JsonLogicValue]))
  } yield (expr, data)

  private def staticTestRunner(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    expected: JsonLogicValue,
    loggerOpt: Option[JsonLogicValue => IO[Unit]] = None
  ): IO[Expectations] =
    JsonLogicEvaluator
      .tailRecursive[IO]
      .evaluate(expr, data, None)
      .flatMap {
        case Right(result) =>
          loggerOpt match {
            case Some(logger) => logger(result).as(expect(result == expected))
            case None         => IO.pure(expect(result == expected))
          }
        case Left(ex) => IO.raiseError(ex)
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
        case Right(_) => failure("Expected an error but evaluation succeeded")
      }

  test("should evaluate named variable to expected value given array syntax") {
    val exprStr =
      """
        |{ "var" : ["a"] }
        |""".stripMargin

    val dataStr =
      """
        | { "a": 1, "b": 2 }
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(1))
    }
  }

  test("should evaluate named variable to expected value given syntactic sugar") {
    val exprStr =
      """
        |{ "var" : "a" }
        |""".stripMargin

    val dataStr =
      """
        | { "a": 1, "b": 2 }
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(1))
    }
  }

  test("should evaluate named variable to default value when not found") {
    val exprStr =
      """
        |{"var":["z", 26]}
        |""".stripMargin

    val dataStr =
      """
        | { "a": 1, "b": 2 }
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(26))
    }
  }

  test("should evaluate named variable using dot-notation path") {
    val exprStr =
      """
        |{"var" : "champ.name"}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "champ" : {
        |    "name" : "Fezzig",
        |    "height" : 223
        |  },
        |  "challenger" : {
        |    "name" : "Dread Pirate Roberts",
        |    "height" : 183
        |  }
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("Fezzig"))
    }
  }

  test("should retrieve array index from variable") {
    val exprStr =
      """
        |{"var": 1}
        |""".stripMargin

    val dataStr =
      """
        |["zero", "one", "two"]
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("one"))
    }
  }

  test("should be able to compute the value of a variable name") {
    val exprStr =
      """
        |{"var": {"var": "to_get"}}
        |""".stripMargin

    val dataStr =
      """
        |{"to_get": "dynamic", "dynamic": "Got this value"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("Got this value"))
    }
  }

  test("should allow for mixing literals and data") {
    val exprStr =
      """
        |{ "and" : [
        |  {"<" : [ { "var" : "temp" }, 110 ]},
        |  {"==" : [ { "var" : "pie.filling" }, "apple" ] }
        |] }
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "temp" : 100,
        |  "pie" : { "filling" : "apple" }
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("can use dot-notation with missing & missing_some") {
    val exprStr =
      """
        |{"if":[
        |  {"missing": [{"var": "key"}]},
        |  "Found missing",
        |  "OK to proceed"
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "key": "dict.Alice",
        |  "dict": {
        |    "Alice": 2,
        |    "Bob": 3
        |  }
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("OK to proceed"))
    }
  }

  test("var: \"\" should return the entire variable") {
    val exprStr =
      """
        |{ "cat" : [
        |    "Hello, ",
        |    {"var":""}
        |] }
        |""".stripMargin

    val dataStr =
      """
        |"Dolly"
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("Hello, Dolly"))
    }
  }

  test("unary operator can parse single value without array") {
    val exprStr =
      """
        |{"!": true}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("operators can take variables as inputs") {
    val exprStr =
      """
        |{ "<": [0, {"var":"temp"}, 100]}
        |""".stripMargin

    val dataStr =
      """
        |{"temp" : 37}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("operators can take expressions as inputs") {
    val exprStr =
      """
        |{ "<": [0, { "+": [10, {"var":"temp"}]}, 100]}
        |""".stripMargin

    val dataStr =
      """
        |{"temp" : 37}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("missing returns an array of missing keys") {
    val exprStr =
      """
        |{"missing":["a", "b"]}
        |""".stripMargin

    val dataStr =
      """
        |{"a":"apple", "c":"carrot"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(StrValue("b"))))
    }
  }

  test("missing can match all keys") {
    val exprStr =
      """
        |{"missing":["a", "b"]}
        |""".stripMargin

    val dataStr =
      """
        |{"a":"apple", "b":"banana"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List()))
    }
  }

  test("missing can combine with conditional strings to return labels") {
    val exprStr =
      """
        |{"if":[
        |  {"missing":["a", "b"]},
        |  "Not enough fruit",
        |  "OK to proceed"
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{"a":"apple", "b":"banana"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("OK to proceed"))
    }
  }

  test("missing_some can find at least 1 of the given variables") {
    val exprStr =
      """
        |{"missing_some":[1, ["a", "b", "c"]]}
        |""".stripMargin

    val dataStr =
      """
        |{"a":"apple"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List()))
    }
  }

  test("missing_some returns which variables are missing") {
    val exprStr =
      """
        |{"missing_some":[2, ["a", "b", "c"]]}
        |""".stripMargin

    val dataStr =
      """
        |{"a":"apple"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(StrValue("b"), StrValue("c"))))
    }
  }

  test("missing_some and missing can combine and compose with conditional returns") {
    val exprStr =
      """
        |{"if" :[
        |    {"merge": [
        |      {"missing":["first_name", "last_name"]},
        |      {"missing_some":[1, ["cell_phone", "home_phone"] ]}
        |    ]},
        |    "We require first name, last name, and one phone number.",
        |    "OK to proceed"
        |  ]}
        |""".stripMargin

    val dataStr =
      """
        |{"first_name":"Bruce", "last_name":"Wayne"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("We require first name, last name, and one phone number."))
    }
  }

  test("missing_some and missing can combine and compose with conditional returns") {
    val exprStr =
      """
        |{"if" :[
        |    {"merge": [
        |      {"missing":["first_name", "last_name"]},
        |      {"missing_some":[1, ["cell_phone", "home_phone"] ]}
        |    ]},
        |    "We require first name, last name, and one phone number.",
        |    "OK to proceed"
        |  ]}
        |""".stripMargin

    val dataStr =
      """
        |{"first_name":"Bruce", "last_name":"Wayne"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("We require first name, last name, and one phone number."))
    }
  }

  test("simple if/else can return value when true") {
    val exprStr =
      """
        |{"if" : [ true, "yes", "no" ]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("yes"))
    }
  }

  test("simple if/else can return value when false") {
    val exprStr =
      """
        |{"if" : [ false, "yes", "no" ]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("no"))
    }
  }

  test("if/else can handle additional branches") {
    val exprStr =
      """
        |{"if" : [
        |  {"<": [{"var":"temp"}, 0] },
        |  "freezing",
        |
        |  {"<": [{"var":"temp"}, 100] },
        |  "liquid",
        |
        |  "gas"
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{"temp":55}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("liquid"))
    }
  }

  test("'==' can test loose equality between like types (js-like)") {
    val exprStr =
      """
        |{"==" : [1, 1]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("'==' can test loose equality between un-like types (js-like)") {
    val exprStr =
      """
        |{"==" : [1, "1"]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("'==' uses custom 'truthy' definition when testing equality (js-like)") {
    val exprStr =
      """
        |{"==" : [0, false]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("'===' enforces strictly type equality between like types") {
    val exprStr =
      """
        |{"===" : [1, 1]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("'===' enforces strictly type equality between un-like types") {
    val exprStr =
      """
        |{"===" : [1, "1"]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("!= can test not-equal with type coercion for like types") {
    val exprStr =
      """
        |{"!=" : [1, 2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("!= can test not-equal with type coercion for un-like types") {
    val exprStr =
      """
        |{"!=" : [1, "1"]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("!== can test strict not-equal with type coercion for like types") {
    val exprStr =
      """
        |{"!==" : [1, 2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("!== can test strict not-equal with type coercion for un-like types") {
    val exprStr =
      """
        |{"!==" : [1, "1"]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("! can negate a boolean") {
    val exprStr =
      """
        |{"!": [true]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("!! can cast to false") {
    val exprStr =
      """
        |{"!!": [ [] ] }
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("!! can cast to true") {
    val exprStr =
      """
        |{"!!": ["0"] }
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`or` can return truth argument") {
    val exprStr =
      """
        |{"or": [true, false]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`or` can return boolean value when true in first position") {
    val exprStr =
      """
        |{"or": [true, false]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`or` can return true") {
    val exprStr =
      """
        |{"or": [false, true]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`or` returns the first 'truthy' argument of boolean") {
    val exprStr =
      """
        |{"or":[false, true]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`or` returns the first 'truthy' argument of string") {
    val exprStr =
      """
        |{"or":[false, "a"]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("a"))
    }
  }

  test("`or` can evaulate array and return the first 'truthy' argument of string") {
    val exprStr =
      """
        |{"or":[false, 0, "a"]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("a"))
    }
  }

  test("`and` can return true") {
    val exprStr =
      """
        |{"and": [true, true]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`and` can return false") {
    val exprStr =
      """
        |{"and": [true, false]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`and` returns last argument if all truthy") {
    val exprStr =
      """
        |{"and":[true,"a",3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(3))
    }
  }

  test("`and` returns first falsy argument") {
    val exprStr =
      """
        |{"and": [true,"",3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue(""))
    }
  }

  test("`>` tests greater than for two ints") {
    val exprStr =
      """
        |{">" : [2, 1]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`>=` tests greater than or equal for two ints") {
    val exprStr =
      """
        |{">=" : [1, 1]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`<` tests less than for two ints") {
    val exprStr =
      """
        |{"<" : [1, 2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`<=` tests less than or equal for two ints") {
    val exprStr =
      """
        |{"<=" : [1, 1]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`<` can test for exclusive-range with three inputs: (lb, x, ub) => lb < x < ub") {
    val exprStr =
      """
        |{"<" : [1, 2, 3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`<` can lower bound check exclusive-range with three inputs") {
    val exprStr =
      """
        |{"<" : [1, 1, 3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`<` can upper bound check exclusive-range with three inputs") {
    val exprStr =
      """
        |{"<" : [1, 4, 3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`<=` can test for inclusive-range with three inputs: (lb, x, ub) => lb <= x <= ub") {
    val exprStr =
      """
        |{"<=" : [1, 2, 3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`<=` can lower bound check inclusive-range with three inputs") {
    val exprStr =
      """
        |{"<=" : [1, 1, 3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`<=` can upper bound check inclusive-range with three inputs") {
    val exprStr =
      """
        |{"<=" : [1, 4, 3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`max` can find max of array of ints") {
    val exprStr =
      """
        |{"max":[1,2,3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(3))
    }
  }

  test("`min` can find min of array of ints") {
    val exprStr =
      """
        |{"min":[1,2,3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(1))
    }
  }

  test("`+` can add two ints") {
    val exprStr =
      """
        |{"+":[4,2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(6))
    }
  }

  test("`+` can add array of ints") {
    val exprStr =
      """
        |{"+":[2,2,2,2,2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(10))
    }
  }

  test("`+` with a single arg will cast to a number") {
    val exprStr =
      """
        |{"+" : "3.14"}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, FloatValue(3.14))
    }
  }

  test("`*` can multiply two ints") {
    val exprStr =
      """
        |{"*":[4,2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(8))
    }
  }

  test("`*` can multiply array of ints") {
    val exprStr =
      """
        |{"*":[2,2,2,2,2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(32))
    }
  }

  test("`-` can subtract two ints") {
    val exprStr =
      """
        |{"-":[4,2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(2))
    }
  }

  test("`-` can invert a single positive integer to negative") {
    val exprStr =
      """
        |{"-": 2}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(-2))
    }
  }

  test("`-` can invert a single negative integer to positive") {
    val exprStr =
      """
        |{"-": -2}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(2))
    }
  }

  test("`/` can divide two ints") {
    val exprStr =
      """
        |{"/":[4,2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(2))
    }
  }

  test("`%` can find remainder after dividing two ints") {
    val exprStr =
      """
        |{"%": [101,2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(1))
    }
  }

  test("`map` can apply an expr over every element of an array of ints") {
    val exprStr =
      """
        |{"map":[
        |  {"var":"integers"},
        |  {"*":[{"var":""},2]}
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{"integers":[1,2,3,4,5]}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(2), IntValue(4), IntValue(6), IntValue(8), IntValue(10))))
    }
  }

  test("`map` can apply an expr over every element of an array of floats") {
    val exprStr =
      """
        |{"map":[
        |  {"var":"floats"},
        |  {"*":[{"var":""}, 2.0]}
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{"floats":[ 1.1, 2.2, 3.3, 4.4, 5.5 ]}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(
          expr,
          data,
          ArrayValue(List(FloatValue(2.2), FloatValue(4.4), FloatValue(6.6), FloatValue(8.8), FloatValue(11.0)))
        )
    }
  }

  test("`filter` can keep only elements of an array that pass a test") {
    val exprStr =
      """
        |{"filter":[
        |  {"var":"integers"},
        |  {"%":[{"var":""},2]}
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{"integers":[1,2,3,4,5]}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(1), IntValue(3), IntValue(5))))
    }
  }

  test("`reduce` can keep combine all elements of an array") {
    val exprStr =
      """
        |{"reduce":[
        |    {"var":"integers"},
        |    {"+":[{"var":"current"}, {"var":"accumulator"}]},
        |    0
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{"integers":[1,2,3,4,5]}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(15))
    }
  }

  test("`reduce` can use expressions in the initial condition arg") {
    val exprStr =
      """
        |{"reduce":[
        |    {"var":"integers"},
        |    {"+":[{"var":"current"}, {"var":"accumulator"}]},
        |    {"var":"initial"}
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "integers":[1,2,3,4,5],
        |  "initial": 5
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(20))
    }
  }

  test("`all` expects a test to succeed for every element of an array") {
    val exprStr =
      """
        |{"all" : [ [1,2,3], {">":[{"var":""}, 0]} ]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`some` expects a test to succeed for some threshold number of elements") {
    val exprStr =
      """
        |{"some" : [ [-1,0,1], {">":[{"var":""}, 0]} ]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`none` expects a test to fail for every element of an array") {
    val exprStr =
      """
        |{"none" : [ [-3,-2,-1], {">":[{"var":""}, 0]} ]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`some` can be used to test an objects properties") {
    val exprStr =
      """
        |{"some" : [ {"var":"pies"}, {"==":[{"var":"filling"}, "apple"]} ]}
        |""".stripMargin

    val dataStr =
      """
        |{"pies":[
        |  {"filling":"pumpkin","temp":110},
        |  {"filling":"rhubarb","temp":210},
        |  {"filling":"apple","temp":310}
        |]}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`merge` can combine multiple arrays") {
    val exprStr =
      """
        |{"merge":[ [1,2], [3,4] ]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(1), IntValue(2), IntValue(3), IntValue(4))))
    }
  }

  test("`merge` can combine single elements and arrays") {
    val exprStr =
      """
        |{"merge":[ 1, 2, [3,4] ]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(1), IntValue(2), IntValue(3), IntValue(4))))
    }
  }

  test("ops can compose for more complex tests") {
    val exprStr =
      """
        |{"missing" :
        |  { "merge" : [
        |    "vin",
        |    {"if": [{"var":"financing"}, ["apr", "term"], [] ]}
        |  ]}
        |}
        |""".stripMargin

    val dataStr =
      """
        |{"financing":true}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(StrValue("vin"), StrValue("apr"), StrValue("term"))))
    }
  }

  test("`in` can test if a value is in an array") {
    val exprStr =
      """
        |{"in":[ "Ringo", ["John", "Paul", "George", "Ringo"] ]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`in` can test if a string contains a substring") {
    val exprStr =
      """
        |{"in":["Spring", "Springfield"]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`cat` can concatenate two strings together") {
    val exprStr =
      """
        |{"cat": ["I love", " pie"]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("I love pie"))
    }
  }

  test("`cat` can concatenate strings and expressions together") {
    val exprStr =
      """
        |{"cat": ["I love ", {"var":"filling"}, " pie"]}
        |""".stripMargin

    val dataStr =
      """
        |{"filling":"apple", "temp":110}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("I love apple pie"))
    }
  }

  test("`substr` can get a portion of a string with a defined start index") {
    val exprStr =
      """
        |{"substr": ["jsonlogic", 4]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("logic"))
    }
  }

  test("`substr` can take a negative first arg to get a portion of a string") {
    val exprStr =
      """
        |{"substr": ["jsonlogic", -5]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("logic"))
    }
  }

  test("`substr` can take two args (start, length) to get a portion of a string") {
    val exprStr =
      """
        |{"substr": ["jsonlogic", 1, 3]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("son"))
    }
  }

  test("`substr` can take two args to start and specify how many characters to stop before end") {
    val exprStr =
      """
        |{"substr": ["jsonlogic", 4, -2]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("log"))
    }
  }

  test("impl can process fizzbuzz logic accurately") {
    val exprStr =
      """
        |{
        |  "map": [
        |    { "var": "list" },
        |    {
        |      "if": [
        |        { "==": [{ "%": [{ "var": "" }, 15] }, 0] },
        |        "fizzbuzz",
        |
        |        { "==": [{ "%": [{ "var": "" }, 3] }, 0] },
        |        "fizz",
        |
        |        { "==": [{ "%": [{ "var": "" }, 5] }, 0] },
        |        "buzz",
        |
        |        { "var": "" }
        |      ]
        |    }
        |  ]
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "list": [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(
          expr,
          data,
          ArrayValue(
            List(
              IntValue(1),
              IntValue(2),
              StrValue("fizz"),
              IntValue(4),
              StrValue("buzz"),
              StrValue("fizz"),
              IntValue(7),
              IntValue(8),
              StrValue("fizz"),
              StrValue("buzz"),
              IntValue(11),
              StrValue("fizz"),
              IntValue(13),
              IntValue(14),
              StrValue("fizzbuzz"),
              IntValue(16),
              IntValue(17),
              StrValue("fizz"),
              IntValue(19),
              StrValue("buzz"),
              StrValue("fizz"),
              IntValue(22),
              IntValue(23),
              StrValue("fizz"),
              StrValue("buzz"),
              IntValue(26),
              StrValue("fizz"),
              IntValue(28),
              IntValue(29),
              StrValue("fizzbuzz")
            )
          )
        )
    }
  }

  test("division by zero should throw error for integer division") {
    val exprStr = """{"/":[10, 0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        expectError(expr, data)
    }
  }

  test("division by zero should throw error for float division") {
    val exprStr = """{"/":[10.5, 0.0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        expectError(expr, data)
    }
  }

  test("division by zero should throw error for mixed types") {
    val exprStr = """{"/":[10, 0.0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        expectError(expr, data)
    }
  }

  test("modulo by zero should throw error for integer modulo") {
    val exprStr = """{"%":[10, 0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        expectError(expr, data)
    }
  }

  test("modulo by zero should throw error for float modulo") {
    val exprStr = """{"%":[10.5, 0.0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        expectError(expr, data)
    }
  }

  test("array access with negative index should return null") {
    val exprStr = """{"var": -1}"""
    val dataStr = """["zero", "one", "two"]"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, NullValue)
    }
  }

  test("array access with out-of-bounds index should return null") {
    val exprStr = """{"var": 5}"""
    val dataStr = """["zero", "one", "two"]"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, NullValue)
    }
  }

  test("string to number coercion should handle invalid numbers gracefully") {
    val exprStr = """{"==":[1, "not_a_number"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("reduce with empty array and no initial value should return null") {
    val exprStr = """{"reduce":[[], {"+":[{"var":"current"}, {"var":"accumulator"}]}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, NullValue)
    }
  }

  test("reduce with empty array and initial value should return initial value") {
    val exprStr = """{"reduce":[[], {"+":[{"var":"current"}, {"var":"accumulator"}]}, 42]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(42))
    }
  }

  test("reduce with non-empty array and no initial value should use first element") {
    val exprStr = """{"reduce":[[1,2,3,4], {"+":[{"var":"current"}, {"var":"accumulator"}]}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(10)) // 1 + 2 + 3 + 4 = 10
    }
  }

  test("reduce with single element array and no initial value should return that element") {
    val exprStr = """{"reduce":[[42], {"+":[{"var":"current"}, {"var":"accumulator"}]}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(42))
    }
  }

  test("division should work correctly with valid operands") {
    val exprStr = """{"/":[10, 2]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(5))
    }
  }

  test("modulo should work correctly with valid operands") {
    val exprStr = """{"%":[10, 3]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(1))
    }
  }

  test("array access should handle edge cases correctly") {
    val testCases = List(
      ("""{"var": 0}""", """["first", "second"]""", StrValue("first")),
      ("""{"var": 1}""", """["first", "second"]""", StrValue("second")),
      ("""{"var": 2}""", """["first", "second"]""", NullValue),
      ("""{"var": -1}""", """["first", "second"]""", NullValue)
    )

    testCases.foldLeft(IO.pure(success)) {
      case (acc, (exprStr, dataStr, expected)) =>
        acc.flatMap { prevResult =>
          parseTestJson(exprStr, dataStr).flatMap {
            case (expr, data) =>
              staticTestRunner(expr, data, expected).map(result => prevResult.and(result))
          }
        }
    }
  }

  test("type coercion should handle various string formats") {
    val testCases = List(
      ("""{"==":[1, "1"]}""", BoolValue(true)),
      ("""{"==":[0, ""]}""", BoolValue(true)),
      ("""{"==":[1, "not_a_number"]}""", BoolValue(false)),
      ("""{"==":[123, "123abc"]}""", BoolValue(false))
    )

    testCases.foldLeft(IO.pure(success)) {
      case (acc, (exprStr, expected)) =>
        acc.flatMap { prevResult =>
          parseTestJson(exprStr, "null").flatMap {
            case (expr, data) =>
              staticTestRunner(expr, data, expected).map(result => prevResult.and(result))
          }
        }
    }
  }

  test("MapExpression should decode object with nested expressions") {
    val exprStr =
      """
        |{
        |  "initiator": ["var", "machineId"],
        |  "timestamp": 12345
        |}
        |""".stripMargin

    val dataStr =
      """
        |{"machineId": "abc-123"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(
          expr,
          data,
          MapValue(Map("initiator" -> StrValue("abc-123"), "timestamp" -> IntValue(12345)))
        )
    }
  }

  test("MapExpression should evaluate nested expressions correctly") {
    val exprStr =
      """
        |{
        |  "sum": ["+", ["var", "a"], ["var", "b"]],
        |  "product": ["*", ["var", "a"], ["var", "b"]],
        |  "constant": 42
        |}
        |""".stripMargin

    val dataStr =
      """
        |{"a": 5, "b": 3}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(
          expr,
          data,
          MapValue(Map("sum" -> IntValue(8), "product" -> IntValue(15), "constant" -> IntValue(42)))
        )
    }
  }

  test("MapExpression should handle all constant values as ConstExpression(MapValue)") {
    val exprStr =
      """
        |{
        |  "name": "John",
        |  "age": 30,
        |  "active": true
        |}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        // Should decode as ConstExpression(MapValue(...)) since no expressions present
        staticTestRunner(
          expr,
          data,
          MapValue(Map("name" -> StrValue("John"), "age" -> IntValue(30), "active" -> BoolValue(true)))
        )
    }
  }

  test("MapExpression should handle deeply nested expressions") {
    val exprStr =
      """
        |{
        |  "result": {
        |    "value": ["+", 1, 2]
        |  }
        |}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, MapValue(Map("result" -> MapValue(Map("value" -> IntValue(3))))))
    }
  }

  test("MapExpression should encode and decode round-trip correctly") {
    val exprStr =
      """
        |{
        |  "computed": ["var", "x"],
        |  "static": 100
        |}
        |""".stripMargin

    import io.circe.syntax._
    import io.circe.parser

    for {
      expr <- IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))
      json = expr.asJson
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield expect(decoded == expr)
  }

  test("MapExpression should handle empty objects") {
    val exprStr = """{}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, MapValue.empty)
    }
  }

  test("MapExpression with mixed expression and constant fields") {
    val exprStr =
      """
        |{
        |  "userId": ["var", "uid"],
        |  "action": "create",
        |  "timestamp": ["var", "ts"],
        |  "version": 1
        |}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "uid": "user-456",
        |  "ts": 1609459200
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(
          expr,
          data,
          MapValue(
            Map(
              "userId"    -> StrValue("user-456"),
              "action"    -> StrValue("create"),
              "timestamp" -> IntValue(1609459200),
              "version"   -> IntValue(1)
            )
          )
        )
    }
  }

  test("`count` can count elements in an array") {
    val exprStr =
      """
        |{"count": [[1, 2, 3, 4, 5]]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(5))
    }
  }

  test("`count` can count elements from a variable array") {
    val exprStr =
      """
        |{"count": [{"var": "items"}]}
        |""".stripMargin

    val dataStr =
      """
        |{"items": [10, 20, 30]}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(3))
    }
  }

  test("`count` can count elements matching a predicate") {
    val exprStr =
      """
        |{"count": [
        |  [1, 2, 3, 4, 5],
        |  {">":[{"var":""}, 2]}
        |]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(3))
    }
  }

  test("`count` can count elements in empty array") {
    val exprStr =
      """
        |{"count": [[]]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(0))
    }
  }

  test("`count` with predicate returns zero when no elements match") {
    val exprStr =
      """
        |{"count": [
        |  [1, 2, 3],
        |  {">":[{"var":""}, 10]}
        |]}
        |""".stripMargin

    val dataStr =
      """
        |null
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(0))
    }
  }

  test("`count` can count complex objects matching a predicate") {
    val exprStr =
      """
        |{"count": [
        |  {"var": "users"},
        |  {"==":[{"var":"status"}, "active"]}
        |]}
        |""".stripMargin

    val dataStr =
      """
        |{
        |  "users": [
        |    {"name": "Alice", "status": "active"},
        |    {"name": "Bob", "status": "inactive"},
        |    {"name": "Charlie", "status": "active"}
        |  ]
        |}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(2))
    }
  }

  test("simple count with var lookup") {
    val exprStr = """{"count":[{"var":"options"}]}"""
    val dataStr = """{"options": ["A", "B", "C"]}"""

    for {
      expr   <- IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))
      data   <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      either <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
      result <- IO.fromEither(either)
    } yield expect(result == IntValue(3))
  }

  test("count inside and operation") {
    val exprStr = """{"and":[true, {"count":[{"var":"options"}]}]}"""
    val dataStr = """{"options": ["A", "B", "C"]}"""

    for {
      expr   <- IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))
      data   <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      either <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
      result <- IO.fromEither(either)
    } yield expect(result == IntValue(3))
  }

  test("count with comparison") {
    val exprStr = """{">":[{"count":[{"var":"options"}]}, 1]}"""
    val dataStr = """{"options": ["A", "B", "C"]}"""

    for {
      expr   <- IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))
      data   <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      either <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
      result <- IO.fromEither(either)
    } yield expect(result == BoolValue(true))
  }

  test("if with count in one branch - evaluates other branch") {
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

    for {
      expr   <- IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))
      data   <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      either <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
      result <- IO.fromEither(either)
    } yield expect(result == StrValue("Alice"))
  }

  test("`length` can get the length of an array") {
    val exprStr =
      """
        |{"length": [[1, 2, 3, 4, 5]]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(5))
    }
  }

  test("`length` can get the length of a string") {
    val exprStr =
      """
        |{"length": ["hello"]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(5))
    }
  }

  test("`find` can find first element matching a predicate") {
    val exprStr =
      """
        |{"find": [
        |  [1, 2, 3, 4, 5],
        |  {">":[{"var":""}, 3]}
        |]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(4))
    }
  }

  test("`find` returns null when no element matches") {
    val exprStr =
      """
        |{"find": [
        |  [1, 2, 3],
        |  {">":[{"var":""}, 10]}
        |]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, NullValue)
    }
  }

  test("`lower` can convert string to lowercase") {
    val exprStr =
      """
        |{"lower": ["HELLO WORLD"]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("hello world"))
    }
  }

  test("`upper` can convert string to uppercase") {
    val exprStr =
      """
        |{"upper": ["hello world"]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("HELLO WORLD"))
    }
  }

  test("`join` can join array elements with separator") {
    val exprStr =
      """
        |{"join": [["a", "b", "c"], ","]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("a,b,c"))
    }
  }

  test("`join` can join numeric array elements") {
    val exprStr =
      """
        |{"join": [[1, 2, 3], "-"]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("1-2-3"))
    }
  }

  test("`split` can split string by separator") {
    val exprStr =
      """
        |{"split": ["a,b,c", ","]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(StrValue("a"), StrValue("b"), StrValue("c"))))
    }
  }

  test("`split` preserves empty strings") {
    val exprStr =
      """
        |{"split": ["a,,c", ","]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(StrValue("a"), StrValue(""), StrValue("c"))))
    }
  }

  test("`default` returns first non-null truthy value") {
    val exprStr =
      """
        |{"default": [null, false, 0, "hello"]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("hello"))
    }
  }

  test("`default` returns null when all values are null or falsy") {
    val exprStr =
      """
        |{"default": [null, false, 0]}
        |""".stripMargin

    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, NullValue)
    }
  }

  test("new operations can compose together") {
    val exprStr =
      """
        |{"length": {"split": [{"upper": [{"var": "name"}]}, " "]}}
        |""".stripMargin

    val dataStr =
      """
        |{"name": "john doe"}
        |""".stripMargin

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(2))
    }
  }

  test("`unique` can remove duplicate elements from an array") {
    val exprStr = """{"unique": [[1, 2, 2, 3, 1, 4, 3]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(1), IntValue(2), IntValue(3), IntValue(4))))
    }
  }

  test("`unique` preserves order of first occurrence") {
    val exprStr = """{"unique": [["a", "b", "a", "c", "b"]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(StrValue("a"), StrValue("b"), StrValue("c"))))
    }
  }

  test("`unique` handles empty array") {
    val exprStr = """{"unique": [[]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List()))
    }
  }

  test("`slice` can extract portion of array with start index") {
    val exprStr = """{"slice": [[1, 2, 3, 4, 5], 2]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(3), IntValue(4), IntValue(5))))
    }
  }

  test("`slice` can extract portion of array with start and end indices") {
    val exprStr = """{"slice": [[1, 2, 3, 4, 5], 1, 4]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(2), IntValue(3), IntValue(4))))
    }
  }

  test("`slice` supports negative start index") {
    val exprStr = """{"slice": [[1, 2, 3, 4, 5], -2]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(4), IntValue(5))))
    }
  }

  test("`slice` supports negative end index") {
    val exprStr = """{"slice": [[1, 2, 3, 4, 5], 1, -1]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(2), IntValue(3), IntValue(4))))
    }
  }

  test("`reverse` can reverse an array") {
    val exprStr = """{"reverse": [[1, 2, 3, 4, 5]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(5), IntValue(4), IntValue(3), IntValue(2), IntValue(1))))
    }
  }

  test("`reverse` handles empty array") {
    val exprStr = """{"reverse": [[]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List()))
    }
  }

  test("`flatten` can flatten nested arrays one level") {
    val exprStr = """{"flatten": [[[1, 2], [3, 4], [5]]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(1), IntValue(2), IntValue(3), IntValue(4), IntValue(5))))
    }
  }

  test("`flatten` does not recursively flatten") {
    val exprStr = """{"flatten": [[[1, [2, 3]], [4, 5]]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(
          expr,
          data,
          ArrayValue(List(IntValue(1), ArrayValue(List(IntValue(2), IntValue(3))), IntValue(4), IntValue(5)))
        )
    }
  }

  test("`flatten` handles array with non-array elements") {
    val exprStr = """{"flatten": [[1, [2, 3], 4, [5]]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(1), IntValue(2), IntValue(3), IntValue(4), IntValue(5))))
    }
  }

  test("`trim` can remove leading and trailing whitespace") {
    val exprStr = """{"trim": ["  hello world  "]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("hello world"))
    }
  }

  test("`trim` handles string with no whitespace") {
    val exprStr = """{"trim": ["hello"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("hello"))
    }
  }

  test("`trim` preserves internal whitespace") {
    val exprStr = """{"trim": ["  hello   world  "]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("hello   world"))
    }
  }

  test("`startsWith` returns true when string starts with prefix") {
    val exprStr = """{"startsWith": ["hello world", "hello"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`startsWith` returns false when string does not start with prefix") {
    val exprStr = """{"startsWith": ["hello world", "world"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`startsWith` is case-sensitive") {
    val exprStr = """{"startsWith": ["hello world", "Hello"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`endsWith` returns true when string ends with suffix") {
    val exprStr = """{"endsWith": ["hello world", "world"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`endsWith` returns false when string does not end with suffix") {
    val exprStr = """{"endsWith": ["hello world", "hello"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`endsWith` is case-sensitive") {
    val exprStr = """{"endsWith": ["hello world", "World"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`abs` can get absolute value of positive integer") {
    val exprStr = """{"abs": [5]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(5))
    }
  }

  test("`abs` can get absolute value of negative integer") {
    val exprStr = """{"abs": [-5]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(5))
    }
  }

  test("`abs` can get absolute value of negative float") {
    val exprStr = """{"abs": [-3.14]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, FloatValue(3.14))
    }
  }

  test("`round` can round float to nearest integer") {
    val exprStr = """{"round": [3.6]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(4))
    }
  }

  test("`round` uses HALF_UP rounding") {
    val exprStr = """{"round": [2.5]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(3))
    }
  }

  test("`round` handles negative numbers") {
    val exprStr = """{"round": [-2.7]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(-3))
    }
  }

  test("`floor` can get floor of positive float") {
    val exprStr = """{"floor": [3.9]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(3))
    }
  }

  test("`floor` can get floor of negative float") {
    val exprStr = """{"floor": [-2.1]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(-3))
    }
  }

  test("`ceil` can get ceiling of positive float") {
    val exprStr = """{"ceil": [3.1]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(4))
    }
  }

  test("`ceil` can get ceiling of negative float") {
    val exprStr = """{"ceil": [-2.9]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(-2))
    }
  }

  test("`pow` can compute power of two integers") {
    val exprStr = """{"pow": [2, 3]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(8))
    }
  }

  test("`pow` can compute power with float base") {
    val exprStr = """{"pow": [2.5, 2]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, FloatValue(6.25))
    }
  }

  test("`pow` can compute power with negative exponent") {
    val exprStr = """{"pow": [2, -2]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, FloatValue(0.25))
    }
  }

  test("`pow` handles zero exponent") {
    val exprStr = """{"pow": [5, 0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(1))
    }
  }

  test("`pow` handles fractional exponent") {
    val exprStr = """{"pow": [4, 0.5]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(2))
    }
  }

  test("`pow` handles fractional exponent and decimal base") {
    val exprStr = """{"pow": [6.25, 0.5]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, FloatValue(2.5))
    }
  }

  test("`has` returns true when map contains key") {
    val exprStr = """{"has": [{"a": 1, "b": 2}, "a"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`has` returns false when map does not contain key") {
    val exprStr = """{"has": [{"a": 1, "b": 2}, "c"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("`has` can check keys in variable map") {
    val exprStr = """{"has": [{"var": "obj"}, "name"]}"""
    val dataStr = """{"obj": {"name": "Alice", "age": 30}}"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`entries` can convert map to array of key-value pairs") {
    val exprStr = """{"entries": [{"a": 1, "b": 2}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(
          expr,
          data,
          ArrayValue(
            List(
              ArrayValue(List(StrValue("a"), IntValue(1))),
              ArrayValue(List(StrValue("b"), IntValue(2)))
            )
          )
        )
    }
  }

  test("`entries` handles empty map") {
    val exprStr = """{"entries": [{}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List()))
    }
  }

  test("`entries` can work with variable maps") {
    val exprStr = """{"entries": [{"var": "data"}]}"""
    val dataStr = """{"data": {"x": 10, "y": 20}}"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(
          expr,
          data,
          ArrayValue(
            List(
              ArrayValue(List(StrValue("x"), IntValue(10))),
              ArrayValue(List(StrValue("y"), IntValue(20)))
            )
          )
        )
    }
  }

  test("new array operations can compose together") {
    val exprStr = """{"reverse": [{"unique": [{"flatten": [[[1, 2], [2, 3], [3, 4]]]}]}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, ArrayValue(List(IntValue(4), IntValue(3), IntValue(2), IntValue(1))))
    }
  }

  test("new string operations can compose together") {
    val exprStr = """{"startsWith": [{"trim": [{"lower": ["  HELLO WORLD  "]}]}, "hello"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("new math operations can compose together") {
    val exprStr = """{"abs": [{"floor": [{"pow": [2.5, 2]}]}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, IntValue(6))
    }
  }

  test("`typeof` returns correct type for null") {
    val exprStr = """{"typeof": [null]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("null"))
    }
  }

  test("`typeof` returns correct type for boolean") {
    val exprStr = """{"typeof": [true]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("bool"))
    }
  }

  test("`typeof` returns correct type for integer") {
    val exprStr = """{"typeof": [42]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("int"))
    }
  }

  test("`typeof` returns correct type for float") {
    val exprStr = """{"typeof": [3.14]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("float"))
    }
  }

  test("`typeof` returns correct type for string") {
    val exprStr = """{"typeof": ["hello"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("string"))
    }
  }

  test("`typeof` returns correct type for array") {
    val exprStr = """{"typeof": [[1, 2, 3]]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("array"))
    }
  }

  test("`typeof` returns correct type for map") {
    val exprStr = """{"typeof": [{"key": "value"}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("map"))
    }
  }

  test("`typeof` can work with variables") {
    val exprStr = """{"typeof": [{"var": "data"}]}"""
    val dataStr = """{"data": 123}"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, StrValue("int"))
    }
  }

  test("`typeof` can be used in type validation") {
    val exprStr = """{"===": [{"typeof": [{"var": "age"}]}, "int"]}"""
    val dataStr = """{"age": 30}"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }

  test("`typeof` can be used to distinguish int from float") {
    val exprStr = """{"===": [{"typeof": [{"var": "value"}]}, "float"]}"""
    val dataStr = """{"value": 3.14}"""

    parseTestJson(exprStr, dataStr).flatMap {
      case (expr, data) =>
        staticTestRunner(expr, data, BoolValue(true))
    }
  }
}
