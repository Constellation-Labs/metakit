package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic._

import io.circe.parser
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object JsonLogicSpec extends SimpleIOSuite with Checkers {

  def parseTestJson(expr: String, data: String): IO[(JsonLogicExpression, JsonLogicValue)] = for {
    expr <- IO.fromEither(parser.parse(expr).flatMap(_.as[JsonLogicExpression]))
    data <- IO.fromEither(parser.parse(data).flatMap(_.as[JsonLogicValue]))
  } yield (expr, data)

  private def staticTestRunner(
    expr:      JsonLogicExpression,
    data:      JsonLogicValue,
    expected:  JsonLogicValue,
    loggerOpt: Option[JsonLogicValue => F[Unit]] = None
  ): IO[Expectations] =
    JsonLogicEvaluator
      .tailRecursive[F]
      .evaluate(expr, data, None)
      .flatTap { result =>
        loggerOpt match {
          case Some(logger) => logger(result)
          case None         => IO.unit
        }
      }
      .map(actual => expect(actual == expected))

  private def expectError(
    expr: JsonLogicExpression,
    data: JsonLogicValue
  ): IO[Expectations] =
    JsonLogicEvaluator
      .tailRecursive[F]
      .evaluate(expr, data, None)
      .attempt
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr) flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      expectError(expr, data)
    }
  }

  test("division by zero should throw error for float division") {
    val exprStr = """{"/":[10.5, 0.0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      expectError(expr, data)
    }
  }

  test("division by zero should throw error for mixed types") {
    val exprStr = """{"/":[10, 0.0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      expectError(expr, data)
    }
  }

  test("modulo by zero should throw error for integer modulo") {
    val exprStr = """{"%":[10, 0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      expectError(expr, data)
    }
  }

  test("modulo by zero should throw error for float modulo") {
    val exprStr = """{"%":[10.5, 0.0]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      expectError(expr, data)
    }
  }

  test("array access with negative index should return null") {
    val exprStr = """{"var": -1}"""
    val dataStr = """["zero", "one", "two"]"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      staticTestRunner(expr, data, NullValue)
    }
  }

  test("array access with out-of-bounds index should return null") {
    val exprStr = """{"var": 5}"""
    val dataStr = """["zero", "one", "two"]"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      staticTestRunner(expr, data, NullValue)
    }
  }

  test("string to number coercion should handle invalid numbers gracefully") {
    val exprStr = """{"==":[1, "not_a_number"]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      staticTestRunner(expr, data, BoolValue(false))
    }
  }

  test("reduce with empty array and no initial value should return null") {
    val exprStr = """{"reduce":[[], {"+":[{"var":"current"}, {"var":"accumulator"}]}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      staticTestRunner(expr, data, NullValue)
    }
  }

  test("reduce with empty array and initial value should return initial value") {
    val exprStr = """{"reduce":[[], {"+":[{"var":"current"}, {"var":"accumulator"}]}, 42]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      staticTestRunner(expr, data, IntValue(42))
    }
  }

  test("reduce with non-empty array and no initial value should use first element") {
    val exprStr = """{"reduce":[[1,2,3,4], {"+":[{"var":"current"}, {"var":"accumulator"}]}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      staticTestRunner(expr, data, IntValue(10)) // 1 + 2 + 3 + 4 = 10
    }
  }

  test("reduce with single element array and no initial value should return that element") {
    val exprStr = """{"reduce":[[42], {"+":[{"var":"current"}, {"var":"accumulator"}]}]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      staticTestRunner(expr, data, IntValue(42))
    }
  }

  test("division should work correctly with valid operands") {
    val exprStr = """{"/":[10, 2]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
      staticTestRunner(expr, data, IntValue(5))
    }
  }

  test("modulo should work correctly with valid operands") {
    val exprStr = """{"%":[10, 3]}"""
    val dataStr = """null"""

    parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
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

    testCases.foldLeft(IO.pure(success)) { case (acc, (exprStr, dataStr, expected)) =>
      acc.flatMap { prevResult =>
        parseTestJson(exprStr, dataStr).flatMap { case (expr, data) =>
          staticTestRunner(expr, data, expected).map(result => prevResult and result)
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

    testCases.foldLeft(IO.pure(success)) { case (acc, (exprStr, expected)) =>
      acc.flatMap { prevResult =>
        parseTestJson(exprStr, "null").flatMap { case (expr, data) =>
          staticTestRunner(expr, data, expected).map(result => prevResult and result)
        }
      }
    }
  }
}
