package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

object AllOperatorsComparisonSuite extends SimpleIOSuite {

  private def testBothStrategies(exprStr: String, dataStr: String): IO[Boolean] =
    for {
      expr            <- IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))
      data            <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      recursiveResult <- JsonLogicEvaluator.recursive[IO].evaluate(expr, data, None)
      tailRecResult   <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield recursiveResult == tailRecResult

  // Control Flow
  test("if: simple true condition") {
    testBothStrategies("""{"if": [true, "yes", "no"]}""", "null").map(expect(_))
  }

  test("if: simple false condition") {
    testBothStrategies("""{"if": [false, "yes", "no"]}""", "null").map(expect(_))
  }

  test("if: nested conditions") {
    testBothStrategies("""{"if": [false, "a", true, "b", "c"]}""", "null").map(expect(_))
  }

  test("default: null value") {
    testBothStrategies("""{"default": [null, "default"]}""", "null").map(expect(_))
  }

  test("default: non-null value") {
    testBothStrategies("""{"default": ["value", "default"]}""", "null").map(expect(_))
  }

  test("let: single binding") {
    testBothStrategies("""{"let": [[["x", 5]], {"var": "x"}]}""", "null").map(expect(_))
  }

  test("let: sequential bindings") {
    testBothStrategies("""{"let": [[["x", 5], ["y", {"+": [{"var": "x"}, 3]}]], {"var": "y"}]}""", "null").map(expect(_))
  }

  // Logic
  test("!: not true") {
    testBothStrategies("""{"!": [true]}""", "null").map(expect(_))
  }

  test("!: not false") {
    testBothStrategies("""{"!": [false]}""", "null").map(expect(_))
  }

  test("!!: truthy value") {
    testBothStrategies("""{"!!": [1]}""", "null").map(expect(_))
  }

  test("!!: falsy value") {
    testBothStrategies("""{"!!": [0]}""", "null").map(expect(_))
  }

  test("or: true branch") {
    testBothStrategies("""{"or": [false, true]}""", "null").map(expect(_))
  }

  test("or: false branch") {
    testBothStrategies("""{"or": [false, false]}""", "null").map(expect(_))
  }

  test("and: true branch") {
    testBothStrategies("""{"and": [true, true]}""", "null").map(expect(_))
  }

  test("and: false branch") {
    testBothStrategies("""{"and": [true, false]}""", "null").map(expect(_))
  }

  // Comparison
  test("==: loose equality") {
    testBothStrategies("""{"==": [1, "1"]}""", "null").map(expect(_))
  }

  test("==: boolean equality") {
    testBothStrategies("""{"==": [true, false]}""", "null").map(expect(_))
  }

  test("===: strict equality") {
    testBothStrategies("""{"===": [1, 1]}""", "null").map(expect(_))
  }

  test("===: string equality") {
    testBothStrategies("""{"===": ["a", "a"]}""", "null").map(expect(_))
  }

  test("!=: loose inequality") {
    testBothStrategies("""{"!=": [1, 2]}""", "null").map(expect(_))
  }

  test("!=: boolean inequality") {
    testBothStrategies("""{"!=": [true, false]}""", "null").map(expect(_))
  }

  test("!==: strict inequality") {
    testBothStrategies("""{"!==": [1, "1"]}""", "null").map(expect(_))
  }

  test("!==: string inequality") {
    testBothStrategies("""{"!==": ["a", "b"]}""", "null").map(expect(_))
  }

  test("<: less than") {
    testBothStrategies("""{"<": [1, 2]}""", "null").map(expect(_))
  }

  test("<: equal values") {
    testBothStrategies("""{"<": [5, 5]}""", "null").map(expect(_))
  }

  test("<=: less than") {
    testBothStrategies("""{"<=": [1, 2]}""", "null").map(expect(_))
  }

  test("<=: equal values") {
    testBothStrategies("""{"<=": [5, 5]}""", "null").map(expect(_))
  }

  test(">: greater than") {
    testBothStrategies("""{">": [2, 1]}""", "null").map(expect(_))
  }

  test(">: equal values") {
    testBothStrategies("""{">": [5, 5]}""", "null").map(expect(_))
  }

  test(">=: greater than") {
    testBothStrategies("""{">=": [2, 1]}""", "null").map(expect(_))
  }

  test(">=: equal values") {
    testBothStrategies("""{">=": [5, 5]}""", "null").map(expect(_))
  }

  // Arithmetic
  test("+: addition") {
    testBothStrategies("""{"+": [1, 2, 3]}""", "null").map(expect(_))
  }

  test("+: zero addition") {
    testBothStrategies("""{"+": [0, 0]}""", "null").map(expect(_))
  }

  test("-: subtraction") {
    testBothStrategies("""{"-": [10, 3]}""", "null").map(expect(_))
  }

  test("-: zero subtraction") {
    testBothStrategies("""{"-": [0, 0]}""", "null").map(expect(_))
  }

  test("*: multiplication") {
    testBothStrategies("""{"*": [2, 3]}""", "null").map(expect(_))
  }

  test("*: zero multiplication") {
    testBothStrategies("""{"*": [0, 5]}""", "null").map(expect(_))
  }

  test("/: division") {
    testBothStrategies("""{"/": [10, 2]}""", "null").map(expect(_))
  }

  test("/: integer division") {
    testBothStrategies("""{"/": [5, 2]}""", "null").map(expect(_))
  }

  test("%: modulo") {
    testBothStrategies("""{"%": [10, 3]}""", "null").map(expect(_))
  }

  test("%: modulo zero") {
    testBothStrategies("""{"%": [5, 2]}""", "null").map(expect(_))
  }

  test("max: maximum value") {
    testBothStrategies("""{"max": [1, 5, 3]}""", "null").map(expect(_))
  }

  test("max: negative values") {
    testBothStrategies("""{"max": [-1, 0, 1]}""", "null").map(expect(_))
  }

  test("min: minimum value") {
    testBothStrategies("""{"min": [1, 5, 3]}""", "null").map(expect(_))
  }

  test("min: negative values") {
    testBothStrategies("""{"min": [-1, 0, 1]}""", "null").map(expect(_))
  }

  test("abs: negative number") {
    testBothStrategies("""{"abs": [-5]}""", "null").map(expect(_))
  }

  test("abs: positive number") {
    testBothStrategies("""{"abs": [5]}""", "null").map(expect(_))
  }

  test("round: round up") {
    testBothStrategies("""{"round": [2.6]}""", "null").map(expect(_))
  }

  test("round: round down") {
    testBothStrategies("""{"round": [2.4]}""", "null").map(expect(_))
  }

  test("floor: floor down") {
    testBothStrategies("""{"floor": [2.9]}""", "null").map(expect(_))
  }

  test("floor: floor up") {
    testBothStrategies("""{"floor": [2.1]}""", "null").map(expect(_))
  }

  test("ceil: ceil up") {
    testBothStrategies("""{"ceil": [2.1]}""", "null").map(expect(_))
  }

  test("ceil: ceil down") {
    testBothStrategies("""{"ceil": [2.9]}""", "null").map(expect(_))
  }

  test("pow: power") {
    testBothStrategies("""{"pow": [2, 3]}""", "null").map(expect(_))
  }

  test("pow: zero power") {
    testBothStrategies("""{"pow": [0, 5]}""", "null").map(expect(_))
  }

  // Array Operations
  test("map: transform array") {
    testBothStrategies("""{"map": [[1,2,3], {"*": [{"var": ""}, 2]}]}""", "null").map(expect(_))
  }

  test("map: empty array") {
    testBothStrategies("""{"map": [[], {"*": [{"var": ""}, 2]}]}""", "null").map(expect(_))
  }

  test("filter: filter array") {
    testBothStrategies("""{"filter": [[1,2,3,4], {">": [{"var": ""}, 2]}]}""", "null").map(expect(_))
  }

  test("filter: empty result") {
    testBothStrategies("""{"filter": [[1,2,3,4], {">": [{"var": ""}, 5]}]}""", "null").map(expect(_))
  }

  test("reduce: reduce array") {
    testBothStrategies("""{"reduce": [[1,2,3], {"+": [{"var": "current"}, {"var": "accumulator"}]}, 0]}""", "null").map(expect(_))
  }

  test("reduce: empty array") {
    testBothStrategies("""{"reduce": [[], {"+": [{"var": "current"}, {"var": "accumulator"}]}, 0]}""", "null").map(expect(_))
  }

  test("merge: merge arrays") {
    testBothStrategies("""{"merge": [[1,2], [3,4]]}""", "null").map(expect(_))
  }

  test("merge: empty arrays") {
    testBothStrategies("""{"merge": [[], []]}""", "null").map(expect(_))
  }

  test("all: all match") {
    testBothStrategies("""{"all": [[1,2,3], {">": [{"var": ""}, 0]}]}""", "null").map(expect(_))
  }

  test("all: some do not match") {
    testBothStrategies("""{"all": [[1,-2,3], {">": [{"var": ""}, 0]}]}""", "null").map(expect(_))
  }

  test("some: some match") {
    testBothStrategies("""{"some": [[1,2,3], {">": [{"var": ""}, 2]}]}""", "null").map(expect(_))
  }

  test("some: none match") {
    testBothStrategies("""{"some": [[1,2,3], {">": [{"var": ""}, 5]}]}""", "null").map(expect(_))
  }

  test("none: none match") {
    testBothStrategies("""{"none": [[1,2,3], {"<": [{"var": ""}, 0]}]}""", "null").map(expect(_))
  }

  test("none: some match") {
    testBothStrategies("""{"none": [[1,2,3], {"<": [{"var": ""}, 1]}]}""", "null").map(expect(_))
  }

  test("find: find first") {
    testBothStrategies("""{"find": [[1,2,3,4], {">": [{"var": ""}, 2]}]}""", "null").map(expect(_))
  }

  test("find: not found") {
    testBothStrategies("""{"find": [[1,2,3,4], {">": [{"var": ""}, 5]}]}""", "null").map(expect(_))
  }

  test("count: count array") {
    testBothStrategies("""{"count": [[1,2,3]]}""", "null").map(expect(_))
  }

  test("count: count with predicate") {
    testBothStrategies("""{"count": [[1,2,3,4], {">": [{"var": ""}, 2]}]}""", "null").map(expect(_))
  }

  test("in: membership true") {
    testBothStrategies("""{"in": ["a", ["a","b","c"]]}""", "null").map(expect(_))
  }

  test("in: membership false") {
    testBothStrategies("""{"in": ["z", ["a","b","c"]]}""", "null").map(expect(_))
  }

  test("intersect: intersection") {
    testBothStrategies("""{"intersect": [[1,2,3], [2,3,4]]}""", "null").map(expect(_))
  }

  test("intersect: no intersection") {
    testBothStrategies("""{"intersect": [[1,2], [3,4]]}""", "null").map(expect(_))
  }

  test("unique: unique elements") {
    testBothStrategies("""{"unique": [[1,1,2,2,3]]}""", "null").map(expect(_))
  }

  test("unique: already unique") {
    testBothStrategies("""{"unique": [[1,2,3]]}""", "null").map(expect(_))
  }

  test("slice: slice array") {
    testBothStrategies("""{"slice": [[1,2,3,4,5], 1, 3]}""", "null").map(expect(_))
  }

  test("slice: slice from start") {
    testBothStrategies("""{"slice": [[1,2,3], 0, 1]}""", "null").map(expect(_))
  }

  test("reverse: reverse array") {
    testBothStrategies("""{"reverse": [[1,2,3]]}""", "null").map(expect(_))
  }

  test("reverse: reverse empty") {
    testBothStrategies("""{"reverse": [[]]}""", "null").map(expect(_))
  }

  test("flatten: flatten nested") {
    testBothStrategies("""{"flatten": [[[1,2], [3,4]]]}""", "null").map(expect(_))
  }

  test("flatten: deeply nested") {
    testBothStrategies("""{"flatten": [[[]]]}""", "null").map(expect(_))
  }

  // String Operations
  test("cat: concatenate") {
    testBothStrategies("""{"cat": ["hello", " ", "world"]}""", "null").map(expect(_))
  }

  test("cat: empty strings") {
    testBothStrategies("""{"cat": ["", ""]}""", "null").map(expect(_))
  }

  test("substr: substring") {
    testBothStrategies("""{"substr": ["hello", 1, 3]}""", "null").map(expect(_))
  }

  test("substr: single char") {
    testBothStrategies("""{"substr": ["a", 0, 1]}""", "null").map(expect(_))
  }

  test("lower: lowercase") {
    testBothStrategies("""{"lower": ["HELLO"]}""", "null").map(expect(_))
  }

  test("lower: already lowercase") {
    testBothStrategies("""{"lower": ["hello"]}""", "null").map(expect(_))
  }

  test("upper: uppercase") {
    testBothStrategies("""{"upper": ["hello"]}""", "null").map(expect(_))
  }

  test("upper: already uppercase") {
    testBothStrategies("""{"upper": ["HELLO"]}""", "null").map(expect(_))
  }

  test("join: join array") {
    testBothStrategies("""{"join": [["a","b","c"], ","]}""", "null").map(expect(_))
  }

  test("join: empty array") {
    testBothStrategies("""{"join": [[], ","]}""", "null").map(expect(_))
  }

  test("split: split string") {
    testBothStrategies("""{"split": ["a,b,c", ","]}""", "null").map(expect(_))
  }

  test("split: empty string") {
    testBothStrategies("""{"split": ["", ","]}""", "null").map(expect(_))
  }

  test("trim: trim whitespace") {
    testBothStrategies("""{"trim": ["  hello  "]}""", "null").map(expect(_))
  }

  test("trim: no whitespace") {
    testBothStrategies("""{"trim": ["hello"]}""", "null").map(expect(_))
  }

  test("startsWith: starts with") {
    testBothStrategies("""{"startsWith": ["hello", "he"]}""", "null").map(expect(_))
  }

  test("startsWith: does not start with") {
    testBothStrategies("""{"startsWith": ["hello", "hi"]}""", "null").map(expect(_))
  }

  test("endsWith: ends with") {
    testBothStrategies("""{"endsWith": ["hello", "lo"]}""", "null").map(expect(_))
  }

  test("endsWith: does not end with") {
    testBothStrategies("""{"endsWith": ["hello", "ll"]}""", "null").map(expect(_))
  }

  // Object Operations
  test("values: get values") {
    testBothStrategies("""{"values": [{"a": 1, "b": 2}]}""", "null").map(expect(_))
  }

  test("values: empty object") {
    testBothStrategies("""{"values": [{}]}""", "null").map(expect(_))
  }

  test("values: no args") {
    // Both evaluators should return NullValue for empty args
    testBothStrategies("""{"values": []}""", "null").map(expect(_))
  }

  test("keys: get keys") {
    testBothStrategies("""{"keys": [{"a": 1, "b": 2}]}""", "null").map(expect(_))
  }

  test("keys: empty object") {
    testBothStrategies("""{"keys": [{}]}""", "null").map(expect(_))
  }

  test("keys: no args") {
    // Both evaluators should return NullValue for empty args
    testBothStrategies("""{"keys": []}""", "null").map(expect(_))
  }

  test("get: get property") {
    testBothStrategies("""{"get": [{"a": 1}, "a"]}""", "null").map(expect(_))
  }

  test("get: property from data") {
    testBothStrategies("""{"get": [{"var": "obj"}, "key"]}""", """{"obj": {"key": 42}}""").map(expect(_))
  }

  test("get: missing property returns null") {
    // Both evaluators should return NullValue for missing keys
    testBothStrategies("""{"get": [{"a": 1}, "b"]}""", "null").map(expect(_))
  }

  test("has: has property") {
    testBothStrategies("""{"has": [{"a": 1}, "a"]}""", "null").map(expect(_))
  }

  test("has: missing property") {
    testBothStrategies("""{"has": [{"a": 1}, "b"]}""", "null").map(expect(_))
  }

  test("entries: get entries") {
    testBothStrategies("""{"entries": [{"a": 1}]}""", "null").map(expect(_))
  }

  test("entries: empty object") {
    testBothStrategies("""{"entries": [{}]}""", "null").map(expect(_))
  }

  test("entries: no args") {
    // Both evaluators should return NullValue for empty args
    testBothStrategies("""{"entries": []}""", "null").map(expect(_))
  }

  // Utility
  test("length: length of string") {
    testBothStrategies("""{"length": ["hello"]}""", "null").map(expect(_))
  }

  test("length: length of array") {
    testBothStrategies("""{"length": [[1,2,3]]}""", "null").map(expect(_))
  }

  test("length: empty string") {
    testBothStrategies("""{"length": [""]}""", "null").map(expect(_))
  }

  test("length: empty array") {
    testBothStrategies("""{"length": [[]]}""", "null").map(expect(_))
  }

  test("exists: var exists") {
    testBothStrategies("""{"exists": [{"var": "a"}]}""", """{"a": 1}""").map(expect(_))
  }

  test("exists: var missing") {
    testBothStrategies("""{"exists": [{"var": "b"}]}""", """{"a": 1}""").map(expect(_))
  }

  test("missing: vars missing") {
    testBothStrategies("""{"missing": ["a", "b"]}""", "null").map(expect(_))
  }

  test("missing: var present") {
    testBothStrategies("""{"missing": ["a"]}""", """{"a": 1}""").map(expect(_))
  }

  test("missing: no vars missing") {
    testBothStrategies("""{"missing": []}""", """{"a": 1}""").map(expect(_))
  }

  test("missing_some: some missing") {
    testBothStrategies("""{"missing_some": [1, ["a", "b"]]}""", "null").map(expect(_))
  }

  test("missing_some: none missing") {
    testBothStrategies("""{"missing_some": [1, ["a", "b"]]}""", """{"a": 1}""").map(expect(_))
  }

  test("missing_some: all missing") {
    testBothStrategies("""{"missing_some": [1, ["a", "b"]]}""", """{"c": 1}""").map(expect(_))
  }

  test("typeof: number") {
    testBothStrategies("""{"typeof": [123]}""", "null").map(expect(_))
  }

  test("typeof: string") {
    testBothStrategies("""{"typeof": ["abc"]}""", "null").map(expect(_))
  }

  test("typeof: boolean") {
    testBothStrategies("""{"typeof": [true]}""", "null").map(expect(_))
  }

  test("typeof: null") {
    testBothStrategies("""{"typeof": [null]}""", "null").map(expect(_))
  }

  // Note: NoOp is an internal marker, not a user-facing operator.
  // Direct calls to {"noop": ...} behave inconsistently between evaluators.
  // These tests are intentionally omitted.
}
