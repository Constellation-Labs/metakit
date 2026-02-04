# Task: Create AllOperatorsComparisonSuite

Create a comprehensive test suite that verifies both evaluation strategies (recursive and tail-recursive) produce identical results for ALL JSON Logic operators.

## File to create
`src/test/scala/json_logic/AllOperatorsComparisonSuite.scala`

## Requirements

1. Test EVERY operator listed below
2. For each operator, create at least 2 test cases covering different scenarios
3. Each test must verify: `recursive.result == tailRecursive.result`
4. Use the existing `testBothStrategies` pattern from `EvaluationStrategyComparisonSuite`

## Operators to test (63 total)

### Control Flow
- `if` - conditional: `{"if": [condition, then, else]}`
- `default` - default value: `{"default": [expr, defaultVal]}`
- `let` - variable binding: `{"let": [[[name, expr]], body]}`

### Logic
- `!` - not: `{"!": [true]}`
- `!!` - double negation/truthy: `{"!!": [1]}`
- `or` - logical or: `{"or": [false, true]}`
- `and` - logical and: `{"and": [true, true]}`

### Comparison
- `==` - loose equality: `{"==": [1, "1"]}`
- `===` - strict equality: `{"===": [1, 1]}`
- `!=` - loose inequality: `{"!=": [1, 2]}`
- `!==` - strict inequality: `{"!==": [1, "1"]}`
- `<` - less than: `{"<": [1, 2]}`
- `<=` - less or equal: `{"<=": [1, 1]}`
- `>` - greater than: `{">": [2, 1]}`
- `>=` - greater or equal: `{">=": [2, 2]}`

### Arithmetic
- `+` - addition: `{"+": [1, 2, 3]}`
- `-` - subtraction: `{"-": [10, 3]}`
- `*` - multiplication: `{"*": [2, 3]}`
- `/` - division: `{"/": [10, 2]}`
- `%` - modulo: `{"%": [10, 3]}`
- `max` - maximum: `{"max": [1, 5, 3]}`
- `min` - minimum: `{"min": [1, 5, 3]}`
- `abs` - absolute value: `{"abs": [-5]}`
- `round` - round: `{"round": [2.6]}`
- `floor` - floor: `{"floor": [2.9]}`
- `ceil` - ceiling: `{"ceil": [2.1]}`
- `pow` - power: `{"pow": [2, 3]}`

### Array Operations
- `map` - transform array: `{"map": [[1,2,3], {"*": [{"var": ""}, 2]}]}`
- `filter` - filter array: `{"filter": [[1,2,3,4], {">": [{"var": ""}, 2]}]}`
- `reduce` - reduce array: `{"reduce": [[1,2,3], {"+": [{"var": "current"}, {"var": "accumulator"}]}, 0]}`
- `merge` - merge arrays: `{"merge": [[1,2], [3,4]]}`
- `all` - all match: `{"all": [[1,2,3], {">": [{"var": ""}, 0]}]}`
- `some` - some match: `{"some": [[1,2,3], {">": [{"var": ""}, 2]}]}`
- `none` - none match: `{"none": [[1,2,3], {"<": [{"var": ""}, 0]}]}`
- `find` - find first: `{"find": [[1,2,3,4], {">": [{"var": ""}, 2]}]}`
- `count` - count (with optional predicate): `{"count": [[1,2,3]]}` and `{"count": [[1,2,3,4], {">": [{"var": ""}, 2]}]}`
- `in` - membership: `{"in": ["a", ["a","b","c"]]}`
- `intersect` - intersection: `{"intersect": [[1,2,3], [2,3,4]]}`
- `unique` - unique elements: `{"unique": [[1,1,2,2,3]]}`
- `slice` - slice array: `{"slice": [[1,2,3,4,5], 1, 3]}`
- `reverse` - reverse: `{"reverse": [[1,2,3]]}`
- `flatten` - flatten nested: `{"flatten": [[[1,2], [3,4]]]}`

### String Operations
- `cat` - concatenate: `{"cat": ["hello", " ", "world"]}`
- `substr` - substring: `{"substr": ["hello", 1, 3]}`
- `lower` - lowercase: `{"lower": ["HELLO"]}`
- `upper` - uppercase: `{"upper": ["hello"]}`
- `join` - join array: `{"join": [["a","b","c"], ","]}`
- `split` - split string: `{"split": ["a,b,c", ","]}`
- `trim` - trim whitespace: `{"trim": ["  hello  "]}`
- `startsWith` - starts with: `{"startsWith": ["hello", "he"]}`
- `endsWith` - ends with: `{"endsWith": ["hello", "lo"]}`

### Object Operations
- `values` - get values: `{"values": [{"a": 1, "b": 2}]}`
- `keys` - get keys: `{"keys": [{"a": 1, "b": 2}]}`
- `get` - get property: `{"get": [{"a": 1}, "a"]}`
- `has` - has property: `{"has": [{"a": 1}, "a"]}`
- `entries` - get entries: `{"entries": [{"a": 1}]}`

### Utility
- `length` - length of string/array: `{"length": ["hello"]}` and `{"length": [[1,2,3]]}`
- `exists` - check vars exist: `{"exists": [{"var": "a"}]}`
- `missing` - check missing vars: `{"missing": ["a", "b"]}`
- `missing_some` - check if some missing: `{"missing_some": [1, ["a", "b"]]}`
- `typeof` - type of value: `{"typeof": [123]}`

### Special
- `noop` - no operation: `{"noop": [1, 2, 3]}`

## Test structure

```scala
package json_logic

import cats.effect.IO
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator
import io.circe.parser
import weaver.SimpleIOSuite

object AllOperatorsComparisonSuite extends SimpleIOSuite {

  private def testBothStrategies(exprStr: String, dataStr: String): IO[Boolean] =
    for {
      expr <- IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      recursiveResult  <- JsonLogicEvaluator.recursive[IO].evaluate(expr, data, None)
      tailRecResult    <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield recursiveResult == tailRecResult

  // Control Flow
  test("if: simple condition") {
    testBothStrategies("""{"if": [true, "yes", "no"]}""", "null").map(expect(_))
  }
  
  test("if: nested conditions") {
    testBothStrategies("""{"if": [false, "a", true, "b", "c"]}""", "null").map(expect(_))
  }

  // ... continue for all 63 operators with 2+ test cases each
}
```

## Important notes

1. Use `"null"` as dataStr when no external data needed
2. For operators that access data, provide appropriate data JSON
3. Test edge cases where reasonable (empty arrays, null values, etc.)
4. Do NOT use operator names as variable names in `let` bindings (parser conflict)
5. Organize tests by category with comments
