package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasExhaustedException, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.{JsonLogicEvaluator, JsonLogicRuntime}

import io.circe.parser
import weaver.{Expectations, SimpleIOSuite}

/**
 * Wave-2 cross-language parity pins, each matching the Rust reference (`rust/jlvm-core`):
 *
 *   1. SCALE BOUND — string -> number coercion rejects any decimal whose effective scale magnitude
 *      (fractional digits minus exponent) exceeds `NumericOps.MaxDecimalScale` = 10_000, mirroring
 *      `Ratio::MAX_DECIMAL_SCALE` (ratio.rs). Without the bound, `Ratio.fromBigDecimal` would
 *      materialize `10^|scale|` (a memory bomb for "1e-2000000000") and Scala would compute an exact
 *      value where Rust/TS error. Coerced `==` comparisons treat an out-of-bound string as
 *      unparseable (`false`, not an error), exactly like Rust `coercion.rs::safe_parse_decimal`.
 *
 *   2. SUBSTR / SLICE i64 EXTREMES — indices accept the full i64 range with saturating index
 *      arithmetic (Rust `op_substr` / `op_slice`); values beyond i64 error ("<role> out of range").
 *
 *   3. DEPTH CAP — `JsonLogicRuntime.MaxEvalDepth` = 256, one unit per evaluated expression node
 *      (operator args, literal elements, if/let children, callback runs), enforced by BOTH runtime
 *      strategies and by the gas-metered path, matching Rust `MAX_EVAL_DEPTH` (eval.rs) exactly.
 */
object ParityWave2Suite extends SimpleIOSuite {

  private def parse(exprStr: String): IO[JsonLogicExpression] =
    IO.fromEither(parser.parse(exprStr).flatMap(_.as[JsonLogicExpression]))

  private def evalTailRec(exprStr: String): IO[Either[JsonLogicException, JsonLogicValue]] =
    parse(exprStr).flatMap(JsonLogicEvaluator.tailRecursive[IO].evaluate(_, MapValue.empty, None))

  private def evalRecursive(exprStr: String): IO[Either[JsonLogicException, JsonLogicValue]] =
    parse(exprStr).flatMap(JsonLogicEvaluator.recursive[IO].evaluate(_, MapValue.empty, None))

  private def expectBoth(exprStr: String)(check: Either[JsonLogicException, JsonLogicValue] => Expectations): IO[Expectations] =
    for {
      tr  <- evalTailRec(exprStr)
      rec <- evalRecursive(exprStr)
    } yield check(tr).and(check(rec))

  private def isOk(label: String): Either[JsonLogicException, JsonLogicValue] => Expectations = {
    case Right(_)  => success
    case Left(err) => failure(s"$label: expected success, got error: ${err.getMessage}")
  }

  private def isError(label: String): Either[JsonLogicException, JsonLogicValue] => Expectations = {
    case Left(_)  => success
    case Right(v) => failure(s"$label: expected an error, got $v")
  }

  private def isValue(label: String, expected: JsonLogicValue): Either[JsonLogicException, JsonLogicValue] => Expectations = {
    case Right(v)  => expect.same(expected, v)
    case Left(err) => failure(s"$label: expected $expected, got error: ${err.getMessage}")
  }

  // --- 1. decimal scale bound -------------------------------------------------------------

  test("string coercion accepts |scale| == 10000 (both bound edges)") {
    for {
      a <- expectBoth("""{"+": ["1e-10000"]}""")(isOk("1e-10000"))
      b <- expectBoth("""{"+": ["1e10000"]}""")(isOk("1e10000"))
    } yield a.and(b)
  }

  test("string coercion rejects |scale| == 10001 (one past the bound)") {
    for {
      a <- expectBoth("""{"+": ["1e-10001"]}""")(isError("1e-10001"))
      b <- expectBoth("""{"+": ["1e10001"]}""")(isError("1e10001"))
    } yield a.and(b)
  }

  test("string coercion rejects the 1e-2000000000 memory bomb (fast, no 10^|scale| allocation)") {
    expectBoth("""{"+": ["1e-2000000000"]}""")(isError("1e-2000000000"))
  }

  test("coerced == treats an out-of-bound decimal string as unparseable: false, not an error") {
    expectBoth("""{"==": [1.5, "1e-2000000000"]}""")(isValue("== vs 1e-2000000000", BoolValue(false)))
  }

  // --- 2. substr / slice at i64 extremes ---------------------------------------------------

  test("substr saturates at i64 extremes exactly like Rust op_substr") {
    for {
      a <- expectBoth("""{"substr": ["hello", -9223372036854775808]}""")(isValue("substr i64::MIN", StrValue("hello")))
      b <- expectBoth("""{"substr": ["hello", 1, 9223372036854775807]}""")(isValue("substr len i64::MAX", StrValue("ello")))
      c <- expectBoth("""{"substr": ["hello", 9223372036854775807]}""")(isValue("substr start i64::MAX", StrValue("")))
      d <- expectBoth("""{"substr": ["hello", 0, -9223372036854775808]}""")(isValue("substr neg-len i64::MIN", StrValue("")))
    } yield a.and(b).and(c).and(d)
  }

  test("substr/slice indices beyond the i64 range are an error (Rust bigint_to_i64 parity)") {
    for {
      a <- expectBoth("""{"substr": ["hello", 9223372036854775808]}""")(isError("substr start > i64::MAX"))
      b <- expectBoth("""{"slice": [[1, 2, 3], -9223372036854775809]}""")(isError("slice start < i64::MIN"))
    } yield a.and(b)
  }

  test("slice saturates at i64 extremes exactly like Rust op_slice") {
    val arr123 = ArrayValue(List(IntValue(1), IntValue(2), IntValue(3)))
    for {
      a <- expectBoth("""{"slice": [[1, 2, 3], -9223372036854775808]}""")(isValue("slice i64::MIN", arr123))
      b <- expectBoth("""{"slice": [[1, 2, 3], 0, 9223372036854775807]}""")(isValue("slice end i64::MAX", arr123))
      c <- expectBoth("""{"slice": [[1, 2, 3], 9223372036854775807]}""")(isValue("slice start i64::MAX", ArrayValue(Nil)))
    } yield a.and(b).and(c)
  }

  // --- 3. MaxEvalDepth --------------------------------------------------------------------

  /** A chain of `n` nested `{"!": [...]}` ops over `true`: max node depth is n + 1 (the constant). */
  private def nestedNot(n: Int): JsonLogicExpression =
    (1 to n).foldLeft(ConstExpression(BoolValue(true)): JsonLogicExpression) { (acc, _) =>
      ApplyExpression(JsonLogicOp.NotOp, List(acc))
    }

  test("255 nested operators evaluate (max node depth 256 == MaxEvalDepth) in both strategies") {
    val expr = nestedNot(JsonLogicRuntime.MaxEvalDepth - 1)
    for {
      tr  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, MapValue.empty, None)
      rec <- JsonLogicEvaluator.recursive[IO].evaluate(expr, MapValue.empty, None)
    } yield
      isValue("255 nested (tailrec)", BoolValue(false))(tr)
        .and(isValue("255 nested (recursive)", BoolValue(false))(rec))
  }

  test("256 nested operators exceed MaxEvalDepth (node depth 257) in both strategies") {
    val expr = nestedNot(JsonLogicRuntime.MaxEvalDepth)
    val expectedMsg = s"Recursion depth limit exceeded (${JsonLogicRuntime.MaxEvalDepth})"

    def checkErr(label: String): Either[JsonLogicException, JsonLogicValue] => Expectations = {
      case Left(err) => expect.same(expectedMsg, err.getMessage)
      case Right(v)  => failure(s"$label: expected depth error, got $v")
    }

    for {
      tr  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, MapValue.empty, None)
      rec <- JsonLogicEvaluator.recursive[IO].evaluate(expr, MapValue.empty, None)
    } yield checkErr("256 nested (tailrec)")(tr).and(checkErr("256 nested (recursive)")(rec))
  }

  test("callback runs count toward the depth cap (map body resumes from the map node's depth)") {
    // map node at depth 1, callback body root at depth 2: a body of k nested `!` over {"var":""}
    // has its ops at depths 2..k+1 and the var at k+2. k = 254 -> max 256 (ok);
    // k = 255 -> 257 (error). Matches Rust, where op_map's callback eval continues from the
    // shared depth cell.
    def mapWithBody(k: Int): JsonLogicExpression = {
      val body = (1 to k).foldLeft(VarExpression(Left(""), None): JsonLogicExpression) { (acc, _) =>
        ApplyExpression(JsonLogicOp.NotOp, List(acc))
      }
      ApplyExpression(JsonLogicOp.MapOp, List(ConstExpression(ArrayValue(List(IntValue(1)))), body))
    }

    for {
      ok  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(mapWithBody(254), MapValue.empty, None)
      err <- JsonLogicEvaluator.tailRecursive[IO].evaluate(mapWithBody(255), MapValue.empty, None)
    } yield isOk("map body at the cap")(ok).and(isError("map body past the cap")(err))
  }

  test("let bindings and result count toward the depth cap") {
    // let node at depth 1; binding/result expressions are children at depth 2.
    def letWithBinding(k: Int): JsonLogicExpression = {
      val bound = nestedNot(k)
      ApplyExpression(
        JsonLogicOp.LetOp,
        List(
          ArrayExpression(List(ArrayExpression(List(ConstExpression(StrValue("x")), bound)))),
          VarExpression(Left("x"), None)
        )
      )
    }
    // binding root at depth 2, ops at 2..k+1, const at k+2: k = 254 ok, k = 255 error.
    for {
      ok  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(letWithBinding(254), MapValue.empty, None)
      err <- JsonLogicEvaluator.tailRecursive[IO].evaluate(letWithBinding(255), MapValue.empty, None)
    } yield isOk("let binding at the cap")(ok).and(isError("let binding past the cap")(err))
  }

  test("untaken if branches never count toward the depth cap (lazy, like Rust)") {
    val deepElse = nestedNot(400) // far past the cap, but never evaluated
    val expr = ApplyExpression(
      JsonLogicOp.IfElseOp,
      List(ConstExpression(BoolValue(true)), ConstExpression(IntValue(1)), deepElse)
    )
    JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, MapValue.empty, None).map(isValue("lazy else", IntValue(1)))
  }

  test("the gas-metered evaluator enforces the same depth cap with a non-gas error") {
    val expr = nestedNot(JsonLogicRuntime.MaxEvalDepth)
    JsonLogicEvaluator
      .tailRecursive[IO]
      .evaluateWithGas(expr, MapValue.empty, None, GasLimit.Unlimited)
      .map {
        case Left(_: GasExhaustedException) => failure("expected a depth error, got gas exhaustion")
        case Left(err) => expect.same(s"Recursion depth limit exceeded (${JsonLogicRuntime.MaxEvalDepth})", err.getMessage)
        case Right(v)  => failure(s"expected depth error, got ${v.value}")
      }
  }
}
