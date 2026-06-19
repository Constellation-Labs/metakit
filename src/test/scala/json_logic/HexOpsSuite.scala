package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.HexOps
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

/**
 * Conformance + behavior tests for the JLVM `hex_to_int` opcode.
 *
 * `hex_to_int` parses a single arbitrary-length hex string (reusing the shared [[HexBytes]] codec,
 * so it inherits the lowercase / `0x`-prefixed / even-length grammar of the crypto opcodes) and
 * returns it as an UNSIGNED big-endian [[IntValue]] (BigInt, arbitrary precision). The result is
 * always non-negative; the empty body `"0x"` is `0`. The canonical conformance vectors below are
 * the cross-language math facts (identical across the Scala / Rust / TS evaluators).
 *
 * Each vector is exercised both end-to-end through the evaluator and directly through [[HexOps]],
 * plus the error cases (odd-length body, non-hex chars, non-string argument).
 */
object HexOpsSuite extends SimpleIOSuite {

  private def evalExpr(exprJson: String, dataJson: String = "{}"): IO[Either[JsonLogicException, JsonLogicValue]] =
    for {
      expr <- IO.fromEither(parser.parse(exprJson).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse(dataJson).flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield out

  // Canonical (hex -> expected BigInt) conformance vectors. The 64-byte all-ones value is expressed
  // as BigInt(2).pow(512) - 1 (NOT transcribed as a literal), per the shared-vector convention.
  private val vectors: List[(String, BigInt)] = List(
    "0x"                   -> BigInt(0),
    "0x00"                 -> BigInt(0),
    "0xff"                 -> BigInt(255),
    "0x0100"               -> BigInt(256),
    "0x00ff"               -> BigInt(255),
    "0xdeadbeef"           -> BigInt(3735928559L),
    "0xffffffffffffffff"   -> BigInt("18446744073709551615"), // 2^64 - 1 (> Long/Double range)
    "0x010000000000000000" -> BigInt("18446744073709551616"), // 2^64
    ("0x" + "f" * 128)     -> (BigInt(2).pow(512) - 1) // 64-byte all-ones
  )

  vectors.foreach {
    case (hex, expected) =>
      test(s"hex_to_int($hex) == $expected (end-to-end)") {
        evalExpr(s"""{"hex_to_int":["$hex"]}""").map(r => expect(r == Right(IntValue(expected))))
      }

      pureTest(s"hex_to_int($hex) == $expected (direct)") {
        expect(HexOps.hexToInt(List(StrValue(hex))) == Right(IntValue(expected)))
      }
  }

  test("hex_to_int is always non-negative for a large value") {
    evalExpr(s"""{"hex_to_int":["0x${"f" * 128}"]}""").map {
      case Right(IntValue(v)) => expect(v >= 0) && expect(v == BigInt(2).pow(512) - 1)
      case other              => failure(s"expected a non-negative IntValue, got $other")
    }
  }

  // ===========================================================================
  // Error cases: must RAISE (a JsonLogicException), not return a value.
  // ===========================================================================

  test("hex_to_int rejects an odd-length hex body (0xfff)") {
    evalExpr("""{"hex_to_int":["0xfff"]}""").map(r => expect(r.isLeft))
  }

  test("hex_to_int rejects non-hex characters (0xzz)") {
    evalExpr("""{"hex_to_int":["0xzz"]}""").map(r => expect(r.isLeft))
  }

  test("hex_to_int rejects a non-string argument (integer 5)") {
    evalExpr("""{"hex_to_int":[5]}""").map(r => expect(r.isLeft))
  }

  pureTest("hex_to_int direct: error cases surface as a Left, never a thrown exception") {
    expect(HexOps.hexToInt(List(StrValue("0xfff"))).isLeft) && // odd length
    expect(HexOps.hexToInt(List(StrValue("0xzz"))).isLeft) && // non-hex
    expect(HexOps.hexToInt(List(IntValue(5))).isLeft) && // non-string arg
    expect(HexOps.hexToInt(Nil).isLeft) && // wrong arity (none)
    expect(HexOps.hexToInt(List(StrValue("0x01"), StrValue("0x02"))).isLeft) // wrong arity (two)
  }
}
