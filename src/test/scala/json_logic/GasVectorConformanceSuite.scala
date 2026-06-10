package json_logic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core.{JsonLogicExpression, JsonLogicValue}
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasConfig, GasExhaustedException, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.{Decoder, Json, parser}
import weaver.SimpleIOSuite

/**
 * Cross-language conformance suite for GAS METERING.
 *
 * Runs the shared gas test vectors
 * (`src/test/resources/conformance/gas_test_vectors.json`), which are also
 * executed by the Rust implementation
 * (`metakit-sdk/rust/jlvm-core/tests/gas_differential.rs` against the synced
 * copy in `metakit-sdk/shared/gas_test_vectors.json`). Scala (this repo) is
 * the REFERENCE gas meter: every `expected` value was produced by running
 * `JsonLogicEvaluator.evaluateWithGas` with `GasConfig.Default` (see
 * [[GasVectorGenerator]]), so every case must pass here unconditionally and
 * with EXACT equality:
 *
 *   - integer `expected`: evaluation must SUCCEED and report exactly that
 *     `gasUsed` (the gas-counter delta) under the declared `gasLimit`;
 *   - `"OOG"` `expected`: evaluation must FAIL with the distinct
 *     [[GasExhaustedException]] (any other failure is a conformance bug).
 *
 * The charging contract these vectors pin down is normative per metakit
 * PR #37 (charge-once; base + depthPenalty + inputScaledCost pre-charged
 * atomically before the primitive; output-scaled residual only for
 * split/merge/flatten/slice/substr; var lookups charge varAccess +
 * #pathSegments at lookup; if/let charge no base cost).
 */
object GasVectorConformanceSuite extends SimpleIOSuite {

  private val VectorsPath = "src/test/resources/conformance/gas_test_vectors.json"

  final case class Vectors(description: String, version: String, tests: List[Category])
  final case class Category(category: String, note: Option[String], cases: List[VecCase])

  final case class VecCase(
    expr: String,
    data: String,
    gasLimit: Long,
    expected: Json,
    note: Option[String]
  )

  implicit private val caseDecoder: Decoder[VecCase] = Decoder.forProduct5(
    "expr",
    "data",
    "gasLimit",
    "expected",
    "note"
  )(VecCase.apply)

  implicit private val categoryDecoder: Decoder[Category] = Decoder.forProduct3(
    "category",
    "note",
    "cases"
  )(Category.apply)

  implicit private val vectorsDecoder: Decoder[Vectors] = Decoder.forProduct3(
    "description",
    "version",
    "tests"
  )(Vectors.apply)

  // Loaded eagerly so tests can be registered per category during object init.
  private val vectors: Vectors = {
    val raw = new String(Files.readAllBytes(Paths.get(VectorsPath)), StandardCharsets.UTF_8)
    parser
      .parse(raw)
      .flatMap(_.as[Vectors])
      .fold(err => throw new RuntimeException(s"Failed to load gas vectors: $err"), identity)
  }

  private val evaluator = JsonLogicEvaluator.tailRecursive[IO]

  final private case class CaseOutcome(label: String, pass: Boolean, detail: String)

  private def runCase(c: VecCase): IO[CaseOutcome] = {
    val label = c.note.fold(c.expr)(n => s"${c.expr}  ($n)")

    def outcome(pass: Boolean, detail: String): CaseOutcome = CaseOutcome(label, pass, detail)

    val parsed: Either[String, (JsonLogicExpression, JsonLogicValue)] =
      for {
        exprJson <- parser.parse(c.expr).left.map(e => s"EXPR-PARSE: ${e.getMessage}")
        expr     <- exprJson.as[JsonLogicExpression].left.map(e => s"EXPR-DECODE: ${e.getMessage}")
        dataJson <- parser.parse(c.data).left.map(e => s"DATA-PARSE: ${e.getMessage}")
        data     <- dataJson.as[JsonLogicValue].left.map(e => s"DATA-DECODE: ${e.getMessage}")
      } yield (expr, data)

    parsed match {
      case Left(err) =>
        IO.pure(outcome(pass = false, s"$label\n    $err"))
      case Right((expr, data)) =>
        val expectsOog = c.expected.asString.contains("OOG")
        val expectedGas = c.expected.asNumber.flatMap(_.toLong)

        evaluator
          .evaluateWithGas(expr, data, None, GasLimit(c.gasLimit), GasConfig.Default)
          .attempt
          .map {
            case Left(raised) =>
              outcome(pass = false, s"$label\n    RAISED: ${raised.getMessage}")
            case Right(Left(_: GasExhaustedException)) =>
              if (expectsOog) outcome(pass = true, s"$label\n    OOG as expected")
              else outcome(pass = false, s"$label\n    expected gasUsed=${expectedGas.orNull} but ran OUT OF GAS")
            case Right(Left(otherErr)) =>
              outcome(pass = false, s"$label\n    NON-GAS EVAL-ERR: ${otherErr.getMessage}")
            case Right(Right(result)) =>
              if (expectsOog)
                outcome(
                  pass = false,
                  s"$label\n    expected OOG but evaluation succeeded with gasUsed=${result.gasUsed.amount}"
                )
              else
                expectedGas match {
                  case Some(expected) =>
                    val got = result.gasUsed.amount
                    outcome(
                      pass = got == expected,
                      s"$label\n    gasLimit=${c.gasLimit} expected gasUsed=$expected got=$got"
                    )
                  case None =>
                    outcome(pass = false, s"$label\n    malformed expected: ${c.expected.noSpaces}")
                }
          }
    }
  }

  // Register one weaver test per category.
  vectors.tests.foreach { cat =>
    test(s"[${cat.category}] shared gas vectors (exact gasUsed equality)") {
      cat.cases
        .foldRight(IO.pure(List.empty[CaseOutcome])) { (c, acc) =>
          for {
            o    <- runCase(c)
            rest <- acc
          } yield o :: rest
        }
        .map { outcomes =>
          val total = outcomes.length
          val passed = outcomes.count(_.pass)
          val failures = outcomes.filterNot(_.pass)
          val header = s"[${cat.category}] $passed/$total cases"
          val failMsg =
            if (failures.isEmpty) ""
            else "\n  failures:\n" + failures.map(o => "  " + o.detail).mkString("\n")
          expect(failures.isEmpty, s"$header$failMsg")
        }
    }
  }
}
