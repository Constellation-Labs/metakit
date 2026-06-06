package json_logic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicValue.showJsonLogicValue
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.{Decoder, parser}
import weaver.SimpleIOSuite

/**
 * Cross-language conformance suite for the base JSON Logic VM.
 *
 * Runs the SHARED test vectors that are also executed by the Rust
 * (`metakit-sdk/rust/jlvm-core/tests/differential.rs`) and TypeScript
 * (`metakit-sdk/packages/typescript/tests/json-logic-vectors.test.ts`)
 * implementations. The vector file is the cross-language source of truth, synced
 * from `metakit-sdk/shared/json_logic_test_vectors.json` into
 * `src/test/resources/conformance/json_logic_test_vectors.json`.
 *
 * Comparison strategy (matched to the Rust/TS oracles):
 *
 *   1. STRUCTURAL (primary): the `expected` string is parsed as JSON into a
 *      [[JsonLogicValue]] and compared against the evaluated value with numbers
 *      compared by value (int-vs-float tolerant), exactly like Rust's
 *      `json_struct_eq` and TS's `toEqual`. This is the authoritative assertion.
 *
 *   2. TEXTUAL (secondary): the evaluated value is rendered via `Show` into the
 *      same textual JSON form the vectors use (spaces after `,` and `:`,
 *      e.g. `[1, 2, 3]` / `{"a": 1, "b": 2}`) and is checked to equal the raw
 *      `expected` string. This guards the exact rendering format.
 *
 * One weaver test is registered per category; each reports per-case pass/fail.
 */
object SharedVectorConformanceSuite extends SimpleIOSuite {

  private val VectorsPath = "src/test/resources/conformance/json_logic_test_vectors.json"

  final case class Vectors(description: String, version: String, tests: List[Category])
  final case class Category(category: String, note: Option[String], cases: List[VecCase])
  final case class VecCase(expr: String, data: String, expected: String, note: Option[String])

  private implicit val caseDecoder: Decoder[VecCase] = Decoder.forProduct4(
    "expr",
    "data",
    "expected",
    "note"
  )(VecCase.apply)

  private implicit val categoryDecoder: Decoder[Category] = Decoder.forProduct3(
    "category",
    "note",
    "cases"
  )(Category.apply)

  private implicit val vectorsDecoder: Decoder[Vectors] = Decoder.forProduct3(
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
      .fold(err => throw new RuntimeException(s"Failed to load shared vectors: $err"), identity)
  }

  /**
   * Structural, numbers-by-value comparison, mirroring Rust's `json_struct_eq`
   * and the TS harness's `toEqual`. Maps and arrays are compared deeply; numeric
   * leaves compare by BigDecimal value so `IntValue(5)` matches `FloatValue(5.0)`.
   */
  private def structEq(a: JsonLogicValue, b: JsonLogicValue): Boolean =
    (a, b) match {
      case (NullValue, NullValue)         => true
      case (BoolValue(x), BoolValue(y))   => x == y
      case (StrValue(x), StrValue(y))     => x == y
      case (IntValue(x), IntValue(y))     => x == y
      case (FloatValue(x), FloatValue(y)) => x == y
      case (IntValue(x), FloatValue(y))   => BigDecimal(x) == y
      case (FloatValue(x), IntValue(y))   => x == BigDecimal(y)
      case (ArrayValue(xs), ArrayValue(ys)) =>
        xs.length == ys.length && xs.zip(ys).forall { case (p, q) => structEq(p, q) }
      case (MapValue(xs), MapValue(ys)) =>
        xs.keySet == ys.keySet && xs.keys.forall(k => structEq(xs(k), ys(k)))
      case _ => false
    }

  /**
   * Known cross-language divergences: cases the shared vectors (and the Rust/TS
   * reference impls) cover but the Scala JLVM does NOT yet implement. These are
   * REAL discrepancies (not explained by any unmerged fix). Per the conformance
   * policy we do not alter the vectors or the JLVM semantics here; instead we
   * record each divergence so the suite stays green while keeping the gap loud:
   * if a listed case ever starts passing, the suite FAILS, signaling that the
   * entry must be removed.
   *
   * Keyed by (category, raw expr string). Documented for review:
   *
   *   - object / `{"get": [map, key, default]}`: Scala `get` only accepts the
   *     2-arg `[map, key]` form (missing key -> null) and errors on a 3rd
   *     `default` arg. Rust/TS accept the 3-arg default form
   *     (see metakit-sdk rust/jlvm-core/src/eval.rs::op_get, which explicitly
   *     notes "Scala handleGetOp only supports [Map, Str]").
   *
   *   - let_bindings / object-form `{"let": [{name: expr, ...}, result]}`: Scala
   *     `let` only accepts the array-of-pairs form
   *     `{"let": [[[name, expr], ...], result]}`. Rust/TS additionally accept the
   *     object form used by the vectors
   *     (see rust/jlvm-core/src/eval.rs::eval_let "convenience object form ...
   *     as used by the conformance vectors").
   */
  private val KnownDivergences: Set[(String, String)] = Set(
    "object" -> """{"get": [{"var": "obj"}, "missing", "default"]}""",
    "let_bindings" -> """{"let": [{"x": 5}, {"var": "x"}]}""",
    "let_bindings" -> """{"let": [{"x": 5}, {"+": [{"var": "x"}, 1]}]}""",
    "let_bindings" -> """{"let": [{"doubled": {"*": [{"var": "x"}, 2]}}, {"+": [{"var": "doubled"}, 1]}]}"""
  )

  private final case class CaseOutcome(
    category:     String,
    expr:         String,
    label:        String,
    structPass:   Boolean,
    textPass:     Boolean,
    detail:       String
  ) {
    def isKnownDivergence: Boolean = KnownDivergences.contains((category, expr))
  }

  private def runCase(category: String, c: VecCase): IO[CaseOutcome] = {
    val label = c.note.fold(c.expr)(n => s"${c.expr}  ($n)")

    def outcome(structPass: Boolean, textPass: Boolean, detail: String): CaseOutcome =
      CaseOutcome(category, c.expr, label, structPass, textPass, detail)

    val parsed: Either[String, (JsonLogicExpression, JsonLogicValue, JsonLogicValue)] =
      for {
        exprJson     <- parser.parse(c.expr).left.map(e => s"EXPR-PARSE: ${e.getMessage}")
        expr         <- exprJson.as[JsonLogicExpression].left.map(e => s"EXPR-DECODE: ${e.getMessage}")
        dataJson     <- parser.parse(c.data).left.map(e => s"DATA-PARSE: ${e.getMessage}")
        data         <- dataJson.as[JsonLogicValue].left.map(e => s"DATA-DECODE: ${e.getMessage}")
        expectedJson <- parser.parse(c.expected).left.map(e => s"EXPECTED-PARSE: ${e.getMessage}")
        expected     <- expectedJson.as[JsonLogicValue].left.map(e => s"EXPECTED-DECODE: ${e.getMessage}")
      } yield (expr, data, expected)

    parsed match {
      case Left(err) =>
        IO.pure(outcome(structPass = false, textPass = false, s"$label\n    $err"))
      case Right((expr, data, expected)) =>
        JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None).map {
          case Left(evalErr) =>
            outcome(structPass = false, textPass = false, s"$label\n    EVAL-ERR: ${evalErr.getMessage}")
          case Right(result) =>
            val sOk      = structEq(result, expected)
            val rendered = showJsonLogicValue.show(result)
            val tOk      = rendered == c.expected
            val detail =
              s"""$label
                 |    data     = ${c.data}
                 |    expected = ${c.expected}
                 |    got      = $rendered
                 |    struct=${if (sOk) "ok" else "FAIL"} text=${if (tOk) "ok" else "FAIL"}""".stripMargin
            outcome(structPass = sOk, textPass = tOk, detail)
        }
    }
  }

  // Register one weaver test per category.
  vectors.tests.foreach { cat =>
    test(s"[${cat.category}] shared vectors") {
      cat.cases.traverseOutcomes(cat.category).map { outcomes =>
        val total      = outcomes.length
        val structPass = outcomes.count(_.structPass)
        val textPass   = outcomes.count(_.textPass)

        val (known, enforced) = outcomes.partition(_.isKnownDivergence)

        // Enforced cases (everything not on the documented-divergence allowlist)
        // MUST pass. Structural is authoritative (matches the Rust/TS oracles);
        // textual is a secondary rendering-format check.
        val structFailures   = enforced.filterNot(_.structPass)
        val textOnlyFailures = enforced.filter(o => o.structPass && !o.textPass)

        // xfail guard: each known divergence must STILL be failing. If one starts
        // passing, the JLVM gained the feature and the allowlist entry is stale —
        // fail loudly so it gets removed.
        val resolvedDivergences = known.filter(_.structPass)

        val header =
          s"[${cat.category}] struct ${structPass}/${total}, text ${textPass}/${total}" +
            (if (known.nonEmpty) s", known-divergences ${known.length}" else "")

        val structMsg =
          if (structFailures.isEmpty) ""
          else "\n  structural failures:\n" + structFailures.map(o => "  " + o.detail).mkString("\n")
        val textMsg =
          if (textOnlyFailures.isEmpty) ""
          else "\n  textual-only divergences:\n" + textOnlyFailures.map(o => "  " + o.detail).mkString("\n")
        val resolvedMsg =
          if (resolvedDivergences.isEmpty) ""
          else
            "\n  known-divergence(s) now PASSING (remove from KnownDivergences):\n" +
              resolvedDivergences.map(o => "  " + o.detail).mkString("\n")

        expect(structFailures.isEmpty, s"$header$structMsg$textMsg")
          .and(expect(textOnlyFailures.isEmpty, s"$header$structMsg$textMsg"))
          .and(expect(resolvedDivergences.isEmpty, s"$header$resolvedMsg"))
      }
    }
  }

  // Small helper to evaluate all cases in a category sequentially.
  private implicit class CaseListOps(cs: List[VecCase]) {
    def traverseOutcomes(category: String): IO[List[CaseOutcome]] =
      cs.foldRight(IO.pure(List.empty[CaseOutcome])) { (c, acc) =>
        for {
          o    <- runCase(category, c)
          rest <- acc
        } yield o :: rest
      }
  }
}
