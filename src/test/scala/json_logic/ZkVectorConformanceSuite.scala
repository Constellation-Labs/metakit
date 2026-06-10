package json_logic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicValue.showJsonLogicValue
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator
import io.constellationnetwork.metagraph_sdk.numerics.Ratio

import io.circe.{Decoder, parser}
import weaver.SimpleIOSuite

/**
 * Cross-language conformance suite for the ZK / auth-DB / crypto opcodes.
 *
 * Runs the SHARED ZK opcode test vectors that are also executed by the Rust
 * implementation (`metakit-sdk/rust/jlvm-core/tests/zk_differential.rs`). The
 * vector file is the cross-language source of truth, synced byte-exactly from
 * `metakit-sdk/shared/zk_opcode_test_vectors.json` into
 * `src/test/resources/conformance/zk_opcode_test_vectors.json`. Scala (this
 * repo) is the REFERENCE implementation: every `expected` value in the vectors
 * was produced by (or independently cross-checks) the Scala semantics, so every
 * case must pass here unconditionally.
 *
 * Two kinds of case (the shared error convention):
 *
 *   - VALUE cases carry `expected`. The evaluated result must match it both
 *     STRUCTURALLY (parsed-JSON deep equality, numbers by value) and TEXTUALLY
 *     (the `Show` rendering equals the raw `expected` string), exactly like
 *     [[SharedVectorConformanceSuite]].
 *
 *   - ERROR cases carry `"error": true` and NO `expected`. Evaluation MUST FAIL
 *     (a `Left(JsonLogicException)` or a raised error). If evaluation instead
 *     produces a value, that is a soundness/parity bug and the suite fails.
 *
 * One weaver test is registered per category; each reports per-case pass/fail.
 */
object ZkVectorConformanceSuite extends SimpleIOSuite {

  /**
   * Known cross-language divergences, keyed by (category, raw expr string),
   * with the same xfail-guard policy as [[SharedVectorConformanceSuite]]: a
   * listed case must STILL be failing — if it starts passing, the suite fails
   * loudly so the stale entry gets removed.
   *
   * Currently listed (vectors v1.7.0):
   *
   *   - `ecvrf_verify` / `known_answer`: the single valid-proof ECVRF case. The
   *     v1.7.0 vectors still carry the draft-irtf-cfrg-vrf-10 FOUR-point
   *     challenge proof (mislabelled "RFC 9381"), while the Scala reference
   *     (`MiraclEcVrf25519`) implements the final RFC 9381 §5.4.3 FIVE-point
   *     challenge (suite || 0x02 || Y || H || Gamma || U || V || 0x00) and is
   *     anchored on the official Appendix B.3 vectors. Scala therefore
   *     correctly REJECTS the draft-10 pi (`{valid: false, beta: null}`). The
   *     Rust side is being re-anchored on the official RFC 9381 Appendix B.3
   *     Example 17 pi/beta in vectors v1.8.0; re-syncing v1.8.0 removes these
   *     two entries. The tampered / wrong-width ecvrf cases are unaffected
   *     (false/error in both constructions) and remain enforced.
   */
  private val DivergentEcvrfDraft10Expr: String =
    "{\"ecvrf_verify\":[\"0x3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c\",\"0x72\"," +
    "\"0xf3141cd382dc42909d19ec5110469e4feae18300e94f304590abdced48aed593f7eaf3eb2f1a968cba3f6e23b386aeea" +
    "ab7b1ea44a256e811892e13eeae7c9f6ea8992557453eac11c4d5476b1f35a08\"]}"

  private val KnownDivergences: Set[(String, String)] = Set(
    ("ecvrf_verify", DivergentEcvrfDraft10Expr),
    ("known_answer", DivergentEcvrfDraft10Expr)
  )

  private val VectorsPath = "src/test/resources/conformance/zk_opcode_test_vectors.json"

  final case class Vectors(description: String, version: String, tests: List[Category])
  final case class Category(category: String, note: Option[String], cases: List[VecCase])

  final case class VecCase(
    expr: String,
    data: String,
    expected: Option[String],
    error: Option[Boolean],
    note: Option[String]
  ) {
    def mustError: Boolean = error.contains(true)
  }

  implicit private val caseDecoder: Decoder[VecCase] = Decoder.forProduct5(
    "expr",
    "data",
    "expected",
    "error",
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
      .fold(err => throw new RuntimeException(s"Failed to load shared ZK vectors: $err"), identity)
  }

  /**
   * Structural, numbers-by-value comparison, mirroring Rust's structural check
   * in `zk_differential.rs` (and [[SharedVectorConformanceSuite]]'s `structEq`).
   */
  private def structEq(a: JsonLogicValue, b: JsonLogicValue): Boolean =
    (a, b) match {
      case (NullValue, NullValue)         => true
      case (BoolValue(x), BoolValue(y))   => x == y
      case (StrValue(x), StrValue(y))     => x == y
      case (IntValue(x), IntValue(y))     => x == y
      case (FloatValue(x), FloatValue(y)) => x == y
      case (IntValue(x), FloatValue(y))   => Ratio(x) == y
      case (FloatValue(x), IntValue(y))   => x == Ratio(y)
      case (ArrayValue(xs), ArrayValue(ys)) =>
        xs.length == ys.length && xs.zip(ys).forall { case (p, q) => structEq(p, q) }
      case (MapValue(xs), MapValue(ys)) =>
        xs.keySet == ys.keySet && xs.keys.forall(k => structEq(xs(k), ys(k)))
      case _ => false
    }

  final private case class CaseOutcome(category: String, expr: String, label: String, pass: Boolean, detail: String) {
    def isKnownDivergence: Boolean = KnownDivergences.contains((category, expr))
  }

  private def runCase(category: String, c: VecCase): IO[CaseOutcome] = {
    val label = c.note.fold(c.expr)(n => s"${c.expr}  ($n)")

    def outcome(pass: Boolean, detail: String): CaseOutcome =
      CaseOutcome(category, c.expr, label, pass, detail)

    val parsed: Either[String, (JsonLogicExpression, JsonLogicValue)] =
      for {
        exprJson <- parser.parse(c.expr).left.map(e => s"EXPR-PARSE: ${e.getMessage}")
        expr     <- exprJson.as[JsonLogicExpression].left.map(e => s"EXPR-DECODE: ${e.getMessage}")
        dataJson <- parser.parse(c.data).left.map(e => s"DATA-PARSE: ${e.getMessage}")
        data     <- dataJson.as[JsonLogicValue].left.map(e => s"DATA-DECODE: ${e.getMessage}")
      } yield (expr, data)

    parsed match {
      case Left(err) if c.mustError =>
        // A case the decoder itself rejects still satisfies "evaluation MUST fail".
        IO.pure(outcome(pass = true, s"$label\n    failed as required (decode): $err"))
      case Left(err) =>
        IO.pure(outcome(pass = false, s"$label\n    $err"))
      case Right((expr, data)) =>
        JsonLogicEvaluator
          .tailRecursive[IO]
          .evaluate(expr, data, None)
          .attempt
          .map {
            case Left(raised) =>
              if (c.mustError)
                outcome(pass = true, s"$label\n    failed as required (raised): ${raised.getMessage}")
              else
                outcome(pass = false, s"$label\n    RAISED: ${raised.getMessage}")
            case Right(Left(evalErr)) =>
              if (c.mustError)
                outcome(pass = true, s"$label\n    failed as required: ${evalErr.getMessage}")
              else
                outcome(pass = false, s"$label\n    EVAL-ERR: ${evalErr.getMessage}")
            case Right(Right(result)) =>
              val rendered = showJsonLogicValue.show(result)
              if (c.mustError)
                outcome(
                  pass = false,
                  s"$label\n    expected FAILURE but evaluation succeeded with: $rendered"
                )
              else {
                val expectedRaw = c.expected.getOrElse("")
                val structPass = parser
                  .parse(expectedRaw)
                  .flatMap(_.as[JsonLogicValue])
                  .fold(_ => false, expected => structEq(result, expected))
                val textPass = rendered == expectedRaw
                val detail =
                  s"""$label
                     |    data     = ${c.data}
                     |    expected = $expectedRaw
                     |    got      = $rendered
                     |    struct=${if (structPass) "ok" else "FAIL"} text=${if (textPass) "ok" else "FAIL"}""".stripMargin
                outcome(pass = structPass && textPass, detail)
              }
          }
    }
  }

  // Register one weaver test per category.
  vectors.tests.foreach { cat =>
    test(s"[${cat.category}] shared ZK vectors") {
      cat.cases
        .foldRight(IO.pure(List.empty[CaseOutcome])) { (c, acc) =>
          for {
            o    <- runCase(cat.category, c)
            rest <- acc
          } yield o :: rest
        }
        .map { outcomes =>
          val total = outcomes.length
          val passed = outcomes.count(_.pass)

          val (known, enforced) = outcomes.partition(_.isKnownDivergence)

          // Enforced cases (everything not on the documented-divergence
          // allowlist) MUST pass.
          val failures = enforced.filterNot(_.pass)

          // xfail guard: each known divergence must STILL be failing. If one
          // starts passing, the vectors were re-anchored (v1.8.0 ecvrf re-sync)
          // and the allowlist entry is stale — fail loudly so it gets removed.
          val resolvedDivergences = known.filter(_.pass)

          val header =
            s"[${cat.category}] ${passed}/${total} cases" +
            (if (known.nonEmpty) s", known-divergences ${known.length}" else "")
          val failMsg =
            if (failures.isEmpty) ""
            else "\n  failures:\n" + failures.map(o => "  " + o.detail).mkString("\n")
          val resolvedMsg =
            if (resolvedDivergences.isEmpty) ""
            else
              "\n  known-divergence(s) now PASSING (remove from KnownDivergences):\n" +
              resolvedDivergences.map(o => "  " + o.detail).mkString("\n")

          expect(failures.isEmpty, s"$header$failMsg")
            .and(expect(resolvedDivergences.isEmpty, s"$header$resolvedMsg"))
        }
    }
  }
}
