package json_logic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.core.{JsonLogicExpression, JsonLogicValue}
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasConfig, GasExhaustedException, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.syntax._
import io.circe.{Json, JsonObject, Printer, parser}

/**
 * Generator for the cross-language GAS conformance vectors
 * (`src/test/resources/conformance/gas_test_vectors.json`).
 *
 * Every `expected` value in the vector file is PRODUCED BY RUNNING the Scala
 * gas meter (`JsonLogicEvaluator.evaluateWithGas` with `GasConfig.Default`) —
 * never hand-computed. Re-run after any change to the charging contract:
 *
 *   sbt "Test/runMain json_logic.GasVectorGenerator"
 *
 * Each case is evaluated twice:
 *   - once with `GasLimit.Unlimited` to measure the true gas consumption
 *     (used to resolve the relative limit specs `Exact` / `ExactMinus`), and
 *   - once with the declared `gasLimit`, producing `expected`: the exact
 *     `gasUsed` integer on success, or the string "OOG" when the meter raises
 *     `GasExhaustedException`.
 *
 * Any other evaluation failure aborts generation: gas vectors must only
 * contain programs that either succeed or run out of gas.
 */
object GasVectorGenerator extends IOApp {

  private val OutPath = "src/test/resources/conformance/gas_test_vectors.json"

  sealed private trait LimitSpec
  private case class Abs(amount: Long) extends LimitSpec

  /** Resolved to the measured (unlimited) gas consumption: an exactly-at-limit success. */
  private case object Exact extends LimitSpec

  /** Resolved to measured - n: an out-of-gas case n short of the requirement. */
  private case class ExactMinus(n: Long) extends LimitSpec

  final private case class CaseSpec(
    expr: String,
    data: String,
    limit: LimitSpec,
    note: Option[String] = None
  )

  final private case class CategorySpec(category: String, note: Option[String], cases: List[CaseSpec])

  // --- crypto fixtures (verified-true cases lifted from the shared ZK opcode vectors v1.8.0) ----

  private val PoseidonTwoInputs =
    "{\"poseidon\":[\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"0x0000000000000000000000000000000000000000000000000000000000000002\"]}"

  private val PoseidonFourInputs =
    "{\"poseidon\":[\"0x0000000000000000000000000000000000000000000000000000000000000004\",\"0x0000000000000000000000000000000000000000000000000000000000000005\",\"0x0000000000000000000000000000000000000000000000000000000000000006\",\"0x0000000000000000000000000000000000000000000000000000000000000007\"]}"

  private val PmtVerifyEightSiblings =
    "{\"pmt_verify\":[\"0x047e3f50a0bf1da6c86860a77474b1f6ee1a807660c2556f34e046f34155f54f\",\"0x0cc5c2f21d3b979fa5284982d35ceccb66333b93d325333dd780a6a3ced1c5f5\",42,[\"0x166f24e25f67126bbff81d9c8f064c913d2127f180a906dfed933ecf251a56bc\",\"0x2dee93c5a666459646ea7d22cca9e1bcfed71e6951b953611d11dda32ea09d78\",\"0x1445dc1092ecebc0a6001d45b9bd4d85705c66e435abc23436da8d39a6f37d08\",\"0x07f9d837cb17b0d36320ffe93ba52345f1b728571a568265caac97559dbc952a\",\"0x18f43331537ee2af2e3d758d50f72106467c6eea50371dd528d57eb2b856d238\",\"0x1069673dcdb12263df301a6ff584a7ec261a44cb9dc68df067a4774460b1f1e1\",\"0x2098f5fb9e239eab3ceac3f27b81e481dc3124d55ffed523a839ee8446b64864\",\"0x0000000000000000000000000000000000000000000000000000000000000000\"]]}"

  private val SchnorrVerifyValid =
    "{\"schnorr_verify\":[\"0x234403317325635150f68fd4b3403ae81e7513abee02b83478fb37c7ae1b47db0bf44b96107a81ab3b5192e1f2f920eb4fe6a382d6df663e8af2ae97cf8e3ae5\",\"0x617574686f72697a65207472616e73666572\",\"0x18186954bd891097c44985153dfbbee526e9bfa9798037c42e0e23ca023d5c942a9cb437b23ce7ff0d0a1dccabd0ec7619d7d045b3134a98f7d7eb34b3fd9a470ba9e88831ee2663248a9087524a7034a9eef889e7d912f2cfe8798045071663\"]}"

  private val BlsVerifyValid =
    "{\"bls_verify\":[\"0x864350e49b8b46468478af45cfb9c167357c8701e0c7a2dcd401028de506080f632400e2059ab58532eb2b3912078d80\",\"0x636f6e7374656c6c6174696f6e2d736e617073686f742d30783031\",\"0xa816e2440371eea63b85484f0111914874974cfb8f83833b214ba365bc1bc46cfd070d75c8decb6e9d9bcea0e2a2b92214cfe0bed5c00a7702741a2e92186454f76ba5e4e86804908e7a2f38a0f123941b3513bff5a4af6951c6c7a8e61b04ee\"]}"

  private val BlsAggregateVerifyFourKeys =
    "{\"bls_aggregate_verify\":[[\"0x96512b63cded51762b89ba53811524508ad33a3a990306e5e07097c787ad801dfb160ec6959472b9a188cafcc101f282\",\"0xb0de40cd41c728cd90408081b764b7cc40889dd3dbd499f2f6f771455e1ea799ff859f401db923933ca48a695dc6c3f3\",\"0xb0d93cc62d599b3557eb65a9a08519d9b1a96e5090ac9fadf84ccb6e090a2298edf3a552000dfd0338a37dc86ba65a49\",\"0xaa6230be32948f5f5f746fe050250e3c003d0f00827131381e3e8e8cb2d2bd8fe6d37c10b3433074c6dfa5dee04d1cdd\"],\"0x636f6d6d69747465652d726f756e642d37\",\"0xa3f4674d9b713ca0598e394a19c98e5312eafd2b4e3698b41090651332d507d330d5a9e36aa46f8247ec84e1e0302c1c08bdd8f7944dc7a8daa0cb8c07b6c3837015b6c8533247c1c8876102d9650857c00924f9d7999f4df8a2a30af33c48d4\"]}"

  // --- the corpus -------------------------------------------------------------------------------

  private val Corpus: List[CategorySpec] = List(
    CategorySpec(
      "constants",
      Some("Literals are never charged: constants, array literals and object literals cost 0 gas."),
      List(
        CaseSpec("42", "{}", Abs(1000)),
        CaseSpec("[1, 2, 3]", "{}", Abs(1000), Some("array literal: elements are constants, no op charge")),
        CaseSpec("{\"a\": 1, \"b\": 2}", "{}", Abs(1000), Some("object literal: no op charge"))
      )
    ),
    CategorySpec(
      "simple_ops",
      Some("Single operation over constant args: base(op) + depthPenalty(1) [+ input-scaled term]."),
      List(
        CaseSpec("{\"!\": [true]}", "{}", Abs(1000)),
        CaseSpec("{\"==\": [1, 1]}", "{}", Abs(1000)),
        CaseSpec(
          "{\"max\": [1, 2, 3]}",
          "{}",
          Abs(1000),
          Some("max over a 3-arg list adds sizeCost(3) on top of base + depth")
        ),
        CaseSpec("{\"typeof\": [42]}", "{}", Abs(1000))
      )
    ),
    CategorySpec(
      "control_flow",
      Some(
        "if/let are evaluated lazily but still charge their flat base cost (ifElse = 10) once per node " +
        "at the dispatch site, with NO depth penalty (depth is undefined at the lazy dispatch site; " +
        "see the GasConfig schedule comment). Condition / bindings / taken branch pay for themselves; " +
        "untaken branches pay nothing."
      ),
      List(
        CaseSpec("{\"if\": [true, 1, 2]}", "{}", Abs(1000), Some("constant condition + constant branch: only the if base cost")),
        CaseSpec(
          "{\"if\": [{\">\": [5, 3]}, {\"+\": [1, 2]}, 99]}",
          "{}",
          Abs(1000),
          Some("condition + taken then-branch are charged; untaken else-branch is not")
        ),
        CaseSpec(
          "{\"let\": [{\"a\": 1}, {\"var\": \"a\"}]}",
          "{}",
          Abs(1000),
          Some("constant binding: let base cost + the var lookup")
        ),
        CaseSpec(
          "{\"let\": [{\"a\": {\"+\": [1, 2]}}, {\"+\": [{\"var\": \"a\"}, 1]}]}",
          "{}",
          Abs(1000),
          Some("let base cost + binding expression + result expression")
        )
      )
    ),
    CategorySpec(
      "arithmetic_depth",
      Some("Nested arithmetic: each op charges once with depthPenalty(5 * height-of-op-over-its-args); no subtree re-charge."),
      List(
        CaseSpec("{\"+\": [1, 2]}", "{}", Abs(1000)),
        CaseSpec("{\"+\": [1, {\"+\": [2, 3]}]}", "{}", Abs(1000)),
        CaseSpec("{\"+\": [{\"+\": [1, {\"+\": [2, 3]}]}, 4]}", "{}", Abs(1000), Some("3-deep chain: depth penalties 5/10/15")),
        CaseSpec("{\"*\": [{\"+\": [1, 2]}, {\"-\": [5, 3]}]}", "{}", Abs(1000), Some("two depth-1 children under a depth-2 parent")),
        CaseSpec("{\"pow\": [2, 10]}", "{}", Abs(1000), Some("pow adds |exponent| as an input-scaled term"))
      )
    ),
    CategorySpec(
      "var_paths",
      Some("Variable lookups charge varAccess(2) + #pathSegments once at lookup time (Java String.split('.') segment count)."),
      List(
        CaseSpec("{\"var\": \"x\"}", "{\"x\": 42}", Abs(1000)),
        CaseSpec("{\"var\": \"a.b.c\"}", "{\"a\": {\"b\": {\"c\": 123}}}", Abs(1000), Some("3 path segments")),
        CaseSpec("{\"var\": \"\"}", "{\"x\": 1}", Abs(1000), Some("whole-data access: empty key still counts 1 segment")),
        CaseSpec(
          "{\"var\": [\"missing\", \"fallback\"]}",
          "{}",
          Abs(1000),
          Some("lookup is charged even when the default is substituted")
        )
      )
    ),
    CategorySpec(
      "collections",
      Some("Collection ops add sizeCost(#elements) up front; per-element callback runs charge their own ops against the same counter."),
      List(
        CaseSpec("{\"map\": [[1, 2, 3], {\"+\": [{\"var\": \"\"}, 1]}]}", "{}", Abs(1000)),
        CaseSpec("{\"filter\": [[1, 2, 3, 4], {\">\": [{\"var\": \"\"}, 2]}]}", "{}", Abs(1000)),
        CaseSpec(
          "{\"reduce\": [[1, 2, 3], {\"+\": [{\"var\": \"current\"}, {\"var\": \"accumulator\"}]}, 0]}",
          "{}",
          Abs(1000)
        ),
        CaseSpec(
          "{\"all\": [[1, 2, 3], {\">\": [{\"var\": \"\"}, 0]}]}",
          "{}",
          Abs(1000),
          Some("all evaluates the predicate for EVERY element (no short-circuit), charging each run")
        ),
        CaseSpec(
          "{\"merge\": [[1, 2], [3, 4]]}",
          "{}",
          Abs(1000),
          Some("merge charges an output-scaled residual: sizeCost(#merged elements) after the primitive")
        )
      )
    ),
    CategorySpec(
      "strings",
      Some("cat/join pre-charge the coerced output length from the inputs; split/substr post-charge an output residual."),
      List(
        CaseSpec("{\"cat\": [\"foo\", \"bar\"]}", "{}", Abs(1000), Some("input-scaled: sum of coerced arg lengths")),
        CaseSpec(
          "{\"join\": [[\"a\", \"b\", \"c\"], \"-\"]}",
          "{}",
          Abs(1000),
          Some("input-scaled: element lengths + separator * (n-1)")
        ),
        CaseSpec("{\"split\": [\"a,b,c\", \",\"]}", "{}", Abs(1000), Some("output residual: 2 * #pieces, charged after the primitive")),
        CaseSpec("{\"substr\": [\"hello world\", 0, 5]}", "{}", Abs(1000), Some("output residual: produced string length"))
      )
    ),
    CategorySpec(
      "crypto",
      Some(
        "ZK / crypto opcodes (fixtures lifted from the shared ZK opcode vectors; all verify successfully). " +
        "Per-element components (poseidon per-input, pmt per-sibling, bls-aggregate per-key) are pre-charged from the args."
      ),
      List(
        CaseSpec(PoseidonTwoInputs, "{}", Abs(1_000_000), Some("poseidon: 150 base + 150 per input (2 inputs)")),
        CaseSpec(PoseidonFourInputs, "{}", Abs(1_000_000), Some("poseidon: 150 base + 150 per input (4 inputs)")),
        CaseSpec(PmtVerifyEightSiblings, "{}", Abs(1_000_000), Some("pmt_verify: 200 base + 300 per sibling (8 siblings)")),
        CaseSpec(SchnorrVerifyValid, "{}", Abs(1_000_000), Some("schnorr_verify: flat 45000")),
        CaseSpec(BlsVerifyValid, "{}", Abs(1_000_000), Some("bls_verify: flat 120000")),
        CaseSpec(
          BlsAggregateVerifyFourKeys,
          "{}",
          Abs(1_000_000),
          Some("bls_aggregate_verify: 120000 base + 15000 per public key (4 keys)")
        )
      )
    ),
    CategorySpec(
      "oog",
      Some(
        "Out-of-gas behavior. expected = \"OOG\" asserts the meter fails with the distinct gas-exhaustion error; " +
        "an integer asserts success with that exact gasUsed. NOTE the gas-starved var case: the runtime swallows " +
        "a failed lookup into default/null, so a var lookup that cannot afford its charge yields null and consumes 0."
      ),
      List(
        CaseSpec(
          PoseidonTwoInputs,
          "{}",
          Abs(400),
          Some(
            "size-scaled pre-charge OOG: limit covers poseidon base(150)+depth(5) but not the per-input " +
            "term (300); OOG fires BEFORE the permutation runs"
          )
        ),
        CaseSpec("{\"+\": [1, {\"+\": [2, 3]}]}", "{}", Exact, Some("exactly-at-limit: succeeds with gasUsed == gasLimit")),
        CaseSpec("{\"+\": [1, {\"+\": [2, 3]}]}", "{}", ExactMinus(1), Some("one gas short of the requirement")),
        CaseSpec(
          "{\"+\": [{\"*\": [3, 4]}, {\"*\": [5, 6]}]}",
          "{}",
          Abs(20),
          Some("OOG mid-args: first multiplication fits, the second one's pre-charge does not")
        ),
        CaseSpec(
          "{\"cat\": [\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\", \"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"]}",
          "{}",
          Abs(50),
          Some("cat input-scaled pre-charge OOG: base(5)+depth(5) fit, the 60-char length term does not")
        ),
        CaseSpec(
          "{\"var\": \"x\"}",
          "{\"x\": 42}",
          Abs(2),
          Some(
            "gas-starved var lookup: the lookup needs 3 (varAccess 2 + 1 segment) but only 2 remain; the " +
            "runtime swallows the failure into null and consumes NOTHING (gasUsed = 0, evaluation succeeds)"
          )
        ),
        CaseSpec(
          "{\"map\": [[1, 2, 3, 4, 5], {\"+\": [{\"var\": \"\"}, 1]}]}",
          "{}",
          Abs(55),
          Some("OOG inside a callback run: the third element's `+` pre-charge exhausts the counter")
        )
      )
    )
  )

  // --- generation -------------------------------------------------------------------------------

  private val evaluator = JsonLogicEvaluator.tailRecursive[IO]

  private def parse(c: CaseSpec): IO[(JsonLogicExpression, JsonLogicValue)] =
    IO.fromEither(
      (for {
        exprJson <- parser.parse(c.expr).left.map(e => s"expr parse: $e")
        expr     <- exprJson.as[JsonLogicExpression].left.map(e => s"expr decode: $e")
        dataJson <- parser.parse(c.data).left.map(e => s"data parse: $e")
        data     <- dataJson.as[JsonLogicValue].left.map(e => s"data decode: $e")
      } yield (expr, data)).left.map(msg => new RuntimeException(s"${c.expr}: $msg"))
    )

  private def runCase(category: String, c: CaseSpec): IO[Json] =
    for {
      parsed <- parse(c)
      (expr, data) = parsed
      measuredRes <- evaluator.evaluateWithGas(expr, data, None, GasLimit.Unlimited, GasConfig.Default)
      measured <- measuredRes match {
        case Right(r) => IO.pure(r.gasUsed.amount)
        case Left(err) =>
          IO.raiseError(new RuntimeException(s"[$category] ${c.expr}: unlimited run failed: ${err.getMessage}"))
      }
      limit = c.limit match {
        case Abs(n)        => n
        case Exact         => measured
        case ExactMinus(n) => measured - n
      }
      limitedRes <- evaluator.evaluateWithGas(expr, data, None, GasLimit(limit), GasConfig.Default)
      expected <- limitedRes match {
        case Right(r)                       => IO.pure(Json.fromLong(r.gasUsed.amount))
        case Left(_: GasExhaustedException) => IO.pure(Json.fromString("OOG"))
        case Left(err) =>
          IO.raiseError(new RuntimeException(s"[$category] ${c.expr}: limited run failed NON-gas: ${err.getMessage}"))
      }
      _ <- IO.println(f"[$category%-16s] measured=$measured%8d limit=$limit%8d expected=${expected.noSpaces}%8s  ${c.expr.take(60)}")
    } yield
      Json.fromJsonObject(
        JsonObject.fromIterable(
          List("expr" := c.expr, "data" := c.data, "gasLimit" := limit, "expected" -> expected) ++
          c.note.map(n => "note" := n).toList
        )
      )

  private def categoryJson(spec: CategorySpec): IO[Json] =
    spec.cases.traverse(runCase(spec.category, _)).map { cases =>
      Json.fromJsonObject(
        JsonObject.fromIterable(
          List("category" := spec.category) ++
          spec.note.map(n => "note" := n).toList ++
          List("cases" := Json.fromValues(cases))
        )
      )
    }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      categories <- Corpus.traverse(categoryJson)
      doc = Json.fromJsonObject(
        JsonObject.fromIterable(
          List(
            "description" := (
              "JLVM gas-metering cross-language test vectors. Scala (metakit) is the reference gas meter; " +
              "every implementation must reproduce `expected` EXACTLY: the integer gasUsed reported when " +
              "evaluating `expr` against `data` under `gasLimit` with the default gas schedule, or the " +
              "string \"OOG\" when metering must fail with the distinct gas-exhaustion error. The charging " +
              "contract is normative per metakit PR #37: each op consumes exactly once " +
              "base(op) + depthPenalty + inputScaledCost atomically BEFORE the primitive runs, plus an " +
              "output-scaled residual after it for split/merge/flatten/slice/substr only; var lookups " +
              "consume varAccess + #pathSegments at lookup; the lazily-dispatched if/let charge their " +
              "flat base cost once per node at the dispatch site with NO depth penalty (untaken branches " +
              "cost nothing); gasUsed is the gas-counter delta. Generated by " +
              "`sbt \"Test/runMain json_logic.GasVectorGenerator\"` — " +
              "expected values are PRODUCED BY RUNNING the Scala meter, never hand-computed."
            ),
            "version" := "1.1.0",
            "tests" := Json.fromValues(categories)
          )
        )
      )
      rendered = Printer.spaces2.copy(colonLeft = " ").print(doc) + "\n"
      _ <- IO(Files.write(Paths.get(OutPath), rendered.getBytes(StandardCharsets.UTF_8)))
      _ <- IO.println(s"\nWrote $OutPath")
    } yield ExitCode.Success
}
