package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicOp._
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser.decode
import weaver.SimpleIOSuite

object JsonLogicGasEstimatorSuite extends SimpleIOSuite {

  private val cfg = GasConfig.Default

  private def parse(s: String): JsonLogicExpression =
    decode[JsonLogicExpression](s).fold(e => throw new RuntimeException(s"parse failed: $e"), identity)

  private def estimateCost(json: String): Long =
    JsonLogicGasEstimator.estimate(parse(json), cfg).cost.amount

  private def meteredGas(json: String, data: JsonLogicValue): IO[Long] =
    JsonLogicEvaluator
      .recursive[IO]
      .evaluateWithGas(parse(json), data, None, GasLimit.Default, cfg)
      .flatMap(IO.fromEither)
      .map(_.gasUsed.amount)

  // ---- baseCost is the shared op->cost table (single source of truth with the evaluator) ----
  test("baseCost mirrors GasConfig for representative ops") {
    IO.pure(
      expect.all(
        JsonLogicGasEstimator.baseCost(AddOp)(cfg) == cfg.add,
        JsonLogicGasEstimator.baseCost(MapOp)(cfg) == cfg.map,
        JsonLogicGasEstimator.baseCost(ReduceOp)(cfg) == cfg.reduce,
        JsonLogicGasEstimator.baseCost(Groth16VerifyOp)(cfg) == cfg.groth16Verify,
        JsonLogicGasEstimator.baseCost(ProveDlogVerifyOp)(cfg) == cfg.proveDlogVerify,
        JsonLogicGasEstimator.baseCost(ProveDhTupleVerifyOp)(cfg) == cfg.proveDhtupleVerify,
        JsonLogicGasEstimator.baseCost(IfElseOp)(cfg) == cfg.ifElse
      )
    )
  }

  // ---- estimate implements the documented static formula (base + depthPenalty + varCost) ----
  test("estimate: hand-computed structural costs") {
    IO.pure(
      expect.all(
        // {"+":[1,2]} = add(5) + depthPenalty(depth 1 = 5); const args are free
        estimateCost("""{"+":[1,2]}""") == cfg.add.amount + cfg.depthPenalty(1L).amount,
        // {"var":"a.b"} = varAccess(2) + dot-segments(2)
        estimateCost("""{"var":"a.b"}""") == cfg.varAccess.amount + 2L,
        // {"+":[{"var":"a"},2]} = add-node(5+5) + var "a"(2+1)
        estimateCost("""{"+":[{"var":"a"},2]}""") ==
          cfg.add.amount + cfg.depthPenalty(1L).amount + cfg.varAccess.amount + 1L
      )
    )
  }

  // ---- the estimate equals the real metered charge for NON-scaling ops (the large common class:
  //      control flow, logic, comparison, var/const) — one grounded source of truth ----
  test("estimate == metered gasUsed: var + nested comparison (no scaling, single path)") {
    val j = """{"and":[{">":[{"var":"x"},1]},{"<":[{"var":"x"},100]}]}"""
    meteredGas(j, MapValue(Map("x" -> IntValue(BigInt(5))))).map(g => expect(estimateCost(j) == g))
  }

  // ---- for ops that DO scale, the static walk omits the scaled term, so it is a floor (<=):
  //      variadic '+' carries an input-scaled (n-1) charge the estimator does not count ----
  test("estimate <= metered gasUsed: variadic arithmetic (scaled term omitted -> floor)") {
    meteredGas("""{"+":[1,2]}""", MapValue.empty).map(g => expect(estimateCost("""{"+":[1,2]}""") <= g))
  }

  // ---- if is modelled as the worst branch: estimate >= actual (only one branch runs) ----
  test("estimate(if) >= metered gasUsed (lazy branch selection)") {
    // condition true -> the cheap then-branch runs; estimate must still cover the heavier else
    val j = """{"if":[{"==":[1,1]},{"+":[1,2]},{"*":[{"*":[3,4]},{"*":[5,6]}]}]}"""
    meteredGas(j, MapValue.empty).map(g => expect(estimateCost(j) >= g))
  }

  // ---- documents the known limitation: input-scaled ops are NOT counted statically ----
  test("scaled ops (map) count base+var only — element count needs the data") {
    // estimate has no per-element term; the metered run over a 3-element array charges more
    val j = """{"map":[{"var":"xs"},{"+":[{"var":""},1]}]}"""
    val data = MapValue(Map("xs" -> ArrayValue(List(IntValue(BigInt(1)), IntValue(BigInt(2)), IntValue(BigInt(3))))))
    meteredGas(j, data).map(g => expect(estimateCost(j) > 0L && estimateCost(j) <= g))
  }
}
