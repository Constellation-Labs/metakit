package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic._

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object GasMeteringSuite extends SimpleIOSuite with Checkers {

  test("GasCost addition") {
    val cost1 = GasCost(10)
    val cost2 = GasCost(20)
    IO.pure(expect(cost1 + cost2 == GasCost(30)))
  }

  test("GasCost multiplication") {
    val cost = GasCost(10)
    IO.pure(expect(cost * 5 == GasCost(50)))
  }

  test("GasLimit.consume succeeds with sufficient gas") {
    val limit = GasLimit(100)
    val cost = GasCost(30)

    IO.pure(
      limit.consume(cost) match {
        case Right(remaining) => expect(remaining.amount == 70)
        case Left(_)          => failure("Should succeed")
      }
    )
  }

  test("GasLimit.consume fails with insufficient gas") {
    val limit = GasLimit(20)
    val cost = GasCost(50)

    IO.pure(
      limit.consume(cost) match {
        case Left(err) =>
          expect.all(
            err.required.amount == 50,
            err.available.amount == 20
          )
        case Right(_) => failure("Should fail")
      }
    )
  }

  test("GasLimit.canAfford returns true when sufficient") {
    val limit = GasLimit(100)
    val cost = GasCost(50)
    IO.pure(expect(limit.canAfford(cost)))
  }

  test("GasLimit.canAfford returns false when insufficient") {
    val limit = GasLimit(30)
    val cost = GasCost(50)
    IO.pure(expect(!limit.canAfford(cost)))
  }

  test("GasUsed accumulation with GasUsed") {
    val used1 = GasUsed(10)
    val used2 = GasUsed(20)
    IO.pure(expect(used1 + used2 == GasUsed(30)))
  }

  test("GasUsed accumulation with GasCost") {
    val used = GasUsed(10)
    val cost = GasCost(15)
    IO.pure(expect(used + cost == GasUsed(25)))
  }

  test("GasUsed.Zero constant") {
    IO.pure(expect(GasUsed.Zero.amount == 0L))
  }

  test("GasCost.Zero constant") {
    IO.pure(expect(GasCost.Zero.amount == 0L))
  }

  test("GasLimit.Unlimited constant") {
    IO.pure(expect(GasLimit.Unlimited.amount == Long.MaxValue))
  }

  test("GasLimit.Default constant") {
    IO.pure(expect(GasLimit.Default.amount == 1_000_000L))
  }

  test("GasConfig.Default has expected values") {
    val config = GasConfig.Default
    IO.pure(
      expect.all(
        config.add == GasCost(5),
        config.map == GasCost(10),
        config.filter == GasCost(10),
        config.depthPenaltyMultiplier == 5L
      )
    )
  }

  test("GasConfig.Dev has lower costs") {
    val config = GasConfig.Dev
    IO.pure(
      expect.all(
        config.map == GasCost(5),
        config.filter == GasCost(5),
        config.reduce == GasCost(8)
      )
    )
  }

  test("GasConfig.Mainnet has higher costs") {
    val config = GasConfig.Mainnet
    IO.pure(
      expect.all(
        config.pow == GasCost(50),
        config.unique == GasCost(30),
        config.split == GasCost(25),
        config.reduce == GasCost(20),
        config.depthPenaltyMultiplier == 10L
      )
    )
  }

  test("GasConfig.depthPenalty calculates correctly") {
    val config = GasConfig.Default
    IO.pure(
      expect.all(
        config.depthPenalty(0) == GasCost(0),
        config.depthPenalty(1) == GasCost(5),
        config.depthPenalty(2) == GasCost(10),
        config.depthPenalty(5) == GasCost(25)
      )
    )
  }

  test("EvaluationResult.pure has zero gas") {
    val result = EvaluationResult.pure(IntValue(42))
    IO.pure(
      expect.all(
        result.value == IntValue(42),
        result.gasUsed == GasUsed.Zero,
        result.maxDepth == 0,
        result.operationCount == 0
      )
    )
  }

  test("EvaluationResult.withCost") {
    val result = EvaluationResult.withCost(IntValue(42), GasCost(100), depth = 2)
    IO.pure(
      expect.all(
        result.value == IntValue(42),
        result.gasUsed.amount == 100,
        result.maxDepth == 2,
        result.operationCount == 1
      )
    )
  }

  test("evaluateWithGas returns EvaluationResult") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val expr = ConstExpression(IntValue(42))
    val data = MapValue.empty

    evaluator
      .evaluateWithGas(expr, data, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect(result.value == IntValue(42))
      }
  }

  test("evaluateWithGas with simple expression") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val expr = ApplyExpression(
      JsonLogicOp.AddOp,
      List(ConstExpression(IntValue(10)), ConstExpression(IntValue(20)))
    )
    val data = MapValue.empty

    evaluator
      .evaluateWithGas(expr, data, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect(result.value == IntValue(30))
      }
  }

  test("gas consumption is monotonic (property test)") {
    val genCost = Gen.chooseNum(0L, 1000L).map(GasCost(_))

    forall(Gen.listOf(genCost)) { costs =>
      val total = costs.foldLeft(GasUsed.Zero)(_ + _)
      expect(total.amount >= 0)
    }
  }

  test("multiple consume operations") {
    val limit = GasLimit(100)
    val result = for {
      limit1 <- limit.consume(GasCost(20))
      limit2 <- limit1.consume(GasCost(30))
      limit3 <- limit2.consume(GasCost(40))
    } yield limit3

    IO.pure(
      result match {
        case Right(remaining) => expect(remaining.amount == 10)
        case Left(_)          => failure("Should succeed")
      }
    )
  }

  test("consume fails on exact boundary") {
    val limit = GasLimit(50)
    val cost = GasCost(51)

    IO.pure(
      limit.consume(cost) match {
        case Left(_)  => success
        case Right(_) => failure("Should fail when cost exceeds limit")
      }
    )
  }

  test("consume succeeds on exact boundary") {
    val limit = GasLimit(50)
    val cost = GasCost(50)

    IO.pure(
      limit.consume(cost) match {
        case Right(remaining) => expect(remaining.amount == 0)
        case Left(_)          => failure("Should succeed when cost equals limit")
      }
    )
  }

  test("gas exhaustion during map operation") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val largeArray = ArrayValue(List.fill(100)(IntValue(1)))
    val addOne = ApplyExpression(
      JsonLogicOp.AddOp,
      List(VarExpression(Left("")), ConstExpression(IntValue(1)))
    )
    val mapExpr = ApplyExpression(
      JsonLogicOp.MapOp,
      List(ConstExpression(largeArray), ConstExpression(FunctionValue(addOne)))
    )

    val smallLimit = GasLimit(100)

    evaluator
      .evaluateWithGas(mapExpr, MapValue.empty, None, smallLimit, GasConfig.Default)
      .attempt
      .map {
        case Left(_: GasExhaustedException) => success
        case Left(other)                    => failure(s"Expected GasExhaustedException but got: $other")
        case Right(_)                       => failure("Expected gas exhaustion")
      }
  }

  test("gas exhaustion during filter operation") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val largeArray = ArrayValue(List.fill(50)(IntValue(1)))
    val isPositive = ApplyExpression(
      JsonLogicOp.Gt,
      List(VarExpression(Left("")), ConstExpression(IntValue(0)))
    )
    val filterExpr = ApplyExpression(
      JsonLogicOp.FilterOp,
      List(ConstExpression(largeArray), ConstExpression(FunctionValue(isPositive)))
    )

    val smallLimit = GasLimit(80)

    evaluator
      .evaluateWithGas(filterExpr, MapValue.empty, None, smallLimit, GasConfig.Default)
      .attempt
      .map {
        case Left(_: GasExhaustedException) => success
        case Left(other)                    => failure(s"Expected GasExhaustedException but got: $other")
        case Right(_)                       => failure("Expected gas exhaustion")
      }
  }

  test("large array map operation consumes more gas") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val smallArray = ArrayValue(List.fill(5)(IntValue(1)))
    val largeArray = ArrayValue(List.fill(50)(IntValue(1)))
    val addOne = ApplyExpression(
      JsonLogicOp.AddOp,
      List(VarExpression(Left("")), ConstExpression(IntValue(1)))
    )

    val smallMapExpr = ApplyExpression(
      JsonLogicOp.MapOp,
      List(ConstExpression(smallArray), ConstExpression(FunctionValue(addOne)))
    )
    val largeMapExpr = ApplyExpression(
      JsonLogicOp.MapOp,
      List(ConstExpression(largeArray), ConstExpression(FunctionValue(addOne)))
    )

    for {
      smallResult <- evaluator.evaluateWithGas(smallMapExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      largeResult <- evaluator.evaluateWithGas(largeMapExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
    } yield expect.all(
      largeResult.gasUsed.amount > smallResult.gasUsed.amount,
      largeResult.operationCount > smallResult.operationCount
    )
  }

  test("nested operations increase depth and gas cost") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val nestedExpr = ApplyExpression(
      JsonLogicOp.AddOp,
      List(
        ApplyExpression(
          JsonLogicOp.AddOp,
          List(
            ApplyExpression(
              JsonLogicOp.AddOp,
              List(ConstExpression(IntValue(1)), ConstExpression(IntValue(2)))
            ),
            ConstExpression(IntValue(3))
          )
        ),
        ConstExpression(IntValue(4))
      )
    )

    val flatExpr = ApplyExpression(
      JsonLogicOp.AddOp,
      List(ConstExpression(IntValue(1)), ConstExpression(IntValue(2)))
    )

    for {
      nestedResult <- evaluator.evaluateWithGas(nestedExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      flatResult <- evaluator.evaluateWithGas(flatExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
    } yield expect.all(
      nestedResult.gasUsed.amount > flatResult.gasUsed.amount,
      nestedResult.maxDepth > flatResult.maxDepth,
      nestedResult.maxDepth >= 3
    )
  }

  test("all operation short-circuits on first false") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val array = ArrayValue(List(IntValue(1), IntValue(0), IntValue(1), IntValue(1)))
    val isPositive = ApplyExpression(
      JsonLogicOp.Gt,
      List(VarExpression(Left("")), ConstExpression(IntValue(0)))
    )
    val allExpr = ApplyExpression(
      JsonLogicOp.AllOp,
      List(ConstExpression(array), ConstExpression(FunctionValue(isPositive)))
    )

    evaluator
      .evaluateWithGas(allExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect.all(
          result.value == BoolValue(false),
          result.operationCount < 10
        )
      }
  }

  test("find operation short-circuits on first match") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val array = ArrayValue(List.fill(100)(IntValue(0)) ++ List(IntValue(42)) ++ List.fill(100)(IntValue(0)))
    val isFortyTwo = ApplyExpression(
      JsonLogicOp.EqOp,
      List(VarExpression(Left("")), ConstExpression(IntValue(42)))
    )
    val findExpr = ApplyExpression(
      JsonLogicOp.FindOp,
      List(ConstExpression(array), ConstExpression(FunctionValue(isFortyTwo)))
    )

    evaluator
      .evaluateWithGas(findExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect.all(
          result.value == IntValue(42),
          result.operationCount < 200
        )
      }
  }

  test("reduce operation with large array") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val array = ArrayValue(List.fill(20)(IntValue(1)))
    val sumExpr = ApplyExpression(
      JsonLogicOp.AddOp,
      List(
        VarExpression(Left("accumulator")),
        VarExpression(Left("current"))
      )
    )
    val reduceExpr = ApplyExpression(
      JsonLogicOp.ReduceOp,
      List(
        ConstExpression(array),
        ConstExpression(FunctionValue(sumExpr))
      )
    )

    evaluator
      .evaluateWithGas(reduceExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect.all(
          result.value == IntValue(20),
          result.gasUsed.amount > 100
        )
      }
  }

  test("variable access with deep path") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val deepData = MapValue(
      Map(
        "level1" -> MapValue(
          Map(
            "level2" -> MapValue(
              Map(
                "level3" -> MapValue(
                  Map("value" -> IntValue(42))
                )
              )
            )
          )
        )
      )
    )
    val varExpr = VarExpression(Left("level1.level2.level3.value"))

    evaluator
      .evaluateWithGas(varExpr, deepData, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect.all(
          result.value == IntValue(42),
          result.gasUsed.amount > 0
        )
      }
  }

  test("unique operation with large duplicate array") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val arrayWithDupes = ArrayValue(List.fill(50)(IntValue(1)) ++ List.fill(50)(IntValue(2)))
    val uniqueExpr = ApplyExpression(
      JsonLogicOp.UniqueOp,
      List(ConstExpression(arrayWithDupes))
    )

    evaluator
      .evaluateWithGas(uniqueExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect.all(
          result.value == ArrayValue(List(IntValue(1), IntValue(2))),
          result.gasUsed.amount > 50
        )
      }
  }

  test("pow operation with large exponent consumes more gas") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val smallPow = ApplyExpression(
      JsonLogicOp.PowOp,
      List(ConstExpression(IntValue(2)), ConstExpression(IntValue(3)))
    )
    val largePow = ApplyExpression(
      JsonLogicOp.PowOp,
      List(ConstExpression(IntValue(2)), ConstExpression(IntValue(100)))
    )

    for {
      smallResult <- evaluator.evaluateWithGas(smallPow, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      largeResult <- evaluator.evaluateWithGas(largePow, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
    } yield expect(largeResult.gasUsed.amount > smallResult.gasUsed.amount)
  }

  test("if-else operation only evaluates taken branch") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val expensiveOp = ApplyExpression(
      JsonLogicOp.MapOp,
      List(
        ConstExpression(ArrayValue(List.fill(100)(IntValue(1)))),
        ConstExpression(
          FunctionValue(
            ApplyExpression(JsonLogicOp.AddOp, List(VarExpression(Left("")), ConstExpression(IntValue(1))))
          )
        )
      )
    )
    val ifExpr = ApplyExpression(
      JsonLogicOp.IfElseOp,
      List(
        ConstExpression(BoolValue(true)),
        ConstExpression(IntValue(42)),
        expensiveOp
      )
    )

    evaluator
      .evaluateWithGas(ifExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect.all(
          result.value == IntValue(42),
          result.gasUsed.amount < 100
        )
      }
  }

  test("gas cost is predictable for same operation") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val expr = ApplyExpression(
      JsonLogicOp.AddOp,
      List(ConstExpression(IntValue(10)), ConstExpression(IntValue(20)))
    )

    for {
      result1 <- evaluator.evaluateWithGas(expr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      result2 <- evaluator.evaluateWithGas(expr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
    } yield expect.all(
      result1.gasUsed == result2.gasUsed,
      result1.operationCount == result2.operationCount,
      result1.maxDepth == result2.maxDepth
    )
  }

  test("count operation with predicate") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val array = ArrayValue(List(IntValue(1), IntValue(2), IntValue(3), IntValue(4), IntValue(5)))
    val isEven = ApplyExpression(
      JsonLogicOp.EqOp,
      List(
        ApplyExpression(JsonLogicOp.ModuloOp, List(VarExpression(Left("")), ConstExpression(IntValue(2)))),
        ConstExpression(IntValue(0))
      )
    )
    val countExpr = ApplyExpression(
      JsonLogicOp.CountOp,
      List(ConstExpression(array), ConstExpression(FunctionValue(isEven)))
    )

    evaluator
      .evaluateWithGas(countExpr, MapValue.empty, None, GasLimit.Default, GasConfig.Default)
      .map { result =>
        expect.all(
          result.value == IntValue(2),
          result.gasUsed.amount > 0
        )
      }
  }
}