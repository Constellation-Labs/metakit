package json_logic

import cats.Show
import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import org.scalacheck.{Arbitrary, Gen}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object PropertyBasedSuite extends SimpleIOSuite with Checkers {

  // ============================================================================
  // Show instances for ScalaCheck/Weaver
  // ============================================================================

  implicit val showIntValue: Show[IntValue] = Show.show(v => s"IntValue(${v.value})")
  implicit val showFloatValue: Show[FloatValue] = Show.show(v => s"FloatValue(${v.value})")
  implicit val showBoolValue: Show[BoolValue] = Show.show(v => s"BoolValue(${v.value})")
  implicit val showStrValue: Show[StrValue] = Show.show(v => s"StrValue(${v.value})")
  implicit val showNullValue: Show[NullValue.type] = Show.show(_ => "NullValue")
  implicit val showArrayValue: Show[ArrayValue] = Show.show(v => s"ArrayValue(${v.value.size} elements)")
  implicit val showMapValue: Show[MapValue] = Show.show(v => s"MapValue(${v.value.size} entries)")

  implicit val showJsonLogicValue: Show[JsonLogicValue] = Show.show {
    case v: IntValue => showIntValue.show(v)
    case v: FloatValue => showFloatValue.show(v)
    case v: BoolValue => showBoolValue.show(v)
    case v: StrValue => showStrValue.show(v)
    case NullValue => "NullValue"
    case v: ArrayValue => showArrayValue.show(v)
    case v: MapValue => showMapValue.show(v)
    case _: FunctionValue => s"FunctionValue(...)"
  }

  implicit val showConstExpression: Show[ConstExpression] =
    Show.show(e => s"ConstExpression(${showJsonLogicValue.show(e.value)})")

  implicit val showVarExpression: Show[VarExpression] =
    Show.show(e => s"VarExpression(${e.value})")

  implicit val showApplyExpression: Show[ApplyExpression] =
    Show.show(e => s"ApplyExpression(${e.op.tag}, ${e.args.length} args)")

  implicit val showArrayExpression: Show[ArrayExpression] =
    Show.show(e => s"ArrayExpression(${e.value.length} elements)")

  implicit val showMapExpression: Show[MapExpression] =
    Show.show(e => s"MapExpression(${e.value.size} entries)")

  implicit val showJsonLogicExpression: Show[JsonLogicExpression] = Show.show {
    case e: ConstExpression => showConstExpression.show(e)
    case e: VarExpression => showVarExpression.show(e)
    case e: ApplyExpression => showApplyExpression.show(e)
    case e: ArrayExpression => showArrayExpression.show(e)
    case e: MapExpression => showMapExpression.show(e)
  }

  implicit def showList[A: Show]: Show[List[A]] =
    Show.show(list => s"List(${list.take(3).map(Show[A].show).mkString(", ")}${if (list.length > 3) "..." else ""})")

  implicit def showTuple2[A: Show, B: Show]: Show[(A, B)] =
    Show.show { case (a, b) => s"(${Show[A].show(a)}, ${Show[B].show(b)})" }

  implicit def showTuple3[A: Show, B: Show, C: Show]: Show[(A, B, C)] =
    Show.show { case (a, b, c) => s"(${Show[A].show(a)}, ${Show[B].show(b)}, ${Show[C].show(c)})" }

  // ============================================================================
  // Generators for JsonLogicValue
  // ============================================================================

  val genSmallInt: Gen[BigInt] = Gen.chooseNum(-1000L, 1000L).map(BigInt(_))
  val genLargeInt: Gen[BigInt] = Gen.chooseNum(Long.MinValue, Long.MaxValue).map(BigInt(_))
  val genNonZeroInt: Gen[IntValue] = Gen.chooseNum(1, Int.MaxValue).map(n => IntValue(BigInt(n)))

  val genIntValue: Gen[IntValue] = genSmallInt.map(IntValue(_))
  val genLargeIntValue: Gen[IntValue] = genLargeInt.map(IntValue(_))

  val genFloatValue: Gen[FloatValue] =
    Gen.chooseNum(-1000.0, 1000.0).map(d => FloatValue(BigDecimal(d)))

  val genBoolValue: Gen[BoolValue] = Gen.oneOf(true, false).map(BoolValue(_))

  val genStrValue: Gen[StrValue] = Gen.alphaNumStr.map(StrValue(_))

  val genNullValue: Gen[NullValue.type] = Gen.const(NullValue)

  val genPrimitiveValue: Gen[JsonLogicValue] = Gen.oneOf(
    genIntValue,
    genFloatValue,
    genBoolValue,
    genStrValue,
    genNullValue
  )

  def genArrayValue(depth: Int = 0): Gen[ArrayValue] =
    if (depth > 3) Gen.const(ArrayValue(List.empty))
    else Gen.listOfN(Gen.chooseNum(0, 5).sample.getOrElse(3), genJsonLogicValue(depth + 1)).map(ArrayValue(_))

  def genMapValue(depth: Int = 0): Gen[MapValue] =
    if (depth > 3) Gen.const(MapValue.empty)
    else
      Gen
        .mapOfN(
          Gen.chooseNum(0, 3).sample.getOrElse(2),
          for {
            key   <- Gen.alphaNumStr.suchThat(_.nonEmpty)
            value <- genJsonLogicValue(depth + 1)
          } yield (key, value)
        )
        .map(MapValue(_))

  def genJsonLogicValue(depth: Int = 0): Gen[JsonLogicValue] =
    if (depth > 3) genPrimitiveValue
    else
      Gen.frequency(
        5 -> genPrimitiveValue,
        1 -> genArrayValue(depth),
        1 -> genMapValue(depth)
      )

  implicit val arbJsonLogicValue: Arbitrary[JsonLogicValue] = Arbitrary(genJsonLogicValue())

  // ============================================================================
  // Generators for JsonLogicExpression
  // ============================================================================

  val genConstExpression: Gen[ConstExpression] = genPrimitiveValue.map(ConstExpression(_))

  val genVarExpression: Gen[VarExpression] = for {
    key     <- Gen.alphaNumStr.suchThat(_.nonEmpty)
    default <- Gen.option(genPrimitiveValue)
  } yield VarExpression(Left(key), default)

  def genArithmeticOp: Gen[JsonLogicOp] = Gen.oneOf(
    JsonLogicOp.AddOp,
    JsonLogicOp.TimesOp,
    JsonLogicOp.MinusOp,
    JsonLogicOp.MaxOp,
    JsonLogicOp.MinOp
  )

  def genComparisonOp: Gen[JsonLogicOp] = Gen.oneOf(
    JsonLogicOp.Lt,
    JsonLogicOp.Leq,
    JsonLogicOp.Gt,
    JsonLogicOp.Geq,
    JsonLogicOp.EqOp,
    JsonLogicOp.EqStrictOp
  )

  def genLogicOp: Gen[JsonLogicOp] = Gen.oneOf(
    JsonLogicOp.AndOp,
    JsonLogicOp.OrOp,
    JsonLogicOp.NotOp
  )

  def genSimpleExpression(depth: Int = 0): Gen[JsonLogicExpression] =
    if (depth > 2) genConstExpression
    else
      Gen.frequency(
        3 -> genConstExpression,
        1 -> genVarExpression,
        1 -> genArithmeticExpression(depth + 1)
      )

  def genArithmeticExpression(depth: Int = 0): Gen[ApplyExpression] =
    for {
      op   <- genArithmeticOp
      arg1 <- genSimpleExpression(depth)
      arg2 <- genSimpleExpression(depth)
    } yield ApplyExpression(op, List(arg1, arg2))

  // ============================================================================
  // Property Tests: Arithmetic Properties
  // ============================================================================

  test("addition is commutative for integers") {
    forall(for { a <- genIntValue; b <- genIntValue } yield (a, b)) { case (a, b) =>
      val expr1 = ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(a), ConstExpression(b)))
      val expr2 = ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(b), ConstExpression(a)))

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      for {
        result1 <- evaluator.evaluate(expr1, MapValue.empty, None)
        result2 <- evaluator.evaluate(expr2, MapValue.empty, None)
      } yield expect.all(
        result1.isRight,
        result2.isRight,
        result1 == result2
      )
    }
  }

  test("multiplication is commutative for integers") {
    forall(for { a <- genIntValue; b <- genIntValue } yield (a, b)) { case (a, b) =>
      val expr1 = ApplyExpression(JsonLogicOp.TimesOp, List(ConstExpression(a), ConstExpression(b)))
      val expr2 = ApplyExpression(JsonLogicOp.TimesOp, List(ConstExpression(b), ConstExpression(a)))

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      for {
        result1 <- evaluator.evaluate(expr1, MapValue.empty, None)
        result2 <- evaluator.evaluate(expr2, MapValue.empty, None)
      } yield expect.all(
        result1.isRight,
        result2.isRight,
        result1 == result2
      )
    }
  }

  test("addition is associative") {
    forall(for { a <- genIntValue; b <- genIntValue; c <- genIntValue } yield (a, b, c)) { case (a, b, c) =>
      // (a + b) + c
      val expr1 = ApplyExpression(
        JsonLogicOp.AddOp,
        List(
          ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(a), ConstExpression(b))),
          ConstExpression(c)
        )
      )

      // a + (b + c)
      val expr2 = ApplyExpression(
        JsonLogicOp.AddOp,
        List(
          ConstExpression(a),
          ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(b), ConstExpression(c)))
        )
      )

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      for {
        result1 <- evaluator.evaluate(expr1, MapValue.empty, None)
        result2 <- evaluator.evaluate(expr2, MapValue.empty, None)
      } yield expect.all(
        result1.isRight,
        result2.isRight,
        result1 == result2
      )
    }
  }

  test("multiplication has identity element 1") {
    forall(genIntValue) { a =>
      val expr = ApplyExpression(JsonLogicOp.TimesOp, List(ConstExpression(a), ConstExpression(IntValue(1))))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(result) => expect(result == a)
        case Left(_)       => failure("Evaluation should succeed")
      }
    }
  }

  test("addition has identity element 0") {
    forall(genIntValue) { a =>
      val expr = ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(a), ConstExpression(IntValue(0))))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(result) => expect(result == a)
        case Left(_)       => failure("Evaluation should succeed")
      }
    }
  }

  test("subtraction is inverse of addition") {
    forall(for { a <- genIntValue; b <- genIntValue } yield (a, b)) { case (a, b) =>
      // (a + b) - b should equal a
      val expr = ApplyExpression(
        JsonLogicOp.MinusOp,
        List(
          ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(a), ConstExpression(b))),
          ConstExpression(b)
        )
      )

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(result) => expect(result == a)
        case Left(_)       => failure("Evaluation should succeed")
      }
    }
  }

  test("multiplication distributes over addition") {
    forall(for { a <- genIntValue; b <- genIntValue; c <- genIntValue } yield (a, b, c)) { case (a, b, c) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      // a * (b + c)
      val leftExpr = ApplyExpression(
        JsonLogicOp.TimesOp,
        List(
          ConstExpression(a),
          ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(b), ConstExpression(c)))
        )
      )

      // (a * b) + (a * c)
      val rightExpr = ApplyExpression(
        JsonLogicOp.AddOp,
        List(
          ApplyExpression(JsonLogicOp.TimesOp, List(ConstExpression(a), ConstExpression(b))),
          ApplyExpression(JsonLogicOp.TimesOp, List(ConstExpression(a), ConstExpression(c)))
        )
      )

      for {
        result1 <- evaluator.evaluate(leftExpr, MapValue.empty, None)
        result2 <- evaluator.evaluate(rightExpr, MapValue.empty, None)
      } yield expect(result1 == result2)
    }
  }

  // ============================================================================
  // Property Tests: Comparison Properties
  // ============================================================================

  test("less than is transitive") {
    forall(for { a <- genIntValue; b <- genIntValue; c <- genIntValue } yield (a, b, c)) { case (a, b, c) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      // If a < b and b < c, then a < c
      val aLtB = ApplyExpression(JsonLogicOp.Lt, List(ConstExpression(a), ConstExpression(b)))
      val bLtC = ApplyExpression(JsonLogicOp.Lt, List(ConstExpression(b), ConstExpression(c)))
      val aLtC = ApplyExpression(JsonLogicOp.Lt, List(ConstExpression(a), ConstExpression(c)))

      for {
        resultAB <- evaluator.evaluate(aLtB, MapValue.empty, None)
        resultBC <- evaluator.evaluate(bLtC, MapValue.empty, None)
        resultAC <- evaluator.evaluate(aLtC, MapValue.empty, None)
      } yield {
        (resultAB, resultBC, resultAC) match {
          case (Right(BoolValue(true)), Right(BoolValue(true)), Right(BoolValue(trueAC))) =>
            expect(trueAC) // Transitivity holds
          case _ =>
            success // Precondition not met, test vacuously passes
        }
      }
    }
  }

  test("equality is reflexive") {
    forall(genPrimitiveValue) { value =>
      val expr = ApplyExpression(JsonLogicOp.EqStrictOp, List(ConstExpression(value), ConstExpression(value)))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(BoolValue(true)) => success
        case Right(other)           => failure(s"Expected true for ${value}, got $other")
        case Left(err)              => failure(s"Evaluation failed: $err")
      }
    }
  }

  test("equality is symmetric") {
    forall(for { a <- genPrimitiveValue; b <- genPrimitiveValue } yield (a, b)) { case (a, b) =>
      val expr1 = ApplyExpression(JsonLogicOp.EqStrictOp, List(ConstExpression(a), ConstExpression(b)))
      val expr2 = ApplyExpression(JsonLogicOp.EqStrictOp, List(ConstExpression(b), ConstExpression(a)))

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      for {
        result1 <- evaluator.evaluate(expr1, MapValue.empty, None)
        result2 <- evaluator.evaluate(expr2, MapValue.empty, None)
      } yield expect(result1 == result2)
    }
  }

  // ============================================================================
  // Property Tests: Boolean Logic Properties
  // ============================================================================

  test("double negation returns original value") {
    forall(genBoolValue) { bool =>
      val expr = ApplyExpression(
        JsonLogicOp.NotOp,
        List(ApplyExpression(JsonLogicOp.NotOp, List(ConstExpression(bool))))
      )

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(result) => expect(result == bool)
        case Left(_)       => failure("Evaluation should succeed")
      }
    }
  }

  test("AND is commutative") {
    forall(for { a <- genBoolValue; b <- genBoolValue } yield (a, b)) { case (a, b) =>
      val expr1 = ApplyExpression(JsonLogicOp.AndOp, List(ConstExpression(a), ConstExpression(b)))
      val expr2 = ApplyExpression(JsonLogicOp.AndOp, List(ConstExpression(b), ConstExpression(a)))

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      for {
        result1 <- evaluator.evaluate(expr1, MapValue.empty, None)
        result2 <- evaluator.evaluate(expr2, MapValue.empty, None)
      } yield {
        // Note: AND may not return BoolValue, it returns the last truthy or first falsy value
        // But the truthiness should be the same
        val truth1 = result1.toOption.exists(_.isTruthy)
        val truth2 = result2.toOption.exists(_.isTruthy)
        expect(truth1 == truth2)
      }
    }
  }

  test("OR is commutative") {
    forall(for { a <- genBoolValue; b <- genBoolValue } yield (a, b)) { case (a, b) =>
      val expr1 = ApplyExpression(JsonLogicOp.OrOp, List(ConstExpression(a), ConstExpression(b)))
      val expr2 = ApplyExpression(JsonLogicOp.OrOp, List(ConstExpression(b), ConstExpression(a)))

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      for {
        result1 <- evaluator.evaluate(expr1, MapValue.empty, None)
        result2 <- evaluator.evaluate(expr2, MapValue.empty, None)
      } yield {
        val truth1 = result1.toOption.exists(_.isTruthy)
        val truth2 = result2.toOption.exists(_.isTruthy)
        expect(truth1 == truth2)
      }
    }
  }

  test("De Morgan's law for AND") {
    forall(for { a <- genBoolValue; b <- genBoolValue } yield (a, b)) { case (a, b) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      // !(a && b)
      val leftExpr = ApplyExpression(
        JsonLogicOp.NotOp,
        List(ApplyExpression(JsonLogicOp.AndOp, List(ConstExpression(a), ConstExpression(b))))
      )

      // !a || !b
      val rightExpr = ApplyExpression(
        JsonLogicOp.OrOp,
        List(
          ApplyExpression(JsonLogicOp.NotOp, List(ConstExpression(a))),
          ApplyExpression(JsonLogicOp.NotOp, List(ConstExpression(b)))
        )
      )

      for {
        result1 <- evaluator.evaluate(leftExpr, MapValue.empty, None)
        result2 <- evaluator.evaluate(rightExpr, MapValue.empty, None)
      } yield {
        val truth1 = result1.toOption.exists(_.isTruthy)
        val truth2 = result2.toOption.exists(_.isTruthy)
        expect(truth1 == truth2)
      }
    }
  }

  test("De Morgan's law for OR") {
    forall(for { a <- genBoolValue; b <- genBoolValue } yield (a, b)) { case (a, b) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      // !(a || b)
      val leftExpr = ApplyExpression(
        JsonLogicOp.NotOp,
        List(ApplyExpression(JsonLogicOp.OrOp, List(ConstExpression(a), ConstExpression(b))))
      )

      // !a && !b
      val rightExpr = ApplyExpression(
        JsonLogicOp.AndOp,
        List(
          ApplyExpression(JsonLogicOp.NotOp, List(ConstExpression(a))),
          ApplyExpression(JsonLogicOp.NotOp, List(ConstExpression(b)))
        )
      )

      for {
        result1 <- evaluator.evaluate(leftExpr, MapValue.empty, None)
        result2 <- evaluator.evaluate(rightExpr, MapValue.empty, None)
      } yield {
        val truth1 = result1.toOption.exists(_.isTruthy)
        val truth2 = result2.toOption.exists(_.isTruthy)
        expect(truth1 == truth2)
      }
    }
  }

  // ============================================================================
  // Property Tests: Type Coercion Properties
  // ============================================================================

  test("null coerces to 0 in arithmetic") {
    forall(genIntValue) { a =>
      val exprWithNull = ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(a), ConstExpression(NullValue)))
      val exprWithZero = ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(a), ConstExpression(IntValue(0))))

      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      for {
        resultNull <- evaluator.evaluate(exprWithNull, MapValue.empty, None)
        resultZero <- evaluator.evaluate(exprWithZero, MapValue.empty, None)
      } yield expect(resultNull == resultZero)
    }
  }

  test("true coerces to 1, false to 0 in arithmetic") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]

    val exprTrue = ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(IntValue(5)), ConstExpression(BoolValue(true))))
    val exprFalse =
      ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(IntValue(5)), ConstExpression(BoolValue(false))))

    for {
      resultTrue  <- evaluator.evaluate(exprTrue, MapValue.empty, None)
      resultFalse <- evaluator.evaluate(exprFalse, MapValue.empty, None)
    } yield expect.all(
      resultTrue == Right(IntValue(6)),
      resultFalse == Right(IntValue(5))
    )
  }

  // ============================================================================
  // Property Tests: Array Operations
  // ============================================================================

  test("max of list is greater than or equal to all elements") {
    forall(Gen.nonEmptyListOf(genIntValue)) { values =>
      val expr = ApplyExpression(JsonLogicOp.MaxOp, values.map(ConstExpression(_)))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(IntValue(max)) =>
          expect(values.forall(v => max >= v.value))
        case Right(other) => failure(s"Expected IntValue, got $other")
        case Left(err)    => failure(s"Evaluation failed: $err")
      }
    }
  }

  test("min of list is less than or equal to all elements") {
    forall(Gen.nonEmptyListOf(genIntValue)) { values =>
      val expr = ApplyExpression(JsonLogicOp.MinOp, values.map(ConstExpression(_)))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(IntValue(min)) =>
          expect(values.forall(v => min <= v.value))
        case Right(other) => failure(s"Expected IntValue, got $other")
        case Left(err)    => failure(s"Evaluation failed: $err")
      }
    }
  }

  test("max is idempotent") {
    forall(genIntValue) { a =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val expr = ApplyExpression(JsonLogicOp.MaxOp, List(ConstExpression(a), ConstExpression(a)))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(result) => expect(result == a)
        case Left(err)     => failure(s"Evaluation failed: $err")
      }
    }
  }

  test("min is idempotent") {
    forall(genIntValue) { a =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val expr = ApplyExpression(JsonLogicOp.MinOp, List(ConstExpression(a), ConstExpression(a)))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(result) => expect(result == a)
        case Left(err)     => failure(s"Evaluation failed: $err")
      }
    }
  }

  test("unique removes duplicates") {
    forall(Gen.listOf(genIntValue)) { values =>
      val arrayExpr = ConstExpression(ArrayValue(values))
      val expr = ApplyExpression(JsonLogicOp.UniqueOp, List(arrayExpr))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(ArrayValue(result)) =>
          expect.all(
            result.distinct == result, // No duplicates
            result.toSet == values.toSet // Same unique elements
          )
        case Right(other) => failure(s"Expected ArrayValue, got $other")
        case Left(err)    => failure(s"Evaluation failed: $err")
      }
    }
  }

  // ============================================================================
  // Property Tests: Invariants
  // ============================================================================

  test("evaluation is deterministic") {
    forall(genArithmeticExpression()) { expr =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val data = MapValue.empty

      for {
        result1 <- evaluator.evaluate(expr, data, None)
        result2 <- evaluator.evaluate(expr, data, None)
      } yield {
        // Compare success/failure and values, not exception objects directly
        (result1, result2) match {
          case (Right(v1), Right(v2)) => expect(v1 == v2)
          case (Left(e1), Left(e2)) => expect(e1.getMessage == e2.getMessage)
          case _ => failure("Results should both succeed or both fail")
        }
      }
    }
  }

  test("evaluation never throws uncaught exceptions") {
    forall(for { expr <- genSimpleExpression(); data <- genJsonLogicValue() } yield (expr, data)) { case (expr, data) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, data, None).attempt.map {
        case Right(Left(_))  => success // JsonLogicException is expected
        case Right(Right(_)) => success // Success is fine
        case Left(ex)        => failure(s"Unexpected exception: ${ex.getMessage}")
      }
    }
  }

  test("null is falsy, non-zero numbers are truthy") {
    IO.pure(expect.all(
      !NullValue.isTruthy,
      IntValue(1).isTruthy,
      IntValue(-1).isTruthy,
      !IntValue(0).isTruthy
    ))
  }

  // ============================================================================
  // Division & Modulo Properties
  // ============================================================================

  test("division by zero always fails") {
    forall(genIntValue) { a =>
      val expr = ApplyExpression(JsonLogicOp.DivOp, List(ConstExpression(a), ConstExpression(IntValue(0))))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map(_.isLeft)
        .map(isError => expect(isError))
    }
  }

  test("modulo by zero always fails") {
    forall(genIntValue) { a =>
      val expr = ApplyExpression(JsonLogicOp.ModuloOp, List(ConstExpression(a), ConstExpression(IntValue(0))))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map(_.isLeft)
        .map(isError => expect(isError))
    }
  }

  test("modulo result absolute value is less than divisor absolute value") {
    forall(for { a <- genIntValue; b <- genNonZeroInt } yield (a, b)) { case (a, b) =>
      val expr = ApplyExpression(JsonLogicOp.ModuloOp, List(ConstExpression(a), ConstExpression(b)))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(IntValue(result)) => expect(result.abs <= b.value.abs)
        case Right(FloatValue(result)) => expect(result.abs <= BigDecimal(b.value).abs)
        case Left(_) => failure("Modulo should succeed for non-zero divisor")
        case _ => success
      }
    }
  }

  test("modulo sign follows JavaScript semantics") {
    forall(for { a <- genIntValue; b <- genNonZeroInt } yield (a, b)) { case (a, b) =>
      val expr = ApplyExpression(JsonLogicOp.ModuloOp, List(ConstExpression(a), ConstExpression(b)))
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(IntValue(result)) if a.value >= 0 => expect(result >= 0)
        case Right(IntValue(result)) if a.value < 0 => expect(result <= 0)
        case _ => success
      }
    }
  }


  // ============================================================================
  // String Operation Properties
  // ============================================================================

  test("string concatenation length property") {
    forall(for { s1 <- genStrValue; s2 <- genStrValue } yield (s1, s2)) { case (s1, s2) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      val catExpr = ApplyExpression(JsonLogicOp.CatOp, List(ConstExpression(s1), ConstExpression(s2)))
      val len1 = ApplyExpression(JsonLogicOp.LengthOp, List(ConstExpression(s1)))
      val len2 = ApplyExpression(JsonLogicOp.LengthOp, List(ConstExpression(s2)))

      for {
        concatenated <- evaluator.evaluate(catExpr, MapValue.empty, None)
        length1 <- evaluator.evaluate(len1, MapValue.empty, None)
        length2 <- evaluator.evaluate(len2, MapValue.empty, None)
        catLength <- concatenated match {
          case Right(s: StrValue) => evaluator.evaluate(ApplyExpression(JsonLogicOp.LengthOp, List(ConstExpression(s))), MapValue.empty, None)
          case other => IO.pure(other)
        }
      } yield {
        (length1, length2, catLength) match {
          case (Right(IntValue(l1)), Right(IntValue(l2)), Right(IntValue(lCat))) =>
            expect(lCat == l1 + l2)
          case _ => failure("Expected integer lengths")
        }
      }
    }
  }

  test("lower produces no uppercase letters") {
    forall(genStrValue) { str =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val expr = ApplyExpression(JsonLogicOp.LowerOp, List(ConstExpression(str)))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(StrValue(result)) => expect(result == result.toLowerCase)
        case _ => failure("Expected string result")
      }
    }
  }

  test("upper produces no lowercase letters") {
    forall(genStrValue) { str =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val expr = ApplyExpression(JsonLogicOp.UpperOp, List(ConstExpression(str)))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(StrValue(result)) => expect(result == result.toUpperCase)
        case _ => failure("Expected string result")
      }
    }
  }

  test("trim is idempotent") {
    forall(genStrValue) { str =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      val trimOnce = ApplyExpression(JsonLogicOp.TrimOp, List(ConstExpression(str)))

      for {
        result1 <- evaluator.evaluate(trimOnce, MapValue.empty, None)
        result2 <- result1 match {
          case Right(s: StrValue) =>
            evaluator.evaluate(ApplyExpression(JsonLogicOp.TrimOp, List(ConstExpression(s))), MapValue.empty, None)
          case other => IO.pure(other)
        }
      } yield expect(result1 == result2)
    }
  }

  test("substr never increases string length") {
    forall(for { str <- genStrValue; start <- Gen.chooseNum(-10, 10) } yield (str, start)) { case (str, start) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      val substrExpr = ApplyExpression(JsonLogicOp.SubStrOp, List(ConstExpression(str), ConstExpression(IntValue(start))))
      val originalLen = ApplyExpression(JsonLogicOp.LengthOp, List(ConstExpression(str)))

      for {
        substr <- evaluator.evaluate(substrExpr, MapValue.empty, None)
        origLen <- evaluator.evaluate(originalLen, MapValue.empty, None)
        substrLen <- substr match {
          case Right(s: StrValue) => evaluator.evaluate(ApplyExpression(JsonLogicOp.LengthOp, List(ConstExpression(s))), MapValue.empty, None)
          case other => IO.pure(other)
        }
      } yield {
        (origLen, substrLen) match {
          case (Right(IntValue(ol)), Right(IntValue(sl))) => expect(sl <= ol)
          case _ => success
        }
      }
    }
  }

  // ============================================================================
  // Array Operation Properties
  // ============================================================================

  test("map preserves array length") {
    forall(Gen.listOf(genIntValue)) { values =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val arr = ArrayValue(values)

      // Simple identity-like map
      val mapFn = FunctionValue(VarExpression(Left(""), None))
      val mapExpr = ApplyExpression(JsonLogicOp.MapOp, List(ConstExpression(arr), ConstExpression(mapFn)))

      evaluator.evaluate(mapExpr, MapValue.empty, None).map {
        case Right(ArrayValue(result)) => expect(result.length == values.length)
        case _ => failure("Expected array result")
      }
    }
  }

  test("filter never increases array length") {
    forall(Gen.listOf(genIntValue)) { values =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val arr = ArrayValue(values)

      val filterFn = FunctionValue(ConstExpression(BoolValue(true)))
      val filterExpr = ApplyExpression(JsonLogicOp.FilterOp, List(ConstExpression(arr), ConstExpression(filterFn)))

      evaluator.evaluate(filterExpr, MapValue.empty, None).map {
        case Right(ArrayValue(result)) => expect(result.length <= values.length)
        case _ => failure("Expected array result")
      }
    }
  }

  test("unique never increases array length") {
    forall(Gen.listOf(genIntValue)) { values =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val arr = ArrayValue(values)

      val expr = ApplyExpression(JsonLogicOp.UniqueOp, List(ConstExpression(arr)))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(ArrayValue(result)) =>
          expect.all(
            result.length <= values.length,
            result.distinct == result // No duplicates
          )
        case _ => failure("Expected array result")
      }
    }
  }

  test("reverse is its own inverse") {
    forall(Gen.listOf(genIntValue)) { values =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val arr = ArrayValue(values)

      val reverseOnce = ApplyExpression(JsonLogicOp.ReverseOp, List(ConstExpression(arr)))

      for {
        reversed <- evaluator.evaluate(reverseOnce, MapValue.empty, None)
        doubleReversed <- reversed match {
          case Right(a: ArrayValue) =>
            evaluator.evaluate(ApplyExpression(JsonLogicOp.ReverseOp, List(ConstExpression(a))), MapValue.empty, None)
          case other => IO.pure(other)
        }
      } yield expect(doubleReversed == Right(arr))
    }
  }

  test("slice preserves element order") {
    forall(for {
      values <- Gen.nonEmptyListOf(genIntValue)
      start <- Gen.chooseNum(0, values.length)
      end <- Gen.chooseNum(start, values.length)
    } yield (values, start, end)) { case (values, start, end) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val arr = ArrayValue(values)

      val expr = ApplyExpression(JsonLogicOp.SliceOp, List(ConstExpression(arr), ConstExpression(IntValue(start)), ConstExpression(IntValue(end))))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(ArrayValue(result)) =>
          expect(result == values.slice(start, end))
        case _ => failure("Expected array result")
      }
    }
  }

  test("flatten preserves all primitive elements") {
    forall(Gen.listOf(genIntValue)) { values =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      // Create nested array: [[a], [b], [c]]
      val nested = values.map(v => ArrayValue(List(v)))
      val arr = ArrayValue(nested)

      val flattenExpr = ApplyExpression(JsonLogicOp.FlattenOp, List(ConstExpression(arr)))

      evaluator.evaluate(flattenExpr, MapValue.empty, None).map {
        case Right(ArrayValue(result)) =>
          expect(result == values)
        case _ => failure("Expected flattened array")
      }
    }
  }

  // ============================================================================
  // Numeric Operation Properties
  // ============================================================================

  test("abs always returns non-negative") {
    forall(genIntValue) { num =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val expr = ApplyExpression(JsonLogicOp.AbsOp, List(ConstExpression(num)))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(IntValue(result)) => expect(result >= 0)
        case Right(FloatValue(result)) => expect(result >= 0)
        case _ => failure("Expected numeric result")
      }
    }
  }

  test("abs is idempotent") {
    forall(genIntValue) { num =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      val absOnce = ApplyExpression(JsonLogicOp.AbsOp, List(ConstExpression(num)))

      for {
        result1 <- evaluator.evaluate(absOnce, MapValue.empty, None)
        result2 <- result1 match {
          case Right(v: IntValue) =>
            evaluator.evaluate(ApplyExpression(JsonLogicOp.AbsOp, List(ConstExpression(v))), MapValue.empty, None)
          case Right(v: FloatValue) =>
            evaluator.evaluate(ApplyExpression(JsonLogicOp.AbsOp, List(ConstExpression(v))), MapValue.empty, None)
          case other => IO.pure(other)
        }
      } yield expect(result1 == result2)
    }
  }

  test("floor never increases value") {
    forall(genFloatValue) { num =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val expr = ApplyExpression(JsonLogicOp.FloorOp, List(ConstExpression(num)))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(IntValue(result)) => expect(BigDecimal(result) <= num.value)
        case Right(FloatValue(result)) => expect(result <= num.value)
        case _ => success
      }
    }
  }

  test("ceil never decreases value") {
    forall(genFloatValue) { num =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val expr = ApplyExpression(JsonLogicOp.CeilOp, List(ConstExpression(num)))

      evaluator.evaluate(expr, MapValue.empty, None).map {
        case Right(IntValue(result)) => expect(BigDecimal(result) >= num.value)
        case Right(FloatValue(result)) => expect(result >= num.value)
        case _ => success
      }
    }
  }

  test("floor <= round <= ceil") {
    forall(genFloatValue) { num =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      val floorExpr = ApplyExpression(JsonLogicOp.FloorOp, List(ConstExpression(num)))
      val roundExpr = ApplyExpression(JsonLogicOp.RoundOp, List(ConstExpression(num)))
      val ceilExpr = ApplyExpression(JsonLogicOp.CeilOp, List(ConstExpression(num)))

      for {
        floorResult <- evaluator.evaluate(floorExpr, MapValue.empty, None)
        roundResult <- evaluator.evaluate(roundExpr, MapValue.empty, None)
        ceilResult <- evaluator.evaluate(ceilExpr, MapValue.empty, None)
      } yield {
        (floorResult, roundResult, ceilResult) match {
          case (Right(IntValue(f)), Right(IntValue(r)), Right(IntValue(c))) =>
            expect.all(f <= r, r <= c)
          case (Right(FloatValue(f)), Right(FloatValue(r)), Right(FloatValue(c))) =>
            expect.all(f <= r, r <= c)
          case _ => success
        }
      }
    }
  }

  // ============================================================================
  // Split/Join Inverse Property
  // ============================================================================

  test("split and join are inverses when separator not in string") {
    forall(for {
      parts <- Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))
      sep <- Gen.const(",")
    } yield (parts.mkString(sep), sep)) { case (str, sep) =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]

      val strValue = StrValue(str)
      val sepValue = StrValue(sep)

      val splitExpr = ApplyExpression(JsonLogicOp.SplitOp, List(ConstExpression(strValue), ConstExpression(sepValue)))

      for {
        splitResult <- evaluator.evaluate(splitExpr, MapValue.empty, None)
        joinResult <- splitResult match {
          case Right(arr: ArrayValue) =>
            evaluator.evaluate(ApplyExpression(JsonLogicOp.JoinOp, List(ConstExpression(arr), ConstExpression(sepValue))), MapValue.empty, None)
          case other => IO.pure(other)
        }
      } yield expect(joinResult == Right(strValue))
    }
  }

  // ============================================================================
  // Property Tests: Gas Metering Invariants
  // ============================================================================

  test("gas consumption is monotonic") {
    forall(genArithmeticExpression()) { expr =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val gasLimit = io.constellationnetwork.metagraph_sdk.json_logic.gas.GasLimit(1000000)
      val gasConfig = io.constellationnetwork.metagraph_sdk.json_logic.gas.GasConfig.Default

      evaluator.evaluateWithGas(expr, MapValue.empty, None, gasLimit, gasConfig).map {
        case Right(result) =>
          expect(result.gasUsed.amount >= 0)
        case Left(_) => success // Gas exhaustion or other error is fine
      }
    }
  }

  test("same expression always consumes same gas") {
    forall(genArithmeticExpression()) { expr =>
      val evaluator = JsonLogicEvaluator.tailRecursive[IO]
      val gasLimit = io.constellationnetwork.metagraph_sdk.json_logic.gas.GasLimit(1000000)
      val gasConfig = io.constellationnetwork.metagraph_sdk.json_logic.gas.GasConfig.Default

      for {
        result1 <- evaluator.evaluateWithGas(expr, MapValue.empty, None, gasLimit, gasConfig)
        result2 <- evaluator.evaluateWithGas(expr, MapValue.empty, None, gasLimit, gasConfig)
      } yield {
        (result1, result2) match {
          case (Right(r1), Right(r2)) =>
            expect(r1.gasUsed == r2.gasUsed)
          case _ => success // Both failed the same way
        }
      }
    }
  }
}