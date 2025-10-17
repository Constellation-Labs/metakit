package json_logic

import cats.Id
import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.ResultContext._
import io.constellationnetwork.metagraph_sdk.json_logic.{GasCost, GasMetrics, ResultContext}

import weaver.SimpleIOSuite

object ResultContextSuite extends SimpleIOSuite {

  test("Id context - pure returns value unchanged") {
    val rc = ResultContext[Id]
    val value = 42
    val result = rc.pure(value)

    IO.pure(expect(result == 42))
  }

  test("Id context - map applies function") {
    val rc = ResultContext[Id]
    val value = 10
    val result = rc.map(value)(_ * 2)

    IO.pure(expect(result == 20))
  }

  test("Id context - flatMap applies function") {
    val rc = ResultContext[Id]
    val value = 5
    val result = rc.flatMap(value)(x => x + 3)

    IO.pure(expect(result == 8))
  }

  test("Id context - sequence returns list unchanged") {
    val rc = ResultContext[Id]
    val values = List(1, 2, 3, 4, 5)
    val result = rc.sequence(values)

    IO.pure(expect(result == List(1, 2, 3, 4, 5)))
  }

  test("WithGas context - pure wraps value with zero metrics") {
    val rc = ResultContext[ResultContext.WithGas]
    val value = "test"
    val (result, metrics) = rc.pure(value)

    IO.pure(
      expect(result == "test") and
        expect(metrics == GasMetrics.zero)
    )
  }

  test("WithGas context - map applies function and preserves metrics") {
    val rc = ResultContext[ResultContext.WithGas]
    val input = ("hello", GasMetrics(GasCost(100), 5, 2))
    val (result, metrics) = rc.map(input)(_.toUpperCase)

    IO.pure(
      expect(result == "HELLO") and
        expect(metrics.cost == GasCost(100)) and
        expect(metrics.depth == 5) and
        expect(metrics.opCount == 2)
    )
  }

  test("WithGas context - flatMap combines metrics") {
    val rc = ResultContext[ResultContext.WithGas]
    val input = (10, GasMetrics(GasCost(50), 3, 1))
    val (result, metrics) = rc.flatMap(input) { x =>
      (x * 2, GasMetrics(GasCost(30), 4, 2))
    }

    IO.pure(
      expect(result == 20) and
        expect(metrics.cost == GasCost(80)) and
        expect(metrics.depth == 4) and
        expect(metrics.opCount == 3)
    )
  }

  test("WithGas context - flatMap takes max depth from both metrics") {
    val rc = ResultContext[ResultContext.WithGas]
    val input = (5, GasMetrics(GasCost(10), 7, 1))
    val (result, metrics) = rc.flatMap(input) { x =>
      (x + 1, GasMetrics(GasCost(5), 3, 1))
    }

    IO.pure(
      expect(result == 6) and
        expect(metrics.depth == 7)
    )
  }

  test("WithGas context - sequence combines all metrics") {
    val rc = ResultContext[ResultContext.WithGas]
    val inputs = List(
      (1, GasMetrics(GasCost(10), 2, 1)),
      (2, GasMetrics(GasCost(20), 3, 1)),
      (3, GasMetrics(GasCost(15), 4, 1))
    )
    val (results, metrics) = rc.sequence(inputs)

    IO.pure(
      expect(results == List(1, 2, 3)) and
        expect(metrics.cost == GasCost(45)) and
        expect(metrics.depth == 4) and
        expect(metrics.opCount == 3)
    )
  }

  test("WithGas context - sequence with empty list returns zero metrics") {
    val rc = ResultContext[ResultContext.WithGas]
    val inputs = List.empty[(Int, GasMetrics)]
    val (results, metrics) = rc.sequence(inputs)

    IO.pure(
      expect(results == List.empty) and
        expect(metrics == GasMetrics.zero)
    )
  }

  test("WithGas context - chaining map operations preserves metrics") {
    val rc = ResultContext[ResultContext.WithGas]
    val initial = (10, GasMetrics(GasCost(100), 5, 3))
    val step1 = rc.map(initial)(_ * 2)
    val step2 = rc.map(step1)(_ + 5)
    val (result, metrics) = step2

    IO.pure(
      expect(result == 25) and
        expect(metrics.cost == GasCost(100)) and
        expect(metrics.depth == 5) and
        expect(metrics.opCount == 3)
    )
  }

  test("WithGas context - chaining flatMap operations accumulates metrics") {
    val rc = ResultContext[ResultContext.WithGas]
    val initial = (10, GasMetrics(GasCost(10), 1, 1))

    val step1 = rc.flatMap(initial) { x =>
      (x * 2, GasMetrics(GasCost(5), 2, 1))
    }

    val step2 = rc.flatMap(step1) { x =>
      (x + 10, GasMetrics(GasCost(3), 3, 1))
    }

    val (result, metrics) = step2

    IO.pure(
      expect(result == 30) and
        expect(metrics.cost == GasCost(18)) and
        expect(metrics.depth == 3) and
        expect(metrics.opCount == 3)
    )
  }

  test("ResultContext.apply summons implicit instance for Id") {
    val rc = ResultContext[Id]
    val result = rc.pure(42)

    IO.pure(expect(result == 42))
  }

  test("ResultContext.apply summons implicit instance for WithGas") {
    val rc = ResultContext[ResultContext.WithGas]
    val (value, metrics) = rc.pure("test")

    IO.pure(
      expect(value == "test") and
        expect(metrics == GasMetrics.zero)
    )
  }

  test("Id and WithGas contexts are type-safe") {
    val idRc = ResultContext[Id]
    val gasRc = ResultContext[ResultContext.WithGas]

    val idResult: Int = idRc.pure(42)
    val gasResult: (String, GasMetrics) = gasRc.pure("hello")

    IO.pure(
      expect(idResult == 42) and
        expect(gasResult._1 == "hello")
    )
  }

  test("WithGas context - flatMap with zero metrics on right") {
    val rc = ResultContext[ResultContext.WithGas]
    val input = (100, GasMetrics(GasCost(50), 5, 2))
    val (result, metrics) = rc.flatMap(input) { x =>
      rc.pure(x / 2)
    }

    IO.pure(
      expect(result == 50) and
        expect(metrics.cost == GasCost(50)) and
        expect(metrics.depth == 5) and
        expect(metrics.opCount == 2)
    )
  }

  test("WithGas context - sequence respects metric combination order") {
    val rc = ResultContext[ResultContext.WithGas]

    val inputs1 = List(
      (1, GasMetrics(GasCost(10), 2, 1)),
      (2, GasMetrics(GasCost(20), 3, 1))
    )

    val inputs2 = List(
      (2, GasMetrics(GasCost(20), 3, 1)),
      (1, GasMetrics(GasCost(10), 2, 1))
    )

    val (_, metrics1) = rc.sequence(inputs1)
    val (_, metrics2) = rc.sequence(inputs2)

    IO.pure(
      expect(metrics1.cost == metrics2.cost) and
        expect(metrics1.depth == metrics2.depth) and
        expect(metrics1.opCount == metrics2.opCount)
    )
  }

  test("syntax - map on Id context") {
    

    val value: Id[Int] = 10
    val result = value.map(_ * 2)

    IO.pure(expect(result == 20))
  }

  test("syntax - map on WithGas context") {
    val value: ResultContext.WithGas[String] = ("hello", GasMetrics(GasCost(50), 3, 1))
    val (result, metrics) = value.map(_.toUpperCase)

    IO.pure(
      expect(result == "HELLO") and
        expect(metrics.cost == GasCost(50))
    )
  }

  test("syntax - flatMap on Id context") {
    val value: Id[Int] = 5
    val result = value.flatMap(x => x + 10)

    IO.pure(expect(result == 15))
  }

  test("syntax - flatMap on WithGas context") {
    

    val value: ResultContext.WithGas[Int] = (10, GasMetrics(GasCost(20), 2, 1))
    val (result, metrics) = value.flatMap { x =>
      (x * 2, GasMetrics(GasCost(10), 3, 1))
    }

    IO.pure(
      expect(result == 20) and
        expect(metrics.cost == GasCost(30)) and
        expect(metrics.depth == 3)
    )
  }

  test("syntax - sequence on List[Id]") {
    

    val values: List[Id[Int]] = List(1, 2, 3, 4)
    val result = values.sequence

    IO.pure(expect(result == List(1, 2, 3, 4)))
  }

  test("syntax - sequence on List[WithGas]") {

    val values: List[ResultContext.WithGas[Int]] = List(
      (1, GasMetrics(GasCost(10), 2, 1)),
      (2, GasMetrics(GasCost(15), 3, 1))
    )
    val (results, metrics) = values.sequence

    IO.pure(
      expect(results == List(1, 2)) and
        expect(metrics.cost == GasCost(25)) and
        expect(metrics.depth == 3)
    )
  }

  test("syntax - rpure wraps value in Id context") {
    

    val result = 42.pure[Id]

    IO.pure(expect(result == 42))
  }

  test("syntax - rpure wraps value in WithGas context") {
    

    val (value, metrics) = "test".pure[ResultContext.WithGas]

    IO.pure(
      expect(value == "test") and
        expect(metrics == GasMetrics.zero)
    )
  }

  test("syntax - chaining map operations") {
    

    val initial: ResultContext.WithGas[Int] = (10, GasMetrics(GasCost(100), 5, 2))
    val result = initial
      .map(_ * 2)
      .map(_ + 5)

    val (value, metrics) = result

    IO.pure(
      expect(value == 25) and
        expect(metrics.cost == GasCost(100))
    )
  }

  test("syntax - chaining flatMap operations") {

    val initial: ResultContext.WithGas[Int] = (5, GasMetrics(GasCost(10), 1, 1))
    val result = initial
      .flatMap(x => (x * 2, GasMetrics(GasCost(5), 2, 1)))
      .flatMap(x => (x + 10, GasMetrics(GasCost(3), 3, 1)))

    val (value, metrics) = result

    IO.pure(
      expect(value == 20) and
        expect(metrics.cost == GasCost(18)) and
        expect(metrics.depth == 3)
    )
  }

  test("syntax - mixed map and flatMap") {

    val initial: ResultContext.WithGas[Int] = (10, GasMetrics(GasCost(10), 1, 1))
    val result = initial
      .map(_ * 2)
      .flatMap(x => (x + 5, GasMetrics(GasCost(5), 2, 1)))
      .map(_ - 3)

    val (value, metrics) = result

    IO.pure(
      expect(value == 22) and
        expect(metrics.cost == GasCost(15)) and
        expect(metrics.depth == 2)
    )
  }
}