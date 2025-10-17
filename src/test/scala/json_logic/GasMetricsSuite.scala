package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic.{GasCost, GasMetrics}

import weaver.SimpleIOSuite

object GasMetricsSuite extends SimpleIOSuite {

  test("GasMetrics.zero has zero values") {
    val zero = GasMetrics.zero
    IO.pure(
      expect(zero.cost == GasCost.Zero) and
        expect(zero.depth == 0) and
        expect(zero.opCount == 0)
    )
  }

  test("GasMetrics.single creates metrics with one operation") {
    val cost = GasCost(100)
    val depth = 5
    val metrics = GasMetrics.single(cost, depth)

    IO.pure(
      expect(metrics.cost == cost) and
        expect(metrics.depth == depth) and
        expect(metrics.opCount == 1)
    )
  }

  test("GasMetrics.fromCost creates metrics with zero depth and ops") {
    val cost = GasCost(50)
    val metrics = GasMetrics.fromCost(cost)

    IO.pure(
      expect(metrics.cost == cost) and
        expect(metrics.depth == 0) and
        expect(metrics.opCount == 0)
    )
  }

  test("combine adds costs and operation counts") {
    val m1 = GasMetrics(GasCost(100), 3, 5)
    val m2 = GasMetrics(GasCost(50), 2, 3)
    val combined = m1.combine(m2)

    IO.pure(
      expect(combined.cost == GasCost(150)) and
        expect(combined.opCount == 8)
    )
  }

  test("combine takes maximum depth") {
    val m1 = GasMetrics(GasCost(100), 5, 2)
    val m2 = GasMetrics(GasCost(50), 3, 1)
    val combined = m1.combine(m2)

    IO.pure(expect(combined.depth == 5))
  }

  test("combine takes maximum depth when second is larger") {
    val m1 = GasMetrics(GasCost(100), 3, 2)
    val m2 = GasMetrics(GasCost(50), 7, 1)
    val combined = m1.combine(m2)

    IO.pure(expect(combined.depth == 7))
  }

  test("combine is associative") {
    val m1 = GasMetrics(GasCost(100), 3, 2)
    val m2 = GasMetrics(GasCost(50), 5, 1)
    val m3 = GasMetrics(GasCost(75), 4, 3)

    val leftAssoc = m1.combine(m2).combine(m3)
    val rightAssoc = m1.combine(m2.combine(m3))

    IO.pure(
      expect(leftAssoc.cost == rightAssoc.cost) and
        expect(leftAssoc.depth == rightAssoc.depth) and
        expect(leftAssoc.opCount == rightAssoc.opCount)
    )
  }

  test("combine with zero is identity") {
    val m = GasMetrics(GasCost(100), 5, 3)
    val combined = m.combine(GasMetrics.zero)

    IO.pure(
      expect(combined.cost == m.cost) and
        expect(combined.depth == m.depth) and
        expect(combined.opCount == m.opCount)
    )
  }

  test("withCost adds additional cost") {
    val m = GasMetrics(GasCost(100), 5, 2)
    val updated = m.withCost(GasCost(50))

    IO.pure(
      expect(updated.cost == GasCost(150)) and
        expect(updated.depth == 5) and
        expect(updated.opCount == 2)
    )
  }

  test("withDepth updates depth to higher value") {
    val m = GasMetrics(GasCost(100), 3, 2)
    val updated = m.withDepth(7)

    IO.pure(
      expect(updated.depth == 7) and
        expect(updated.cost == GasCost(100)) and
        expect(updated.opCount == 2)
    )
  }

  test("withDepth keeps existing depth if higher") {
    val m = GasMetrics(GasCost(100), 8, 2)
    val updated = m.withDepth(5)

    IO.pure(expect(updated.depth == 8))
  }

  test("incrementOps increases operation count by one") {
    val m = GasMetrics(GasCost(100), 5, 2)
    val updated = m.incrementOps

    IO.pure(
      expect(updated.opCount == 3) and
        expect(updated.cost == GasCost(100)) and
        expect(updated.depth == 5)
    )
  }

  test("chaining operations works correctly") {
    val m = GasMetrics.zero
      .withCost(GasCost(50))
      .withDepth(3)
      .incrementOps
      .withCost(GasCost(25))
      .withDepth(5)
      .incrementOps

    IO.pure(
      expect(m.cost == GasCost(75)) and
        expect(m.depth == 5) and
        expect(m.opCount == 2)
    )
  }

  test("combining multiple metrics accumulates correctly") {
    val metrics = List(
      GasMetrics(GasCost(10), 1, 1),
      GasMetrics(GasCost(20), 2, 1),
      GasMetrics(GasCost(30), 3, 1),
      GasMetrics(GasCost(40), 2, 1)
    )

    val combined = metrics.foldLeft(GasMetrics.zero)(_.combine(_))

    IO.pure(
      expect(combined.cost == GasCost(100)) and
        expect(combined.depth == 3) and
        expect(combined.opCount == 4)
    )
  }

  test("show instance produces readable output") {
    val m = GasMetrics(GasCost(100), 5, 3)
    val shown = cats.Show[GasMetrics].show(m)

    IO.pure(
      expect(shown.contains("100")) and
        expect(shown.contains("5")) and
        expect(shown.contains("3"))
    )
  }
}