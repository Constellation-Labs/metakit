package io.constellationnetwork.metagraph_sdk.json_logic

import cats.Show

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

@derive(encoder, decoder)
case class GasMetrics(cost: GasCost, depth: Int, opCount: Int) {
  def combine(other: GasMetrics): GasMetrics =
    GasMetrics(
      cost + other.cost,
      Math.max(depth, other.depth),
      opCount + other.opCount
    )

  def withCost(additionalCost: GasCost): GasMetrics =
    copy(cost = cost + additionalCost)

  def withDepth(newDepth: Int): GasMetrics =
    copy(depth = Math.max(depth, newDepth))

  def incrementOps: GasMetrics =
    copy(opCount = opCount + 1)
}

object GasMetrics {
  val zero: GasMetrics = GasMetrics(GasCost.Zero, 0, 0)

  def single(cost: GasCost, depth: Int): GasMetrics =
    GasMetrics(cost, depth, 1)

  def fromCost(cost: GasCost): GasMetrics =
    GasMetrics(cost, 0, 0)

  implicit val showInstance: Show[GasMetrics] = Show.show { metrics =>
    s"GasMetrics(cost=${metrics.cost.amount}, depth=${metrics.depth}, opCount=${metrics.opCount})"
  }
}