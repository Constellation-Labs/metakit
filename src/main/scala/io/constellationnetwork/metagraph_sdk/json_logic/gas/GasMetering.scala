package io.constellationnetwork.metagraph_sdk.json_logic.gas

import cats.syntax.all._
import cats.{Monad, Show}

import io.constellationnetwork.metagraph_sdk.json_logic.core.{JsonLogicException, JsonLogicValue}

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

@derive(encoder, decoder)
case class GasCost(amount: Long) {
  def +(other: GasCost): GasCost = GasCost(amount + other.amount)
  def *(multiplier: Long): GasCost = GasCost(amount * multiplier)
}

object GasCost {
  val Zero: GasCost = GasCost(0L)

  implicit val showInstance: Show[GasCost] =
    Show.show(gc => s"${gc.amount} gas")
}

@derive(encoder, decoder)
case class GasLimit(amount: Long) {
  def canAfford(cost: GasCost): Boolean = amount >= cost.amount

  def consume(cost: GasCost): Either[GasExhaustedException, GasLimit] =
    if (canAfford(cost))
      GasLimit(amount - cost.amount).asRight[GasExhaustedException]
    else
      GasExhaustedException(required = cost, available = this).asLeft[GasLimit]
}

object GasLimit {
  val Unlimited: GasLimit = GasLimit(Long.MaxValue)
  val Default: GasLimit = GasLimit(1_000_000L)

  implicit val showInstance: Show[GasLimit] =
    Show.show(gl => s"${gl.amount} gas limit")
}

@derive(encoder, decoder)
case class GasUsed(amount: Long) {
  def +(cost: GasCost): GasUsed = GasUsed(amount + cost.amount)
  def +(other: GasUsed): GasUsed = GasUsed(amount + other.amount)
}

object GasUsed {
  val Zero: GasUsed = GasUsed(0L)

  implicit val showInstance: Show[GasUsed] =
    Show.show(gu => s"${gu.amount} gas used")
}

@derive(encoder, decoder)
case class GasExhaustedException(
  required: GasCost,
  available: GasLimit
) extends JsonLogicException(
      s"Gas exhausted: required ${required.amount}, available ${available.amount}"
    )

@derive(encoder, decoder)
case class GasConfig(
  ifElse: GasCost = GasCost(10),
  default: GasCost = GasCost(5),
  not: GasCost = GasCost(1),
  doubleNot: GasCost = GasCost(1),
  or: GasCost = GasCost(2),
  and: GasCost = GasCost(2),
  eq: GasCost = GasCost(3),
  eqStrict: GasCost = GasCost(2),
  neq: GasCost = GasCost(3),
  neqStrict: GasCost = GasCost(2),
  lt: GasCost = GasCost(3),
  leq: GasCost = GasCost(3),
  gt: GasCost = GasCost(3),
  geq: GasCost = GasCost(3),
  add: GasCost = GasCost(5),
  minus: GasCost = GasCost(5),
  times: GasCost = GasCost(8),
  div: GasCost = GasCost(10),
  modulo: GasCost = GasCost(10),
  max: GasCost = GasCost(5),
  min: GasCost = GasCost(5),
  abs: GasCost = GasCost(2),
  round: GasCost = GasCost(3),
  floor: GasCost = GasCost(3),
  ceil: GasCost = GasCost(3),
  pow: GasCost = GasCost(20),
  map: GasCost = GasCost(10),
  filter: GasCost = GasCost(10),
  reduce: GasCost = GasCost(15),
  merge: GasCost = GasCost(5),
  all: GasCost = GasCost(10),
  some: GasCost = GasCost(10),
  none: GasCost = GasCost(10),
  find: GasCost = GasCost(10),
  count: GasCost = GasCost(5),
  in: GasCost = GasCost(8),
  intersect: GasCost = GasCost(15),
  unique: GasCost = GasCost(20),
  slice: GasCost = GasCost(5),
  reverse: GasCost = GasCost(5),
  flatten: GasCost = GasCost(10),
  cat: GasCost = GasCost(5),
  substr: GasCost = GasCost(8),
  lower: GasCost = GasCost(3),
  upper: GasCost = GasCost(3),
  join: GasCost = GasCost(10),
  split: GasCost = GasCost(15),
  trim: GasCost = GasCost(5),
  startsWith: GasCost = GasCost(5),
  endsWith: GasCost = GasCost(5),
  mapValues: GasCost = GasCost(5),
  mapKeys: GasCost = GasCost(5),
  get: GasCost = GasCost(3),
  has: GasCost = GasCost(3),
  entries: GasCost = GasCost(10),
  length: GasCost = GasCost(1),
  exists: GasCost = GasCost(5),
  missing: GasCost = GasCost(10),
  missingSome: GasCost = GasCost(15),
  typeOf: GasCost = GasCost(1),
  const: GasCost = GasCost.Zero,
  varAccess: GasCost = GasCost(2),
  depthPenaltyMultiplier: Long = 5L,
  collectionSizeMultiplier: Long = 1L
) {
  def depthPenalty(depth: Long): GasCost = GasCost(depth * depthPenaltyMultiplier)
  def sizeCost(size: Long): GasCost = GasCost(size * collectionSizeMultiplier)
}

object GasConfig {
  val Default: GasConfig = GasConfig()

  val Dev: GasConfig = GasConfig().copy(
    map = GasCost(5),
    filter = GasCost(5),
    reduce = GasCost(8)
  )

  val Mainnet: GasConfig = GasConfig().copy(
    pow = GasCost(50),
    unique = GasCost(30),
    split = GasCost(25),
    reduce = GasCost(20),
    depthPenaltyMultiplier = 10L
  )
}

@derive(encoder, decoder)
case class EvaluationResult[A](
  value: A,
  gasUsed: GasUsed,
  maxDepth: Int,
  operationCount: Long
)

object EvaluationResult {
  def pure[A](value: A): EvaluationResult[A] =
    EvaluationResult(value, GasUsed.Zero, maxDepth = 0, operationCount = 0)

  def withCost[A](value: A, cost: GasCost, depth: Int = 0): EvaluationResult[A] =
    EvaluationResult(value, GasUsed(cost.amount), maxDepth = depth, operationCount = 1)

  implicit def showInstance[A: Show]: Show[EvaluationResult[A]] = Show.show { result =>
    s"EvaluationResult(value=${result.value.show}, gas=${result.gasUsed.amount}, depth=${result.maxDepth}, ops=${result.operationCount})"
  }
}

object GasTracking {

  object syntax {
    implicit class GasTrackingOps[F[_]: Monad](operation: F[Either[JsonLogicException, JsonLogicValue]]) {

      def withGas(
        costFn: GasConfig => GasCost,
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      )(costModifier: JsonLogicValue => GasCost = _ => GasCost.Zero): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] = {
        val baseCost = costFn(gasConfig) + gasConfig.depthPenalty(depth.toLong)

        gasLimit
          .consume(baseCost)
          .fold(
            err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
            _ =>
              operation.map(_.map { value =>
                val total = baseCost + costModifier(value)
                (value, total, depth, 1)
              })
          )
      }
    }

    implicit class PreValidatedOps(args: List[JsonLogicValue]) {

      def validateThen[F[_]: Monad](
        errorCheck: List[JsonLogicValue] => Option[JsonLogicException]
      )(
        operation: List[JsonLogicValue] => F[Either[JsonLogicException, JsonLogicValue]]
      ): F[Either[JsonLogicException, JsonLogicValue]] =
        errorCheck(args).fold(operation(args))(err => err.asLeft[JsonLogicValue].pure[F])
    }
  }
}
