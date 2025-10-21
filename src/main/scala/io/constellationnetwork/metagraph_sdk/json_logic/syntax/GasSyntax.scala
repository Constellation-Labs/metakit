package io.constellationnetwork.metagraph_sdk.json_logic.syntax

import cats.Monad

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

trait GasSyntax {

  implicit class GasEvaluatorSyntaxOps[F[_]: Monad](evaluator: JsonLogicEvaluator[F]) {

    def evalWithGas(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      gasLimit: GasLimit,
      ctx: Option[JsonLogicValue] = None,
      gasConfig: GasConfig = GasConfig.Default
    ): F[Either[JsonLogicException, EvaluationResult[JsonLogicValue]]] =
      evaluator.evaluateWithGas(expr, data, ctx, gasLimit, gasConfig)
  }

  implicit class GasLimitSyntaxOps(limit: GasLimit) {

    def consumeGas(cost: GasCost): Either[GasExhaustedException, GasLimit] =
      limit.consume(cost)

    def hasEnough(cost: GasCost): Boolean =
      limit.amount >= cost.amount
  }

  implicit class GasCostSyntaxOps(cost: GasCost) {

    def +(other: GasCost): GasCost =
      GasCost(cost.amount + other.amount)

    def *(multiplier: Long): GasCost =
      GasCost(cost.amount * multiplier)
  }
}