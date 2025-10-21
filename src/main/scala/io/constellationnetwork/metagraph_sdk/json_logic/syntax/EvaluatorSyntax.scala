package io.constellationnetwork.metagraph_sdk.json_logic.syntax

import cats.Monad

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

trait EvaluatorSyntax {

  implicit class EvaluatorSyntaxOps[F[_]: Monad](evaluator: JsonLogicEvaluator[F]) {

    def evalAll(redexList: List[(JsonLogicExpression, JsonLogicValue)], ctx: Option[JsonLogicValue] = None): F[Either[JsonLogicException, List[JsonLogicValue]]] =
      evaluator.evaluateAll(redexList, ctx)

    def evalSingle(expr: JsonLogicExpression, data: JsonLogicValue, ctx: Option[JsonLogicValue] = None): F[Either[JsonLogicException, JsonLogicValue]] =
      evaluator.evaluate(expr, data, ctx)
  }
}