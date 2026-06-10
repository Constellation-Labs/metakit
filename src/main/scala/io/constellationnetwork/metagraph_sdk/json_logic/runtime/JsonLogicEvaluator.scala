package io.constellationnetwork.metagraph_sdk.json_logic.runtime

import cats.MonadThrow
import cats.effect.{Ref, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.core.{JsonLogicException, JsonLogicExpression, JsonLogicValue}
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{EvaluationResult, GasConfig, GasLimit, GasUsed}
import io.constellationnetwork.metagraph_sdk.json_logic.semantics.{GasAwareSemantics, JsonLogicSemantics}

trait JsonLogicEvaluator[F[_]] {
  def evaluate(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    ctx: Option[JsonLogicValue] = None
  ): F[Either[JsonLogicException, JsonLogicValue]]

  def evaluate(
    redex: (JsonLogicExpression, JsonLogicValue),
    ctx: Option[JsonLogicValue]
  ): F[Either[JsonLogicException, JsonLogicValue]]

  def evaluateAll(
    redexList: List[(JsonLogicExpression, JsonLogicValue)],
    ctx: Option[JsonLogicValue]
  ): F[Either[JsonLogicException, List[JsonLogicValue]]]

  def evaluateWithGas(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    ctx: Option[JsonLogicValue] = None,
    gasLimit: GasLimit,
    gasConfig: GasConfig = GasConfig.Default
  ): F[Either[JsonLogicException, EvaluationResult[JsonLogicValue]]]
}

object JsonLogicEvaluator {

  def make[F[_]: Sync]: JsonLogicEvaluator[F] = tailRecursive[F]

  def tailRecursive[F[_]: Sync]: JsonLogicEvaluator[F] = new JsonLogicEvaluator[F] {

    override def evaluate(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, JsonLogicValue]] = {

      def evalFn: JsonLogicSemantics.EvaluationCallback[F, cats.Id] =
        (e, c, d) => evaluateRecursive(e, c, data, d)

      def evaluateRecursive(
        expression: JsonLogicExpression,
        context: Option[JsonLogicValue],
        vars: JsonLogicValue,
        baseDepth: Int
      ): F[Either[JsonLogicException, JsonLogicValue]] = {
        val semantics = JsonLogicSemantics.make[F, cats.Id](vars, evalFn)
        JsonLogicRuntime.evaluate(expression, context, baseDepth)(MonadThrow[F], ResultContext.idContext, semantics)
      }

      evaluateRecursive(expr, ctx, data, 0)
    }

    override def evaluateWithGas(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue],
      gasLimit: GasLimit,
      gasConfig: GasConfig
    ): F[Either[JsonLogicException, EvaluationResult[JsonLogicValue]]] =
      Ref.of[F, GasLimit](gasLimit).flatMap { gasLimitRef =>
        def evaluateGasAware(
          expression: JsonLogicExpression,
          context: Option[JsonLogicValue],
          depth: Int,
          recDepth: Int,
          vars: JsonLogicValue
        ): F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] = {

          def evalFn(
            e: JsonLogicExpression,
            c: Option[JsonLogicValue],
            d: Int,
            rd: Int
          ): F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
            evaluateGasAware(e, c, d, rd, vars)

          val semantics = GasAwareSemantics.makeWithRef[F](vars, gasLimitRef, gasConfig, evalFn, depth)
          JsonLogicRuntime
            .evaluate(expression, context, recDepth)(Sync[F], ResultContext.gasContext, semantics)
            .map(_.map {
              case (value, metrics) =>
                (value, metrics.withDepth(depth + 1).incrementOps)
            })
        }

        evaluateGasAware(expr, ctx, 0, 0, data).flatMap {
          case Left(err)               => err.asLeft[EvaluationResult[JsonLogicValue]].pure[F]
          case Right((value, metrics)) =>
            // Report gasUsed as the gas-ref delta: the gas that was ACTUALLY consumed
            // (each op charged exactly once at evaluation time, see GasAwareSemantics).
            gasLimitRef.get.map { remaining =>
              EvaluationResult(value, GasUsed(gasLimit.amount - remaining.amount), metrics.depth, metrics.opCount.toLong)
                .asRight[JsonLogicException]
            }
        }
      }

    override def evaluate(
      redex: (JsonLogicExpression, JsonLogicValue),
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, JsonLogicValue]] =
      evaluate(redex._1, redex._2, ctx)

    override def evaluateAll(
      redexList: List[(JsonLogicExpression, JsonLogicValue)],
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, List[JsonLogicValue]]] =
      redexList.traverse {
        case (expr, data) =>
          evaluate(expr, data, ctx)
      }.map(_.sequence)
  }

  def recursive[F[_]: Sync]: JsonLogicEvaluator[F] = new JsonLogicEvaluator[F] {
    // Recursive strategy - uses direct recursive evaluation (may cause stack overflow on deep expressions)
    override def evaluate(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, JsonLogicValue]] = {

      def evalFn: JsonLogicSemantics.EvaluationCallback[F, cats.Id] =
        (e, c, d) => evaluateRecursive(e, c, data, d)

      def evaluateRecursive(
        expression: JsonLogicExpression,
        context: Option[JsonLogicValue],
        vars: JsonLogicValue,
        baseDepth: Int
      ): F[Either[JsonLogicException, JsonLogicValue]] = {
        val semantics = JsonLogicSemantics.make[F, cats.Id](vars, evalFn)
        JsonLogicRuntime.evaluateDirect(expression, context, baseDepth)(MonadThrow[F], ResultContext.idContext, semantics)
      }

      evaluateRecursive(expr, ctx, data, 0)
    }

    override def evaluateWithGas(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue],
      gasLimit: GasLimit,
      gasConfig: GasConfig
    ): F[Either[JsonLogicException, EvaluationResult[JsonLogicValue]]] =
      tailRecursive[F].evaluateWithGas(expr, data, ctx, gasLimit, gasConfig)

    override def evaluate(
      redex: (JsonLogicExpression, JsonLogicValue),
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, JsonLogicValue]] = evaluate(redex._1, redex._2, ctx)

    override def evaluateAll(
      redexList: List[(JsonLogicExpression, JsonLogicValue)],
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, List[JsonLogicValue]]] =
      redexList.traverse { case (e, d) => evaluate(e, d, ctx) }.map(_.sequence)
  }
}
