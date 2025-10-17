package io.constellationnetwork.metagraph_sdk.json_logic

import cats.MonadThrow
import cats.syntax.all._

trait JsonLogicEvaluatorV2[F[_]] {
  def evaluate(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    ctx: Option[JsonLogicValue] = None
  ): F[JsonLogicValue]

  def evaluate(
    redex: (JsonLogicExpression, JsonLogicValue),
    ctx: Option[JsonLogicValue]
  ): F[JsonLogicValue]

  def evaluateAll(
    redexList: List[(JsonLogicExpression, JsonLogicValue)],
    ctx: Option[JsonLogicValue]
  ): F[List[JsonLogicValue]]

  def evaluateWithGas(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    ctx: Option[JsonLogicValue] = None,
    gasLimit: GasLimit,
    gasConfig: GasConfig = GasConfig.Default
  ): F[EvaluationResult[JsonLogicValue]]
}

object JsonLogicEvaluatorV2 {

  def make[F[_]: MonadThrow]: JsonLogicEvaluatorV2[F] = tailRecursive[F]

  def tailRecursive[F[_]: MonadThrow]: JsonLogicEvaluatorV2[F] = new JsonLogicEvaluatorV2[F] {

    override def evaluate(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue]
    ): F[JsonLogicValue] = {

      def evalFn: JsonLogicSemanticsV2.EvaluationCallback[F, cats.Id] =
        (e, c) => evaluateRecursive(e, c, data)

      def evaluateRecursive(
        expression: JsonLogicExpression,
        context: Option[JsonLogicValue],
        vars: JsonLogicValue
      ): F[Either[JsonLogicException, JsonLogicValue]] = {
        val semantics = JsonLogicSemanticsV2.make[F, cats.Id](vars, evalFn)
        JsonLogicRuntimeV2.evaluate(expression, context)(MonadThrow[F], ResultContext.idContext, semantics)
      }

      evaluateRecursive(expr, ctx, data).flatMap {
        case Right(value) => value.pure[F]
        case Left(ex) => ex.raiseError[F, JsonLogicValue]
      }
    }

    override def evaluateWithGas(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue],
      gasLimit: GasLimit,
      gasConfig: GasConfig
    ): F[EvaluationResult[JsonLogicValue]] = {

      def evaluateGasAware(
        expression: JsonLogicExpression,
        context: Option[JsonLogicValue],
        depth: Int,
        vars: JsonLogicValue
      ): F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] = {

        def evalFn(e: JsonLogicExpression, c: Option[JsonLogicValue], d: Int):
          F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
          evaluateGasAware(e, c, d, vars)

        val semantics = GasAwareSemanticsV2.make[F](vars, gasLimit, gasConfig, evalFn)
        JsonLogicRuntimeV2.evaluate(expression, context)(MonadThrow[F], ResultContext.gasContext, semantics)
          .map(_.map {
            case (value, metrics) =>
              (value, metrics.withDepth(depth + 1).incrementOps)
          })
      }

      evaluateGasAware(expr, ctx, 0, data).flatMap {
        case Left(err) => err.raiseError[F, EvaluationResult[JsonLogicValue]]
        case Right((value, metrics)) =>
          EvaluationResult(value, GasUsed(metrics.cost.amount), metrics.depth, metrics.opCount.toLong).pure[F]
      }
    }

    override def evaluate(
      redex: (JsonLogicExpression, JsonLogicValue),
      ctx: Option[JsonLogicValue]
    ): F[JsonLogicValue] =
      evaluate(redex._1, redex._2, ctx)

    override def evaluateAll(
      redexList: List[(JsonLogicExpression, JsonLogicValue)],
      ctx: Option[JsonLogicValue]
    ): F[List[JsonLogicValue]] =
      redexList.traverse {
        case (expr, data) =>
          evaluate(expr, data, ctx)
      }
  }

  def recursive[F[_]: MonadThrow]: JsonLogicEvaluatorV2[F] = new JsonLogicEvaluatorV2[F] {
    // Recursive strategy - uses direct recursive evaluation (may cause stack overflow on deep expressions)
    override def evaluate(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue]
    ): F[JsonLogicValue] = {

      def evalFn: JsonLogicSemanticsV2.EvaluationCallback[F, cats.Id] =
        (e, c) => evaluateRecursive(e, c, data)

      def evaluateRecursive(
        expression: JsonLogicExpression,
        context: Option[JsonLogicValue],
        vars: JsonLogicValue
      ): F[Either[JsonLogicException, JsonLogicValue]] = {
        val semantics = JsonLogicSemanticsV2.make[F, cats.Id](vars, evalFn)
        JsonLogicRuntimeV2.evaluateDirect(expression, context)(MonadThrow[F], ResultContext.idContext, semantics)
      }

      evaluateRecursive(expr, ctx, data).flatMap {
        case Right(value) => value.pure[F]
        case Left(ex) => ex.raiseError[F, JsonLogicValue]
      }
    }

    override def evaluateWithGas(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue],
      gasLimit: GasLimit,
      gasConfig: GasConfig
    ): F[EvaluationResult[JsonLogicValue]] = tailRecursive[F].evaluateWithGas(expr, data, ctx, gasLimit, gasConfig)

    override def evaluate(
      redex: (JsonLogicExpression, JsonLogicValue),
      ctx: Option[JsonLogicValue]
    ): F[JsonLogicValue] = evaluate(redex._1, redex._2, ctx)

    override def evaluateAll(
      redexList: List[(JsonLogicExpression, JsonLogicValue)],
      ctx: Option[JsonLogicValue]
    ): F[List[JsonLogicValue]] = redexList.traverse { case (e, d) => evaluate(e, d, ctx) }
  }
}