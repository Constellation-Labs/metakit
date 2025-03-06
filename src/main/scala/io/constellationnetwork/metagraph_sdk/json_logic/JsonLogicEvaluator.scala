package io.constellationnetwork.metagraph_sdk.json_logic

import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Monad, MonadThrow}

trait JsonLogicEvaluator[F[_]] {

  def evaluate(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    ctx:  Option[JsonLogicValue]
  ): F[JsonLogicValue]

  def evaluate(
    redex: (JsonLogicExpression, JsonLogicValue),
    ctx:   Option[JsonLogicValue]
  ): F[JsonLogicValue]

  def evaluateAll(
    redexList: List[(JsonLogicExpression, JsonLogicValue)],
    ctx:       Option[JsonLogicValue]
  ): F[List[JsonLogicValue]]
}

object JsonLogicEvaluator {

  def apply[F[_]](implicit ev: JsonLogicEvaluator[F]): JsonLogicEvaluator[F] = ev

  def make[F[_]: MonadThrow](
    strategy: JsonLogicRuntime.EvaluationStrategy
  ): JsonLogicEvaluator[F] = new JsonLogicEvaluator[F] {

    override def evaluate(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx:  Option[JsonLogicValue]
    ): F[JsonLogicValue] = {
      lazy val evalFn: (JsonLogicExpression, Option[JsonLogicValue]) => F[Either[JsonLogicException, JsonLogicValue]] =
        (e, c) => strategy[F](e, c)(Monad[F], semantics)

      lazy val semantics: JsonLogicSemantics[F] =
        JsonLogicSemantics.make[F](data, evalFn)

      strategy[F](expr, ctx)(Monad[F], semantics).flatMap {
        case Right(value) => value.pure[F]
        case Left(ex)     => ex.raiseError
      }
    }

    override def evaluate(
      redex: (JsonLogicExpression, JsonLogicValue),
      ctx:   Option[JsonLogicValue] = None
    ): F[JsonLogicValue] =
      evaluate(redex._1, redex._2, ctx)

    override def evaluateAll(
      redexList: List[(JsonLogicExpression, JsonLogicValue)],
      ctx:       Option[JsonLogicValue] = None
    ): F[List[JsonLogicValue]] =
      redexList.traverse { case (expr, data) =>
        evaluate(expr, data, ctx)
      }
  }

  def recursive[F[_]: MonadThrow]: JsonLogicEvaluator[F] =
    make[F](JsonLogicRuntime.RecursiveStrategy)

  def tailRecursive[F[_]: MonadThrow]: JsonLogicEvaluator[F] =
    make[F](JsonLogicRuntime.TailRecursiveStrategy)

  implicit class EvaluatorOps[F[_]](private val evaluator: JsonLogicEvaluator[F]) {

    def withLogging(
      logger: String => F[Unit]
    )(implicit F: Monad[F]): JsonLogicEvaluator[F] = new JsonLogicEvaluator[F] {

      override def evaluate(
        expr: JsonLogicExpression,
        data: JsonLogicValue,
        ctx:  Option[JsonLogicValue]
      ): F[JsonLogicValue] =
        for {
          _      <- logger(s"Evaluating expression: $expr")
          result <- evaluator.evaluate(expr, data, ctx)
          _      <- logger(s"Result: $result")
        } yield result

      override def evaluate(
        redex: (JsonLogicExpression, JsonLogicValue),
        ctx:   Option[JsonLogicValue] = None
      ): F[JsonLogicValue] =
        evaluate(redex._1, redex._2, ctx)

      override def evaluateAll(
        redexList: List[(JsonLogicExpression, JsonLogicValue)],
        ctx:       Option[JsonLogicValue] = None
      ): F[List[JsonLogicValue]] =
        redexList.traverse { case (expr, data) =>
          evaluate(expr, data, ctx)
        }
    }
  }
}
