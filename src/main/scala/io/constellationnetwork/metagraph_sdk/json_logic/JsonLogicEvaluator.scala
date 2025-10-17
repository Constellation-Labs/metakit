package io.constellationnetwork.metagraph_sdk.json_logic

import cats.syntax.all._
import cats.{Monad, MonadThrow}

trait JsonLogicEvaluator[F[_]] {

  def evaluateWithGas(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    ctx: Option[JsonLogicValue],
    gasLimit: GasLimit,
    gasConfig: GasConfig = GasConfig.Default
  ): F[EvaluationResult[JsonLogicValue]]

  def evaluate(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    ctx: Option[JsonLogicValue]
  ): F[JsonLogicValue]

  def evaluate(
    redex: (JsonLogicExpression, JsonLogicValue),
    ctx: Option[JsonLogicValue]
  ): F[JsonLogicValue]

  def evaluateAll(
    redexList: List[(JsonLogicExpression, JsonLogicValue)],
    ctx: Option[JsonLogicValue]
  ): F[List[JsonLogicValue]]
}

object JsonLogicEvaluator {

  def apply[F[_]](implicit ev: JsonLogicEvaluator[F]): JsonLogicEvaluator[F] = ev

  def make[F[_]: MonadThrow](
    strategy: JsonLogicRuntime.EvaluationStrategy
  ): JsonLogicEvaluator[F] = new JsonLogicEvaluator[F] {

    override def evaluateWithGas(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue],
      gasLimit: GasLimit,
      gasConfig: GasConfig = GasConfig.Default
    ): F[EvaluationResult[JsonLogicValue]] = {

      def evaluateGasAware(
        expr: JsonLogicExpression,
        ctx: Option[JsonLogicValue],
        limit: GasLimit,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] = {

        val baseSemantics: JsonLogicSemantics[F] = JsonLogicSemantics.make[F](
          data,
          (e, c) => evaluateGasAware(e, c, limit, depth + 1).map(_.map(_._1))
        )

        lazy val gasAwareEval: GasAwareSemantics.EvaluationCallbackWithGas[F] =
          (e, c, l, _, d) => evaluateGasAware(e, c, l, d)

        val gasAwareSemantics: GasAwareSemantics[F] = GasAwareSemantics.fromBase(baseSemantics, gasAwareEval)

        expr match {
          case ConstExpression(v) =>
            (v, gasConfig.const, depth, 0).asRight[JsonLogicException].pure[F]

          case VarExpression(Left(key), _) =>
            gasAwareSemantics
              .getVarWithGas(key, ctx, gasConfig)
              .map(_.map {
                case (value, cost) => (value, cost, depth, 0)
              })

          case VarExpression(Right(varExpr), _) =>
            evaluateGasAware(varExpr, ctx, limit, depth + 1).flatMap {
              case Left(err) => err.asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
              case Right((StrValue(name), exprCost, exprDepth, exprOpCount)) =>
                gasAwareSemantics
                  .getVarWithGas(name, ctx, gasConfig)
                  .map(_.map {
                    case (value, varCost) =>
                      (value, exprCost + varCost, Math.max(depth, exprDepth), exprOpCount + 1)
                  })
              case Right((ArrayValue(StrValue(name) :: _), exprCost, exprDepth, exprOpCount)) =>
                gasAwareSemantics
                  .getVarWithGas(name, ctx, gasConfig)
                  .map(_.map {
                    case (value, varCost) =>
                      (value, exprCost + varCost, Math.max(depth, exprDepth), exprOpCount + 1)
                  })
              case Right((v, _, _, _)) =>
                JsonLogicException(s"Got non-string input for var expression: $v")
                  .asLeft[(JsonLogicValue, GasCost, Int, Int)]
                  .pure[F]
            }

          case ArrayExpression(args) =>
            val arrayCost = gasConfig.arrayExpr + gasConfig.depthPenalty(depth.toLong)

            limit
              .consume(arrayCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  args
                    .foldLeftM[F, Either[JsonLogicException, (List[JsonLogicValue], GasCost, Int, Int)]](
                      (List.empty[JsonLogicValue], arrayCost, depth, 1).asRight[JsonLogicException]
                    ) {
                      case (accEither, argExpr) =>
                        accEither match {
                          case Left(err) => err.asLeft[(List[JsonLogicValue], GasCost, Int, Int)].pure[F]
                          case Right((values, accGas, maxDepth, opCount)) =>
                            evaluateGasAware(argExpr, ctx, remainingLimit, depth + 1).map {
                              case Left(err) => err.asLeft[(List[JsonLogicValue], GasCost, Int, Int)]
                              case Right((value, cost, argDepth, argOps)) =>
                                (values :+ value, accGas + cost, Math.max(maxDepth, argDepth), opCount + argOps)
                                  .asRight[JsonLogicException]
                            }
                        }
                    }
                    .map(_.map {
                      case (values, totalGas, maxDepth, opCount) =>
                        (ArrayValue(values), totalGas, maxDepth, opCount)
                    })
              )

          case MapExpression(map) =>
            val mapCost = gasConfig.mapExpr + gasConfig.depthPenalty(depth.toLong)

            limit
              .consume(mapCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  map.toList
                    .foldLeftM[F, Either[JsonLogicException, (Map[String, JsonLogicValue], GasCost, Int, Int)]](
                      (Map.empty[String, JsonLogicValue], mapCost, depth, 1).asRight[JsonLogicException]
                    ) {
                      case (accEither, (key, valueExpr)) =>
                        accEither match {
                          case Left(err) => err.asLeft[(Map[String, JsonLogicValue], GasCost, Int, Int)].pure[F]
                          case Right((pairs, accGas, maxDepth, opCount)) =>
                            evaluateGasAware(valueExpr, ctx, remainingLimit, depth + 1).map {
                              case Left(err) => err.asLeft[(Map[String, JsonLogicValue], GasCost, Int, Int)]
                              case Right((value, cost, valueDepth, valueOps)) =>
                                (pairs + (key -> value), accGas + cost, Math.max(maxDepth, valueDepth), opCount + valueOps)
                                  .asRight[JsonLogicException]
                            }
                        }
                    }
                    .map(_.map {
                      case (pairs, totalGas, maxDepth, opCount) =>
                        (MapValue(pairs), totalGas, maxDepth, opCount)
                    })
              )

          case ApplyExpression(op, args) =>
            def isIfElseBranchArg(argIndex: Int): Boolean = op match {
              case JsonLogicOp.IfElseOp =>
                val isOddIndex = argIndex % 2 == 1
                val isLastArg = argIndex == args.length - 1
                isOddIndex || isLastArg
              case _ => false
            }

            args
              .foldLeftM[F, Either[JsonLogicException, (List[JsonLogicValue], GasCost, Int, Int)]](
                (List.empty[JsonLogicValue], GasCost.Zero, depth, 0).asRight[JsonLogicException]
              ) {
                case (accEither, argExpr) =>
                  accEither match {
                    case Left(err) => err.asLeft[(List[JsonLogicValue], GasCost, Int, Int)].pure[F]
                    case Right((values, accGas, maxDepth, opCount)) =>
                      argExpr match {
                        case ConstExpression(FunctionValue(innerExpr)) if isCallbackArg(op, values.size) =>
                          (values :+ FunctionValue(innerExpr), accGas, maxDepth, opCount).asRight[JsonLogicException].pure[F]
                        case expr: JsonLogicExpression if isCallbackArg(op, values.size) =>
                          (values :+ FunctionValue(expr), accGas, maxDepth, opCount).asRight[JsonLogicException].pure[F]
                        case expr: JsonLogicExpression if isIfElseBranchArg(values.size) =>
                          (values :+ FunctionValue(expr), accGas, maxDepth, opCount).asRight[JsonLogicException].pure[F]
                        case expr: JsonLogicExpression =>
                          evaluateGasAware(expr, ctx, limit, depth + 1).map {
                            case Left(err) => err.asLeft[(List[JsonLogicValue], GasCost, Int, Int)]
                            case Right((value, cost, argDepth, argOps)) =>
                              (values :+ value, accGas + cost, Math.max(maxDepth, argDepth), opCount + argOps)
                                .asRight[JsonLogicException]
                          }
                        case _ =>
                          (values :+ NullValue, accGas, maxDepth, opCount).asRight[JsonLogicException].pure[F]
                      }
                  }
              }
              .flatMap {
                case Left(err) => err.asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
                case Right((argValues, argGas, maxArgDepth, argOpCount)) =>
                  gasAwareSemantics
                    .applyOpWithGas(op, argValues, limit, gasConfig, depth)
                    .map(_.map {
                      case (result, opCost, opDepth, opOps) =>
                        (result, argGas + opCost, Math.max(maxArgDepth, opDepth), argOpCount + opOps)
                    })
              }
        }
      }

      def isCallbackArg(op: JsonLogicOp, argIndex: Int): Boolean = op match {
        case JsonLogicOp.MapOp | JsonLogicOp.FilterOp | JsonLogicOp.AllOp | JsonLogicOp.SomeOp | JsonLogicOp.NoneOp | JsonLogicOp.FindOp |
            JsonLogicOp.CountOp =>
          argIndex == 1
        case JsonLogicOp.ReduceOp =>
          argIndex == 1
        case _ =>
          false
      }

      evaluateGasAware(expr, ctx, gasLimit, 0).flatMap {
        case Left(err) => MonadThrow[F].raiseError(err)
        case Right((value, gasUsed, maxDepth, opCount)) =>
          EvaluationResult(value, GasUsed(gasUsed.amount), maxDepth, opCount.toLong).pure[F]
      }
    }

    override def evaluate(
      expr: JsonLogicExpression,
      data: JsonLogicValue,
      ctx: Option[JsonLogicValue]
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
      ctx: Option[JsonLogicValue] = None
    ): F[JsonLogicValue] =
      evaluate(redex._1, redex._2, ctx)

    override def evaluateAll(
      redexList: List[(JsonLogicExpression, JsonLogicValue)],
      ctx: Option[JsonLogicValue] = None
    ): F[List[JsonLogicValue]] =
      redexList.traverse {
        case (expr, data) =>
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

      override def evaluateWithGas(
        expr: JsonLogicExpression,
        data: JsonLogicValue,
        ctx: Option[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig = GasConfig.Default
      ): F[EvaluationResult[JsonLogicValue]] =
        for {
          _      <- logger(s"Evaluating with gas: $expr (limit: ${gasLimit.amount})")
          result <- evaluator.evaluateWithGas(expr, data, ctx, gasLimit, gasConfig)
          _      <- logger(s"Result: ${result.value}, Gas used: ${result.gasUsed.amount}")
        } yield result

      override def evaluate(
        expr: JsonLogicExpression,
        data: JsonLogicValue,
        ctx: Option[JsonLogicValue]
      ): F[JsonLogicValue] =
        for {
          _      <- logger(s"Evaluating expression: $expr")
          result <- evaluator.evaluate(expr, data, ctx)
          _      <- logger(s"Result: $result")
        } yield result

      override def evaluate(
        redex: (JsonLogicExpression, JsonLogicValue),
        ctx: Option[JsonLogicValue] = None
      ): F[JsonLogicValue] =
        evaluate(redex._1, redex._2, ctx)

      override def evaluateAll(
        redexList: List[(JsonLogicExpression, JsonLogicValue)],
        ctx: Option[JsonLogicValue] = None
      ): F[List[JsonLogicValue]] =
        redexList.traverse {
          case (expr, data) =>
            evaluate(expr, data, ctx)
        }
    }
  }
}
