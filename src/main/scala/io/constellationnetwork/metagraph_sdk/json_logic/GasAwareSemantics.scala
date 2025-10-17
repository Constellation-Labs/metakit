package io.constellationnetwork.metagraph_sdk.json_logic

import cats.Monad
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.GasTracking.syntax._
import io.constellationnetwork.metagraph_sdk.json_logic.JsonLogicOp._

trait GasAwareSemantics[F[_]] {
  def applyOpWithGas(
    op: JsonLogicOp,
    args: List[JsonLogicValue],
    gasLimit: GasLimit,
    gasConfig: GasConfig,
    depth: Int
  ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]]

  def getVarWithGas(
    key: String,
    ctx: Option[JsonLogicValue],
    gasConfig: GasConfig
  ): F[Either[JsonLogicException, (JsonLogicValue, GasCost)]]
}

object GasAwareSemantics {

  type EvaluationCallbackWithGas[F[_]] =
    (
      JsonLogicExpression,
      Option[JsonLogicValue],
      GasLimit,
      GasConfig,
      Int
    ) => F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]]

  def fromBase[F[_]: Monad](
    baseSemantics: JsonLogicSemantics[F],
    gasAwareEval: EvaluationCallbackWithGas[F]
  ): GasAwareSemantics[F] =
    new GasAwareSemantics[F] {

      override def applyOpWithGas(
        op: JsonLogicOp,
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        op match {
          case DivOp | ModuloOp =>
            args
              .validateThen(divisionByZeroCheck)(baseSemantics.applyOp(op))
              .withGas(getOpCost(op), gasLimit, gasConfig, depth)()

          case AddOp | TimesOp | MinusOp =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth)(arithmeticSizeModifier(op, args))

          case PowOp =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth)(powExponentModifier(args))

          case CatOp =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth) {
                case StrValue(s) => GasCost(s.length.toLong)
                case _           => GasCost.Zero
              }

          case SplitOp =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth) {
                case ArrayValue(arr) => GasCost(arr.size.toLong * 2)
                case _               => GasCost.Zero
              }

          case MergeOp =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth) {
                case ArrayValue(arr) => GasCost(arr.size.toLong)
                case MapValue(m)     => GasCost(m.size.toLong)
                case _               => GasCost.Zero
              }

          case UniqueOp =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth) { _ =>
                args match {
                  case ArrayValue(arr) :: Nil => GasCost((arr.size.toLong * arr.size.toLong) / 10)
                  case _                      => GasCost.Zero
                }
              }

          case Lt | Leq | Gt | Geq =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth) { _ =>
                args match {
                  case _ :: _ :: _ :: Nil => GasCost(1)
                  case _                  => GasCost.Zero
                }
              }

          case OrOp =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth) { _ =>
                val evaluatedCount = args.takeWhile(!_.isTruthy).length + 1
                GasCost(evaluatedCount.toLong)
              }

          case AndOp =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth)(_ => GasCost(args.size.toLong))

          case MapOp =>
            handleMapOpWithGas(args, gasLimit, gasConfig, depth)

          case FilterOp =>
            handleFilterOpWithGas(args, gasLimit, gasConfig, depth)

          case ReduceOp =>
            handleReduceOpWithGas(args, gasLimit, gasConfig, depth)

          case AllOp =>
            handleAllOpWithGas(args, gasLimit, gasConfig, depth)

          case NoneOp =>
            handleNoneOpWithGas(args, gasLimit, gasConfig, depth)

          case SomeOp =>
            handleSomeOpWithGas(args, gasLimit, gasConfig, depth)

          case CountOp =>
            args match {
              case ArrayValue(_) :: FunctionValue(_) :: Nil =>
                handleCountOpWithGas(args, gasLimit, gasConfig, depth)
              case _ =>
                baseSemantics
                  .applyOp(op)(args)
                  .withGas(getOpCost(op), gasLimit, gasConfig, depth)()
            }

          case FindOp =>
            handleFindOpWithGas(args, gasLimit, gasConfig, depth)

          case _ =>
            baseSemantics
              .applyOp(op)(args)
              .withGas(getOpCost(op), gasLimit, gasConfig, depth)()
        }

      override def getVarWithGas(
        key: String,
        ctx: Option[JsonLogicValue],
        gasConfig: GasConfig
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost)]] = {
        val cost = gasConfig.varAccess + GasCost(key.split("\\.").length.toLong)
        baseSemantics.getVar(key, ctx).map(_.map((_, cost)))
      }

      private def getOpCost(op: JsonLogicOp)(config: GasConfig): GasCost = op match {
        case NoOp          => GasCost.Zero
        case MissingNoneOp => config.exists
        case ExistsOp      => config.exists
        case MissingSomeOp => config.missingSome
        case IfElseOp      => config.ifElse
        case EqOp          => config.eq
        case EqStrictOp    => config.eqStrict
        case NEqOp         => config.neq
        case NEqStrictOp   => config.neqStrict
        case NotOp         => config.not
        case NOp           => config.doubleNot
        case OrOp          => config.or
        case AndOp         => config.and
        case Lt            => config.lt
        case Leq           => config.leq
        case Gt            => config.gt
        case Geq           => config.geq
        case ModuloOp      => config.modulo
        case MaxOp         => config.max
        case MinOp         => config.min
        case AddOp         => config.add
        case TimesOp       => config.times
        case MinusOp       => config.minus
        case DivOp         => config.div
        case MergeOp       => config.merge
        case InOp          => config.in
        case CatOp         => config.cat
        case SubStrOp      => config.substr
        case MapOp         => config.map
        case FilterOp      => config.filter
        case ReduceOp      => config.reduce
        case AllOp         => config.all
        case NoneOp        => config.none
        case SomeOp        => config.some
        case MapValuesOp   => config.mapValues
        case MapKeysOp     => config.mapKeys
        case GetOp         => config.get
        case IntersectOp   => config.intersect
        case CountOp       => config.count
        case LengthOp      => config.length
        case FindOp        => config.find
        case LowerOp       => config.lower
        case UpperOp       => config.upper
        case JoinOp        => config.join
        case SplitOp       => config.split
        case DefaultOp     => config.default
        case UniqueOp      => config.unique
        case SliceOp       => config.slice
        case ReverseOp     => config.reverse
        case FlattenOp     => config.flatten
        case TrimOp        => config.trim
        case StartsWithOp  => config.startsWith
        case EndsWithOp    => config.endsWith
        case AbsOp         => config.abs
        case RoundOp       => config.round
        case FloorOp       => config.floor
        case CeilOp        => config.ceil
        case PowOp         => config.pow
        case HasOp         => config.has
        case EntriesOp     => config.entries
        case TypeOfOp      => config.typeOf
      }

      private def divisionByZeroCheck(args: List[JsonLogicValue]): Option[JsonLogicException] =
        args match {
          case _ :: IntValue(zero) :: Nil if zero == 0 => Some(JsonLogicException("Division by zero"))
          case _ :: FloatValue(r) :: Nil if r == 0     => Some(JsonLogicException("Division by zero"))
          case _                                       => None
        }

      private def arithmeticSizeModifier(_op: JsonLogicOp, args: List[JsonLogicValue])(_value: JsonLogicValue): GasCost =
        args match {
          case ArrayValue(arr) :: Nil => GasCost(arr.size.toLong)
          case list if list.size > 1  => GasCost((list.size - 1).toLong)
          case _                      => GasCost.Zero
        }

      private def powExponentModifier(args: List[JsonLogicValue])(_value: JsonLogicValue): GasCost =
        args match {
          case _ :: IntValue(exp) :: Nil   => GasCost(exp.abs.toLong.min(1000))
          case _ :: FloatValue(exp) :: Nil => GasCost(exp.abs.toLong.min(1000))
          case _                           => GasCost.Zero
        }

      private def handleMapOpWithGas(
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            val baseCost = gasConfig.map + gasConfig.depthPenalty(depth.toLong)

            gasLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  arr
                    .foldLeftM[F, Either[JsonLogicException, (List[JsonLogicValue], GasCost, Int, Int)]](
                      (List.empty[JsonLogicValue], baseCost, depth, 1).asRight[JsonLogicException]
                    ) {
                      case (accEither, element) =>
                        accEither match {
                          case Left(err) => err.asLeft[(List[JsonLogicValue], GasCost, Int, Int)].pure[F]
                          case Right((results, accGas, maxDepth, opCount)) =>
                            val newTotalGas = accGas + GasCost(1)
                            if (!gasLimit.canAfford(newTotalGas)) {
                              (GasExhaustedException(
                                required = newTotalGas,
                                available = gasLimit
                              ): JsonLogicException).asLeft[(List[JsonLogicValue], GasCost, Int, Int)].pure[F]
                            } else {
                              gasAwareEval(expr, element.some, remainingLimit, gasConfig, depth + 1).map {
                                case Left(err) => err.asLeft[(List[JsonLogicValue], GasCost, Int, Int)]
                                case Right((value, callbackGas, cbDepth, cbOps)) =>
                                  val totalGasAfterCallback = accGas + callbackGas
                                  if (!gasLimit.canAfford(totalGasAfterCallback)) {
                                    (GasExhaustedException(
                                      required = totalGasAfterCallback,
                                      available = gasLimit
                                    ): JsonLogicException).asLeft[(List[JsonLogicValue], GasCost, Int, Int)]
                                  } else {
                                    (results :+ value, totalGasAfterCallback, Math.max(maxDepth, cbDepth), opCount + cbOps)
                                      .asRight[JsonLogicException]
                                  }
                              }
                            }
                        }
                    }
                    .map(_.map { case (results, totalGas, maxDepth, totalOps) => (ArrayValue(results), totalGas, maxDepth, totalOps) })
              )

          case _ =>
            JsonLogicException(s"Unexpected input to ${MapOp.tag}, got $args").asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
        }

      private def handleFilterOpWithGas(
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            val baseCost = gasConfig.filter + gasConfig.depthPenalty(depth.toLong)

            gasLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  arr
                    .foldLeftM[F, Either[JsonLogicException, (List[JsonLogicValue], GasCost, Int, Int)]](
                      (List.empty[JsonLogicValue], baseCost, depth, 1).asRight[JsonLogicException]
                    ) {
                      case (accEither, element) =>
                        accEither match {
                          case Left(err) => err.asLeft[(List[JsonLogicValue], GasCost, Int, Int)].pure[F]
                          case Right((results, accGas, maxDepth, opCount)) =>
                            if (!gasLimit.canAfford(accGas)) {
                              (GasExhaustedException(
                                required = accGas,
                                available = gasLimit
                              ): JsonLogicException).asLeft[(List[JsonLogicValue], GasCost, Int, Int)].pure[F]
                            } else {
                              gasAwareEval(expr, element.some, remainingLimit, gasConfig, depth + 1).map {
                                case Left(err) => err.asLeft[(List[JsonLogicValue], GasCost, Int, Int)]
                                case Right((value, callbackGas, cbDepth, cbOps)) =>
                                  val totalGasAfterCallback = accGas + callbackGas
                                  if (!gasLimit.canAfford(totalGasAfterCallback)) {
                                    (GasExhaustedException(
                                      required = totalGasAfterCallback,
                                      available = gasLimit
                                    ): JsonLogicException).asLeft[(List[JsonLogicValue], GasCost, Int, Int)]
                                  } else {
                                    val newResults = if (value.isTruthy) results :+ element else results
                                    (newResults, totalGasAfterCallback, Math.max(maxDepth, cbDepth), opCount + cbOps)
                                      .asRight[JsonLogicException]
                                  }
                              }
                            }
                        }
                    }
                    .map(_.map { case (results, totalGas, maxDepth, totalOps) => (ArrayValue(results), totalGas, maxDepth, totalOps) })
              )

          case _ =>
            JsonLogicException(s"Unexpected input to ${FilterOp.tag}").asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
        }

      private def handleReduceOpWithGas(
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil if arr.nonEmpty =>
            val baseCost = gasConfig.reduce + gasConfig.depthPenalty(depth.toLong)

            gasLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  arr.tail.foldLeftM[F, Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]](
                    ((arr.head: JsonLogicValue), baseCost, depth, 1).asRight[JsonLogicException]
                  ) {
                    case (accEither, item) =>
                      accEither match {
                        case Left(err) => err.asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
                        case Right((accValue, accGas, maxDepth, opCount)) =>
                          val ctx = MapValue(Map("current" -> item, "accumulator" -> accValue))
                          gasAwareEval(expr, ctx.some, remainingLimit, gasConfig, depth + 1).map {
                            case Left(err) => err.asLeft[(JsonLogicValue, GasCost, Int, Int)]
                            case Right((value, callbackGas, cbDepth, cbOps)) =>
                              (value, accGas + callbackGas, Math.max(maxDepth, cbDepth), opCount + cbOps).asRight[JsonLogicException]
                          }
                      }
                  }
              )

          case ArrayValue(arr) :: FunctionValue(expr) :: (init: JsonLogicPrimitive) :: Nil =>
            val baseCost = gasConfig.reduce + gasConfig.depthPenalty(depth.toLong)

            gasLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  arr.foldLeftM[F, Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]](
                    ((init: JsonLogicValue), baseCost, depth, 1).asRight[JsonLogicException]
                  ) {
                    case (accEither, item) =>
                      accEither match {
                        case Left(err) => err.asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
                        case Right((accValue, accGas, maxDepth, opCount)) =>
                          val ctx = MapValue(Map("current" -> item, "accumulator" -> accValue))
                          gasAwareEval(expr, ctx.some, remainingLimit, gasConfig, depth + 1).map {
                            case Left(err) => err.asLeft[(JsonLogicValue, GasCost, Int, Int)]
                            case Right((value, callbackGas, cbDepth, cbOps)) =>
                              (value, accGas + callbackGas, Math.max(maxDepth, cbDepth), opCount + cbOps).asRight[JsonLogicException]
                          }
                      }
                  }
              )

          case ArrayValue(arr) :: FunctionValue(_) :: Nil if arr.isEmpty =>
            JsonLogicException("Cannot reduce empty array without initial value")
              .asLeft[(JsonLogicValue, GasCost, Int, Int)]
              .pure[F]

          case _ =>
            JsonLogicException(s"Unexpected input to ${ReduceOp.tag}").asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
        }

      private def handleAllOpWithGas(
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        args match {
          case NullValue :: FunctionValue(_) :: Nil =>
            (BoolValue(false): JsonLogicValue, gasConfig.all + gasConfig.depthPenalty(depth.toLong), depth, 1)
              .asRight[JsonLogicException]
              .pure[F]

          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            val baseCost = gasConfig.all + gasConfig.depthPenalty(depth.toLong)

            gasLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  arr
                    .foldLeftM[F, Either[JsonLogicException, (Boolean, GasCost, Int, Int)]](
                      (true, baseCost, depth, 1).asRight[JsonLogicException]
                    ) {
                      case (accEither, element) =>
                        accEither match {
                          case Left(err) => err.asLeft[(Boolean, GasCost, Int, Int)].pure[F]
                          case Right((false, accGas, maxDepth, opCount)) =>
                            (false, accGas, maxDepth, opCount).asRight[JsonLogicException].pure[F] // Short-circuit
                          case Right((true, accGas, maxDepth, opCount)) =>
                            gasAwareEval(expr, element.some, remainingLimit, gasConfig, depth + 1).map {
                              case Left(err) => err.asLeft[(Boolean, GasCost, Int, Int)]
                              case Right((value, callbackGas, cbDepth, cbOps)) =>
                                (value.isTruthy, accGas + callbackGas, Math.max(maxDepth, cbDepth), opCount + cbOps)
                                  .asRight[JsonLogicException]
                            }
                        }
                    }
                    .map(_.map { case (result, totalGas, maxDepth, totalOps) => (BoolValue(result), totalGas, maxDepth, totalOps) })
              )

          case _ =>
            JsonLogicException(s"Unexpected input to ${AllOp.tag}, got $args")
              .asLeft[(JsonLogicValue, GasCost, Int, Int)]
              .pure[F]
        }

      private def handleNoneOpWithGas(
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            val baseCost = gasConfig.none + gasConfig.depthPenalty(depth.toLong)

            gasLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  arr
                    .foldLeftM[F, Either[JsonLogicException, (Boolean, GasCost, Int, Int)]](
                      (true, baseCost, depth, 1).asRight[JsonLogicException]
                    ) {
                      case (accEither, element) =>
                        accEither match {
                          case Left(err) => err.asLeft[(Boolean, GasCost, Int, Int)].pure[F]
                          case Right((false, accGas, maxDepth, opCount)) =>
                            (false, accGas, maxDepth, opCount).asRight[JsonLogicException].pure[F] // Short-circuit
                          case Right((true, accGas, maxDepth, opCount)) =>
                            gasAwareEval(expr, element.some, remainingLimit, gasConfig, depth + 1).map {
                              case Left(err) => err.asLeft[(Boolean, GasCost, Int, Int)]
                              case Right((value, callbackGas, cbDepth, cbOps)) =>
                                (!value.isTruthy, accGas + callbackGas, Math.max(maxDepth, cbDepth), opCount + cbOps)
                                  .asRight[JsonLogicException]
                            }
                        }
                    }
                    .map(_.map { case (result, totalGas, maxDepth, totalOps) => (BoolValue(result), totalGas, maxDepth, totalOps) })
              )

          case _ =>
            JsonLogicException(s"Unexpected input to ${NoneOp.tag}").asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
        }

      private def handleSomeOpWithGas(
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            handleSomeOpWithGasImpl(arr, expr, 1, gasLimit, gasConfig, depth)

          case ArrayValue(arr) :: FunctionValue(expr) :: IntValue(min) :: Nil =>
            handleSomeOpWithGasImpl(arr, expr, min.toInt, gasLimit, gasConfig, depth)

          case _ =>
            JsonLogicException(s"Unexpected input to ${SomeOp.tag}").asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
        }

      private def handleSomeOpWithGasImpl(
        arr: List[JsonLogicValue],
        expr: JsonLogicExpression,
        threshold: Int,
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] = {
        val baseCost = gasConfig.some + gasConfig.depthPenalty(depth.toLong)

        gasLimit
          .consume(baseCost)
          .fold(
            err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
            remainingLimit =>
              arr
                .foldLeftM[F, Either[JsonLogicException, (Int, GasCost, Int, Int)]](
                  (0, baseCost, depth, 1).asRight[JsonLogicException]
                ) {
                  case (accEither, element) =>
                    accEither match {
                      case Left(err) => err.asLeft[(Int, GasCost, Int, Int)].pure[F]
                      case Right((count, accGas, maxDepth, opCount)) if count >= threshold =>
                        (count, accGas, maxDepth, opCount).asRight[JsonLogicException].pure[F] // Short-circuit
                      case Right((count, accGas, maxDepth, opCount)) =>
                        gasAwareEval(expr, element.some, remainingLimit, gasConfig, depth + 1).map {
                          case Left(err) => err.asLeft[(Int, GasCost, Int, Int)]
                          case Right((value, callbackGas, cbDepth, cbOps)) =>
                            val newCount = if (value.isTruthy) count + 1 else count
                            (newCount, accGas + callbackGas, Math.max(maxDepth, cbDepth), opCount + cbOps).asRight[JsonLogicException]
                        }
                    }
                }
                .map(_.map { case (count, totalGas, maxDepth, totalOps) => (BoolValue(count >= threshold), totalGas, maxDepth, totalOps) })
          )
      }

      private def handleCountOpWithGas(
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            val baseCost = gasConfig.count + gasConfig.depthPenalty(depth.toLong)

            gasLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit =>
                  arr
                    .foldLeftM[F, Either[JsonLogicException, (Int, GasCost, Int, Int)]](
                      (0, baseCost, depth, 1).asRight[JsonLogicException]
                    ) {
                      case (accEither, element) =>
                        accEither match {
                          case Left(err) => err.asLeft[(Int, GasCost, Int, Int)].pure[F]
                          case Right((count, accGas, maxDepth, opCount)) =>
                            gasAwareEval(expr, element.some, remainingLimit, gasConfig, depth + 1).map {
                              case Left(err) => err.asLeft[(Int, GasCost, Int, Int)]
                              case Right((value, callbackGas, cbDepth, cbOps)) =>
                                val newCount = if (value.isTruthy) count + 1 else count
                                (newCount, accGas + callbackGas, Math.max(maxDepth, cbDepth), opCount + cbOps).asRight[JsonLogicException]
                            }
                        }
                    }
                    .map(_.map { case (count, totalGas, maxDepth, totalOps) => (IntValue(count), totalGas, maxDepth, totalOps) })
              )

          case _ =>
            JsonLogicException(s"Unexpected input to ${CountOp.tag}, got $args")
              .asLeft[(JsonLogicValue, GasCost, Int, Int)]
              .pure[F]
        }

      private def handleFindOpWithGas(
        args: List[JsonLogicValue],
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            val baseCost = gasConfig.find + gasConfig.depthPenalty(depth.toLong)

            gasLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
                remainingLimit => {
                  def searchLoop(
                    remaining: List[JsonLogicValue],
                    accGas: GasCost,
                    maxDepth: Int,
                    opCount: Int
                  ): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] =
                    remaining match {
                      case Nil =>
                        (NullValue: JsonLogicValue, accGas, maxDepth, opCount).asRight[JsonLogicException].pure[F]
                      case element :: tail =>
                        gasAwareEval(expr, element.some, remainingLimit, gasConfig, depth + 1).flatMap {
                          case Left(err) => err.asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F]
                          case Right((value, callbackGas, cbDepth, cbOps)) =>
                            val newGas = accGas + callbackGas
                            val newMaxDepth = Math.max(maxDepth, cbDepth)
                            val newOpCount = opCount + cbOps
                            if (value.isTruthy)
                              (element: JsonLogicValue, newGas, newMaxDepth, newOpCount).asRight[JsonLogicException].pure[F]
                            else
                              searchLoop(tail, newGas, newMaxDepth, newOpCount)
                        }
                    }

                  searchLoop(arr, baseCost, depth, 1)
                }
              )

          case _ =>
            JsonLogicException(s"Unexpected input to ${FindOp.tag}, got $args")
              .asLeft[(JsonLogicValue, GasCost, Int, Int)]
              .pure[F]
        }
    }
}