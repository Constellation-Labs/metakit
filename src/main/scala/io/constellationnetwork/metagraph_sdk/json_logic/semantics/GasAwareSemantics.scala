package io.constellationnetwork.metagraph_sdk.json_logic.semantics

import cats.effect.{Ref, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicOp._
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasConfig, GasCost, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.ResultContext

object GasAwareSemantics {

  def make[F[_]: Sync](
    vars: JsonLogicValue,
    gasLimit: GasLimit,
    gasConfig: GasConfig,
    evaluationStrategy: (
      JsonLogicExpression,
      Option[JsonLogicValue],
      Int
    ) => F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]],
    currentDepth: Int = 0
  ): F[JsonLogicSemantics[F, ResultContext.WithGas]] =
    Ref.of[F, GasLimit](gasLimit).map { gasLimitRef =>
      makeWithRef(vars, gasLimitRef, gasConfig, evaluationStrategy, currentDepth)
    }

  def makeWithRef[F[_]: Sync](
    vars: JsonLogicValue,
    gasLimitRef: Ref[F, GasLimit],
    gasConfig: GasConfig,
    evaluationStrategy: (
      JsonLogicExpression,
      Option[JsonLogicValue],
      Int
    ) => F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]],
    currentDepth: Int = 0
  ): JsonLogicSemantics[F, ResultContext.WithGas] = {

    def wrappedEval(
      expr: JsonLogicExpression,
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
      evaluationStrategy(expr, ctx, currentDepth + 1)

    val baseSemantics = JsonLogicSemantics.make[F, ResultContext.WithGas](vars, wrappedEval)

    new JsonLogicSemantics[F, ResultContext.WithGas] {

      override def getVar(
        key: String,
        ctx: Option[JsonLogicValue] = None
      ): F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
        baseSemantics
          .getVar(key, ctx)
          .map(_.map {
            case (value, metrics) =>
              val varCost = gasConfig.varAccess + GasCost(key.split("\\.").length.toLong)
              (value, metrics.withCost(varCost))
          })

      override def applyOp(
        op: JsonLogicOp
      ): List[ResultContext.WithGas[JsonLogicValue]] => F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
        args => {
          val baseCost = getOpCost(op)(gasConfig)
          val argValues = args.map { case (value, _) => value }

          gasLimitRef.get.flatMap { currentLimit =>
            currentLimit
              .consume(baseCost)
              .fold(
                err => (err: JsonLogicException).asLeft[ResultContext.WithGas[JsonLogicValue]].pure[F],
                newLimit =>
                  gasLimitRef.set(newLimit) >>
                  baseSemantics.applyOp(op)(args).flatMap {
                    case Right((value, metrics)) =>
                      val additionalCost = getAdditionalCost(op, argValues, value)
                      val argMaxDepth = if (args.isEmpty) 0 else args.map(_._2.depth).max
                      val newDepth = argMaxDepth + 1
                      val depthPenalty = gasConfig.depthPenalty(newDepth.toLong)

                      gasLimitRef.get.flatMap { limit =>
                        limit
                          .consume(metrics.cost + depthPenalty + additionalCost)
                          .fold(
                            err => (err: JsonLogicException).asLeft[ResultContext.WithGas[JsonLogicValue]].pure[F],
                            finalLimit =>
                              gasLimitRef
                                .set(finalLimit)
                                .as(
                                  (value, metrics.withCost(baseCost + depthPenalty + additionalCost).withDepth(newDepth))
                                    .asRight[JsonLogicException]
                                )
                          )
                      }
                    case Left(err) => err.asLeft[ResultContext.WithGas[JsonLogicValue]].pure[F]
                  }
              )
          }
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

      private def getAdditionalCost(op: JsonLogicOp, args: List[JsonLogicValue], result: JsonLogicValue): GasCost =
        op match {
          case CatOp =>
            result match {
              case StrValue(s) => gasConfig.sizeCost(s.length.toLong)
              case _           => GasCost.Zero
            }
          case SplitOp =>
            result match {
              case ArrayValue(arr) => gasConfig.sizeCost(arr.size.toLong * 2)
              case _               => GasCost.Zero
            }
          case MergeOp =>
            result match {
              case ArrayValue(arr) => gasConfig.sizeCost(arr.size.toLong)
              case MapValue(m)     => gasConfig.sizeCost(m.size.toLong)
              case _               => GasCost.Zero
            }
          case UniqueOp =>
            args match {
              case ArrayValue(arr) :: Nil => gasConfig.sizeCost(arr.size.toLong)
              case _                      => GasCost.Zero
            }
          case PowOp =>
            args match {
              case _ :: IntValue(exp) :: Nil   => GasCost(exp.abs.toLong)
              case _ :: FloatValue(exp) :: Nil => GasCost(exp.abs.toLong)
              case _                           => GasCost.Zero
            }
          case AddOp | TimesOp | MinusOp =>
            args match {
              case ArrayValue(arr) :: Nil => gasConfig.sizeCost(arr.size.toLong)
              case list if list.size > 1  => gasConfig.sizeCost((list.size - 1).toLong)
              case _                      => GasCost.Zero
            }
          case MapOp | FilterOp | AllOp | NoneOp | SomeOp | FindOp | CountOp =>
            args match {
              case ArrayValue(arr) :: _ => gasConfig.sizeCost(arr.size.toLong)
              case _                    => GasCost.Zero
            }
          case ReverseOp =>
            args match {
              case ArrayValue(arr) :: Nil => gasConfig.sizeCost(arr.size.toLong)
              case _                      => GasCost.Zero
            }
          case FlattenOp =>
            result match {
              case ArrayValue(arr) => gasConfig.sizeCost(arr.size.toLong)
              case _               => GasCost.Zero
            }
          case SliceOp =>
            result match {
              case ArrayValue(arr) => gasConfig.sizeCost(arr.size.toLong)
              case _               => GasCost.Zero
            }
          case InOp =>
            args match {
              case _ :: ArrayValue(arr) :: Nil => gasConfig.sizeCost(arr.size.toLong)
              case _ :: StrValue(s) :: Nil     => gasConfig.sizeCost(s.length.toLong / 10)
              case _                           => GasCost.Zero
            }
          case IntersectOp =>
            args match {
              case ArrayValue(a) :: ArrayValue(b) :: Nil => gasConfig.sizeCost(a.size.toLong + b.size.toLong)
              case _                                     => GasCost.Zero
            }
          case ReduceOp =>
            args match {
              case ArrayValue(arr) :: _ => gasConfig.sizeCost(arr.size.toLong)
              case _                    => GasCost.Zero
            }
          case MaxOp | MinOp =>
            args match {
              case ArrayValue(arr) :: Nil => gasConfig.sizeCost(arr.size.toLong)
              case list                   => gasConfig.sizeCost(list.size.toLong)
            }
          case JoinOp =>
            result match {
              case StrValue(s) => gasConfig.sizeCost(s.length.toLong)
              case _           => GasCost.Zero
            }
          case SubStrOp =>
            result match {
              case StrValue(s) => gasConfig.sizeCost(s.length.toLong)
              case _           => GasCost.Zero
            }
          case MapValuesOp | MapKeysOp =>
            args match {
              case MapValue(m) :: Nil => gasConfig.sizeCost(m.size.toLong)
              case _                  => GasCost.Zero
            }
          case EntriesOp =>
            result match {
              case ArrayValue(arr) => gasConfig.sizeCost(arr.size.toLong * 2) // Each entry creates a 2-element array
              case _               => GasCost.Zero
            }
          case _ => GasCost.Zero
        }
    }
  }
}
