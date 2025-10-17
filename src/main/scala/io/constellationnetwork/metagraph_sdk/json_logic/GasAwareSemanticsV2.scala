package io.constellationnetwork.metagraph_sdk.json_logic

import cats.Monad
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.JsonLogicOp._

object GasAwareSemanticsV2 {

  def make[F[_]: Monad](
    vars: JsonLogicValue,
    gasLimit: GasLimit,
    gasConfig: GasConfig,
    evaluationStrategy: (JsonLogicExpression, Option[JsonLogicValue], Int) =>
      F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]]
  ): JsonLogicSemanticsV2[F, ResultContext.WithGas] = {

    def wrappedEval(expr: JsonLogicExpression, ctx: Option[JsonLogicValue]):
      F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
      evaluationStrategy(expr, ctx, 0)

    val baseSemantics = JsonLogicSemanticsV2.make[F, ResultContext.WithGas](vars, wrappedEval)

    new JsonLogicSemanticsV2[F, ResultContext.WithGas] {

      override def getVar(key: String, ctx: Option[JsonLogicValue] = None):
        F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
        baseSemantics.getVar(key, ctx).map(_.map {
          case (value, metrics) =>
            val varCost = gasConfig.varAccess + GasCost(key.split("\\.").length.toLong)
            (value, metrics.withCost(varCost))
        })

      override def applyOp(op: JsonLogicOp): List[JsonLogicValue] =>
        F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
        args => {
          val baseCost = getOpCost(op)(gasConfig)

          gasLimit.consume(baseCost).fold(
            err => (err: JsonLogicException).asLeft[ResultContext.WithGas[JsonLogicValue]].pure[F],
            _ =>
              baseSemantics.applyOp(op)(args).map {
                case Right((value, metrics)) =>
                  val additionalCost = getAdditionalCost(op, args, value)
                  val depthPenalty = gasConfig.depthPenalty(metrics.depth.toLong)
                  val totalCost = baseCost + depthPenalty + metrics.cost + additionalCost

                  if (gasLimit.canAfford(totalCost)) {
                    (value, metrics.withCost(baseCost + depthPenalty + additionalCost)).asRight[JsonLogicException]
                  } else {
                    (GasExhaustedException(
                      required = totalCost,
                      available = gasLimit
                    ): JsonLogicException).asLeft[ResultContext.WithGas[JsonLogicValue]]
                  }
                case Left(err) => err.asLeft[ResultContext.WithGas[JsonLogicValue]]
              }
          )
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
          case CatOp => result match {
            case StrValue(s) => GasCost(s.length.toLong)
            case _ => GasCost.Zero
          }
          case SplitOp => result match {
            case ArrayValue(arr) => GasCost(arr.size.toLong * 2)
            case _ => GasCost.Zero
          }
          case MergeOp => result match {
            case ArrayValue(arr) => GasCost(arr.size.toLong)
            case MapValue(m) => GasCost(m.size.toLong)
            case _ => GasCost.Zero
          }
          case UniqueOp => args match {
            case ArrayValue(arr) :: Nil => GasCost((arr.size.toLong * arr.size.toLong) / 10)
            case _ => GasCost.Zero
          }
          case PowOp => args match {
            case _ :: IntValue(exp) :: Nil => GasCost(exp.abs.toLong.min(1000))
            case _ :: FloatValue(exp) :: Nil => GasCost(exp.abs.toLong.min(1000))
            case _ => GasCost.Zero
          }
          case AddOp | TimesOp | MinusOp => args match {
            case ArrayValue(arr) :: Nil => GasCost(arr.size.toLong)
            case list if list.size > 1 => GasCost((list.size - 1).toLong)
            case _ => GasCost.Zero
          }
          case _ => GasCost.Zero
        }
    }
  }
}