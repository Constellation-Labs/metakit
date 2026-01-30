package io.constellationnetwork.metagraph_sdk.json_logic.semantics

import cats.Monad
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicOp._
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.CoercionOps._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.NumericOps._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.ResultContext._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.{JsonLogicRuntime, ResultContext}

trait JsonLogicSemantics[F[_], Result[_]] {
  def getVar(key: String, ctx: Option[JsonLogicValue] = None): F[Either[JsonLogicException, Result[JsonLogicValue]]]
  def applyOp(op: JsonLogicOp): List[Result[JsonLogicValue]] => F[Either[JsonLogicException, Result[JsonLogicValue]]]
}

object JsonLogicSemantics {

  type EvaluationCallback[F[_], Result[_]] =
    (JsonLogicExpression, Option[JsonLogicValue]) => F[Either[JsonLogicException, Result[JsonLogicValue]]]

  def apply[F[_], Result[_]](implicit ev: JsonLogicSemantics[F, Result]): JsonLogicSemantics[F, Result] = ev

  def make[F[_]: Monad, Result[_]: ResultContext](
    vars: JsonLogicValue,
    evaluationStrategy: EvaluationCallback[F, Result]
  ): JsonLogicSemantics[F, Result] =
    new JsonLogicSemantics[F, Result] {

      override def getVar(
        key: String,
        ctx: Option[JsonLogicValue] = None
      ): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def combineState(
          base: JsonLogicValue,
          extOpt: Option[JsonLogicValue]
        ): Either[JsonLogicException, JsonLogicValue] = (base, extOpt) match {
          case (v, None)                            => v.asRight[JsonLogicException]
          case (_, Some(NullValue))                 => base.asRight[JsonLogicException]
          case (_, Some(_: JsonLogicPrimitive))     => base.asRight[JsonLogicException]
          case (ArrayValue(l), Some(ArrayValue(r))) => ArrayValue(l ++ r).asRight
          case (MapValue(l), Some(MapValue(r)))     => MapValue(l ++ r).asRight
          case (_, Some(ctx))                       => ctx.asRight[JsonLogicException]
        }

        def getChild(
          parent: JsonLogicValue,
          segment: String
        ): Either[JsonLogicException, JsonLogicValue] = parent match {
          case ArrayValue(elements) =>
            segment.toLongOption match {
              case Some(idx) if idx >= 0 && idx < elements.length =>
                elements(idx.toInt).asRight
              case _ =>
                NullValue.asRight
            }

          case MapValue(m) =>
            m.get(segment) match {
              case Some(child) => child.asRight[JsonLogicException]
              case None        => NullValue.asRight
            }

          case _ =>
            NullValue.asRight
        }

        if (key.isEmpty) ctx.getOrElse(vars).pure[Result].asRight[JsonLogicException].pure[F]
        else if (key.endsWith(".")) (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
        else {
          val segments = key.split("\\.").toList
          (for {
            combined <- combineState(vars, ctx)
            finalVal <- segments.foldLeft(combined.asRight[JsonLogicException]) { (acc, seg) =>
              acc.flatMap(getChild(_, seg))
            }
          } yield finalVal.pure[Result]).pure[F]
        }
      }

      override def applyOp(op: JsonLogicOp): List[Result[JsonLogicValue]] => F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        op match {
          case NoOp          => _ => JsonLogicException("Got unexpected NoOp!").asLeft[Result[JsonLogicValue]].pure[F]
          case MissingNoneOp => handleMissingNone
          case ExistsOp      => handleExists
          case MissingSomeOp => handleMissingSome
          case IfElseOp      => handleIfElseOp
          case EqOp          => handleEqOp
          case EqStrictOp    => handleEqStrictOp
          case NEqOp         => handleNEqOp
          case NEqStrictOp   => handleNEqStrictOp
          case NotOp         => handleNotOp
          case NOp           => handleNOp
          case OrOp          => handleOrOp
          case AndOp         => handleAndOp
          case Lt            => handleLt
          case Leq           => handleLeq
          case Gt            => handleGt
          case Geq           => handleGeq
          case ModuloOp      => handleModuloOp
          case MaxOp         => handleMaxOp
          case MinOp         => handleMinOp
          case AddOp         => handleAddOp
          case TimesOp       => handleTimesOp
          case MinusOp       => handleMinusOp
          case DivOp         => handleDivOp
          case MergeOp       => handleMergeOp
          case InOp          => handleInOp
          case CatOp         => handleCatOp
          case SubStrOp      => handleSubstrOp
          case MapOp         => handleMapOp
          case FilterOp      => handleFilterOp
          case ReduceOp      => handleReduceOp
          case AllOp         => handleAllOp
          case NoneOp        => handleNoneOp
          case SomeOp        => handleSomeOp
          case MapValuesOp   => handleMapValuesOp
          case MapKeysOp     => handleMapKeysOp
          case GetOp         => handleGetOp
          case IntersectOp   => handleIntersectOp
          case CountOp       => handleCountOp
          case LengthOp      => handleLengthOp
          case FindOp        => handleFindOp
          case LowerOp       => handleLowerOp
          case UpperOp       => handleUpperOp
          case JoinOp        => handleJoinOp
          case SplitOp       => handleSplitOp
          case DefaultOp     => handleDefaultOp
          case UniqueOp      => handleUniqueOp
          case SliceOp       => handleSliceOp
          case ReverseOp     => handleReverseOp
          case FlattenOp     => handleFlattenOp
          case TrimOp        => handleTrimOp
          case StartsWithOp  => handleStartsWithOp
          case EndsWithOp    => handleEndsWithOp
          case AbsOp         => handleAbsOp
          case RoundOp       => handleRoundOp
          case FloorOp       => handleFloorOp
          case CeilOp        => handleCeilOp
          case PowOp         => handlePowOp
          case HasOp         => handleHasOp
          case EntriesOp     => handleEntriesOp
          case TypeOfOp      => handleTypeOfOp
        }

      private def isFieldMissing(field: JsonLogicValue): F[Option[JsonLogicValue]] = field match {
        case v @ StrValue(key) =>
          getVar(key).map {
            case Right(result) =>
              if (result.extractValue == NullValue) v.some else None
            case Left(_) => v.some
          }
        case v @ IntValue(key) =>
          getVar(key.toString).map {
            case Right(result) =>
              if (result.extractValue == NullValue) v.some else None
            case Left(_) => v.some
          }
        case v @ FloatValue(key) =>
          getVar(key.toString).map {
            case Right(result) =>
              if (result.extractValue == NullValue) v.some else None
            case Left(_) => v.some
          }
        case v => v.some.pure[F]
      }

      private def handleMissingNone(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        val combined = ResultContext[Result].sequence(args)
        val values = ResultContext[Result].extract(combined)

        def impl(list: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
          list
            .traverseFilter(isFieldMissing)
            .map(l => (ArrayValue(l): JsonLogicValue).asRight[JsonLogicException])

        (values match {
          case ArrayValue(arr) :: Nil => impl(arr)
          case _                      => impl(values)
        }).map(_.map(v => ResultContext[Result].flatMap(combined)(_ => v.pure[Result])))
      }

      private def handleExists(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          val result = values match {
            case ArrayValue(arr) :: Nil => BoolValue(!arr.contains(NullValue))
            case _                      => BoolValue(!values.contains(NullValue))
          }
          (result: JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
        }

      private def handleMissingSome(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        val combined = ResultContext[Result].sequence(args)
        val values = ResultContext[Result].extract(combined)

        def impl(list: List[JsonLogicValue], minRequired: Int): F[Either[JsonLogicException, JsonLogicValue]] =
          list.traverseFilter(isFieldMissing).map { missingFields =>
            val presentCount = list.length - missingFields.length

            if (presentCount >= minRequired) (ArrayValue(Nil): JsonLogicValue).asRight[JsonLogicException]
            else (ArrayValue(missingFields): JsonLogicValue).asRight[JsonLogicException]
          }

        (values match {
          case ArrayValue(arr) :: Nil => impl(arr, 1)
          case IntValue(min) :: ArrayValue(arr) :: Nil if min > 0 =>
            safeToInt(min, "missing_some min").fold(
              err => err.asLeft[JsonLogicValue].pure[F],
              minInt => impl(arr, minInt)
            )
          case _ =>
            JsonLogicException(s"Unexpected input for `${MissingSomeOp.tag}' got $values")
              .asLeft[JsonLogicValue]
              .pure[F]
        }).map(_.map(v => ResultContext[Result].flatMap(combined)(_ => v.pure[Result])))
      }

      private def handleIfElseOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        val combined = ResultContext[Result].sequence(args)
        val values = ResultContext[Result].extract(combined)

        if (values.length < 3 || values.length % 2 == 0) {
          JsonLogicException(s"Unexpected input to `${IfElseOp.tag}` got $values")
            .asLeft[Result[JsonLogicValue]]
            .pure[F]
        } else {
          val selectedBranch = values
            .grouped(2)
            .collectFirst { case List(cond, FunctionValue(branchExpr)) if cond.isTruthy => branchExpr }
            .orElse(values.lastOption.collect { case FunctionValue(elseExpr) => elseExpr })

          selectedBranch match {
            case Some(branchExpr) =>
              evaluationStrategy(branchExpr, None).map(_.map(branchResult => ResultContext[Result].flatMap(combined)(_ => branchResult)))
            case None => JsonLogicException("failed during if/else evaluation").asLeft[Result[JsonLogicValue]].pure[F]
          }
        }
      }

      private def handleEqOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(left: JsonLogicValue, right: JsonLogicValue): Either[JsonLogicException, Result[JsonLogicValue]] = for {
          lc   <- coerceToPrimitive(left)
          rc   <- coerceToPrimitive(right)
          test <- compareCoercedValues(lc, rc)
        } yield (BoolValue(test): JsonLogicValue).pure[Result]

        args.withMetrics { values =>
          values match {
            case l :: r :: Nil => impl(l, r)
            case _             => JsonLogicException(s"Unexpected input for `${EqOp.tag}` got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      // Strict equality compares types and values directly.
      // Arrays and maps use structural value equality (deep comparison).
      private def handleEqStrictOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          val boolResult = values match {
            case NullValue :: NullValue :: Nil         => true
            case BoolValue(l) :: BoolValue(r) :: Nil   => l == r
            case StrValue(l) :: StrValue(r) :: Nil     => l == r
            case IntValue(l) :: IntValue(r) :: Nil     => l == r
            case FloatValue(l) :: FloatValue(r) :: Nil => l == r
            case ArrayValue(l) :: ArrayValue(r) :: Nil => l == r
            case MapValue(l) :: MapValue(r) :: Nil     => l == r
            case _                                     => false
          }
          (BoolValue(boolResult): JsonLogicValue).pure[Result].asRight[JsonLogicException]
        }

      private def handleNEqOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(left: JsonLogicValue, right: JsonLogicValue): Either[JsonLogicException, JsonLogicValue] = for {
          lc   <- coerceToPrimitive(left)
          rc   <- coerceToPrimitive(right)
          test <- compareCoercedValues(lc, rc)
        } yield BoolValue(!test): JsonLogicValue

        args.withMetrics { values =>
          values match {
            case l :: r :: Nil => impl(l, r).map(_.pure[Result])
            case _             => JsonLogicException(s"Unexpected input for `${NEqOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleNEqStrictOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          val boolResult = values match {
            case BoolValue(l) :: BoolValue(r) :: Nil   => l != r
            case StrValue(l) :: StrValue(r) :: Nil     => l != r
            case IntValue(l) :: IntValue(r) :: Nil     => l != r
            case FloatValue(l) :: FloatValue(r) :: Nil => l != r
            case ArrayValue(l) :: ArrayValue(r) :: Nil => l != r
            case MapValue(l) :: MapValue(r) :: Nil     => l != r
            case _                                     => false
          }
          (BoolValue(boolResult): JsonLogicValue).pure[Result].asRight[JsonLogicException]
        }

      private def handleNotOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case v :: Nil => (BoolValue(!v.isTruthy): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _        => JsonLogicException(s"Unexpected input for `${NOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleNOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case v :: Nil => (BoolValue(v.isTruthy): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _        => JsonLogicException(s"Unexpected input for `${NOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleOrOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          val result = if (values.isEmpty) {
            BoolValue(false): JsonLogicValue
          } else {
            values.collectFirst { case value if value.isTruthy => value }.getOrElse(values.last)
          }
          result.pure[Result].asRight[JsonLogicException]
        }

      private def handleAndOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          val result = values.foldLeft(BoolValue(true): JsonLogicValue) {
            case (acc, el) =>
              if (!acc.isTruthy) acc else if (!el.isTruthy) el else el
          }
          result.pure[Result].asRight[JsonLogicException]
        }

      private def handleLt(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def compareTwo(l: JsonLogicValue, r: JsonLogicValue): Either[JsonLogicException, Boolean] =
          for {
            ln <- promoteToNumeric(l)
            rn <- promoteToNumeric(r)
          } yield compareNumeric(ln, rn) < 0

        def compareThree(a: JsonLogicValue, b: JsonLogicValue, c: JsonLogicValue): Either[JsonLogicException, Boolean] =
          for {
            an <- promoteToNumeric(a)
            bn <- promoteToNumeric(b)
            cn <- promoteToNumeric(c)
          } yield compareNumeric(an, bn) < 0 && compareNumeric(bn, cn) < 0

        args.withMetrics { values =>
          values match {
            case l :: r :: Nil      => compareTwo(l, r).map(b => (BoolValue(b): JsonLogicValue).pure[Result])
            case a :: b :: c :: Nil => compareThree(a, b, c).map(b => (BoolValue(b): JsonLogicValue).pure[Result])
            case _                  => JsonLogicException(s"Unexpected input for `${Lt.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleLeq(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        def compareTwo(l: JsonLogicValue, r: JsonLogicValue): Either[JsonLogicException, Boolean] =
          for {
            ln <- promoteToNumeric(l)
            rn <- promoteToNumeric(r)
          } yield compareNumeric(ln, rn) <= 0

        def compareThree(a: JsonLogicValue, b: JsonLogicValue, c: JsonLogicValue): Either[JsonLogicException, Boolean] =
          for {
            an <- promoteToNumeric(a)
            bn <- promoteToNumeric(b)
            cn <- promoteToNumeric(c)
          } yield compareNumeric(an, bn) <= 0 && compareNumeric(bn, cn) <= 0

        args.withMetrics { values =>
          values match {
            case l :: r :: Nil      => compareTwo(l, r).map(b => (BoolValue(b): JsonLogicValue).pure[Result])
            case a :: b :: c :: Nil => compareThree(a, b, c).map(b => (BoolValue(b): JsonLogicValue).pure[Result])
            case _                  => JsonLogicException(s"Unexpected input for `${Leq.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleGt(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        def compareTwo(l: JsonLogicValue, r: JsonLogicValue): Either[JsonLogicException, Boolean] =
          for {
            ln <- promoteToNumeric(l)
            rn <- promoteToNumeric(r)
          } yield compareNumeric(ln, rn) > 0

        args.withMetrics { values =>
          values match {
            case l :: r :: Nil => compareTwo(l, r).map(b => (BoolValue(b): JsonLogicValue).pure[Result])
            case _             => JsonLogicException(s"Unexpected input for `${Gt.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleGeq(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        def compareTwo(l: JsonLogicValue, r: JsonLogicValue): Either[JsonLogicException, Boolean] =
          for {
            ln <- promoteToNumeric(l)
            rn <- promoteToNumeric(r)
          } yield compareNumeric(ln, rn) >= 0

        args.withMetrics { values =>
          values match {
            case l :: r :: Nil => compareTwo(l, r).map(b => (BoolValue(b): JsonLogicValue).pure[Result])
            case _             => JsonLogicException(s"Unexpected input for `${Geq.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleModuloOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case l :: r :: Nil =>
              (for {
                ln <- promoteToNumeric(l)
                rn <- promoteToNumeric(r)
              } yield
                if (rn.toBigDecimal == 0) {
                  JsonLogicException("Division by zero in modulo operation").asLeft[Result[JsonLogicValue]]
                } else {
                  // Note: BigDecimal's % uses truncated division (same as JavaScript/Java)
                  // e.g., -7 % 3 = -1 (not 2 as in Python's floored division)
                  combineNumeric(_ % _)(ln, rn).pure[Result].asRight[JsonLogicException]
                }).fold(_.asLeft[Result[JsonLogicValue]], identity)
            case _ =>
              JsonLogicException(s"Unexpected input for `${ModuloOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleMaxOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, Result[JsonLogicValue]] =
          if (list.isEmpty) {
            JsonLogicException(s"Unexpected input for `${MaxOp.tag}`: list cannot be empty").asLeft
          } else {
            list.traverse(promoteToNumeric).map { numerics =>
              val maxValue = numerics.map(_.toBigDecimal).max
              val hasFloat = numerics.exists(_.isInstanceOf[FloatResult])

              val result: JsonLogicValue = if (!hasFloat && maxValue.isWhole && maxValue.isValidLong) {
                IntValue(maxValue.toBigInt)
              } else {
                FloatValue(maxValue)
              }
              result.pure[Result]
            }
          }

        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: Nil => impl(arr)
            case _                      => impl(values)
          }
        }
      }

      private def handleMinOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, Result[JsonLogicValue]] =
          if (list.isEmpty) {
            JsonLogicException(s"Unexpected input for `${MinOp.tag}`: list cannot be empty").asLeft
          } else {
            list.traverse(promoteToNumeric).map { numerics =>
              val minValue = numerics.map(_.toBigDecimal).min
              val hasFloat = numerics.exists(_.isInstanceOf[FloatResult])

              val result: JsonLogicValue = if (!hasFloat && minValue.isWhole && minValue.isValidLong) {
                IntValue(minValue.toBigInt)
              } else {
                FloatValue(minValue)
              }
              result.pure[Result]
            }
          }

        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: Nil => impl(arr)
            case _                      => impl(values)
          }
        }
      }

      private def handleAddOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          if (list.isEmpty) {
            JsonLogicException(s"Unexpected input for `${AddOp.tag}`: list cannot be empty").asLeft
          } else if (list.size == 1 && list.head.isInstanceOf[StrValue]) {
            promoteToNumeric(list.head).map(_.toJsonLogicValue)
          } else {
            reduceNumeric(list, _ + _).map(v => v: JsonLogicValue)
          }

        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: Nil => impl(arr).map(_.pure[Result])
            case _                      => impl(values).map(_.pure[Result])
          }
        }
      }

      private def handleTimesOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, Result[JsonLogicValue]] =
          if (list.isEmpty) JsonLogicException(s"Unexpected input for `${TimesOp.tag}`: list cannot be empty").asLeft
          else reduceNumeric(list, _ * _).map(v => (v: JsonLogicValue).pure[Result])

        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: Nil => impl(arr)
            case _                      => impl(values)
          }
        }
      }

      private def handleMinusOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case v :: Nil =>
              promoteToNumeric(v).map { n =>
                combineNumeric((a, _) => -a)(n, IntResult(0)).pure[Result]
              }
            case l :: r :: Nil =>
              for {
                ln <- promoteToNumeric(l)
                rn <- promoteToNumeric(r)
              } yield combineNumeric(_ - _)(ln, rn).pure[Result]
            case _ =>
              JsonLogicException(s"Unexpected input for `${MinusOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleDivOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case l :: r :: Nil =>
              (for {
                ln <- promoteToNumeric(l)
                rn <- promoteToNumeric(r)
              } yield
                if (rn.toBigDecimal == 0) {
                  JsonLogicException("Division by zero").asLeft[Result[JsonLogicValue]]
                } else {
                  // Use safeDivide for explicit DECIMAL128 precision
                  combineNumeric(safeDivide)(ln, rn).pure[Result].asRight[JsonLogicException]
                }).fold(_.asLeft[Result[JsonLogicValue]], identity)
            case _ =>
              JsonLogicException(s"Unexpected input for `${DivOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleMergeOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        def impl(arr: List[JsonLogicValue]): Either[JsonLogicException, Result[JsonLogicValue]] = {
          val flattened = arr.foldLeft(List.empty[JsonLogicValue]) {
            case (acc, ArrayValue(elems)) => acc ++ elems
            case (acc, elem)              => acc :+ elem
          }
          (ArrayValue(flattened): JsonLogicValue).pure[Result].asRight
        }

        args.withMetrics { values =>
          values match {
            case maps if maps.forall(_.isInstanceOf[MapValue]) =>
              values
                .pure[F]
                .map(_.collect { case MapValue(m) => m }.foldLeft(Map.empty[String, JsonLogicValue])(_ ++ _))
                .map(m => (MapValue(m): JsonLogicValue).pure[Result].asRight[JsonLogicException])
            case ArrayValue(arr) :: Nil => impl(arr).pure[F]
            case other                  => impl(other).pure[F]
          }
        }
      }

      private def handleInOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        // For string containment, primitives are converted to their string representation
        def strImpl(toFind: JsonLogicPrimitive, str: String): Either[JsonLogicException, Result[JsonLogicValue]] = {
          val toFindStr = toFind match {
            case BoolValue(value)  => value.toString
            case IntValue(value)   => value.toString
            case FloatValue(value) => value.toString
            case StrValue(value)   => value
          }

          (BoolValue(str.contains(toFindStr)): JsonLogicValue).pure[Result].asRight[JsonLogicException]
        }

        def arrImpl(toFind: JsonLogicValue, arr: List[JsonLogicValue]): Either[JsonLogicException, Result[JsonLogicValue]] =
          (BoolValue(arr.contains(toFind)): JsonLogicValue).pure[Result].asRight[JsonLogicException]

        args.withMetrics { values =>
          values match {
            case NullValue :: _ :: Nil => (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
            case (toFind: JsonLogicPrimitive) :: StrValue(str) :: Nil => strImpl(toFind, str).pure[F]
            case (toFind: JsonLogicValue) :: ArrayValue(arr) :: Nil   => arrImpl(toFind, arr).pure[F]
            case _ => JsonLogicException(s"Unexpected input to `${InOp.tag}` got $values").asLeft[Result[JsonLogicValue]].pure[F]
          }
        }
      }

      private def handleIntersectOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def arrImpl(
          toFind: List[JsonLogicValue],
          arr: List[JsonLogicValue]
        ): Either[JsonLogicException, Result[JsonLogicValue]] =
          (BoolValue(toFind.forall(arr.toSet.contains)): JsonLogicValue).pure[Result].asRight[JsonLogicException]

        args.withMetrics { values =>
          values match {
            // null as first arg treated as empty set - empty set is subset of any set
            case NullValue :: _ :: Nil =>
              (BoolValue(true): JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
            // null as second arg - elements cannot be in null
            case ArrayValue(_) :: NullValue :: Nil =>
              (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
            case ArrayValue(toFind) :: ArrayValue(arr) :: Nil => arrImpl(toFind, arr).pure[F]
            case _ =>
              JsonLogicException(s"Unexpected input to `${IntersectOp.tag}`: expected two arrays, got $values")
                .asLeft[Result[JsonLogicValue]]
                .pure[F]
          }
        }
      }

      private def handleCatOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics {
          _.traverse {
            case NullValue => "".asRight
            case FunctionValue(expr) =>
              JsonLogicException(s"Unexpected input for `${CatOp.tag}` got $expr").asLeft[JsonLogicValue]
            case coll: JsonLogicCollection =>
              JsonLogicException(s"Unexpected input for `${CatOp.tag}` got $coll").asLeft[JsonLogicValue]
            case BoolValue(value)  => value.toString.asRight[JsonLogicException]
            case IntValue(value)   => value.toString.asRight[JsonLogicException]
            case FloatValue(value) => value.toString.asRight[JsonLogicException]
            case StrValue(value)   => value.asRight[JsonLogicException]
          }
            .map(argStrings => (StrValue(argStrings.mkString): JsonLogicValue).pure[Result])
            .pure[F]
        }

      private def handleSubstrOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(str: String, start: Int, length: Int): Either[JsonLogicException, Result[JsonLogicValue]] =
          for {
            s <- Option(str).toRight(JsonLogicException("substr expects a non-null string"))
            strLen = s.length
            rawStart = if (start < 0) strLen + start else start
            startIdx = Math.max(0, Math.min(rawStart, strLen))
            endIdx = if (length >= 0) Math.min(startIdx + length, strLen) else Math.max(0, strLen + length)
            substr = if (startIdx >= strLen || endIdx <= startIdx) "" else s.substring(startIdx, endIdx)
          } yield (StrValue(substr): JsonLogicValue).pure[Result]

        args.withMetrics {
          case StrValue(str) :: IntValue(start) :: Nil =>
            safeToInt(start, "substr start").flatMap(s => impl(str, s, str.length))
          case StrValue(str) :: IntValue(start) :: IntValue(length) :: Nil =>
            for {
              s <- safeToInt(start, "substr start")
              l <- safeToInt(length, "substr length")
              r <- impl(str, s, l)
            } yield r
          case _ => JsonLogicException(s"Unexpected input to `${SubStrOp.tag}` got $values").asLeft[Result[JsonLogicValue]]
        }
      }

      private def handleMapOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(arr: List[JsonLogicValue], expr: JsonLogicExpression): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr
            .traverse(el => evaluationStrategy(expr, el.some))
            .map(_.sequence.map(_.sequence.map(ArrayValue(_))))

        args.extractValues match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${MapOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleFilterOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some).map {
              case Right(result) =>
                (el, result.extractValue.isTruthy).asRight[JsonLogicException]
              case Left(err) => err.asLeft[(JsonLogicValue, Boolean)]
            }
          }.map {
            _.sequence.map { pairs =>
              val filtered = pairs.collect { case (el, isTruthy) if isTruthy => el }
              (ArrayValue(filtered): JsonLogicValue).pure[Result]
            }
          }

        args.extractValues match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${FilterOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleReduceOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression,
          init: JsonLogicValue
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.foldLeftM[F, Either[JsonLogicException, Result[JsonLogicValue]]](init.pure[Result].asRight) { (accEither, item) =>
            accEither match {
              case Left(err) => err.asLeft[Result[JsonLogicValue]].pure[F]
              case Right(accResult) =>
                evaluationStrategy(expr, MapValue(Map("current" -> item, "accumulator" -> accResult.extractValue)).some).map {
                  case Right(newResult) =>
                    val RC = ResultContext[Result]
                    val combined = RC.flatMap(accResult)(_ => newResult)
                    combined.asRight[JsonLogicException]
                  case Left(err) => err.asLeft[Result[JsonLogicValue]]
                }
            }
          }

        args.extractValues match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            if (arr.isEmpty) {
              (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
            } else {
              impl(arr.tail, expr, arr.head)
            }
          case ArrayValue(arr) :: FunctionValue(expr) :: (init: JsonLogicPrimitive) :: Nil => impl(arr, expr, init)
          case _ => JsonLogicException(s"Unexpected input to ${ReduceOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleAllOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some).map {
              case Right(result) => result.map(_.isTruthy).asRight[JsonLogicException]
              case Left(err)     => err.asLeft[Result[Boolean]]
            }
          }.map(_.sequence.map(_.sequence.map(_.forall(identity)).map(BoolValue(_)): Result[JsonLogicValue]))

        args.withMetrics {
          case NullValue :: FunctionValue(_) :: Nil => (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${AllOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleNoneOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some).map {
              case Right(result) => result.map(v => !v.isTruthy).asRight[JsonLogicException]
              case Left(err)     => err.asLeft[Result[Boolean]]
            }
          }.map {
            _.sequence.map {
              _.sequence.map(_.forall(identity)).map(BoolValue(_)): Result[JsonLogicValue]
            }
          }

        args.extractValues match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${NoneOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleSomeOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression,
          threshold: Int
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some).map {
              case Right(result) =>
                result.extractValue.isTruthy.asRight[JsonLogicException]
              case Left(err) => err.asLeft[Boolean]
            }
          }.map {
            _.sequence.map { bools =>
              (BoolValue(bools.count(identity) >= threshold): JsonLogicValue).pure[Result]
            }
          }

        args.extractValues match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr, 1)
          case ArrayValue(arr) :: FunctionValue(expr) :: IntValue(min) :: Nil =>
            safeToInt(min, "some threshold").fold(
              err => err.asLeft[Result[JsonLogicValue]].pure[F],
              minInt => impl(arr, expr, minInt)
            )
          case _ => JsonLogicException(s"Unexpected input to ${SomeOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleMapValuesOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case NullValue :: Nil   => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case MapValue(v) :: Nil => (ArrayValue(v.values.toList): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input for `${MapValuesOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleMapKeysOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case NullValue :: Nil   => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case MapValue(v) :: Nil => (ArrayValue(v.keys.map(StrValue(_)).toList): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input for `${MapKeysOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleGetOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def implMap(
          input: Map[String, JsonLogicValue],
          key: String
        ): Either[JsonLogicException, Result[JsonLogicValue]] = {
          val availableKeys = input.keys.take(5).mkString(", ") + (if (input.size > 5) ", ..." else "")
          Either.fromOption(
            input.get(key).map(_.pure[Result]),
            JsonLogicException(s"Could not find key '$key' in map with keys: [$availableKeys]")
          )
        }

        args.withMetrics { values =>
          values match {
            case MapValue(v) :: StrValue(k) :: Nil => implMap(v, k)
            case _ => JsonLogicException(s"Unexpected input to ${GetOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleCountOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def countSimple(arr: List[JsonLogicValue]): Either[JsonLogicException, Result[JsonLogicValue]] =
          (IntValue(arr.length): JsonLogicValue).pure[Result].asRight[JsonLogicException]

        def countWithPredicate(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some).map {
              case Right(result) => result.extractValue.isTruthy.asRight[JsonLogicException]
              case Left(err)     => err.asLeft[Boolean]
            }
          }.map {
            _.sequence.map { bools =>
              (IntValue(bools.count(identity)): JsonLogicValue).pure[Result]
            }
          }

        args.extractValues match {
          case ArrayValue(arr) :: Nil                        => countSimple(arr).pure[F]
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => countWithPredicate(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${CountOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleLengthOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: Nil => (IntValue(arr.length): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case StrValue(str) :: Nil   => (IntValue(str.length): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${LengthOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleFindOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr
            .foldLeftM[F, Either[JsonLogicException, Option[JsonLogicValue]]](None.asRight) {
              case (Right(acc @ Some(_)), _) =>
                (acc.asRight[JsonLogicException]: Either[JsonLogicException, Option[JsonLogicValue]]).pure[F]
              case (Right(None), el) =>
                evaluationStrategy(expr, el.some).map {
                  case Right(result) =>
                    (if (result.extractValue.isTruthy) Some(el) else None).asRight[JsonLogicException]
                  case Left(err) => err.asLeft[Option[JsonLogicValue]]
                }
              case (Left(err), _) => err.asLeft[Option[JsonLogicValue]].pure[F]
            }
            .map {
              case Right(Some(value)) => (value: JsonLogicValue).pure[Result].asRight[JsonLogicException]
              case Right(None)        => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
              case Left(err)          => err.asLeft[Result[JsonLogicValue]]
            }

        args.extractValues match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${FindOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleLowerOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case StrValue(str) :: Nil => (StrValue(str.toLowerCase): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${LowerOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleUpperOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case StrValue(str) :: Nil => (StrValue(str.toUpperCase): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${UpperOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleJoinOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def arrayToString(value: JsonLogicValue): String = value match {
          case NullValue        => ""
          case BoolValue(v)     => v.toString
          case IntValue(v)      => v.toString
          case FloatValue(v)    => v.toString
          case StrValue(v)      => v
          case ArrayValue(_)    => ""
          case MapValue(_)      => ""
          case FunctionValue(_) => ""
        }

        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: StrValue(separator) :: Nil =>
              (StrValue(arr.map(arrayToString).mkString(separator)): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${JoinOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleSplitOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case StrValue(str) :: StrValue(separator) :: Nil =>
              if (separator.isEmpty)
                JsonLogicException("Split separator cannot be empty").asLeft[Result[JsonLogicValue]]
              else
                (ArrayValue(str.split(java.util.regex.Pattern.quote(separator), -1).map(StrValue(_)).toList): JsonLogicValue)
                  .pure[Result]
                  .asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${SplitOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleDefaultOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values.collectFirst {
            case v if v != NullValue && v.isTruthy => v
          }
            .getOrElse(NullValue)
            .asRight[JsonLogicException]
            .map(_.pure[Result])
            .pure[F]
        }

      private def handleUniqueOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: Nil =>
              // Use LinkedHashSet for O(n) distinct while preserving insertion order
              val seen = scala.collection.mutable.LinkedHashSet.empty[JsonLogicValue]
              arr.foreach(seen.add)
              (ArrayValue(seen.toList): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${UniqueOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleSliceOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: IntValue(start) :: Nil =>
              safeToInt(start, "slice start").map { s =>
                val startIdx = if (s < 0) Math.max(0, arr.length + s) else s
                (ArrayValue(arr.drop(startIdx)): JsonLogicValue).pure[Result]
              }
            case ArrayValue(arr) :: IntValue(start) :: IntValue(end) :: Nil =>
              for {
                s <- safeToInt(start, "slice start")
                e <- safeToInt(end, "slice end")
              } yield {
                val startIdx = if (s < 0) Math.max(0, arr.length + s) else s
                val endIdx = if (e < 0) Math.max(0, arr.length + e) else e
                (ArrayValue(arr.slice(startIdx, endIdx)): JsonLogicValue).pure[Result]
              }
            case _ => JsonLogicException(s"Unexpected input to ${SliceOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleReverseOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: Nil => (ArrayValue(arr.reverse): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${ReverseOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleFlattenOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case ArrayValue(arr) :: Nil =>
              val flattened = arr.flatMap {
                case ArrayValue(inner) => inner
                case other             => List(other)
              }
              (ArrayValue(flattened): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${FlattenOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleTrimOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case StrValue(str) :: Nil => (StrValue(str.trim): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${TrimOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleStartsWithOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case StrValue(str) :: StrValue(prefix) :: Nil =>
              (BoolValue(str.startsWith(prefix)): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            // Null handling: null prefix or null string returns false
            case StrValue(_) :: NullValue :: Nil =>
              (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case NullValue :: _ :: Nil =>
              (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${StartsWithOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleEndsWithOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case StrValue(str) :: StrValue(suffix) :: Nil =>
              (BoolValue(str.endsWith(suffix)): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            // Null handling: null suffix or null string returns false
            case StrValue(_) :: NullValue :: Nil =>
              (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case NullValue :: _ :: Nil =>
              (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${EndsWithOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleAbsOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case IntValue(v) :: Nil => ((IntValue(v.abs): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case FloatValue(v) :: Nil =>
              ((FloatValue(v.abs): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case v :: Nil =>
              promoteToNumeric(v).map {
                case IntResult(n)   => (IntValue(n.abs): JsonLogicValue).pure[Result]
                case FloatResult(n) => (FloatValue(n.abs): JsonLogicValue).pure[Result]
              }
            case _ => JsonLogicException(s"Unexpected input to ${AbsOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleRoundOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case IntValue(v) :: Nil => ((IntValue(v): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case FloatValue(v) :: Nil =>
              val rounded = v.setScale(0, BigDecimal.RoundingMode.HALF_UP)
              if (rounded.isValidLong) ((IntValue(rounded.toBigInt): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
              else ((FloatValue(rounded): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
            case v :: Nil =>
              promoteToNumeric(v).map {
                case IntResult(n) => (IntValue(n): JsonLogicValue).pure[Result]
                case FloatResult(n) =>
                  val rounded = n.setScale(0, BigDecimal.RoundingMode.HALF_UP)
                  if (rounded.isValidLong) (IntValue(rounded.toBigInt): JsonLogicValue).pure[Result]
                  else (FloatValue(rounded): JsonLogicValue).pure[Result]
              }
            case _ => JsonLogicException(s"Unexpected input to ${RoundOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleFloorOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case IntValue(v) :: Nil => ((IntValue(v): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case FloatValue(v) :: Nil =>
              val floored = v.setScale(0, BigDecimal.RoundingMode.FLOOR)
              if (floored.isValidLong) ((IntValue(floored.toBigInt): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
              else ((FloatValue(floored): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
            case v :: Nil =>
              promoteToNumeric(v).map {
                case IntResult(n) => (IntValue(n): JsonLogicValue).pure[Result]
                case FloatResult(n) =>
                  val floored = n.setScale(0, BigDecimal.RoundingMode.FLOOR)
                  if (floored.isValidLong) (IntValue(floored.toBigInt): JsonLogicValue).pure[Result]
                  else (FloatValue(floored): JsonLogicValue).pure[Result]
              }
            case _ => JsonLogicException(s"Unexpected input to ${FloorOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleCeilOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case IntValue(v) :: Nil => ((IntValue(v): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case FloatValue(v) :: Nil =>
              val ceiled = v.setScale(0, BigDecimal.RoundingMode.CEILING)
              if (ceiled.isValidLong) ((IntValue(ceiled.toBigInt): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
              else ((FloatValue(ceiled): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
            case v :: Nil =>
              promoteToNumeric(v).map {
                case IntResult(n) => (IntValue(n): JsonLogicValue).pure[Result]
                case FloatResult(n) =>
                  val ceiled = n.setScale(0, BigDecimal.RoundingMode.CEILING)
                  if (ceiled.isValidLong) (IntValue(ceiled.toBigInt): JsonLogicValue).pure[Result]
                  else (FloatValue(ceiled): JsonLogicValue).pure[Result]
              }
            case _ => JsonLogicException(s"Unexpected input to ${CeilOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handlePowOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        val maxSafeExponent = 999

        def isNonNegativeIntExp(num: NumericResult): Boolean = num match {
          case IntResult(e)   => e >= 0 && e.isValidInt && e <= maxSafeExponent
          case FloatResult(e) => e.isWhole && e >= 0 && e <= maxSafeExponent
        }

        def computeExactPow(baseNum: NumericResult, expInt: Int): Either[JsonLogicException, Result[JsonLogicValue]] = {
          val baseDecimal = baseNum.toBigDecimal
          if (baseDecimal.isWhole) {
            // Use BigInt.pow for exact integer result
            val result = baseDecimal.toBigInt.pow(expInt)
            ((IntValue(result): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
          } else {
            // Use BigDecimal.pow for better precision with fractional base
            val result = baseDecimal.pow(expInt)
            if (result.isWhole && result.isValidLong)
              ((IntValue(result.toBigInt): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
            else
              ((FloatValue(result): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
          }
        }

        def computeDoublePow(baseNum: NumericResult, expDouble: Double): Either[JsonLogicException, Result[JsonLogicValue]] = {
          val powResult = Math.pow(baseNum.toBigDecimal.toDouble, expDouble)
          if (powResult.isInfinity) {
            JsonLogicException(s"Power operation resulted in infinity").asLeft[Result[JsonLogicValue]]
          } else if (powResult.isNaN) {
            JsonLogicException(s"Power operation resulted in NaN").asLeft[Result[JsonLogicValue]]
          } else if (powResult.isWhole && powResult.isValidInt) {
            ((IntValue(BigInt(powResult.toInt)): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
          } else {
            ((FloatValue(BigDecimal(powResult)): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
          }
        }

        args.withMetrics { values =>
          values match {
            case IntValue(base) :: IntValue(exp) :: Nil if exp >= 0 && exp.isValidInt && exp <= maxSafeExponent =>
              ((IntValue(base.pow(exp.toInt)): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case IntValue(_) :: IntValue(exp) :: Nil if exp > maxSafeExponent =>
              JsonLogicException(
                s"Exponent $exp exceeds maximum safe value $maxSafeExponent for integer exponentiation"
              ).asLeft[Result[JsonLogicValue]]
            case base :: exp :: Nil =>
              for {
                baseNum <- promoteToNumeric(base)
                expNum  <- promoteToNumeric(exp)
                result <-
                  if (isNonNegativeIntExp(expNum)) {
                    // Use exact arithmetic for non-negative integer exponents
                    computeExactPow(baseNum, expNum.toBigDecimal.toInt)
                  } else {
                    // Fall back to Double for negative or fractional exponents
                    val expDouble = expNum.toBigDecimal.toDouble
                    if (expDouble.abs > maxSafeExponent) {
                      JsonLogicException(
                        s"Exponent magnitude ${expDouble.abs} exceeds maximum safe value $maxSafeExponent"
                      ).asLeft[Result[JsonLogicValue]]
                    } else {
                      computeDoublePow(baseNum, expDouble)
                    }
                  }
              } yield result
            case _ => JsonLogicException(s"Unexpected input to ${PowOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleHasOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case MapValue(m) :: StrValue(key) :: Nil =>
              ((BoolValue(m.contains(key)): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${HasOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleEntriesOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case MapValue(m) :: Nil =>
              val entries = m.toList.map { case (k, v) => ArrayValue(List(StrValue(k), v)) }
              ((ArrayValue(entries): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input to ${EntriesOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleTypeOfOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case value :: Nil => (StrValue(value.tag): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _            => JsonLogicException(s"Unexpected input to ${TypeOfOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }
    }

  implicit class semanticOpsV2[F[_]: Monad, Result[_]: ResultContext](sem: JsonLogicSemantics[F, Result]) {

    def evaluateWith(
      program: JsonLogicExpression,
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
      implicit val implicitSem: JsonLogicSemantics[F, Result] = sem
      JsonLogicRuntime.evaluate(program, ctx)
    }
  }
}
