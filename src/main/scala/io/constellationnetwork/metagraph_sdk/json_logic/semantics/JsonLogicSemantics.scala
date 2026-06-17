package io.constellationnetwork.metagraph_sdk.json_logic.semantics

import cats.syntax.all._
import cats.{Monad, MonadThrow}

import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicOp._
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.CoercionOps._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.NumericOps._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.{AuthDbOps, CryptoOps}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.ResultContext._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.{JsonLogicRuntime, ResultContext}
import io.constellationnetwork.metagraph_sdk.numerics.Ratio
import io.constellationnetwork.metagraph_sdk.numerics.RatioOps.implicits._

trait JsonLogicSemantics[F[_], Result[_]] {
  def getVar(key: String, ctx: Option[JsonLogicValue] = None): F[Either[JsonLogicException, Result[JsonLogicValue]]]

  /**
   * Apply `op` to its (already evaluated) arguments. `depth` is the runtime recursion depth of
   * the applying node (see [[JsonLogicRuntime.MaxEvalDepth]]): handlers that run callbacks
   * (map / filter / reduce / ...) resume nested evaluation FROM it, so the depth guard counts
   * callback runs exactly like the Rust reference's shared depth cell.
   */
  def applyOp(op: JsonLogicOp, depth: Int): List[Result[JsonLogicValue]] => F[Either[JsonLogicException, Result[JsonLogicValue]]]

  /** Depth-less convenience overload: applies at recursion depth 0 (top of tree). */
  def applyOp(op: JsonLogicOp): List[Result[JsonLogicValue]] => F[Either[JsonLogicException, Result[JsonLogicValue]]] =
    applyOp(op, 0)

  /**
   * Flat per-node charge for ops the runtime dispatches LAZILY (`if` / `let`) without ever
   * reaching [[applyOp]]. The runtime invokes this once at the dispatch site, BEFORE the
   * node's children are evaluated. `None` (the default) means the semantics meters nothing;
   * gas-aware semantics return the base-cost consumption against the shared gas ref.
   */
  def chargeBase(op: JsonLogicOp): Option[F[Either[JsonLogicException, Unit]]] = None
}

object JsonLogicSemantics {

  /**
   * Nested-evaluation callback. The third argument is the runtime recursion depth the nested
   * run resumes FROM (its root expression then evaluates at `depth + 1`), threading the
   * [[JsonLogicRuntime.MaxEvalDepth]] guard across callback boundaries.
   */
  type EvaluationCallback[F[_], Result[_]] =
    (JsonLogicExpression, Option[JsonLogicValue], Int) => F[Either[JsonLogicException, Result[JsonLogicValue]]]

  def apply[F[_], Result[_]](implicit ev: JsonLogicSemantics[F, Result]): JsonLogicSemantics[F, Result] = ev

  def make[F[_]: MonadThrow, Result[_]: ResultContext](
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

      override def applyOp(
        op: JsonLogicOp,
        depth: Int
      ): List[Result[JsonLogicValue]] => F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        op match {
          case NoOp                 => _ => JsonLogicException("Got unexpected NoOp!").asLeft[Result[JsonLogicValue]].pure[F]
          case MissingNoneOp        => handleMissingNone
          case ExistsOp             => handleExists
          case MissingSomeOp        => handleMissingSome
          case IfElseOp             => handleIfElseOp(_, depth)
          case LetOp                => handleLetOp
          case EqOp                 => handleEqOp
          case EqStrictOp           => handleEqStrictOp
          case NEqOp                => handleNEqOp
          case NEqStrictOp          => handleNEqStrictOp
          case NotOp                => handleNotOp
          case NOp                  => handleNOp
          case OrOp                 => handleOrOp
          case AndOp                => handleAndOp
          case Lt                   => handleLt
          case Leq                  => handleLeq
          case Gt                   => handleGt
          case Geq                  => handleGeq
          case ModuloOp             => handleModuloOp
          case MaxOp                => handleMaxOp
          case MinOp                => handleMinOp
          case AddOp                => handleAddOp
          case TimesOp              => handleTimesOp
          case MinusOp              => handleMinusOp
          case DivOp                => handleDivOp
          case MergeOp              => handleMergeOp
          case InOp                 => handleInOp
          case CatOp                => handleCatOp
          case SubStrOp             => handleSubstrOp
          case MapOp                => handleMapOp(_, depth)
          case FilterOp             => handleFilterOp(_, depth)
          case ReduceOp             => handleReduceOp(_, depth)
          case AllOp                => handleAllOp(_, depth)
          case NoneOp               => handleNoneOp(_, depth)
          case SomeOp               => handleSomeOp(_, depth)
          case MapValuesOp          => handleMapValuesOp
          case MapKeysOp            => handleMapKeysOp
          case GetOp                => handleGetOp
          case IntersectOp          => handleIntersectOp
          case CountOp              => handleCountOp(_, depth)
          case LengthOp             => handleLengthOp
          case FindOp               => handleFindOp(_, depth)
          case LowerOp              => handleLowerOp
          case UpperOp              => handleUpperOp
          case JoinOp               => handleJoinOp
          case SplitOp              => handleSplitOp
          case DefaultOp            => handleDefaultOp
          case UniqueOp             => handleUniqueOp
          case SliceOp              => handleSliceOp
          case ReverseOp            => handleReverseOp
          case FlattenOp            => handleFlattenOp
          case TrimOp               => handleTrimOp
          case StartsWithOp         => handleStartsWithOp
          case EndsWithOp           => handleEndsWithOp
          case AbsOp                => handleAbsOp
          case RoundOp              => handleRoundOp
          case FloorOp              => handleFloorOp
          case CeilOp               => handleCeilOp
          case PowOp                => handlePowOp
          case HasOp                => handleHasOp
          case EntriesOp            => handleEntriesOp
          case TypeOfOp             => handleTypeOfOp
          case PoseidonOp           => handlePoseidonOp
          case PmtVerifyOp          => handlePmtVerifyOp
          case Groth16VerifyOp      => handleGroth16VerifyOp
          case EcVrfVerifyOp        => handleEcVrfVerifyOp
          case Bn254AddOp           => handleBn254AddOp
          case Bn254MulOp           => handleBn254MulOp
          case Bn254PairingOp       => handleBn254PairingOp
          case BlsVerifyOp          => handleBlsVerifyOp
          case BlsAggregateVerifyOp => handleBlsAggregateVerifyOp
          case SchnorrVerifyOp      => handleSchnorrVerifyOp
          case SmtVerifyOp          => handleSmtVerifyOp
          case MptVerifyOp          => handleMptVerifyOp
          case MptPrefixVerifyOp    => handleMptPrefixVerifyOp
          case ProveDlogVerifyOp    => handleProveDlogVerifyOp
          case ProveDhTupleVerifyOp => handleProveDhTupleVerifyOp
          case SigmaVerifyOp        => handleSigmaVerifyOp
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
          getVar(floatToPlainString(key)).map {
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

      private def handleIfElseOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
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
              evaluationStrategy(branchExpr, None, depth).map(
                _.map(branchResult => ResultContext[Result].flatMap(combined)(_ => branchResult))
              )
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
            // `!==` is the negation of `===`: values of mismatched types are NOT strictly equal, hence strictly
            // not-equal. Matches the JSON Logic reference / TS (`!==` === `!(===)`).
            case _ => true
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
                  combineNumeric((a, b) => a.mod(b))(ln, rn).pure[Result].asRight[JsonLogicException]
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
              val maxValue = numerics.map(_.toRatio).reduce((a, b) => a.max(b))
              val hasFloat = numerics.exists(_.isFloat)

              val result: JsonLogicValue = if (!hasFloat && maxValue.isInteger) {
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
              val minValue = numerics.map(_.toRatio).reduce((a, b) => a.min(b))
              val hasFloat = numerics.exists(_.isFloat)

              val result: JsonLogicValue = if (!hasFloat && minValue.isInteger) {
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
            reduceNumeric(list, (a, b) => a + b).map(v => v: JsonLogicValue)
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
          else reduceNumeric(list, (a, b) => a * b).map(v => (v: JsonLogicValue).pure[Result])

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
                combineNumeric((a, _) => Ratio.Zero - a)(n, IntResult(0)).pure[Result]
              }
            case l :: r :: Nil =>
              for {
                ln <- promoteToNumeric(l)
                rn <- promoteToNumeric(r)
              } yield combineNumeric((a, b) => a - b)(ln, rn).pure[Result]
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
                  // Exact rational division — no rounding anywhere in the evaluator path.
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
            case FloatValue(value) => floatToPlainString(value)
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
            case FloatValue(value) => floatToPlainString(value).asRight[JsonLogicException]
            case StrValue(value)   => value.asRight[JsonLogicException]
          }
            .map(argStrings => (StrValue(argStrings.mkString): JsonLogicValue).pure[Result])
            .pure[F]
        }

      private def handleSubstrOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        // i64-saturating index semantics, byte-matching Rust `op_substr` (eval.rs): start/length accept the
        // full i64 range (values beyond it error via safeToI64), index arithmetic saturates at the i64
        // bounds, and the clamps below make saturation equivalent to exact (unbounded) arithmetic.
        def impl(str: String, start: Long, length: Long): Either[JsonLogicException, Result[JsonLogicValue]] =
          for {
            s <- Option(str).toRight(JsonLogicException("substr expects a non-null string"))
            strLen = s.length.toLong
            rawStart = if (start < 0L) saturatingAddI64(strLen, start) else start
            startIdx = Math.max(0L, Math.min(rawStart, strLen))
            endIdx =
              if (length >= 0L) Math.min(saturatingAddI64(startIdx, length), strLen)
              else Math.max(0L, saturatingAddI64(strLen, length))
            substr = if (startIdx >= strLen || endIdx <= startIdx) "" else s.substring(startIdx.toInt, endIdx.toInt)
          } yield (StrValue(substr): JsonLogicValue).pure[Result]

        args.withMetrics {
          case StrValue(str) :: IntValue(start) :: Nil =>
            safeToI64(start, "substr start").flatMap(s => impl(str, s, str.length.toLong))
          case StrValue(str) :: IntValue(start) :: IntValue(length) :: Nil =>
            for {
              s <- safeToI64(start, "substr start")
              l <- safeToI64(length, "substr length")
              r <- impl(str, s, l)
            } yield r
          case _ => JsonLogicException(s"Unexpected input to `${SubStrOp.tag}` got $values").asLeft[Result[JsonLogicValue]]
        }
      }

      private def handleMapOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(arr: List[JsonLogicValue], expr: JsonLogicExpression): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr
            .traverse(el => evaluationStrategy(expr, el.some, depth))
            .map(_.sequence.map(_.sequence.map(ArrayValue(_))))

        args.extractValues match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${MapOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleFilterOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some, depth).map {
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

      private def handleReduceOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression,
          init: JsonLogicValue
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.foldLeftM[F, Either[JsonLogicException, Result[JsonLogicValue]]](init.pure[Result].asRight) { (accEither, item) =>
            accEither match {
              case Left(err) => err.asLeft[Result[JsonLogicValue]].pure[F]
              case Right(accResult) =>
                evaluationStrategy(expr, MapValue(Map("current" -> item, "accumulator" -> accResult.extractValue)).some, depth).map {
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

      private def handleAllOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some, depth).map {
              case Right(result) => result.map(_.isTruthy).asRight[JsonLogicException]
              case Left(err)     => err.asLeft[Result[Boolean]]
            }
          }.map(_.sequence.map(_.sequence.map(_.forall(identity)).map(BoolValue(_)): Result[JsonLogicValue]))

        args.withMetrics {
          case NullValue :: FunctionValue(_) :: Nil => (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
          // `all` over an EMPTY array is false (JSON Logic reference / TS), not vacuously true.
          case ArrayValue(Nil) :: FunctionValue(_) :: Nil =>
            (BoolValue(false): JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${AllOp.tag}, got $values").asLeft[Result[JsonLogicValue]].pure[F]
        }
      }

      private def handleNoneOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some, depth).map {
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

      private def handleSomeOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression,
          threshold: Int
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some, depth).map {
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
            case Nil                => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case NullValue :: Nil   => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case MapValue(v) :: Nil => (ArrayValue(v.values.toList): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input for `${MapValuesOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleMapKeysOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case Nil                => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case NullValue :: Nil   => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case MapValue(v) :: Nil => (ArrayValue(v.keys.map(StrValue(_)).toList): JsonLogicValue).pure[Result].asRight[JsonLogicException]
            case _ => JsonLogicException(s"Unexpected input for `${MapKeysOp.tag}' got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleGetOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        // Look up `key` in `input`, returning `fallback` when the key is absent.
        // Mirrors Rust `op_get` (rust/jlvm-core/src/eval.rs): the 2-arg form falls
        // back to NullValue, the 3-arg form falls back to the supplied default.
        def implMap(
          input: Map[String, JsonLogicValue],
          key: String,
          fallback: JsonLogicValue
        ): Either[JsonLogicException, Result[JsonLogicValue]] =
          input.get(key) match {
            case Some(value) => value.pure[Result].asRight[JsonLogicException]
            case None        => fallback.pure[Result].asRight[JsonLogicException]
          }

        args.withMetrics { values =>
          values match {
            case MapValue(v) :: StrValue(k) :: Nil            => implMap(v, k, NullValue)
            case MapValue(v) :: StrValue(k) :: default :: Nil => implMap(v, k, default)
            case _ => JsonLogicException(s"Unexpected input to ${GetOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }
      }

      private def handleCountOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def countSimple(arr: List[JsonLogicValue]): Either[JsonLogicException, Result[JsonLogicValue]] =
          (IntValue(arr.length): JsonLogicValue).pure[Result].asRight[JsonLogicException]

        def countWithPredicate(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr.traverse { el =>
            evaluationStrategy(expr, el.some, depth).map {
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

      private def handleFindOp(args: List[Result[JsonLogicValue]], depth: Int): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          arr
            .foldLeftM[F, Either[JsonLogicException, Option[JsonLogicValue]]](None.asRight) {
              case (Right(acc @ Some(_)), _) =>
                (acc.asRight[JsonLogicException]: Either[JsonLogicException, Option[JsonLogicValue]]).pure[F]
              case (Right(None), el) =>
                evaluationStrategy(expr, el.some, depth).map {
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
          case FloatValue(v)    => floatToPlainString(v)
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
          // i64-saturating index semantics, byte-matching Rust `op_slice` (eval.rs): start/end accept the
          // full i64 range (values beyond it error via safeToI64) and the saturating add + clamps yield
          // the same indices as exact arithmetic (see handleSubstrOp for the same hazard).
          def clampIndex(idx: Long, len: Long): Long =
            if (idx < 0L) Math.max(0L, saturatingAddI64(len, idx)) else Math.min(idx, len)

          values match {
            case ArrayValue(arr) :: IntValue(start) :: Nil =>
              safeToI64(start, "slice start").map { s =>
                val startIdx = clampIndex(s, arr.length.toLong)
                (ArrayValue(arr.drop(startIdx.toInt)): JsonLogicValue).pure[Result]
              }
            case ArrayValue(arr) :: IntValue(start) :: IntValue(end) :: Nil =>
              for {
                s <- safeToI64(start, "slice start")
                e <- safeToI64(end, "slice end")
              } yield {
                val len = arr.length.toLong
                val startIdx = clampIndex(s, len)
                val endIdx = clampIndex(e, len)
                (ArrayValue(arr.slice(startIdx.toInt, endIdx.toInt)): JsonLogicValue).pure[Result]
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
              ((IntValue(v.roundHalfUp): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
            case v :: Nil =>
              promoteToNumeric(v).map {
                case IntResult(n)   => (IntValue(n): JsonLogicValue).pure[Result]
                case FloatResult(n) => (IntValue(n.roundHalfUp): JsonLogicValue).pure[Result]
              }
            case _ => JsonLogicException(s"Unexpected input to ${RoundOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleFloorOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case IntValue(v) :: Nil => ((IntValue(v): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case FloatValue(v) :: Nil =>
              ((IntValue(v.floor): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
            case v :: Nil =>
              promoteToNumeric(v).map {
                case IntResult(n)   => (IntValue(n): JsonLogicValue).pure[Result]
                case FloatResult(n) => (IntValue(n.floor): JsonLogicValue).pure[Result]
              }
            case _ => JsonLogicException(s"Unexpected input to ${FloorOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handleCeilOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          values match {
            case IntValue(v) :: Nil => ((IntValue(v): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case FloatValue(v) :: Nil =>
              ((IntValue(v.ceil): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight
            case v :: Nil =>
              promoteToNumeric(v).map {
                case IntResult(n)   => (IntValue(n): JsonLogicValue).pure[Result]
                case FloatResult(n) => (IntValue(n.ceil): JsonLogicValue).pure[Result]
              }
            case _ => JsonLogicException(s"Unexpected input to ${CeilOp.tag}, got $values").asLeft[Result[JsonLogicValue]]
          }
        }

      private def handlePowOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
        val maxSafeExponent = 999

        args.withMetrics { values =>
          values match {
            case IntValue(base) :: IntValue(exp) :: Nil if exp >= 0 && exp.isValidInt && exp <= maxSafeExponent =>
              ((IntValue(base.pow(exp.toInt)): JsonLogicValue).pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
            case IntValue(_) :: IntValue(exp) :: Nil if exp > maxSafeExponent =>
              // Error text matches the Rust reference (eval.rs op_pow) byte-for-byte.
              JsonLogicException(
                s"Exponent $exp exceeds maximum safe value $maxSafeExponent"
              ).asLeft[Result[JsonLogicValue]]
            case base :: exp :: Nil =>
              for {
                baseNum <- promoteToNumeric(base)
                expNum  <- promoteToNumeric(exp)
                result <- expNum.toRatio.toBigIntExact match {
                  case None =>
                    // Deterministic VM: only integer exponents are supported (no Math.pow / irrational results).
                    JsonLogicException(
                      s"Exponent must be an integer for deterministic exponentiation, got ${expNum.toJsonLogicValue}"
                    ).asLeft[Result[JsonLogicValue]]
                  case Some(e) if e.abs > maxSafeExponent =>
                    JsonLogicException(
                      s"Exponent magnitude ${e.abs} exceeds maximum safe value $maxSafeExponent"
                    ).asLeft[Result[JsonLogicValue]]
                  case Some(e) =>
                    val br = baseNum.toRatio
                    if (e < 0 && br.numerator == 0) {
                      JsonLogicException("Zero cannot be raised to a negative power").asLeft[Result[JsonLogicValue]]
                    } else {
                      val powed = if (e >= 0) br.pow(e.toInt) else br.inverse.pow(-e.toInt)
                      val jlv: JsonLogicValue =
                        if (!baseNum.isFloat && e >= 0 && powed.isInteger) IntValue(powed.toBigInt)
                        else FloatValue(powed)
                      (jlv.pure[Result]: Result[JsonLogicValue]).asRight[JsonLogicException]
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
            case Nil => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
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

      /**
       * Defensive boundary for the pure crypto opcodes. Every `CryptoOps.*` op is TOTAL by contract
       * (returns `Either[JsonLogicException, JsonLogicValue]`, never throws); this guard is the
       * belt-and-suspenders that GUARANTEES a residual or future throw in the pure layer can never
       * escape INTO `F[_]` (where it would surface as a raised error in the consensus combiner —
       * block-poisoning / node-crash risk). It catches any non-fatal `Throwable` and turns it into a
       * deterministic `Left`, tagged with the opcode. On the happy path it is a no-op (the op already
       * returned a `Left`/`Right`), so it does NOT change any behavior or conformance vector.
       */
      private def guardOp(op: String)(
        e: => Either[JsonLogicException, JsonLogicValue]
      ): Either[JsonLogicException, JsonLogicValue] =
        Either.catchNonFatal(e).fold(t => JsonLogicException(s"$op: unexpected internal error: ${t.getMessage}").asLeft, identity)

      private def handlePoseidonOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("poseidon")(CryptoOps.poseidon(values)).map(_.pure[Result])
        }

      private def handlePmtVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("pmt_verify")(CryptoOps.pmtVerify(values)).map(_.pure[Result])
        }

      private def handleGroth16VerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("groth16_verify")(CryptoOps.groth16Verify(values)).map(_.pure[Result])
        }

      private def handleEcVrfVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("ecvrf_verify")(CryptoOps.ecVrfVerify(values)).map(_.pure[Result])
        }

      private def handleBn254AddOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("bn254_add")(CryptoOps.bn254Add(values)).map(_.pure[Result])
        }

      private def handleBn254MulOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("bn254_mul")(CryptoOps.bn254Mul(values)).map(_.pure[Result])
        }

      private def handleBn254PairingOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("bn254_pairing")(CryptoOps.bn254Pairing(values)).map(_.pure[Result])
        }

      private def handleBlsVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("bls_verify")(CryptoOps.blsVerify(values)).map(_.pure[Result])
        }

      private def handleBlsAggregateVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("bls_aggregate_verify")(CryptoOps.blsAggregateVerify(values)).map(_.pure[Result])
        }

      private def handleSchnorrVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("schnorr_verify")(CryptoOps.schnorrVerify(values)).map(_.pure[Result])
        }

      // Sigma-protocol leaves. prove_dlog_verify is a first-class alias for schnorr_verify (the
      // DLog leaf); prove_dhtuple_verify is the standalone DDH / Diffie-Hellman-tuple leaf.
      private def handleProveDlogVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("prove_dlog_verify")(CryptoOps.proveDlogVerify(values)).map(_.pure[Result])
        }

      private def handleProveDhTupleVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("prove_dhtuple_verify")(CryptoOps.proveDhTupleVerify(values)).map(_.pure[Result])
        }

      // Recursive CDS Σ-protocol tree verifier (ring + threshold). Pure over already-evaluated
      // proposition/proof MapValue trees + a hex message; gas is pre-charged per-leaf/per-node
      // from the proposition shape in the gas-aware layer before any curve arithmetic runs.
      private def handleSigmaVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          guardOp("sigma_verify")(CryptoOps.sigmaVerify(values)).map(_.pure[Result])
        }

      // WAVE 3 -- auth-DB verifiers. Unlike the pure CryptoOps above, these run in F (the verifiers
      // are F[_]: MonadThrow: JsonBinaryHasher), so the handler awaits the effectful Either result and
      // lifts the value into Result, preserving the upstream metrics via withMetrics.
      private def handleSmtVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          AuthDbOps.smtVerify[F](values).map(_.map(_.pure[Result]))
        }

      private def handleMptVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          AuthDbOps.mptVerify[F](values).map(_.map(_.pure[Result]))
        }

      private def handleMptPrefixVerifyOp(args: List[Result[JsonLogicValue]]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        args.withMetrics { values =>
          AuthDbOps.mptPrefixVerify[F](values).map(_.map(_.pure[Result]))
        }

      // Let is handled specially in the runtime; this should not be reached
      private def handleLetOp(
        @annotation.unused args: List[Result[JsonLogicValue]]
      ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
        JsonLogicException("let operator should be handled in runtime, not semantics").asLeft[Result[JsonLogicValue]].pure[F]
    }

  implicit class semanticOpsV2[F[_]: Monad, Result[_]: ResultContext](sem: JsonLogicSemantics[F, Result]) {

    def evaluateWith(
      program: JsonLogicExpression,
      ctx: Option[JsonLogicValue],
      baseDepth: Int = 0
    ): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
      implicit val implicitSem: JsonLogicSemantics[F, Result] = sem
      JsonLogicRuntime.evaluate(program, ctx, baseDepth)
    }
  }
}
