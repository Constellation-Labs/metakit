package io.constellationnetwork.metagraph_sdk.json_logic

import cats.Monad
import cats.data.EitherT
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.JsonLogicOp._

trait JsonLogicSemantics[F[_]] {
  def getVar(key: String, ctx: Option[JsonLogicValue] = None): F[Either[JsonLogicException, JsonLogicValue]]
  def applyOp(op: JsonLogicOp): List[JsonLogicValue] => F[Either[JsonLogicException, JsonLogicValue]]
}

object JsonLogicSemantics {

  type EvaluationCallback[F[_]] =
    (JsonLogicExpression, Option[JsonLogicValue]) => F[Either[JsonLogicException, JsonLogicValue]]

  def apply[F[_]](implicit ev: JsonLogicSemantics[F]): JsonLogicSemantics[F] = ev

  def make[F[_]: Monad](
    vars: JsonLogicValue,
    evaluationStrategy: EvaluationCallback[F]
  ): JsonLogicSemantics[F] =
    new JsonLogicSemantics[F] {

      override def getVar(
        key: String,
        ctx: Option[JsonLogicValue] = None
      ): F[Either[JsonLogicException, JsonLogicValue]] = {

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

        if (key.isEmpty) ctx.getOrElse(vars).asRight[JsonLogicException].pure[F]
        else if (key.endsWith(".")) NullValue.asRight[JsonLogicException].pure[F].widen
        else {
          val segments = key.split("\\.").toList
          (for {
            combined <- combineState(vars, ctx)
            finalVal <- segments.foldLeft(combined.asRight[JsonLogicException]) { (acc, seg) =>
              acc.flatMap(getChild(_, seg))
            }
          } yield finalVal).pure[F]
        }
      }

      override def applyOp(op: JsonLogicOp): List[JsonLogicValue] => F[Either[JsonLogicException, JsonLogicValue]] =
        op match {
          case NoOp          => _ => JsonLogicException("Got unexpected NoOp!").asLeft[JsonLogicValue].pure[F]
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

      /** If 'key' is missing then Some('key') is returned, else if found return None */
      private def isFieldMissing(field: JsonLogicValue): F[Option[JsonLogicValue]] = field match {
        case v @ StrValue(key) =>
          getVar(key).map {
            case Right(NullValue) => v.some // NullValue means field is missing
            case Right(_)         => None // Field exists
            case Left(_)          => v.some // Error also means missing
          }
        case v @ IntValue(key) =>
          getVar(key.toString).map {
            case Right(NullValue) => v.some
            case Right(_)         => None
            case Left(_)          => v.some
          }
        case v @ FloatValue(key) =>
          getVar(key.toString).map {
            case Right(NullValue) => v.some
            case Right(_)         => None
            case Left(_)          => v.some
          }
        case v => v.some.pure[F]
      }

      private def handleMissingNone(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(list: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
          list
            .traverseFilter(isFieldMissing)
            .map(l => (ArrayValue(l): JsonLogicValue).asRight[JsonLogicException])

        args match {
          case ArrayValue(arr) :: Nil => impl(arr)
          case _                      => impl(args)
        }
      }

      private def handleExists(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(list: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
          (BoolValue(!list.contains(NullValue)): JsonLogicValue).asRight[JsonLogicException].pure[F]

        args match {
          case ArrayValue(arr) :: Nil => impl(arr)
          case _                      => impl(args)
        }
      }

      private def handleMissingSome(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(list: List[JsonLogicValue], minRequired: Int): F[Either[JsonLogicException, JsonLogicValue]] =
          list.traverseFilter(isFieldMissing).map { missingFields =>
            val presentCount = list.length - missingFields.length

            if (presentCount >= minRequired) ArrayValue(Nil).asRight
            else ArrayValue(missingFields).asRight
          }

        args match {
          case ArrayValue(arr) :: Nil                                                   => impl(arr, 1)
          case IntValue(min) :: ArrayValue(arr) :: Nil if min > 0 && min < Int.MaxValue => impl(arr, min.toInt)
          case _ =>
            JsonLogicException(s"Unexpected input for `${MissingSomeOp.tag}' got $args")
              .asLeft[JsonLogicValue]
              .pure[F]
        }
      }

      private def handleIfElseOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        if (args.length < 3 || args.length % 2 == 0) {
          JsonLogicException(s"Unexpected input to `${IfElseOp.tag}` got $args")
            .asLeft[JsonLogicValue]
            .pure[F]
        } else {
          args
            .grouped(2)
            .collectFirst { case List(cond, value) if cond.isTruthy => value }
            .orElse(args.lastOption)
            .toRight(JsonLogicException("failed during if/else evaluation"))
            .pure[F]
        }

      private def handleEqOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(left: JsonLogicValue, right: JsonLogicValue): Either[JsonLogicException, JsonLogicValue] = for {
          lc   <- CoercedValue.coerceToPrimitive(left)
          rc   <- CoercedValue.coerceToPrimitive(right)
          test <- CoercedValue.compareCoercedValues(lc, rc)
        } yield BoolValue(test)

        (args match {
          case l :: r :: Nil => impl(l, r)
          case _             => JsonLogicException(s"Unexpected input for `${EqOp.tag}` got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleEqStrictOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case BoolValue(l) :: BoolValue(r) :: Nil   => l == r
          case StrValue(l) :: StrValue(r) :: Nil     => l == r
          case IntValue(l) :: IntValue(r) :: Nil     => l == r
          case FloatValue(l) :: FloatValue(r) :: Nil => l == r
          case ArrayValue(l) :: ArrayValue(r) :: Nil => l == r
          case MapValue(l) :: MapValue(r) :: Nil     => l == r
          case _                                     => false
        }).asRight[JsonLogicException].map(BoolValue(_): JsonLogicValue).pure[F]

      private def handleNEqOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(left: JsonLogicValue, right: JsonLogicValue): Either[JsonLogicException, JsonLogicValue] = for {
          lc   <- CoercedValue.coerceToPrimitive(left)
          rc   <- CoercedValue.coerceToPrimitive(right)
          test <- CoercedValue.compareCoercedValues(lc, rc)
        } yield BoolValue(!test)

        args match {
          case l :: r :: Nil => impl(l, r).pure[F]
          case _             => JsonLogicException(s"Unexpected input for `${NEqOp.tag}' got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleNEqStrictOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case BoolValue(l) :: BoolValue(r) :: Nil   => l != r
          case StrValue(l) :: StrValue(r) :: Nil     => l != r
          case IntValue(l) :: IntValue(r) :: Nil     => l != r
          case FloatValue(l) :: FloatValue(r) :: Nil => l != r
          case ArrayValue(l) :: ArrayValue(r) :: Nil => l != r
          case MapValue(l) :: MapValue(r) :: Nil     => l != r
          case _                                     => false
        }).asRight[JsonLogicException].map(BoolValue(_): JsonLogicValue).pure[F]

      private def handleNotOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(v: JsonLogicValue): Either[JsonLogicException, JsonLogicValue] =
          BoolValue(!v.isTruthy).asRight[JsonLogicException]

        (args match {
          case v :: Nil => impl(v)
          case _        => JsonLogicException(s"Unexpected input for `${NOp.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleNOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(v: JsonLogicValue): Either[JsonLogicException, JsonLogicValue] =
          BoolValue(v.isTruthy).asRight[JsonLogicException]

        args match {
          case v :: Nil => impl(v).pure[F]
          case _        => JsonLogicException(s"Unexpected input for `${NOp.tag}' got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleOrOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        args
          .pure[F]
          .map(_.isEmpty)
          .ifF(
            (BoolValue(false): JsonLogicValue).asRight[JsonLogicException],
            args.collectFirst { case value if value.isTruthy => value }
              .getOrElse(args.last)
              .asRight[JsonLogicException]
          )

      private def handleAndOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        args
          .foldLeft(BoolValue(true): JsonLogicValue) {
            case (acc, el) =>
              if (!acc.isTruthy) acc else if (!el.isTruthy) el else el
          }
          .asRight[JsonLogicException]
          .pure[F]

      private def handleLt(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

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

        (args match {
          case l :: r :: Nil      => compareTwo(l, r).map(BoolValue(_))
          case a :: b :: c :: Nil => compareThree(a, b, c).map(BoolValue(_))
          case _                  => JsonLogicException(s"Unexpected input for `${Lt.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleLeq(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

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

        (args match {
          case l :: r :: Nil      => compareTwo(l, r).map(BoolValue(_))
          case a :: b :: c :: Nil => compareThree(a, b, c).map(BoolValue(_))
          case _                  => JsonLogicException(s"Unexpected input for `${Leq.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleGt(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        def compareTwo(l: JsonLogicValue, r: JsonLogicValue): Either[JsonLogicException, Boolean] =
          for {
            ln <- promoteToNumeric(l)
            rn <- promoteToNumeric(r)
          } yield compareNumeric(ln, rn) > 0

        (args match {
          case l :: r :: Nil => compareTwo(l, r).map(BoolValue(_))
          case _             => JsonLogicException(s"Unexpected input for `${Gt.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleGeq(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        def compareTwo(l: JsonLogicValue, r: JsonLogicValue): Either[JsonLogicException, Boolean] =
          for {
            ln <- promoteToNumeric(l)
            rn <- promoteToNumeric(r)
          } yield compareNumeric(ln, rn) >= 0

        (args match {
          case l :: r :: Nil => compareTwo(l, r).map(BoolValue(_))
          case _             => JsonLogicException(s"Unexpected input for `${Geq.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleModuloOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case IntValue(_) :: IntValue(r) :: Nil if r == 0 =>
            JsonLogicException("Division by zero in modulo operation").asLeft[JsonLogicValue]
          case FloatValue(_) :: FloatValue(r) :: Nil if r == 0 =>
            JsonLogicException("Division by zero in modulo operation").asLeft[JsonLogicValue]
          case IntValue(_) :: FloatValue(r) :: Nil if r == 0 =>
            JsonLogicException("Division by zero in modulo operation").asLeft[JsonLogicValue]
          case FloatValue(_) :: IntValue(r) :: Nil if r == 0 =>
            JsonLogicException("Division by zero in modulo operation").asLeft[JsonLogicValue]
          case IntValue(l) :: IntValue(r) :: Nil =>
            IntValue(l % r).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil =>
            FloatValue(l % r).asRight
          case IntValue(l) :: FloatValue(r) :: Nil =>
            FloatValue(BigDecimal(l) % r).asRight
          case FloatValue(l) :: IntValue(r) :: Nil =>
            FloatValue(l % BigDecimal(r)).asRight
          case _ => JsonLogicException(s"Unexpected input for `${ModuloOp.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleMaxOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          if (list.isEmpty) {
            JsonLogicException(s"Unexpected input for `${MaxOp.tag}`: list cannot be empty").asLeft
          } else {
            list.traverse(promoteToNumeric).map { numerics =>
              val maxValue = numerics.map(_.toBigDecimal).max
              val hasFloat = numerics.exists(_.isInstanceOf[FloatResult])

              if (!hasFloat && maxValue.isWhole && maxValue.isValidLong) {
                IntValue(maxValue.toBigInt)
              } else {
                FloatValue(maxValue)
              }
            }
          }

        (args match {
          case ArrayValue(arr) :: Nil => impl(arr)
          case _                      => impl(args)
        }).pure[F]
      }

      private def handleMinOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          if (list.isEmpty) {
            JsonLogicException(s"Unexpected input for `${MinOp.tag}`: list cannot be empty").asLeft
          } else {
            list.traverse(promoteToNumeric).map { numerics =>
              val minValue = numerics.map(_.toBigDecimal).min
              val hasFloat = numerics.exists(_.isInstanceOf[FloatResult])

              if (!hasFloat && minValue.isWhole && minValue.isValidLong) {
                IntValue(minValue.toBigInt)
              } else {
                FloatValue(minValue)
              }
            }
          }

        (args match {
          case ArrayValue(arr) :: Nil => impl(arr)
          case _                      => impl(args)
        }).pure[F]
      }

      private def handleAddOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          if (list.isEmpty) {
            JsonLogicException(s"Unexpected input for `${AddOp.tag}`: list cannot be empty").asLeft
          } else if (list.size == 1 && list.head.isInstanceOf[StrValue]) {
            // Special case: single string argument converts to number
            promoteToNumeric(list.head).map(_.toJsonLogicValue)
          } else {
            reduceNumeric(list, _ + _, 0)
          }

        (args match {
          case ArrayValue(arr) :: Nil => impl(arr)
          case _                      => impl(args)
        }).pure[F]
      }

      private def handleTimesOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          if (list.isEmpty) {
            JsonLogicException(s"Unexpected input for `${TimesOp.tag}`: list cannot be empty").asLeft
          } else {
            reduceNumeric(list, _ * _, 1)
          }

        (args match {
          case ArrayValue(arr) :: Nil => impl(arr)
          case _                      => impl(args)
        }).pure[F]
      }

      private def handleMinusOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        (args match {
          case v :: Nil =>
            // Unary negation
            promoteToNumeric(v).map { n =>
              combineNumeric((a, _) => -a)(n, IntResult(0))
            }
          case l :: r :: Nil =>
            // Binary subtraction
            for {
              ln <- promoteToNumeric(l)
              rn <- promoteToNumeric(r)
            } yield combineNumeric(_ - _)(ln, rn)
          case _ =>
            JsonLogicException(s"Unexpected input for `${MinusOp.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleDivOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case IntValue(_) :: IntValue(r) :: Nil if r == 0 =>
            JsonLogicException("Division by zero").asLeft[JsonLogicValue]
          case FloatValue(_) :: FloatValue(r) :: Nil if r == 0 =>
            JsonLogicException("Division by zero").asLeft[JsonLogicValue]
          case IntValue(_) :: FloatValue(r) :: Nil if r == 0 =>
            JsonLogicException("Division by zero").asLeft[JsonLogicValue]
          case FloatValue(_) :: IntValue(r) :: Nil if r == 0 =>
            JsonLogicException("Division by zero").asLeft[JsonLogicValue]
          case IntValue(l) :: IntValue(r) :: Nil =>
            IntValue(l / r).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil =>
            FloatValue(l / r).asRight
          case IntValue(l) :: FloatValue(r) :: Nil =>
            FloatValue(BigDecimal(l) / r).asRight
          case FloatValue(l) :: IntValue(r) :: Nil =>
            FloatValue(l / BigDecimal(r)).asRight
          case _ => JsonLogicException(s"Unexpected input for `${DivOp.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleMergeOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        def impl(arr: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] = {
          val flattened = arr.foldLeft(List.empty[JsonLogicValue]) {
            case (acc, ArrayValue(elems)) => acc ++ elems
            case (acc, elem)              => acc :+ elem
          }
          Right(ArrayValue(flattened))
        }

        args match {
          case maps if maps.forall(_.isInstanceOf[MapValue]) =>
            maps
              .pure[F]
              .map(_.collect { case MapValue(m) => m }.foldLeft(Map.empty[String, JsonLogicValue])(_ ++ _))
              .map(MapValue(_).asRight)
          case ArrayValue(arr) :: Nil => impl(arr).pure[F]
          case other                  => impl(other).pure[F]
        }
      }

      private def handleInOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def strImpl(toFind: JsonLogicPrimitive, str: String): Either[JsonLogicException, JsonLogicValue] = {
          val toFindStr = toFind match {
            case BoolValue(value)  => value.toString
            case IntValue(value)   => value.toString
            case FloatValue(value) => value.toString
            case StrValue(value)   => value
          }

          BoolValue(str.contains(toFindStr)).asRight[JsonLogicException]
        }

        def arrImpl(toFind: JsonLogicValue, arr: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          BoolValue(arr.contains(toFind)).asRight[JsonLogicException]

        args match {
          case NullValue :: _ :: Nil                                => BoolValue(false).asRight[JsonLogicException].pure[F].widen
          case (toFind: JsonLogicPrimitive) :: StrValue(str) :: Nil => strImpl(toFind, str).pure[F]
          case (toFind: JsonLogicValue) :: ArrayValue(arr) :: Nil   => arrImpl(toFind, arr).pure[F]
          case _ => JsonLogicException(s"Unexpected input to `${InOp.tag}` got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleIntersectOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def arrImpl(
          toFind: List[JsonLogicValue],
          arr: List[JsonLogicValue]
        ): Either[JsonLogicException, JsonLogicValue] =
          BoolValue(toFind.forall(arr.contains)).asRight[JsonLogicException]

        args match {
          case NullValue :: _ :: Nil                        => BoolValue(false).asRight[JsonLogicException].pure[F].widen
          case ArrayValue(toFind) :: ArrayValue(arr) :: Nil => arrImpl(toFind, arr).pure[F]
          case _ =>
            JsonLogicException(s"Unexpected input to `${IntersectOp.tag}` got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleCatOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        args.traverse {
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
          .map(argStrings => StrValue(argStrings.mkString): JsonLogicValue)
          .pure[F]

      private def handleSubstrOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(str: String, start: Int, length: Int): Either[JsonLogicException, JsonLogicValue] =
          for {
            s <- Option(str).toRight(JsonLogicException("substr expects a non-null string"))
            strLen = s.length
            rawStart = if (start < 0) strLen + start else start
            startIdx = Math.max(0, Math.min(rawStart, strLen))
            endIdx = if (length >= 0) Math.min(startIdx + length, strLen) else Math.max(0, strLen + length)
            substr = if (startIdx >= strLen || endIdx <= startIdx) "" else s.substring(startIdx, endIdx)
          } yield StrValue(substr)

        (args match {
          case StrValue(str) :: IntValue(start) :: Nil                     => impl(str, start.toInt, str.length)
          case StrValue(str) :: IntValue(start) :: IntValue(length) :: Nil => impl(str, start.toInt, length.toInt)
          case _ => JsonLogicException(s"Unexpected input to `${SubStrOp.tag}` got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleMapOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(arr: List[JsonLogicValue], expr: JsonLogicExpression): F[Either[JsonLogicException, JsonLogicValue]] =
          arr
            .traverse(el => evaluationStrategy(expr, el.some))
            .map(_.sequence.map(ArrayValue(_)))

        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${MapOp.tag}, got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleFilterOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, JsonLogicValue]] = (for {
          listOpts <- EitherT(
            arr.traverse { el =>
              evaluationStrategy(expr, el.some)
                .map(_.map { r =>
                  Option.when(r.isTruthy)(el)
                })
            }
              .map(_.sequence)
          )
        } yield ArrayValue(listOpts.collect { case Some(e) => e }): JsonLogicValue).value

        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${FilterOp.tag}").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleReduceOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression,
          init: JsonLogicValue
        ): F[Either[JsonLogicException, JsonLogicValue]] =
          arr.foldLeftM(init.asRight[JsonLogicException]) { (accEither, item) =>
            accEither match {
              case Left(err) => err.asLeft[JsonLogicValue].pure[F]
              case Right(accValue) =>
                val ctx = MapValue(Map("current" -> item, "accumulator" -> accValue))
                evaluationStrategy(expr, ctx.some)
            }
          }

        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil =>
            if (arr.isEmpty) {
              JsonLogicException(s"Cannot reduce empty array without initial value").asLeft[JsonLogicValue].pure[F]
            } else {
              impl(arr.tail, expr, arr.head)
            }
          case ArrayValue(arr) :: FunctionValue(expr) :: (init: JsonLogicPrimitive) :: Nil => impl(arr, expr, init)
          case _ => JsonLogicException(s"Unexpected input to ${ReduceOp.tag}").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleAllOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, JsonLogicValue]] = (for {
          listBools <- EitherT(
            arr
              .traverse(el => evaluationStrategy(expr, el.some).map(_.map(_.isTruthy)))
              .map(_.sequence)
          )
        } yield BoolValue(listBools.forall(identity)): JsonLogicValue).value

        args match {
          case NullValue :: FunctionValue(_) :: Nil          => BoolValue(false).asRight[JsonLogicException].pure[F].widen
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${AllOp.tag}, got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleNoneOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, JsonLogicValue]] = (for {
          listBools <- EitherT(
            arr
              .traverse(el => evaluationStrategy(expr, el.some).map(_.map(_.isTruthy)))
              .map(_.sequence)
          )
        } yield BoolValue(listBools.forall(x => !x)): JsonLogicValue).value

        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${NoneOp.tag}").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleSomeOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression,
          threshold: Int
        ): F[Either[JsonLogicException, JsonLogicValue]] =
          (for {
            listBools <- EitherT(
              arr
                .traverse(el => evaluationStrategy(expr, el.some).map(_.map(_.isTruthy)))
                .map(_.sequence)
            )
            test = listBools.count(identity) >= threshold
          } yield BoolValue(test): JsonLogicValue).value

        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil                  => impl(arr, expr, 1)
          case ArrayValue(arr) :: FunctionValue(expr) :: IntValue(min) :: Nil => impl(arr, expr, min.toInt)
          case _ => JsonLogicException(s"Unexpected input to ${SomeOp.tag}").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleMapValuesOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case NullValue :: Nil   => NullValue.asRight
          case MapValue(v) :: Nil => ArrayValue(v.values.toList).asRight
          case _                  => JsonLogicException(s"Unexpected input for `${MapValuesOp.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleMapKeysOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case NullValue :: Nil   => NullValue.asRight
          case MapValue(v) :: Nil => ArrayValue(v.keys.map(StrValue(_)).toList).asRight
          case _                  => JsonLogicException(s"Unexpected input for `${MapKeysOp.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleGetOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def implMap(
          map: Map[String, JsonLogicValue],
          key: String
        ): Either[JsonLogicException, JsonLogicValue] =
          Either.fromOption(map.get(key), JsonLogicException(s"Could not find key $key in the provided map $map"))

        (args match {
          case MapValue(v) :: StrValue(k) :: Nil => implMap(v, k)
          case _ => JsonLogicException(s"Unexpected input to ${GetOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleCountOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def countSimple(arr: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          IntValue(arr.length).asRight[JsonLogicException]

        def countWithPredicate(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, JsonLogicValue]] = (for {
          listBools <- EitherT(
            arr
              .traverse(el => evaluationStrategy(expr, el.some).map(_.map(_.isTruthy)))
              .map(_.sequence)
          )
          count = listBools.count(identity)
        } yield IntValue(count): JsonLogicValue).value

        args match {
          case ArrayValue(arr) :: Nil                        => countSimple(arr).pure[F]
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => countWithPredicate(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${CountOp.tag}, got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleLengthOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case ArrayValue(arr) :: Nil => IntValue(arr.length).asRight[JsonLogicException]
          case StrValue(str) :: Nil   => IntValue(str.length).asRight[JsonLogicException]
          case _                      => JsonLogicException(s"Unexpected input to ${LengthOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleFindOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(
          arr: List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, JsonLogicValue]] =
          arr
            .foldLeftM[F, Option[JsonLogicValue]](None) {
              case (acc @ Some(_), _) => (acc: Option[JsonLogicValue]).pure[F]
              case (None, el) =>
                evaluationStrategy(expr, el.some).map {
                  case Right(value) if value.isTruthy => Some(el)
                  case _                              => None
                }
            }
            .map {
              case Some(value) => value.asRight[JsonLogicException]
              case None        => NullValue.asRight[JsonLogicException]
            }

        args match {
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${FindOp.tag}, got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleLowerOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case StrValue(str) :: Nil => StrValue(str.toLowerCase).asRight[JsonLogicException]
          case _                    => JsonLogicException(s"Unexpected input to ${LowerOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleUpperOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case StrValue(str) :: Nil => StrValue(str.toUpperCase).asRight[JsonLogicException]
          case _                    => JsonLogicException(s"Unexpected input to ${UpperOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleJoinOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

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

        (args match {
          case ArrayValue(arr) :: StrValue(separator) :: Nil =>
            StrValue(arr.map(arrayToString).mkString(separator)).asRight[JsonLogicException]
          case _ => JsonLogicException(s"Unexpected input to ${JoinOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleSplitOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case StrValue(str) :: StrValue(separator) :: Nil =>
            ArrayValue(str.split(java.util.regex.Pattern.quote(separator), -1).map(StrValue(_)).toList)
              .asRight[JsonLogicException]
          case _ => JsonLogicException(s"Unexpected input to ${SplitOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleDefaultOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        args.collectFirst {
          case v if v != NullValue && v.isTruthy => v
        }
          .getOrElse(NullValue)
          .asRight[JsonLogicException]
          .pure[F]

      private def handleUniqueOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case ArrayValue(arr) :: Nil => ArrayValue(arr.distinct).asRight[JsonLogicException]
          case _                      => JsonLogicException(s"Unexpected input to ${UniqueOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleSliceOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case ArrayValue(arr) :: IntValue(start) :: Nil =>
            val startIdx = if (start < 0) Math.max(0, arr.length + start.toInt) else start.toInt
            ArrayValue(arr.drop(startIdx)).asRight[JsonLogicException]
          case ArrayValue(arr) :: IntValue(start) :: IntValue(end) :: Nil =>
            val startIdx = if (start < 0) Math.max(0, arr.length + start.toInt) else start.toInt
            val endIdx = if (end < 0) Math.max(0, arr.length + end.toInt) else end.toInt
            ArrayValue(arr.slice(startIdx, endIdx)).asRight[JsonLogicException]
          case _ => JsonLogicException(s"Unexpected input to ${SliceOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleReverseOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case ArrayValue(arr) :: Nil => ArrayValue(arr.reverse).asRight[JsonLogicException]
          case _                      => JsonLogicException(s"Unexpected input to ${ReverseOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleFlattenOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case ArrayValue(arr) :: Nil =>
            val flattened = arr.flatMap {
              case ArrayValue(inner) => inner
              case other             => List(other)
            }
            ArrayValue(flattened).asRight[JsonLogicException]
          case _ => JsonLogicException(s"Unexpected input to ${FlattenOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleTrimOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case StrValue(str) :: Nil => StrValue(str.trim).asRight[JsonLogicException]
          case _                    => JsonLogicException(s"Unexpected input to ${TrimOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleStartsWithOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case StrValue(str) :: StrValue(prefix) :: Nil => BoolValue(str.startsWith(prefix)).asRight[JsonLogicException]
          case _ => JsonLogicException(s"Unexpected input to ${StartsWithOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleEndsWithOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case StrValue(str) :: StrValue(suffix) :: Nil => BoolValue(str.endsWith(suffix)).asRight[JsonLogicException]
          case _ => JsonLogicException(s"Unexpected input to ${EndsWithOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleAbsOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        (args match {
          case IntValue(v) :: Nil   => IntValue(v.abs).asRight[JsonLogicException]
          case FloatValue(v) :: Nil => FloatValue(v.abs).asRight[JsonLogicException]
          case v :: Nil =>
            promoteToNumeric(v).map {
              case IntResult(n)   => IntValue(n.abs)
              case FloatResult(n) => FloatValue(n.abs)
            }
          case _ => JsonLogicException(s"Unexpected input to ${AbsOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleRoundOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        (args match {
          case IntValue(v) :: Nil => IntValue(v).asRight[JsonLogicException]
          case FloatValue(v) :: Nil =>
            val rounded = v.setScale(0, BigDecimal.RoundingMode.HALF_UP)
            if (rounded.isValidLong) IntValue(rounded.toBigInt).asRight
            else FloatValue(rounded).asRight
          case v :: Nil =>
            promoteToNumeric(v).map {
              case IntResult(n) => IntValue(n)
              case FloatResult(n) =>
                val rounded = n.setScale(0, BigDecimal.RoundingMode.HALF_UP)
                if (rounded.isValidLong) IntValue(rounded.toBigInt) else FloatValue(rounded)
            }
          case _ => JsonLogicException(s"Unexpected input to ${RoundOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleFloorOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        (args match {
          case IntValue(v) :: Nil => IntValue(v).asRight[JsonLogicException]
          case FloatValue(v) :: Nil =>
            val floored = v.setScale(0, BigDecimal.RoundingMode.FLOOR)
            if (floored.isValidLong) IntValue(floored.toBigInt).asRight
            else FloatValue(floored).asRight
          case v :: Nil =>
            promoteToNumeric(v).map {
              case IntResult(n) => IntValue(n)
              case FloatResult(n) =>
                val floored = n.setScale(0, BigDecimal.RoundingMode.FLOOR)
                if (floored.isValidLong) IntValue(floored.toBigInt) else FloatValue(floored)
            }
          case _ => JsonLogicException(s"Unexpected input to ${FloorOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleCeilOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        (args match {
          case IntValue(v) :: Nil => IntValue(v).asRight[JsonLogicException]
          case FloatValue(v) :: Nil =>
            val ceiled = v.setScale(0, BigDecimal.RoundingMode.CEILING)
            if (ceiled.isValidLong) IntValue(ceiled.toBigInt).asRight
            else FloatValue(ceiled).asRight
          case v :: Nil =>
            promoteToNumeric(v).map {
              case IntResult(n) => IntValue(n)
              case FloatResult(n) =>
                val ceiled = n.setScale(0, BigDecimal.RoundingMode.CEILING)
                if (ceiled.isValidLong) IntValue(ceiled.toBigInt) else FloatValue(ceiled)
            }
          case _ => JsonLogicException(s"Unexpected input to ${CeilOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handlePowOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {
        import NumericOps._

        val maxSafeExponent = 999

        (args match {
          case IntValue(base) :: IntValue(exp) :: Nil if exp >= 0 && exp.isValidInt && exp <= maxSafeExponent =>
            IntValue(base.pow(exp.toInt)).asRight[JsonLogicException]
          case IntValue(_) :: IntValue(exp) :: Nil if exp > maxSafeExponent =>
            JsonLogicException(
              s"Exponent $exp exceeds maximum safe value $maxSafeExponent for integer exponentiation"
            ).asLeft[JsonLogicValue]
          case base :: exp :: Nil =>
            for {
              baseNum <- promoteToNumeric(base)
              expNum  <- promoteToNumeric(exp)
              result <- {
                val expDouble = expNum.toBigDecimal.toDouble
                if (expDouble.abs > maxSafeExponent) {
                  JsonLogicException(
                    s"Exponent magnitude ${expDouble.abs} exceeds maximum safe value $maxSafeExponent"
                  ).asLeft[JsonLogicValue]
                } else {
                  val powResult = Math.pow(baseNum.toBigDecimal.toDouble, expDouble)
                  if (powResult.isInfinity) {
                    JsonLogicException(s"Power operation resulted in infinity").asLeft[JsonLogicValue]
                  } else if (powResult.isNaN) {
                    JsonLogicException(s"Power operation resulted in NaN").asLeft[JsonLogicValue]
                  } else if (powResult.isWhole && powResult.isValidInt) {
                    IntValue(BigInt(powResult.toInt)).asRight[JsonLogicException]
                  } else {
                    FloatValue(BigDecimal(powResult)).asRight[JsonLogicException]
                  }
                }
              }
            } yield result
          case _ => JsonLogicException(s"Unexpected input to ${PowOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]
      }

      private def handleHasOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case MapValue(m) :: StrValue(key) :: Nil => BoolValue(m.contains(key)).asRight[JsonLogicException]
          case _ => JsonLogicException(s"Unexpected input to ${HasOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleEntriesOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case MapValue(m) :: Nil =>
            val entries = m.toList.map { case (k, v) => ArrayValue(List(StrValue(k), v)) }
            ArrayValue(entries).asRight[JsonLogicException]
          case _ => JsonLogicException(s"Unexpected input to ${EntriesOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleTypeOfOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case value :: Nil => StrValue(value.tag).asRight[JsonLogicException]
          case _            => JsonLogicException(s"Unexpected input to ${TypeOfOp.tag}, got $args").asLeft[JsonLogicValue]
        }).pure[F]
    }

  implicit class semanticOps[F[_]: Monad](sem: JsonLogicSemantics[F]) {

    def evaluateWith(
      program: JsonLogicExpression,
      ctx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, JsonLogicValue]] =
      JsonLogicRuntime.evaluate(program, ctx)(Monad[F], sem)
  }
}
