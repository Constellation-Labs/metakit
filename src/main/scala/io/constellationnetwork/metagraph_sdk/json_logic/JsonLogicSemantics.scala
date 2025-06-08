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
    vars:               JsonLogicValue,
    evaluationStrategy: EvaluationCallback[F]
  ): JsonLogicSemantics[F] =
    new JsonLogicSemantics[F] {

      override def getVar(
        key: String,
        ctx: Option[JsonLogicValue] = None
      ): F[Either[JsonLogicException, JsonLogicValue]] = {

        def combineState(
          base:   JsonLogicValue,
          extOpt: Option[JsonLogicValue]
        ): Either[JsonLogicException, JsonLogicValue] = (base, extOpt) match {
          case (v, None)                            => v.asRight[JsonLogicException]
          case (_, Some(NullValue))                 => base.asRight[JsonLogicException]
          case (_, Some(_: JsonLogicPrimitive))     => base.asRight[JsonLogicException]
          case (ArrayValue(l), Some(ArrayValue(r))) => ArrayValue(l ++ r).asRight
          case (MapValue(l), Some(MapValue(r)))     => MapValue(l ++ r).asRight
          case _ => JsonLogicException("Failed to combine semantic state and runtime context").asLeft
        }

        def getChild(
          parent:  JsonLogicValue,
          segment: String
        ): Either[JsonLogicException, JsonLogicValue] = parent match {
          case ArrayValue(elements) =>
            segment.toLongOption match {
              case Some(idx) =>
                elements.get(idx) match {
                  case Some(child) => child.asRight
                  case None =>
                    JsonLogicException(s"Variable '$key' not found (array index $segment out of range)").asLeft
                }
              case None => JsonLogicException(s"Segment '$segment' is not a valid array index").asLeft
            }

          case MapValue(m) =>
            m.get(segment) match {
              case Some(child) => child.asRight[JsonLogicException]
              case None        => JsonLogicException(s"Variable '$key' not found (map key '$segment' missing)").asLeft
            }

          case _ =>
            JsonLogicException(s"Found a non-map/array while traversing '$segment' in dot-notation for key '$key'")
              .asLeft[JsonLogicValue]
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
          case ExistsOp      => handleMissingNone
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
        }

      /** If 'key' is missing then Some('key') is returned, else if found return None */
      private def isFieldMissing(field: JsonLogicValue): F[Option[JsonLogicValue]] = field match {
        case v @ StrValue(key)   => getVar(key).map(res => if (res.isLeft) v.some else None)
        case v @ IntValue(key)   => getVar(key.toString).map(res => if (res.isLeft) v.some else None)
        case v @ FloatValue(key) => getVar(key.toString).map(res => if (res.isLeft) v.some else None)
        case v                   => v.some.pure[F]
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

      private def handleMissingSome(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(list: List[JsonLogicValue], minRequired: Int): F[Either[JsonLogicException, JsonLogicValue]] =
          list.traverseFilter(isFieldMissing).map { missingFields =>
            val presentCount = list.length - missingFields.length

            if (presentCount >= minRequired) ArrayValue(Nil).asRight
            else ArrayValue(missingFields).asRight
          }

        args match {
          case ArrayValue(arr) :: Nil                                                     => impl(arr, 1)
          case IntValue(min) :: ArrayValue(arr) :: Nil if (min > 0 && min < Int.MaxValue) => impl(arr, min.toInt)
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
          case _ => JsonLogicException(s"Unexpected input for `${EqOp.tag}` got $args").asLeft[JsonLogicValue]
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
          case _ => JsonLogicException(s"Unexpected input for `${NEqOp.tag}' got $args").asLeft[JsonLogicValue].pure[F]
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
          case _ => JsonLogicException(s"Unexpected input for `${NOp.tag}' got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleOrOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        args
          .pure[F]
          .map(_.isEmpty)
          .ifF(
            (BoolValue(false): JsonLogicValue).asRight[JsonLogicException],
            args
              .collectFirst { case value if value.isTruthy => value }
              .getOrElse(args.last)
              .asRight[JsonLogicException]
          )

      private def handleAndOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        args
          .foldLeft(BoolValue(true): JsonLogicValue) { case (acc, el) =>
            if (!acc.isTruthy) acc else if (!el.isTruthy) el else el
          }
          .asRight[JsonLogicException]
          .pure[F]

      private def handleLt(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case NullValue :: NullValue :: Nil                          => BoolValue(false).asRight
          case NullValue :: IntValue(i) :: Nil                        => BoolValue(0 < i).asRight
          case IntValue(i) :: NullValue :: Nil                        => BoolValue(i < 0).asRight
          case NullValue :: FloatValue(f) :: Nil                      => BoolValue(0 < f).asRight
          case FloatValue(f) :: NullValue :: Nil                      => BoolValue(f < 0).asRight
          case IntValue(l) :: IntValue(r) :: Nil                      => BoolValue(l < r).asRight
          case IntValue(r) :: IntValue(g) :: IntValue(b) :: Nil       => BoolValue(r < g && g < b).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil                  => BoolValue(l < r).asRight
          case FloatValue(r) :: FloatValue(g) :: FloatValue(b) :: Nil => BoolValue(r < g && g < b).asRight
          case _ => JsonLogicException(s"Unexpected input for `${Lt.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleLeq(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case NullValue :: NullValue :: Nil                          => BoolValue(true).asRight
          case NullValue :: IntValue(i) :: Nil                        => BoolValue(0 <= i).asRight
          case IntValue(i) :: NullValue :: Nil                        => BoolValue(i <= 0).asRight
          case NullValue :: FloatValue(f) :: Nil                      => BoolValue(0 <= f).asRight
          case FloatValue(f) :: NullValue :: Nil                      => BoolValue(f <= 0).asRight
          case IntValue(l) :: IntValue(r) :: Nil                      => BoolValue(l <= r).asRight
          case IntValue(r) :: IntValue(g) :: IntValue(b) :: Nil       => BoolValue(r <= g && g <= b).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil                  => BoolValue(l <= r).asRight
          case FloatValue(r) :: FloatValue(g) :: FloatValue(b) :: Nil => BoolValue(r <= g && g <= b).asRight
          case _ => JsonLogicException(s"Unexpected input for `${Leq.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleGt(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case NullValue :: NullValue :: Nil         => BoolValue(false).asRight
          case NullValue :: IntValue(i) :: Nil       => BoolValue(0 > i).asRight
          case IntValue(i) :: NullValue :: Nil       => BoolValue(i > 0).asRight
          case NullValue :: FloatValue(f) :: Nil     => BoolValue(0 > f).asRight
          case FloatValue(f) :: NullValue :: Nil     => BoolValue(f > 0).asRight
          case IntValue(l) :: IntValue(r) :: Nil     => BoolValue(l > r).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil => BoolValue(l > r).asRight
          case _ => JsonLogicException(s"Unexpected input for `${Gt.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleGeq(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case NullValue :: NullValue :: Nil         => BoolValue(true).asRight
          case NullValue :: IntValue(i) :: Nil       => BoolValue(0 >= i).asRight
          case IntValue(i) :: NullValue :: Nil       => BoolValue(i >= 0).asRight
          case NullValue :: FloatValue(f) :: Nil     => BoolValue(0 >= f).asRight
          case FloatValue(f) :: NullValue :: Nil     => BoolValue(f >= 0).asRight
          case IntValue(l) :: IntValue(r) :: Nil     => BoolValue(l >= r).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil => BoolValue(l >= r).asRight
          case _ => JsonLogicException(s"Unexpected input for `${Geq.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleModuloOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case IntValue(l) :: IntValue(r) :: Nil if (l.toInt == 0 || r.toInt == 0) =>
            new JsonLogicException("Division by zero in modulo operation").asLeft[JsonLogicValue]
          case FloatValue(l) :: FloatValue(r) :: Nil if (l.toInt == 0 || r.toInt == 0) =>
            new JsonLogicException("Division by zero in modulo operation").asLeft[JsonLogicValue]
          case IntValue(l) :: IntValue(r) :: Nil     => IntValue(l % r).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil => FloatValue(l % r).asRight
          case _ => JsonLogicException(s"Unexpected input for `${ModuloOp.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleMaxOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          list match {
            case Nil => JsonLogicException(s"Unexpected input for `${MaxOp.tag}`: list cannot be empty").asLeft

            case intValues @ (_: IntValue) :: _ if list.forall(_.isInstanceOf[IntValue]) =>
              IntValue(intValues.collect { case IntValue(i) => i }.max).asRight

            case floatValues @ (_: FloatValue) :: _ if list.forall(_.isInstanceOf[FloatValue]) =>
              FloatValue(floatValues.collect { case FloatValue(f) => f }.max).asRight

            case _ =>
              JsonLogicException(
                s"Unexpected input for `${MaxOp.tag}`: all arguments must be IntValue or all FloatValue"
              ).asLeft
          }

        args match {
          case ArrayValue(arr) :: Nil => impl(arr).pure[F]
          case _                      => impl(args).pure[F]
        }
      }

      private def handleMinOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          list match {
            case Nil => JsonLogicException(s"Unexpected input for `${MinOp.tag}`: list cannot be empty").asLeft

            case intValues @ (_: IntValue) :: _ if list.forall(_.isInstanceOf[IntValue]) =>
              IntValue(intValues.collect { case IntValue(i) => i }.min).asRight

            case floatValues @ (_: FloatValue) :: _ if list.forall(_.isInstanceOf[FloatValue]) =>
              FloatValue(floatValues.collect { case FloatValue(f) => f }.min).asRight

            case _ =>
              JsonLogicException(
                s"Unexpected input for `${MinOp.tag}`: all arguments must be IntValue or all FloatValue"
              ).asLeft
          }

        args match {
          case ArrayValue(arr) :: Nil => impl(arr).pure[F]
          case _                      => impl(args).pure[F]
        }
      }

      private def handleAddOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def implStr(str: String): Either[JsonLogicException, JsonLogicValue] =
          Option(BigDecimal(str)) match {
            case Some(num) =>
              if (num.isValidInt) IntValue(num.toBigIntExact.get).asRight
              else if (num.isValidLong) IntValue(num.toBigIntExact.get).asRight
              else FloatValue(num).asRight
            case None => JsonLogicException(s"Failed to decode number as IntValue or FloatValue").asLeft
          }

        def implList(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
          list match {
            case Nil => JsonLogicException(s"Unexpected input for `${AddOp.tag}`: list cannot be empty").asLeft

            case NullValue :: Nil                     => IntValue(0).asRight
            case NullValue :: NullValue :: Nil        => IntValue(0).asRight
            case (int: IntValue) :: NullValue :: Nil  => int.asRight
            case (fl: FloatValue) :: NullValue :: Nil => fl.asRight
            case NullValue :: (int: IntValue) :: Nil  => int.asRight
            case NullValue :: (fl: FloatValue) :: Nil => fl.asRight

            case intValues @ (_: IntValue) :: _ if list.forall(_.isInstanceOf[IntValue]) =>
              IntValue(intValues.collect { case IntValue(i) => i }.sum).asRight

            case floatValues @ (_: FloatValue) :: _ if list.forall(_.isInstanceOf[FloatValue]) =>
              FloatValue(floatValues.collect { case FloatValue(f) => f }.sum).asRight

            case _ =>
              JsonLogicException(
                s"Unexpected input for `${AddOp.tag}`: all arguments must be IntValue or all FloatValue, got $list"
              ).asLeft
          }

        args match {
          case StrValue(str) :: Nil   => implStr(str).pure[F]
          case ArrayValue(arr) :: Nil => implList(arr).pure[F]
          case _                      => implList(args).pure[F]
        }
      }

      private def handleTimesOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(list: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] = list match {
          case Nil =>
            JsonLogicException(s"Unexpected input for `${TimesOp.tag}`: list cannot be empty").asLeft

          case NullValue :: NullValue :: Nil     => IntValue(0).asRight
          case IntValue(_) :: NullValue :: Nil   => IntValue(0).asRight
          case NullValue :: IntValue(_) :: Nil   => IntValue(0).asRight
          case FloatValue(_) :: NullValue :: Nil => FloatValue(0.0).asRight
          case NullValue :: FloatValue(_) :: Nil => FloatValue(0.0).asRight

          case intValues @ (_: IntValue) :: _ if list.forall(_.isInstanceOf[IntValue]) =>
            IntValue(intValues.collect { case IntValue(i) => i }.product).asRight

          case floatValues @ (_: FloatValue) :: _ if list.forall(_.isInstanceOf[FloatValue]) =>
            FloatValue(floatValues.collect { case FloatValue(f) => f }.product).asRight

          case _ if list.forall(v => v.isInstanceOf[IntValue] || v.isInstanceOf[FloatValue]) =>
            FloatValue(
              list.collect {
                case iv: IntValue   => iv.asFloatValue.value
                case fv: FloatValue => fv.value
              }.product
            ).asRight

          case _ =>
            JsonLogicException(
              s"Unexpected input for `${TimesOp.tag}`: all arguments must be IntValue or all FloatValue. Got $list"
            ).asLeft
        }

        args match {
          case ArrayValue(arr) :: Nil => impl(arr).pure[F]
          case _                      => impl(args).pure[F]
        }
      }

      private def handleMinusOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case NullValue :: NullValue :: Nil         => IntValue(0).asRight
          case (int: IntValue) :: NullValue :: Nil   => int.asRight
          case (fl: FloatValue) :: NullValue :: Nil  => fl.asRight
          case NullValue :: IntValue(i) :: Nil       => IntValue(0 - i).asRight
          case NullValue :: FloatValue(f) :: Nil     => FloatValue(0.0 - f).asRight
          case IntValue(i) :: Nil                    => IntValue(-i).asRight
          case IntValue(l) :: IntValue(r) :: Nil     => IntValue(l - r).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil => FloatValue(l - r).asRight
          case _ =>
            JsonLogicException(s"Unexpected input for `${MinusOp.tag}' got $args")
              .asLeft[JsonLogicValue]
        }).pure[F]

      private def handleDivOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case IntValue(l) :: IntValue(r) :: Nil if (l.toInt == 0 || r.toInt == 0) =>
            new JsonLogicException("Division by zero").asLeft[JsonLogicValue]
          case FloatValue(l) :: FloatValue(r) :: Nil if (l.toInt == 0 || r.toInt == 0) =>
            FloatValue(l % r).asRight
            new JsonLogicException("Division by zero").asLeft[JsonLogicValue]
          case IntValue(l) :: IntValue(r) :: Nil     => IntValue(l / r).asRight
          case FloatValue(l) :: FloatValue(r) :: Nil => FloatValue(l / r).asRight
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
          case NullValue :: _ :: Nil => BoolValue(false).asRight[JsonLogicException].pure[F].widen
          case (toFind: JsonLogicPrimitive) :: StrValue(str) :: Nil => strImpl(toFind, str).pure[F]
          case (toFind: JsonLogicValue) :: ArrayValue(arr) :: Nil   => arrImpl(toFind, arr).pure[F]
          case _ => JsonLogicException(s"Unexpected input to `${InOp.tag}` got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleIntersectOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def arrImpl(
          toFind: List[JsonLogicValue],
          arr:    List[JsonLogicValue]
        ): Either[JsonLogicException, JsonLogicValue] =
          BoolValue(toFind.forall(arr.contains)).asRight[JsonLogicException]

        args match {
          case NullValue :: _ :: Nil => BoolValue(false).asRight[JsonLogicException].pure[F].widen
          case ArrayValue(toFind) :: ArrayValue(arr) :: Nil => arrImpl(toFind, arr).pure[F]
          case _ =>
            JsonLogicException(s"Unexpected input to `${IntersectOp.tag}` got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleCatOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        args
          .traverse {
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
          arr:  List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, JsonLogicValue]] = (for {
          listOpts <- EitherT(
            arr
              .traverse { el =>
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
          arr:  List[JsonLogicValue],
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
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr, arr.head.getDefault)
          case ArrayValue(arr) :: FunctionValue(expr) :: (init: JsonLogicPrimitive) :: Nil => impl(arr, expr, init)
          case _ => JsonLogicException(s"Unexpected input to ${ReduceOp.tag}").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleAllOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(
          arr:  List[JsonLogicValue],
          expr: JsonLogicExpression
        ): F[Either[JsonLogicException, JsonLogicValue]] = (for {
          listBools <- EitherT(
            arr
              .traverse(el => evaluationStrategy(expr, el.some).map(_.map(_.isTruthy)))
              .map(_.sequence)
          )
        } yield BoolValue(listBools.forall(identity)): JsonLogicValue).value

        args match {
          case NullValue :: FunctionValue(_) :: Nil => BoolValue(false).asRight[JsonLogicException].pure[F].widen
          case ArrayValue(arr) :: FunctionValue(expr) :: Nil => impl(arr, expr)
          case _ => JsonLogicException(s"Unexpected input to ${AllOp.tag}, got $args").asLeft[JsonLogicValue].pure[F]
        }
      }

      private def handleNoneOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] = {

        def impl(
          arr:  List[JsonLogicValue],
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
          arr:       List[JsonLogicValue],
          expr:      JsonLogicExpression,
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
          case _ => JsonLogicException(s"Unexpected input for `${MapValuesOp.tag}' got $args").asLeft[JsonLogicValue]
        }).pure[F]

      private def handleMapKeysOp(args: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
        (args match {
          case NullValue :: Nil   => NullValue.asRight
          case MapValue(v) :: Nil => ArrayValue(v.keys.map(StrValue(_)).toList).asRight
          case _ => JsonLogicException(s"Unexpected input for `${MapKeysOp.tag}' got $args").asLeft[JsonLogicValue]
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
    }

  implicit class semanticOps[F[_]: Monad](sem: JsonLogicSemantics[F]) {

    def evaluateWith(
      program: JsonLogicExpression,
      ctx:     Option[JsonLogicValue]
    ): F[Either[JsonLogicException, JsonLogicValue]] =
      JsonLogicRuntime.evaluate(program, ctx)(Monad[F], sem)
  }
}
