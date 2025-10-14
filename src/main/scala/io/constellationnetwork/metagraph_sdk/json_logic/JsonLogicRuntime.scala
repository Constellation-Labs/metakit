package io.constellationnetwork.metagraph_sdk.json_logic

import cats.Monad
import cats.data.EitherT
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.JsonLogicOp._

object JsonLogicRuntime {

  private val callbackOps: Set[JsonLogicOp] = Set(MapOp, FilterOp, AllOp, SomeOp, NoneOp, CountOp, FindOp)

  trait EvaluationStrategy {

    def apply[F[_]: Monad: JsonLogicSemantics](
      expr: JsonLogicExpression,
      ctx: Option[JsonLogicValue] = None
    ): F[Either[JsonLogicException, JsonLogicValue]]
  }

  object RecursiveStrategy extends EvaluationStrategy {

    override def apply[F[_]: Monad: JsonLogicSemantics](
      expr: JsonLogicExpression,
      ctx: Option[JsonLogicValue] = None
    ): F[Either[JsonLogicException, JsonLogicValue]] = evaluate(expr, ctx)
  }

  object TailRecursiveStrategy extends EvaluationStrategy {

    override def apply[F[_]: Monad: JsonLogicSemantics](
      expr: JsonLogicExpression,
      ctx: Option[JsonLogicValue] = None
    ): F[Either[JsonLogicException, JsonLogicValue]] = interpret(expr, ctx)
  }

  /*
    Accessor method to look up a variable in the "vars" within of a given JsonLogicSemantics, optionally, a ctx can
    be provided to extend the current scope of evaluation
   */
  private def lookupVar[F[_]: Monad: JsonLogicSemantics](key: String, defaultOpt: Option[JsonLogicValue])(
    ctx: Option[JsonLogicValue] = None
  ): F[Either[JsonLogicException, JsonLogicValue]] =
    JsonLogicSemantics[F]
      .getVar(key, ctx)
      .map {
        case Right(NullValue) if key.nonEmpty =>
          defaultOpt match {
            case Some(d) => d.asRight[JsonLogicException]
            case None    => NullValue.asRight[JsonLogicException]
          }
        case Right(value) =>
          value.asRight[JsonLogicException]
        case Left(_) =>
          defaultOpt match {
            case Some(d) => d.asRight[JsonLogicException]
            case None    => NullValue.asRight[JsonLogicException]
          }
      }

  def evaluate[F[_]: Monad: JsonLogicSemantics](
    expr: JsonLogicExpression,
    ctx: Option[JsonLogicValue] = None
  ): F[Either[JsonLogicException, JsonLogicValue]] = {

    def run: JsonLogicExpression => F[Either[JsonLogicException, JsonLogicValue]] = {
      case ConstExpression(v)                   => Monad[F].pure(Right(v))
      case VarExpression(Left(key), defaultOpt) => lookupVar(key, defaultOpt)(ctx)
      case VarExpression(Right(expr), defaultOpt) =>
        run(expr).flatMap {
          case Right(StrValue(name))                  => lookupVar(name, defaultOpt)(ctx)
          case Right(ArrayValue(StrValue(name) :: _)) => lookupVar(name, defaultOpt)(ctx)
          case Right(v)                               => JsonLogicException(s"Got non-string input: $v").asLeft[JsonLogicValue].pure[F]
          case Left(ex)                               => ex.asLeft[JsonLogicValue].pure[F]
        }
      case ArrayExpression(args) => args.traverse(run).map(_.sequence.map(ArrayValue(_)))
      case MapExpression(map) =>
        map.toList.traverse { case (k, expr) => run(expr).map(_.map(k -> _)) }.map(_.sequence.map(pairs => MapValue(pairs.toMap)))
      case ApplyExpression(ReduceOp, List(arr, cb: JsonLogicExpression, init)) =>
        callbackFunc(ReduceOp, arr, cb, init.some)
      case ApplyExpression(op, List(arr, cb: JsonLogicExpression)) if callbackOps.contains(op) =>
        callbackFunc(op, arr, cb)
      case ApplyExpression(CountOp, List(arr)) =>
        run(arr).flatMap {
          case Right(v) => JsonLogicSemantics[F].applyOp(CountOp)(List(v))
          case Left(e)  => e.asLeft[JsonLogicValue].pure[F]
        }
      case ApplyExpression(IfElseOp, args) =>
        evaluateIfElse(args)
      case ApplyExpression(op, args) =>
        for {
          argValues <- args.traverse(run).map(_.sequence)
          result    <- argValues.flatTraverse(JsonLogicSemantics[F].applyOp(op)(_))
        } yield result
    }

    def callbackFunc(
      op: JsonLogicOp,
      arrExpr: JsonLogicExpression,
      cbExpr: JsonLogicExpression,
      initExpr: Option[JsonLogicExpression] = None
    ): F[Either[JsonLogicException, JsonLogicValue]] =
      (for {
        arrVal     <- EitherT(run(arrExpr))
        initValOpt <- EitherT(initExpr.traverse(run).map(_.sequence))
        args <- EitherT.fromEither((arrVal, initValOpt) match {
          case (arr, Some(init)) => List(arr, FunctionValue(cbExpr), init).asRight[JsonLogicException]
          case (arr, None)       => List(arr, FunctionValue(cbExpr)).asRight[JsonLogicException]
        })
        result <- EitherT(JsonLogicSemantics[F].applyOp(op)(args))
      } yield result).value

    def evaluateIfElse(args: List[JsonLogicExpression]): F[Either[JsonLogicException, JsonLogicValue]] =
      args match {
        case Nil =>
          JsonLogicException("If/else requires at least one argument").asLeft[JsonLogicValue].pure[F]
        case condition :: thenBranch :: rest =>
          run(condition).flatMap {
            case Right(condValue) =>
              condValue.isTruthy
                .pure[F]
                .ifM(
                  ifTrue = run(thenBranch),
                  ifFalse = rest match {
                    case Nil              => (NullValue: JsonLogicValue).asRight[JsonLogicException].pure[F]
                    case List(elseBranch) => run(elseBranch)
                    case moreArgs         => evaluateIfElse(moreArgs)
                  }
                )
            case Left(err) => err.asLeft[JsonLogicValue].pure[F]
          }
        case _ =>
          JsonLogicException("If/else malformed: condition without then-branch")
            .asLeft[JsonLogicValue]
            .pure[F]
      }

    run(expr)
  }

  def interpret[F[_]: Monad: JsonLogicSemantics](
    expr: JsonLogicExpression,
    ctx: Option[JsonLogicValue] = None
  ): F[Either[JsonLogicException, JsonLogicValue]] = {

    val initStack: Frame.Stack = List(Frame.Eval(expr, None))

    Monad[F].tailRecM[Frame.Stack, Either[JsonLogicException, JsonLogicValue]](initStack) {
      case Nil =>
        JsonLogicException("Empty stack: no final result!")
          .asLeft[JsonLogicValue]
          .asRight[Frame.Stack]
          .pure[F]

      case Frame.Eval(ConstExpression(v), contOpt) :: tail =>
        contOpt.continueOrTerminate(v, tail).pure[F]

      case Frame.Eval(ArrayExpression(args), contOpt) :: tail =>
        if (args.isEmpty) {
          contOpt.continueOrTerminate(ArrayValue(List.empty), tail).pure[F]
        } else {
          val newCont = Continuation(
            JsonLogicOp.NoOp,
            Nil,
            args.tail,
            contOpt,
            None,
            None,
            isVarName = false,
            isReduceWithoutInit = false,
            isArray = true
          )
          (Frame.Eval(args.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]
        }

      case Frame.Eval(MapExpression(map), contOpt) :: tail =>
        if (map.isEmpty) contOpt.continueOrTerminate(MapValue.empty, tail).pure[F]
        else {
          val pairs = map.toList
          val (firstKey, firstExpr) = pairs.head
          val remaining = pairs.tail
          val newCont = Continuation(
            JsonLogicOp.NoOp,
            Nil,
            remaining.map(_._2),
            contOpt,
            None,
            None,
            isVarName = false,
            isReduceWithoutInit = false,
            isArray = false,
            mapKeys = List(firstKey) ++ remaining.map(_._1)
          )
          (Frame.Eval(firstExpr, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]
        }

      case Frame.Eval(VarExpression(Left(key), defaultOpt), contOpt) :: tail =>
        lookupVar(key, defaultOpt)(ctx).map {
          case Left(err)    => err.asLeft[JsonLogicValue].asRight[Frame.Stack]
          case Right(value) => contOpt.continueOrTerminate(value, tail)
        }

      case Frame.Eval(VarExpression(Right(expr), defaultOpt), contOpt) :: tail =>
        // Create a continuation that will look up the variable after evaluating expr
        val varNameCont = Continuation(JsonLogicOp.NoOp, Nil, Nil, contOpt, None, defaultOpt, isVarName = true)
        (Frame.Eval(expr, Some(varNameCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]

      // Handle specific callback operators with special argument patterns
      case Frame.Eval(ApplyExpression(op, args), contOpt) :: tail if Frame.isCallbackOp(op) =>
        args match {
          case List(arrExpr, cb: JsonLogicExpression) =>
            val newCont = Continuation(op, Nil, Nil, contOpt, Some(cb))
            (Frame.Eval(arrExpr, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]

          case List(arrExpr) if op == CountOp =>
            val newCont = Continuation(op, Nil, Nil, contOpt, None)
            (Frame.Eval(arrExpr, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]

          case _ =>
            JsonLogicException(s"Invalid arguments for callback operation: $op, got $args")
              .asLeft[JsonLogicValue]
              .asRight[Frame.Stack]
              .pure[F]
        }

      // Handle IfElseOp with lazy evaluation of branches
      case Frame.Eval(ApplyExpression(IfElseOp, args), contOpt) :: tail =>
        if (args.length < 2) {
          JsonLogicException(s"Invalid arguments for if/else operation: expected at least 2 args, got ${args.length}")
            .asLeft[JsonLogicValue]
            .asRight[Frame.Stack]
            .pure[F]
        } else {
          // Start by evaluating the first condition
          val newCont = Continuation(
            IfElseOp,
            Nil,
            args.tail, // Remaining expressions (then-branch, next-condition, ...)
            contOpt,
            None,
            None,
            isVarName = false,
            isReduceWithoutInit = false,
            isArray = false,
            isIfElse = true
          )
          (Frame.Eval(args.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]
        }

      // Handle ReduceOp with special cases for different argument patterns
      case Frame.Eval(ApplyExpression(ReduceOp, args), contOpt) :: tail =>
        args match {
          // Handle case: [arr, cb]
          case List(arrExpr, cb: JsonLogicExpression) =>
            val newCont =
              Continuation(ReduceOp, Nil, Nil, contOpt, Some(cb), None, isVarName = false, isReduceWithoutInit = true)
            (Frame.Eval(arrExpr, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]

          // Handle case: [arr, cb, init]
          case List(arrExpr, cb: JsonLogicExpression, initExpr) =>
            val initCont = Continuation(ReduceOp, Nil, List(arrExpr), contOpt, Some(cb))
            (Frame.Eval(initExpr, Some(initCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]

          case _ =>
            JsonLogicException(s"Invalid arguments for callback operation: $ReduceOp, got $args")
              .asLeft[JsonLogicValue]
              .asRight[Frame.Stack]
              .pure[F]
        }

      // Generic operator application
      case Frame.Eval(ApplyExpression(op, args), contOpt) :: tail =>
        if (args.isEmpty) {
          JsonLogicSemantics[F]
            .applyOp(op)(Nil)
            .map {
              case Left(err)  => err.asLeft[JsonLogicValue].asRight[Frame.Stack]
              case Right(res) => contOpt.continueOrTerminate(res, tail)
            }
        } else {
          val newCont = Continuation(op, Nil, args.tail, contOpt, None)
          (Frame.Eval(args.head, Some(newCont)) :: tail)
            .asLeft[Either[JsonLogicException, JsonLogicValue]]
            .pure[F]
        }

      // Handle applying a value to an array continuation
      case Frame.ApplyValue(
            value,
            cont @ Continuation(_, processed, remaining, parentContOpt, _, _, _, _, true, _, _)
          ) :: tail =>
        if (remaining.isEmpty) {
          // All array elements processed
          val arrayValue = ArrayValue(processed :+ value)
          parentContOpt.continueOrTerminate(arrayValue, tail).pure[F]
        } else {
          // Process next array element
          val newCont = cont.copy(processed = processed :+ value, remaining = remaining.tail)
          (Frame.Eval(remaining.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]
        }

      // Handle applying a value to a map continuation
      case Frame.ApplyValue(
            value,
            cont @ Continuation(_, processed, remaining, parentContOpt, _, _, _, _, false, mapKeys, _)
          ) :: tail if mapKeys.nonEmpty =>
        val newProcessed = processed :+ value
        if (remaining.isEmpty) {
          // All map values processed - construct MapValue
          val pairs = mapKeys.zip(newProcessed).toMap
          parentContOpt.continueOrTerminate(MapValue(pairs), tail).pure[F]
        } else {
          // Process next map value
          val newCont = cont.copy(processed = newProcessed, remaining = remaining.tail)
          (Frame.Eval(remaining.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]
        }

      // Handle applying a value to a varName continuation
      case Frame.ApplyValue(value, Continuation(_, _, _, parentContOpt, _, defaultOpt, true, _, _, _, _)) :: tail =>
        (value match {
          case StrValue(name) =>
            lookupVar(name, defaultOpt)(ctx)
          case ArrayValue(StrValue(name) :: _) =>
            lookupVar(name, defaultOpt)(ctx)
          case v =>
            JsonLogicException(s"Got non-string input: $v").asLeft[JsonLogicValue].pure[F]
        }).map {
          case Left(err)     => err.asLeft[JsonLogicValue].asRight[Frame.Stack]
          case Right(result) => parentContOpt.continueOrTerminate(result, tail)
        }

      // Handle applying a value to callback operations (with predicate)
      case Frame.ApplyValue(value, Continuation(op, _, _, parentContOpt, Some(cbExpr), _, _, _, _, _, _)) :: tail
          if Frame.isCallbackOp(op) =>
        value match {
          case arr @ ArrayValue(_) =>
            JsonLogicSemantics[F]
              .applyOp(op)(List(arr, FunctionValue(cbExpr)))
              .map {
                case Left(err)  => err.asLeft[JsonLogicValue].asRight[Frame.Stack]
                case Right(res) => parentContOpt.continueOrTerminate(res, tail)
              }

          case ex =>
            JsonLogicException(s"Expected array value for callback operation, got: $ex")
              .asLeft[JsonLogicValue]
              .asRight[Frame.Stack]
              .pure[F]
        }

      // Handle applying a value to IfElse continuation (lazy evaluation)
      case Frame.ApplyValue(condValue, Continuation(IfElseOp, _, remaining, parentContOpt, _, _, _, _, _, _, true)) :: tail =>
        remaining match {
          // Pattern: remaining = thenBranch :: moreArgs
          case thenBranch :: rest =>
            if (condValue.isTruthy) {
              // Condition is true: evaluate the then-branch and return
              (Frame.Eval(thenBranch, parentContOpt) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]
            } else if (rest.isEmpty) {
              // No more branches, no else clause - return null
              parentContOpt.continueOrTerminate(NullValue, tail).pure[F]
            } else if (rest.length == 1) {
              // Only one element left: the final else branch
              (Frame.Eval(rest.head, parentContOpt) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]
            } else {
              // More condition/value pairs to evaluate: nextCondition :: nextThen :: moreArgs
              // Continue with the next condition
              val newCont = Continuation(
                IfElseOp,
                Nil,
                rest.tail, // Skip the next condition, keep remaining
                parentContOpt,
                None,
                None,
                isVarName = false,
                isReduceWithoutInit = false,
                isArray = false,
                isIfElse = true
              )
              (Frame.Eval(rest.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]].pure[F]
            }

          case Nil =>
            JsonLogicException("If/else malformed: no remaining expressions")
              .asLeft[JsonLogicValue]
              .asRight[Frame.Stack]
              .pure[F]
        }

      // Handle applying a value to CountOp without callback (simple count)
      case Frame.ApplyValue(value, Continuation(CountOp, _, _, parentContOpt, None, _, _, _, _, _, _)) :: tail =>
        value match {
          case arr @ ArrayValue(_) =>
            JsonLogicSemantics[F]
              .applyOp(CountOp)(List(arr))
              .map {
                case Left(err)  => err.asLeft[JsonLogicValue].asRight[Frame.Stack]
                case Right(res) => parentContOpt.continueOrTerminate(res, tail)
              }

          case ex =>
            JsonLogicException(s"Expected array value for count operation, got: $ex")
              .asLeft[JsonLogicValue]
              .asRight[Frame.Stack]
              .pure[F]
        }

      // Handle ReduceOp without explicit init value (uses first array element)
      case Frame.ApplyValue(
            ArrayValue(elements),
            Continuation(ReduceOp, _, _, parentContOpt, Some(cbExpr), _, _, true, _, _, _)
          ) :: tail =>
        if (elements.isEmpty) {
          parentContOpt.continueOrTerminate(NullValue, tail).pure[F]
        } else {
          val init = elements.head
          val rest = ArrayValue(elements.tail)
          JsonLogicSemantics[F]
            .applyOp(ReduceOp)(List(rest, FunctionValue(cbExpr), init))
            .map {
              case Left(err)  => err.asLeft[JsonLogicValue].asRight[Frame.Stack]
              case Right(res) => parentContOpt.continueOrTerminate(res, tail)
            }
        }

      // Handle ReduceOp with explicit init value
      case Frame.ApplyValue(
            arr @ ArrayValue(_),
            Continuation(ReduceOp, List(init), Nil, parentContOpt, Some(cbExpr), _, _, false, _, _, _)
          ) :: tail =>
        JsonLogicSemantics[F]
          .applyOp(ReduceOp)(List(arr, FunctionValue(cbExpr), init))
          .map {
            case Left(err)  => err.asLeft[JsonLogicValue].asRight[Frame.Stack]
            case Right(res) => parentContOpt.continueOrTerminate(res, tail)
          }

      // Handle ReduceOp continuation (still processing arguments)
      case Frame.ApplyValue(
            value,
            cont @ Continuation(ReduceOp, _, remaining, _, _, _, _, false, _, _, _)
          ) :: tail if remaining.nonEmpty =>
        val newCont = cont.copy(processed = List(value), remaining = remaining.tail)
        (Frame.Eval(remaining.head, Some(newCont)) :: tail)
          .asLeft[Either[JsonLogicException, JsonLogicValue]]
          .pure[F]

      // Standard value application
      case Frame.ApplyValue(
            value,
            Continuation(op, processed, remaining, parentContOpt, None, _, _, _, false, _, _)
          ) :: tail =>
        val newProcessed = processed :+ value
        if (remaining.isEmpty) {
          JsonLogicSemantics[F]
            .applyOp(op)(newProcessed)
            .map {
              case Left(err)  => err.asLeft[JsonLogicValue].asRight[Frame.Stack]
              case Right(res) => parentContOpt.continueOrTerminate(res, tail)
            }
        } else {
          val newCont = Continuation(op, newProcessed, remaining.tail, parentContOpt, None)
          (Frame.Eval(remaining.head, Some(newCont)) :: tail)
            .asLeft[Either[JsonLogicException, JsonLogicValue]]
            .pure[F]
        }

      case unknown =>
        JsonLogicException(s"Unexpected stack configuration: $unknown")
          .asLeft[JsonLogicValue]
          .asRight[Frame.Stack]
          .pure[F]
    }
  }

  /** A frame holds the "state" for each expression that must be evaluated */
  sealed trait Frame

  /**
   * A continuation remembers an operator call with partially evaluated args.
   */
  final case class Continuation(
    op: JsonLogicOp,
    processed: List[JsonLogicValue],
    remaining: List[JsonLogicExpression],
    parent: Option[Continuation],
    callbackExpr: Option[JsonLogicExpression],
    defaultOpt: Option[JsonLogicValue] = None,
    isVarName: Boolean = false,
    isReduceWithoutInit: Boolean = false,
    isArray: Boolean = false,
    mapKeys: List[String] = List.empty,
    isIfElse: Boolean = false
  )

  object Frame {
    type Stack = List[Frame]

    val isCallbackOp: JsonLogicOp => Boolean = callbackOps.contains

    final case class Eval(expr: JsonLogicExpression, contOpt: Option[Continuation]) extends Frame

    final case class ApplyValue(value: JsonLogicValue, cont: Continuation) extends Frame

  }

  object Continuation {

    /**
     * If there's a parent continuation, push a Frame.ApplyValue(cont, value).
     * Otherwise, we are at top-level => done with a final success `Right(value)`.
     */
    implicit class continueOps(contOpt: Option[Continuation]) {

      def continueOrTerminate(
        value: JsonLogicValue,
        tail: Frame.Stack
      ): Either[Frame.Stack, Either[JsonLogicException, JsonLogicValue]] =
        contOpt match {
          case Some(cont) => (Frame.ApplyValue(value, cont) :: tail).asLeft[Either[JsonLogicException, JsonLogicValue]]
          case None       => value.asRight[JsonLogicException].asRight[List[Frame]]
        }
    }
  }
}
