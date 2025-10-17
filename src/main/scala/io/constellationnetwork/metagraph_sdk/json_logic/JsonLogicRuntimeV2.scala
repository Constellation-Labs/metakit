package io.constellationnetwork.metagraph_sdk.json_logic

import cats.Monad
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.ResultContext._

object JsonLogicRuntimeV2 {

  // Shared helper to determine if an argument at a given index is a callback
  private def isCallbackArg(op: JsonLogicOp, argIndex: Int): Boolean = op match {
    case JsonLogicOp.MapOp | JsonLogicOp.FilterOp | JsonLogicOp.AllOp |
         JsonLogicOp.SomeOp | JsonLogicOp.NoneOp | JsonLogicOp.FindOp | JsonLogicOp.CountOp =>
      argIndex == 1

    case JsonLogicOp.ReduceOp =>
      argIndex == 1

    case _ =>
      false
  }

  // Shared helper to extract the value from a Result[JsonLogicValue]
  private def extractValue[Result[_]](result: Result[JsonLogicValue]): JsonLogicValue = result match {
    case v: JsonLogicValue => v
    case (v: JsonLogicValue, _) => v
    case _ => NullValue
  }

  // Shared helper to extract a list from Result[List[JsonLogicValue]]
  private def extractList[Result[_]](result: Result[List[JsonLogicValue]]): List[JsonLogicValue] = result match {
    case list: List[_] => list.asInstanceOf[List[JsonLogicValue]]
    case (list: List[_], _) => list.asInstanceOf[List[JsonLogicValue]]
    case _ => List.empty
  }

  // Direct recursive evaluation (may cause stack overflow on deep expressions??)
  def evaluateDirect[F[_]: Monad, Result[_]: ResultContext](
    program: JsonLogicExpression,
    ctx: Option[JsonLogicValue]
  )(implicit sem: JsonLogicSemanticsV2[F, Result]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

    def evaluateExpression(
      expr: JsonLogicExpression,
      currentCtx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, Result[JsonLogicValue]]] = expr match {
      case ConstExpression(value) =>
        value.pure[Result].asRight[JsonLogicException].pure[F]

      case VarExpression(Left(key), _) =>
        sem.getVar(key, currentCtx)

      case VarExpression(Right(keyExpr), _) =>
        evaluateExpression(keyExpr, currentCtx).flatMap {
          case Right(keyResult) =>
            val keyValue = extractValue(keyResult)
            sem.getVar(keyValue.toString, currentCtx)
          case Left(error) =>
            error.asLeft[Result[JsonLogicValue]].pure[F]
        }

      case ArrayExpression(elements) =>
        elements.traverse(el => evaluateExpression(el, currentCtx)).map { evaluatedElements =>
          evaluatedElements.sequence.map { results =>
            results.sequence.map(ArrayValue(_): JsonLogicValue)
          }
        }

      case MapExpression(map) =>
        map.toList.traverse { case (k, v) =>
          evaluateExpression(v, currentCtx).map(_.map(k -> _))
        }.map { evaluatedPairs =>
          evaluatedPairs.sequence.map { pairs =>
            val (keys, vResults) = pairs.unzip
            vResults.sequence.map(values => MapValue(keys.zip(values).toMap): JsonLogicValue)
          }
        }

      case ApplyExpression(JsonLogicOp.IfElseOp, args) =>
        def evaluateIfElse(argsList: List[JsonLogicExpression]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
          argsList match {
            case Nil =>
              JsonLogicException("If/else requires at least one argument")
                .asLeft[Result[JsonLogicValue]]
                .pure[F]
            case condition :: thenBranch :: rest =>
              evaluateExpression(condition, currentCtx).flatMap {
                case Right(condResult) =>
                  val condValue = extractValue(condResult)
                  condValue.isTruthy
                    .pure[F]
                    .ifM(
                      ifTrue = evaluateExpression(thenBranch, currentCtx),
                      ifFalse = rest match {
                        case Nil => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
                        case List(elseBranch) => evaluateExpression(elseBranch, currentCtx)
                        case moreArgs => evaluateIfElse(moreArgs)
                      }
                    )
                case Left(error) =>
                  error.asLeft[Result[JsonLogicValue]].pure[F]
              }
            case _ =>
              JsonLogicException("If/else malformed: condition without then-branch")
                .asLeft[Result[JsonLogicValue]]
                .pure[F]
          }

        evaluateIfElse(args)

      case ApplyExpression(op, args) =>
        args.zipWithIndex.traverse { case (arg, idx) =>
          if (JsonLogicRuntimeV2.isCallbackArg(op, idx)) {
            (FunctionValue(arg): JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
          } else {
            evaluateExpression(arg, currentCtx)
          }
        }.flatMap { evaluatedArgs =>
          evaluatedArgs.sequence match {
            case Right(resultArgs) =>
              val unwrappedArgs = extractList(resultArgs.sequence)
              sem.applyOp(op)(unwrappedArgs)
            case Left(error) =>
              error.asLeft[Result[JsonLogicValue]].pure[F]
          }
        }
    }

    evaluateExpression(program, ctx)
  }

  // Tail-recursive stack-machine evaluation using tailRecM (stack-safe)
  // Default evaluate uses tail-recursive interpret for stack safety
  def evaluate[F[_]: Monad, Result[_]: ResultContext](
                                                       program: JsonLogicExpression,
                                                       ctx: Option[JsonLogicValue]
  )(implicit sem: JsonLogicSemanticsV2[F, Result]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

    // Stack frames for tail-recursive evaluation
    sealed trait Frame
    case class Eval(expr: JsonLogicExpression, contOpt: Option[Continuation]) extends Frame
    case class ApplyValue(value: Result[JsonLogicValue], cont: Continuation) extends Frame

    case class Continuation(
      op: JsonLogicOp,
      processed: List[Result[JsonLogicValue]],
      remaining: List[JsonLogicExpression],
      parent: Option[Continuation],
      isArray: Boolean = false,
      mapKeys: List[String] = List.empty,
      isIfElse: Boolean = false
    )

    implicit class ContinuationOps(contOpt: Option[Continuation]) {
      def continueOrTerminate(
        value: Result[JsonLogicValue],
        tail: List[Frame]
      ): Either[List[Frame], Either[JsonLogicException, Result[JsonLogicValue]]] =
        contOpt match {
          case Some(cont) => (ApplyValue(value, cont) :: tail).asLeft
          case None => value.asRight[JsonLogicException].asRight
        }
    }

    type Stack = List[Frame]
    val initStack: Stack = List(Eval(program, None))

    Monad[F].tailRecM[Stack, Either[JsonLogicException, Result[JsonLogicValue]]](initStack) {
      case Nil =>
        JsonLogicException("Empty stack: no final result!")
          .asLeft[Result[JsonLogicValue]]
          .asRight[Stack]
          .pure[F]

      case Eval(ConstExpression(v), contOpt) :: tail =>
        contOpt.continueOrTerminate(v.pure[Result], tail).pure[F]

      case Eval(VarExpression(Left(key), _), contOpt) :: tail =>
        sem.getVar(key, ctx).map {
          case Right(value) => contOpt.continueOrTerminate(value, tail)
          case Left(err) => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
        }

      case Eval(VarExpression(Right(keyExpr), _), contOpt) :: tail =>
        (Eval(keyExpr, Some(Continuation(JsonLogicOp.NoOp, Nil, Nil, contOpt))) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]

      case Eval(ArrayExpression(elements), contOpt) :: tail =>
        if (elements.isEmpty) {
          contOpt.continueOrTerminate((ArrayValue(List.empty): JsonLogicValue).pure[Result], tail).pure[F]
        } else {
          val newCont = Continuation(JsonLogicOp.NoOp, Nil, elements.tail, contOpt, isArray = true)
          (Eval(elements.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case Eval(MapExpression(map), contOpt) :: tail =>
        if (map.isEmpty) {
          contOpt.continueOrTerminate((MapValue.empty: JsonLogicValue).pure[Result], tail).pure[F]
        } else {
          val pairs = map.toList
          val (firstKey, firstExpr) = pairs.head
          val remaining = pairs.tail
          val newCont = Continuation(
            JsonLogicOp.NoOp,
            Nil,
            remaining.map(_._2),
            contOpt,
            isArray = false,
            mapKeys = List(firstKey) ++ remaining.map(_._1)
          )
          (Eval(firstExpr, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      // Handle IfElseOp with lazy evaluation of branches
      case Eval(ApplyExpression(JsonLogicOp.IfElseOp, args), contOpt) :: tail =>
        if (args.length < 2) {
          JsonLogicException(s"Invalid arguments for if/else operation: expected at least 2 args, got ${args.length}")
            .asLeft[Result[JsonLogicValue]]
            .asRight[Stack]
            .pure[F]
        } else {
          val newCont = Continuation(
            JsonLogicOp.IfElseOp,
            Nil,
            args.tail,
            contOpt,
            isArray = false,
            mapKeys = List.empty,
            isIfElse = true
          )
          (Eval(args.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case Eval(ApplyExpression(op, args), contOpt) :: tail =>
        if (args.isEmpty) {
          sem.applyOp(op)(Nil).map {
            case Right(res) => contOpt.continueOrTerminate(res, tail)
            case Left(err) => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
          }
        } else if (JsonLogicRuntimeV2.isCallbackArg(op, 0)) {
          // First arg is callback - wrap it in FunctionValue
          val wrappedCallback: Result[JsonLogicValue] = (FunctionValue(args.head): JsonLogicValue).pure[Result]
          val newCont = Continuation(op, List(wrappedCallback), args.tail, contOpt)
          if (args.tail.isEmpty) {
            // No more args - apply the operation
            val unwrappedArgs = List(JsonLogicRuntimeV2.extractValue(wrappedCallback))
            sem.applyOp(op)(unwrappedArgs).map {
              case Right(res) => contOpt.continueOrTerminate(res, tail)
              case Left(err) => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
            }
          } else {
            // More args remain - evaluate next arg
            (Eval(args.tail.head, Some(newCont.copy(remaining = args.tail.tail))) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
          }
        } else {
          val newCont = Continuation(op, Nil, args.tail, contOpt)
          (Eval(args.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case ApplyValue(value, cont @ Continuation(_, processed, remaining, parentContOpt, true, _, _)) :: tail =>
        if (remaining.isEmpty) {
          val arrayValue: Result[JsonLogicValue] = (processed :+ value).sequence.map(arr => ArrayValue(arr): JsonLogicValue)
          parentContOpt.continueOrTerminate(arrayValue, tail).pure[F]
        } else {
          val newCont = cont.copy(processed = processed :+ value, remaining = remaining.tail)
          (Eval(remaining.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case ApplyValue(value, cont @ Continuation(_, processed, remaining, parentContOpt, false, mapKeys, _)) :: tail if mapKeys.nonEmpty =>
        val newProcessed = processed :+ value
        if (remaining.isEmpty) {
          val pairs = mapKeys.zip(newProcessed.map(JsonLogicRuntimeV2.extractValue)).toMap
          parentContOpt.continueOrTerminate((MapValue(pairs): JsonLogicValue).pure[Result], tail).pure[F]
        } else {
          val newCont = cont.copy(processed = newProcessed, remaining = remaining.tail)
          (Eval(remaining.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      // Handle applying a value to IfElse continuation (lazy evaluation)
      case ApplyValue(condValue, Continuation(JsonLogicOp.IfElseOp, _, remaining, parentContOpt, _, _, true)) :: tail =>
        remaining match {
          case thenBranch :: rest =>
            val condVal = JsonLogicRuntimeV2.extractValue(condValue)
            if (condVal.isTruthy) {
              (Eval(thenBranch, parentContOpt) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
            } else if (rest.isEmpty) {
              parentContOpt.continueOrTerminate((NullValue: JsonLogicValue).pure[Result], tail).pure[F]
            } else if (rest.length == 1) {
              (Eval(rest.head, parentContOpt) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
            } else {
              val newCont = Continuation(
                JsonLogicOp.IfElseOp,
                Nil,
                rest.tail,
                parentContOpt,
                isArray = false,
                mapKeys = List.empty,
                isIfElse = true
              )
              (Eval(rest.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
            }

          case Nil =>
            JsonLogicException("If/else malformed: no remaining expressions")
              .asLeft[Result[JsonLogicValue]]
              .asRight[Stack]
              .pure[F]
        }

      case ApplyValue(value, Continuation(JsonLogicOp.NoOp, _, _, parentContOpt, _, _, _)) :: tail =>
        val keyValue = JsonLogicRuntimeV2.extractValue(value)
        sem.getVar(keyValue.toString, ctx).map {
          case Right(result) => parentContOpt.continueOrTerminate(result, tail)
          case Left(err) => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
        }

      case ApplyValue(value, Continuation(op, processed, remaining, parentContOpt, false, _, _)) :: tail =>
        val newProcessed = processed :+ value
        if (remaining.isEmpty) {
          val unwrappedArgs = newProcessed.map(JsonLogicRuntimeV2.extractValue)
          sem.applyOp(op)(unwrappedArgs).map {
            case Right(res) => parentContOpt.continueOrTerminate(res, tail)
            case Left(err) => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
          }
        } else {
          val nextArgIndex = newProcessed.size
          if (JsonLogicRuntimeV2.isCallbackArg(op, nextArgIndex)) {
            // Next arg is a callback - wrap it without evaluating
            val wrappedCallback: Result[JsonLogicValue] = (FunctionValue(remaining.head): JsonLogicValue).pure[Result]
            val updatedProcessed: List[Result[JsonLogicValue]] = newProcessed :+ wrappedCallback
            if (remaining.tail.isEmpty) {
              // No more args - apply operation
              val unwrappedArgs = updatedProcessed.map(JsonLogicRuntimeV2.extractValue)
              sem.applyOp(op)(unwrappedArgs).map {
                case Right(res) => parentContOpt.continueOrTerminate(res, tail)
                case Left(err) => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
              }
            } else {
              // More args remain
              val newCont = Continuation(op, updatedProcessed, remaining.tail.tail, parentContOpt)
              (Eval(remaining.tail.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
            }
          } else {
            val newCont = Continuation(op, newProcessed, remaining.tail, parentContOpt)
            (Eval(remaining.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
          }
        }

      case unknown =>
        JsonLogicException(s"Unexpected stack configuration: $unknown")
          .asLeft[Result[JsonLogicValue]]
          .asRight[Stack]
          .pure[F]
    }
  }
}