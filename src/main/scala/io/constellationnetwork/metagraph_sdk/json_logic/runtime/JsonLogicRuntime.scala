package io.constellationnetwork.metagraph_sdk.json_logic.runtime

import cats.Monad
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.semantics.JsonLogicSemantics

import ResultContext._

object JsonLogicRuntime {

  // Shared helper to determine if an argument at a given index is a callback
  private def isCallbackArg(op: JsonLogicOp, argIndex: Int): Boolean = op match {
    case JsonLogicOp.MapOp | JsonLogicOp.FilterOp | JsonLogicOp.AllOp | JsonLogicOp.SomeOp | JsonLogicOp.NoneOp | JsonLogicOp.FindOp |
        JsonLogicOp.CountOp =>
      argIndex == 1

    case JsonLogicOp.ReduceOp =>
      argIndex == 1

    case _ =>
      false
  }

  private def lookupVar[F[_]: Monad, Result[_]: ResultContext](
    key: String,
    defaultOpt: Option[JsonLogicValue],
    currentCtx: Option[JsonLogicValue]
  )(implicit sem: JsonLogicSemantics[F, Result]): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
    sem.getVar(key, currentCtx).map {
      case Right(result) =>
        result.extractValue match {
          case NullValue if key.nonEmpty =>
            defaultOpt match {
              case Some(d) => d.pure[Result].asRight[JsonLogicException]
              case None    => result.asRight[JsonLogicException]
            }
          case _ =>
            result.asRight[JsonLogicException]
        }
      case Left(_) =>
        defaultOpt match {
          case Some(d) => d.pure[Result].asRight[JsonLogicException]
          case None    => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException]
        }
    }

  // Direct recursive evaluation (may cause stack overflow on deep expressions??)
  def evaluateDirect[F[_]: Monad, Result[_]: ResultContext](
    program: JsonLogicExpression,
    ctx: Option[JsonLogicValue]
  )(implicit sem: JsonLogicSemantics[F, Result]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

    def evaluateExpression(
      expr: JsonLogicExpression,
      currentCtx: Option[JsonLogicValue]
    ): F[Either[JsonLogicException, Result[JsonLogicValue]]] = expr match {
      case ConstExpression(value) =>
        value.pure[Result].asRight[JsonLogicException].pure[F]

      case VarExpression(Left(key), defaultOpt) =>
        lookupVar(key, defaultOpt, currentCtx)

      case VarExpression(Right(keyExpr), defaultOpt) =>
        evaluateExpression(keyExpr, currentCtx).flatMap {
          case Right(keyResult) =>
            keyResult.extractValue match {
              case StrValue(name)                  => lookupVar(name, defaultOpt, currentCtx)
              case ArrayValue(StrValue(name) :: _) => lookupVar(name, defaultOpt, currentCtx)
              case v                               => JsonLogicException(s"Got non-string input: $v").asLeft[Result[JsonLogicValue]].pure[F]
            }
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
        map.toList.traverse {
          case (k, v) =>
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
                  condResult.extractValue.isTruthy
                    .pure[F]
                    .ifM(
                      ifTrue = evaluateExpression(thenBranch, currentCtx),
                      ifFalse = rest match {
                        case Nil              => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
                        case List(elseBranch) => evaluateExpression(elseBranch, currentCtx)
                        case moreArgs         => evaluateIfElse(moreArgs)
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
        args.zipWithIndex.traverse {
          case (arg, idx) =>
            if (JsonLogicRuntime.isCallbackArg(op, idx)) {
              arg match {
                case ConstExpression(fv: FunctionValue) =>
                  (fv: JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
                case _ =>
                  (FunctionValue(arg): JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
              }
            } else {
              evaluateExpression(arg, currentCtx)
            }
        }.flatMap { evaluatedArgs =>
          evaluatedArgs.sequence match {
            case Right(resultArgs) =>
              sem.applyOp(op)(resultArgs)
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
  )(implicit sem: JsonLogicSemantics[F, Result]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

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
      isIfElse: Boolean = false,
      varDefault: Option[JsonLogicValue] = None
    )

    implicit class ContinuationOps(contOpt: Option[Continuation]) {
      def continueOrTerminate(
        value: Result[JsonLogicValue],
        tail: List[Frame]
      ): Either[List[Frame], Either[JsonLogicException, Result[JsonLogicValue]]] =
        contOpt match {
          case Some(cont) => (ApplyValue(value, cont) :: tail).asLeft
          case None       => value.asRight[JsonLogicException].asRight
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

      case Eval(VarExpression(Left(key), defaultOpt), contOpt) :: tail =>
        sem.getVar(key, ctx).map {
          case Right(result) =>
            val finalResult = result.extractValue match {
              case NullValue if key.nonEmpty =>
                defaultOpt match {
                  case Some(d) => d.pure[Result]
                  case None    => result
                }
              case _ => result
            }
            contOpt.continueOrTerminate(finalResult, tail)
          case Left(_) =>
            val finalResult = defaultOpt match {
              case Some(d) => d.pure[Result]
              case None    => (NullValue: JsonLogicValue).pure[Result]
            }
            contOpt.continueOrTerminate(finalResult, tail)
        }

      case Eval(VarExpression(Right(keyExpr), defaultOpt), contOpt) :: tail =>
        (Eval(keyExpr, Some(Continuation(JsonLogicOp.NoOp, Nil, Nil, contOpt, varDefault = defaultOpt))) :: tail)
          .asLeft[Either[JsonLogicException, Result[JsonLogicValue]]]
          .pure[F]

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
            case Left(err)  => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
          }
        } else if (JsonLogicRuntime.isCallbackArg(op, 0)) {
          // First arg is callback - check if it's already a FunctionValue constant
          val wrappedCallback: Result[JsonLogicValue] = args.head match {
            case ConstExpression(fv: FunctionValue) => (fv: JsonLogicValue).pure[Result]
            case _                                  => (FunctionValue(args.head): JsonLogicValue).pure[Result]
          }
          val newCont = Continuation(op, List(wrappedCallback), args.tail, contOpt)
          if (args.tail.isEmpty) {
            // No more args - apply the operation
            sem.applyOp(op)(List(wrappedCallback)).map {
              case Right(res) => contOpt.continueOrTerminate(res, tail)
              case Left(err)  => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
            }
          } else {
            // More args remain - evaluate next arg
            (Eval(args.tail.head, Some(newCont.copy(remaining = args.tail.tail))) :: tail)
              .asLeft[Either[JsonLogicException, Result[JsonLogicValue]]]
              .pure[F]
          }
        } else {
          val newCont = Continuation(op, Nil, args.tail, contOpt)
          (Eval(args.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case ApplyValue(value, cont @ Continuation(_, processed, remaining, parentContOpt, true, _, _, _)) :: tail =>
        if (remaining.isEmpty) {
          val arrayValue: Result[JsonLogicValue] = (processed :+ value).sequence.map(arr => ArrayValue(arr): JsonLogicValue)
          parentContOpt.continueOrTerminate(arrayValue, tail).pure[F]
        } else {
          val newCont = cont.copy(processed = processed :+ value, remaining = remaining.tail)
          (Eval(remaining.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case ApplyValue(value, cont @ Continuation(_, processed, remaining, parentContOpt, false, mapKeys, _, _)) :: tail
          if mapKeys.nonEmpty =>
        val newProcessed = processed :+ value
        if (remaining.isEmpty) {
          val pairs = mapKeys.zip(newProcessed.map(_.extractValue)).toMap
          parentContOpt.continueOrTerminate((MapValue(pairs): JsonLogicValue).pure[Result], tail).pure[F]
        } else {
          val newCont = cont.copy(processed = newProcessed, remaining = remaining.tail)
          (Eval(remaining.head, Some(newCont)) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      // Handle applying a value to IfElse continuation (lazy evaluation)
      case ApplyValue(condValue, Continuation(JsonLogicOp.IfElseOp, _, remaining, parentContOpt, _, _, true, _)) :: tail =>
        remaining match {
          case thenBranch :: rest =>
            if (condValue.extractValue.isTruthy) {
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

      case ApplyValue(value, Continuation(JsonLogicOp.NoOp, _, _, parentContOpt, _, _, _, defaultOpt)) :: tail =>
        value.extractValue match {
          case StrValue(name) =>
            sem.getVar(name, ctx).map {
              case Right(result) =>
                val finalResult = result.extractValue match {
                  case NullValue if name.nonEmpty =>
                    defaultOpt match {
                      case Some(d) => d.pure[Result]
                      case None    => result
                    }
                  case _ => result
                }
                parentContOpt.continueOrTerminate(finalResult, tail)
              case Left(_) =>
                val finalResult = defaultOpt match {
                  case Some(d) => d.pure[Result]
                  case None    => (NullValue: JsonLogicValue).pure[Result]
                }
                parentContOpt.continueOrTerminate(finalResult, tail)
            }
          case ArrayValue(StrValue(name) :: _) =>
            sem.getVar(name, ctx).map {
              case Right(result) =>
                val finalResult = result.extractValue match {
                  case NullValue if name.nonEmpty =>
                    defaultOpt match {
                      case Some(d) => d.pure[Result]
                      case None    => result
                    }
                  case _ => result
                }
                parentContOpt.continueOrTerminate(finalResult, tail)
              case Left(_) =>
                val finalResult = defaultOpt match {
                  case Some(d) => d.pure[Result]
                  case None    => (NullValue: JsonLogicValue).pure[Result]
                }
                parentContOpt.continueOrTerminate(finalResult, tail)
            }
          case v =>
            JsonLogicException(s"Got non-string input: $v")
              .asLeft[Result[JsonLogicValue]]
              .asRight[Stack]
              .pure[F]
        }

      case ApplyValue(value, Continuation(op, processed, remaining, parentContOpt, false, _, _, _)) :: tail =>
        val newProcessed = processed :+ value
        if (remaining.isEmpty) {
          sem.applyOp(op)(newProcessed).map {
            case Right(res) => parentContOpt.continueOrTerminate(res, tail)
            case Left(err)  => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
          }
        } else {
          val nextArgIndex = newProcessed.size
          if (JsonLogicRuntime.isCallbackArg(op, nextArgIndex)) {
            // Next arg is callback - check if it's already a FunctionValue constant
            val wrappedCallback: Result[JsonLogicValue] = remaining.head match {
              case ConstExpression(fv: FunctionValue) => (fv: JsonLogicValue).pure[Result]
              case _                                  => (FunctionValue(remaining.head): JsonLogicValue).pure[Result]
            }
            val updatedProcessed: List[Result[JsonLogicValue]] = newProcessed :+ wrappedCallback
            if (remaining.tail.isEmpty) {
              // No more args - apply operation
              sem.applyOp(op)(updatedProcessed).map {
                case Right(res) => parentContOpt.continueOrTerminate(res, tail)
                case Left(err)  => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
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
