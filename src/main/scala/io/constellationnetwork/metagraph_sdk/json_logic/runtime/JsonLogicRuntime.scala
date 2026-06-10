package io.constellationnetwork.metagraph_sdk.json_logic.runtime

import cats.Monad
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.semantics.JsonLogicSemantics
import io.constellationnetwork.metagraph_sdk.std.JsonCanonicalizer

import ResultContext._

object JsonLogicRuntime {

  /**
   * Maximum expression-recursion depth, enforced identically by both runtime strategies
   * ([[evaluate]] and [[evaluateDirect]]) and matching the Rust reference `MAX_EVAL_DEPTH`
   * (rust/jlvm-core/src/eval.rs) exactly: one unit is consumed per evaluated expression NODE —
   * nested operator arguments, array/map literal elements, computed var keys, `if`/`let`
   * children, and callback runs (map/filter/reduce/...) all count, with the root node at
   * depth 1. Lazily-skipped work (untaken `if` branches) is never evaluated and therefore
   * never counts. Callback bodies resume from the invoking node's depth via the evaluation
   * callback, so the count is global across nested runtime runs, exactly like Rust's shared
   * depth cell. Exceeding the limit is a normal [[JsonLogicException]]
   * ("Recursion depth limit exceeded (256)"), never a StackOverflowError.
   */
  val MaxEvalDepth: Int = 256

  private def depthExceeded[A]: Either[JsonLogicException, A] =
    JsonLogicException(s"Recursion depth limit exceeded ($MaxEvalDepth)").asLeft

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

  /**
   * Desugar the two accepted `let` surface forms into an ordered list of
   * `(name, valueExpr)` bindings plus the result expression. Mirrors Rust
   * `eval_let` (rust/jlvm-core/src/eval.rs), which accepts both:
   *   - array form : `{"let": [[[name, expr], ...], resultExpr]}`
   *   - object form: `{"let": [{name: expr, ...},   resultExpr]}` (as used by the
   *     cross-language conformance vectors and the TS evaluator).
   * Bindings are returned in order so callers evaluate them sequentially with each
   * prior binding already in scope.
   *
   * Ordering:
   *   - ARRAY form keeps its explicit insertion order (unchanged).
   *   - OBJECT form is evaluated in RFC-8785 sorted-key order (UTF-16 code units) for
   *     crypto-determinism: a JSON object has no inherent member order, so we sort by
   *     the SAME key comparator the canonicalizer uses ([[JsonCanonicalizer.keyOrdering]])
   *     to stay byte-identical with the Rust (`canonical::utf16_cmp`) and TS impls.
   */
  private[runtime] def normalizeLetArgs(
    args: List[JsonLogicExpression]
  ): Either[JsonLogicException, (List[(String, JsonLogicExpression)], JsonLogicExpression)] =
    args match {
      case ArrayExpression(bindings) :: resultExpr :: Nil =>
        bindings.traverse {
          case ArrayExpression(ConstExpression(StrValue(name)) :: valueExpr :: Nil) =>
            (name -> valueExpr).asRight[JsonLogicException]
          case invalid =>
            JsonLogicException(s"let binding must be [name, expr], got: $invalid").asLeft
        }
          .map(_ -> resultExpr)

      case MapExpression(bindings) :: resultExpr :: Nil =>
        // Object-form bindings: a JSON object has no inherent member order, so evaluate
        // in RFC-8785 sorted-key order (UTF-16 code units) using the canonicalizer's key
        // comparator. Each binding then sees the prior (sorted-order) bindings in scope.
        (bindings.toList.sortBy(_._1)(JsonCanonicalizer.keyOrdering) -> resultExpr).asRight[JsonLogicException]

      case _ =>
        JsonLogicException("let requires [[bindings...], resultExpr]").asLeft
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

  // Direct recursive evaluation. Bounded by MaxEvalDepth, so JVM-stack overflow is unreachable
  // for any accepted program. `baseDepth` is the recursion depth already consumed by enclosing
  // runs (callback bodies resume from the invoking node's depth); the program root evaluates at
  // `baseDepth + 1`.
  def evaluateDirect[F[_]: Monad, Result[_]: ResultContext](
    program: JsonLogicExpression,
    ctx: Option[JsonLogicValue],
    baseDepth: Int = 0
  )(implicit sem: JsonLogicSemantics[F, Result]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

    // Same lazy-dispatch base charge as the tail-recursive runtime: if/let never reach
    // sem.applyOp, so their flat base cost (no depth penalty) is charged once per node here.
    def chargeBaseThen(op: JsonLogicOp)(
      proceed: => F[Either[JsonLogicException, Result[JsonLogicValue]]]
    ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
      sem.chargeBase(op) match {
        case None => proceed
        case Some(charge) =>
          charge.flatMap {
            case Right(()) => proceed
            case Left(err) => err.asLeft[Result[JsonLogicValue]].pure[F]
          }
      }

    def evaluateExpression(
      expr: JsonLogicExpression,
      currentCtx: Option[JsonLogicValue],
      depth: Int
    ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
      if (depth > MaxEvalDepth) depthExceeded[Result[JsonLogicValue]].pure[F]
      else
        expr match {
          case ConstExpression(value) =>
            value.pure[Result].asRight[JsonLogicException].pure[F]

          case VarExpression(Left(key), defaultOpt) =>
            lookupVar(key, defaultOpt, currentCtx)

          case VarExpression(Right(keyExpr), defaultOpt) =>
            evaluateExpression(keyExpr, currentCtx, depth + 1).flatMap {
              case Right(keyResult) =>
                keyResult.extractValue match {
                  case StrValue(name)                  => lookupVar(name, defaultOpt, currentCtx)
                  case ArrayValue(StrValue(name) :: _) => lookupVar(name, defaultOpt, currentCtx)
                  case v => JsonLogicException(s"Got non-string input: $v").asLeft[Result[JsonLogicValue]].pure[F]
                }
              case Left(error) =>
                error.asLeft[Result[JsonLogicValue]].pure[F]
            }

          case ArrayExpression(elements) =>
            elements.traverse(el => evaluateExpression(el, currentCtx, depth + 1)).map { evaluatedElements =>
              evaluatedElements.sequence.map { results =>
                results.sequence.map(ArrayValue(_): JsonLogicValue)
              }
            }

          case MapExpression(map) =>
            map.toList.traverse {
              case (k, v) =>
                evaluateExpression(v, currentCtx, depth + 1).map(_.map(k -> _))
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
                  evaluateExpression(condition, currentCtx, depth + 1).flatMap {
                    case Right(condResult) =>
                      condResult.extractValue.isTruthy
                        .pure[F]
                        .ifM(
                          ifTrue = evaluateExpression(thenBranch, currentCtx, depth + 1),
                          ifFalse = rest match {
                            case Nil              => (NullValue: JsonLogicValue).pure[Result].asRight[JsonLogicException].pure[F]
                            case List(elseBranch) => evaluateExpression(elseBranch, currentCtx, depth + 1)
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

            chargeBaseThen(JsonLogicOp.IfElseOp)(evaluateIfElse(args))

          case ApplyExpression(JsonLogicOp.LetOp, args) =>
            // Both surface forms (array of [name, expr] pairs and the object form
            // {name: expr, ...}) desugar to the same ordered binding list. Bindings are
            // evaluated sequentially, each in the context of previous bindings, and the
            // result expression sees all bindings in scope (mirrors Rust `eval_let`).
            chargeBaseThen(JsonLogicOp.LetOp)(JsonLogicRuntime.normalizeLetArgs(args) match {
              case Right((bindings, resultExpr)) =>
                def processBindings(
                  remaining: List[(String, JsonLogicExpression)],
                  accumulatedBindings: Map[String, JsonLogicValue]
                ): F[Either[JsonLogicException, Result[JsonLogicValue]]] =
                  remaining match {
                    case Nil =>
                      // All bindings processed, evaluate result with accumulated context
                      val letCtx = currentCtx match {
                        case Some(MapValue(existing)) => MapValue(existing ++ accumulatedBindings).some
                        case Some(other)              => MapValue(accumulatedBindings + ("" -> other)).some
                        case None                     => MapValue(accumulatedBindings).some
                      }
                      evaluateExpression(resultExpr, letCtx, depth + 1)

                    case (name, valueExpr) :: rest =>
                      // Evaluate binding expression in context with accumulated bindings
                      val bindingCtx = currentCtx match {
                        case Some(MapValue(existing)) => MapValue(existing ++ accumulatedBindings).some
                        case Some(other)              => MapValue(accumulatedBindings + ("" -> other)).some
                        case None                     => if (accumulatedBindings.isEmpty) None else MapValue(accumulatedBindings).some
                      }
                      evaluateExpression(valueExpr, bindingCtx, depth + 1).flatMap {
                        case Right(valueResult) =>
                          processBindings(rest, accumulatedBindings + (name -> valueResult.extractValue))
                        case Left(error) =>
                          error.asLeft[Result[JsonLogicValue]].pure[F]
                      }
                  }

                processBindings(bindings, Map.empty)

              case Left(error) =>
                error.asLeft[Result[JsonLogicValue]].pure[F]
            })

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
                  evaluateExpression(arg, currentCtx, depth + 1)
                }
            }.flatMap { evaluatedArgs =>
              evaluatedArgs.sequence match {
                case Right(resultArgs) =>
                  // Callback handlers resume nested evaluation from this node's depth.
                  sem.applyOp(op, depth)(resultArgs)
                case Left(error) =>
                  error.asLeft[Result[JsonLogicValue]].pure[F]
              }
            }
        }

    evaluateExpression(program, ctx, baseDepth + 1)
  }

  // Tail-recursive stack-machine evaluation using tailRecM (stack-safe)
  // Default evaluate uses tail-recursive interpret for stack safety.
  // `baseDepth` is the recursion depth already consumed by enclosing runs (callback bodies
  // resume from the invoking node's depth via the semantics' evaluation callback); the
  // program root evaluates at `baseDepth + 1` and the MaxEvalDepth guard applies per node.
  def evaluate[F[_]: Monad, Result[_]: ResultContext](
    program: JsonLogicExpression,
    ctx: Option[JsonLogicValue],
    baseDepth: Int = 0
  )(implicit sem: JsonLogicSemantics[F, Result]): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {

    // Stack frames for tail-recursive evaluation. `depth` on an Eval frame is the recursion
    // depth at which that expression node evaluates (root = baseDepth + 1).
    sealed trait Frame
    case class Eval(expr: JsonLogicExpression, contOpt: Option[Continuation], depth: Int) extends Frame
    case class ApplyValue(value: Result[JsonLogicValue], cont: Continuation) extends Frame

    // `childDepth` is the depth at which this continuation's pending child expressions
    // evaluate (= the owning node's depth + 1); the owning node itself sits at childDepth - 1.
    case class Continuation(
      op: JsonLogicOp,
      processed: List[Result[JsonLogicValue]],
      remaining: List[JsonLogicExpression],
      parent: Option[Continuation],
      isArray: Boolean = false,
      mapKeys: List[String] = List.empty,
      isIfElse: Boolean = false,
      varDefault: Option[JsonLogicValue] = None,
      childDepth: Int = 1
    )

    // Special continuation for let bindings. `childDepth` is the depth of the binding /
    // result expressions (= the let node's depth + 1).
    case class LetContinuation(
      currentName: String,
      remainingBindings: List[JsonLogicExpression],
      resultExpr: JsonLogicExpression,
      accumulatedBindings: Map[String, JsonLogicValue],
      parent: Option[Continuation],
      originalCtx: Option[JsonLogicValue],
      childDepth: Int
    )

    case class EvalLet(expr: JsonLogicExpression, cont: LetContinuation) extends Frame
    case class ApplyLetValue(value: Result[JsonLogicValue], cont: LetContinuation) extends Frame

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
    val initStack: Stack = List(Eval(program, None, baseDepth + 1))

    // `if` / `let` are dispatched lazily below and never reach sem.applyOp, so their flat
    // base cost is charged here, once per node, BEFORE any child is evaluated (no depth
    // penalty: depth is undefined at the lazy dispatch site — see GasAwareSemantics).
    // Untaken branches still cost nothing. No-op for non-metering semantics.
    def chargeBaseThen(op: JsonLogicOp)(
      proceed: => F[Either[Stack, Either[JsonLogicException, Result[JsonLogicValue]]]]
    ): F[Either[Stack, Either[JsonLogicException, Result[JsonLogicValue]]]] =
      sem.chargeBase(op) match {
        case None => proceed
        case Some(charge) =>
          charge.flatMap {
            case Right(()) => proceed
            case Left(err) => err.asLeft[Result[JsonLogicValue]].asRight[Stack].pure[F]
          }
      }

    Monad[F].tailRecM[Stack, Either[JsonLogicException, Result[JsonLogicValue]]](initStack) {
      case Nil =>
        JsonLogicException("Empty stack: no final result!")
          .asLeft[Result[JsonLogicValue]]
          .asRight[Stack]
          .pure[F]

      // MaxEvalDepth guard: one unit per evaluated expression node, exactly like the Rust
      // reference's per-`eval`-call depth cell. Checked before the node does any work.
      case Eval(_, _, depth) :: _ if depth > MaxEvalDepth =>
        depthExceeded[Result[JsonLogicValue]].asRight[Stack].pure[F]

      case EvalLet(_, letCont) :: _ if letCont.childDepth > MaxEvalDepth =>
        depthExceeded[Result[JsonLogicValue]].asRight[Stack].pure[F]

      case Eval(ConstExpression(v), contOpt, _) :: tail =>
        contOpt.continueOrTerminate(v.pure[Result], tail).pure[F]

      case Eval(VarExpression(Left(key), defaultOpt), contOpt, _) :: tail =>
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

      case Eval(VarExpression(Right(keyExpr), defaultOpt), contOpt, depth) :: tail =>
        (Eval(
          keyExpr,
          Some(Continuation(JsonLogicOp.NoOp, Nil, Nil, contOpt, varDefault = defaultOpt, childDepth = depth + 1)),
          depth + 1
        ) :: tail)
          .asLeft[Either[JsonLogicException, Result[JsonLogicValue]]]
          .pure[F]

      case Eval(ArrayExpression(elements), contOpt, depth) :: tail =>
        if (elements.isEmpty) {
          contOpt.continueOrTerminate((ArrayValue(List.empty): JsonLogicValue).pure[Result], tail).pure[F]
        } else {
          val newCont = Continuation(JsonLogicOp.NoOp, Nil, elements.tail, contOpt, isArray = true, childDepth = depth + 1)
          (Eval(elements.head, Some(newCont), depth + 1) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case Eval(MapExpression(map), contOpt, depth) :: tail =>
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
            mapKeys = List(firstKey) ++ remaining.map(_._1),
            childDepth = depth + 1
          )
          (Eval(firstExpr, Some(newCont), depth + 1) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      // Handle LetOp. Two surface forms, both desugared to the same ordered,
      // scope-aware binding pipeline (mirrors Rust `eval_let` + the TS evaluator):
      //   - array form : {"let": [[[name, expr], ...], resultExpr]}
      //   - object form: {"let": [{name: expr, ...},   resultExpr]}
      // Each binding is evaluated sequentially with prior bindings already in scope.
      case Eval(ApplyExpression(JsonLogicOp.LetOp, args), contOpt, depth) :: tail =>
        chargeBaseThen(JsonLogicOp.LetOp) {
          JsonLogicRuntime.normalizeLetArgs(args) match {
            case Right((bindings, resultExpr)) =>
              bindings match {
                case Nil =>
                  // No bindings, just evaluate result (a child of the let node).
                  (Eval(resultExpr, contOpt, depth + 1) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
                case (name, valueExpr) :: rest =>
                  val restPairs = rest.map { case (n, e) => ArrayExpression(ConstExpression(StrValue(n)) :: e :: Nil) }
                  val letCont = LetContinuation(name, restPairs, resultExpr, Map.empty, contOpt, ctx, childDepth = depth + 1)
                  (EvalLet(valueExpr, letCont) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
              }
            case Left(err) =>
              err.asLeft[Result[JsonLogicValue]].asRight[Stack].pure[F]
          }
        }

      // Handle IfElseOp with lazy evaluation of branches
      case Eval(ApplyExpression(JsonLogicOp.IfElseOp, args), contOpt, depth) :: tail =>
        chargeBaseThen(JsonLogicOp.IfElseOp) {
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
              isIfElse = true,
              childDepth = depth + 1
            )
            (Eval(args.head, Some(newCont), depth + 1) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
          }
        }

      case Eval(ApplyExpression(op, args), contOpt, depth) :: tail =>
        if (args.isEmpty) {
          sem.applyOp(op, depth)(Nil).map {
            case Right(res) => contOpt.continueOrTerminate(res, tail)
            case Left(err)  => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
          }
        } else if (JsonLogicRuntime.isCallbackArg(op, 0)) {
          // First arg is callback - check if it's already a FunctionValue constant
          val wrappedCallback: Result[JsonLogicValue] = args.head match {
            case ConstExpression(fv: FunctionValue) => (fv: JsonLogicValue).pure[Result]
            case _                                  => (FunctionValue(args.head): JsonLogicValue).pure[Result]
          }
          val newCont = Continuation(op, List(wrappedCallback), args.tail, contOpt, childDepth = depth + 1)
          if (args.tail.isEmpty) {
            // No more args - apply the operation
            sem.applyOp(op, depth)(List(wrappedCallback)).map {
              case Right(res) => contOpt.continueOrTerminate(res, tail)
              case Left(err)  => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
            }
          } else {
            // More args remain - evaluate next arg
            (Eval(args.tail.head, Some(newCont.copy(remaining = args.tail.tail)), depth + 1) :: tail)
              .asLeft[Either[JsonLogicException, Result[JsonLogicValue]]]
              .pure[F]
          }
        } else {
          val newCont = Continuation(op, Nil, args.tail, contOpt, childDepth = depth + 1)
          (Eval(args.head, Some(newCont), depth + 1) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case ApplyValue(value, cont @ Continuation(_, processed, remaining, parentContOpt, true, _, _, _, childDepth)) :: tail =>
        if (remaining.isEmpty) {
          val arrayValue: Result[JsonLogicValue] = (processed :+ value).sequence.map(arr => ArrayValue(arr): JsonLogicValue)
          parentContOpt.continueOrTerminate(arrayValue, tail).pure[F]
        } else {
          val newCont = cont.copy(processed = processed :+ value, remaining = remaining.tail)
          (Eval(remaining.head, Some(newCont), childDepth) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      case ApplyValue(value, cont @ Continuation(_, processed, remaining, parentContOpt, false, mapKeys, _, _, childDepth)) :: tail
          if mapKeys.nonEmpty =>
        val newProcessed = processed :+ value
        if (remaining.isEmpty) {
          val pairs = mapKeys.zip(newProcessed.map(_.extractValue)).toMap
          parentContOpt.continueOrTerminate((MapValue(pairs): JsonLogicValue).pure[Result], tail).pure[F]
        } else {
          val newCont = cont.copy(processed = newProcessed, remaining = remaining.tail)
          (Eval(remaining.head, Some(newCont), childDepth) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
        }

      // Handle applying a value to IfElse continuation (lazy evaluation). Every chained
      // condition / branch is a direct child of the SAME if node, so they all evaluate at
      // the node's childDepth (matching the Rust eval_if loop).
      case ApplyValue(condValue, Continuation(JsonLogicOp.IfElseOp, _, remaining, parentContOpt, _, _, true, _, childDepth)) :: tail =>
        remaining match {
          case thenBranch :: rest =>
            if (condValue.extractValue.isTruthy) {
              (Eval(thenBranch, parentContOpt, childDepth) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
            } else if (rest.isEmpty) {
              parentContOpt.continueOrTerminate((NullValue: JsonLogicValue).pure[Result], tail).pure[F]
            } else if (rest.length == 1) {
              (Eval(rest.head, parentContOpt, childDepth) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
            } else {
              val newCont = Continuation(
                JsonLogicOp.IfElseOp,
                Nil,
                rest.tail,
                parentContOpt,
                isArray = false,
                mapKeys = List.empty,
                isIfElse = true,
                childDepth = childDepth
              )
              (Eval(rest.head, Some(newCont), childDepth) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
            }

          case Nil =>
            JsonLogicException("If/else malformed: no remaining expressions")
              .asLeft[Result[JsonLogicValue]]
              .asRight[Stack]
              .pure[F]
        }

      case ApplyValue(value, Continuation(JsonLogicOp.NoOp, _, _, parentContOpt, _, _, _, defaultOpt, _)) :: tail =>
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

      case ApplyValue(value, Continuation(op, processed, remaining, parentContOpt, false, _, _, _, childDepth)) :: tail =>
        val newProcessed = processed :+ value
        if (remaining.isEmpty) {
          // Apply at the op node's own depth (childDepth - 1); callback handlers resume
          // nested evaluation from it.
          sem.applyOp(op, childDepth - 1)(newProcessed).map {
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
              sem.applyOp(op, childDepth - 1)(updatedProcessed).map {
                case Right(res) => parentContOpt.continueOrTerminate(res, tail)
                case Left(err)  => err.asLeft[Result[JsonLogicValue]].asRight[Stack]
              }
            } else {
              // More args remain
              val newCont = Continuation(op, updatedProcessed, remaining.tail.tail, parentContOpt, childDepth = childDepth)
              (Eval(remaining.tail.head, Some(newCont), childDepth) :: tail)
                .asLeft[Either[JsonLogicException, Result[JsonLogicValue]]]
                .pure[F]
            }
          } else {
            val newCont = Continuation(op, newProcessed, remaining.tail, parentContOpt, childDepth = childDepth)
            (Eval(remaining.head, Some(newCont), childDepth) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
          }
        }

      // Handle EvalLet: evaluate expression in let context
      case EvalLet(expr, letCont) :: tail =>
        // Build context with accumulated bindings
        val letCtx = letCont.originalCtx match {
          case Some(MapValue(existing)) => MapValue(existing ++ letCont.accumulatedBindings).some
          case Some(other) if letCont.accumulatedBindings.nonEmpty =>
            MapValue(letCont.accumulatedBindings + ("" -> other)).some
          case Some(other) => Some(other)
          case None if letCont.accumulatedBindings.nonEmpty =>
            MapValue(letCont.accumulatedBindings).some
          case None => None
        }
        // Evaluate expression with let context, then apply to let continuation
        expr match {
          case ConstExpression(v) =>
            (ApplyLetValue(v.pure[Result], letCont) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]
          case VarExpression(Left(key), defaultOpt) =>
            sem.getVar(key, letCtx).map {
              case Right(result) =>
                val finalResult = result.extractValue match {
                  case NullValue if key.nonEmpty => defaultOpt.map(_.pure[Result]).getOrElse(result)
                  case _                         => result
                }
                (ApplyLetValue(finalResult, letCont) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]]
              case Left(_) =>
                val finalResult = defaultOpt.map(_.pure[Result]).getOrElse((NullValue: JsonLogicValue).pure[Result])
                (ApplyLetValue(finalResult, letCont) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]]
            }
          case _ =>
            // For complex expressions, evaluate them and capture result. The binding
            // expression is a child of the let node, so the nested run resumes from
            // childDepth - 1 (its root then evaluates at childDepth).
            sem.evaluateWith(expr, letCtx, letCont.childDepth - 1).map {
              case Right(result) =>
                (ApplyLetValue(result, letCont) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]]
              case Left(err) =>
                err.asLeft[Result[JsonLogicValue]].asRight[Stack]
            }
        }

      // Handle ApplyLetValue: process evaluated binding value
      case ApplyLetValue(value, letCont) :: tail =>
        val newBindings = letCont.accumulatedBindings + (letCont.currentName -> value.extractValue)

        letCont.remainingBindings match {
          case Nil =>
            // All bindings processed, evaluate result expression with full context
            val resultCtx = letCont.originalCtx match {
              case Some(MapValue(existing)) => MapValue(existing ++ newBindings).some
              case Some(other)              => MapValue(newBindings + ("" -> other)).some
              case None                     => MapValue(newBindings).some
            }
            // Evaluate result and return to parent continuation. The result expression is a
            // child of the let node (depth = childDepth), so resume from childDepth - 1.
            sem.evaluateWith(letCont.resultExpr, resultCtx, letCont.childDepth - 1).map {
              case Right(result) =>
                letCont.parent.continueOrTerminate(result, tail)
              case Left(err) =>
                err.asLeft[Result[JsonLogicValue]].asRight[Stack]
            }

          case ArrayExpression(ConstExpression(StrValue(nextName)) :: nextValueExpr :: Nil) :: rest =>
            // Process next binding
            val nextLetCont =
              LetContinuation(nextName, rest, letCont.resultExpr, newBindings, letCont.parent, letCont.originalCtx, letCont.childDepth)
            (EvalLet(nextValueExpr, nextLetCont) :: tail).asLeft[Either[JsonLogicException, Result[JsonLogicValue]]].pure[F]

          case invalid :: _ =>
            JsonLogicException(s"let binding must be [name, expr], got: $invalid")
              .asLeft[Result[JsonLogicValue]]
              .asRight[Stack]
              .pure[F]
        }

      case unknown =>
        JsonLogicException(s"Unexpected stack configuration: $unknown")
          .asLeft[Result[JsonLogicValue]]
          .asRight[Stack]
          .pure[F]
    }
  }
}
