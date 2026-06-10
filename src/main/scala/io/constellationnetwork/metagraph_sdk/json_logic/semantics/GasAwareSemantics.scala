package io.constellationnetwork.metagraph_sdk.json_logic.semantics

import cats.effect.{Ref, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicOp._
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasConfig, GasCost, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.ops.NumericOps.floatToPlainString
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.ResultContext

/**
 * Gas-aware semantics: meters every operation against a shared gas `Ref`.
 *
 * Charging contract (charge-once, pre-charge):
 *   - Each operation consumes EXACTLY ONCE from the gas ref:
 *     `baseCost(op) + depthPenalty(depth) + inputScaledCost(op, args) [+ outputScaledCost(op, result)]`.
 *     Child sub-results have already consumed their own cost while they were evaluated; an
 *     ancestor never re-consumes its subtree (no compounding with depth).
 *   - `baseCost + depthPenalty + inputScaledCost` is consumed BEFORE the underlying primitive
 *     runs, so out-of-gas is raised before any input-scaled work (pairings, BLS aggregation,
 *     proof folds, string building) is performed.
 *   - `outputScaledCost` is the residual component that can only be observed on the produced
 *     value (split piece count, flatten/slice/merge output size, substr output length). It is
 *     consumed AFTER the primitive; the work it prices is bounded by already-paid-for inputs.
 *   - The lazily-dispatched control-flow ops (`if` / `let`) never reach `applyOp`; the runtime
 *     charges their flat base cost (NO depth penalty — depth is undefined at the lazy dispatch
 *     site) via `chargeBase` once per node, before any child is evaluated. Untaken branches
 *     still cost nothing.
 *   - Variable accesses consume `varAccess + pathSegments` at lookup time.
 *   - Total consumption (the gas-ref delta) is the authoritative gasUsed reported by the
 *     evaluator.
 */
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

      /** Atomically consume `cost` from the shared gas ref, or fail with GasExhaustedException. */
      private def consumeGas(cost: GasCost): F[Either[JsonLogicException, Unit]] =
        gasLimitRef.modify { limit =>
          limit.consume(cost) match {
            case Right(newLimit) => (newLimit, ().asRight[JsonLogicException])
            case Left(err)       => (limit, (err: JsonLogicException).asLeft[Unit])
          }
        }

      /**
       * Flat per-node base charge for the lazily-dispatched control-flow ops (`if` / `let`),
       * consumed by the runtime at the dispatch site BEFORE any child is evaluated. These ops
       * never reach `applyOp`, and their depth (max evaluated-child metric depth + 1, the input
       * to `depthPenalty` everywhere else) is undefined at dispatch by construction — children
       * are unevaluated and if/let are depth-transparent in the metrics flow — so the charge is
       * the base cost ONLY, with no depth penalty (see the schedule comment in GasConfig).
       * Evaluated children (condition / bindings / taken branch) still pay for themselves;
       * untaken branches still pay nothing.
       */
      override def chargeBase(op: JsonLogicOp): Option[F[Either[JsonLogicException, Unit]]] =
        Some(consumeGas(getOpCost(op)(gasConfig)))

      override def getVar(
        key: String,
        ctx: Option[JsonLogicValue] = None
      ): F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] = {
        val varCost = gasConfig.varAccess + GasCost(key.split("\\.").length.toLong)
        // The lookup itself is the work being priced, so consume from the gas ref here
        // (exactly once); ancestors never re-consume it.
        consumeGas(varCost).flatMap {
          case Left(err) => err.asLeft[ResultContext.WithGas[JsonLogicValue]].pure[F]
          case Right(()) =>
            baseSemantics
              .getVar(key, ctx)
              .map(_.map {
                case (value, metrics) =>
                  (value, metrics.withCost(varCost))
              })
        }
      }

      override def applyOp(
        op: JsonLogicOp
      ): List[ResultContext.WithGas[JsonLogicValue]] => F[Either[JsonLogicException, ResultContext.WithGas[JsonLogicValue]]] =
        args => {
          val argValues = args.map { case (value, _) => value }
          val argMaxDepth = if (args.isEmpty) 0 else args.map(_._2.depth).max
          val newDepth = argMaxDepth + 1
          val depthPenalty = gasConfig.depthPenalty(newDepth.toLong)
          // Everything derivable from the (already evaluated, already paid-for) inputs is
          // pre-charged BEFORE the primitive runs: out-of-gas must be raised before any
          // input-scaled work (Miller loops, BLS key aggregation, proof folds, string
          // concatenation) is performed. Children consumed their own cost while they were
          // evaluated, so it is NOT re-consumed here (charge-once).
          val preCost = getOpCost(op)(gasConfig) + depthPenalty + getInputScaledCost(op, argValues)

          consumeGas(preCost).flatMap {
            case Left(err) => err.asLeft[ResultContext.WithGas[JsonLogicValue]].pure[F]
            case Right(()) =>
              baseSemantics.applyOp(op)(args).flatMap {
                case Right((value, metrics)) =>
                  // Residual component only observable on the produced value (e.g. split piece
                  // count); the work it prices is bounded by inputs that were already paid for.
                  val outputCost = getOutputScaledCost(op, value)
                  consumeGas(outputCost).map {
                    case Left(err) => err.asLeft[ResultContext.WithGas[JsonLogicValue]]
                    case Right(()) =>
                      (value, metrics.withCost(preCost + outputCost).withDepth(newDepth))
                        .asRight[JsonLogicException]
                  }
                case Left(err) => err.asLeft[ResultContext.WithGas[JsonLogicValue]].pure[F]
              }
          }
        }

      private def getOpCost(op: JsonLogicOp)(config: GasConfig): GasCost = op match {
        case NoOp                 => GasCost.Zero
        case MissingNoneOp        => config.exists
        case ExistsOp             => config.exists
        case MissingSomeOp        => config.missingSome
        case IfElseOp             => config.ifElse
        case LetOp                => config.ifElse // Similar cost to if/else (control flow)
        case EqOp                 => config.eq
        case EqStrictOp           => config.eqStrict
        case NEqOp                => config.neq
        case NEqStrictOp          => config.neqStrict
        case NotOp                => config.not
        case NOp                  => config.doubleNot
        case OrOp                 => config.or
        case AndOp                => config.and
        case Lt                   => config.lt
        case Leq                  => config.leq
        case Gt                   => config.gt
        case Geq                  => config.geq
        case ModuloOp             => config.modulo
        case MaxOp                => config.max
        case MinOp                => config.min
        case AddOp                => config.add
        case TimesOp              => config.times
        case MinusOp              => config.minus
        case DivOp                => config.div
        case MergeOp              => config.merge
        case InOp                 => config.in
        case CatOp                => config.cat
        case SubStrOp             => config.substr
        case MapOp                => config.map
        case FilterOp             => config.filter
        case ReduceOp             => config.reduce
        case AllOp                => config.all
        case NoneOp               => config.none
        case SomeOp               => config.some
        case MapValuesOp          => config.mapValues
        case MapKeysOp            => config.mapKeys
        case GetOp                => config.get
        case IntersectOp          => config.intersect
        case CountOp              => config.count
        case LengthOp             => config.length
        case FindOp               => config.find
        case LowerOp              => config.lower
        case UpperOp              => config.upper
        case JoinOp               => config.join
        case SplitOp              => config.split
        case DefaultOp            => config.default
        case UniqueOp             => config.unique
        case SliceOp              => config.slice
        case ReverseOp            => config.reverse
        case FlattenOp            => config.flatten
        case TrimOp               => config.trim
        case StartsWithOp         => config.startsWith
        case EndsWithOp           => config.endsWith
        case AbsOp                => config.abs
        case RoundOp              => config.round
        case FloorOp              => config.floor
        case CeilOp               => config.ceil
        case PowOp                => config.pow
        case HasOp                => config.has
        case EntriesOp            => config.entries
        case TypeOfOp             => config.typeOf
        case PoseidonOp           => config.poseidon
        case PmtVerifyOp          => config.pmtVerify
        case Groth16VerifyOp      => config.groth16Verify
        case EcVrfVerifyOp        => config.ecvrfVerify
        case Bn254AddOp           => config.bn254Add
        case Bn254MulOp           => config.bn254Mul
        case Bn254PairingOp       => config.bn254Pairing
        case BlsVerifyOp          => config.blsVerify
        case BlsAggregateVerifyOp => config.blsAggregateVerify
        case SchnorrVerifyOp      => config.schnorrVerify
        case SmtVerifyOp          => config.smtVerify
        case MptVerifyOp          => config.mptVerify
        case MptPrefixVerifyOp    => config.mptPrefixVerify
      }

      /**
       * Length of the string a value coerces to in `cat` / `join` (mirrors handleCatOp's
       * coercion and handleJoinOp's arrayToString). Collections / functions price at zero:
       * `cat` rejects them and `join` renders them as the empty string.
       */
      private def coercedStringLength(value: JsonLogicValue): Long = value match {
        case NullValue         => 0L
        case BoolValue(value)  => value.toString.length.toLong
        case IntValue(value)   => value.toString.length.toLong
        case FloatValue(value) => floatToPlainString(value).length.toLong
        case StrValue(value)   => value.length.toLong
        case _                 => 0L
      }

      /**
       * Size-scaled cost derivable from the argument values ALONE. Consumed BEFORE the
       * primitive runs, so out-of-gas is raised before the input-scaled work is performed.
       * Output sizes that are exactly determined by the inputs (cat / join string length,
       * entries count) are re-derived from the inputs and charged here as well.
       */
      private def getInputScaledCost(op: JsonLogicOp, args: List[JsonLogicValue]): GasCost =
        op match {
          // cat output length == sum of the coerced input string lengths; charge it up front.
          case CatOp =>
            gasConfig.sizeCost(args.map(coercedStringLength).sum)
          // join output length == sum of coerced element lengths + separators; charge it up front.
          case JoinOp =>
            args match {
              case ArrayValue(arr) :: StrValue(separator) :: Nil =>
                gasConfig.sizeCost(arr.map(coercedStringLength).sum + separator.length.toLong * Math.max(0, arr.size - 1).toLong)
              case _ => GasCost.Zero
            }
          // entries produces exactly one [key, value] pair per map entry.
          case EntriesOp =>
            args match {
              case MapValue(m) :: Nil => gasConfig.sizeCost(m.size.toLong * 2)
              case _                  => GasCost.Zero
            }
          case UniqueOp =>
            args match {
              case ArrayValue(arr) :: Nil => gasConfig.sizeCost(arr.size.toLong)
              case _                      => GasCost.Zero
            }
          case PowOp =>
            args match {
              case _ :: IntValue(exp) :: Nil   => GasCost(exp.abs.toLong)
              case _ :: FloatValue(exp) :: Nil => GasCost(exp.numerator.abs.toLong)
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
          case MapValuesOp | MapKeysOp =>
            args match {
              case MapValue(m) :: Nil => gasConfig.sizeCost(m.size.toLong)
              case _                  => GasCost.Zero
            }
          // poseidon cost scales with the number of inputs (the permutation width = #inputs + 1).
          case PoseidonOp =>
            args match {
              case ArrayValue(arr) :: Nil => gasConfig.poseidonPerInput * arr.size.toLong
              case list                   => gasConfig.poseidonPerInput * list.size.toLong
            }
          // pmt_verify cost scales with path length (= number of siblings).
          case PmtVerifyOp =>
            args match {
              case _ :: _ :: _ :: ArrayValue(siblings) :: Nil => gasConfig.pmtPerSibling * siblings.size.toLong
              case _                                          => GasCost.Zero
            }
          // bn254_pairing cost scales with the number of (G1, G2) pairs (each adds a Miller loop).
          // Mirror CryptoOps.bn254Pairing's single-vs-list disambiguation: a lone
          // ArrayValue is the pairs list only when every element is itself a pair (array).
          case Bn254PairingOp =>
            args match {
              case ArrayValue(pairs) :: Nil if pairs.forall(_.isInstanceOf[ArrayValue]) =>
                gasConfig.bn254PairingPerPair * pairs.size.toLong
              case list => gasConfig.bn254PairingPerPair * list.size.toLong
            }
          // bls_aggregate_verify cost scales with the number of public keys summed into the aggregate.
          case BlsAggregateVerifyOp =>
            args match {
              case ArrayValue(pks) :: _ :: _ :: Nil => gasConfig.blsAggregatePerKey * pks.size.toLong
              case _                                => GasCost.Zero
            }
          // smt_verify cost scales with the authentication-path depth (#siblings in the proof).
          case SmtVerifyOp =>
            args match {
              case _ :: MapValue(proof) :: Nil =>
                proof.get("siblings") match {
                  case Some(ArrayValue(siblings)) => gasConfig.smtPerSibling * siblings.size.toLong
                  case _                          => GasCost.Zero
                }
              case _ => GasCost.Zero
            }
          // mpt_verify cost scales with the number of nodes in the proof witness.
          case MptVerifyOp =>
            args match {
              case _ :: _ :: _ :: MapValue(proof) :: Nil =>
                proof.get("witness") match {
                  case Some(ArrayValue(witness)) => gasConfig.mptPerNode * witness.size.toLong
                  case _                         => GasCost.Zero
                }
              case _ => GasCost.Zero
            }
          // mpt_prefix_verify cost scales with the number of entries proven complete under the prefix.
          case MptPrefixVerifyOp =>
            args match {
              case _ :: _ :: MapValue(entries) :: _ :: Nil => gasConfig.mptPrefixPerEntry * entries.size.toLong
              case _                                       => GasCost.Zero
            }
          case _ => GasCost.Zero
        }

      /**
       * Residual size-scaled cost that is only observable on the PRODUCED value and cannot be
       * derived from the inputs without re-doing the op (split piece count depends on string
       * content; merge / flatten / slice / substr output sizes depend on clamping or collision
       * behavior). Consumed AFTER the primitive runs; the work these ops perform is linear in
       * inputs that were already evaluated and paid for, so nothing unbounded runs un-charged.
       */
      private def getOutputScaledCost(op: JsonLogicOp, result: JsonLogicValue): GasCost =
        op match {
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
          case SubStrOp =>
            result match {
              case StrValue(s) => gasConfig.sizeCost(s.length.toLong)
              case _           => GasCost.Zero
            }
          case _ => GasCost.Zero
        }
    }
  }
}
