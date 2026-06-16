package io.constellationnetwork.metagraph_sdk.json_logic.gas

import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicOp._
import io.constellationnetwork.metagraph_sdk.json_logic.core._

/**
 * Static, execution-free gas estimation over a JSON-Logic expression.
 *
 * Walks the expression tree and sums the per-op base cost, depth penalty, and var-access cost.
 * It deliberately does NOT account for the input/output-SCALED components of the schedule
 * (collection element counts, proof/witness shapes, produced string lengths, AND the variadic
 * `(n-1)` charge on `+`/`*`/`-`). Most of these are data-dependent; a couple are statically
 * derivable, but ALL are omitted on purpose, to keep this a pure `base + depthPenalty + varCost`
 * walk that does not duplicate the evaluator's per-op scaling rules (no second copy to drift).
 * Therefore `estimate` is:
 *
 *   - EXACT for expressions whose ops carry no scaling — the large common class: control flow,
 *     logic, comparison, get/has/typeof, and var/const access; and
 *   - a FLOOR (under-count) for ops that scale (arithmetic, collections, crypto, string building),
 *     since their scaled term is omitted.
 *
 * Along the control-flow dimension it is conservative: lazy `if` is modelled as
 * `base + sum(conditions) + max(values)` — the single worst branch, never the sum of all branches.
 *
 * The omitted scaled terms are recoverable by the caller when the data is in hand (the event
 * payload + current state size the input collections; proof shape sizes the crypto ops); only
 * intermediate/derived collection sizes need execution. The authoritative charge is always the
 * metered evaluation — this is a cheap pre-flight "ballpark how much might this cost" quote that
 * never runs the VM.
 *
 * The per-op cost table (`baseCost`) is the SINGLE SOURCE OF TRUTH shared with the evaluator
 * (`GasAwareSemantics.getOpCost` delegates here), so an estimate can never drift from the real
 * charge.
 */
object JsonLogicGasEstimator {

  /** Flat per-op base cost. Shared by the evaluator (charge) and the estimator (quote). */
  def baseCost(op: JsonLogicOp)(config: GasConfig): GasCost = op match {
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
    case ProveDlogVerifyOp    => config.proveDlogVerify
    case ProveDhTupleVerifyOp => config.proveDhtupleVerify
    case SigmaVerifyOp        => config.sigmaVerify
  }

  /**
   * Static gas estimate for `expr` under `config`. See the object doc for exactly what is and is
   * not counted. The result's `cost` is the quote; `depth`/`opCount` are the structural metrics.
   */
  def estimate(expr: JsonLogicExpression, config: GasConfig): GasMetrics =
    expr match {
      case ConstExpression(_) =>
        GasMetrics.zero

      case VarExpression(Left(key), _) =>
        // getVar charges varAccess + one unit per dot-path segment; no depth penalty.
        GasMetrics(config.varAccess + GasCost(key.split("\\.").length.toLong), depth = 0, opCount = 1)

      case VarExpression(Right(inner), _) =>
        // Dynamic var key is itself an expression; the segment count is unknown, charge one unit.
        val i = estimate(inner, config)
        GasMetrics(config.varAccess + GasCost(1L) + i.cost, i.depth, i.opCount + 1)

      case ArrayExpression(items) =>
        items.foldLeft(GasMetrics.zero)((acc, e) => acc.combine(estimate(e, config)))

      case MapExpression(fields) =>
        fields.values.foldLeft(GasMetrics.zero)((acc, e) => acc.combine(estimate(e, config)))

      case ApplyExpression(IfElseOp, args) =>
        // Lazy control flow: every condition may run, but only ONE value branch is taken.
        // Worst executed path = base + sum(conditions) + max(values). No depth penalty (the
        // dispatch is depth-transparent — mirrors GasAwareSemantics.chargeBase).
        // args = [cond1, val1, cond2, val2, ..., (default)]
        val pairs = args.grouped(2).toList
        val conds = pairs.collect { case List(c, _) => estimate(c, config) }
        val values = pairs.collect { case List(_, v) => estimate(v, config) } ++
          pairs.collect { case List(d) => estimate(d, config) }
        val condCost = conds.foldLeft(GasMetrics.zero)(_.combine(_))
        val branch = values.maxByOption(_.cost.amount).getOrElse(GasMetrics.zero)
        GasMetrics.single(baseCost(IfElseOp)(config), depth = 0).combine(condCost).combine(branch)

      case ApplyExpression(LetOp, args) =>
        // Bindings and body are all evaluated; base charged once, no depth penalty (control flow).
        val children = args.foldLeft(GasMetrics.zero)((acc, e) => acc.combine(estimate(e, config)))
        GasMetrics.single(baseCost(LetOp)(config), depth = 0).combine(children)

      case ApplyExpression(op, args) =>
        // Eager op: newDepth = max(child depth) + 1; charge base + depthPenalty(newDepth).
        // Children already accounted their own cost (charge-once), so add their aggregate.
        val children = args.map(estimate(_, config))
        val childDepth = if (children.isEmpty) 0 else children.map(_.depth).max
        val newDepth = childDepth + 1
        val childAgg = children.foldLeft(GasMetrics.zero)(_.combine(_))
        val nodeCost = baseCost(op)(config) + config.depthPenalty(newDepth.toLong)
        GasMetrics(nodeCost + childAgg.cost, newDepth, childAgg.opCount + 1)
    }
}
