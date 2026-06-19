package io.constellationnetwork.metagraph_sdk.json_logic.gas

import cats.syntax.all._
import cats.{Monad, Show}

import io.constellationnetwork.metagraph_sdk.json_logic.core.{JsonLogicException, JsonLogicValue}

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

@derive(encoder, decoder)
case class GasCost(amount: Long) {
  def +(other: GasCost): GasCost = GasCost(amount + other.amount)
  def *(multiplier: Long): GasCost = GasCost(amount * multiplier)
}

object GasCost {
  val Zero: GasCost = GasCost(0L)

  implicit val showInstance: Show[GasCost] =
    Show.show(gc => s"${gc.amount} gas")
}

@derive(encoder, decoder)
case class GasLimit(amount: Long) {
  def canAfford(cost: GasCost): Boolean = amount >= cost.amount

  def consume(cost: GasCost): Either[GasExhaustedException, GasLimit] =
    if (canAfford(cost))
      GasLimit(amount - cost.amount).asRight[GasExhaustedException]
    else
      GasExhaustedException(required = cost, available = this).asLeft[GasLimit]
}

object GasLimit {
  val Unlimited: GasLimit = GasLimit(Long.MaxValue)
  val Default: GasLimit = GasLimit(1_000_000L)

  implicit val showInstance: Show[GasLimit] =
    Show.show(gl => s"${gl.amount} gas limit")
}

@derive(encoder, decoder)
case class GasUsed(amount: Long) {
  def +(cost: GasCost): GasUsed = GasUsed(amount + cost.amount)
  def +(other: GasUsed): GasUsed = GasUsed(amount + other.amount)
}

object GasUsed {
  val Zero: GasUsed = GasUsed(0L)

  implicit val showInstance: Show[GasUsed] =
    Show.show(gu => s"${gu.amount} gas used")
}

@derive(encoder, decoder)
case class GasExhaustedException(
  required: GasCost,
  available: GasLimit
) extends JsonLogicException(
      s"Gas exhausted: required ${required.amount}, available ${available.amount}"
    )

@derive(encoder, decoder)
case class GasConfig(
  ifElse: GasCost = GasCost(10),
  default: GasCost = GasCost(5),
  not: GasCost = GasCost(1),
  doubleNot: GasCost = GasCost(1),
  or: GasCost = GasCost(2),
  and: GasCost = GasCost(2),
  eq: GasCost = GasCost(3),
  eqStrict: GasCost = GasCost(2),
  neq: GasCost = GasCost(3),
  neqStrict: GasCost = GasCost(2),
  lt: GasCost = GasCost(3),
  leq: GasCost = GasCost(3),
  gt: GasCost = GasCost(3),
  geq: GasCost = GasCost(3),
  add: GasCost = GasCost(5),
  minus: GasCost = GasCost(5),
  times: GasCost = GasCost(8),
  div: GasCost = GasCost(10),
  modulo: GasCost = GasCost(10),
  max: GasCost = GasCost(5),
  min: GasCost = GasCost(5),
  abs: GasCost = GasCost(2),
  round: GasCost = GasCost(3),
  floor: GasCost = GasCost(3),
  ceil: GasCost = GasCost(3),
  pow: GasCost = GasCost(20),
  // hex_to_int: parse a hex string -> unsigned big-endian BigInt. Priced identically to `modulo`
  // (a single bounded numeric transform; no per-element scaling, so no input-scaled term).
  hexToInt: GasCost = GasCost(10),
  map: GasCost = GasCost(10),
  filter: GasCost = GasCost(10),
  reduce: GasCost = GasCost(15),
  merge: GasCost = GasCost(5),
  all: GasCost = GasCost(10),
  some: GasCost = GasCost(10),
  none: GasCost = GasCost(10),
  find: GasCost = GasCost(10),
  count: GasCost = GasCost(5),
  in: GasCost = GasCost(8),
  intersect: GasCost = GasCost(15),
  unique: GasCost = GasCost(20),
  slice: GasCost = GasCost(5),
  reverse: GasCost = GasCost(5),
  flatten: GasCost = GasCost(10),
  cat: GasCost = GasCost(5),
  substr: GasCost = GasCost(8),
  lower: GasCost = GasCost(3),
  upper: GasCost = GasCost(3),
  join: GasCost = GasCost(10),
  split: GasCost = GasCost(15),
  trim: GasCost = GasCost(5),
  startsWith: GasCost = GasCost(5),
  endsWith: GasCost = GasCost(5),
  mapValues: GasCost = GasCost(5),
  mapKeys: GasCost = GasCost(5),
  get: GasCost = GasCost(3),
  has: GasCost = GasCost(3),
  // set/unset: build a NEW map = input with one key added/replaced/removed. Priced identically to
  // `merge` (GasCost(5)) — the sibling map-combining op — as a single bounded structural transform.
  set: GasCost = GasCost(5),
  unset: GasCost = GasCost(5),
  entries: GasCost = GasCost(10),
  length: GasCost = GasCost(1),
  exists: GasCost = GasCost(5),
  missing: GasCost = GasCost(10),
  missingSome: GasCost = GasCost(15),
  typeOf: GasCost = GasCost(1),
  // Charging schedule (see GasAwareSemantics for the mechanics). Every operation is charged
  // EXACTLY ONCE against the shared gas ref:
  //   base(op) + depthPenalty(depth) + inputScaledCost(args) [+ outputScaledCost(result)].
  // Children pay for themselves when they are evaluated; ancestors never re-charge their subtree.
  //
  // Control flow (`if` / `let`, both priced by `ifElse`). The runtime dispatches these LAZILY
  // and they never reach applyOp, so the runtime charges their flat base cost once per node at
  // the dispatch site, BEFORE any child is evaluated. They carry NO depth penalty: the penalty's
  // input everywhere else is max(evaluated-child metric depth) + 1, which is undefined at the
  // lazy dispatch site (children unevaluated; if/let are depth-transparent in the metrics flow),
  // so base-only is the deliberate, documented choice. Evaluated children (condition, bindings,
  // the taken branch) pay for themselves as usual; untaken branches pay nothing.
  // The base, depth, and input-scaled components are consumed BEFORE the primitive runs, so
  // out-of-gas is raised before any input-scaled work (Miller loops, BLS aggregation, proof
  // folds, string building) is performed. Only residual components that are observable solely on
  // the produced value (split piece count, merge/flatten/slice output size, substr output length)
  // are consumed after the primitive; the work they price is bounded by already-paid-for inputs.
  //
  // ZK / crypto opcodes. Costs are set relative to real compute and are the DoS bound for the VM:
  //   - groth16Verify is by far the most expensive (a BN254 pairing product + final exponentiation),
  //   - ecvrf is high (Ed25519 scalar muls + hash-to-curve),
  //   - pmtVerify is a flat base plus a per-sibling cost (pmtPerSibling) pre-charged from the
  //     proof's sibling count in the gas-aware layer before any hashing runs, since cost scales
  //     with path length,
  //   - poseidon is a flat base plus a per-input cost (poseidonPerInput) pre-charged from the
  //     input count before the permutation runs, since each input widens the permutation.
  poseidon: GasCost = GasCost(150),
  poseidonPerInput: GasCost = GasCost(150),
  pmtVerify: GasCost = GasCost(200),
  pmtPerSibling: GasCost = GasCost(300),
  groth16Verify: GasCost = GasCost(250_000),
  ecvrfVerify: GasCost = GasCost(50_000),
  // ZK / crypto opcodes -- second wave (BN254 curve, BLS12-381, Schnorr). Costs follow the
  // wave-1 scale (groth16Verify = 250k, ecvrfVerify = 50k) and are the DoS bound for the VM:
  //   - bn254Pairing is the most expensive: a flat base plus a per-pair cost (bn254PairingPerPair,
  //     pre-charged from the pairs count in the gas-aware layer BEFORE any Miller loop runs), since
  //     each pair adds a Miller loop; the final exponentiation is amortized once across the product,
  //   - blsVerify / blsAggregateVerify are high (hash-to-curve + two pairings); aggregation adds a
  //     per-key cost (blsAggregatePerKey, pre-charged from the key count before any key is summed)
  //     for each extra public key summed into the aggregate,
  //   - schnorrVerify is medium (two BN254 scalar multiplications + a point add + a SHA-256),
  //   - bn254Mul (a scalar multiplication) is far more expensive than bn254Add (a single point add).
  bn254Add: GasCost = GasCost(500),
  bn254Mul: GasCost = GasCost(40_000),
  bn254Pairing: GasCost = GasCost(45_000),
  bn254PairingPerPair: GasCost = GasCost(35_000),
  blsVerify: GasCost = GasCost(120_000),
  blsAggregateVerify: GasCost = GasCost(120_000),
  blsAggregatePerKey: GasCost = GasCost(15_000),
  schnorrVerify: GasCost = GasCost(45_000),
  // ZK / crypto opcodes -- third wave (clear-text authenticated databases: SMT + MPT). These run an
  // authentication-path / witness fold that hashes one canonical-JSON commitment per node, so cost is
  // a flat base plus a per-element charge (pre-charged from the proof shape in the gas-aware layer
  // before the fold runs):
  //   - smt_verify cost scales with the proof DEPTH (#siblings on the authentication path),
  //   - mpt_verify cost scales with the #nodes in the proof witness,
  //   - mpt_prefix_verify cost scales with the #entries proven complete under the prefix.
  // The scale sits between pmt_verify (Poseidon path) and the pairing ops: each per-element step is a
  // canonical-bytes SHA-256, materially cheaper than a Miller loop but dearer than a Poseidon round.
  smtVerify: GasCost = GasCost(500),
  smtPerSibling: GasCost = GasCost(400),
  mptVerify: GasCost = GasCost(500),
  mptPerNode: GasCost = GasCost(400),
  mptPrefixVerify: GasCost = GasCost(1_000),
  mptPrefixPerEntry: GasCost = GasCost(800),
  // ZK / crypto opcodes -- sigma protocols (classical no-trusted-setup Ergo/EIP-11 family). Both
  // are standalone single-leaf Σ-guards on BN254 G1 and follow the wave-2 scale:
  //   - proveDlogVerify is the DLog leaf, priced IDENTICALLY to schnorrVerify (it is a thin alias:
  //     two BN254 scalar muls + a point add + a SHA-256),
  //   - proveDhTupleVerify is the DDH / Diffie-Hellman-tuple leaf, ~2x schnorr: it does FOUR BN254
  //     scalar muls (z·g, z·h, e·u, e·v) + two point adds + one SHA-256 over the bound transcript.
  // Both carry a fixed arity (no variadic per-element scaling), so the base cost is pre-charged
  // from the op alone in the gas-aware layer (no input-scaled term), exactly like schnorrVerify.
  proveDlogVerify: GasCost = GasCost(45_000),
  proveDhtupleVerify: GasCost = GasCost(85_000),
  // sigma_verify -- the recursive CDS ring/threshold tree verifier. Unlike the fixed-arity leaves,
  // its cost is the SHAPE of the proposition tree, pre-charged from the (already-evaluated) tree in
  // the gas-aware layer BEFORE any curve arithmetic runs (the DoS bound):
  //   total = sigmaVerify (base, incl. one root SHA-256 over the serialized tree, whose length is
  //           bounded by the leaf+node counts already charged)
  //         + per-DLog-leaf    (sigmaVerifyPerDlogLeaf    ~ proveDlogVerify: 2 muls + 1 add)
  //         + per-DHTuple-leaf (sigmaVerifyPerDhtupleLeaf ~ proveDhtupleVerify: 4 muls + 2 adds)
  //         + per-connective node (sigmaVerifyPerNode: AND challenge copy / OR XOR fold /
  //           THRESHOLD GF(2^8) interpolation; the per-child interpolation term is folded into the
  //           per-leaf/per-node walk, dominated by the curve work above).
  sigmaVerify: GasCost = GasCost(45_000),
  sigmaVerifyPerDlogLeaf: GasCost = GasCost(45_000),
  sigmaVerifyPerDhtupleLeaf: GasCost = GasCost(85_000),
  sigmaVerifyPerNode: GasCost = GasCost(2_000),
  const: GasCost = GasCost.Zero,
  varAccess: GasCost = GasCost(2),
  depthPenaltyMultiplier: Long = 5L,
  collectionSizeMultiplier: Long = 1L
) {
  def depthPenalty(depth: Long): GasCost = GasCost(depth * depthPenaltyMultiplier)
  def sizeCost(size: Long): GasCost = GasCost(size * collectionSizeMultiplier)
}

object GasConfig {
  val Default: GasConfig = GasConfig()

  val Dev: GasConfig = GasConfig().copy(
    map = GasCost(5),
    filter = GasCost(5),
    reduce = GasCost(8)
  )

  val Mainnet: GasConfig = GasConfig().copy(
    pow = GasCost(50),
    unique = GasCost(30),
    split = GasCost(25),
    reduce = GasCost(20),
    depthPenaltyMultiplier = 10L
  )
}

@derive(encoder, decoder)
case class EvaluationResult[A](
  value: A,
  gasUsed: GasUsed,
  maxDepth: Int,
  operationCount: Long
)

object EvaluationResult {
  def pure[A](value: A): EvaluationResult[A] =
    EvaluationResult(value, GasUsed.Zero, maxDepth = 0, operationCount = 0)

  def withCost[A](value: A, cost: GasCost, depth: Int = 0): EvaluationResult[A] =
    EvaluationResult(value, GasUsed(cost.amount), maxDepth = depth, operationCount = 1)

  implicit def showInstance[A: Show]: Show[EvaluationResult[A]] = Show.show { result =>
    s"EvaluationResult(value=${result.value.show}, gas=${result.gasUsed.amount}, depth=${result.maxDepth}, ops=${result.operationCount})"
  }
}

object GasTracking {

  object syntax {
    implicit class GasTrackingOps[F[_]: Monad](operation: F[Either[JsonLogicException, JsonLogicValue]]) {

      def withGas(
        costFn: GasConfig => GasCost,
        gasLimit: GasLimit,
        gasConfig: GasConfig,
        depth: Int
      )(costModifier: JsonLogicValue => GasCost = _ => GasCost.Zero): F[Either[JsonLogicException, (JsonLogicValue, GasCost, Int, Int)]] = {
        val baseCost = costFn(gasConfig) + gasConfig.depthPenalty(depth.toLong)

        gasLimit
          .consume(baseCost)
          .fold(
            err => (err: JsonLogicException).asLeft[(JsonLogicValue, GasCost, Int, Int)].pure[F],
            _ =>
              operation.map(_.map { value =>
                val total = baseCost + costModifier(value)
                (value, total, depth, 1)
              })
          )
      }
    }

    implicit class PreValidatedOps(args: List[JsonLogicValue]) {

      def validateThen[F[_]: Monad](
        errorCheck: List[JsonLogicValue] => Option[JsonLogicException]
      )(
        operation: List[JsonLogicValue] => F[Either[JsonLogicException, JsonLogicValue]]
      ): F[Either[JsonLogicException, JsonLogicValue]] =
        errorCheck(args).fold(operation(args))(err => err.asLeft[JsonLogicValue].pure[F])
    }
  }
}
