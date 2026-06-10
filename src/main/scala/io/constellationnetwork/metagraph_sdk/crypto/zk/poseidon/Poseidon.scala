package io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon

import scala.annotation.tailrec

/**
 * Poseidon hash over the BN254 / alt_bn128 scalar field (Fr), matching the
 * circomlib / iden3 reference implementation EXACTLY.
 *
 * This is the de-facto Ethereum-ecosystem Poseidon for BN254 and is byte-for-byte
 * compatible with `circomlibjs`' `poseidon(...)` (the unoptimized reference path in
 * `src/poseidon_reference.js`) and with circom's `Poseidon` template.
 *
 * Construction:
 *   - S-box: x^5
 *   - RF = 8 full rounds, RP partial rounds (RP varies by width t, see
 *     [[PoseidonConstants.partialRounds]])
 *   - Round constants `C` and MDS matrix `M` sourced from circomlib (see
 *     [[PoseidonConstants]])
 *
 * Semantics (identical to circomlib's `poseidon([...])`):
 *   - hashing `n` inputs uses width `t = n + 1`
 *   - the state is initialised as `[0, in_0, in_1, ..., in_{n-1}]`, i.e. the
 *     capacity element (state[0]) starts at 0
 *   - the permutation is applied once and `state[0]` is returned
 *
 * All arithmetic is over Fr, i.e. `BigInt` reduced modulo the field modulus
 * [[Poseidon.R]].
 *
 * ==Security: PUBLIC INPUTS ONLY — not constant-time==
 *
 * `BigInt` (JVM `BigInteger`) arithmetic is variable-time in its operand
 * values, so this implementation leaks input-dependent timing. That is fine
 * for every current call site (the `poseidon` / `pmt_verify` JLVM opcodes and
 * Merkle building, which only ever hash caller-supplied PUBLIC data), but it
 * means this function must NEVER be used to hash secrets (note values,
 * nullifier keys, private commitments). Secret-side Poseidon hashing belongs
 * in the SP1 prover stack (metakit-sdk `rust/zk-shielded`), which runs on the
 * prover's own machine.
 */
object Poseidon {

  /**
   * The BN254 (alt_bn128) scalar field modulus `R`. Fr arithmetic is just
   * `BigInt` reduced modulo `R`. This is the same scalar field used by the
   * Groth16 verification machinery in the wider `crypto.zk` package.
   */
  val R: BigInt =
    BigInt("21888242871839275222246405745257275088548364400416034343698204186575808495617")

  /** Largest input width (t) for which circomlib constants are bundled here. */
  private val MaxWidth: Int = PoseidonConstants.partialRounds.keys.max

  /** Largest number of inputs [[hash]] supports (width t = inputs + 1). */
  val MaxInputs: Int = MaxWidth - 1

  /**
   * Hash a sequence of field elements with circomlib semantics.
   *
   * @param inputs the inputs to hash. Must be non-empty and no longer than
   *               `MaxWidth - 1` (the widths for which constants are bundled).
   *               Each input must already be a canonical field element, i.e.
   *               `0 <= input < R`; out-of-range inputs are rejected (this
   *               mirrors circomlib, which only operates on canonical Fr
   *               elements and treats >= R as invalid).
   * @return the Poseidon hash as a canonical field element in `[0, R)`.
   */
  def hash(inputs: Seq[BigInt]): BigInt = {
    require(inputs.nonEmpty, "Poseidon.hash requires at least one input")
    val t = inputs.length + 1
    require(
      t <= MaxWidth,
      s"Poseidon.hash supports at most ${MaxWidth - 1} inputs (width t <= $MaxWidth); got ${inputs.length}"
    )
    inputs.zipWithIndex.foreach {
      case (in, i) =>
        require(
          in >= 0 && in < R,
          s"Poseidon input[$i] is not a canonical BN254 field element (must be in [0, R)): $in"
        )
    }

    // State is [capacity=0, in_0, in_1, ...]; circomlib initialises the capacity to 0.
    val state0 = BigInt(0) +: inputs.toVector
    permute(state0, t)
  }

  /**
   * Convenience 2-to-1 compression for use as a binary Merkle tree node hash.
   * Equivalent to `hash(Seq(left, right))` and therefore uses width t = 3.
   */
  def compress(left: BigInt, right: BigInt): BigInt =
    hash(Seq(left, right))

  /** x^5 mod R, the Poseidon S-box. */
  private def pow5(a: BigInt): BigInt = {
    val a2 = a * a   % R
    val a4 = a2 * a2 % R
    a4 * a           % R
  }

  /**
   * Run the full Poseidon permutation on `state` for width `t` and return
   * `state[0]`.
   */
  private def permute(state: Vector[BigInt], t: Int): BigInt = {
    val c = PoseidonConstants.roundConstants(t)
    val m = PoseidonConstants.mdsMatrix(t)
    val rf = PoseidonConstants.FullRounds
    val rp = PoseidonConstants.partialRounds(t)
    val totalRounds = rf + rp
    val halfRf = rf / 2

    @tailrec
    def loop(r: Int, s: Vector[BigInt]): Vector[BigInt] =
      if (r >= totalRounds) s
      else {
        // ARK: add round constants
        val afterArk = Vector.tabulate(t)(i => (s(i) + c(r * t + i)) % R)

        // S-box: full rounds apply x^5 to every element; partial rounds only to state[0]
        val isFullRound = r < halfRf || r >= halfRf + rp
        val afterSbox =
          if (isFullRound) afterArk.map(pow5)
          else afterArk.updated(0, pow5(afterArk(0)))

        // Mix: state[i] = sum_j M[i][j] * state[j]
        val mixed = Vector.tabulate(t) { i =>
          val row = m(i)
          row.indices.foldLeft(BigInt(0)) { (acc, j) =>
            (acc + row(j) * afterSbox(j)) % R
          }
        }

        loop(r + 1, mixed)
      }

    loop(0, state).head
  }
}
