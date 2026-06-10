package io.constellationnetwork.metagraph_sdk.crypto.bls

import org.miracl.core.BLS12381._

/**
 * Thin Scala wrapper over MIRACL Core's BLS12-381 signature scheme, consumed
 * as-is (no MIRACL source is modified). Provides single-signature verification
 * (delegating to [[BLS.core_verify]]) and same-message N-of-N signature
 * aggregation verification (which MIRACL has no built-in helper for, so it is
 * implemented here directly with the pairing engine).
 *
 * ==Ciphersuite==
 * MIRACL's `BLS` is the "minimal-signature-size" variant. The hash-to-curve
 * domain separation tag baked into [[BLS.bls_hash_to_point]] is
 * {{{
 *   BLS_SIG_BLS12381G1_XMD:SHA-256_SVDW_RO_NUL_
 * }}}
 * i.e. messages are hashed to '''G1''' with `expand_message_xmd` over SHA-256
 * and the SvdW map (random-oracle encoding). This corresponds to the IRTF BLS
 * signature draft's "minimal-signature-size" profile, except MIRACL uses SvdW
 * rather than SSWU as the base map — so this is interoperable with other MIRACL
 * BLS deployments, not with `BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_` (eth2)
 * signatures.
 *
 * ==Groups and sizes==
 *   - Signatures live in '''G1''' and are serialized COMPRESSED via
 *     `ECP.toBytes(buf, true)`. BLS12-381 has `MODBYTES = 48`; the standard
 *     (non-alt) MIRACL compression prefixes one flag byte, so a signature is
 *     `1 + 48 = 49` bytes ([[SignatureBytes]]).
 *   - Public keys live in '''G2''' and are serialized COMPRESSED via
 *     `ECP2.toBytes(buf, true)`: `1 + 2 * 48 = 97` bytes ([[PublicKeyBytes]]).
 *   - Private keys are `MODBYTES = 48` bytes ([[PrivateKeyBytes]]).
 *
 * ==Verification relations==
 *   - Single: accept iff `e(-sig, G2gen) · e(H(m), pk) == 1`, equivalently
 *     `e(sig, G2gen) == e(H(m), pk)` (exactly [[BLS.core_verify]]).
 *   - Same-message aggregate of `N` signers over one message `m`: the aggregate
 *     signature is the G1 sum `S = Σ_i sig_i` and the relation checked is
 *     `e(-S, G2gen) · e(H(m), Σ_i pk_i) == 1`, equivalently
 *     `e(S, G2gen) == e(H(m), Σ_i pk_i)`. This is the threshold / multisig case
 *     (one message, many signers); it is NOT secure for distinct messages
 *     without the rogue-key defenses that distinct-message aggregation requires.
 */
object MiraclBls12381 {

  /** Compressed G1 signature size: `1 + MODBYTES`. */
  val SignatureBytes: Int = 1 + BLS.BFS // 49

  /** Compressed G2 public-key size: `1 + 2 * MODBYTES`. */
  val PublicKeyBytes: Int = 1 + 2 * BLS.BGS // 97

  /** Private-key size: `MODBYTES`. */
  val PrivateKeyBytes: Int = BLS.BGS // 48

  // BLS.init() builds the G2 generator precomputation table used by core_verify.
  // It is idempotent and cheap; we run it once at class-load.
  private val initOk: Boolean = BLS.init() == BLS.BLS_OK

  /**
   * Verify a single BLS signature. Returns `false` (never throws) for any
   * malformed input or failed check. `pk` is a compressed G2 point
   * ([[PublicKeyBytes]]), `sig` a compressed G1 point ([[SignatureBytes]]),
   * `msg` arbitrary message bytes.
   */
  def verify(pk: Array[Byte], msg: Array[Byte], sig: Array[Byte]): Boolean =
    if (!initOk || pk.length != PublicKeyBytes || sig.length != SignatureBytes) false
    else
      try BLS.core_verify(sig, msg, pk) == BLS.BLS_OK
      catch { case _: Throwable => false }

  /**
   * Verify an aggregate of `N` BLS signatures over the SAME message. `aggSig`
   * is the compressed G1 sum of the individual signatures; `pks` are the `N`
   * compressed G2 public keys; `msg` the common message. Returns `false` (never
   * throws) on any malformed input, non-member point, or failed pairing check.
   *
   * Relation: `e(-aggSig, G2gen) · e(H(m), Σ pk_i) == 1`.
   */
  def aggregateVerify(pks: List[Array[Byte]], msg: Array[Byte], aggSig: Array[Byte]): Boolean =
    if (!initOk || pks.isEmpty || aggSig.length != SignatureBytes || pks.exists(_.length != PublicKeyBytes)) false
    else
      try {
        // Aggregate signature point (G1), must be a valid subgroup member.
        val sig = ECP.fromBytes(aggSig)
        if (sig.is_infinity() || !PAIR.G1member(sig)) false
        else {
          // Aggregate public key = Σ pk_i (G2), each a valid subgroup member.
          val aggPk = new ECP2()
          var membersOk = true
          pks.foreach { pkBytes =>
            val pk = ECP2.fromBytes(pkBytes)
            if (pk.is_infinity() || !PAIR.G2member(pk)) membersOk = false
            else aggPk.add(pk)
          }
          if (!membersOk || aggPk.is_infinity()) false
          else {
            val hm = BLS.bls_hash_to_point(msg)
            val g = ECP2.generator()
            val negSig = new ECP(sig)
            negSig.neg()
            // e(G, -aggSig) * e(aggPk, H(m)) == 1  (ate2 args: ECP2, ECP, ECP2, ECP)
            val v: FP12 = PAIR.fexp(PAIR.ate2(g, negSig, aggPk, hm))
            v.isunity()
          }
        }
      } catch { case _: Throwable => false }
}
