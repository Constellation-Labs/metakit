package io.constellationnetwork.metagraph_sdk.crypto.bls

import java.math.BigInteger
import java.util.Arrays

import org.bouncycastle.crypto.bls._
import org.bouncycastle.math.ec.{ECCurve, ECPoint}

/**
 * BLS12-381 signatures over the Eth2 / draft-irtf-cfrg-bls-signature ProofOfPossession (PoP)
 * ciphersuite, backed by BouncyCastle 1.85's `org.bouncycastle.crypto.bls.*`.
 *
 * This is a BYTE-FOR-BYTE port of Constellation's canonical
 * `io.constellationnetwork.security.bls.BlsSigner` (tessellation-bls). It replaces metakit's
 * earlier MIRACL-SVDW `MiraclBls12381` wrapper, which used the (interoperable-with-MIRACL-only)
 * `BLS_SIG_BLS12381G1_XMD:SHA-256_SVDW_RO_NUL_` suite. This implementation matches the published
 * Eth2 / IETF BLS test vectors instead.
 *
 * ==Ciphersuite (the byte-identity contract)==
 *   - Scheme: ProofOfPossession (PoP), `org.bouncycastle.crypto.bls.BLS12_381ProofOfPossession`.
 *   - Variant: minimal-pubkey-size -- public keys in '''G1''', signatures in '''G2'''.
 *   - Hash-to-curve: `expand_message_xmd` over SHA-256 with the SSWU map (random-oracle encoding).
 *   - Signature DST: `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`
 *   - Proof-of-possession DST: `BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`
 *   - Key generation: `BLS12_381BasicScheme.keyGen` (HKDF over the IKM, IETF KeyGen).
 *
 * ==Groups and serialized sizes==
 *   - [[PublicKeyBytes]] -- 48-byte compressed G1 point (`BLS12_381Serialization.compressG1`).
 *   - [[SignatureBytes]] -- 96-byte compressed G2 point (`BLS12_381Serialization.compressG2`);
 *     also the wire form of a PoP.
 *   - Secret keys are scalars in `Z_r`; never serialized to the wire.
 *
 * ==Use case (committee / threshold)==
 * A committee of N validators each sign the SAME message; the N individual signatures aggregate to
 * one 96-byte G2 signature, verified against the N public keys + the single message via
 * [[fastAggregateVerify]]. Only the same-message aggregate path is exposed (exactly as the
 * canonical `BlsSigner`); distinct-message `AggregateVerify` is intentionally omitted.
 *
 * The verification entry points ([[verify]], [[fastAggregateVerify]]) never throw: malformed or
 * non-canonical inputs (bad point encoding, non-subgroup member, wrong width) return `false`. This
 * is what the JLVM crypto opcodes consume.
 */
object Bls12381 {

  /** Compressed G1 public-key size (minimal-pubkey-size variant). */
  val PublicKeyBytes: Int = 48

  /** Compressed G2 signature / PoP size (minimal-pubkey-size variant). */
  val SignatureBytes: Int = 96

  /** The single BLS12-381 G1 curve instance, needed to decompress 48-byte public-key bytes back to an ECPoint. */
  private val g1Curve: ECCurve = BLS12_381G1.createCurve()

  private def decompressPk(pk: Array[Byte]): ECPoint =
    BLS12_381Serialization.decompressG1(pk, g1Curve)

  private def decompressSig(sig: Array[Byte]): BLS12_381G2Point =
    BLS12_381Serialization.decompressG2(sig)

  private def compressPk(point: ECPoint): Array[Byte] =
    BLS12_381Serialization.compressG1(point)

  private def compressSig(point: BLS12_381G2Point): Array[Byte] =
    BLS12_381Serialization.compressG2(point)

  /** IETF KeyGen: derive the secret scalar from input keying material (`ikm` MUST be >= 32 bytes). */
  def keyGen(ikm: Array[Byte], keyInfo: Array[Byte]): BigInteger =
    BLS12_381BasicScheme.keyGen(ikm, keyInfo)

  /** Derive the 48-byte compressed-G1 public key for a secret scalar. */
  def skToPk(sk: BigInteger): Array[Byte] =
    compressPk(BLS12_381BasicScheme.skToPk(sk))

  /** Sign `message` with `sk` (PoP scheme `sign`, DST `..._SIG_..._POP_`). Returns a 96-byte compressed-G2 signature. */
  def sign(sk: BigInteger, message: Array[Byte]): Array[Byte] =
    compressSig(BLS12_381ProofOfPossession.sign(sk, message))

  /**
   * Verify a single signature against a single public key + message.
   *
   * `pk` is a 48-byte compressed G1 point, `sig` a 96-byte compressed G2 point, `message` arbitrary
   * bytes. Returns `false` (never throws) on any malformed input or failed check.
   */
  def verify(pk: Array[Byte], message: Array[Byte], sig: Array[Byte]): Boolean =
    if (pk.length != PublicKeyBytes || sig.length != SignatureBytes) false
    else
      try BLS12_381ProofOfPossession.verify(decompressPk(pk), message, decompressSig(sig))
      catch { case _: Throwable => false }

  /** Aggregate N signatures (over the same message) into one 96-byte compressed-G2 signature. */
  def aggregate(sigs: List[Array[Byte]]): Array[Byte] = {
    val points: Array[BLS12_381G2Point] = sigs.map(decompressSig).toArray
    compressSig(BLS12_381Aggregation.aggregate(points))
  }

  /**
   * Verify an aggregate signature against N public keys + the single shared message
   * (same-message `fastAggregateVerify`).
   *
   * `pks` are N 48-byte compressed G1 points, `agg` a 96-byte compressed G2 point, `message`
   * arbitrary bytes. Returns `false` (never throws) on any malformed input, non-member point, or
   * failed pairing check.
   */
  def fastAggregateVerify(pks: List[Array[Byte]], message: Array[Byte], agg: Array[Byte]): Boolean =
    if (pks.isEmpty || agg.length != SignatureBytes || pks.exists(_.length != PublicKeyBytes)) false
    else
      try {
        val points: Array[ECPoint] = pks.map(decompressPk).toArray
        BLS12_381ProofOfPossession.fastAggregateVerify(points, message, decompressSig(agg))
      } catch { case _: Throwable => false }

  /** Produce a proof-of-possession for `sk` (PoP scheme `popProve`, DST `..._POP_..._POP_`, message = pk). */
  def popProve(sk: BigInteger): Array[Byte] =
    compressSig(BLS12_381ProofOfPossession.popProve(sk))

  /** Verify a proof-of-possession binds `pop` to `pk`. */
  def popVerify(pk: Array[Byte], pop: Array[Byte]): Boolean =
    if (pk.length != PublicKeyBytes || pop.length != SignatureBytes) false
    else
      try BLS12_381ProofOfPossession.popVerify(decompressPk(pk), decompressSig(pop))
      catch { case _: Throwable => false }

  /** Lowercase hex of a byte array (no `0x` prefix). */
  private[bls] def hex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  /** Byte-equality on two arrays (constant-time not required for public values). */
  private[bls] def bytesEqual(a: Array[Byte], b: Array[Byte]): Boolean =
    Arrays.equals(a, b)
}
