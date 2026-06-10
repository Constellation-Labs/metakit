package crypto.bls

import java.math.BigInteger
import java.util.Arrays

import io.constellationnetwork.metagraph_sdk.crypto.bls.Bls12381

import weaver.SimpleIOSuite

/**
 * Known-answer + round-trip suite for metakit's eth2-ciphersuite BLS12-381 primitive
 * ([[Bls12381]]), backed by BouncyCastle 1.85's ProofOfPossession scheme.
 *
 * This suite is the byte-identity contract. It guards two things:
 *
 *   1. BYTE-IDENTICAL to Constellation's canonical `io.constellationnetwork.security.bls.BlsSigner`
 *      (tessellation-bls): the exact KAT scalar / message / pubkey / signature strings from
 *      tessellation-bls `BlsSignerSuite` are reproduced here verbatim and must match byte-for-byte.
 *
 *   2. MATCHES the published Eth2 / IETF BLS test vectors
 *      (github.com/ethereum/bls12-381-tests v0.1.2, ciphersuite
 *      BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_): `verify` valid / wrong-pubkey / tampered
 *      cases, and `fast_aggregate_verify` valid / extra-pubkey cases, with the exact published
 *      hex pubkeys, messages and signatures.
 *
 * The tessellation-bls KAT (privkey3 / message = 0xab*32) is itself the published Eth2 vector
 * `verify_valid_case_195246ee3bd3b6ec`, so (1) and (2) coincide on that case -- the published
 * vector value is identical to the canonical Constellation value.
 */
object Bls12381Suite extends SimpleIOSuite {

  private def fromHex(h: String): Array[Byte] = {
    val s = if (h.startsWith("0x")) h.substring(2) else h
    s.grouped(2).map(p => Integer.parseInt(p, 16).toByte).toArray
  }

  private def hex(bytes: Array[Byte]): String = bytes.map(b => f"${b & 0xff}%02x").mkString

  // ===========================================================================
  // (1) tessellation-bls KAT -- reproduced VERBATIM from BlsSignerSuite.scala.
  // ===========================================================================

  private val katSkHex =
    "328388aff0d4a5b7dc9205abd374e7e98f3cd9f3418edb4eafda5fb16473d216"
  private val katMessage: Array[Byte] = Array.fill[Byte](32)(0xab.toByte)
  private val katExpectedPk =
    "b53d21a4cfd562c469cc81514d4ce5a6b577d8403d32a394dc265dd190b47fa9f829fdd7963afdf972e5e77854051f6f"
  private val katExpectedSig =
    "ae82747ddeefe4fd64cf9cedb9b04ae3e8a43420cd255e3c7cd06a8d88b7c7f8638543719981c5d16fa3527c468c25f0" +
    "026704a6951bde891360c7e8d12ddee0559004ccdbe6046b55bae1b257ee97f7cdb955773d7cf29adf3ccbb9975e4eb9"

  private def katSk: BigInteger = new BigInteger(katSkHex, 16)

  pureTest("KAT - skToPk reproduces the canonical tessellation-bls / Eth2 compressed-G1 public key") {
    expect.eql(katExpectedPk, hex(Bls12381.skToPk(katSk)))
  }

  pureTest("KAT - sign reproduces the canonical tessellation-bls / Eth2 compressed-G2 signature (DST guard)") {
    expect.eql(katExpectedSig, hex(Bls12381.sign(katSk, katMessage)))
  }

  pureTest("KAT - the canonical signature verifies against the canonical public key") {
    expect(Bls12381.verify(fromHex(katExpectedPk), katMessage, fromHex(katExpectedSig)))
  }

  // ===========================================================================
  // (2) Published Eth2 / IETF vectors (ethereum/bls12-381-tests v0.1.2).
  // ===========================================================================

  // verify_valid_case_e8a50c445c855360: privkey1, message = 0x00*32 (the IETF draft canonical case).
  private val v1Pk =
    "0xa491d1b0ecd9bb917989f0e74f0dea0422eac4a873e5e2644f368dffb9a6e20fd6e10c1b77654d067c0618f6e5a7f79a"
  private val v1Msg = "0x" + "00" * 32
  private val v1Sig =
    "0xb6ed936746e01f8ecf281f020953fbf1f01debd5657c4a383940b020b26507f6076334f91e2366c96e9ab279fb515809" +
    "0352ea1c5b0c9274504f4f0e7053af24802e51e4568d164fe986834f41e55c8e850ce1f98458c0cfc9ab380b55285a55"

  pureTest("Eth2 verify_valid_case (privkey1, msg=0x00*32) -> true") {
    expect(Bls12381.verify(fromHex(v1Pk), fromHex(v1Msg), fromHex(v1Sig)))
  }

  // verify_valid_case_195246ee3bd3b6ec: privkey3, message = 0xab*32 == the tessellation KAT case.
  pureTest("Eth2 verify_valid_case (privkey3, msg=0xab*32) -> true (== tessellation KAT, byte-identical)") {
    expect(Bls12381.verify(fromHex(katExpectedPk), katMessage, fromHex(katExpectedSig)))
  }

  // verify_wrong_pubkey_case_2f09d443ab8a3ac2: privkey2's signature, privkey1's pubkey -> false.
  private val vWrongPk =
    "0xb301803f8b5ac4a1133581fc676dfedc60d891dd5fa99028805e5ea5b08d3491af75d0707adab3b70c6a6a580217bf81"
  pureTest("Eth2 verify_wrong_pubkey_case -> false") {
    expect(!Bls12381.verify(fromHex(vWrongPk), fromHex(v1Msg), fromHex(v1Sig)))
  }

  // Tampered-signature negative: flip the last 4 bytes of a valid signature.
  pureTest("Eth2 verify with a tampered signature -> false") {
    val tampered = fromHex(v1Sig)
    val n = tampered.length
    tampered(n - 1) = 0xff.toByte; tampered(n - 2) = 0xff.toByte
    tampered(n - 3) = 0xff.toByte; tampered(n - 4) = 0xff.toByte
    expect(!Bls12381.verify(fromHex(v1Pk), fromHex(v1Msg), tampered))
  }

  // fast_aggregate_verify_valid_3d7576f3c0e3570a: 3 pubkeys, message = 0xab*32.
  private val faqPks = List(
    "0xa491d1b0ecd9bb917989f0e74f0dea0422eac4a873e5e2644f368dffb9a6e20fd6e10c1b77654d067c0618f6e5a7f79a",
    "0xb301803f8b5ac4a1133581fc676dfedc60d891dd5fa99028805e5ea5b08d3491af75d0707adab3b70c6a6a580217bf81",
    "0xb53d21a4cfd562c469cc81514d4ce5a6b577d8403d32a394dc265dd190b47fa9f829fdd7963afdf972e5e77854051f6f"
  )
  private val faqMsg = "0x" + "ab" * 32
  private val faqAggSig =
    "0x9712c3edd73a209c742b8250759db12549b3eaf43b5ca61376d9f30e2747dbcf842d8b2ac0901d2a093713e20284a767" +
    "0fcf6954e9ab93de991bb9b313e664785a075fc285806fa5224c82bde146561b446ccfc706a64b8579513cfc4ff1d930"

  pureTest("Eth2 fast_aggregate_verify_valid (3 pubkeys, msg=0xab*32) -> true") {
    expect(Bls12381.fastAggregateVerify(faqPks.map(fromHex), fromHex(faqMsg), fromHex(faqAggSig)))
  }

  // fast_aggregate_verify_extra_pubkey_5a38e6b4017fe4dd: the same aggregate + an EXTRA 4th pubkey -> false.
  pureTest("Eth2 fast_aggregate_verify_extra_pubkey (4 pubkeys for a 3-signer aggregate) -> false") {
    val extra = faqPks :+ faqPks(2) // generator appends SkToPk(PRIVKEYS[-1]) == privkey3's pubkey
    expect(!Bls12381.fastAggregateVerify(extra.map(fromHex), fromHex(faqMsg), fromHex(faqAggSig)))
  }

  // ===========================================================================
  // Round-trips, sizes, aggregate, PoP, and defensive no-throw behaviour.
  // ===========================================================================

  private def freshSk(seed: Int): BigInteger = {
    val ikm = Array.fill[Byte](32)((seed & 0xff).toByte)
    Bls12381.keyGen(ikm, Array.emptyByteArray)
  }

  pureTest("sizes - public key is 48 bytes, signature is 96 bytes") {
    val sk = freshSk(7)
    val pk = Bls12381.skToPk(sk)
    val sig = Bls12381.sign(sk, "msg".getBytes("UTF-8"))
    expect.eql(48, pk.length).and(expect.eql(96, sig.length))
  }

  pureTest("aggregate round-trip - 3 keys sign the SAME message; fastAggregateVerify true, tampered false") {
    val message = "constellation-snapshot-hash-0xdeadbeef".getBytes("UTF-8")
    val tampered = "constellation-snapshot-hash-0xDEADBEEF".getBytes("UTF-8")
    val sks = List(11, 12, 13).map(freshSk)
    val pks = sks.map(Bls12381.skToPk)
    val sigs = sks.map(sk => Bls12381.sign(sk, message))
    val agg = Bls12381.aggregate(sigs)
    expect
      .eql(96, agg.length)
      .and(expect(Bls12381.fastAggregateVerify(pks, message, agg)))
      .and(expect(!Bls12381.fastAggregateVerify(pks, tampered, agg)))
  }

  pureTest("PoP - popProve then popVerify is true; verifying against a different key is false") {
    val sk = freshSk(21)
    val other = freshSk(22)
    val pop = Bls12381.popProve(sk)
    expect(Bls12381.popVerify(Bls12381.skToPk(sk), pop))
      .and(expect(!Bls12381.popVerify(Bls12381.skToPk(other), pop)))
  }

  pureTest("defensive - verify returns false (no throw) on wrong-width / malformed inputs") {
    val sk = freshSk(31)
    val pk = Bls12381.skToPk(sk)
    val sig = Bls12381.sign(sk, "m".getBytes("UTF-8"))
    expect(!Bls12381.verify(new Array[Byte](47), "m".getBytes("UTF-8"), sig)) // bad pk width
      .and(expect(!Bls12381.verify(pk, "m".getBytes("UTF-8"), new Array[Byte](95)))) // bad sig width
      .and(expect(!Bls12381.verify(Array.fill[Byte](48)(0xff.toByte), "m".getBytes("UTF-8"), sig))) // non-canonical pk
      .and(expect(!Bls12381.fastAggregateVerify(Nil, "m".getBytes("UTF-8"), sig))) // empty pubkey set
  }

  pureTest("determinism - skToPk and sign are deterministic for the same scalar/message") {
    val sk = freshSk(41)
    val msg = "determinism".getBytes("UTF-8")
    expect(Arrays.equals(Bls12381.skToPk(sk), Bls12381.skToPk(sk)))
      .and(expect(Arrays.equals(Bls12381.sign(sk, msg), Bls12381.sign(sk, msg))))
  }
}
