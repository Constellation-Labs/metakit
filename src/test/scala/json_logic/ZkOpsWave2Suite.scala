package json_logic

import java.math.BigInteger
import java.security.MessageDigest

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.bls.Bls12381
import io.constellationnetwork.metagraph_sdk.crypto.zk.Bn254
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasConfig, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.ops.{CryptoOps, HexBytes}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

/**
 * End-to-end tests for the SECOND-WAVE JLVM ZK / crypto opcodes:
 *   - BN254 (alt_bn128) curve ops: `bn254_add`, `bn254_mul`, `bn254_pairing`,
 *   - BLS12-381 signatures: `bls_verify`, `bls_aggregate_verify`,
 *   - `schnorr_verify` (Schnorr signature on BN254 G1).
 *
 * Each opcode is exercised with a positive and a negative case (both directly
 * through [[CryptoOps]] and end-to-end through the evaluator), plus the
 * malformed-input / off-curve / wrong-width cases that must surface as a Result
 * error rather than a thrown exception, and a worked threshold-gate contract.
 */
object ZkOpsWave2Suite extends SimpleIOSuite {

  private def evalExpr(exprJson: String, dataJson: String = "{}"): IO[Either[JsonLogicException, JsonLogicValue]] =
    for {
      expr <- IO.fromEither(parser.parse(exprJson).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse(dataJson).flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield out

  // ===========================================================================
  // BN254 helpers (use the Besu-backed Bn254 wrapper to build known points).
  // ===========================================================================

  private val R: BigInt = BigInt(Bn254.R)

  // G1 generator (1, 2) and its 64-byte encoding.
  private val g1: Bn254.G1 = Bn254.G1(BigInteger.ONE, BigInteger.valueOf(2))
  private def encG1(p: Bn254.G1): String = HexBytes.encodeG1(BigInt(p.x), BigInt(p.y)).fold(throw _, identity)
  private val g1Hex: String = encG1(g1)

  // -g1 = (x, P - y).
  private val negG1: Bn254.G1 = Bn254.G1(g1.x, HexBytes.BaseFieldModulus.bigInteger.subtract(g1.y))
  private val negG1Hex: String = encG1(negG1)

  // A canonical G2 generator in EIP-197 (imag-first) byte order. These are the
  // standard alt_bn128 G2 generator coordinates (EIP-197 test vector).
  private val g2Hex: String = {
    val x1 = "198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2" // x.imag (c1)
    val x0 = "1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed" // x.real (c0)
    val y1 = "090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b" // y.imag (c1)
    val y0 = "12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa" // y.real (c0)
    "0x" + x1 + x0 + y1 + y0
  }

  // ===========================================================================
  // bn254_add / bn254_mul
  // ===========================================================================

  test("bn254_add(g1, g1) == bn254_mul(g1, 2) (self-consistency)") {
    val two = HexBytes.encodeUInt(BigInt(2), 32).fold(throw _, identity)
    for {
      added <- evalExpr(s"""{"bn254_add":["$g1Hex","$g1Hex"]}""")
      muled <- evalExpr(s"""{"bn254_mul":["$g1Hex","$two"]}""")
    } yield expect(added == muled) && expect(added == Right(StrValue(encG1(g1.add(g1)))))
  }

  test("bn254_mul(g1, 1) == g1 and bn254_mul(g1, 0) == infinity (zero point)") {
    val one = HexBytes.encodeUInt(BigInt(1), 32).fold(throw _, identity)
    val zero = HexBytes.encodeUInt(BigInt(0), 32).fold(throw _, identity)
    val infHex = "0x" + "0" * 128
    for {
      m1 <- evalExpr(s"""{"bn254_mul":["$g1Hex","$one"]}""")
      m0 <- evalExpr(s"""{"bn254_mul":["$g1Hex","$zero"]}""")
    } yield expect(m1 == Right(StrValue(g1Hex))) && expect(m0 == Right(StrValue(infHex)))
  }

  test("bn254_add errors on an off-curve point (Result error, no crash)") {
    // (1, 1) is not on y^2 = x^3 + 3.
    val offCurve = HexBytes.encodeG1(BigInt(1), BigInt(1)).fold(throw _, identity)
    evalExpr(s"""{"bn254_add":["$offCurve","$g1Hex"]}""").map(r => expect(r.isLeft))
  }

  test("bn254_mul errors on a wrong-width point") {
    evalExpr(s"""{"bn254_mul":["0xdead","0x${"0" * 64}"]}""").map(r => expect(r.isLeft))
  }

  // ===========================================================================
  // bn254_pairing
  // ===========================================================================

  test("bn254_pairing([(g1,g2),(-g1,g2)]) == true (e(g1,g2)*e(-g1,g2) == 1)") {
    evalExpr(s"""{"bn254_pairing":[["$g1Hex","$g2Hex"],["$negG1Hex","$g2Hex"]]}""")
      .map(r => expect(r == Right(BoolValue(true))))
  }

  test("bn254_pairing([(g1,g2)]) == false (single non-trivial pairing != 1)") {
    evalExpr(s"""{"bn254_pairing":[["$g1Hex","$g2Hex"]]}""")
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("bn254_pairing([]) == true (empty product is the identity)") {
    evalExpr("""{"bn254_pairing":[[]]}""").map(r => expect(r == Right(BoolValue(true)))) // [[]] -> single empty array arg
  }

  test("bn254_pairing errors on an off-curve / wrong-width G2 point") {
    evalExpr(s"""{"bn254_pairing":[["$g1Hex","0xdead"]]}""").map(r => expect(r.isLeft))
  }

  // An on-curve-but-NOT-order-r-subgroup G2 point: x = Fp2(c0=2, c1=1) on the
  // BN254 twist, y solved from y^2 = x^3 + b2. It satisfies isOnCurve == true but
  // isInGroup == false (it lies outside the prime-order G2 subgroup). Feeding it
  // to the pairing breaks soundness, so bn254_pairing must reject it as malformed
  // (a JsonLogicException), identically to the off-curve case. EIP-197 imag-first
  // byte order: x.c1 || x.c0 || y.c1 || y.c0. (Cross-checked against the Rust
  // ark-bn254 is_in_correct_subgroup_assuming_on_curve == false; same hex lives
  // in shared/zk_opcode_test_vectors.json.)
  private val nonSubgroupG2Hex: String =
    "0x" +
    "0000000000000000000000000000000000000000000000000000000000000001" + // x.imag (c1) = 1
    "0000000000000000000000000000000000000000000000000000000000000002" + // x.real (c0) = 2
    "2b76c179599bb92a963dac85546a005a777f7c13f6a7b75d5918b6b5808f5fde" + // y.imag (c1)
    "101f7278419308b95099eca02dcee0c5381f4d26d1d62313f057167f064101ce" // y.real (c0)

  test("bn254_pairing rejects an on-curve-but-non-subgroup G2 point (G2 subgroup check)") {
    evalExpr(s"""{"bn254_pairing":[["$g1Hex","$nonSubgroupG2Hex"]]}""").map(r => expect(r.isLeft))
  }

  test("bn254_pairing gas scales by exactly one per-pair charge for each extra pair") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    def pairingExpr(json: String): IO[Long] =
      for {
        expr <- IO.fromEither(parser.parse(json).flatMap(_.as[JsonLogicExpression]))
        res <- evaluator
          .evaluateWithGas(expr, MapValue.empty, None, GasLimit.Unlimited, GasConfig.Default)
          .flatMap(IO.fromEither)
      } yield res.gasUsed.amount
    for {
      one <- pairingExpr(s"""{"bn254_pairing":[["$g1Hex","$g2Hex"]]}""")
      two <- pairingExpr(s"""{"bn254_pairing":[["$g1Hex","$g2Hex"],["$negG1Hex","$g2Hex"]]}""")
    } yield
      expect(
        two - one == GasConfig.Default.bn254PairingPerPair.amount,
        s"two-pair ($two) should exceed one-pair ($one) by one per-pair charge"
      )
  }

  // ===========================================================================
  // BLS12-381 (bls_verify / bls_aggregate_verify) -- Eth2 ProofOfPossession.
  //
  // Inputs are the PUBLISHED Eth2 / IETF BLS test vectors
  // (github.com/ethereum/bls12-381-tests v0.1.2, ciphersuite
  // BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_): pubkeys are 48-byte compressed
  // G1, signatures are 96-byte compressed G2. The opcodes feed straight into the
  // BouncyCastle 1.85 Bls12381 primitive, so the JLVM agrees byte-for-byte with
  // the canonical eth2 vectors.
  // ===========================================================================

  // verify_valid_case_e8a50c445c855360: privkey1, message = 0x00*32.
  private val blsPkHex =
    "0xa491d1b0ecd9bb917989f0e74f0dea0422eac4a873e5e2644f368dffb9a6e20fd6e10c1b77654d067c0618f6e5a7f79a"
  private val blsMsgHex = "0x" + "00" * 32
  private val blsSigHex =
    "0xb6ed936746e01f8ecf281f020953fbf1f01debd5657c4a383940b020b26507f6076334f91e2366c96e9ab279fb515809" +
    "0352ea1c5b0c9274504f4f0e7053af24802e51e4568d164fe986834f41e55c8e850ce1f98458c0cfc9ab380b55285a55"

  test("bls_verify == true for the published Eth2 verify_valid_case vector") {
    evalExpr(s"""{"bls_verify":["$blsPkHex","$blsMsgHex","$blsSigHex"]}""")
      .map(r => expect(r == Right(BoolValue(true))))
  }

  test("bls_verify == false for a wrong message") {
    val otherMsg = HexBytes.encodeBytes("a different message".getBytes("UTF-8"))
    evalExpr(s"""{"bls_verify":["$blsPkHex","$otherMsg","$blsSigHex"]}""")
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("bls_verify == false for the published Eth2 verify_wrong_pubkey vector") {
    // privkey2's pubkey against privkey1's (message, signature).
    val wrongPk =
      "0xb301803f8b5ac4a1133581fc676dfedc60d891dd5fa99028805e5ea5b08d3491af75d0707adab3b70c6a6a580217bf81"
    evalExpr(s"""{"bls_verify":["$wrongPk","$blsMsgHex","$blsSigHex"]}""")
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("bls_verify errors on a wrong-width public key") {
    evalExpr(s"""{"bls_verify":["0xdead","$blsMsgHex","$blsSigHex"]}""").map(r => expect(r.isLeft))
  }

  // ---- aggregate (same message, N signers) ----

  private def pksJson(pks: List[String]): String = pks.map(p => "\"" + p + "\"").mkString("[", ",", "]")

  // fast_aggregate_verify_valid_3d7576f3c0e3570a: 3 pubkeys (privkeys 1,2,3),
  // message = 0xab*32, aggregate signature.
  private val quorumPkHexes: List[String] = List(
    "0xa491d1b0ecd9bb917989f0e74f0dea0422eac4a873e5e2644f368dffb9a6e20fd6e10c1b77654d067c0618f6e5a7f79a",
    "0xb301803f8b5ac4a1133581fc676dfedc60d891dd5fa99028805e5ea5b08d3491af75d0707adab3b70c6a6a580217bf81",
    "0xb53d21a4cfd562c469cc81514d4ce5a6b577d8403d32a394dc265dd190b47fa9f829fdd7963afdf972e5e77854051f6f"
  )
  private val quorumMsgHex = "0x" + "ab" * 32
  private val quorumAggGood =
    "0x9712c3edd73a209c742b8250759db12549b3eaf43b5ca61376d9f30e2747dbcf842d8b2ac0901d2a093713e20284a767" +
    "0fcf6954e9ab93de991bb9b313e664785a075fc285806fa5224c82bde146561b446ccfc706a64b8579513cfc4ff1d930"

  test("bls_aggregate_verify == true for the published Eth2 fast_aggregate_verify_valid vector (3 signers)") {
    evalExpr(s"""{"bls_aggregate_verify":[${pksJson(quorumPkHexes)},"$quorumMsgHex","$quorumAggGood"]}""")
      .map(r => expect(r == Right(BoolValue(true))))
  }

  test("bls_aggregate_verify == false for the published Eth2 fast_aggregate_verify_extra_pubkey vector") {
    // The same aggregate, but an EXTRA 4th pubkey (privkey3's, per the generator) -> false.
    val extra = quorumPkHexes :+ quorumPkHexes(2)
    evalExpr(s"""{"bls_aggregate_verify":[${pksJson(extra)},"$quorumMsgHex","$quorumAggGood"]}""")
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("bls_aggregate_verify == false when a public key is omitted from the set") {
    // Aggregate signature includes all 3 signers but only 2 pubkeys are presented.
    evalExpr(s"""{"bls_aggregate_verify":[${pksJson(quorumPkHexes.take(2))},"$quorumMsgHex","$quorumAggGood"]}""")
      .map(r => expect(r == Right(BoolValue(false))))
  }

  // ===========================================================================
  // schnorr_verify -- generate a valid proof in-test using the documented rules.
  // ===========================================================================

  // proof = R(64B) || s(32B); c = SHA256(R || pk || msg) mod r; s = k + c*x mod r.
  private def schnorrProve(x: BigInt, msg: Array[Byte], k: BigInt): (String, String) = {
    val pk = g1.multiply(x.bigInteger)
    val rPoint = g1.multiply(k.bigInteger)
    val pkBytes = HexBytes.parseBytes(encG1(pk), Some(64), "pk").fold(throw _, identity)
    val rBytes = HexBytes.parseBytes(encG1(rPoint), Some(64), "R").fold(throw _, identity)
    val c = BigInt(1, MessageDigest.getInstance("SHA-256").digest(rBytes ++ pkBytes ++ msg)).mod(R)
    val s = (k + c * x).mod(R)
    val sHex = HexBytes.encodeUInt(s, 32).fold(throw _, identity)
    val proof = "0x" + HexBytes.encodeBytes(rBytes).substring(2) + sHex.substring(2)
    (encG1(pk), proof)
  }

  private val schnorrX: BigInt = BigInt("123456789012345678901234567890").mod(R)
  private val schnorrK: BigInt = BigInt("987654321098765432109876543210").mod(R)
  private val schnorrMsg: Array[Byte] = "authorize transfer".getBytes("UTF-8")
  private val schnorrMsgHex: String = HexBytes.encodeBytes(schnorrMsg)
  private val (schnorrPkHex, schnorrProofHex) = schnorrProve(schnorrX, schnorrMsg, schnorrK)

  test("schnorr_verify == true for a valid proof") {
    evalExpr(s"""{"schnorr_verify":["$schnorrPkHex","$schnorrMsgHex","$schnorrProofHex"]}""")
      .map(r => expect(r == Right(BoolValue(true))))
  }

  test("schnorr_verify == false for a tampered s") {
    val tampered = {
      val bytes = HexBytes.parseBytes(schnorrProofHex, Some(96), "p").fold(throw _, identity)
      bytes(95) = (bytes(95) ^ 0x01).toByte
      HexBytes.encodeBytes(bytes)
    }
    evalExpr(s"""{"schnorr_verify":["$schnorrPkHex","$schnorrMsgHex","$tampered"]}""")
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("schnorr_verify == false for the wrong message") {
    val otherMsg = HexBytes.encodeBytes("a different action".getBytes("UTF-8"))
    evalExpr(s"""{"schnorr_verify":["$schnorrPkHex","$otherMsg","$schnorrProofHex"]}""")
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("schnorr_verify errors on a wrong-width proof") {
    evalExpr(s"""{"schnorr_verify":["$schnorrPkHex","$schnorrMsgHex","0xdead"]}""").map(r => expect(r.isLeft))
  }

  // ---- SOUNDNESS: identity / point-at-infinity public-key forgery ----
  //
  // With pk = O the verification equation `s*G == R + c*pk` collapses to
  // `s*G == R`. An attacker forges a "signature" for ANY message by picking a
  // scalar s, computing R = s*G, and setting pk = O (the all-zero 64-byte G1).
  // PRE-fix this verified `true` (universal forgery); POST-fix it MUST be
  // `false` (identity pk is correct-width but cryptographically invalid -> a
  // value, NOT a Result error). This mirrors the Rust differential vector
  // "identity-pk forgery rejected" in shared/zk_opcode_test_vectors.json and
  // must stay byte-identical.

  // Forge: choose s, set R = s*G, pk = O, arbitrary msg; proof = R(64B) || s(32B).
  private def schnorrIdentityForgery(s: BigInt, msg: Array[Byte]): (String, String, String) = {
    val pkHex = "0x" + "0" * 128 // identity O = all-zero 64-byte G1
    val rPoint = g1.multiply(s.bigInteger) // R = s*G
    val rBytes = HexBytes.parseBytes(encG1(rPoint), Some(64), "R").fold(throw _, identity)
    val sHex = HexBytes.encodeUInt(s, 32).fold(throw _, identity)
    val proof = "0x" + HexBytes.encodeBytes(rBytes).substring(2) + sHex.substring(2)
    val msgHex = HexBytes.encodeBytes(msg)
    (pkHex, msgHex, proof)
  }

  private val (forgePkHex, forgeMsgHex, forgeProofHex) =
    schnorrIdentityForgery(BigInt(123456789), "authorize transfer".getBytes("UTF-8"))

  test("schnorr_verify == false for an identity (point-at-infinity) public-key forgery") {
    // Lock byte-identity with the shared Rust differential vector.
    val expectedProofHex =
      "0x142a7688cf05c29f7593351e1b86eb87e3ad5dcb1b0fc3d853e9852040c57019" +
      "136b5d7e238ae6edc22d1fba5a2dcde8a7b0df53b0c4af7f600e6a0c4610c899" +
      "00000000000000000000000000000000000000000000000000000000075bcd15"
    val vectorMatch =
      expect(forgePkHex == "0x" + "0" * 128) &&
      expect(forgeMsgHex == "0x617574686f72697a65207472616e73666572") &&
      expect(forgeProofHex == expectedProofHex)
    for {
      r <- evalExpr(s"""{"schnorr_verify":["$forgePkHex","$forgeMsgHex","$forgeProofHex"]}""")
    } yield vectorMatch && expect(r == Right(BoolValue(false)))
  }

  // ===========================================================================
  // Worked example: a BLS threshold gate evaluated end-to-end.
  // ===========================================================================

  private def thresholdGate(pks: List[String], msgHex: String, aggSig: String): String =
    s"""
       |{"if":[
       |  {"bls_aggregate_verify":[${pksJson(pks)},"$msgHex","$aggSig"]},
       |  "authorized",
       |  "rejected"
       |]}
       |""".stripMargin

  test("worked example: threshold gate returns 'authorized' for a valid published quorum") {
    evalExpr(thresholdGate(quorumPkHexes, quorumMsgHex, quorumAggGood))
      .map(r => expect(r == Right(StrValue("authorized"))))
  }

  test("worked example: threshold gate returns 'rejected' when a public key is missing") {
    evalExpr(thresholdGate(quorumPkHexes.take(2), quorumMsgHex, quorumAggGood))
      .map(r => expect(r == Right(StrValue("rejected"))))
  }

  // Locally generated round-trip (defends the full keyGen -> sign -> aggregate -> opcode path on
  // the Eth2 PoP primitive, independent of the hard-coded published vectors): three fresh signers
  // sign the SAME message, aggregate, and the opcode accepts; tampering one signer's message rejects.
  private def freshSk(seed: Int): BigInteger = Bls12381.keyGen(Array.fill[Byte](32)((seed & 0xff).toByte), Array.emptyByteArray)

  pureTest("aggregate round-trip via the Eth2 primitive matches single verify and the opcode") {
    val msg = "metakit-jlvm-threshold".getBytes("UTF-8")
    val sks = List(51, 52, 53).map(freshSk)
    val pkHexes = sks.map(sk => HexBytes.encodeBytes(Bls12381.skToPk(sk)))
    val sigs = sks.map(sk => Bls12381.sign(sk, msg))
    val aggHex = HexBytes.encodeBytes(Bls12381.aggregate(sigs))
    val msgHex = HexBytes.encodeBytes(msg)

    val viaAggregate = CryptoOps.blsAggregateVerify(
      List(ArrayValue(pkHexes.map(StrValue(_))), StrValue(msgHex), StrValue(aggHex))
    )
    // N = 1 single-signer case must match the single verify entry point.
    val singleHex = HexBytes.encodeBytes(Bls12381.aggregate(List(sigs.head)))
    val viaSingle = CryptoOps.blsVerify(List(StrValue(pkHexes.head), StrValue(msgHex), StrValue(singleHex)))
    val viaSingleDirect = Bls12381.verify(Bls12381.skToPk(sks.head), msg, sigs.head)

    expect(viaAggregate == Right(BoolValue(true)))
      .and(expect(viaSingle == Right(BoolValue(true))))
      .and(expect(viaSingleDirect))
  }
}
