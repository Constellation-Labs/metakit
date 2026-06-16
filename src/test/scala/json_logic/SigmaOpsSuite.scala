package json_logic

import java.math.BigInteger
import java.security.MessageDigest

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.zk.Bn254
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasConfig, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.ops.{CryptoOps, HexBytes}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

/**
 * End-to-end tests for the SIGMA-PROTOCOL JLVM opcodes (classical, no-trusted-setup, Ergo / EIP-11
 * family) on BN254 G1:
 *   - `prove_dlog_verify`     -- the DLog Σ-leaf (first-class alias for `schnorr_verify`),
 *   - `prove_dhtuple_verify`  -- the DDH / Diffie–Hellman-tuple Σ-leaf (∃w. u=g^w ∧ v=h^w),
 *   - and the two commitment-recovery helpers (`dlogComputeCommitment` / `dhtupleComputeCommitment`)
 *     the DEFERRED `sigma_verify` tree will reuse (see docs/sigma-verify.md).
 *
 * The Schnorr / DLog conventions are reused exactly from [[ZkOpsWave2Suite]] (generator (1,2), the
 * SHA256(transcript) mod R Fiat-Shamir family, the `0x`-fixed-width hex codec). Each opcode gets a
 * valid case (true), tampered cases (false), the cryptographically-wrong case (false), the
 * off-curve / identity cases (hard error), and a gas-charged check.
 */
object SigmaOpsSuite extends SimpleIOSuite {

  private def evalExpr(exprJson: String, dataJson: String = "{}"): IO[Either[JsonLogicException, JsonLogicValue]] =
    for {
      expr <- IO.fromEither(parser.parse(exprJson).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse(dataJson).flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield out

  private val R: BigInt = BigInt(Bn254.R)

  // BN254 G1 generator (1, 2); matches CryptoOps.SchnorrGenerator and ZkOpsWave2Suite.g1.
  private val g1: Bn254.G1 = Bn254.G1(BigInteger.ONE, BigInteger.valueOf(2))
  private def encG1(p: Bn254.G1): String = HexBytes.encodeG1(BigInt(p.x), BigInt(p.y)).fold(throw _, identity)
  private def g1Bytes(p: Bn254.G1): Array[Byte] = HexBytes.parseBytes(encG1(p), Some(64), "g1").fold(throw _, identity)
  private def sha256(bytes: Array[Byte]): BigInt = BigInt(1, MessageDigest.getInstance("SHA-256").digest(bytes))

  // ===========================================================================
  // prove_dlog_verify -- parity with schnorr_verify on the SAME vectors.
  // ===========================================================================

  // Identical inline Schnorr prover to ZkOpsWave2Suite.schnorrProve: proof = R(64B) || s(32B);
  // c = SHA256(R || pk || msg) mod R; s = k + c*x mod R.
  private def schnorrProve(x: BigInt, msg: Array[Byte], k: BigInt): (String, String) = {
    val pk = g1.multiply(x.bigInteger)
    val rPoint = g1.multiply(k.bigInteger)
    val c = sha256(g1Bytes(rPoint) ++ g1Bytes(pk) ++ msg).mod(R)
    val s = (k + c * x).mod(R)
    val sHex = HexBytes.encodeUInt(s, 32).fold(throw _, identity)
    val proof = "0x" + HexBytes.encodeBytes(g1Bytes(rPoint)).substring(2) + sHex.substring(2)
    (encG1(pk), proof)
  }

  // Same scalars as the canonical Schnorr vectors in ZkOpsWave2Suite, so this is a true parity check.
  private val schnorrX: BigInt = BigInt("123456789012345678901234567890").mod(R)
  private val schnorrK: BigInt = BigInt("987654321098765432109876543210").mod(R)
  private val schnorrMsg: Array[Byte] = "authorize transfer".getBytes("UTF-8")
  private val schnorrMsgHex: String = HexBytes.encodeBytes(schnorrMsg)
  private val (schnorrPkHex, schnorrProofHex) = schnorrProve(schnorrX, schnorrMsg, schnorrK)

  test("prove_dlog_verify == true for a valid Schnorr/DLog proof") {
    evalExpr(s"""{"prove_dlog_verify":["$schnorrPkHex","$schnorrMsgHex","$schnorrProofHex"]}""")
      .map(r => expect(r == Right(BoolValue(true))))
  }

  test("prove_dlog_verify agrees with schnorr_verify byte-for-byte (parity, both true)") {
    for {
      viaDlog    <- evalExpr(s"""{"prove_dlog_verify":["$schnorrPkHex","$schnorrMsgHex","$schnorrProofHex"]}""")
      viaSchnorr <- evalExpr(s"""{"schnorr_verify":["$schnorrPkHex","$schnorrMsgHex","$schnorrProofHex"]}""")
    } yield expect(viaDlog == viaSchnorr) && expect(viaDlog == Right(BoolValue(true)))
  }

  test("prove_dlog_verify agrees with schnorr_verify on a tampered proof (both false)") {
    val tampered = {
      val bytes = HexBytes.parseBytes(schnorrProofHex, Some(96), "p").fold(throw _, identity)
      bytes(95) = (bytes(95) ^ 0x01).toByte
      HexBytes.encodeBytes(bytes)
    }
    for {
      viaDlog    <- evalExpr(s"""{"prove_dlog_verify":["$schnorrPkHex","$schnorrMsgHex","$tampered"]}""")
      viaSchnorr <- evalExpr(s"""{"schnorr_verify":["$schnorrPkHex","$schnorrMsgHex","$tampered"]}""")
    } yield expect(viaDlog == viaSchnorr) && expect(viaDlog == Right(BoolValue(false)))
  }

  test("prove_dlog_verify rejects the identity public-key forgery (false, mirrors schnorr_verify)") {
    // pk = O collapses s*G == R + c*pk to s*G == R; pick s, set R = s*G -> universal forgery.
    val s = BigInt(123456789)
    val pkHex = "0x" + "0" * 128
    val rPoint = g1.multiply(s.bigInteger)
    val sHex = HexBytes.encodeUInt(s, 32).fold(throw _, identity)
    val proof = "0x" + HexBytes.encodeBytes(g1Bytes(rPoint)).substring(2) + sHex.substring(2)
    for {
      viaDlog    <- evalExpr(s"""{"prove_dlog_verify":["$pkHex","$schnorrMsgHex","$proof"]}""")
      viaSchnorr <- evalExpr(s"""{"schnorr_verify":["$pkHex","$schnorrMsgHex","$proof"]}""")
    } yield expect(viaDlog == viaSchnorr) && expect(viaDlog == Right(BoolValue(false)))
  }

  test("prove_dlog_verify errors on a wrong-width proof (hard error, role re-labelled)") {
    evalExpr(s"""{"prove_dlog_verify":["$schnorrPkHex","$schnorrMsgHex","0xdead"]}""").map { r =>
      expect(r.isLeft) && expect(r.swap.toOption.exists(_.getMessage.contains("prove_dlog_verify")))
    }
  }

  // ===========================================================================
  // prove_dhtuple_verify -- in-test prover using the documented STRONG-FS rules.
  //
  //   proof = a1(64B) || a2(64B) || z(32B); a1 = g^r, a2 = h^r, z = r + e*w mod R;
  //   e = SHA256(g || h || u || v || a1 || a2 || msg) mod R  (STRONG Fiat-Shamir).
  // ===========================================================================

  // Build the strong-FS challenge over the canonical fixed-width transcript, byte-identical to
  // CryptoOps.proveDhTupleVerify (this is the LOAD-BEARING binding: full statement + both
  // commitments + message).
  private def dhChallenge(g: Bn254.G1, h: Bn254.G1, u: Bn254.G1, v: Bn254.G1, a1: Bn254.G1, a2: Bn254.G1, msg: Array[Byte]): BigInt =
    sha256(g1Bytes(g) ++ g1Bytes(h) ++ g1Bytes(u) ++ g1Bytes(v) ++ g1Bytes(a1) ++ g1Bytes(a2) ++ msg).mod(R)

  private def proofBytes(a1: Bn254.G1, a2: Bn254.G1, z: BigInt): String = {
    val zHex = HexBytes.encodeUInt(z.mod(R), 32).fold(throw _, identity)
    "0x" + HexBytes.encodeBytes(g1Bytes(a1)).substring(2) +
    HexBytes.encodeBytes(g1Bytes(a2)).substring(2) + zHex.substring(2)
  }

  /** A valid DH-tuple statement (g,h,u,v) and its proof for witness `w` and nonce `r`. */
  private def dhTupleProve(
    w: BigInt,
    r: BigInt,
    gScalar: BigInt,
    hScalar: BigInt,
    msg: Array[Byte]
  ): (Bn254.G1, Bn254.G1, Bn254.G1, Bn254.G1, String) = {
    // Pick two independent bases g, h as multiples of the generator (both non-identity).
    val g = g1.multiply(gScalar.bigInteger)
    val h = g1.multiply(hScalar.bigInteger)
    val u = g.multiply(w.bigInteger) // u = g^w
    val v = h.multiply(w.bigInteger) // v = h^w
    val a1 = g.multiply(r.bigInteger) // a1 = g^r
    val a2 = h.multiply(r.bigInteger) // a2 = h^r
    val e = dhChallenge(g, h, u, v, a1, a2, msg)
    val z = (r + e * w).mod(R)
    (g, h, u, v, proofBytes(a1, a2, z))
  }

  private val dhW: BigInt = BigInt("111122223333444455556666777788889999").mod(R)
  private val dhR: BigInt = BigInt("424242424242424242424242424242424242").mod(R)
  private val dhMsg: Array[Byte] = "authorize compose".getBytes("UTF-8")
  private val dhMsgHex: String = HexBytes.encodeBytes(dhMsg)
  private val (dhG, dhH, dhU, dhV, dhProof) = dhTupleProve(dhW, dhR, BigInt(3), BigInt(5), dhMsg)
  private val (dhGHex, dhHHex, dhUHex, dhVHex) = (encG1(dhG), encG1(dhH), encG1(dhU), encG1(dhV))

  private def dhExpr(g: String, h: String, u: String, v: String, msg: String, proof: String): String =
    s"""{"prove_dhtuple_verify":["$g","$h","$u","$v","$msg","$proof"]}"""

  test("prove_dhtuple_verify == true for a valid DH-tuple proof") {
    evalExpr(dhExpr(dhGHex, dhHHex, dhUHex, dhVHex, dhMsgHex, dhProof))
      .map(r => expect(r == Right(BoolValue(true))))
  }

  test("prove_dhtuple_verify == false for a tampered z (last response byte flipped)") {
    val tampered = {
      val bytes = HexBytes.parseBytes(dhProof, Some(160), "p").fold(throw _, identity)
      bytes(159) = (bytes(159) ^ 0x01).toByte
      HexBytes.encodeBytes(bytes)
    }
    evalExpr(dhExpr(dhGHex, dhHHex, dhUHex, dhVHex, dhMsgHex, tampered))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("prove_dhtuple_verify == false for a tampered a1 (first commitment byte flipped)") {
    // Flipping a low byte of a1's x-coordinate almost surely lands off-curve (=> hard error) or on
    // a different on-curve point. To keep this a clean WELL-FORMED-but-wrong case, replace a1 with a
    // DIFFERENT valid on-curve point (g^(r+1)): the verification equation and the strong-FS challenge
    // both change, so it must be false (not an error).
    val badA1 = dhG.multiply((dhR + 1).bigInteger)
    val z = HexBytes.parseBytes(dhProof, Some(160), "p").fold(throw _, identity).slice(128, 160)
    val a2 = dhH.multiply(dhR.bigInteger)
    val tampered = "0x" + HexBytes.encodeBytes(g1Bytes(badA1)).substring(2) +
      HexBytes.encodeBytes(g1Bytes(a2)).substring(2) + HexBytes.encodeBytes(z).substring(2)
    evalExpr(dhExpr(dhGHex, dhHHex, dhUHex, dhVHex, dhMsgHex, tampered))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("prove_dhtuple_verify == false when v is swapped (statement is not a DH tuple for this proof)") {
    // Swap v for a DIFFERENT valid on-curve image v' = h^(w+1): now (g,h,u,v') is not a DH tuple for
    // the witness the proof was built for, so the second equation z*h == a2 + e*v' fails -> false.
    val vSwapped = encG1(dhH.multiply((dhW + 1).bigInteger))
    evalExpr(dhExpr(dhGHex, dhHHex, dhUHex, vSwapped, dhMsgHex, dhProof))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("prove_dhtuple_verify == false for a NON-DH-tuple (v = h^w', w' != w)") {
    // u = g^w but v = h^w' with w' != w: there is NO single witness w'' with u=g^w'' AND v=h^w''
    // (g, h are independent generators), so no valid proof exists; the honestly-built proof for w
    // verifies false against this rebound v. This is the core soundness property of the DDH leaf.
    val wPrime = (dhW + BigInt(7)).mod(R)
    val vWrong = encG1(dhH.multiply(wPrime.bigInteger))
    evalExpr(dhExpr(dhGHex, dhHHex, dhUHex, vWrong, dhMsgHex, dhProof))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("prove_dhtuple_verify == false when the message is changed (strong-FS binds msg)") {
    val otherMsg = HexBytes.encodeBytes("authorize a DIFFERENT compose".getBytes("UTF-8"))
    evalExpr(dhExpr(dhGHex, dhHHex, dhUHex, dhVHex, otherMsg, dhProof))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("prove_dhtuple_verify == false when a statement base is changed (strong-FS binds g,h,u,v)") {
    // Re-bind g to g' = g^2 while keeping the original proof. The strong-FS challenge changes AND
    // u = g^w no longer holds for g', so it must be false. Proves the challenge binds the statement.
    val gPrime = encG1(dhG.multiply(BigInt(2).bigInteger))
    evalExpr(dhExpr(gPrime, dhHHex, dhUHex, dhVHex, dhMsgHex, dhProof))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("prove_dhtuple_verify errors on an off-curve statement point (hard error, no crash)") {
    // (1, 1) is not on y^2 = x^3 + 3.
    val offCurve = HexBytes.encodeG1(BigInt(1), BigInt(1)).fold(throw _, identity)
    evalExpr(dhExpr(offCurve, dhHHex, dhUHex, dhVHex, dhMsgHex, dhProof)).map(r => expect(r.isLeft))
  }

  test("prove_dhtuple_verify errors on a wrong-width proof (hard error)") {
    evalExpr(dhExpr(dhGHex, dhHHex, dhUHex, dhVHex, dhMsgHex, "0xdead")).map(r => expect(r.isLeft))
  }

  test("prove_dhtuple_verify == false for an identity statement base (g = O is a forgery vector)") {
    // g = O makes z*g == a1 + e*u collapse to O == a1 + e*u, satisfiable by free choice of a1.
    // Correct-WIDTH but cryptographically invalid -> false (a value), NOT an error.
    val identity = "0x" + "0" * 128
    evalExpr(dhExpr(identity, dhHHex, dhUHex, dhVHex, dhMsgHex, dhProof))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("prove_dhtuple_verify == false for an identity image point (v = O degenerates hiding)") {
    val identity = "0x" + "0" * 128
    evalExpr(dhExpr(dhGHex, dhHHex, dhUHex, identity, dhMsgHex, dhProof))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("prove_dhtuple_verify errors on the wrong arity") {
    evalExpr(s"""{"prove_dhtuple_verify":["$dhGHex","$dhHHex","$dhUHex"]}""").map(r => expect(r.isLeft))
  }

  // ---- gas ----

  test("prove_dhtuple_verify charges its base gas cost (exactly, fixed arity has no scaling term)") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    for {
      expr <- IO.fromEither(parser.parse(dhExpr(dhGHex, dhHHex, dhUHex, dhVHex, dhMsgHex, dhProof)).flatMap(_.as[JsonLogicExpression]))
      res <- evaluator
        .evaluateWithGas(expr, MapValue.empty, None, GasLimit.Unlimited, GasConfig.Default)
        .flatMap(IO.fromEither)
    } yield
      expect(res.value == BoolValue(true)) &&
      // base(prove_dhtuple_verify) + depthPenalty(1); arg constants cost 0. No input-scaled term.
      expect(
        res.gasUsed.amount ==
          GasConfig.Default.proveDhtupleVerify.amount + GasConfig.Default.depthPenalty(1L).amount,
        s"expected dhtuple base+depth, got ${res.gasUsed.amount}"
      )
  }

  test("prove_dhtuple_verify is priced ~2x prove_dlog_verify and runs out of gas under a tight limit") {
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    // A limit below the dhtuple base must exhaust before the (4-mul) primitive runs.
    val tight = GasLimit(GasConfig.Default.proveDhtupleVerify.amount - 1L)
    for {
      expr <- IO.fromEither(parser.parse(dhExpr(dhGHex, dhHHex, dhUHex, dhVHex, dhMsgHex, dhProof)).flatMap(_.as[JsonLogicExpression]))
      res  <- evaluator.evaluateWithGas(expr, MapValue.empty, None, tight, GasConfig.Default)
    } yield
      expect(res.isLeft) &&
      expect(GasConfig.Default.proveDhtupleVerify.amount == 2 * GasConfig.Default.proveDlogVerify.amount - 5_000)
  }

  // ===========================================================================
  // Commitment-recovery helpers (the deferred sigma_verify tree reuses these).
  // ===========================================================================

  pureTest("dlogComputeCommitment recovers R for a valid Schnorr transcript: z*G - e*pk == R") {
    // Rebuild the SAME transcript the standalone verifier uses, then check the helper returns R.
    val x = schnorrX
    val k = schnorrK
    val pk = g1.multiply(x.bigInteger)
    val rPoint = g1.multiply(k.bigInteger) // the commitment R = k*G
    val e = sha256(g1Bytes(rPoint) ++ g1Bytes(pk) ++ schnorrMsg).mod(R)
    val z = (k + e * x).mod(R)
    val recovered = CryptoOps.dlogComputeCommitment(pk, e, z)
    expect(recovered.x == rPoint.x && recovered.y == rPoint.y)
  }

  pureTest("dlogComputeCommitment != R when z is wrong (defends the reconstruction primitive)") {
    val x = schnorrX
    val k = schnorrK
    val pk = g1.multiply(x.bigInteger)
    val rPoint = g1.multiply(k.bigInteger)
    val e = sha256(g1Bytes(rPoint) ++ g1Bytes(pk) ++ schnorrMsg).mod(R)
    val zBad = (k + e * x + BigInt(1)).mod(R) // off by one
    val recovered = CryptoOps.dlogComputeCommitment(pk, e, zBad)
    expect(!(recovered.x == rPoint.x && recovered.y == rPoint.y))
  }

  pureTest("dhtupleComputeCommitment recovers a1 and a2 for a valid DH-tuple transcript") {
    // z*g - e*u == a1  and  z*h - e*v == a2 for the honest (g,h,u,v,a1,a2,z) transcript.
    val w = dhW
    val r = dhR
    val g = dhG
    val h = dhH
    val u = g.multiply(w.bigInteger)
    val v = h.multiply(w.bigInteger)
    val a1 = g.multiply(r.bigInteger)
    val a2 = h.multiply(r.bigInteger)
    val e = dhChallenge(g, h, u, v, a1, a2, dhMsg)
    val z = (r + e * w).mod(R)
    val rec1 = CryptoOps.dhtupleComputeCommitment(g, u, e, z)
    val rec2 = CryptoOps.dhtupleComputeCommitment(h, v, e, z)
    expect(rec1.x == a1.x && rec1.y == a1.y) && expect(rec2.x == a2.x && rec2.y == a2.y)
  }

  // ===========================================================================
  // Worked example: a DH-tuple guard evaluated end-to-end (the morphism-guard shape).
  // ===========================================================================

  private def dhTupleGate(g: String, h: String, u: String, v: String, msg: String, proof: String): String =
    s"""{"if":[ ${dhExpr(g, h, u, v, msg, proof)}, "authorized", "rejected"]}"""

  test("worked example: DH-tuple gate returns 'authorized' for a valid proof") {
    evalExpr(dhTupleGate(dhGHex, dhHHex, dhUHex, dhVHex, dhMsgHex, dhProof))
      .map(r => expect(r == Right(StrValue("authorized"))))
  }

  test("worked example: DH-tuple gate returns 'rejected' for a non-DH-tuple statement") {
    val vWrong = encG1(dhH.multiply((dhW + BigInt(7)).bigInteger))
    evalExpr(dhTupleGate(dhGHex, dhHHex, dhUHex, vWrong, dhMsgHex, dhProof))
      .map(r => expect(r == Right(StrValue("rejected"))))
  }
}
