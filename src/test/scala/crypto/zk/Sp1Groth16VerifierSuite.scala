package crypto.zk

import java.math.BigInteger

import cats.effect.IO

import scala.io.Source

import io.constellationnetwork.metagraph_sdk.crypto.zk.{Bn254, Groth16Verifier, Sp1Groth16Verifier}

import io.circe.parser.{parse => parseJson}
import org.hyperledger.besu.crypto.altbn128.{AltBn128Fq2Point, AltBn128Point, Fq2}
import weaver.SimpleIOSuite

/**
 * Pure-JVM verification of a REAL SP1 Groth16-BN254 proof (no native deps, no
 * Docker). The decisive correctness check is the positive test, which verifies
 * an actual proof produced by SP1 (circuit v6.1.0) for the JsonLogic program
 * {{{ {"if":[{">":[{"var":"amount"},100]},"premium","standard"]} }}} on
 * `{"amount":150}`, yielding the public output `"premium"`.
 *
 * The fixture (vkey, public values, proof bytes) lives in
 * `src/test/resources/zk/sp1-groth16-premium.json`.
 */
object Sp1Groth16VerifierSuite extends SimpleIOSuite {

  // ---------------------------------------------------------------------------
  // Fixture loading
  // ---------------------------------------------------------------------------

  private def hexToBytes(hex0: String): Array[Byte] = {
    val hex = if (hex0.startsWith("0x")) hex0.substring(2) else hex0
    require(hex.length % 2 == 0, s"odd-length hex string: ${hex.length}")
    hex.grouped(2).map(h => Integer.parseInt(h, 16).toByte).toArray
  }

  final private case class Fixture(vkey: Array[Byte], publicValues: Array[Byte], proofBytes: Array[Byte])

  private val fixture: Fixture = {
    val raw = {
      val src = Source.fromInputStream(getClass.getResourceAsStream("/zk/sp1-groth16-premium.json"), "UTF-8")
      try src.mkString
      finally src.close()
    }
    val json = parseJson(raw).fold(throw _, identity)
    val cur = json.hcursor
    def field(name: String): String = cur.get[String](name).fold(throw _, identity)
    Fixture(
      vkey = hexToBytes(field("vkey")),
      publicValues = hexToBytes(field("publicValues")),
      proofBytes = hexToBytes(field("proofBytes"))
    )
  }

  /** Flip the lowest bit of the byte at `idx` (0-based) of a copy of `bytes`. */
  private def flipByte(bytes: Array[Byte], idx: Int): Array[Byte] = {
    val copy = bytes.clone()
    copy(idx) = (copy(idx) ^ 0x01).toByte
    copy
  }

  // ---------------------------------------------------------------------------
  // 0. Pairing sanity check against a known EIP-197 identity (run BEFORE the
  //    real-proof test). A wrong pairing is the #1 risk, so we first prove the
  //    vendored BN254 pairing satisfies the bilinearity identity
  //      e(a*G1, b*G2) * e((-(a*b mod r))*G1, G2) == 1
  //    which forces the Miller loop, final exponentiation and GT-identity
  //    comparison to all be correct. We also check the degenerate
  //      e(G1, G2) * e(-G1, G2) == 1.
  // ---------------------------------------------------------------------------

  private def unsigned(bytes: Array[Byte]): BigInteger =
    new BigInteger(1, if (bytes.isEmpty) Array[Byte](0) else bytes)

  private val g1: Bn254.G1 = {
    val g = AltBn128Point.g1()
    Bn254.G1(unsigned(g.getX.toBytes), unsigned(g.getY.toBytes))
  }
  private val g2: Bn254.G2 =
    // BN254 G2 generator (real, imag for x and y), EIP-197 canonical values.
    Bn254.G2(
      xReal = new BigInteger("10857046999023057135944570762232829481370756359578518086990519993285655852781"),
      xImag = new BigInteger("11559732032986387107991004021392285783925812861821192530917403151452391805634"),
      yReal = new BigInteger("8495653923123431417604973247489272438418190587263600148770280649306958101930"),
      yImag = new BigInteger("4082367875863433681332203403145435568316851327593401208105741076214120093531")
    )

  pureTest("EIP-197 pairing identity: e(G1, G2) * e(-G1, G2) == 1") {
    val negG1 = Bn254.G1(g1.x, Bn254.P.subtract(g1.y))
    expect(Bn254.pairingProductIsOne(Seq(g1 -> g2, negG1 -> g2)))
  }

  pureTest("EIP-197 pairing bilinearity: e(a*G1, b*G2) * e(-(ab)*G1, G2) == 1") {
    val a = BigInteger.valueOf(31337L)
    val b = BigInteger.valueOf(271828L)
    val aG1 = g1.multiply(a)
    // b * G2 via the underlying point arithmetic on the BN254 G2 generator.
    val g2Besu = new AltBn128Fq2Point(
      Fq2.create(g2.xReal, g2.xImag),
      Fq2.create(g2.yReal, g2.yImag)
    )
    val bG2Besu = g2Besu.multiply(b.mod(Bn254.R))
    val bG2 = Bn254.G2(
      xReal = unsigned(bG2Besu.getX.getCoefficients()(0).toBytes),
      xImag = unsigned(bG2Besu.getX.getCoefficients()(1).toBytes),
      yReal = unsigned(bG2Besu.getY.getCoefficients()(0).toBytes),
      yImag = unsigned(bG2Besu.getY.getCoefficients()(1).toBytes)
    )
    val ab = a.multiply(b).mod(Bn254.R)
    val negAbG1 = g1.multiply(Bn254.R.subtract(ab))
    expect(Bn254.pairingProductIsOne(Seq(aG1 -> bG2, negAbG1 -> g2)))
  }

  pureTest("non-pairing: e(G1, G2) alone is NOT 1") {
    expect(!Bn254.pairingProductIsOne(Seq(g1 -> g2)))
  }

  // ---------------------------------------------------------------------------
  // 1. The real proof verifies.
  // ---------------------------------------------------------------------------

  test("verify(programVKey, publicValues, proofBytes) SUCCEEDS for the real SP1 proof") {
    IO {
      val result = Sp1Groth16Verifier.verify(fixture.vkey, fixture.publicValues, fixture.proofBytes)
      expect(result == Right(()))
    }
  }

  // ---------------------------------------------------------------------------
  // 2. Tampered proof (flip a byte in the last proof field element) is rejected.
  // ---------------------------------------------------------------------------

  test("tampered proof (flip one byte of the last proof field element) is REJECTED") {
    IO {
      // Last 32-byte word of the 356-byte proofBytes => indices 324..355; flip the last byte.
      val tampered = flipByte(fixture.proofBytes, fixture.proofBytes.length - 1)
      val result = Sp1Groth16Verifier.verify(fixture.vkey, fixture.publicValues, tampered)
      expect(result.isLeft)
    }
  }

  // ---------------------------------------------------------------------------
  // 3. Wrong public values (flip a byte) is rejected.
  // ---------------------------------------------------------------------------

  test("wrong publicValues (flip a byte) is REJECTED") {
    IO {
      val tampered = flipByte(fixture.publicValues, 0)
      val result = Sp1Groth16Verifier.verify(fixture.vkey, tampered, fixture.proofBytes)
      expect(result.isLeft)
    }
  }

  // ---------------------------------------------------------------------------
  // 4. Wrong selector is rejected.
  // ---------------------------------------------------------------------------

  test("wrong selector is REJECTED") {
    IO {
      val tampered = flipByte(fixture.proofBytes, 0) // corrupt the 4-byte selector
      val result = Sp1Groth16Verifier.verify(fixture.vkey, fixture.publicValues, tampered)
      expect(result == Left("wrong verifier selector"))
    }
  }

  // ---------------------------------------------------------------------------
  // Extra coverage: a flipped programVKey changes Groth16 input[0] and must fail.
  // ---------------------------------------------------------------------------

  test("wrong programVKey is REJECTED") {
    IO {
      val tampered = flipByte(fixture.vkey, 0)
      val result = Sp1Groth16Verifier.verify(tampered, fixture.publicValues, fixture.proofBytes)
      expect(result.isLeft)
    }
  }

  // ---------------------------------------------------------------------------
  // Soundness hardening: proof-point validation (on-curve, subgroup, range).
  //   Mirrors the Rust crypto.rs adversarial unit tests. The proof[8] words live
  //   at byte offset 4 + 32*3 = 100; word i is [100 + 32*i, 100 + 32*(i+1)).
  //   Layout (EIP-197): 0=A.x 1=A.y 2=B.x_imag 3=B.x_real 4=B.y_imag 5=B.y_real
  //   6=C.x 7=C.y.
  // ---------------------------------------------------------------------------

  private val ProofWordBase: Int = 4 + 32 * 3

  /** Overwrite proof word `idx` with the 32-byte big-endian encoding of `value`. */
  private def setProofWord(bytes: Array[Byte], idx: Int, value: BigInteger): Array[Byte] = {
    val copy = bytes.clone()
    val word = new Array[Byte](32)
    val raw = value.toByteArray // big-endian, possibly with a sign byte or shorter
    val src = if (raw.length > 32) raw.takeRight(32) else raw
    System.arraycopy(src, 0, word, 32 - src.length, src.length)
    System.arraycopy(word, 0, copy, ProofWordBase + idx * 32, 32)
    copy
  }

  private def proofWord(bytes: Array[Byte], idx: Int): BigInteger = {
    val word = new Array[Byte](32)
    System.arraycopy(bytes, ProofWordBase + idx * 32, word, 0, 32)
    new BigInteger(1, word)
  }

  // An on-curve BN254 G2 point at x=(real=2, imag=1) that is NOT in the order-r
  // subgroup (constructed offline; see crypto.rs ADV_NONSUB_B).
  private val NonSubgroupBxReal: BigInteger = BigInteger.valueOf(2)
  private val NonSubgroupBxImag: BigInteger = BigInteger.ONE
  private val NonSubgroupByReal: BigInteger =
    new BigInteger("7292567877523311580221095596750716176434782432868683424513645834767876293070")
  private val NonSubgroupByImag: BigInteger =
    new BigInteger("19659275751359636165940301690575149581329631496732780143538578556285923319774")

  test("ADVERSARIAL: off-curve proof point A is REJECTED as false (no encoding prefix)") {
    IO {
      // A.y := A.y + 1 -> still < P, but the point is no longer on the curve.
      val ay = proofWord(fixture.proofBytes, 1)
      val tampered = setProofWord(fixture.proofBytes, 1, ay.add(BigInteger.ONE).mod(Bn254.P))
      val result = Sp1Groth16Verifier.verify(fixture.vkey, fixture.publicValues, tampered)
      expect(result.isLeft).and(expect(result.swap.exists(!_.startsWith(Groth16Verifier.EncodingErrorPrefix))))
    }
  }

  test("ADVERSARIAL: on-curve-but-non-subgroup proof point B is REJECTED (subgroup check fires)") {
    IO {
      // Isolate the subgroup check: the planted point must pass the on-curve
      // check (otherwise this test would also pass via the wrong rejection path).
      val planted = Bn254.G2(NonSubgroupBxReal, NonSubgroupBxImag, NonSubgroupByReal, NonSubgroupByImag)
      val t0 = setProofWord(fixture.proofBytes, 2, NonSubgroupBxImag) // B.x_imag
      val t1 = setProofWord(t0, 3, NonSubgroupBxReal) // B.x_real
      val t2 = setProofWord(t1, 4, NonSubgroupByImag) // B.y_imag
      val t3 = setProofWord(t2, 5, NonSubgroupByReal) // B.y_real
      val result = Sp1Groth16Verifier.verify(fixture.vkey, fixture.publicValues, t3)
      expect(planted.isOnCurve)
        .and(expect(!planted.isInGroup))
        .and(expect(result.isLeft))
        .and(expect(result.swap.exists(_.contains("subgroup"))))
        .and(expect(result.swap.exists(!_.startsWith(Groth16Verifier.EncodingErrorPrefix))))
    }
  }

  test("ADVERSARIAL: a proof coordinate >= P is an ENCODING error (not a false verdict)") {
    IO {
      // A.x := P (== base-field modulus) -> non-canonical encoding.
      val tampered = setProofWord(fixture.proofBytes, 0, Bn254.P)
      val result = Sp1Groth16Verifier.verify(fixture.vkey, fixture.publicValues, tampered)
      expect(result.isLeft).and(expect(result.swap.exists(_.startsWith(Groth16Verifier.EncodingErrorPrefix))))
    }
  }
}
