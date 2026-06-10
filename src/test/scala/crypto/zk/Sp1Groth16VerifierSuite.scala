package crypto.zk

import java.math.BigInteger

import cats.effect.IO

import scala.io.Source

import io.constellationnetwork.metagraph_sdk.crypto.zk.{Bn254, Sp1Groth16Verifier}

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
}
