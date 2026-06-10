package crypto.zk

import java.math.BigInteger

import cats.effect.IO

import scala.io.Source

import io.constellationnetwork.metagraph_sdk.crypto.zk.{Bn254, Sp1Groth16Verifier}

import io.circe.parser.{parse => parseJson}
import org.miracl.core.{BLS12381 => MBLS, BN254 => MBN254, ED25519 => MED}
import weaver.SimpleIOSuite

/**
 * THE GATE (Deliverable 2): does MIRACL Core's `BN254` curve equal Ethereum's
 * alt_bn128, so that it could verify the real SP1 Groth16 proof identically to
 * the vendored Hyperledger Besu verifier?
 *
 * ==Verdict: NO.==
 *
 * MIRACL ships the original Barreto–Naehrig BN254 (a.k.a. "Naehrig" BN254),
 * whose base-field prime and group order are
 * {{{
 *   p = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
 *   r = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
 * }}}
 * Ethereum's alt_bn128 (EIP-196/197), the curve every SP1/gnark Groth16 proof
 * is defined over, uses a different BN seed and therefore a different field
 * {{{
 *   p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
 *   r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
 * }}}
 * They are incompatible curves, so MIRACL's BN254 pairing cannot be substituted
 * for Besu's alt_bn128 in the Groth16 verifier. This suite proves that finding
 * end-to-end and documents what we keep instead.
 *
 * What this suite establishes:
 *   (a) MIRACL BN254 ROM constants != alt_bn128 (the gate fails — verbatim ROM).
 *   (b) MIRACL's BN254 pairing code is nonetheless correct *on its own curve*
 *       (bilinearity + non-degeneracy), so the failure is a curve-parameter
 *       mismatch, not a broken pairing implementation.
 *   (c) MIRACL ED25519 and BLS12381 ROM constants DO match their canonical
 *       values (these curves are safe to adopt from MIRACL).
 *   (d) The real SP1 Groth16 fixture still verifies TRUE via the Besu verifier
 *       and FALSE when tampered — i.e. Besu remains the correct Groth16 backend.
 */
object MiraclBn254CompatSuite extends SimpleIOSuite {

  // ---------------------------------------------------------------------------
  // MIRACL ROM -> BigInteger reconstruction.
  //
  // MIRACL stores field elements as little-endian limbs of `BASEBITS` bits each
  // (long[]). BASEBITS is 56 for BN254/ED25519 and 58 for BLS12381.
  // ---------------------------------------------------------------------------

  private def reconstruct(limbs: Array[Long], baseBits: Int): BigInteger =
    limbs.zipWithIndex.foldLeft(BigInteger.ZERO) {
      case (acc, (limb, i)) =>
        acc.add(BigInteger.valueOf(limb).shiftLeft(baseBits * i))
    }

  // alt_bn128 (Ethereum / EIP-197) constants.
  private val AltBn128P: BigInteger =
    new BigInteger("30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47", 16)
  private val AltBn128R: BigInteger =
    new BigInteger("30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001", 16)

  // ---------------------------------------------------------------------------
  // (a) THE GATE: MIRACL BN254 ROM != alt_bn128.
  // ---------------------------------------------------------------------------

  pureTest("GATE: MIRACL BN254 base-field prime is NOT alt_bn128's prime") {
    val miraclP = reconstruct(MBN254.ROM.Modulus, MBN254.CONFIG_BIG.BASEBITS)
    val naehrigP =
      new BigInteger("2523648240000001ba344d80000000086121000000000013a700000000000013", 16)
    expect(miraclP == naehrigP)
      .and(expect(miraclP != AltBn128P))
      .and(expect(Bn254.P == AltBn128P)) // our Besu-backed wrapper IS alt_bn128
  }

  pureTest("GATE: MIRACL BN254 group order is NOT alt_bn128's group order") {
    val miraclR = reconstruct(MBN254.ROM.CURVE_Order, MBN254.CONFIG_BIG.BASEBITS)
    val naehrigR =
      new BigInteger("2523648240000001ba344d8000000007ff9f800000000010a10000000000000d", 16)
    expect(miraclR == naehrigR)
      .and(expect(miraclR != AltBn128R))
      .and(expect(Bn254.R == AltBn128R))
  }

  // ---------------------------------------------------------------------------
  // (b) MIRACL's BN254 pairing is correct ON ITS OWN CURVE: bilinearity and
  //     non-degeneracy. This proves the pairing implementation is sound — the
  //     gate fails purely because the curve parameters differ from alt_bn128.
  // ---------------------------------------------------------------------------

  pureTest("MIRACL BN254 pairing is non-degenerate: e(G1, G2) != 1") {
    val g1 = MBN254.ECP.generator()
    val g2 = MBN254.ECP2.generator()
    val gt = MBN254.PAIR.fexp(MBN254.PAIR.ate(g2, g1))
    expect(!gt.isunity())
  }

  pureTest("MIRACL BN254 pairing is bilinear: e(a*G1, b*G2) == e(G1, G2)^(a*b)") {
    val g1 = MBN254.ECP.generator()
    val g2 = MBN254.ECP2.generator()

    val a = new MBN254.BIG(31337)
    val b = new MBN254.BIG(271828)

    val aG1 = MBN254.PAIR.G1mul(g1, a)
    val bG2 = MBN254.PAIR.G2mul(g2, b)

    // LHS = e(a*G1, b*G2)
    val lhs = MBN254.PAIR.fexp(MBN254.PAIR.ate(bG2, aG1))

    // RHS = e(G1, G2)^(a*b)
    val base = MBN254.PAIR.fexp(MBN254.PAIR.ate(g2, g1))
    val ab = MBN254.BIG.modmul(a, b, new MBN254.BIG(MBN254.ROM.CURVE_Order))
    val rhs = MBN254.PAIR.GTpow(base, ab)

    expect(lhs.equals(rhs))
  }

  pureTest("MIRACL BN254 product-of-pairings identity: e(G1,G2) * e(-G1,G2) == 1") {
    val g1 = MBN254.ECP.generator()
    val g2 = MBN254.ECP2.generator()
    val negG1 = new MBN254.ECP()
    negG1.copy(g1)
    negG1.neg()

    val ml = MBN254.PAIR.initmp()
    MBN254.PAIR.another(ml, g2, g1)
    MBN254.PAIR.another(ml, g2, negG1)
    val gt = MBN254.PAIR.fexp(MBN254.PAIR.miller(ml))
    expect(gt.isunity())
  }

  // ---------------------------------------------------------------------------
  // (c) MIRACL ED25519 and BLS12381 ROM constants MATCH canonical values.
  // ---------------------------------------------------------------------------

  pureTest("MIRACL ED25519 ROM matches canonical Ed25519 (p = 2^255 - 19, standard order)") {
    val p = reconstruct(MED.ROM.Modulus, MED.CONFIG_BIG.BASEBITS)
    val order = reconstruct(MED.ROM.CURVE_Order, MED.CONFIG_BIG.BASEBITS)
    val expectedP = BigInteger.TWO.pow(255).subtract(BigInteger.valueOf(19))
    val expectedOrder =
      BigInteger.TWO
        .pow(252)
        .add(new BigInteger("27742317777372353535851937790883648493"))
    expect(p == expectedP).and(expect(order == expectedOrder))
  }

  pureTest("MIRACL BLS12381 ROM matches canonical BLS12-381 constants") {
    val p = reconstruct(MBLS.ROM.Modulus, MBLS.CONFIG_BIG.BASEBITS)
    val r = reconstruct(MBLS.ROM.CURVE_Order, MBLS.CONFIG_BIG.BASEBITS)
    val expectedP = new BigInteger(
      "1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab",
      16
    )
    val expectedR =
      new BigInteger("73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001", 16)
    expect(p == expectedP).and(expect(r == expectedR))
  }

  // ---------------------------------------------------------------------------
  // (d) The real SP1 Groth16 fixture verifies via the Besu-backed verifier
  //     (which IS alt_bn128) — TRUE for the real proof, FALSE when tampered.
  //     This is the fallback the gate mandates: keep Besu for Groth16.
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

  test("FALLBACK: real SP1 Groth16 fixture verifies TRUE via Besu (alt_bn128)") {
    IO {
      val result = Sp1Groth16Verifier.verify(fixture.vkey, fixture.publicValues, fixture.proofBytes)
      expect(result == Right(()))
    }
  }

  test("FALLBACK: tampered SP1 Groth16 fixture verifies FALSE via Besu (alt_bn128)") {
    IO {
      val tampered = fixture.proofBytes.clone()
      tampered(tampered.length - 1) = (tampered(tampered.length - 1) ^ 0x01).toByte
      val result = Sp1Groth16Verifier.verify(fixture.vkey, fixture.publicValues, tampered)
      expect(result.isLeft)
    }
  }
}
