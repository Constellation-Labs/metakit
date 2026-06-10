package io.constellationnetwork.metagraph_sdk.crypto.zk

import java.math.BigInteger

import org.hyperledger.besu.crypto.altbn128._

/**
 * Thin BN254 (alt_bn128) helper over Hyperledger Besu's pure-Java field/curve
 * implementation. Provides the few primitives a Groth16 verifier needs:
 *   - G1 point construction, addition and scalar multiplication,
 *   - G2 point construction (with EIP-197 Fp2 coordinate ordering documented
 *     at the call sites), and
 *   - a multi-pairing product check, `e(A0,B0) * e(A1,B1) * ... == 1`.
 *
 * Field elements are unreduced `BigInteger`s; the underlying `Fq`/`Fq2`
 * arithmetic reduces them modulo the base field prime P.
 *
 * Fp2 coordinate convention: throughout this codebase an Fp2 element is written
 * as `a0 + a1 * i` and constructed with [[Bn254.fq2]]`(real = a0, imag = a1)`.
 * This matches Besu's `Fq2.create(c0 = real, c1 = imag)` and the SP1/gnark
 * Solidity verifier, whose VK constants use `_0 = real` and `_1 = imag`. Note
 * that the on-chain pairing precompile (and the raw `proof` encoding) lay out
 * Fp2 elements in the *opposite* order, `(imag, real)`; callers are responsible
 * for swapping when decoding such inputs.
 */
object Bn254 {

  /** BN254 base field prime P (Fp modulus). */
  val P: BigInteger =
    new BigInteger("21888242871839275222246405745257275088696311157297823662689037894645226208583")

  /** BN254 scalar field prime R (Fr modulus / curve group order). */
  val R: BigInteger =
    new BigInteger("21888242871839275222246405745257275088548364400416034343698204186575808495617")

  /** A G1 affine point `(x, y)`, or the point at infinity. */
  final case class G1(x: BigInteger, y: BigInteger) {

    private[zk] def toBesu: AltBn128Point =
      new AltBn128Point(Fq.create(x), Fq.create(y))

    def add(other: G1): G1 = G1.fromBesu(toBesu.add(other.toBesu))

    /** Scalar multiplication; the scalar is reduced mod R. */
    def multiply(scalar: BigInteger): G1 =
      G1.fromBesu(toBesu.multiply(scalar.mod(R)))

    def isOnCurve: Boolean = toBesu.isOnCurve
  }

  object G1 {
    val infinity: G1 = G1(BigInteger.ZERO, BigInteger.ZERO)

    private[zk] def fromBesu(p: AltBn128Point): G1 = {
      val xs = p.getX.toBytes
      val ys = p.getY.toBytes
      G1(new BigInteger(1, if (xs.isEmpty) Array[Byte](0) else xs), new BigInteger(1, if (ys.isEmpty) Array[Byte](0) else ys))
    }
  }

  /**
   * A G2 affine point. Each coordinate is an Fp2 element given as
   * `(real, imag)` (i.e. `_0`, `_1` in the Solidity verifier convention).
   */
  final case class G2(
    xReal: BigInteger,
    xImag: BigInteger,
    yReal: BigInteger,
    yImag: BigInteger
  ) {

    private[zk] def toBesu: AltBn128Fq2Point =
      new AltBn128Fq2Point(fq2(xReal, xImag), fq2(yReal, yImag))

    def isOnCurve: Boolean = toBesu.isOnCurve
  }

  /** Build an Fp2 element `real + imag * i` (Besu order: c0 = real, c1 = imag). */
  private[zk] def fq2(real: BigInteger, imag: BigInteger): Fq2 =
    Fq2.create(real, imag)

  /**
   * Multi-pairing product check used by the Groth16 equation. Returns `true`
   * iff `∏ e(g1_i, g2_i) == 1` in the target group GT.
   *
   * Implementation note: `AltBn128Fq12Pairer.pair` returns the *non-finalized*
   * Miller-loop output. Because the final exponentiation is a group
   * homomorphism, we may take the product of the Miller-loop results first and
   * finalize a single time: `finalize(∏ ML(A_i, B_i)) == ∏ e(A_i, B_i)`. This
   * matches the behaviour of the EVM `ECPAIRING` (0x08) precompile, which
   * returns 1 exactly when this product equals the GT identity.
   */
  def pairingProductIsOne(pairs: Seq[(G1, G2)]): Boolean = {
    val millerProduct = pairs.foldLeft(Fq12.one()) {
      case (acc, (g1, g2)) =>
        acc.multiply(AltBn128Fq12Pairer.pair(g1.toBesu, g2.toBesu))
    }
    AltBn128Fq12Pairer.finalize(millerProduct).equals(Fq12.one())
  }
}
