package io.constellationnetwork.metagraph_sdk.crypto.vrf

import java.security.MessageDigest

import org.miracl.core.ED25519._

/**
 * ECVRF-EDWARDS25519-SHA512-TAI implementation per RFC 9381 (final), built on
 * MIRACL Core's ED25519 Edwards arithmetic.
 *
 * Conforms to the published RFC 9381 ECVRF-EDWARDS25519-SHA512-TAI ciphersuite
 * and is anchored on the official RFC 9381 Appendix B.3 test vectors (Examples
 * 16, 17, 18); the generated proofs (pi) match those vectors byte-for-byte:
 *
 *   - suite_string = 0x03
 *   - EC group G = Ed25519 (RFC 8032)
 *   - qLen = 32, cofactor = 8, ptLen = 32, n = 16 (c is 16 bytes), hLen = 64
 *   - Hash = SHA-512
 *   - Key derivation per RFC 8032 §5.1.5
 *   - Nonce generation: ECVRF_nonce_generation_RFC8032 (§5.4.2.2)
 *   - Hash to curve: try_and_increment (§5.4.1.1)
 *   - Challenge generation: ECVRF_challenge_generation (§5.4.3) hashes FIVE
 *     points with the public key Y first: suite||0x02||Y||H||Gamma||U||V||0x00
 *   - zero_string (0x00) suffix in hash_to_curve, hash_points, proof_to_hash
 *
 * Proof format: 80 bytes = Gamma (32) || c (16) || s (32).
 *
 * ==Point / scalar encoding (the compatibility-critical part)==
 * MIRACL's native `ECP.toBytes`/`BIG.toBytes` use big-endian SEC1-style
 * encodings, which are NOT the RFC 8032 wire format. The VRF wire format is
 * RFC 8032 little-endian: a point is its little-endian y-coordinate with the
 * x-sign (LSB of x) stored in the top bit of byte 31; scalars are
 * little-endian. We therefore implement RFC 8032 encode/decode directly on
 * MIRACL's FP/BIG/ECP rather than using MIRACL's serializers. This is what
 * makes outputs byte-identical to the elisabeth-based implementation.
 */
final class MiraclEcVrf25519 {

  import MiraclEcVrf25519._

  /**
   * Generate VRF proof. `secretKey` is a 32-byte Ed25519 seed; returns the
   * 80-byte proof (Gamma || c || s).
   */
  def vrfProof(secretKey: Array[Byte], message: Array[Byte]): Array[Byte] = {
    require(secretKey.length == 32, "Secret key must be 32 bytes")

    // 1. Derive x (secret scalar) and Y (public key) per RFC 8032 §5.1.5
    val hashedSk = sha512(secretKey)
    val x = clampedScalar(hashedSk.take(32))
    val yPoint = basepointMul(x)
    val yBytes = pointToBytes(yPoint)

    // 2. H = ECVRF_hash_to_curve(suite_string, Y, alpha_string)
    val hPoint = hashToCurve(yBytes, message)

    // 3. h_string = point_to_string(H)
    val hBytes = pointToBytes(hPoint)

    // 4. Gamma = [x]*H
    val gammaPoint = pointMul(hPoint, x)
    val gammaBytes = pointToBytes(gammaPoint)

    // 5. k = ECVRF_nonce_generation_RFC8032(SK, h_string)
    val k = nonceGeneration(hashedSk, hBytes)

    // 6. c = ECVRF_challenge_generation(Y, H, Gamma, [k]*B, [k]*H) (RFC 9381 §5.4.3)
    val kB = basepointMul(k)
    val kH = pointMul(hPoint, k)
    val c = hashPoints(yPoint, hPoint, gammaPoint, kB, kH)

    // 7. s = (k + c*x) mod q
    val s = scalarAdd(k, scalarMul(c, x))

    // 8. pi = point_to_string(Gamma) || int_to_string(c, 16) || int_to_string(s, 32)
    val proof = new Array[Byte](ProofBytes)
    System.arraycopy(gammaBytes, 0, proof, 0, PointBytes)
    System.arraycopy(scalarToLeBytes(c, CBytes), 0, proof, PointBytes, CBytes)
    System.arraycopy(scalarToLeBytes(s, ScalarBytes), 0, proof, PointBytes + CBytes, ScalarBytes)
    proof
  }

  /** Verify VRF proof. `publicKey` is 32 bytes, `proof` is 80 bytes. */
  def vrfVerify(publicKey: Array[Byte], message: Array[Byte], proof: Array[Byte]): Boolean =
    if (publicKey.length != PointBytes || proof.length != ProofBytes) false
    else
      try {
        val verdict =
          for {
            yPoint             <- bytesToPoint(publicKey)
            (gammaPoint, c, s) <- decodeProof(proof)
          } yield {
            // H = ECVRF_hash_to_curve(suite_string, Y, alpha_string)
            val hPoint = hashToCurve(publicKey, message)
            // U = [s]*B - [c]*Y
            val uPoint = pointSub(basepointMul(s), pointMul(yPoint, c))
            // V = [s]*H - [c]*Gamma
            val vPoint = pointSub(pointMul(hPoint, s), pointMul(gammaPoint, c))
            // c' = ECVRF_challenge_generation(Y, H, Gamma, U, V) (RFC 9381 §5.4.3);
            // valid iff c == c'.
            scalarEquals16(c, hashPoints(yPoint, hPoint, gammaPoint, uPoint, vPoint))
          }
        verdict.getOrElse(false)
      } catch {
        case _: Exception => false
      }

  /** Extract VRF output hash (beta) from an 80-byte proof, or None. */
  def vrfProofToHash(proof: Array[Byte]): Option[Array[Byte]] =
    if (proof.length != ProofBytes) None
    else
      try
        bytesToPoint(proof.slice(0, PointBytes)).map { gammaPoint =>
          // beta = SHA-512(suite_string || 0x03 || point_to_string([cofactor]*Gamma) || 0x00)
          val cofactorGamma = pointMul(gammaPoint, cofactorScalar)
          val md = MessageDigest.getInstance("SHA-512")
          md.update(SuiteString)
          md.update(0x03.toByte)
          md.update(pointToBytes(cofactorGamma))
          md.update(0x00.toByte)
          md.digest()
        }
      catch {
        case _: Exception => None
      }

  /** Derive the Ed25519 public key from a 32-byte secret seed. */
  def getVerificationKey(secretKey: Array[Byte]): Array[Byte] = {
    require(secretKey.length == 32, "Secret key must be 32 bytes")
    val hashedSk = sha512(secretKey)
    val x = clampedScalar(hashedSk.take(32))
    pointToBytes(basepointMul(x))
  }
}

object MiraclEcVrf25519 {

  /** Shared stateless singleton (mirrors tessellation's `EcVrf25519.default`). */
  val default: MiraclEcVrf25519 = new MiraclEcVrf25519

  val SuiteString: Byte = 0x03
  val PointBytes: Int = 32
  val ScalarBytes: Int = 32
  val CBytes: Int = 16 // n = 16 for Ed25519
  val ProofBytes: Int = PointBytes + CBytes + ScalarBytes // 80

  // ---------------------------------------------------------------------------
  // Curve / field constants (from MIRACL ED25519 ROM).
  // ---------------------------------------------------------------------------

  /** Group order L = 2^252 + 27742317777372353535851937790883648493. */
  private val order: BIG = new BIG(ROM.CURVE_Order)

  /** Edwards d = ROM.CURVE_B (the -121665/121666 constant), as an FP. */
  private def edwardsD: FP = new FP(new BIG(ROM.CURVE_B))

  /** Cofactor 8 as a reduced scalar. */
  private val cofactorScalar: BIG = new BIG(8)

  // ---------------------------------------------------------------------------
  // Scalar arithmetic (mod L), all little-endian on the wire.
  // ---------------------------------------------------------------------------

  /**
   * RFC 8032 clamp on the low 32 bytes of SHA-512(seed): clear bottom 3 bits,
   * clear top bit, set second-highest bit. The clamped value is < L for our
   * arithmetic so we reduce mod L (matching elisabeth's `Scalar.fromBits`
   * which the multiply routines treat mod the group order).
   */
  private def clampedScalar(low32: Array[Byte]): BIG = {
    val pruned = low32.clone()
    pruned(0) = (pruned(0) & 0xf8).toByte
    pruned(31) = (pruned(31) & 0x7f).toByte
    pruned(31) = (pruned(31) | 0x40).toByte
    val b = bigFromLe(pruned)
    b.mod(order)
    b
  }

  /** k = string_to_int(SHA-512(...))  mod L, with a 64-byte little-endian input. */
  private def reduceWideLe(wide64: Array[Byte]): BIG = {
    require(wide64.length == 64, "expected 64-byte wide input")
    // MIRACL DBIG.fromBytes is big-endian; reverse the LE input.
    val be = wide64.reverse
    val d = DBIG.fromBytes(be)
    d.mod(order)
  }

  private def scalarAdd(a: BIG, b: BIG): BIG = BIG.modadd(a, b, order)
  private def scalarMul(a: BIG, b: BIG): BIG = BIG.modmul(a, b, order)

  /** Compare two challenge scalars on their first 16 little-endian bytes. */
  private def scalarEquals16(a: BIG, b: BIG): Boolean = {
    val ab = scalarToLeBytes(a, CBytes)
    val bb = scalarToLeBytes(b, CBytes)
    java.util.Arrays.equals(ab, bb)
  }

  /** Serialize a BIG as `len` little-endian bytes (truncating high bytes). */
  private def scalarToLeBytes(b: BIG, len: Int): Array[Byte] = {
    val be = new Array[Byte](CONFIG_BIG.MODBYTES)
    val tmp = new BIG(b)
    tmp.toBytes(be) // big-endian, MODBYTES long
    val le = be.reverse // now little-endian, MODBYTES long
    val out = new Array[Byte](len)
    System.arraycopy(le, 0, out, 0, math.min(len, le.length))
    out
  }

  /** Build a BIG from up to MODBYTES little-endian bytes (no reduction). */
  private def bigFromLe(le: Array[Byte]): BIG = {
    val be = new Array[Byte](CONFIG_BIG.MODBYTES)
    val n = math.min(le.length, CONFIG_BIG.MODBYTES)
    (0 until n).foreach(i => be(CONFIG_BIG.MODBYTES - 1 - i) = le(i))
    BIG.fromBytes(be)
  }

  // ---------------------------------------------------------------------------
  // Point arithmetic.
  // ---------------------------------------------------------------------------

  private def basepointMul(e: BIG): ECP = ECP.generator().mul(e)
  private def pointMul(p: ECP, e: BIG): ECP = {
    val q = new ECP(p)
    q.mul(e)
  }

  private def pointSub(a: ECP, b: ECP): ECP = {
    val r = new ECP(a)
    r.sub(b)
    r
  }

  // ---------------------------------------------------------------------------
  // RFC 8032 point encode / decode (the compatibility-critical part).
  // ---------------------------------------------------------------------------

  /** point_to_string: 32-byte little-endian y with x's LSB in bit 255. */
  private def pointToBytes(point: ECP): Array[Byte] =
    if (point.is_infinity()) {
      // Encoding of the identity: y = 1, x = 0 -> 0x01 followed by zeros.
      val id = new Array[Byte](PointBytes)
      id(0) = 0x01
      id
    } else {
      val w = new ECP(point)
      w.affine()
      val y = w.getY // BIG, reduced
      val xParity = w.getX.parity() // LSB of x in [0,p)
      val le = scalarToLeBytes(y, PointBytes) // 32 LE bytes of y
      if (xParity == 1) le(31) = (le(31) | 0x80.toByte).toByte
      le
    }

  /** string_to_point: parse y (LE, bit 255 = x sign), recover x = sqrt((y^2-1)/(d*y^2+1)). */
  private def bytesToPoint(bytes: Array[Byte]): Option[ECP] =
    if (bytes.length != PointBytes) None
    else
      try {
        val buf = bytes.clone()
        val xSign = (buf(31) & 0x80) >>> 7
        buf(31) = (buf(31) & 0x7f).toByte
        val yBig = bigFromLe(buf)
        val p = new BIG(ROM.Modulus)
        // Reject y >= p (non-canonical encoding) to match strict decoders.
        if (BIG.comp(yBig, p) >= 0) None
        else {
          val y = new FP(yBig)
          val one = new FP(1)

          // u = y^2 - 1 ; v = d*y^2 + 1
          val y2 = new FP(y); y2.sqr()
          val u = new FP(y2); u.sub(one); u.norm()
          val v = new FP(y2); v.mul(edwardsD); v.add(one); v.norm()

          // x = sqrt(u / v) = sqrt(u * v^{-1})
          val vInv = new FP(v); vInv.inverse(null)
          val uv = new FP(u); uv.mul(vInv); uv.norm()

          // Quadratic-residue check: if uv is not a QR, no point exists for this y.
          if (uv.qr(null) != 1) None
          else {
            val x = uv.sqrt(null)
            x.reduce()
            val xParity = x.sign()
            // Choose the root whose LSB matches xSign; if x == 0 and xSign == 1, reject.
            if (x.iszilch() && xSign == 1) None
            else {
              val xFinal = new FP(x)
              if (xParity != xSign) xFinal.neg()
              xFinal.norm()
              val pt = new ECP(xFinal.redc(), yBig)
              if (pt.is_infinity()) None else Some(pt)
            }
          }
        }
      } catch {
        case _: Exception => None
      }

  // ---------------------------------------------------------------------------
  // ECVRF helpers (mirror tessellation exactly).
  // ---------------------------------------------------------------------------

  private def hashToCurve(publicKey: Array[Byte], alpha: Array[Byte]): ECP = {
    @scala.annotation.tailrec
    def loop(ctr: Int): ECP =
      if (ctr >= 256) throw new RuntimeException("Failed to hash to curve after 256 attempts")
      else {
        val md = MessageDigest.getInstance("SHA-512")
        md.update(SuiteString)
        md.update(0x01.toByte) // one_string
        md.update(publicKey)
        md.update(alpha)
        md.update(ctr.toByte) // ctr_string
        md.update(0x00.toByte) // zero_string (draft-10)
        val hash = md.digest()
        bytesToPoint(hash.slice(0, 32)) match {
          case Some(point) if !pointToBytes(point).forall(_ == 0) =>
            // H = [8]*H (clear cofactor)
            pointMul(point, cofactorScalar)
          case _ => loop(ctr + 1)
        }
      }
    loop(0)
  }

  /** k = SHA-512(hashed_sk[32..63] || h_string) mod q (little-endian). */
  private def nonceGeneration(hashedSk: Array[Byte], hBytes: Array[Byte]): BIG = {
    val truncated = hashedSk.slice(32, 64)
    val md = MessageDigest.getInstance("SHA-512")
    md.update(truncated)
    md.update(hBytes)
    reduceWideLe(md.digest())
  }

  /**
   * ECVRF_challenge_generation (RFC 9381 §5.4.3): hashes FIVE points with the
   * public key Y first.
   *
   * c = SHA-512(suite || 0x02 || Y || H || Gamma || U || V || 0x00)[0..15] as LE int.
   */
  private def hashPoints(y: ECP, h: ECP, gamma: ECP, u: ECP, v: ECP): BIG = {
    val md = MessageDigest.getInstance("SHA-512")
    md.update(SuiteString)
    md.update(0x02.toByte)
    md.update(pointToBytes(y))
    md.update(pointToBytes(h))
    md.update(pointToBytes(gamma))
    md.update(pointToBytes(u))
    md.update(pointToBytes(v))
    md.update(0x00.toByte)
    val hash = md.digest()
    // First 16 bytes as a little-endian integer (< 2^128 < L, no reduction needed).
    bigFromLe(hash.slice(0, CBytes))
  }

  private def decodeProof(proof: Array[Byte]): Option[(ECP, BIG, BIG)] =
    if (proof.length != ProofBytes) None
    else
      bytesToPoint(proof.slice(0, PointBytes)).flatMap { gammaPoint =>
        val c = bigFromLe(proof.slice(PointBytes, PointBytes + CBytes))
        val sBig = bigFromLe(proof.slice(PointBytes + CBytes, ProofBytes))
        // s must be a canonical scalar (< L) for a valid proof.
        if (BIG.comp(sBig, order) >= 0) None else Some((gammaPoint, c, sBig))
      }

  private def sha512(input: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-512").digest(input)
}
