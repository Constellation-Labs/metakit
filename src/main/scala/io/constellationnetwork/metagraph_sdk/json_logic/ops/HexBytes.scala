package io.constellationnetwork.metagraph_sdk.json_logic.ops

import cats.syntax.either._
import cats.syntax.traverse._

import io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon
import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicException

/**
 * Shared codec for the FIXED hex-encoding convention used by every crypto opcode in the JLVM.
 *
 * Convention (implemented exactly):
 *   - All byte / field arguments and returns are lowercase, `0x`-prefixed, big-endian hex strings.
 *   - There is NO new JSON Logic value type; bytes are a validated special-case of `string`, parsed
 *     and validated only at the opcode boundary.
 *   - Every malformed input (bad hex, wrong width, non-canonical field element, etc.) returns a
 *     `JsonLogicException` via `Either` -- this codec NEVER throws to the caller.
 *
 * Validation rules:
 *   - The string must match `^0x[0-9a-f]*$` (lowercase only, `0x` prefix mandatory, hex body may be
 *     empty for arbitrary-width byte args).
 *   - The hex body must have even length (whole bytes).
 *   - When an expected byte width is supplied, the decoded length must equal it exactly.
 *   - Field elements ([[parseFr]]) are exactly 32 bytes AND must be canonical, i.e. the big-endian
 *     value is strictly `< Poseidon.R` (the BN254 / alt_bn128 scalar field modulus). Non-canonical
 *     32-byte values are rejected.
 *
 * Encoding ([[encodeBytes]] / [[encodeFr]]) always produces the canonical lowercase form: `0x` plus
 * the zero-padded hex of the fixed width.
 */
object HexBytes {

  /** Byte width of a BN254 Fr field element. */
  val FrBytes: Int = 32

  /** Byte width of a single BN254 (alt_bn128) base-field coordinate. */
  val FqBytes: Int = 32

  /** Byte width of a serialized BN254 G1 point (`x || y`, 32B each). */
  val G1Bytes: Int = 64

  /** Byte width of a serialized BN254 G2 point (EIP-197 order, 4 x 32B). */
  val G2Bytes: Int = 128

  /** Byte width of a 256-bit big-endian scalar (e.g. a Schnorr response `s`). */
  val ScalarBytes: Int = 32

  /** The BN254 / alt_bn128 scalar field modulus, shared with [[Poseidon.R]]. */
  val Modulus: BigInt = Poseidon.R

  /** The BN254 / alt_bn128 base-field (Fp) modulus P. */
  val BaseFieldModulus: BigInt =
    BigInt("21888242871839275222246405745257275088696311157297823662689037894645226208583")

  private val HexPattern = "^0x[0-9a-f]*$".r

  /**
   * Parse and validate a lowercase `0x`-prefixed hex string into raw bytes (big-endian).
   *
   * @param hex          the candidate hex string.
   * @param expectedLen  if `Some(n)`, the decoded byte length must equal `n`; if `None`, any
   *                     even-length body is accepted (arbitrary-width bytes).
   * @param role         human-readable name of the argument, used only in error messages.
   */
  def parseBytes(
    hex: String,
    expectedLen: Option[Int],
    role: String
  ): Either[JsonLogicException, Array[Byte]] =
    for {
      _ <- Either.cond(
        HexPattern.matches(hex),
        (),
        JsonLogicException(s"$role: malformed hex (expected lowercase ^0x[0-9a-f]*$$): '$hex'")
      )
      body = hex.substring(2)
      _ <- Either.cond(
        body.length % 2 == 0,
        (),
        JsonLogicException(s"$role: odd-length hex body (${body.length} nibbles): '$hex'")
      )
      bytes = decodeUnchecked(body)
      _ <- expectedLen match {
        case Some(n) =>
          Either.cond(
            bytes.length == n,
            (),
            JsonLogicException(s"$role: expected $n bytes, got ${bytes.length}")
          )
        case None => ().asRight[JsonLogicException]
      }
    } yield bytes

  /**
   * Parse a 32-byte hex string into a canonical BN254 Fr field element (`0 <= value < Modulus`).
   * Rejects wrong width and non-canonical values.
   */
  def parseFr(hex: String, role: String): Either[JsonLogicException, BigInt] =
    parseBytes(hex, Some(FrBytes), role).flatMap { bytes =>
      val value = BigInt(1, bytes)
      Either.cond(
        value < Modulus,
        value,
        JsonLogicException(
          s"$role: not a canonical BN254 field element (must be < modulus): $value"
        )
      )
    }

  /**
   * Parse a 32-byte hex string into a canonical BN254 base-field (Fq) coordinate
   * (`0 <= value < P`). Rejects wrong width and non-canonical (`>= P`) values.
   */
  def parseFq(hex: String, role: String): Either[JsonLogicException, BigInt] =
    parseBytes(hex, Some(FqBytes), role).flatMap { bytes =>
      val value = BigInt(1, bytes)
      Either.cond(
        value < BaseFieldModulus,
        value,
        JsonLogicException(
          s"$role: not a canonical BN254 base-field element (must be < P): $value"
        )
      )
    }

  /**
   * Parse a 64-byte hex string into a BN254 G1 affine coordinate pair `(x, y)`.
   * Each 32-byte half is validated as a canonical Fq element (`< P`). The
   * all-zero point `(0, 0)` is the EVM point-at-infinity and is accepted here;
   * on-curve membership is enforced by the caller (it is trivially satisfied by
   * `(0, 0)`).
   */
  def parseG1(hex: String, role: String): Either[JsonLogicException, (BigInt, BigInt)] =
    parseBytes(hex, Some(G1Bytes), role).flatMap { bytes =>
      val x = BigInt(1, bytes.slice(0, FqBytes))
      val y = BigInt(1, bytes.slice(FqBytes, G1Bytes))
      for {
        _ <- Either.cond(x < BaseFieldModulus, (), JsonLogicException(s"$role: x not in base field (>= P): $x"))
        _ <- Either.cond(y < BaseFieldModulus, (), JsonLogicException(s"$role: y not in base field (>= P): $y"))
      } yield (x, y)
    }

  /**
   * Parse a 128-byte hex string into a BN254 G2 affine point in EIP-197 byte
   * order, i.e. each Fp2 coordinate is serialized imaginary-part-first:
   * `x.c1 || x.c0 || y.c1 || y.c0`. Returns `(xReal, xImag, yReal, yImag)` (the
   * Besu / SP1 `(real, imag)` convention), so the caller can build a
   * `Bn254.G2(xReal, xImag, yReal, yImag)` directly. Each 32-byte limb is
   * validated as a canonical Fq element (`< P`).
   */
  def parseG2(hex: String, role: String): Either[JsonLogicException, (BigInt, BigInt, BigInt, BigInt)] =
    parseBytes(hex, Some(G2Bytes), role).flatMap { bytes =>
      def limb(i: Int): BigInt = BigInt(1, bytes.slice(i * FqBytes, (i + 1) * FqBytes))
      // EIP-197 order: imaginary-before-real for each Fp2 coordinate.
      val xImag = limb(0)
      val xReal = limb(1)
      val yImag = limb(2)
      val yReal = limb(3)
      val all = List("x.imag" -> xImag, "x.real" -> xReal, "y.imag" -> yImag, "y.real" -> yReal)
      all.traverse {
        case (name, v) =>
          Either.cond(v < BaseFieldModulus, (), JsonLogicException(s"$role: $name not in base field (>= P): $v"))
      }
        .map(_ => (xReal, xImag, yReal, yImag))
    }

  /**
   * Parse a 32-byte hex string into a non-negative big-endian scalar with NO
   * field-canonicity constraint (any 256-bit value is accepted). Used for
   * Schnorr responses and similar values that are reduced mod the group order
   * by the consuming primitive.
   */
  def parseScalar(hex: String, role: String): Either[JsonLogicException, BigInt] =
    parseBytes(hex, Some(ScalarBytes), role).map(bytes => BigInt(1, bytes))

  /**
   * Validate a `0x`-prefixed lowercase hex string and return its raw hex body (no `0x`), allowing an
   * ODD number of nibbles.
   *
   * Used for auth-DB (MPT/SMT) keys and prefixes, which are nibble-granular paths -- unlike the
   * byte-array arguments of the wave-1/2 crypto ops, a key/prefix may have an odd nibble count (a
   * 1-nibble prefix is legal). The body may be empty (the empty prefix). Returns the lowercase body so
   * it can be handed straight to `Hex(_)`.
   */
  def parseNibbleHex(hex: String, role: String): Either[JsonLogicException, String] =
    Either.cond(
      HexPattern.matches(hex),
      hex.substring(2),
      JsonLogicException(s"$role: malformed hex (expected lowercase ^0x[0-9a-f]*$$): '$hex'")
    )

  /** Encode raw bytes as a lowercase `0x`-prefixed hex string (exactly `bytes.length` bytes wide). */
  def encodeBytes(bytes: Array[Byte]): String =
    "0x" + bytes.map(b => f"${b & 0xff}%02x").mkString

  /** Encode a BN254 G1 point `(x, y)` as a 64-byte `0x`-hex string (`x || y`, 32B each). */
  def encodeG1(x: BigInt, y: BigInt): Either[JsonLogicException, String] =
    for {
      xs <- encodeUInt(x, FqBytes)
      ys <- encodeUInt(y, FqBytes)
    } yield "0x" + xs.substring(2) + ys.substring(2)

  /** Encode a non-negative `BigInt` as a `0x`-prefixed, big-endian, zero-padded hex of `width` bytes. */
  def encodeUInt(value: BigInt, width: Int): Either[JsonLogicException, String] =
    if (value < 0) JsonLogicException(s"cannot encode negative value as hex: $value").asLeft
    else {
      val raw = value.toString(16)
      val padded = "0" * math.max(0, width * 2 - raw.length) + raw
      if (padded.length > width * 2)
        JsonLogicException(s"value $value does not fit in $width bytes").asLeft
      else ("0x" + padded).asRight
    }

  /** Encode a canonical Fr element as a 32-byte `0x`-prefixed hex string. */
  def encodeFr(value: BigInt): Either[JsonLogicException, String] =
    encodeUInt(value, FrBytes)

  // Body is guaranteed even-length and all `[0-9a-f]` by the time this is called.
  private def decodeUnchecked(body: String): Array[Byte] =
    body.grouped(2).map(h => Integer.parseInt(h, 16).toByte).toArray
}
