package io.constellationnetwork.metagraph_sdk.json_logic.ops

import cats.syntax.either._

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

  /** The BN254 / alt_bn128 scalar field modulus, shared with [[Poseidon.R]]. */
  val Modulus: BigInt = Poseidon.R

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

  /** Encode raw bytes as a lowercase `0x`-prefixed hex string (exactly `bytes.length` bytes wide). */
  def encodeBytes(bytes: Array[Byte]): String =
    "0x" + bytes.map(b => f"${b & 0xff}%02x").mkString

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
