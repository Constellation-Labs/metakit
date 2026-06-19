package io.constellationnetwork.metagraph_sdk.json_logic.ops

import cats.syntax.either._

import io.constellationnetwork.metagraph_sdk.json_logic.core._

/**
 * Pure, deterministic hex <-> number opcodes for the JLVM.
 *
 * Like [[CryptoOps]], every function returns `Either[JsonLogicException, JsonLogicValue]` and NEVER
 * throws to the caller: wrong arity, a non-string argument, or malformed hex (odd-length body,
 * non-hex chars, missing `0x`/non-lowercase prefix) all map to a `JsonLogicException`. Byte parsing
 * is delegated to the shared [[HexBytes]] codec so the accepted hex grammar is identical to the
 * crypto opcodes (lowercase, `0x`-prefixed, even-length, big-endian).
 */
object HexOps {

  // ---------------------------------------------------------------------------
  // hex_to_int: [hex] -> non-negative big-endian integer (arbitrary precision).
  // ---------------------------------------------------------------------------

  /**
   * Parse a single arbitrary-length hex string into raw bytes (reusing [[HexBytes.parseBytes]] with
   * no fixed width, exactly as the crypto opcodes parse arbitrary-byte arguments) and interpret
   * those bytes as an UNSIGNED big-endian integer (`BigInt(1, bytes)`). Empty bytes (`"0x"`) yield
   * `0`; the result is ALWAYS non-negative. Whatever the codec accepts (the `0x` prefix, lowercase,
   * even length) is inherited; malformed hex propagates as a `JsonLogicException`.
   */
  def hexToInt(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case hexV :: Nil =>
        for {
          hex   <- expectStr("hex_to_int")(hexV)
          bytes <- HexBytes.parseBytes(hex, None, "hex_to_int")
        } yield IntValue(BigInt(1, bytes))
      case _ =>
        JsonLogicException(s"hex_to_int: expected [hex], got $values").asLeft
    }

  // ---------------------------------------------------------------------------
  // Shared argument helpers.
  // ---------------------------------------------------------------------------

  private def expectStr(role: String)(v: JsonLogicValue): Either[JsonLogicException, String] =
    v match {
      case StrValue(s) => s.asRight
      case other       => JsonLogicException(s"$role: expected a hex string, got ${other.tag}").asLeft
    }
}
