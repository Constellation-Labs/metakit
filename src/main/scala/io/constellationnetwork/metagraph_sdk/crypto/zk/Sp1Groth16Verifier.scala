package io.constellationnetwork.metagraph_sdk.crypto.zk

import java.math.BigInteger
import java.security.MessageDigest

/**
 * Pure-JVM port of SP1's `SP1VerifierGroth16.sol` wrapper (circuit version
 * v6.1.0). It performs the SP1-specific framing checks and then delegates the
 * actual zk check to [[Groth16Verifier]].
 *
 * `proofBytes` layout (all big-endian):
 * {{{
 *   selector : 4 bytes  (== first 4 bytes of VERIFIER_HASH)
 *   exitCode : uint256  (32 bytes)
 *   vkRoot   : uint256  (32 bytes)
 *   nonce    : uint256  (32 bytes)
 *   proof    : uint256[8] (8 * 32 bytes, fixed-size array encoded inline)
 * }}}
 * i.e. `4 + 32 * 11 = 356` bytes.
 *
 * The Groth16 public inputs are assembled as
 * {{{ [programVKey, publicValuesDigest, exitCode, vkRoot, nonce] }}}
 * where `publicValuesDigest = sha256(publicValues) & ((1 << 253) - 1)`.
 */
object Sp1Groth16Verifier {

  /** First 4 bytes of `VERIFIER_HASH()` from SP1VerifierGroth16.sol (v6.1.0). */
  val VerifierSelector: Array[Byte] =
    Array(0x43, 0x88, 0xa2, 0x1c).map(_.toByte)

  /** `VK_ROOT()` from SP1VerifierGroth16.sol (v6.1.0). */
  val VkRoot: BigInteger =
    new BigInteger("002f850ee998974d6cc00e50cd0814b098c05bfade466d28573240d057f25352", 16)

  /** Mask `(1 << 253) - 1` applied to the public-values sha256 digest. */
  private val DigestMask: BigInteger =
    BigInteger.ONE.shiftLeft(253).subtract(BigInteger.ONE)

  private val ExpectedProofLength: Int = 4 + 32 * 11 // selector + (exitCode, vkRoot, nonce, proof[8])

  /** `sha256(publicValues) & ((1 << 253) - 1)`. */
  def hashPublicValues(publicValues: Array[Byte]): BigInteger = {
    val digest = MessageDigest.getInstance("SHA-256").digest(publicValues)
    new BigInteger(1, digest).and(DigestMask)
  }

  /**
   * Verify an SP1 Groth16 proof.
   *
   * @param programVKey  the 32-byte program verification key (`bytes32`).
   * @param publicValues the committed public values, raw bytes.
   * @param proofBytes   the SP1 proof bytes (selector ++ abi-encoded fields).
   * @return `Right(())` on success, `Left(reason)` on any failure.
   */
  def verify(
    programVKey: Array[Byte],
    publicValues: Array[Byte],
    proofBytes: Array[Byte]
  ): Either[String, Unit] =
    for {
      _ <- Either.cond(
        programVKey.length == 32,
        (),
        s"programVKey must be 32 bytes, got ${programVKey.length}"
      )
      _ <- Either.cond(
        proofBytes.length == ExpectedProofLength,
        (),
        s"proofBytes must be $ExpectedProofLength bytes, got ${proofBytes.length}"
      )
      _ <- Either.cond(
        selectorMatches(proofBytes),
        (),
        "wrong verifier selector"
      )
      // abi.decode(proofBytes[4:], (uint256, uint256, uint256, uint256[8]))
      words = decodeWords(proofBytes, offset = 4, count = 11)
      exitCode = words(0)
      vkRoot = words(1)
      nonce = words(2)
      proof = words.slice(3, 11) // uint256[8], inline
      _ <- Either.cond(exitCode.signum == 0, (), "invalid exit code")
      _ <- Either.cond(vkRoot == VkRoot, (), "invalid vk root")
      programVKeyInt = new BigInteger(1, programVKey)
      publicValuesDigest = hashPublicValues(publicValues)
      inputs = Vector(programVKeyInt, publicValuesDigest, exitCode, vkRoot, nonce)
      result <- Groth16Verifier.verifyProof(proof, inputs)
    } yield result

  private def selectorMatches(proofBytes: Array[Byte]): Boolean =
    proofBytes.length >= 4 &&
    proofBytes(0) == VerifierSelector(0) &&
    proofBytes(1) == VerifierSelector(1) &&
    proofBytes(2) == VerifierSelector(2) &&
    proofBytes(3) == VerifierSelector(3)

  /** Decode `count` consecutive big-endian uint256 words starting at `offset`. */
  private def decodeWords(bytes: Array[Byte], offset: Int, count: Int): Vector[BigInteger] =
    Vector.tabulate(count) { i =>
      val start = offset + i * 32
      val word = new Array[Byte](32)
      System.arraycopy(bytes, start, word, 0, 32)
      new BigInteger(1, word)
    }
}
