package io.constellationnetwork.metagraph_sdk.crypto.smt

import java.nio.charset.StandardCharsets

import cats.MonadThrow

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps

/**
 * The single, shared source of truth for SMT positions, node digests, and path bits -- so the prover, the verifier,
 * and the in-memory tree all compute byte-identical hashes.
 *
 * ==Hashing seam (the metakit adaptation)==
 * The tessellation original routed everything through the `Hasher[F]` typeclass (Blake2b/Brotli). metakit has no such
 * typeclass in the SMT/MPT layer; the `MerklePatriciaNode`/`MerklePatriciaCommitment` discipline is to hash CANONICAL
 * BYTES via `std/JsonBinaryHasher` (`computeDigest = Hash.fromBytes(prefix ++ canonicalBytes)`). This object mirrors
 * that EXACTLY:
 *   - NODE digests ([[leafDigest]] / [[internalDigest]]) go through `JsonBinaryHasher[F].computeDigest(commitment.asJson,
 *     prefix)`, identical to how `MerklePatriciaNode` computes its node digests.
 *   - POSITION and the VALUE digest hash raw bytes (a key has no circe `Encoder`, a value is `Array[Byte]`); they use
 *     `Hash.fromBytes` directly -- which is exactly the final step `JsonBinaryHasher` performs (`Hash.fromBytes(bytes)`),
 *     so the same SHA-256-based digest the MPT uses is in force across the whole primitive.
 *
 * The seam is intentionally narrow ([[position]] / [[leafDigest]] / [[internalDigest]] / [[empty]]). A SNARK-friendly
 * hash (Poseidon) can be slotted in later by swapping these four functions and the [[empty]] placeholder; nothing else
 * in the SMT touches a digest pre-image.
 */
object SparseMerkleHashing {

  /** Digest of an empty (default) subtree -- the all-zeros placeholder, never hashed (Diem/JMT convention). */
  val empty: Hash = Hash.empty

  /**
   * The 256-bit position (slot) of a key: the digest of the key bytes. Hashing the key uniformizes the slot
   * distribution. The `Hash` is a 64-char lowercase hex string (32 bytes); [[bit]] reads it big-endian, MSB-first.
   */
  def position[F[_]: MonadThrow](key: Hex): F[Hash] =
    MonadThrow[F].pure(Hash.fromBytes(key.value.getBytes(StandardCharsets.UTF_8)))

  /** Digest of `value` bytes, as committed in a leaf. Raw-bytes SHA-256, matching the MPT's `Hash.fromBytes` seam. */
  def valueDigest[F[_]: MonadThrow](value: Array[Byte]): F[Hash] =
    MonadThrow[F].pure(Hash.fromBytes(value))

  /** Digest of a leaf binding the FULL position and value digest. Depth-independent (the full position is in the pre-image). */
  def leafDigest[F[_]: MonadThrow: JsonBinaryHasher](position: Hash, valueDigest: Hash): F[Hash] =
    JsonBinaryHasher[F].computeDigest(SparseMerkleCommitment.Leaf(position, valueDigest).asJson, SparseMerkleCommitment.LeafPrefix)

  /** Digest of an internal node binding its two child subtree digests in fixed `(left, right)` order. */
  def internalDigest[F[_]: MonadThrow: JsonBinaryHasher](left: Hash, right: Hash): F[Hash] =
    JsonBinaryHasher[F].computeDigest(SparseMerkleCommitment.Internal(left, right).asJson, SparseMerkleCommitment.InternalPrefix)

  /**
   * Combine a child digest `cur` (on the path) with its `sibling`, given the path bit at this depth: bit `false` =>
   * path went LEFT (cur is the left child), bit `true` => path went RIGHT. This is the per-level step both the tree
   * builder and the verifier use.
   */
  def combine[F[_]: MonadThrow: JsonBinaryHasher](bit: Boolean, cur: Hash, sibling: Hash): F[Hash] =
    if (bit) internalDigest[F](sibling, cur)
    else internalDigest[F](cur, sibling)

  /** Total number of position bits (256 -- a SHA-256 hash). */
  val PositionBits: Int = 256

  /**
   * Bit `index` (0 = most-significant bit of the first byte) of a 32-byte position hash, read big-endian. Returns
   * `false` (LEFT) for indices past the 256-bit space (defensive; callers never index past [[PositionBits]]).
   */
  def bit(position: Hash, index: Int): Boolean = {
    val bytes = positionBytes(position)
    val byteIdx = index / 8
    if (byteIdx < 0 || byteIdx >= bytes.length) false
    else {
      val bitInByte = 7 - (index % 8) // MSB-first within the byte
      ((bytes(byteIdx) >> bitInByte) & 1) == 1
    }
  }

  /** Whether two positions agree on the first `prefixLen` bits (used to sanity-check an OtherLeaf shares the queried key's prefix). */
  def sharePrefix(a: Hash, b: Hash, prefixLen: Int): Boolean =
    (0 until prefixLen).forall(i => bit(a, i) == bit(b, i))

  private def positionBytes(position: Hash): Array[Byte] =
    // Hash.value is a hex string (64 chars for SHA-256). Decode to its 32 raw bytes; the hex string IS the canonical
    // position encoding.
    Hex(position.value).toBytes
}
