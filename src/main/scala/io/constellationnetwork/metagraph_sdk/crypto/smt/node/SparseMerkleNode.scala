package io.constellationnetwork.metagraph_sdk.crypto.smt.node

import cats.MonadThrow
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.Node
import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleHashing
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

/**
 * In-memory node of the binary sparse Merkle tree (the reference impl's materialized structure). Each node caches its
 * digest, computed at construction (no invalidation) -- exactly like `MerklePatriciaNode`, and like it extends the
 * shared [[Node]] trait.
 *
 * Canonical, order-independent shape for a subtree holding leaf set `S` (all sharing the prefix down to this depth):
 *   - `|S| == 0` => [[SparseMerkleNode.Empty]] (digest = `Hash.empty`, never hashed),
 *   - `|S| == 1` => [[SparseMerkleNode.Leaf]] (digest binds the FULL position, so it is depth-independent),
 *   - `|S| >= 2` => [[SparseMerkleNode.Internal]] split by bit `depth` into `(left, right)`; either child MAY be `Empty` when all
 *     of `S` shares that bit (a "stem" of internal nodes with empty siblings until the leaves diverge -- this is the
 *     Diem/JMT empty-subtree collapse).
 *
 * Because the shape is a pure function of `S` (and positions are key-hashes, hence uniform), the root digest is
 * independent of the order keys were inserted/removed.
 */
sealed trait SparseMerkleNode extends Node {
  def digest: Hash
}

object SparseMerkleNode {

  /** A collapsed empty (default) subtree. */
  case object Empty extends SparseMerkleNode {
    val digest: Hash = SparseMerkleHashing.empty
  }

  /**
   * A single occupied position. The original `key` (pre-image of `position`) is retained so an [[SparseMerkleNode.Leaf]] proving
   * the ABSENCE of a DIFFERENT key can hand the verifier the genuine occupying key (which the verifier re-hashes to the
   * committed position). `value` is retained so the in-memory tree can answer `get` and the prover can emit the value
   * bytes.
   */
  final case class Leaf private (key: Hex, position: Hash, valueDigest: Hash, value: Array[Byte], digest: Hash) extends SparseMerkleNode

  /** An internal node split by one bit; `left`/`right` may be [[Empty]] (stem). */
  final case class Internal private (left: SparseMerkleNode, right: SparseMerkleNode, digest: Hash) extends SparseMerkleNode

  object Leaf {

    def make[F[_]: MonadThrow: JsonBinaryHasher](key: Hex, position: Hash, valueDigest: Hash, value: Array[Byte]): F[Leaf] =
      SparseMerkleHashing.leafDigest[F](position, valueDigest).map(d => new Leaf(key, position, valueDigest, value, d))
  }

  object Internal {

    def make[F[_]: MonadThrow: JsonBinaryHasher](left: SparseMerkleNode, right: SparseMerkleNode): F[Internal] =
      SparseMerkleHashing.internalDigest[F](left.digest, right.digest).map(d => new Internal(left, right, d))
  }
}
