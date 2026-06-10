package io.constellationnetwork.metagraph_sdk.crypto.smt.node

import cats.MonadThrow
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.smt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

/**
 * Pure structural operations on [[SparseMerkleNode]] trees: upsert, delete, lookup, and authentication-path proof generation.
 * Every op preserves the canonical collapse invariant documented on [[SparseMerkleNode]], so the resulting [[SparseMerkleNode.digest]] is
 * a pure function of the live leaf set (order-independent).
 *
 * These are the building blocks the in-memory [[SparseMerkleTree]] / [[api.SparseMerkleProver]] are written against; keeping
 * them here (operating on the node directly) keeps the `Ref`-backed wrapper thin.
 */
object SparseMerkleNodeOps {

  /**
   * Upsert `(key -> value)` (with precomputed `position` and `valueDigest`) into the subtree rooted at `node`, which
   * sits at `depth`.
   */
  def insert[F[_]: MonadThrow: JsonBinaryHasher](
    node: SparseMerkleNode,
    key: Hex,
    position: Hash,
    valueDigest: Hash,
    value: Array[Byte],
    depth: Int
  ): F[SparseMerkleNode] =
    node match {
      case SparseMerkleNode.Empty =>
        SparseMerkleNode.Leaf.make[F](key, position, valueDigest, value).widen

      case leaf: SparseMerkleNode.Leaf =>
        if (leaf.position == position) SparseMerkleNode.Leaf.make[F](key, position, valueDigest, value).widen
        else mergeLeafWithNew[F](leaf, key, position, valueDigest, value, depth).widen

      case internal: SparseMerkleNode.Internal =>
        if (SparseMerkleHashing.bit(position, depth))
          insert[F](internal.right, key, position, valueDigest, value, depth + 1)
            .flatMap(r => SparseMerkleNode.Internal.make[F](internal.left, r).widen)
        else
          insert[F](internal.left, key, position, valueDigest, value, depth + 1)
            .flatMap(l => SparseMerkleNode.Internal.make[F](l, internal.right).widen)
    }

  /**
   * Delete `position` from the subtree rooted at `node` (at `depth`), re-collapsing stems so the result stays
   * canonical. No-op if absent.
   */
  def remove[F[_]: MonadThrow: JsonBinaryHasher](node: SparseMerkleNode, position: Hash, depth: Int): F[SparseMerkleNode] =
    node match {
      case SparseMerkleNode.Empty => (SparseMerkleNode.Empty: SparseMerkleNode).pure[F]

      case leaf: SparseMerkleNode.Leaf =>
        if (leaf.position == position) (SparseMerkleNode.Empty: SparseMerkleNode).pure[F]
        else (leaf: SparseMerkleNode).pure[F]

      case internal: SparseMerkleNode.Internal =>
        if (SparseMerkleHashing.bit(position, depth))
          remove[F](internal.right, position, depth + 1).flatMap(newR => collapse[F](internal.left, newR))
        else
          remove[F](internal.left, position, depth + 1).flatMap(newL => collapse[F](newL, internal.right))
    }

  /** The value bytes at `position`, or `None`. */
  def get(node: SparseMerkleNode, position: Hash, depth: Int): Option[Array[Byte]] =
    node match {
      case SparseMerkleNode.Empty => None
      case leaf: SparseMerkleNode.Leaf =>
        if (leaf.position == position) Some(leaf.value) else None
      case internal: SparseMerkleNode.Internal =>
        if (SparseMerkleHashing.bit(position, depth)) get(internal.right, position, depth + 1)
        else get(internal.left, position, depth + 1)
    }

  /**
   * Build the authentication path for `key` (already hashed to `position`) against the subtree rooted at `node`,
   * accumulating sibling digests top-down. Returns the [[SparseMerkleProof]] the [[api.SparseMerkleVerifier]] can fold. The in-memory
   * tree never produces a malformed proof, so this is total in the `Right` channel; the `Either` keeps the type uniform
   * with the algebra.
   */
  def prove[F[_]: MonadThrow](
    root: SparseMerkleNode,
    key: Hex,
    position: Hash
  ): F[Either[SparseMerkleProofError, SparseMerkleProof]] = {

    // Walk down accumulating siblings (root-first). Stops at Leaf / Empty.
    def loop(node: SparseMerkleNode, depth: Int, acc: List[SparseMerkleSibling]): Either[SparseMerkleProofError, SparseMerkleProof] =
      node match {
        case SparseMerkleNode.Empty =>
          Right(SparseMerkleProof.Absence(key, AbsenceWitness.Default, acc.reverse))

        case leaf: SparseMerkleNode.Leaf =>
          if (leaf.position == position)
            Right(SparseMerkleProof.Inclusion(key, leaf.value, leaf.valueDigest, acc.reverse))
          else
            Right(
              SparseMerkleProof.Absence(
                key,
                AbsenceWitness.OtherLeaf(leaf.key, leaf.valueDigest),
                acc.reverse
              )
            )

        case internal: SparseMerkleNode.Internal =>
          if (SparseMerkleHashing.bit(position, depth))
            loop(internal.right, depth + 1, SparseMerkleSibling(internal.left.digest) :: acc)
          else
            loop(internal.left, depth + 1, SparseMerkleSibling(internal.right.digest) :: acc)
      }

    loop(root, 0, Nil).pure[F]
  }

  /** Place two distinct-position leaves (the existing `leaf` and a new one) under a fresh internal stem starting at `depth`. */
  private def mergeLeafWithNew[F[_]: MonadThrow: JsonBinaryHasher](
    leaf: SparseMerkleNode.Leaf,
    newKey: Hex,
    newPos: Hash,
    newVd: Hash,
    newValue: Array[Byte],
    depth: Int
  ): F[SparseMerkleNode.Internal] = {
    val existingBit = SparseMerkleHashing.bit(leaf.position, depth)
    val newBit = SparseMerkleHashing.bit(newPos, depth)
    if (existingBit == newBit)
      mergeLeafWithNew[F](leaf, newKey, newPos, newVd, newValue, depth + 1).flatMap { child =>
        if (newBit) SparseMerkleNode.Internal.make[F](SparseMerkleNode.Empty, child)
        else SparseMerkleNode.Internal.make[F](child, SparseMerkleNode.Empty)
      }
    else
      SparseMerkleNode.Leaf.make[F](newKey, newPos, newVd, newValue).flatMap { newLeaf =>
        if (newBit) SparseMerkleNode.Internal.make[F](leaf, newLeaf) // new goes right (newBit true), existing left
        else SparseMerkleNode.Internal.make[F](newLeaf, leaf) // new goes left, existing right
      }
  }

  /**
   * Re-collapse an internal node after a child changed: if exactly one leaf remains in the subtree (one child a Leaf,
   * the other Empty), return that Leaf; otherwise keep an Internal. (A subtree that became fully Empty can only arise
   * from removing the lone leaf, which the Leaf case above already returns as Empty -- so here at least one side is
   * non-empty in the reachable cases; the `(Empty, Empty)` case is handled defensively.)
   */
  private def collapse[F[_]: MonadThrow: JsonBinaryHasher](left: SparseMerkleNode, right: SparseMerkleNode): F[SparseMerkleNode] =
    (left, right) match {
      case (SparseMerkleNode.Empty, r: SparseMerkleNode.Leaf) => (r: SparseMerkleNode).pure[F]
      case (l: SparseMerkleNode.Leaf, SparseMerkleNode.Empty) => (l: SparseMerkleNode).pure[F]
      case (SparseMerkleNode.Empty, SparseMerkleNode.Empty)   => (SparseMerkleNode.Empty: SparseMerkleNode).pure[F]
      case _                                                  => SparseMerkleNode.Internal.make[F](left, right).widen
    }
}
