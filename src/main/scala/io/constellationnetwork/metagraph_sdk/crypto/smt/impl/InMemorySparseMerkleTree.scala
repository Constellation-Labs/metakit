package io.constellationnetwork.metagraph_sdk.crypto.smt.impl

import cats.effect.{Ref, Sync}
import cats.syntax.flatMap._
import cats.syntax.foldable._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.smt._
import io.constellationnetwork.metagraph_sdk.crypto.smt.api.SparseMerkleProver
import io.constellationnetwork.metagraph_sdk.crypto.smt.node.{SparseMerkleNode, SparseMerkleNodeOps}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hex.Hex

/**
 * In-memory reference implementation of [[SparseMerkleTree]] -- a binary sparse Merkle tree with the Diem/JMT
 * empty-subtree collapse.
 *
 * `Ref[F, SparseMerkleNode]`-backed, mirroring `InMemoryMerklePatriciaProducer`'s idiom; the mutators ([[insert]] / [[remove]] /
 * [[withChanges]]) return a NEW tree over a FRESH `Ref` seeded with the recomputed root node, while the unchanged
 * subtrees are shared structurally (the node ADT is immutable, so the new root reuses every untouched child). The
 * receiver is never mutated.
 *
 * Positions are `hash(key)` (uniform), so node count is proportional to live leaves and the [[root]] is
 * order-independent.
 */
final class InMemorySparseMerkleTree[F[_]: Sync: JsonBinaryHasher] private (rootRef: Ref[F, SparseMerkleNode]) extends SparseMerkleTree[F] {

  def get(key: Hex): F[Option[Array[Byte]]] =
    for {
      pos  <- SparseMerkleHashing.position[F](key)
      node <- rootRef.get
    } yield SparseMerkleNodeOps.get(node, pos, 0)

  def root: F[SparseMerkleRoot] =
    rootRef.get.map(node => SparseMerkleRoot(node.digest))

  def insert(key: Hex, value: Array[Byte]): F[SparseMerkleTree[F]] =
    for {
      node    <- rootRef.get
      pos     <- SparseMerkleHashing.position[F](key)
      vd      <- SparseMerkleHashing.valueDigest[F](value)
      updated <- SparseMerkleNodeOps.insert[F](node, key, pos, vd, value, 0)
      tree    <- InMemorySparseMerkleTree.fromNode[F](updated)
    } yield tree

  def remove(key: Hex): F[SparseMerkleTree[F]] =
    for {
      node    <- rootRef.get
      pos     <- SparseMerkleHashing.position[F](key)
      updated <- SparseMerkleNodeOps.remove[F](node, pos, 0)
      tree    <- InMemorySparseMerkleTree.fromNode[F](updated)
    } yield tree

  def withChanges(upserts: Map[Hex, Array[Byte]], removes: Set[Hex]): F[SparseMerkleTree[F]] =
    for {
      node <- rootRef.get
      // removals first (then upserts win). The canonical collapse invariant makes the result independent of the order
      // within each phase.
      afterRemoves <- removes.toList.foldLeftM(node) { (acc, key) =>
        SparseMerkleHashing.position[F](key).flatMap(pos => SparseMerkleNodeOps.remove[F](acc, pos, 0))
      }
      afterUpserts <- upserts.toList.foldLeftM(afterRemoves) {
        case (acc, (key, value)) =>
          for {
            pos  <- SparseMerkleHashing.position[F](key)
            vd   <- SparseMerkleHashing.valueDigest[F](value)
            next <- SparseMerkleNodeOps.insert[F](acc, key, pos, vd, value, 0)
          } yield next
      }
      tree <- InMemorySparseMerkleTree.fromNode[F](afterUpserts)
    } yield tree

  /** A [[SparseMerkleProver]] bound to THIS tree's current root node (snapshot at call time). */
  def prover: F[SparseMerkleProver[F]] =
    rootRef.get.map { node =>
      new SparseMerkleProver[F] {
        def prove(key: Hex): F[Either[SparseMerkleProofError, SparseMerkleProof]] =
          SparseMerkleHashing.position[F](key).flatMap(pos => SparseMerkleNodeOps.prove[F](node, key, pos))
      }
    }
}

object InMemorySparseMerkleTree {

  /** An empty tree. */
  def empty[F[_]: Sync: JsonBinaryHasher]: F[InMemorySparseMerkleTree[F]] =
    fromNode[F](SparseMerkleNode.Empty)

  /** A tree seeded with `initial` key->value bindings. Order-independent: any iteration order of `initial` yields the same root. */
  def make[F[_]: Sync: JsonBinaryHasher](initial: Map[Hex, Array[Byte]] = Map.empty): F[InMemorySparseMerkleTree[F]] =
    initial.toList
      .foldLeftM(SparseMerkleNode.Empty: SparseMerkleNode) {
        case (acc, (key, value)) =>
          for {
            pos  <- SparseMerkleHashing.position[F](key)
            vd   <- SparseMerkleHashing.valueDigest[F](value)
            next <- SparseMerkleNodeOps.insert[F](acc, key, pos, vd, value, 0)
          } yield next
      }
      .flatMap(fromNode[F])

  private def fromNode[F[_]: Sync: JsonBinaryHasher](node: SparseMerkleNode): F[InMemorySparseMerkleTree[F]] =
    Ref.of[F, SparseMerkleNode](node).map(new InMemorySparseMerkleTree[F](_))
}
