package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.MonadThrow
import cats.syntax.bifunctor._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api._
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{
  MerklePatriciaBatchInclusionProof,
  MerklePatriciaInclusionProof,
  MerklePatriciaTrie
}
import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.InMemorySparseMerkleTree
import io.constellationnetwork.metagraph_sdk.crypto.smt.{SparseMerkleProof, SparseMerkleProofError}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

/**
 * One atomically-consistent committed snapshot: the state value, both tries, their roots, the live
 * catalog entries, and the recent-delta ring buffer -- everything a reader needs, derived from ONE
 * read of the `CommittedState` cell. All proof methods on this value attest against the SAME roots
 * by construction.
 *
 * Construction is restricted to the `committed` package (`CommittedState` / `CommittedReplica`):
 * the fields are mutually consistent only because the producing code asserted so.
 */
final case class Committed[F[_], S] private[committed] (
  ordinal: SnapshotOrdinal,
  state: S,
  trie: MerklePatriciaTrie,
  catalog: InMemorySparseMerkleTree[F],
  catalogEntries: SortedMap[Hex, Hash],
  roots: CommittedRoots,
  recentDeltas: Vector[StateDelta]
) {

  /**
   * Inclusion proof for one key against [[CommittedRoots.mptRoot]]. An absent key is a uniform
   * `PathNotFound`: the underlying prover reports a path that diverges mid-edge as
   * `InvalidNodeType`, but on this trie (built and asserted by the committed layer itself) that can
   * only mean the key is not present.
   */
  def proveKey(
    key: CommitKey
  )(implicit F: MonadThrow[F], H: JsonBinaryHasher[F]): F[Either[MerklePatriciaProofError, MerklePatriciaInclusionProof]] =
    MerklePatriciaProver
      .make[F](trie)
      .attestPath(key.toHex)
      .map(_.leftMap {
        case InvalidNodeType(_) => PathNotFound(key.value)
        case other              => other
      })

  /** One batch proof covering all `keys` against [[CommittedRoots.mptRoot]]. */
  def proveKeys(
    keys: List[CommitKey]
  )(implicit F: MonadThrow[F], H: JsonBinaryHasher[F]): F[Either[MerklePatriciaProofError, MerklePatriciaBatchInclusionProof]] =
    MerklePatriciaBatchInclusionProver.make[F](trie).attestPaths(keys.map(_.toHex))

  /**
   * Prefix attestation for a namespace: a batch proof over EVERY key under `ns/`, suitable for
   * complete-slice verification (`MerklePatriciaBatchInclusionVerifier` natively, or the JLVM's
   * `mpt_prefix_verify` with `0x`-prefixed hex).
   */
  def attestNamespace(
    ns: CommitNamespace
  )(implicit F: MonadThrow[F], H: JsonBinaryHasher[F]): F[Either[MerklePatriciaProofError, MerklePatriciaBatchInclusionProof]] =
    MerklePatriciaPrefixProver.make[F](trie).attestPrefix(ns.prefixHex)

  /**
   * SMT proof for a catalog name (e.g. `current:mpt`, `ordinal:42`) against
   * [[CommittedRoots.smtRoot]] -- inclusion when committed, first-class ABSENCE otherwise.
   */
  def proveCatalog(name: String)(implicit F: MonadThrow[F]): F[Either[SparseMerkleProofError, SparseMerkleProof]] =
    catalog.prover.flatMap(_.prove(CommitCatalog.catalogKey(name)))

  /** The delta that produced `ordinal`, if still inside the ring buffer. */
  def deltaFor(ordinal: SnapshotOrdinal): Option[StateDelta] =
    recentDeltas.find(_.ordinal == ordinal)

  /** The full-view replication fallback (see [[CommittedSnapshot]]). */
  def snapshot(implicit view: CommittedView[S]): CommittedSnapshot =
    CommittedSnapshot(ordinal, roots, view.entries(state), catalogEntries)
}
