package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.MonadThrow
import cats.effect.Sync
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api._
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{
  MerklePatriciaBatchInclusionProof,
  MerklePatriciaInclusionProof,
  MerklePatriciaTrie
}
import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.InMemorySparseMerkleTree
import io.constellationnetwork.metagraph_sdk.crypto.smt.{SparseMerkleProof, SparseMerkleRoot}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.schema.SnapshotOrdinal

/**
 * The node's view of the catalog inside one committed snapshot.
 *
 *   - [[LiveCatalog]]: full contents -- the epoch rollup plus the composed TOP tree. Can derive
 *     the next transition, serve proofs, and emit snapshots.
 *   - [[SeededCatalog]]: only the consensus-attested ROOT is known (O(1) bootstrap from the
 *     on-chain breadcrumb). Sufficient to verify `hashCalculatedState` and to serve state-dict
 *     (MPT) proofs; catalog proofs and further transitions require hydration
 *     (`CommittedState.hydrate` / `POST /committed/hydrate`).
 */
sealed trait CatalogView[F[_]] extends Product with Serializable {

  def live: Option[CatalogView.LiveCatalog[F]] = this match {
    case l: CatalogView.LiveCatalog[F] => l.some
    case _                             => none
  }
}

object CatalogView {
  final case class LiveCatalog[F[_]](epochs: EpochCatalog[F], top: InMemorySparseMerkleTree[F]) extends CatalogView[F]
  final case class SeededCatalog[F[_]](catalogRoot: SparseMerkleRoot) extends CatalogView[F]
}

/**
 * One atomically-consistent committed snapshot: the state value, the state-dict trie, the catalog
 * view, the roots, and the recent-delta ring buffer -- everything a reader needs, derived from ONE
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
  catalog: CatalogView[F],
  roots: CommittedRoots,
  recentDeltas: Vector[StateDelta]
) {

  /** The constant on-chain breadcrumb of this snapshot. */
  def breadcrumb: CommittedBreadcrumb = CommittedBreadcrumb(ordinal, roots)

  /** Whether the catalog contents are locally known (vs breadcrumb-seeded). */
  def isHydrated: Boolean = catalog.live.isDefined

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
   * SMT proof for a TOP catalog name (`current:mpt`, `epoch:hot`, `epoch:sealed`, or an extension
   * family) against [[CommittedRoots.catalogRoot]] -- inclusion when committed, first-class
   * ABSENCE otherwise. Requires a hydrated catalog.
   */
  def proveCatalog(
    name: String
  )(implicit F: MonadThrow[F]): F[Either[CommittedProofError, SparseMerkleProof]] =
    catalog.live match {
      case None => (CommittedProofError.CatalogNotHydrated: CommittedProofError).asLeft[SparseMerkleProof].pure[F]
      case Some(l) =>
        l.top.prover
          .flatMap(_.prove(CommitCatalog.catalogKey(name)))
          .map(_.leftMap[CommittedProofError](CommittedProofError.ProofUnavailable(_)))
    }

  /**
   * The catalog attestation of `ordinal` (hot inclusion, two-level sealed inclusion, or
   * non-membership at both levels) against [[CommittedRoots.catalogRoot]]. Requires a hydrated
   * catalog; sealed epochs beyond the retention window yield
   * [[CommittedProofError.EpochPruned]].
   */
  def proveOrdinal(
    target: SnapshotOrdinal
  )(implicit F: Sync[F]): F[Either[CommittedProofError, OrdinalCatalogProof]] =
    catalog.live match {
      case None    => (CommittedProofError.CatalogNotHydrated: CommittedProofError).asLeft[OrdinalCatalogProof].pure[F]
      case Some(l) => l.epochs.proveOrdinal(target.value.value, l.top)
    }

  /** The delta that produced `ordinal`, if still inside the ring buffer. */
  def deltaFor(ordinal: SnapshotOrdinal): Option[StateDelta] =
    recentDeltas.find(_.ordinal == ordinal)

  /** The catalog payload for hydration / replication (None until hydrated). */
  def catalogContents: Option[CatalogContents] =
    catalog.live.map(_.epochs.contents)

  /** The full-view replication fallback (see [[CommittedSnapshot]]); None until hydrated. */
  def snapshot(implicit view: CommittedView[S]): Option[CommittedSnapshot] =
    catalogContents.map(contents => CommittedSnapshot(ordinal, roots, view.entries(state), contents))
}
