package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.Async
import cats.effect.std.AtomicCell
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleTree
import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.InMemorySparseMerkleTree
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash

/**
 * The single writable holder of the committed view -- a PRIVATE `AtomicCell[F, Committed[F, S]]`
 * behind a private constructor. The only way to obtain one wired into a data-application service is
 * `CommittedApp.makeL0` (correct-by-construction: `hashCalculatedState` / `setCalculatedState` /
 * routes all close over the same instance).
 *
 * [[setCommitted]] is the `setCalculatedState` path: it applies the view's delta to the live trie
 * (incremental `insert`/`remove`), REBUILDS the trie purely from the new value, and ASSERTS the two
 * roots are identical -- a divergence means the view's `delta`/`entries` disagree or the wiring is
 * broken, and it fails loudly ([[CommittedStateError.RootDivergence]]) rather than committing a root
 * other nodes cannot reproduce.
 */
final class CommittedState[F[_]: Async: JsonBinaryHasher, S] private (
  view: CommittedView[S],
  maxRecentDeltas: Int,
  cell: AtomicCell[F, Committed[F, S]]
) extends CommittedReader[F, S] {

  def committed: F[Committed[F, S]] = cell.get

  /** Advance the committed view to (`ordinal`, `nextState`). Returns the new committed snapshot. */
  def setCommitted(ordinal: SnapshotOrdinal, nextState: S): F[Committed[F, S]] =
    cell.evalModify { prev =>
      for {
        delta   <- view.delta(prev.state, nextState).pure[F]
        applied <- CommittedCommitment.applyDelta[F](prev.trie, delta)
        derived <- CommittedCommitment.buildTrie[F](view.entries(nextState))
        _ <- CommittedStateError
          .RootDivergence(ordinal, applied.rootNode.digest, derived.rootNode.digest)
          .raiseError[F, Unit]
          .whenA(applied.rootNode.digest != derived.rootNode.digest)
        mptRoot = applied.rootNode.digest
        catalogChanges = CommitCatalog.changesFor(ordinal, mptRoot)
        catalog <- prev.catalog
          .withChanges(catalogChanges.toList.map { case (k, h) => k -> CommitCatalog.rootValueBytes(h) }.toMap, Set.empty)
          .flatMap(CommittedState.requireInMemory[F](_))
        smtRoot <- catalog.root
        roots = CommittedRoots(mptRoot, smtRoot)
        stateDelta = StateDelta(ordinal, prev.roots, roots, delta.upserts, delta.removes)
        deltas = (prev.recentDeltas :+ stateDelta).takeRight(maxRecentDeltas)
        next = Committed(ordinal, nextState, applied, catalog, prev.catalogEntries ++ catalogChanges, roots, deltas)
      } yield (next, next)
    }
}

object CommittedState {

  /** Default ring-buffer depth for recent [[StateDelta]]s (older ordinals fall back to the snapshot route). */
  val DefaultMaxRecentDeltas: Int = 64

  /**
   * Assemble the genesis cell. Package-private: the public assembly path is `CommittedApp.makeL0`.
   */
  private[committed] def make[F[_]: Async: JsonBinaryHasher, S](
    genesisState: S,
    maxRecentDeltas: Int = DefaultMaxRecentDeltas
  )(implicit view: CommittedView[S]): F[CommittedState[F, S]] =
    for {
      trie <- CommittedCommitment.buildTrie[F](view.entries(genesisState))
      mptRoot = trie.rootNode.digest
      catalogChanges = CommitCatalog.changesFor(SnapshotOrdinal.MinValue, mptRoot)
      catalog <- InMemorySparseMerkleTree.make[F](
        catalogChanges.toList.map { case (k, h) => k -> CommitCatalog.rootValueBytes(h) }.toMap
      )
      smtRoot <- catalog.root
      genesis = Committed[F, S](
        SnapshotOrdinal.MinValue,
        genesisState,
        trie,
        catalog,
        SortedMap.from(catalogChanges),
        CommittedRoots(mptRoot, smtRoot),
        Vector.empty
      )
      cell <- AtomicCell[F].of(genesis)
    } yield new CommittedState[F, S](view, maxRecentDeltas, cell)

  private[committed] def requireInMemory[F[_]: Async](tree: SparseMerkleTree[F]): F[InMemorySparseMerkleTree[F]] =
    tree match {
      case t: InMemorySparseMerkleTree[F] => t.pure[F]
      case other                          => CommittedStateError.CatalogImplementationMismatch(other.getClass.getName).raiseError
    }
}

sealed abstract class CommittedStateError(message: String) extends RuntimeException(message)

object CommittedStateError {

  final case class RootDivergence(ordinal: SnapshotOrdinal, applied: Hash, derived: Hash)
      extends CommittedStateError(
        s"committed-state wiring bug at ordinal ${ordinal.value.value}: " +
        s"delta-applied MPT root ${applied.value} != value-derived MPT root ${derived.value} " +
        "(the CommittedView's delta/entries disagree)"
      )

  final case class CatalogImplementationMismatch(className: String)
      extends CommittedStateError(s"catalog withChanges returned an unexpected SparseMerkleTree implementation: $className")
}
