package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.std.AtomicCell
import cats.effect.{Async, Ref}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleRoot
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash

/**
 * The single writable holder of the committed view -- a PRIVATE `AtomicCell[F, Committed[F, S]]`
 * behind a private constructor. The only way to obtain one wired into a data-application service is
 * `CommittedApp.makeL0` (correct-by-construction: `hashCalculatedState` / `setCalculatedState` /
 * `combine` / routes all close over the same instance).
 *
 * ==Transitions==
 * [[setCommitted]] is the `setCalculatedState` path:
 *
 *   - CONTIGUOUS (`ordinal == cell+1`, hydrated): applies the view's delta to the live trie,
 *     REBUILDS the trie purely from the new value and ASSERTS the two roots are identical (a
 *     divergence means the view's `delta`/`entries` disagree -- it fails loudly,
 *     [[CommittedStateError.RootDivergence]], rather than committing a root other nodes cannot
 *     reproduce), then advances the epoch catalog and recomposes the catalog root. If the caller
 *     supplies the consensus breadcrumb for the same ordinal, the locally derived roots must match
 *     it ([[CommittedStateError.BreadcrumbMismatch]]).
 *   - SEED (any ordinal jump, or an unhydrated cell): O(1) bootstrap. Requires the attested
 *     breadcrumb of exactly that ordinal (from the latest SIGNED snapshot's on-chain state);
 *     verifies the value reproduces the breadcrumb's `mptRoot` and adopts its `catalogRoot`
 *     sight-unseen -- consensus attested. The catalog itself starts [[CatalogView.SeededCatalog]];
 *     if the [[CatalogJournal]]'s persisted entries recompose to the attested root, the cell
 *     hydrates immediately (the restart path -- a persistent `levelDb` journal; an `inMemory`
 *     journal is empty after a process restart). Otherwise hydration arrives later via [[hydrate]].
 *
 * ==The work cache==
 * `combine` runs BEFORE `setCalculatedState` and must emit the NEXT breadcrumb without mutating
 * the cell; replay paths (e.g. tessellation's `DataApplicationTraverse`) even fold `combine` over
 * many ordinals with the cell untouched. [[advanceWork]] therefore derives transitions against a
 * bounded breadcrumb-indexed cache seeded from the cell, so consecutive `combine` calls chain off
 * each other's results deterministically.
 */
final class CommittedState[F[_]: Async: JsonBinaryHasher, S] private (
  view: CommittedView[S],
  val config: CommittedConfig,
  journal: CatalogJournal[F],
  cell: AtomicCell[F, Committed[F, S]],
  work: Ref[F, Vector[(CommittedBreadcrumb, EpochCatalog[F])]]
) extends CommittedReader[F, S] {

  def committed: F[Committed[F, S]] = cell.get

  /**
   * The state-dict trie for `target`, computed per [[CommittedConfig.incrementalTrie]]:
   * full rebuild from `view.entries` (default), or derived as
   * `applyDelta(prev.trie, view.delta(prev.state, target))` -- O(churn) trie work.
   *
   * The derivation is TOTAL: it does not assume `target` is contiguous with `prev`. The MPT is
   * canonical (structure is a function of the final key set alone -- `MerklePatriciaDeterminismSuite`),
   * so applying the structural diff from ANY base state yields the same trie a full rebuild would;
   * a non-contiguous `target` (bootstrap/replay) just makes the diff large, never wrong.
   * Read-only with respect to the cell -- `prev.trie` is persistent and never mutated.
   */
  private def trieFor(prev: Committed[F, S], target: S): F[MerklePatriciaTrie] =
    if (config.incrementalTrie) CommittedCommitment.applyDelta[F](prev.trie, view.delta(prev.state, target))
    else CommittedCommitment.buildTrie[F](view.entries(target))

  // ---------------------------------------------------------------------------------------------
  // combine-side: breadcrumb validation + next-breadcrumb derivation (cell is NOT mutated)
  // ---------------------------------------------------------------------------------------------

  /**
   * Validate an incoming on-chain breadcrumb and derive the transition to the next ordinal for
   * state `nextState`. The parent breadcrumb must be RESOLVABLE -- the committed cell, a
   * recent [[advanceWork]] result, or (restart) the journal -- and where it overlaps the cell it
   * must MATCH ([[CommittedStateError.BreadcrumbMismatch]]): this is the follower-side rejection
   * of forged breadcrumbs.
   *
   * The mptRoot is computed via [[trieFor]] (full rebuild by default, incremental when
   * [[CommittedConfig.incrementalTrie]]). Note the base for the incremental derivation is always
   * the CELL's trie, not the (possibly work-cached, non-contiguous) `parent` -- correct in all
   * cases because the MPT is canonical in the final entry set (see [[trieFor]]).
   */
  def advanceWork(parent: CommittedBreadcrumb, nextState: S): F[CommittedBreadcrumb] =
    for {
      prev <- cell.get
      _ <- CommittedStateError
        .BreadcrumbMismatch(parent, prev.breadcrumb.some)
        .raiseError[F, Unit]
        .whenA(parent.ordinal == prev.ordinal && parent.roots != prev.roots)
      parentCatalog <- resolveCatalog(parent).flatMap {
        case Some(c) => c.pure[F]
        case None    => CommittedStateError.BreadcrumbUnresolvable(parent).raiseError[F, EpochCatalog[F]]
      }
      nextTrie <- trieFor(prev, nextState)
      mptRoot = nextTrie.rootNode.digest
      advanced <- parentCatalog.advance(parent.ordinal.value.value, parent.roots.mptRoot)
      (nextCatalog, _) = advanced
      composed <- nextCatalog.compose(mptRoot)
      next = CommittedBreadcrumb(
        SnapshotOrdinal.unsafeApply(parent.ordinal.value.value + 1),
        CommittedRoots(mptRoot, composed._2)
      )
      _ <- work.update(w =>
        ((parent, parentCatalog) +: (next, nextCatalog) +: w.filterNot(e => e._1 == parent || e._1 == next))
          .take(CommittedState.WorkCacheDepth)
      )
    } yield next

  /**
   * The catalog behind a breadcrumb, if this node knows it: the cell (hydrated, matching roots), a
   * recent [[advanceWork]] derivation, or a journal whose recomposition reproduces the
   * breadcrumb's attested catalog root.
   */
  private def resolveCatalog(bc: CommittedBreadcrumb): F[Option[EpochCatalog[F]]] =
    for {
      prev <- cell.get
      fromCell = prev.catalog.live.collect {
        case l if prev.breadcrumb == bc => l.epochs
      }
      fromWork <- fromCell match {
        case s @ Some(_) => (s: Option[EpochCatalog[F]]).pure[F]
        case None        => work.get.map(_.collectFirst { case (b, c) if b == bc => c })
      }
      result <- fromWork match {
        case s @ Some(_) => (s: Option[EpochCatalog[F]]).pure[F]
        case None =>
          journalCatalogMatching(bc.roots).flatMap {
            case s @ Some(_) => (s: Option[EpochCatalog[F]]).pure[F]
            case None        => emptyCatalogMatching(bc.roots)
          }
      }
    } yield result

  /**
   * Rebuild from the journal and accept ONLY if it recomposes to the attested roots. Resolution is
   * by ROOTS -- the catalog root cryptographically commits the full ordinal history, so a matching
   * root uniquely determines the state -- NOT by the breadcrumb's claimed ordinal. Honest callers
   * always pass a consensus-signed, ordinal-bound breadcrumb (the latest snapshot's on-chain
   * state), so root-matching is sufficient. Possible future hardening: also reject a breadcrumb
   * whose claimed ordinal is inconsistent with the recomposed catalog's frontier (defence in depth
   * against a forged ordinal -- currently caught downstream when the derived breadcrumb diverges).
   */
  private def journalCatalogMatching(roots: CommittedRoots): F[Option[EpochCatalog[F]]] =
    journal.contents.flatMap {
      case (hot, level1) =>
        EpochCatalog
          .fromContents[F](config, CatalogContents(config.epochSize, hot, level1, SortedMap.empty))
          .flatMap {
            case Left(_) => none[EpochCatalog[F]].pure[F]
            case Right(catalog) =>
              catalog.compose(roots.mptRoot).map {
                case (_, composedRoot) => Option.when(composedRoot == roots.catalogRoot)(catalog)
              }
          }
    }

  /**
   * The genesis fallback. The genesis breadcrumb's catalog is the EMPTY epoch catalog (no ordinals
   * recorded yet); once the cell advances past genesis its catalog is no longer the live cell, and
   * the journal can never reproduce it either, because the genesis->1 transition records ordinal 0
   * into the journal -- so a journal rebuild yields a catalog that already contains ordinal 0, which
   * recomposes to a DIFFERENT root than the (empty-catalog) genesis root.
   *
   * tessellation re-runs `combine(parent = genesis)` while accepting the first incremental snapshot,
   * AFTER `setCalculatedState` may have advanced the cell; without this fallback that re-run raises
   * [[CommittedStateError.BreadcrumbUnresolvable]] and the metagraph stalls at ordinal 1. Rebuild the
   * empty catalog and accept it ONLY if it recomposes to the attested roots -- true only for genesis,
   * where the state-dict carries just the genesis mptRoot and no history, so this never matches a
   * non-genesis breadcrumb.
   */
  private def emptyCatalogMatching(roots: CommittedRoots): F[Option[EpochCatalog[F]]] =
    EpochCatalog.empty[F](config).flatMap { empty =>
      empty.compose(roots.mptRoot).map {
        case (_, composedRoot) => Option.when(composedRoot == roots.catalogRoot)(empty)
      }
    }

  // ---------------------------------------------------------------------------------------------
  // hash-side: the consensus hash for a state value (cell is NOT mutated)
  // ---------------------------------------------------------------------------------------------

  /**
   * `hashCalculatedState`: `sha256(mptRoot(state) || catalogRoot)`. The catalog root is sourced by
   * call ordering (see `CommittedCommitment` scaladoc):
   *
   *   1. BOOTSTRAP/DOWNLOAD -- `contextBreadcrumb` (the latest SIGNED snapshot's on-chain
   *      breadcrumb) is AHEAD of the cell: use its attested catalog root directly (O(1)).
   *   2. STEADY STATE -- the cell is the parent: derive the transition's catalog root.
   *   3. REPLAY -- fall back to the most recent [[advanceWork]] derivation whose mptRoot matches.
   */
  def hashFor(state: S, contextBreadcrumb: Option[CommittedBreadcrumb]): F[Hash] =
    for {
      prev <- cell.get
      trie <- trieFor(prev, state)
      mptRoot = trie.rootNode.digest
      catalogRoot <- contextBreadcrumb.filter(_.ordinal.value.value > prev.ordinal.value.value) match {
        case Some(b) => b.roots.catalogRoot.pure[F]
        case None =>
          prev.catalog.live match {
            case Some(l) =>
              l.epochs
                .advance(prev.ordinal.value.value, prev.roots.mptRoot)
                .flatMap { case (next, _) => next.compose(mptRoot) }
                .map(_._2)
            case None =>
              work.get
                .map(_.collectFirst { case (b, _) if b.roots.mptRoot == mptRoot => b.roots.catalogRoot })
                .flatMap {
                  case Some(root) => root.pure[F]
                  case None       => CommittedStateError.CatalogNotHydrated(prev.ordinal).raiseError[F, SparseMerkleRoot]
                }
          }
      }
    } yield CommittedRoots.combine(mptRoot, catalogRoot)

  // ---------------------------------------------------------------------------------------------
  // setCalculatedState-side: advancing / seeding the cell
  // ---------------------------------------------------------------------------------------------

  /**
   * Advance the committed view to (`ordinal`, `nextState`). `contextBreadcrumb` is the latest
   * SIGNED snapshot's on-chain breadcrumb, when available -- the transition cross-check and the
   * bootstrap seed. Returns the new committed snapshot.
   */
  def setCommitted(
    ordinal: SnapshotOrdinal,
    nextState: S,
    contextBreadcrumb: Option[CommittedBreadcrumb] = None
  ): F[Committed[F, S]] =
    cell.evalModify { prev =>
      val next: F[Committed[F, S]] =
        if (ordinal == prev.ordinal) recommit(prev, nextState)
        else if (ordinal.value.value == prev.ordinal.value.value + 1 && prev.isHydrated)
          transition(prev, ordinal, nextState, contextBreadcrumb)
        else seed(ordinal, nextState, contextBreadcrumb)
      next.map(c => (c, c))
    }

  /** Re-committing the SAME ordinal is legal only if it is byte-identical (genesis is set twice). */
  private def recommit(prev: Committed[F, S], state: S): F[Committed[F, S]] =
    CommittedCommitment.buildTrie[F](view.entries(state)).flatMap { trie =>
      if (trie.rootNode.digest == prev.roots.mptRoot) prev.pure[F]
      else
        CommittedStateError
          .CommitRewrite(prev.ordinal, prev.roots.mptRoot, trie.rootNode.digest)
          .raiseError[F, Committed[F, S]]
    }

  private def transition(
    prev: Committed[F, S],
    ordinal: SnapshotOrdinal,
    nextState: S,
    contextBreadcrumb: Option[CommittedBreadcrumb]
  ): F[Committed[F, S]] = {
    val epochs = prev.catalog.live.map(_.epochs).get // guarded by isHydrated
    for {
      delta   <- view.delta(prev.state, nextState).pure[F]
      applied <- CommittedCommitment.applyDelta[F](prev.trie, delta)
      // The from-scratch cross-check: catches an unfaithful custom `view.delta` at runtime by
      // paying a full O(state) rebuild every transition. Skipped in incremental mode -- there the
      // `applyDelta == buildTrie` property test is the (offline) guarantee; see
      // [[CommittedConfig.incrementalTrie]] for the full trust model.
      _ <-
        if (config.incrementalTrie) Async[F].unit
        else
          CommittedCommitment.buildTrie[F](view.entries(nextState)).flatMap { derived =>
            CommittedStateError
              .RootDivergence(ordinal, applied.rootNode.digest, derived.rootNode.digest)
              .raiseError[F, Unit]
              .whenA(applied.rootNode.digest != derived.rootNode.digest)
          }
      mptRoot = applied.rootNode.digest
      advanced <- epochs.advance(prev.ordinal.value.value, prev.roots.mptRoot)
      (nextEpochs, sealEvent) = advanced
      composed <- nextEpochs.compose(mptRoot)
      (top, catalogRoot) = composed
      roots = CommittedRoots(mptRoot, catalogRoot)
      _ <- contextBreadcrumb.filter(_.ordinal == ordinal).traverse_ { b =>
        CommittedStateError
          .BreadcrumbMismatch(b, CommittedBreadcrumb(ordinal, roots).some)
          .raiseError[F, Unit]
          .whenA(b.roots != roots)
      }
      _ <- journal.recordOrdinal(prev.ordinal.value.value, prev.roots.mptRoot) >>
      sealEvent.traverse_(journal.recordSeal)
      stateDelta = StateDelta(ordinal, prev.roots, roots, delta.upserts, delta.removes)
      deltas = (prev.recentDeltas :+ stateDelta).takeRight(config.maxRecentDeltas)
    } yield Committed(ordinal, nextState, applied, CatalogView.LiveCatalog(nextEpochs, top), roots, deltas)
  }

  /** O(1) bootstrap: adopt the attested breadcrumb; hydrate from the journal if it still matches. */
  private def seed(
    ordinal: SnapshotOrdinal,
    state: S,
    contextBreadcrumb: Option[CommittedBreadcrumb]
  ): F[Committed[F, S]] =
    contextBreadcrumb match {
      case Some(b) if b.ordinal == ordinal =>
        for {
          trie <- CommittedCommitment.buildTrie[F](view.entries(state))
          mptRoot = trie.rootNode.digest
          _ <- CommittedStateError
            .SeedStateMismatch(ordinal, b.roots.mptRoot, mptRoot)
            .raiseError[F, Unit]
            .whenA(mptRoot != b.roots.mptRoot)
          fromJournal <- journalCatalogMatching(b.roots)
          catalogView <- fromJournal match {
            case Some(epochs) => epochs.compose(mptRoot).map(c => CatalogView.LiveCatalog(epochs, c._1): CatalogView[F])
            case None         => (CatalogView.SeededCatalog[F](b.roots.catalogRoot): CatalogView[F]).pure[F]
          }
          _ <- work.set(Vector.empty)
        } yield Committed(ordinal, state, trie, catalogView, b.roots, Vector.empty)
      case other =>
        CommittedStateError.CannotSeed(ordinal, other.map(_.ordinal)).raiseError[F, Committed[F, S]]
    }

  // ---------------------------------------------------------------------------------------------
  // hydration
  // ---------------------------------------------------------------------------------------------

  /**
   * Install full catalog contents on a SEEDED cell. Trustless: the rebuilt rollup must recompose
   * to the breadcrumb-attested catalog root, so any peer (or operator) can supply the payload. A
   * hydrated cell is returned unchanged. On success the journal is rewritten to match.
   */
  def hydrate(contents: CatalogContents): F[Either[CommittedStateError, Committed[F, S]]] =
    cell.evalModify { prev =>
      prev.catalog match {
        case CatalogView.LiveCatalog(_, _) => (prev, prev.asRight[CommittedStateError]).pure[F]
        case CatalogView.SeededCatalog(attestedRoot) =>
          EpochCatalog.fromContents[F](config, contents).flatMap {
            case Left(err) => (prev, (err: CommittedStateError).asLeft[Committed[F, S]]).pure[F]
            case Right(epochs) =>
              epochs.compose(prev.roots.mptRoot).flatMap {
                case (top, composedRoot) =>
                  if (composedRoot != attestedRoot)
                    (
                      prev,
                      (CommittedStateError.HydrationRootMismatch(attestedRoot, composedRoot): CommittedStateError)
                        .asLeft[Committed[F, S]]
                    ).pure[F]
                  else {
                    val hydrated = prev.copy(catalog = CatalogView.LiveCatalog(epochs, top))
                    journal.reset(contents.hot, contents.level1).as((hydrated, hydrated.asRight[CommittedStateError]))
                  }
              }
          }
      }
    }
}

object CommittedState {

  /** Bounded depth of the combine-side work cache (breadcrumb -> derived catalog). */
  val WorkCacheDepth: Int = 16

  /**
   * Assemble the genesis cell. Package-private: the public assembly path is `CommittedApp.makeL0`.
   */
  private[committed] def make[F[_]: Async: JsonBinaryHasher, S](
    genesisState: S,
    journal: CatalogJournal[F],
    config: CommittedConfig = CommittedConfig.default
  )(implicit view: CommittedView[S]): F[CommittedState[F, S]] =
    for {
      trie   <- CommittedCommitment.buildTrie[F](view.entries(genesisState))
      epochs <- EpochCatalog.empty[F](config)
      mptRoot = trie.rootNode.digest
      composed <- epochs.compose(mptRoot)
      (top, catalogRoot) = composed
      genesis = Committed[F, S](
        SnapshotOrdinal.MinValue,
        genesisState,
        trie,
        CatalogView.LiveCatalog(epochs, top),
        CommittedRoots(mptRoot, catalogRoot),
        Vector.empty
      )
      cell <- AtomicCell[F].of(genesis)
      work <- Ref.of[F, Vector[(CommittedBreadcrumb, EpochCatalog[F])]](Vector.empty)
    } yield new CommittedState[F, S](view, config, journal, cell, work)
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

  final case class BreadcrumbMismatch(claimed: CommittedBreadcrumb, local: Option[CommittedBreadcrumb])
      extends CommittedStateError(
        s"on-chain breadcrumb for ordinal ${claimed.ordinal.value.value} does not match the locally derived commitment " +
        s"(claimed mpt ${claimed.roots.mptRoot.value}, catalog ${claimed.roots.catalogRoot.value.value}; " +
        s"local ${local.map(l => s"mpt ${l.roots.mptRoot.value}, catalog ${l.roots.catalogRoot.value.value}").getOrElse("<none>")}) " +
        "-- rejecting the proposal/transition"
      )

  final case class BreadcrumbUnresolvable(claimed: CommittedBreadcrumb)
      extends CommittedStateError(
        s"cannot resolve the catalog behind breadcrumb ordinal ${claimed.ordinal.value.value}: " +
        "not the committed cell, not a recent combine derivation, and the journal does not recompose to it " +
        "(unhydrated bootstrap? hydrate via /committed/hydrate before participating in consensus)"
      )

  final case class CatalogNotHydrated(ordinal: SnapshotOrdinal)
      extends CommittedStateError(
        s"catalog at ordinal ${ordinal.value.value} is breadcrumb-seeded but not hydrated; " +
        "transitions and catalog proofs require contents (POST /committed/hydrate)"
      )

  final case class CommitRewrite(ordinal: SnapshotOrdinal, committed: Hash, attempted: Hash)
      extends CommittedStateError(
        s"refusing to rewrite committed ordinal ${ordinal.value.value}: mpt ${committed.value} -> ${attempted.value}"
      )

  final case class SeedStateMismatch(ordinal: SnapshotOrdinal, attested: Hash, rebuilt: Hash)
      extends CommittedStateError(
        s"downloaded state at ordinal ${ordinal.value.value} does not reproduce the attested on-chain mptRoot " +
        s"(attested ${attested.value}, rebuilt ${rebuilt.value})"
      )

  final case class CannotSeed(ordinal: SnapshotOrdinal, breadcrumbOrdinal: Option[SnapshotOrdinal])
      extends CommittedStateError(
        s"cannot seed committed state at ordinal ${ordinal.value.value}: " +
        breadcrumbOrdinal.fold("no on-chain breadcrumb available from the node context")(b =>
          s"latest signed breadcrumb is for ordinal ${b.value.value}"
        )
      )

  final case class HydrationRootMismatch(attested: SparseMerkleRoot, composed: SparseMerkleRoot)
      extends CommittedStateError(
        s"hydration contents recompose to catalog root ${composed.value.value}, " +
        s"but the consensus-attested root is ${attested.value.value} -- rejecting"
      )

  final case class MalformedCatalogContents(reason: String) extends CommittedStateError(s"malformed catalog contents: $reason")
}
