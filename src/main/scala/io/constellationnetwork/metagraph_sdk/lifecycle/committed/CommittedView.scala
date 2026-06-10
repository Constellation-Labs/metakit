package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import scala.collection.immutable.{SortedMap, SortedSet}

import io.circe.Json

/**
 * Projects an application state `S` into the committed state dictionary: the canonical, namespaced
 * `CommitKey -> Json` entry set that the two-tier commitment (MPT state-dict + SMT root catalog) is
 * built over.
 *
 * `SortedMap` is REQUIRED -- entries must enumerate in a single canonical order so that diffs,
 * serialized deltas, and route responses are deterministic across nodes regardless of how `S` was
 * constructed in memory. (The MPT root itself is insertion-order independent, but determinism of the
 * surrounding artifacts -- `StateDelta`, `/committed/snapshot` -- rides on the sorted enumeration.)
 */
trait CommittedView[S] {

  /** The canonical entry set of `s`. Values should be canonical JSON projections of the state's leaves. */
  def entries(s: S): SortedMap[CommitKey, Json]

  /**
   * The change-set turning `entries(prev)` into `entries(next)`. The default is a full structural
   * diff; override when the application can produce the delta cheaper (e.g. from its own update
   * stream).
   */
  def delta(prev: S, next: S): CommitDelta = {
    val p = entries(prev)
    val n = entries(next)
    val upserts = n.filter { case (k, v) => !p.get(k).contains(v) }
    val removes = p.keySet.diff(n.keySet)
    CommitDelta(upserts, SortedSet.from(removes))
  }
}

object CommittedView {
  def apply[S](implicit ev: CommittedView[S]): CommittedView[S] = ev
}

/** A canonical change-set over the committed dictionary: removals applied first, then upserts (upsert wins). */
final case class CommitDelta(upserts: SortedMap[CommitKey, Json], removes: SortedSet[CommitKey]) {
  def isEmpty: Boolean = upserts.isEmpty && removes.isEmpty
}

object CommitDelta {
  def empty: CommitDelta = CommitDelta(SortedMap.empty, SortedSet.empty)
}
