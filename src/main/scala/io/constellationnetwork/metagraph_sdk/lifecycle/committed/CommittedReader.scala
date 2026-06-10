package io.constellationnetwork.metagraph_sdk.lifecycle.committed

/**
 * Read-only view over a `CommittedState`'s cell. One call to [[committed]] is ONE atomic read; all
 * derived data (roots, proofs, deltas, snapshot) should be taken from that single [[Committed]]
 * value so they are mutually consistent.
 */
trait CommittedReader[F[_], S] {

  /** The current committed snapshot (a single atomic read of the cell). */
  def committed: F[Committed[F, S]]
}
