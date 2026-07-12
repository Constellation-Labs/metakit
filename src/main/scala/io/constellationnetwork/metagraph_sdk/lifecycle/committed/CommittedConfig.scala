package io.constellationnetwork.metagraph_sdk.lifecycle.committed

/**
 * Chain-wide and node-local configuration of the committed-state module.
 *
 *   - [[epochSize]] -- CONSENSUS-CRITICAL: the number of ordinals per catalog epoch (the hot epoch
 *     SMT is sealed into the level-1 SMT every `epochSize` ordinals). Every node of a metagraph
 *     must run the same value: it determines which epoch tree an `ordinal:<N>` entry lands in and
 *     therefore the catalog root itself.
 *   - [[sealedEpochRetention]] -- NODE-LOCAL serving policy: how many sealed epoch trees (their
 *     full `ordinal -> mptRoot` contents) a node keeps for SERVING ancient-ordinal proofs. The
 *     level-1 roots of pruned epochs are kept forever (32 bytes per epoch), so every proof ever
 *     issued stays VERIFIABLE against the committed catalog root -- retention bounds what a node
 *     can serve, never what the network can verify ("retention is serving, not trust").
 *   - [[maxRecentDeltas]] -- NODE-LOCAL: ring-buffer depth for recent [[StateDelta]]s (older
 *     ordinals fall back to the snapshot route).
 *   - [[incrementalTrie]] -- NODE-LOCAL performance mode. Default `false` = today's behavior,
 *     byte-for-byte: every path full-rebuilds the state-dict MPT from `view.entries`, and
 *     `transition` cross-checks the delta-applied trie against the from-scratch rebuild
 *     ([[CommittedStateError.RootDivergence]]). When `true`, the accept-path tries are DERIVED
 *     (`applyDelta(prev.trie, view.delta(prev.state, target))` -- O(churn) trie work instead of
 *     O(state)) and the transition's from-scratch cross-check is SKIPPED.
 *
 *     Trust model for `true`: correctness rests on `applyDelta`+`view.delta` reproducing the
 *     canonical trie -- guaranteed by the MPT's insertion-order independence (see
 *     `MerklePatriciaDeterminismSuite`) and pinned by the `applyDelta == buildTrie` property
 *     test over random state pairs (`CommittedCommitmentSuite`). Note that `CommittedReplica`
 *     shares the `applyDelta` code path, so it is NOT an independent from-scratch cross-check;
 *     the property test is the offline guarantee. Roots are identical in both modes -- this flag
 *     never changes consensus bytes, only how (and how expensively) they are computed. Enable
 *     after a soak with the default; a custom `CommittedView.delta` override MUST be
 *     structurally faithful before enabling (the divergence assert is what would have caught an
 *     unfaithful one at runtime).
 */
final case class CommittedConfig(
  epochSize: Int = CommittedConfig.DefaultEpochSize,
  sealedEpochRetention: Int = CommittedConfig.DefaultSealedEpochRetention,
  maxRecentDeltas: Int = CommittedConfig.DefaultMaxRecentDeltas,
  incrementalTrie: Boolean = CommittedConfig.DefaultIncrementalTrie
) {
  require(epochSize > 0, "epochSize must be positive")
  require(sealedEpochRetention >= 0, "sealedEpochRetention must be non-negative")
  require(maxRecentDeltas >= 0, "maxRecentDeltas must be non-negative")
}

object CommittedConfig {

  /** 2^16 ordinals per epoch: ~45 days of 1-minute snapshots per sealed tree. */
  val DefaultEpochSize: Int = 65536

  /** Keep the last 4 sealed epoch trees' contents for serving ancient proofs. */
  val DefaultSealedEpochRetention: Int = 4

  /** Default ring-buffer depth for recent deltas. */
  val DefaultMaxRecentDeltas: Int = 64

  /** Full-rebuild + divergence-assert mode by default; opt in to incremental derivation. */
  val DefaultIncrementalTrie: Boolean = false

  val default: CommittedConfig = CommittedConfig()
}
