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
 */
final case class CommittedConfig(
  epochSize: Int = CommittedConfig.DefaultEpochSize,
  sealedEpochRetention: Int = CommittedConfig.DefaultSealedEpochRetention,
  maxRecentDeltas: Int = CommittedConfig.DefaultMaxRecentDeltas
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

  val default: CommittedConfig = CommittedConfig()
}
