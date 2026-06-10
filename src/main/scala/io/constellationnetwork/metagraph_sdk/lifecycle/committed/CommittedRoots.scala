package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleRoot
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/**
 * The two-tier commitment of a snapshot: the state-dict MPT root (tier 1) and the LIVE catalog
 * root (tier 2 -- the full-history epoch rollup, see [[EpochCatalog]]).
 *
 * [[combinedHash]] is the single hash binding the pair: `sha256(rawBytes(mptRoot) ++
 * rawBytes(catalogRoot))` -- 64 bytes of raw digest material, mpt first. This is exactly what
 * `hashCalculatedState` returns, so the snapshot's on-chain calculated-state proof anchors the
 * current state AND its entire root history in one hash.
 */
final case class CommittedRoots(mptRoot: Hash, catalogRoot: SparseMerkleRoot) {
  def combinedHash: Hash = CommittedRoots.combine(mptRoot, catalogRoot)
}

object CommittedRoots {

  /** `sha256(rawBytes(mptRoot) ++ rawBytes(catalogRoot))`, both roots as their 32 raw digest bytes. */
  def combine(mptRoot: Hash, catalogRoot: SparseMerkleRoot): Hash =
    Hash.fromBytes(Hex(mptRoot.value).toBytes ++ Hex(catalogRoot.value.value).toBytes)

  implicit val encoder: Encoder[CommittedRoots] =
    (roots: CommittedRoots) =>
      Json.obj(
        "mptRoot"     -> roots.mptRoot.asJson,
        "catalogRoot" -> roots.catalogRoot.asJson
      )

  implicit val decoder: Decoder[CommittedRoots] = (c: HCursor) =>
    for {
      mptRoot     <- c.downField("mptRoot").as[Hash]
      catalogRoot <- c.downField("catalogRoot").as[SparseMerkleRoot]
    } yield CommittedRoots(mptRoot, catalogRoot)
}

/**
 * The CONSTANT-SIZE on-chain breadcrumb: the [[CommittedRoots]] pair committed at one ordinal.
 * Every snapshot's on-chain state carries exactly one of these (via [[CommittedOnChain]]) -- it
 * never accumulates. It is what lets a freshly syncing node obtain the (consensus-attested)
 * catalog root in O(1), without replaying history: the Ethereum-header model -- each per-step
 * transition was validated by the then-current validators, so the latest signed breadcrumb
 * transitively commits the whole history.
 */
final case class CommittedBreadcrumb(ordinal: SnapshotOrdinal, roots: CommittedRoots)

object CommittedBreadcrumb {

  implicit val encoder: Encoder[CommittedBreadcrumb] =
    (b: CommittedBreadcrumb) =>
      Json.obj(
        "ordinal" -> b.ordinal.asJson,
        "roots"   -> b.roots.asJson
      )

  implicit val decoder: Decoder[CommittedBreadcrumb] = (c: HCursor) =>
    for {
      ordinal <- c.downField("ordinal").as[SnapshotOrdinal]
      roots   <- c.downField("roots").as[CommittedRoots]
    } yield CommittedBreadcrumb(ordinal, roots)
}
