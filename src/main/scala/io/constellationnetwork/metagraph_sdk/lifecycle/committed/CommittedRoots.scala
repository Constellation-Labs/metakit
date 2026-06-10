package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleRoot
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/**
 * The two-tier commitment of a snapshot: the state-dict MPT root (tier 1) and the root-catalog SMT
 * root (tier 2).
 *
 * [[combinedHash]] is the single hash binding the pair: `sha256(rawBytes(mptRoot) ++
 * rawBytes(smtRoot))` -- 64 bytes of raw digest material, mpt first. This (computed over the
 * CANONICAL pair derived purely from the state value, see `CommittedCommitment.deriveHash`) is what
 * `hashCalculatedState` returns, so the on-chain calculated-state proof anchors both tiers and the
 * catalog key scheme in one hash.
 */
final case class CommittedRoots(mptRoot: Hash, smtRoot: SparseMerkleRoot) {
  def combinedHash: Hash = CommittedRoots.combine(mptRoot, smtRoot)
}

object CommittedRoots {

  /** `sha256(rawBytes(mptRoot) ++ rawBytes(smtRoot))`, both roots as their 32 raw digest bytes. */
  def combine(mptRoot: Hash, smtRoot: SparseMerkleRoot): Hash =
    Hash.fromBytes(Hex(mptRoot.value).toBytes ++ Hex(smtRoot.value.value).toBytes)

  implicit val encoder: Encoder[CommittedRoots] =
    (roots: CommittedRoots) =>
      Json.obj(
        "mptRoot" -> roots.mptRoot.asJson,
        "smtRoot" -> roots.smtRoot.asJson
      )

  implicit val decoder: Decoder[CommittedRoots] = (c: HCursor) =>
    for {
      mptRoot <- c.downField("mptRoot").as[Hash]
      smtRoot <- c.downField("smtRoot").as[SparseMerkleRoot]
    } yield CommittedRoots(mptRoot, smtRoot)
}
