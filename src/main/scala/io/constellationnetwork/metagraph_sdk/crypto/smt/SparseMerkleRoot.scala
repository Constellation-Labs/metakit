package io.constellationnetwork.metagraph_sdk.crypto.smt

import cats.Eq

import io.constellationnetwork.security.hash.Hash

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/**
 * Root commitment of a [[SparseMerkleTree]] -- the digest of the root node.
 *
 * Wraps [[Hash]] exactly as `MerklePatriciaTrie`'s root digest is a `Hash`, so the two proof families read alike. The
 * empty tree's root is [[SparseMerkleRoot.empty]] (`Hash.empty`, the all-zeros default-subtree placeholder).
 */
final case class SparseMerkleRoot(value: Hash)

object SparseMerkleRoot {

  /**
   * Root of the empty tree: the all-zeros default-subtree placeholder (`Hash.empty`). Matches the SMT convention that
   * a subtree with zero leaves has the default hash.
   */
  val empty: SparseMerkleRoot = SparseMerkleRoot(Hash.empty)

  implicit val eq: Eq[SparseMerkleRoot] = Eq.by(_.value)

  implicit val encoder: Encoder[SparseMerkleRoot] =
    (root: SparseMerkleRoot) => Json.obj("value" -> root.value.asJson)

  implicit val decoder: Decoder[SparseMerkleRoot] =
    (c: HCursor) => c.downField("value").as[Hash].map(SparseMerkleRoot(_))
}
