package io.constellationnetwork.metagraph_sdk.crypto.smt

import io.constellationnetwork.security.hash.Hash

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/**
 * Domain-separated, Circe-encoded hash pre-images for the two materialized node kinds of the binary sparse Merkle tree.
 *
 * Hashing is routed through `JsonBinaryHasher[F].computeDigest(commitment.asJson, prefix)` -- EXACTLY as
 * `MerklePatriciaCommitment` / `MerklePatriciaNode` do -- so all node hashing stays on the metakit canonical-bytes path
 * (`std/JsonBinaryHasher`, never a hand-rolled digest). The one-byte domain-separation prefixes
 * ([[SparseMerkleCommitment.LeafPrefix]] / [[SparseMerkleCommitment.InternalPrefix]]) keep a leaf pre-image from ever colliding with an
 * internal-node pre-image.
 *
 * The third logical node kind -- an empty/default subtree -- is NOT hashed; its digest is the fixed placeholder
 * `Hash.empty` ([[io.constellationnetwork.metagraph_sdk.crypto.smt.node.SparseMerkleNode.Empty]]). This is the Diem/JMT
 * empty-subtree convention.
 *
 *   - [[SparseMerkleCommitment.Leaf]] binds the FULL 256-bit leaf `position` (= the hashed key) together with the value digest.
 *     Binding the full position (not a depth-relative remainder) makes a leaf's digest independent of where it sits in
 *     the tree, which is what gives the structure its order-independence and lets a verifier recompute any leaf digest
 *     from `(position, valueDigest)` alone.
 *   - [[SparseMerkleCommitment.Internal]] binds the two child subtree digests in fixed `(left, right)` order.
 */
sealed trait SparseMerkleCommitment extends Product with Serializable

object SparseMerkleCommitment {

  /**
   * Domain-separation prefix prepended to a leaf pre-image. Distinct from [[InternalPrefix]] and from the MPT prefixes
   * (this is a separate primitive with its own namespace).
   */
  val LeafPrefix: Array[Byte] = Array(0: Byte)

  /** Domain-separation prefix prepended to an internal-node pre-image. */
  val InternalPrefix: Array[Byte] = Array(1: Byte)

  /** Leaf pre-image: the full 256-bit position (the hashed key) and the value digest. */
  final case class Leaf(position: Hash, valueDigest: Hash) extends SparseMerkleCommitment

  /** Internal-node pre-image: the two child subtree digests, fixed `(left, right)` order. */
  final case class Internal(left: Hash, right: Hash) extends SparseMerkleCommitment

  object Leaf {

    implicit val leafCommitEncoder: Encoder[Leaf] =
      Encoder.instance { c =>
        Json.obj(
          "position"    -> c.position.asJson,
          "valueDigest" -> c.valueDigest.asJson
        )
      }

    implicit val leafCommitDecoder: Decoder[Leaf] =
      Decoder.instance { hCursor =>
        for {
          position    <- hCursor.downField("position").as[Hash]
          valueDigest <- hCursor.downField("valueDigest").as[Hash]
        } yield Leaf(position, valueDigest)
      }
  }

  object Internal {

    implicit val internalCommitEncoder: Encoder[Internal] =
      Encoder.instance { c =>
        Json.obj(
          "left"  -> c.left.asJson,
          "right" -> c.right.asJson
        )
      }

    implicit val internalCommitDecoder: Decoder[Internal] =
      Decoder.instance { hCursor =>
        for {
          left  <- hCursor.downField("left").as[Hash]
          right <- hCursor.downField("right").as[Hash]
        } yield Internal(left, right)
      }
  }

  implicit val smtCommitEncoder: Encoder[SparseMerkleCommitment] = Encoder.instance {
    case c: Leaf =>
      Json.obj("type" -> Json.fromString("Leaf"), "contents" -> c.asJson)
    case c: Internal =>
      Json.obj("type" -> Json.fromString("Internal"), "contents" -> c.asJson)
  }

  implicit val smtCommitDecoder: Decoder[SparseMerkleCommitment] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "Leaf"     => cursor.downField("contents").as[Leaf]
      case "Internal" => cursor.downField("contents").as[Internal]
      case other      => Left(DecodingFailure(s"Unknown SparseMerkleCommitment type: $other", cursor.history))
    }
  }
}
