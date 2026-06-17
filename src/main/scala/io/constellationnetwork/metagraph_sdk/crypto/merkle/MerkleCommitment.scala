package io.constellationnetwork.metagraph_sdk.crypto.merkle

import io.constellationnetwork.security.hash.Hash

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait MerkleCommitment

object MerkleCommitment {
  // Per-variant codecs are DERIVED (derevo magnolia): {"dataDigest":..} and
  // {"leftDigest":..,"rightDigest":..}, byte-identical to the prior hand-rolled Json.obj form
  // (guarded by MerkleCodecKatSuite). The ADT discriminator below ({type,contents}) stays
  // hand-rolled — circe's derived sealed-trait format differs and would change the hashed bytes.
  @derive(encoder, decoder)
  final case class Leaf(dataDigest: Hash) extends MerkleCommitment

  @derive(encoder, decoder)
  final case class Internal(leftDigest: Hash, rightDigest: Hash) extends MerkleCommitment

  implicit val merkleCommitmentEncoder: Encoder[MerkleCommitment] = Encoder.instance {
    case commitment: Leaf =>
      Json.obj(
        "type"     -> Json.fromString("Leaf"),
        "contents" -> commitment.asJson
      )

    case commitment: Internal =>
      Json.obj(
        "type"     -> Json.fromString("Internal"),
        "contents" -> commitment.asJson
      )
  }

  implicit val merkleCommitmentDecoder: Decoder[MerkleCommitment] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "Leaf"     => cursor.downField("contents").as[Leaf]
      case "Internal" => cursor.downField("contents").as[Internal]
      case other      => Left(DecodingFailure(s"Unknown type: $other", cursor.history))
    }
  }
}
