package io.constellationnetwork.metagraph_sdk.crypto.merkle

import io.constellationnetwork.security.hash.Hash

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait MerkleCommitment

object MerkleCommitment {
  final case class Leaf(dataDigest: Hash) extends MerkleCommitment
  final case class Internal(leftDigest: Hash, rightDigest: Hash) extends MerkleCommitment

  object Leaf {

    implicit val leafCommitmentEncoder: Encoder[Leaf] = Encoder.instance { commitment =>
      Json.obj(
        "dataDigest" -> commitment.dataDigest.asJson
      )
    }

    implicit val leafCommitmentDecoder: Decoder[Leaf] =
      Decoder.instance { hCursor =>
        for {
          dataDigest <- hCursor.downField("dataDigest").as[Hash]
        } yield Leaf(dataDigest)
      }
  }

  object Internal {

    implicit val internalCommitmentEncoder: Encoder[Internal] = Encoder.instance { commitment =>
      Json.obj(
        "leftDigest"  -> commitment.leftDigest.asJson,
        "rightDigest" -> commitment.rightDigest.asJson
      )
    }

    implicit val internalCommitmentDecoder: Decoder[Internal] =
      Decoder.instance { hCursor =>
        for {
          leftDigest  <- hCursor.downField("leftDigest").as[Hash]
          rightDigest <- hCursor.downField("rightDigest").as[Hash]
        } yield Internal(leftDigest, rightDigest)
      }
  }

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
