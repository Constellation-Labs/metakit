package io.constellationnetwork.metagraph_sdk.crypto.merkle

import cats.MonadThrow
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.crypto.Node
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

sealed trait MerkleNode extends Node

object MerkleNode {
  private[merkle] val LeafPrefix: Array[Byte] = Array(0: Byte)
  private[merkle] val InternalPrefix: Array[Byte] = Array(1: Byte)

  final case class Leaf private (data: Json, digest: Hash) extends MerkleNode
  final case class Internal private (left: MerkleNode, right: Option[MerkleNode], digest: Hash) extends MerkleNode

  object Leaf {

    def apply[F[_]: MonadThrow: JsonBinaryHasher](data: Json): F[Leaf] = for {
      dataDigest <- JsonBinaryHasher[F].computeDigest(data)
      commitment <- MerkleCommitment.Leaf(dataDigest).pure[F]
      nodeDigest <- JsonBinaryHasher[F].computeDigest(commitment.asJson, LeafPrefix)
    } yield Leaf(data, nodeDigest)

    implicit val leafNodeEncoder: Encoder[Leaf] = Encoder.instance { node =>
      Json.obj(
        "data"   -> node.data,
        "digest" -> node.digest.asJson
      )
    }

    implicit val leafNodeDecoder: Decoder[Leaf] =
      Decoder.instance { hCursor =>
        for {
          data   <- hCursor.downField("data").as[Json]
          digest <- hCursor.downField("digest").as[Hash]
        } yield Leaf(data, digest)
      }
  }

  object Internal {

    def apply[F[_]: MonadThrow: JsonBinaryHasher](
      left: MerkleNode,
      right: Option[MerkleNode]
    ): F[Internal] = for {
      leftDigest  <- left.digest.pure[F]
      rightDigest <- right.map(_.digest).getOrElse(left.digest).pure[F]
      commitment  <- MerkleCommitment.Internal(leftDigest, rightDigest).pure[F]
      nodeDigest  <- JsonBinaryHasher[F].computeDigest(commitment.asJson, InternalPrefix)
    } yield Internal(left, right, nodeDigest)

    implicit val encodeInternalNode: Encoder[Internal] = Encoder.instance { node =>
      Json.obj(
        "left"   -> node.left.asJson,
        "right"  -> node.right.asJson,
        "digest" -> node.digest.asJson
      )
    }

    implicit val decodeInternalNode: Decoder[Internal] =
      Decoder.instance { hCursor =>
        for {
          left   <- hCursor.downField("left").as[MerkleNode]
          right  <- hCursor.downField("right").as[Option[MerkleNode]]
          digest <- hCursor.downField("digest").as[Hash]
        } yield new Internal(left, right, digest)
      }
  }

  implicit val encodeMerkleNode: Encoder[MerkleNode] = Encoder.instance {
    case leaf: Leaf =>
      Json.obj(
        "type"     -> Json.fromString("Leaf"),
        "contents" -> leaf.asJson
      )
    case internal: Internal =>
      Json.obj(
        "type"     -> Json.fromString("Internal"),
        "contents" -> internal.asJson
      )
  }

  implicit val decodeMerkleNode: Decoder[MerkleNode] = Decoder.instance { cursor =>
    cursor.downField("type").as[String].flatMap {
      case "Leaf"     => cursor.downField("contents").as[Leaf]
      case "Internal" => cursor.downField("contents").as[Internal]
      case other      => Left(DecodingFailure(s"Unknown type: $other", cursor.history))
    }
  }
}
