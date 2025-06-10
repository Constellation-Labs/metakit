package io.constellationnetwork.metagraph_sdk.crypto

import io.constellationnetwork.security.hash.Hash

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

trait Node {
  def digest: Hash
}

object Node {

  implicit def encodeNode(implicit digestEncoder: Encoder[Hash]): Encoder[Node] =
    Encoder.instance { node =>
      digestEncoder(node.digest).asJson
    }

  implicit def decodeNode(implicit digestDecoder: Decoder[Hash]): Decoder[Node] =
    Decoder.instance { hCursor =>
      hCursor.as[Hash].map { d =>
        new Node {
          override def digest: Hash = d
        }
      }
    }
}
