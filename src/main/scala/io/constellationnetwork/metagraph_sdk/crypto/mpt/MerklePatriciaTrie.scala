package io.constellationnetwork.metagraph_sdk.crypto.mpt

import cats.MonadThrow

import scala.annotation.tailrec

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaProducer
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class MerklePatriciaTrie(rootNode: MerklePatriciaNode)

object MerklePatriciaTrie {

  implicit val merkleTreeEncoder: Encoder[MerklePatriciaTrie] =
    (tree: MerklePatriciaTrie) => Json.obj("rootNode" -> tree.rootNode.asJson)

  implicit val merkleTreeDecoder: Decoder[MerklePatriciaTrie] = (c: HCursor) =>
    c.downField("rootNode").as[MerklePatriciaNode].map(MerklePatriciaTrie(_))

  /**
   * Create a new MerklePatriciaTrie from a map of data using the stateless producer
   */
  def make[F[_]: JsonBinaryHasher: MonadThrow, A: Encoder](data: Map[Hex, A]): F[MerklePatriciaTrie] =
    MerklePatriciaProducer
      .stateless[F]
      .create(data)

  def collectLeafNodes(trie: MerklePatriciaTrie): List[MerklePatriciaNode.Leaf] = {

    @tailrec
    def traverse(nodes: List[MerklePatriciaNode], acc: List[MerklePatriciaNode.Leaf]): List[MerklePatriciaNode.Leaf] =
      nodes match {
        case Nil                                               => acc
        case (head: MerklePatriciaNode.Leaf) :: tail           => traverse(tail, head :: acc)
        case MerklePatriciaNode.Branch(paths, _) :: tail       => traverse(paths.values.toList ++ tail, acc)
        case MerklePatriciaNode.Extension(_, child, _) :: tail => traverse(child :: tail, acc)
      }

    traverse(List(trie.rootNode), List()).reverse
  }
}
