package io.constellationnetwork.metagraph_sdk.crypto.merkle

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.implicits.{toFlatMapOps, toFunctorOps, toTraverseOps}
import cats.syntax.applicative._
import cats.syntax.applicativeError._

import scala.annotation.tailrec

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

final case class MerkleTree(
  rootNode: MerkleNode,
  leafDigestIndex: Map[Hash, Int]
) {

  /**
   * Get current leaves in the tree
   *
   * @return List of leaf nodes
   */
  def leaves: List[MerkleNode.Leaf] = {
    @tailrec
    def collectLeaves(nodes: List[MerkleNode], acc: List[MerkleNode.Leaf]): List[MerkleNode.Leaf] =
      nodes match {
        case Nil                             => acc
        case (leaf: MerkleNode.Leaf) :: rest => collectLeaves(rest, acc :+ leaf)
        case (internal: MerkleNode.Internal) :: rest =>
          internal.right match {
            case Some(rightNode) => collectLeaves(internal.left :: rightNode :: rest, acc)
            case None            => collectLeaves(internal.left :: rest, acc)
          }
      }

    collectLeaves(List(rootNode), Nil)
  }
}

object MerkleTree {

  implicit def merkleTreeEncoder: Encoder[MerkleTree] = (tree: MerkleTree) =>
    Json.obj(
      "rootNode" -> tree.rootNode.asJson,
      "leafDigestIndex" -> tree.leafDigestIndex.toList
        .sortBy(_._2)
        .map {
          case (digest, index) =>
            Json.obj(
              "digest" -> digest.asJson,
              "index"  -> index.asJson
            )
        }
        .asJson
    )

  implicit def merkleTreeDecoder: Decoder[MerkleTree] = (c: HCursor) =>
    for {
      rootNode        <- c.downField("rootNode").as[MerkleNode]
      leafDigestIndex <- c.downField("leafDigestIndex").as[List[(Hash, Int)]].map(_.toMap)
    } yield MerkleTree(rootNode, leafDigestIndex)

  def create[F[_]: MonadThrow: JsonBinaryHasher, A: Encoder](data: List[A]): F[MerkleTree] =
    NonEmptyList.fromList(data) match {
      case None      => new RuntimeException("Input list must be non-empty").raiseError
      case Some(nel) => create(nel)
    }

  def create[F[_]: MonadThrow: JsonBinaryHasher, A: Encoder](
    data: NonEmptyList[A]
  ): F[MerkleTree] =
    for {
      leafNodes <- data.toList.traverse(el => MerkleNode.Leaf(el.asJson))
      tree      <- fromLeaves(leafNodes)
    } yield tree

  /**
   * Create a Merkle tree from already-created leaf nodes.
   * This is useful when rebuilding a tree from stored leaves.
   */
  def fromLeaves[F[_]: MonadThrow: JsonBinaryHasher](
    leafNodes: List[MerkleNode.Leaf]
  ): F[MerkleTree] = {
    def buildLevel(nodes: List[MerkleNode]): F[List[MerkleNode]] =
      nodes match {
        case Nil           => MonadThrow[F].raiseError(new RuntimeException("Cannot build from empty list"))
        case single :: Nil => single.pure[F].map(List(_))
        case _ =>
          nodes
            .grouped(2)
            .toList
            .traverse[F, MerkleNode] {
              case List(left, right) => MerkleNode.Internal(left, Some(right)).widen[MerkleNode]
              case List(single)      =>
                // Duplicate the single node to maintain balanced tree
                MerkleNode.Internal(single, Some(single)).widen[MerkleNode]
              case _ => MonadThrow[F].raiseError[MerkleNode](new RuntimeException("Unexpected grouping"))
            }
      }

    def buildTree(nodes: List[MerkleNode]): F[MerkleNode] =
      nodes match {
        case single :: Nil => single.pure[F]
        case _             => buildLevel(nodes).flatMap(buildTree)
      }

    if (leafNodes.isEmpty) {
      MonadThrow[F].raiseError(new RuntimeException("Input list must be non-empty"))
    } else {
      for {
        rootNode <- buildTree(leafNodes)
        leafDigestIndex = leafNodes.zipWithIndex.map { case (node, index) => (node.digest, index) }.toMap
      } yield MerkleTree(rootNode, leafDigestIndex)
    }
  }
}
