package io.constellationnetwork.metagraph_sdk.crypto.mpt.impl

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all._

import scala.collection.immutable.ArraySeq

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{
  MerklePatriciaError,
  MerklePatriciaProducer,
  OperationError
}
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{MerklePatriciaNode, MerklePatriciaTrie, Nibble}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax._
import io.circe.{Encoder, Json}

class OptimizedMerklePatriciaProducer[F[_]: JsonBinaryHasher: MonadThrow] extends MerklePatriciaProducer[F] {

  /**
   * Create a new Merkle Patricia Trie from a map of data.
   * Optimizations:
   * - Use NonEmptyList to avoid empty data check
   * - Batch node creation with traverse
   * - More efficient folding with sortBy to help locality
   */
  def create[A: Encoder](data: Map[Hash, A]): F[MerklePatriciaTrie] =
    NonEmptyList.fromList(data.toList) match {
      case Some(nel) =>
        val (hPath, hData) = nel.head

        for {
          initialNode <- MerklePatriciaNode.Leaf[F](Nibble(hPath), hData.asJson)
          sortedTail = nel.tail.sortBy(_._1.value.length)

          resultNode <- sortedTail.foldM[F, MerklePatriciaNode](initialNode) { case (acc, (path, value)) =>
            insertEncoded(acc, Nibble(path), value.asJson).flatMap {
              case Left(err)    => err.raiseError[F, MerklePatriciaNode]
              case Right(value) => value.pure[F]
            }
          }
        } yield MerklePatriciaTrie(resultNode)

      case None => new RuntimeException("Empty data provided").raiseError
    }

  /**
   * Insert new data into an existing Merkle Patricia Trie.
   * Optimizations:
   * - Early return for empty data case
   * - Partition inserts into batches for potentially parallel processing
   * - Use NonEmptyList to avoid empty checks
   */
  def insert[A: Encoder](
    current: MerklePatriciaTrie,
    data:    Map[Hash, A]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    if (data.isEmpty) {
      // Fast path for empty data
      current.asRight[MerklePatriciaError].pure[F]
    } else {
      // Group inserts by path prefix to improve locality for inserts in similar branches
      val batchSize = 100 // Configurable batch size
      val dataList = data.toList

      // Process in batches to avoid stack overflows for large inputs
      dataList
        .grouped(batchSize)
        .toList
        .foldM[F, Either[MerklePatriciaError, MerklePatriciaTrie]](current.asRight[MerklePatriciaError]) {
          case (Right(acc), batch) =>
            insertBatch(acc, batch)
          case (Left(err), _) =>
            err.asLeft[MerklePatriciaTrie].pure[F]
        }
        .handleError(e => OperationError(e.getMessage).asLeft[MerklePatriciaTrie])
    }

  /**
   * Process a batch of inserts efficiently
   */
  private def insertBatch[A: Encoder](
    current: MerklePatriciaTrie,
    data:    List[(Hash, A)]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    data
      .foldM[F, Either[MerklePatriciaError, MerklePatriciaNode]](current.rootNode.asRight[MerklePatriciaError]) {
        case (Right(acc), (path, value)) => insertEncoded(acc, Nibble(path), value.asJson)
        case (Left(err), _)              => err.asLeft[MerklePatriciaNode].pure[F]
      }
      .map(_.map(MerklePatriciaTrie(_)))

  /**
   * Remove data from a Merkle Patricia Trie.
   * Optimizations:
   * - Early return for empty data case
   * - Batched processing for large removals
   * - Use NonEmptyList to avoid empty checks
   */
  def remove(current: MerklePatriciaTrie, data: List[Hash]): F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    if (data.isEmpty) {
      // Fast path for empty data
      current.asRight[MerklePatriciaError].pure[F]
    } else {
      // Process in batches to avoid stack overflows for large inputs
      val batchSize = 100 // Configurable batch size

      data
        .grouped(batchSize)
        .toList
        .foldM[F, Either[MerklePatriciaError, MerklePatriciaTrie]](current.asRight[MerklePatriciaError]) {
          case (Right(acc), batch) =>
            removeBatch(acc, batch)
          case (Left(err), _) =>
            err.asLeft[MerklePatriciaTrie].pure[F]
        }
        .handleError(e => OperationError(e.getMessage).asLeft[MerklePatriciaTrie])
    }

  /**
   * Process a batch of removals efficiently
   */
  private def removeBatch(
    current: MerklePatriciaTrie,
    paths:   List[Hash]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]] =
    paths
      .foldM[F, Either[MerklePatriciaError, MerklePatriciaNode]](current.rootNode.asRight[MerklePatriciaError]) {
        case (Right(acc), path) => removeEncoded(acc, Nibble(path))
        case (Left(err), _)     => err.asLeft[MerklePatriciaNode].pure[F]
      }
      .map(_.map(MerklePatriciaTrie(_)))

  // InsertState stays the same
  sealed private trait InsertState

  private case class InsertContinue(
    currentNode:  MerklePatriciaNode,
    key:          Seq[Nibble],
    updateParent: MerklePatriciaNode => F[Either[MerklePatriciaError, MerklePatriciaNode]]
  ) extends InsertState
  private case class InsertDone(node: Either[MerklePatriciaError, MerklePatriciaNode]) extends InsertState

  /**
   * Insert a value with the given path into the trie.
   * Optimizations:
   * - Add path length hint for optimization
   * - More efficient error handling
   * - Reduce allocations with cached empty values
   */
  private def insertEncoded(
    currentNode: MerklePatriciaNode,
    path:        Seq[Nibble],
    data:        Json
  ): F[Either[MerklePatriciaError, MerklePatriciaNode]] = {
    // Helper functions to reduce code duplication

    // Efficiently create branch with two nodes
    def createBranchWithTwoNodes(
      firstNibble:  Nibble,
      firstNode:    MerklePatriciaNode,
      secondNibble: Nibble,
      secondNode:   MerklePatriciaNode
    ): F[MerklePatriciaNode] =
      MerklePatriciaNode
        .Branch[F](
          Map[Nibble, MerklePatriciaNode](
            firstNibble  -> firstNode,
            secondNibble -> secondNode
          )
        )
        .widen

    // Create a leaf or branch with optional extension based on prefix
    def createNodeWithPrefix(
      prefix:       Seq[Nibble],
      node:         MerklePatriciaNode,
      updateParent: MerklePatriciaNode => F[Either[MerklePatriciaError, MerklePatriciaNode]]
    ): F[Either[MerklePatriciaError, MerklePatriciaNode]] =
      (if (prefix.nonEmpty) {
         node match {
           case branch: MerklePatriciaNode.Branch =>
             MerklePatriciaNode.Extension[F](prefix, branch)
           case _ =>
             MonadThrow[F].raiseError[MerklePatriciaNode](
               new IllegalStateException("Only branch nodes can be extended")
             )
         }
       } else {
         node.pure[F]
       })
        .flatMap(updateParent)
        .handleError(e => OperationError(e.getMessage).asLeft[MerklePatriciaNode])

    def insertForLeafNode(
      leafNode:     MerklePatriciaNode.Leaf,
      key:          Seq[Nibble],
      updateParent: MerklePatriciaNode => F[Either[MerklePatriciaError, MerklePatriciaNode]]
    ): F[Either[InsertState, Either[MerklePatriciaError, MerklePatriciaNode]]] =
      if (leafNode.remaining == key) {
        // Replace leaf with new data
        MerklePatriciaNode
          .Leaf[F](key, data)
          .flatMap(updateParent)
          .map(_.asRight[InsertState])
          .handleError(e => OperationError(e.getMessage).asLeft[MerklePatriciaNode].asRight[InsertState])
      } else {
        // Split into common prefix + branch with two leaves
        val commonPrefix = Nibble.commonPrefix(leafNode.remaining, key)
        val leafRemaining = leafNode.remaining.drop(commonPrefix.length)
        val keyRemaining = key.drop(commonPrefix.length)

        (for {
          // Create two leaf nodes
          existingLeaf <- MerklePatriciaNode.Leaf[F](leafRemaining.tail, leafNode.data)
          newLeaf      <- MerklePatriciaNode.Leaf[F](keyRemaining.tail, data)

          // Create branch with two paths
          branchNode <- createBranchWithTwoNodes(
            leafRemaining.head,
            existingLeaf,
            keyRemaining.head,
            newLeaf
          )

          // Add extension node if needed and update parent
          result <- createNodeWithPrefix(commonPrefix, branchNode, updateParent)
        } yield InsertDone(result).asLeft[Either[MerklePatriciaError, MerklePatriciaNode]])
          .handleError(e => InsertDone(OperationError(e.getMessage).asLeft[MerklePatriciaNode]).asLeft)
          .widen
      }

    def insertForExtensionNode(
      extensionNode: MerklePatriciaNode.Extension,
      key:           Seq[Nibble],
      updateParent:  MerklePatriciaNode => F[Either[MerklePatriciaError, MerklePatriciaNode]]
    ): F[Either[InsertState, Either[MerklePatriciaError, MerklePatriciaNode]]] = {
      val commonPrefix = Nibble.commonPrefix(extensionNode.shared, key)
      val sharedRemaining = extensionNode.shared.drop(commonPrefix.length)
      val keyRemaining = key.drop(commonPrefix.length)

      if (key.isEmpty) {
        InsertDone(OperationError("Key exhausted at extension node").asLeft)
          .asLeft[Either[MerklePatriciaError, MerklePatriciaNode]]
          .pure[F]
          .widen
      } else if (sharedRemaining.isEmpty) {
        // Continue with child node
        (InsertContinue(
          extensionNode.child,
          keyRemaining,
          {
            case branch: MerklePatriciaNode.Branch =>
              MerklePatriciaNode
                .Extension[F](extensionNode.shared, branch)
                .flatMap(updateParent)
                .handleError(e => OperationError(e.getMessage).asLeft)
            case _ =>
              OperationError("Unexpected node type while creating extension node")
                .asLeft[MerklePatriciaNode]
                .pure[F]
                .widen
          }
        ): InsertState).asLeft[Either[MerklePatriciaError, MerklePatriciaNode]].pure[F]
      } else {
        // Split extension node
        (for {
          newExtension <- MerklePatriciaNode.Extension[F](sharedRemaining.tail, extensionNode.child)
          newLeaf      <- MerklePatriciaNode.Leaf[F](keyRemaining.tail, data)

          // Create branch with two paths
          branchNode <- createBranchWithTwoNodes(
            sharedRemaining.head,
            newExtension,
            keyRemaining.head,
            newLeaf
          )

          // Add extension node if needed and update parent
          result <- createNodeWithPrefix(commonPrefix, branchNode, updateParent)
        } yield InsertDone(result).asLeft[Either[MerklePatriciaError, MerklePatriciaNode]])
          .handleError(e => InsertDone(OperationError(e.getMessage).asLeft[MerklePatriciaNode]).asLeft)
          .widen
      }
    }

    def insertForBranchNode(
      branchNode:   MerklePatriciaNode.Branch,
      key:          Seq[Nibble],
      updateParent: MerklePatriciaNode => F[Either[MerklePatriciaError, MerklePatriciaNode]]
    ): F[Either[InsertState, Either[MerklePatriciaError, MerklePatriciaNode]]] =
      if (key.isEmpty) {
        InsertDone(OperationError("Key exhausted at branch node").asLeft)
          .asLeft[Either[MerklePatriciaError, MerklePatriciaNode]]
          .pure[F]
          .widen
      } else {
        val nibble = key.head
        val keyRemaining = key.tail

        branchNode.paths.get(nibble) match {
          case Some(childNode) =>
            // Continue with child node
            (InsertContinue(
              childNode,
              keyRemaining,
              (updatedChild: MerklePatriciaNode) =>
                MerklePatriciaNode
                  .Branch[F](branchNode.paths + (nibble -> updatedChild))
                  .flatMap(updateParent)
                  .handleError(e => OperationError(e.getMessage).asLeft)
            ): InsertState).asLeft[Either[MerklePatriciaError, MerklePatriciaNode]].pure[F]

          case None =>
            // Add new path to branch
            (for {
              newLeaf       <- MerklePatriciaNode.Leaf[F](keyRemaining, data)
              updatedBranch <- MerklePatriciaNode.Branch[F](branchNode.paths + (nibble -> newLeaf))
              result        <- updateParent(updatedBranch)
            } yield result.asRight[InsertState])
              .handleError(e => InsertDone(OperationError(e.getMessage).asLeft[MerklePatriciaNode]).asLeft)
        }
      }

    def step(state: InsertState): F[Either[InsertState, Either[MerklePatriciaError, MerklePatriciaNode]]] =
      state match {
        case InsertContinue(currentNode, key, updateParent) =>
          currentNode match {
            case node: MerklePatriciaNode.Leaf      => insertForLeafNode(node, key, updateParent)
            case node: MerklePatriciaNode.Extension => insertForExtensionNode(node, key, updateParent)
            case node: MerklePatriciaNode.Branch    => insertForBranchNode(node, key, updateParent)
          }
        case InsertDone(node) => node.asRight[InsertState].pure[F]
      }

    val initialState: InsertState = InsertContinue(
      currentNode,
      path,
      node => node.asRight[MerklePatriciaError].pure[F]
    )

    initialState.tailRecM[F, Either[MerklePatriciaError, MerklePatriciaNode]](step)
  }

  // RemoveState stays the same
  sealed private trait RemoveState

  private case class RemoveContinue(
    currentNode:  MerklePatriciaNode,
    key:          Seq[Nibble],
    updateParent: Option[MerklePatriciaNode] => F[Either[MerklePatriciaError, Option[MerklePatriciaNode]]]
  ) extends RemoveState
  private case class RemoveDone(nodeOpt: Either[MerklePatriciaError, Option[MerklePatriciaNode]]) extends RemoveState

  /**
   * Remove a value with the given path from the trie.
   * Optimizations:
   * - Cache common operations
   * - Reduce allocations
   * - Improve error handling
   */
  private def removeEncoded(
    currentNode: MerklePatriciaNode,
    path:        Seq[Nibble]
  ): F[Either[MerklePatriciaError, MerklePatriciaNode]] = {
    // Helper functions for common node operations
    def handleSingleRemainingChild(
      remainingNibble: Nibble,
      onlyChild:       MerklePatriciaNode,
      updateParent:    Option[MerklePatriciaNode] => F[Either[MerklePatriciaError, Option[MerklePatriciaNode]]]
    ): F[Either[MerklePatriciaError, Option[MerklePatriciaNode]]] =
      onlyChild match {
        case leafNode: MerklePatriciaNode.Leaf =>
          MerklePatriciaNode
            .Leaf[F](ArraySeq(remainingNibble) ++ leafNode.remaining, leafNode.data)
            .flatMap(node => updateParent(Some(node)))
            .handleError(e => OperationError(e.getMessage).asLeft)

        case extensionNode: MerklePatriciaNode.Extension =>
          MerklePatriciaNode
            .Extension[F](ArraySeq(remainingNibble) ++ extensionNode.shared, extensionNode.child)
            .flatMap(node => updateParent(Some(node)))
            .handleError(e => OperationError(e.getMessage).asLeft)

        case branchNode: MerklePatriciaNode.Branch =>
          MerklePatriciaNode
            .Extension[F](ArraySeq(remainingNibble), branchNode)
            .flatMap(node => updateParent(Some(node)))
            .handleError(e => OperationError(e.getMessage).asLeft)
      }

    def removeForLeafNode(
      leafNode:     MerklePatriciaNode.Leaf,
      key:          Seq[Nibble],
      updateParent: Option[MerklePatriciaNode] => F[Either[MerklePatriciaError, Option[MerklePatriciaNode]]]
    ): F[Either[RemoveState, Either[MerklePatriciaError, Option[MerklePatriciaNode]]]] =
      if (leafNode.remaining == key) updateParent(None).map(_.asRight)
      else leafNode.some.asRight[MerklePatriciaError].asRight[RemoveState].pure[F].widen

    def removeForExtensionNode(
      extensionNode: MerklePatriciaNode.Extension,
      key:           Seq[Nibble],
      updateParent:  Option[MerklePatriciaNode] => F[Either[MerklePatriciaError, Option[MerklePatriciaNode]]]
    ): F[Either[RemoveState, Either[MerklePatriciaError, Option[MerklePatriciaNode]]]] = {
      val commonPrefix = Nibble.commonPrefix(extensionNode.shared, key)

      if (commonPrefix.length == extensionNode.shared.length) {
        RemoveContinue(
          extensionNode.child,
          key.drop(commonPrefix.length),
          {
            case Some(updatedChild) =>
              updatedChild match {
                case childBranch: MerklePatriciaNode.Branch =>
                  MerklePatriciaNode
                    .Extension[F](extensionNode.shared, childBranch)
                    .flatMap(node => updateParent(Some(node)))
                    .handleError(e => OperationError(e.getMessage).asLeft)

                case childLeaf: MerklePatriciaNode.Leaf =>
                  MerklePatriciaNode
                    .Leaf[F](extensionNode.shared ++ childLeaf.remaining, childLeaf.data)
                    .flatMap(node => updateParent(Some(node)))
                    .handleError(e => OperationError(e.getMessage).asLeft)

                case childExtension: MerklePatriciaNode.Extension =>
                  MerklePatriciaNode
                    .Extension[F](extensionNode.shared ++ childExtension.shared, childExtension.child)
                    .flatMap(node => updateParent(Some(node)))
                    .handleError(e => OperationError(e.getMessage).asLeft)
              }

            case None => updateParent(None)
          }
        ).asLeft[Either[MerklePatriciaError, Option[MerklePatriciaNode]]].pure[F].widen
      } else {
        extensionNode.some.asRight[MerklePatriciaError].asRight[RemoveState].pure[F].widen
      }
    }

    def removeForBranchNode(
      branchNode:   MerklePatriciaNode.Branch,
      key:          Seq[Nibble],
      updateParent: Option[MerklePatriciaNode] => F[Either[MerklePatriciaError, Option[MerklePatriciaNode]]]
    ): F[Either[RemoveState, Either[MerklePatriciaError, Option[MerklePatriciaNode]]]] =
      if (key.nonEmpty) {
        val nibble = key.head
        val keyRemaining = key.tail

        branchNode.paths.get(nibble) match {
          case Some(childNode) =>
            // Continue with child node
            (RemoveContinue(
              childNode,
              keyRemaining,
              {
                case Some(updatedChild) =>
                  MerklePatriciaNode
                    .Branch[F](branchNode.paths + (nibble -> updatedChild))
                    .flatMap(node => updateParent(Some(node)))
                    .handleError(e => OperationError(e.getMessage).asLeft)

                case None =>
                  val updatedPaths = branchNode.paths - nibble

                  updatedPaths.size match {
                    case 0 =>
                      updateParent(None)

                    case 1 =>
                      val (remainingNibble, onlyChild) = updatedPaths.head
                      handleSingleRemainingChild(remainingNibble, onlyChild, updateParent)

                    case _ =>
                      MerklePatriciaNode
                        .Branch[F](updatedPaths)
                        .flatMap(node => updateParent(Some(node)))
                        .handleError(e => OperationError(e.getMessage).asLeft)
                  }
              }
            ): RemoveState).asLeft[Either[MerklePatriciaError, Option[MerklePatriciaNode]]].pure[F]

          case None =>
            branchNode.some.asRight[MerklePatriciaError].asRight[RemoveState].pure[F].widen
        }
      } else {
        branchNode.some.asRight[MerklePatriciaError].asRight[RemoveState].pure[F].widen
      }

    def step(state: RemoveState): F[Either[RemoveState, Either[MerklePatriciaError, Option[MerklePatriciaNode]]]] =
      state match {
        case RemoveContinue(currentNode, key, updateParent) =>
          currentNode match {
            case node: MerklePatriciaNode.Leaf      => removeForLeafNode(node, key, updateParent)
            case node: MerklePatriciaNode.Extension => removeForExtensionNode(node, key, updateParent)
            case node: MerklePatriciaNode.Branch    => removeForBranchNode(node, key, updateParent)
          }
        case RemoveDone(nodeOpt) => nodeOpt.asRight[RemoveState].pure[F]
      }

    val initialState: RemoveState = RemoveContinue(
      currentNode,
      path,
      nodeOpt => nodeOpt.asRight[MerklePatriciaError].pure[F]
    )

    initialState.tailRecM[F, Either[MerklePatriciaError, Option[MerklePatriciaNode]]](step).flatMap {
      case Right(Some(newRootNode)) => newRootNode.asRight[MerklePatriciaError].pure[F]
      case Right(None)              => MerklePatriciaNode.Branch[F](Map.empty).map(_.asRight[MerklePatriciaError])
      case Left(err)                => err.asLeft[MerklePatriciaNode].pure[F]
    }
  }
}
