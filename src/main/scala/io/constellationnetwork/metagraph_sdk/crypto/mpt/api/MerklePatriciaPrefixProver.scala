package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import cats.MonadThrow
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hex.Hex

trait MerklePatriciaPrefixProver[F[_]] {

  /**
   * Generate a batch proof attesting every key in the trie whose path starts with the given
   * prefix.
   *
   * @param prefix The path prefix to enumerate and prove
   * @return A batch proof covering all matching paths, or an error if none match
   */
  def attestPrefix(prefix: Hex): F[Either[MerklePatriciaProofError, MerklePatriciaBatchInclusionProof]]
}

object MerklePatriciaPrefixProver {

  def apply[F[_]](implicit prover: MerklePatriciaPrefixProver[F]): MerklePatriciaPrefixProver[F] = prover

  /**
   * Create a prefix prover from a Merkle Patricia Trie.
   *
   * Walks the trie collecting every leaf whose full path starts with `prefix`, then builds a batch
   * inclusion proof over those paths.
   */
  def make[F[_]: MonadThrow: JsonBinaryHasher](
    trie: MerklePatriciaTrie
  ): MerklePatriciaPrefixProver[F] =
    new MerklePatriciaPrefixProver[F] {

      def attestPrefix(prefix: Hex): F[Either[MerklePatriciaProofError, MerklePatriciaBatchInclusionProof]] = {

        final case class CollectedLeaf(path: Hex, leaf: MerklePatriciaNode.Leaf)

        // Collect every leaf reachable below `node` (whose path so far is `currentPath`).
        def collectAllLeavesUnder(
          node: MerklePatriciaNode,
          currentPath: Seq[Nibble],
          acc: List[CollectedLeaf]
        ): F[List[CollectedLeaf]] =
          node match {
            case leaf: MerklePatriciaNode.Leaf =>
              val fullPath = currentPath ++ leaf.remaining
              (CollectedLeaf(Nibble.toHex(fullPath), leaf) :: acc).pure[F]

            case extension: MerklePatriciaNode.Extension =>
              collectAllLeavesUnder(extension.child, currentPath ++ extension.shared, acc)

            case branch: MerklePatriciaNode.Branch =>
              branch.paths.toList.foldLeftM(acc) {
                case (currentAcc, (nibble, child)) =>
                  collectAllLeavesUnder(child, currentPath :+ nibble, currentAcc)
              }
          }

        // Collect leaves whose full path starts with `targetPrefix`, descending only the relevant
        // sub-trie.
        def collectLeavesUnderPrefix(
          node: MerklePatriciaNode,
          currentPath: Seq[Nibble],
          targetPrefix: Seq[Nibble],
          acc: List[CollectedLeaf]
        ): F[List[CollectedLeaf]] =
          node match {
            case leaf: MerklePatriciaNode.Leaf =>
              val fullPath = currentPath ++ leaf.remaining
              if (fullPath.startsWith(targetPrefix)) (CollectedLeaf(Nibble.toHex(fullPath), leaf) :: acc).pure[F]
              else acc.pure[F]

            case extension: MerklePatriciaNode.Extension =>
              val extendedPath = currentPath ++ extension.shared
              if (targetPrefix.startsWith(extendedPath))
                // The prefix runs through this extension; keep matching the remainder below it.
                collectLeavesUnderPrefix(extension.child, extendedPath, targetPrefix, acc)
              else if (extendedPath.startsWith(targetPrefix))
                // The extension already extends past the prefix; everything below it matches.
                collectAllLeavesUnder(extension.child, extendedPath, acc)
              else acc.pure[F]

            case branch: MerklePatriciaNode.Branch =>
              if (targetPrefix.startsWith(currentPath)) {
                val prefixRemaining = targetPrefix.drop(currentPath.length)
                if (prefixRemaining.isEmpty)
                  // Prefix ends exactly at this branch; everything below matches.
                  branch.paths.toList.foldLeftM(acc) {
                    case (currentAcc, (nibble, child)) =>
                      collectAllLeavesUnder(child, currentPath :+ nibble, currentAcc)
                  }
                else
                  branch.paths.get(prefixRemaining.head) match {
                    case Some(child) =>
                      collectLeavesUnderPrefix(child, currentPath :+ prefixRemaining.head, targetPrefix, acc)
                    case None => acc.pure[F]
                  }
              } else if (currentPath.startsWith(targetPrefix))
                branch.paths.toList.foldLeftM(acc) {
                  case (currentAcc, (nibble, child)) =>
                    collectAllLeavesUnder(child, currentPath :+ nibble, currentAcc)
                }
              else acc.pure[F]
          }

        val prefixNibbles = Nibble(prefix)

        collectLeavesUnderPrefix(trie.rootNode, Seq.empty, prefixNibbles, List.empty).flatMap { leaves =>
          if (leaves.isEmpty)
            (PathNotFound(s"No paths found with prefix: ${prefix.value}"): MerklePatriciaProofError)
              .asLeft[MerklePatriciaBatchInclusionProof]
              .pure[F]
          else
            MerklePatriciaBatchInclusionProver.make[F](trie).attestPaths(leaves.reverse.map(_.path))
        }
          .handleError(e => ProofGenerationError(e.getMessage).asLeft[MerklePatriciaBatchInclusionProof])
      }
    }

  /**
   * Provides syntax extensions for more ergonomic prefix proof generation
   */
  object syntax {

    implicit class MerklePatriciaPrefixOps(private val prefix: Hex) extends AnyVal {

      def attestPrefixInclusion[F[_]](
        implicit P: MerklePatriciaPrefixProver[F]
      ): F[Either[MerklePatriciaProofError, MerklePatriciaBatchInclusionProof]] =
        P.attestPrefix(prefix)
    }
  }
}
