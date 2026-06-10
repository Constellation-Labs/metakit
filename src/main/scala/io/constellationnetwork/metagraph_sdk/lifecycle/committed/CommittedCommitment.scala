package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.MonadThrow
import cats.effect.Sync
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.metagraph_sdk.crypto.mpt.impl.StatelessMerklePatriciaProducer
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{MerklePatriciaNode, MerklePatriciaTrie}
import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleRoot
import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.InMemorySparseMerkleTree
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

import io.circe.Json

/**
 * Pure derivations for the two-tier commitment -- everything here is a function of the entry set
 * (or of an existing trie plus a delta), with NO dependence on node-local history.
 *
 * ==The `hashCalculatedState` decision==
 * `hashCalculatedState(state)` must be a PURE function of the state VALUE: tessellation calls it on
 * freshly deserialized state during snapshot download, where the node has no local history yet, and
 * every node must agree on the hash. The live SMT catalog, however, accumulates HISTORICAL
 * `ordinal:<N>` entries, so its root is a function of history, not of the value.
 *
 * Resolution: [[deriveHash]] = `CommittedRoots.combine(mptRoot, canonicalCatalogRoot)` where the
 * CANONICAL catalog contains exactly one entry, `sha256("current:mpt") -> mptRoot`. That is pure in
 * the value, still binds tier 2's key scheme and hashing discipline into the consensus hash, and
 * loses nothing: each historical `ordinal:<N>` root was the `current:mpt` root at ordinal N and is
 * therefore anchored by snapshot N's own calculated-state proof. The LIVE catalog (with history) is
 * a node-local, verifiable index exposed via `/committed/root` and SMT proofs.
 */
object CommittedCommitment {

  /** The canonical empty trie: an empty Branch root (what removing every key also collapses to). */
  def emptyTrie[F[_]: MonadThrow: JsonBinaryHasher]: F[MerklePatriciaTrie] =
    MerklePatriciaNode.Branch[F](Map.empty).map(MerklePatriciaTrie(_))

  def isEmpty(trie: MerklePatriciaTrie): Boolean =
    trie.rootNode match {
      case MerklePatriciaNode.Branch(paths, _) => paths.isEmpty
      case _                                   => false
    }

  /** Build the state-dict MPT over the full entry set (full rebuild -- the pure derivation path). */
  def buildTrie[F[_]: MonadThrow: JsonBinaryHasher](entries: SortedMap[CommitKey, Json]): F[MerklePatriciaTrie] =
    if (entries.isEmpty) emptyTrie[F]
    else StatelessMerklePatriciaProducer[F].create(entries.toList.map { case (k, v) => k.toHex -> v }.toMap)

  /**
   * Apply a [[CommitDelta]] to an existing trie: removals first, then upserts. If the removals
   * empty the trie, the upserts rebuild from scratch (the canonical single-leaf/extension collapse
   * does not apply when growing out of the empty Branch root, so a fresh `create` keeps the result
   * byte-identical to the full rebuild).
   */
  def applyDelta[F[_]: MonadThrow: JsonBinaryHasher](trie: MerklePatriciaTrie, delta: CommitDelta): F[MerklePatriciaTrie] = {
    val producer = StatelessMerklePatriciaProducer[F]
    val removeKeys = delta.removes.toList.map(_.toHex)
    val upserts = delta.upserts.toList.map { case (k, v) => k.toHex -> v }.toMap

    for {
      afterRemoves <-
        if (removeKeys.isEmpty) trie.pure[F]
        else producer.remove(trie, removeKeys).rethrow
      result <-
        if (upserts.isEmpty) afterRemoves.pure[F]
        else if (isEmpty(afterRemoves)) producer.create(upserts)
        else producer.insert(afterRemoves, upserts).rethrow
    } yield result
  }

  /** Root of the CANONICAL (single-entry) catalog: `{ sha256("current:mpt") -> mptRoot }`. */
  def canonicalCatalogRoot[F[_]: Sync: JsonBinaryHasher](mptRoot: Hash): F[SparseMerkleRoot] =
    InMemorySparseMerkleTree
      .make[F](Map(CommitCatalog.currentMptKey -> CommitCatalog.rootValueBytes(mptRoot)))
      .flatMap(_.root)

  /** The canonical commitment pair, derived purely from the state value. */
  def deriveRoots[F[_]: Sync: JsonBinaryHasher, S: CommittedView](state: S): F[CommittedRoots] =
    for {
      trie    <- buildTrie[F](CommittedView[S].entries(state))
      smtRoot <- canonicalCatalogRoot[F](trie.rootNode.digest)
    } yield CommittedRoots(trie.rootNode.digest, smtRoot)

  /** The consensus-facing calculated-state hash: `combine(mptRoot, canonicalCatalogRoot)`. Pure in the value. */
  def deriveHash[F[_]: Sync: JsonBinaryHasher, S: CommittedView](state: S): F[Hash] =
    deriveRoots[F, S](state).map(_.combinedHash)
}
