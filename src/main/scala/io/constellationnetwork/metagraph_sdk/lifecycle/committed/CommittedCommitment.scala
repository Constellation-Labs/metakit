package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.MonadThrow
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.metagraph_sdk.crypto.mpt.impl.StatelessMerklePatriciaProducer
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{MerklePatriciaNode, MerklePatriciaTrie}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher

import io.circe.Json

/**
 * Pure derivations for tier 1 of the commitment -- the state-dict MPT is a function of the entry
 * set (or of an existing trie plus a delta) alone.
 *
 * ==The `hashCalculatedState` decision==
 * `hashCalculatedState = sha256(mptRoot || liveCatalogRoot)` where the live catalog commits the
 * FULL root history (epoch rollup, [[EpochCatalog]]). The catalog root is NOT derivable from the
 * state value -- it is history -- so the hash implementation sources it from one of two places,
 * matching tessellation's two call orderings:
 *
 *   - STEADY STATE (consensus accept/consume: the snapshot being hashed is NOT yet in the local
 *     snapshot storage, and the committed cell sits at its parent): derive the TRANSITION from the
 *     cell -- `catalogRoot_N = compose(advance(catalog_{N-1}, ordinal:<N-1> -> mptRoot_{N-1}),
 *     current:mpt -> mptRoot(state))`.
 *   - BOOTSTRAP/DOWNLOAD (the snapshot IS already stored -- tessellation prepends it before
 *     fetching calculated state -- and the cell is behind): read the CONSTANT on-chain BREADCRUMB
 *     from that signed snapshot and use its attested `catalogRoot` directly. O(1), no replay: the
 *     Ethereum-header trust model.
 *
 * See `CommittedApp` for the wiring and `docs/committed-namespaces.md` for the full soundness
 * argument over tessellation's call orderings.
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
}
