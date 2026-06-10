package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.Async
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.InMemorySparseMerkleTree
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

/**
 * A verifying follower of a committed state: seeded from a [[CommittedSnapshot]] (rebuilding both
 * tries and CHECKING they reproduce the snapshot's roots), then advanced by [[applyDelta]], which
 * recomputes both roots LOCALLY from the delta's change-set and rejects the delta unless they equal
 * the roots it claims. Nothing is trusted from the wire beyond what the local recomputation proves.
 *
 * This is the consumer-side half of the `/committed/delta/:ordinal` + `/committed/snapshot` routes;
 * the ottochain follow-up wires it to HTTP.
 */
final case class CommittedReplica[F[_]] private (
  ordinal: SnapshotOrdinal,
  trie: MerklePatriciaTrie,
  catalog: InMemorySparseMerkleTree[F],
  catalogEntries: SortedMap[Hex, Hash],
  roots: CommittedRoots
) {

  /**
   * Apply one [[StateDelta]]: requires `delta.parentRoots == roots`, recomputes the MPT and catalog
   * roots locally, and accepts only if both match `delta.roots`.
   */
  def applyDelta(delta: StateDelta)(implicit F: Async[F], H: JsonBinaryHasher[F]): F[Either[ReplicationError, CommittedReplica[F]]] =
    if (delta.parentRoots != roots)
      (ReplicationError.ParentRootsMismatch(delta.ordinal, roots, delta.parentRoots): ReplicationError)
        .asLeft[CommittedReplica[F]]
        .pure[F]
    else
      for {
        applied <- CommittedCommitment.applyDelta[F](trie, CommitDelta(delta.upserts, delta.removes))
        mptRoot = applied.rootNode.digest
        result <-
          if (mptRoot != delta.roots.mptRoot)
            (ReplicationError.MptRootMismatch(delta.ordinal, mptRoot, delta.roots.mptRoot): ReplicationError)
              .asLeft[CommittedReplica[F]]
              .pure[F]
          else {
            val catalogChanges = CommitCatalog.changesFor(delta.ordinal, mptRoot)
            for {
              nextCatalog <- catalog
                .withChanges(catalogChanges.toList.map { case (k, h) => k -> CommitCatalog.rootValueBytes(h) }.toMap, Set.empty)
                .flatMap(CommittedState.requireInMemory[F](_))
              smtRoot <- nextCatalog.root
            } yield
              if (smtRoot != delta.roots.smtRoot)
                (ReplicationError.SmtRootMismatch(delta.ordinal, smtRoot.value, delta.roots.smtRoot.value): ReplicationError)
                  .asLeft[CommittedReplica[F]]
              else
                CommittedReplica(delta.ordinal, applied, nextCatalog, catalogEntries ++ catalogChanges, delta.roots)
                  .asRight[ReplicationError]
          }
      } yield result
}

object CommittedReplica {

  /**
   * Seed a replica from a full snapshot (the ring-buffer-eviction fallback), verifying that the
   * rebuilt tries reproduce the snapshot's claimed roots.
   */
  def fromSnapshot[F[_]: Async: JsonBinaryHasher](
    snapshot: CommittedSnapshot
  ): F[Either[ReplicationError, CommittedReplica[F]]] =
    for {
      trie <- CommittedCommitment.buildTrie[F](snapshot.entries)
      catalog <- InMemorySparseMerkleTree.make[F](
        snapshot.catalog.toList.map { case (k, h) => k -> CommitCatalog.rootValueBytes(h) }.toMap
      )
      smtRoot <- catalog.root
      rebuilt = CommittedRoots(trie.rootNode.digest, smtRoot)
    } yield
      if (rebuilt != snapshot.roots)
        (ReplicationError.SnapshotRootsMismatch(snapshot.ordinal, rebuilt, snapshot.roots): ReplicationError)
          .asLeft[CommittedReplica[F]]
      else
        CommittedReplica(snapshot.ordinal, trie, catalog, snapshot.catalog, snapshot.roots).asRight[ReplicationError]
}

sealed abstract class ReplicationError(message: String) extends RuntimeException(message)

object ReplicationError {

  final case class ParentRootsMismatch(ordinal: SnapshotOrdinal, local: CommittedRoots, claimed: CommittedRoots)
      extends ReplicationError(
        s"delta for ordinal ${ordinal.value.value} does not chain from the replica's roots " +
        s"(local mpt ${local.mptRoot.value}, claimed parent mpt ${claimed.mptRoot.value})"
      )

  final case class MptRootMismatch(ordinal: SnapshotOrdinal, recomputed: Hash, claimed: Hash)
      extends ReplicationError(
        s"delta for ordinal ${ordinal.value.value} is tampered or inconsistent: " +
        s"locally recomputed MPT root ${recomputed.value} != claimed ${claimed.value}"
      )

  final case class SmtRootMismatch(ordinal: SnapshotOrdinal, recomputed: Hash, claimed: Hash)
      extends ReplicationError(
        s"delta for ordinal ${ordinal.value.value} is tampered or inconsistent: " +
        s"locally recomputed catalog root ${recomputed.value} != claimed ${claimed.value}"
      )

  final case class SnapshotRootsMismatch(ordinal: SnapshotOrdinal, rebuilt: CommittedRoots, claimed: CommittedRoots)
      extends ReplicationError(
        s"snapshot for ordinal ${ordinal.value.value} does not reproduce its claimed roots " +
        s"(rebuilt mpt ${rebuilt.mptRoot.value}, claimed mpt ${claimed.mptRoot.value})"
      )
}
