package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash

/**
 * A verifying follower of a committed state: seeded from a [[CommittedSnapshot]] (rebuilding the
 * trie AND the epoch catalog, checking they reproduce the snapshot's roots), then advanced by
 * [[applyDelta]], which recomputes both roots LOCALLY from the delta's change-set -- replaying the
 * same epoch-rollup transition the source performed (hot insert, boundary seal, recompose) -- and
 * rejects the delta unless they equal the roots it claims. Nothing is trusted from the wire beyond
 * what the local recomputation proves.
 *
 * This is the consumer-side half of the `/committed/delta/:ordinal` + `/committed/snapshot`
 * routes; the ottochain follow-up wires it to HTTP.
 */
final case class CommittedReplica[F[_]] private (
  ordinal: SnapshotOrdinal,
  trie: MerklePatriciaTrie,
  epochs: EpochCatalog[F],
  roots: CommittedRoots
) {

  /**
   * Apply one [[StateDelta]]: requires `delta.parentRoots == roots` (and contiguity), recomputes
   * the MPT and catalog roots locally, and accepts only if both match `delta.roots`.
   */
  def applyDelta(delta: StateDelta)(implicit F: Async[F], H: JsonBinaryHasher[F]): F[Either[ReplicationError, CommittedReplica[F]]] =
    if (delta.parentRoots != roots)
      (ReplicationError.ParentRootsMismatch(delta.ordinal, roots, delta.parentRoots): ReplicationError)
        .asLeft[CommittedReplica[F]]
        .pure[F]
    else if (delta.ordinal.value.value != ordinal.value.value + 1)
      (ReplicationError.NonContiguousDelta(ordinal, delta.ordinal): ReplicationError)
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
          else
            for {
              advanced <- epochs.advance(ordinal.value.value, roots.mptRoot)
              composed <- advanced._1.compose(mptRoot)
            } yield
              if (composed._2 != delta.roots.catalogRoot)
                (ReplicationError.CatalogRootMismatch(delta.ordinal, composed._2.value, delta.roots.catalogRoot.value): ReplicationError)
                  .asLeft[CommittedReplica[F]]
              else
                CommittedReplica(delta.ordinal, applied, advanced._1, delta.roots).asRight[ReplicationError]
      } yield result
}

object CommittedReplica {

  /**
   * Seed a replica from a full snapshot (the ring-buffer-eviction fallback), verifying that the
   * rebuilt trie and catalog reproduce the snapshot's claimed roots.
   */
  def fromSnapshot[F[_]: Async: JsonBinaryHasher](
    snapshot: CommittedSnapshot,
    config: CommittedConfig = CommittedConfig.default
  ): F[Either[ReplicationError, CommittedReplica[F]]] =
    for {
      trie    <- CommittedCommitment.buildTrie[F](snapshot.entries)
      catalog <- EpochCatalog.fromContents[F](config, snapshot.catalog)
      result <- catalog match {
        case Left(err) =>
          (ReplicationError.MalformedSnapshot(snapshot.ordinal, err.getMessage): ReplicationError)
            .asLeft[CommittedReplica[F]]
            .pure[F]
        case Right(epochs) =>
          epochs.compose(trie.rootNode.digest).map {
            case (_, catalogRoot) =>
              val rebuilt = CommittedRoots(trie.rootNode.digest, catalogRoot)
              if (rebuilt != snapshot.roots)
                (ReplicationError.SnapshotRootsMismatch(snapshot.ordinal, rebuilt, snapshot.roots): ReplicationError)
                  .asLeft[CommittedReplica[F]]
              else
                CommittedReplica(snapshot.ordinal, trie, epochs, snapshot.roots).asRight[ReplicationError]
          }
      }
    } yield result
}

sealed abstract class ReplicationError(message: String) extends RuntimeException(message)

object ReplicationError {

  final case class ParentRootsMismatch(ordinal: SnapshotOrdinal, local: CommittedRoots, claimed: CommittedRoots)
      extends ReplicationError(
        s"delta for ordinal ${ordinal.value.value} does not chain from the replica's roots " +
        s"(local mpt ${local.mptRoot.value}, claimed parent mpt ${claimed.mptRoot.value})"
      )

  final case class NonContiguousDelta(replicaOrdinal: SnapshotOrdinal, deltaOrdinal: SnapshotOrdinal)
      extends ReplicationError(
        s"delta for ordinal ${deltaOrdinal.value.value} is not contiguous with the replica at ${replicaOrdinal.value.value}"
      )

  final case class MptRootMismatch(ordinal: SnapshotOrdinal, recomputed: Hash, claimed: Hash)
      extends ReplicationError(
        s"delta for ordinal ${ordinal.value.value} is tampered or inconsistent: " +
        s"locally recomputed MPT root ${recomputed.value} != claimed ${claimed.value}"
      )

  final case class CatalogRootMismatch(ordinal: SnapshotOrdinal, recomputed: Hash, claimed: Hash)
      extends ReplicationError(
        s"delta for ordinal ${ordinal.value.value} is tampered or inconsistent: " +
        s"locally recomputed catalog root ${recomputed.value} != claimed ${claimed.value}"
      )

  final case class SnapshotRootsMismatch(ordinal: SnapshotOrdinal, rebuilt: CommittedRoots, claimed: CommittedRoots)
      extends ReplicationError(
        s"snapshot for ordinal ${ordinal.value.value} does not reproduce its claimed roots " +
        s"(rebuilt mpt ${rebuilt.mptRoot.value}, claimed mpt ${claimed.mptRoot.value})"
      )

  final case class MalformedSnapshot(ordinal: SnapshotOrdinal, reason: String)
      extends ReplicationError(s"snapshot for ordinal ${ordinal.value.value} carries malformed catalog contents: $reason")
}
