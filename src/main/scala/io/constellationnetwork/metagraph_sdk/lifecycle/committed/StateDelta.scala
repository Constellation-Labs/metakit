package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.schema.SnapshotOrdinal

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/**
 * The replication unit: the canonical change-set that took the committed state from
 * `parentRoots` (the previous snapshot's commitment) to `roots` at `ordinal`.
 *
 * A replica applies `removes` then `upserts` to its own trie, recomputes both roots LOCALLY, and
 * accepts the delta only if they equal `roots` (see `CommittedReplica.applyDelta`) -- so a tampered
 * delta (modified value, dropped key, forged roots) is rejected without trusting the sender.
 */
final case class StateDelta(
  ordinal: SnapshotOrdinal,
  parentRoots: CommittedRoots,
  roots: CommittedRoots,
  upserts: SortedMap[CommitKey, Json],
  removes: SortedSet[CommitKey]
)

object StateDelta {

  implicit val encoder: Encoder[StateDelta] =
    (d: StateDelta) =>
      Json.obj(
        "ordinal"     -> d.ordinal.asJson,
        "parentRoots" -> d.parentRoots.asJson,
        "roots"       -> d.roots.asJson,
        "upserts"     -> Encoder.encodeMap[CommitKey, Json].apply(d.upserts),
        "removes"     -> d.removes.toList.asJson
      )

  implicit val decoder: Decoder[StateDelta] = (c: HCursor) =>
    for {
      ordinal     <- c.downField("ordinal").as[SnapshotOrdinal]
      parentRoots <- c.downField("parentRoots").as[CommittedRoots]
      roots       <- c.downField("roots").as[CommittedRoots]
      upserts     <- c.downField("upserts").as[Map[CommitKey, Json]].map(SortedMap.from(_))
      removes     <- c.downField("removes").as[List[CommitKey]].map(SortedSet.from(_))
    } yield StateDelta(ordinal, parentRoots, roots, upserts, removes)
}
