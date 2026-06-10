package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/**
 * The full committed view at one snapshot: the entire entry dictionary plus the live catalog
 * entries. This is the replication FALLBACK -- a replica that has missed more deltas than the
 * source's ring buffer retains rebuilds from a snapshot (verifying it reproduces `roots`) and then
 * resumes the delta stream.
 */
final case class CommittedSnapshot(
  ordinal: SnapshotOrdinal,
  roots: CommittedRoots,
  entries: SortedMap[CommitKey, Json],
  catalog: SortedMap[Hex, Hash]
)

object CommittedSnapshot {

  implicit val encoder: Encoder[CommittedSnapshot] =
    (s: CommittedSnapshot) =>
      Json.obj(
        "ordinal" -> s.ordinal.asJson,
        "roots"   -> s.roots.asJson,
        "entries" -> Encoder.encodeMap[CommitKey, Json].apply(s.entries),
        "catalog" -> Encoder.encodeMap[Hex, Hash].apply(s.catalog)
      )

  implicit val decoder: Decoder[CommittedSnapshot] = (c: HCursor) =>
    for {
      ordinal <- c.downField("ordinal").as[SnapshotOrdinal]
      roots   <- c.downField("roots").as[CommittedRoots]
      entries <- c.downField("entries").as[Map[CommitKey, Json]].map(SortedMap.from(_))
      catalog <- c.downField("catalog").as[Map[Hex, Hash]].map(SortedMap.from(_))
    } yield CommittedSnapshot(ordinal, roots, entries, catalog)
}
