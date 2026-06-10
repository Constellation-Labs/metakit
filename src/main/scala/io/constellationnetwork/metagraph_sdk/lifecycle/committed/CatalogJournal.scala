package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import java.nio.file.Path

import cats.MonadThrow
import cats.effect.{Async, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.metagraph_sdk.storage.Collection
import io.constellationnetwork.metagraph_sdk.storage.impl.{LevelDbCollection, RefMapCollection}
import io.constellationnetwork.security.hash.Hash

import io.circe.Json
import io.circe.syntax.EncoderOps

/**
 * Node-local, write-through persistence of the catalog's history entries -- the level-1 sealed
 * epoch roots (`epoch:<E>`) plus the current hot epoch (`ordinal:<N>`), pruned to the hot window
 * on each seal. Backed by metakit's existing LevelDB-backed [[Collection]] (or an in-memory map
 * for tests).
 *
 * The journal is the node's OWN data, in the same local trust domain as tessellation's persisted
 * calculated state: on restart, a cell seeded at ordinal N can hydrate from it immediately (the
 * rebuilt catalog must still compose to the breadcrumb's attested root, so a stale or corrupt
 * journal degrades to the unhydrated path rather than to a wrong root).
 *
 * Note the journal does NOT persist retained sealed epochs' contents -- those are a serving cache
 * ([[CommittedConfig.sealedEpochRetention]]); after a restart a node serves ancient proofs again
 * only for epochs it re-fetches. Verifiability is unaffected.
 */
final class CatalogJournal[F[_]: MonadThrow] private (store: Collection[F, String, Json]) {

  /** Record `ordinal:<N> -> mptRoot` (the hot-epoch insert of one transition). */
  def recordOrdinal(ordinal: Long, mptRoot: Hash): F[Unit] =
    store.put(CommitCatalog.ordinalName(ordinal), mptRoot.asJson)

  /** Record a seal: persist `epoch:<E> -> root` and prune the sealed ordinals from the hot window. */
  def recordSeal(event: EpochCatalog.SealEvent): F[Unit] =
    store.put(CommitCatalog.epochName(event.epoch), event.root.value.asJson) >>
      store.removeBatch(event.sealedOrdinals.map(CommitCatalog.ordinalName))

  /** Replace the journal wholesale (used when hydration installs a fresher catalog). */
  def reset(hot: SortedMap[Long, Hash], level1: SortedMap[Long, Hash]): F[Unit] =
    for {
      existing <- store.dump
      _        <- store.removeBatch(existing.map(_._1))
      _        <- store.putBatch(hot.toList.map { case (o, h) => CommitCatalog.ordinalName(o) -> h.asJson })
      _        <- store.putBatch(level1.toList.map { case (e, h) => CommitCatalog.epochName(e) -> h.asJson })
    } yield ()

  /** The persisted `(hot ordinal -> mptRoot, sealed epoch -> root)` maps. */
  def contents: F[(SortedMap[Long, Hash], SortedMap[Long, Hash])] =
    store.dump.flatMap { pairs =>
      pairs
        .traverse {
          case (name, json) =>
            json.as[Hash].liftTo[F].map(name -> _)
        }
        .map { decoded =>
          val hot = decoded.collect { case (CatalogJournal.OrdinalName(n), h) => n -> h }
          val level1 = decoded.collect { case (CatalogJournal.EpochName(e), h) => e -> h }
          (SortedMap.from(hot), SortedMap.from(level1))
        }
    }
}

object CatalogJournal {

  private object OrdinalName {
    def unapply(name: String): Option[Long] =
      Option.when(name.startsWith("ordinal:"))(name.stripPrefix("ordinal:")).flatMap(_.toLongOption)
  }

  private object EpochName {
    def unapply(name: String): Option[Long] =
      Option.when(name.startsWith("epoch:"))(name.stripPrefix("epoch:")).flatMap(_.toLongOption)
  }

  /** LevelDB-backed journal at `dbPath` (metakit's existing [[LevelDbCollection]]). */
  def levelDb[F[_]: Async](dbPath: Path): Resource[F, CatalogJournal[F]] =
    LevelDbCollection.make[F, String, Json](dbPath).map(new CatalogJournal[F](_))

  /** In-memory journal (tests / ephemeral nodes). */
  def inMemory[F[_]: Async]: F[CatalogJournal[F]] =
    RefMapCollection.make[F, String, Json].map(new CatalogJournal[F](_))
}
