package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal

import io.circe.Json
import weaver.SimpleIOSuite

/**
 * Replication contract: a verifying replica seeded from a snapshot follows the StateDelta stream
 * with byte-identical roots at every step (including across epoch-seal boundaries), rejects
 * tampered or out-of-order deltas, and falls back to the snapshot route once the source's ring
 * buffer has evicted what it missed.
 */
object CommittedReplicationSuite extends SimpleIOSuite {
  import ToyFixtures._

  private def ord(n: Long): SnapshotOrdinal = SnapshotOrdinal.unsafeApply(n)

  private val s0 = ToyState(Map("aaa" -> 1, "bbb" -> 2), Map("alpha" -> "x"))

  private val stream = List(
    ToyState(Map("aaa" -> 5, "bbb" -> 2), Map("alpha" -> "x")), // modify
    ToyState(Map("aaa" -> 5), Map("alpha" -> "x", "beta" -> "y")), // remove + add
    ToyState(Map("aaa" -> 5, "ccc" -> 7), Map("beta" -> "y")) // add + remove
  )

  private def snapshotOf(c: Committed[IO, ToyState]): CommittedSnapshot =
    c.snapshot.getOrElse(throw new RuntimeException("hydrated source expected"))

  test("a replica applying the delta stream matches the source's roots at every step") {
    for {
      source  <- mkCommitted(s0)
      genesis <- source.committed
      replica <- CommittedReplica.fromSnapshot[IO](snapshotOf(genesis)).flatMap(IO.fromEither(_))

      result <- stream.zipWithIndex.foldLeftM((replica, List.empty[Boolean])) {
        case ((rep, acc), (s, i)) =>
          for {
            c     <- source.setCommitted(ord(i.toLong + 1), s)
            delta <- IO.fromOption(c.deltaFor(ord(i.toLong + 1)))(new RuntimeException("delta missing"))
            next  <- rep.applyDelta(delta).flatMap(IO.fromEither(_))
          } yield (next, acc :+ (next.roots == c.roots && next.ordinal == c.ordinal))
      }
    } yield expect(result._2.forall(identity))
  }

  test("a replica follows across an epoch-seal boundary with identical roots") {
    val config = CommittedConfig(epochSize = 2, sealedEpochRetention = 2)
    val states = (1 to 6).toList.map(i => ToyState(Map("aaa" -> i), Map.empty))
    for {
      source  <- mkCommitted(s0, config)
      genesis <- source.committed
      replica <- CommittedReplica.fromSnapshot[IO](snapshotOf(genesis), config).flatMap(IO.fromEither(_))

      result <- states.zipWithIndex.foldLeftM((replica, List.empty[Boolean])) {
        case ((rep, acc), (s, i)) =>
          for {
            c     <- source.setCommitted(ord(i.toLong + 1), s)
            delta <- IO.fromOption(c.deltaFor(ord(i.toLong + 1)))(new RuntimeException("delta missing"))
            next  <- rep.applyDelta(delta).flatMap(IO.fromEither(_))
          } yield (next, acc :+ (next.roots == c.roots))
      }
      (finalReplica, checks) = result
    } yield
      expect.all(
        checks.forall(identity),
        finalReplica.epochs.level1Entries.keySet == Set(0L, 1L) // the replica sealed the same epochs
      )
  }

  test("a tampered delta is rejected (modified upsert, dropped remove, forged roots)") {
    for {
      source  <- mkCommitted(s0)
      genesis <- source.committed
      replica <- CommittedReplica.fromSnapshot[IO](snapshotOf(genesis)).flatMap(IO.fromEither(_))
      c1      <- source.setCommitted(ord(1), stream(1))
      delta   <- IO.fromOption(c1.deltaFor(ord(1)))(new RuntimeException("delta missing"))

      tamperedValue = delta.copy(upserts = delta.upserts.map { case (k, _) => k -> Json.fromString("evil") })
      tamperedRemoves = delta.copy(removes = delta.removes.take(0))
      forgedRoots = delta.copy(roots = delta.roots.copy(mptRoot = genesis.roots.mptRoot))

      r1 <- replica.applyDelta(tamperedValue)
      r2 <- replica.applyDelta(tamperedRemoves)
      r3 <- replica.applyDelta(forgedRoots)
      ok <- replica.applyDelta(delta)
    } yield
      expect.all(
        r1.left.exists(_.isInstanceOf[ReplicationError.MptRootMismatch]),
        r2.left.exists(_.isInstanceOf[ReplicationError.MptRootMismatch]),
        r3.left.exists(_.isInstanceOf[ReplicationError.MptRootMismatch]),
        ok.isRight
      )
  }

  test("a forged catalog root is rejected by the replica's local recomputation") {
    for {
      source  <- mkCommitted(s0)
      genesis <- source.committed
      replica <- CommittedReplica.fromSnapshot[IO](snapshotOf(genesis)).flatMap(IO.fromEither(_))
      c1      <- source.setCommitted(ord(1), stream.head)
      delta   <- IO.fromOption(c1.deltaFor(ord(1)))(new RuntimeException("delta missing"))
      forged = delta.copy(roots = delta.roots.copy(catalogRoot = genesis.roots.catalogRoot))
      result <- replica.applyDelta(forged)
    } yield expect(result.left.exists(_.isInstanceOf[ReplicationError.CatalogRootMismatch]))
  }

  test("a delta that does not chain from the replica's roots is rejected") {
    for {
      source  <- mkCommitted(s0)
      genesis <- source.committed
      replica <- CommittedReplica.fromSnapshot[IO](snapshotOf(genesis)).flatMap(IO.fromEither(_))
      _       <- source.setCommitted(ord(1), stream.head)
      c2      <- source.setCommitted(ord(2), stream(1))
      delta2  <- IO.fromOption(c2.deltaFor(ord(2)))(new RuntimeException("delta missing"))
      skipped <- replica.applyDelta(delta2) // replica never saw ordinal 1
    } yield expect(skipped.left.exists(_.isInstanceOf[ReplicationError.ParentRootsMismatch]))
  }

  test("a snapshot that does not reproduce its claimed roots is rejected") {
    for {
      genesis <- mkCommitted(s0).flatMap(_.committed)
      forged = snapshotOf(genesis).copy(entries = snapshotOf(genesis).entries - CommitKey.unsafe("fiber/aaa"))
      result <- CommittedReplica.fromSnapshot[IO](forged)
    } yield expect(result.left.exists(_.isInstanceOf[ReplicationError.SnapshotRootsMismatch]))
  }

  test("ring-buffer eviction forces the snapshot fallback, after which the stream resumes") {
    for {
      source  <- mkCommitted(s0, CommittedConfig(maxRecentDeltas = 1))
      genesis <- source.committed
      replica <- CommittedReplica.fromSnapshot[IO](snapshotOf(genesis)).flatMap(IO.fromEither(_))

      _  <- source.setCommitted(ord(1), stream.head)
      _  <- source.setCommitted(ord(2), stream(1))
      c3 <- source.setCommitted(ord(3), stream(2))

      // the replica is at genesis; the delta it needs (ordinal 1) has been evicted
      evicted = c3.deltaFor(ord(1)).isEmpty

      // fallback: rebuild from the source's current snapshot (verified against its roots)
      refreshed <- CommittedReplica.fromSnapshot[IO](snapshotOf(c3)).flatMap(IO.fromEither(_))

      // ...and the stream resumes from there
      c4     <- source.setCommitted(ord(4), s0)
      delta4 <- IO.fromOption(c4.deltaFor(ord(4)))(new RuntimeException("delta missing"))
      next   <- refreshed.applyDelta(delta4).flatMap(IO.fromEither(_))
    } yield expect.all(evicted, refreshed.roots == c3.roots, next.roots == c4.roots)
  }
}
