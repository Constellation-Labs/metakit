package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal

import weaver.SimpleIOSuite

/**
 * The O(1) bootstrap contract: a fresh node verifies and adopts a downloaded state given ONLY the
 * state value and the latest signed snapshot's on-chain breadcrumb (no history replay); hydration
 * is verify-gated against the attested catalog root; the journal provides the local restart path;
 * and the combine-side breadcrumb validation accepts honest transitions and rejects forged ones.
 */
object CommittedBootstrapSuite extends SimpleIOSuite {
  import ToyFixtures._

  private def ord(n: Long): SnapshotOrdinal = SnapshotOrdinal.unsafeApply(n)

  private def state(i: Int): ToyState = ToyState(Map("aaa" -> i, "bbb" -> i * 7), Map("r" -> s"v$i"))

  private val config = CommittedConfig(epochSize = 2, sealedEpochRetention = 4)

  /** A source chain advanced to ordinal `n`. */
  private def source(n: Int, journal: Option[CatalogJournal[IO]] = None): IO[CommittedState[IO, ToyState]] =
    for {
      j  <- journal.fold(CatalogJournal.inMemory[IO])(IO.pure)
      st <- CommittedState.make[IO, ToyState](state(0), j, config)
      _  <- (1 to n).toList.traverse_(i => st.setCommitted(ord(i.toLong), state(i)))
    } yield st

  /** A "fresh downloading node": its own empty in-memory journal (behaves like the old None). */
  private val freshNode: IO[CommittedState[IO, ToyState]] =
    CatalogJournal.inMemory[IO].flatMap(CommittedState.make[IO, ToyState](state(0), _, config))

  test("O(1) bootstrap: hashCalculatedState is verifiable from state + breadcrumb alone") {
    for {
      src  <- source(5)
      cSrc <- src.committed
      bc = cSrc.breadcrumb

      fresh <- freshNode
      // the downloading node has NO history; it hashes the fetched state with the attested breadcrumb
      h <- fresh.hashFor(state(5), Some(bc))
    } yield expect(h == cSrc.roots.combinedHash)
  }

  test("seeding adopts the attested roots; the catalog starts unhydrated") {
    for {
      src    <- source(5)
      cSrc   <- src.committed
      fresh  <- freshNode
      seeded <- fresh.setCommitted(ord(5), state(5), Some(cSrc.breadcrumb))
    } yield
      expect.all(
        seeded.ordinal == ord(5),
        seeded.roots == cSrc.roots,
        !seeded.isHydrated,
        seeded.breadcrumb == cSrc.breadcrumb
      )
  }

  test("seeding rejects a state that does not reproduce the attested mptRoot, and a breadcrumb for another ordinal") {
    for {
      src          <- source(5)
      cSrc         <- src.committed
      fresh        <- freshNode
      wrongState   <- fresh.setCommitted(ord(5), state(4), Some(cSrc.breadcrumb)).attempt
      wrongOrdinal <- fresh.setCommitted(ord(6), state(5), Some(cSrc.breadcrumb)).attempt
      noBreadcrumb <- fresh.setCommitted(ord(5), state(5), None).attempt
    } yield
      expect.all(
        wrongState.left.exists(_.isInstanceOf[CommittedStateError.SeedStateMismatch]),
        wrongOrdinal.left.exists(_.isInstanceOf[CommittedStateError.CannotSeed]),
        noBreadcrumb.left.exists(_.isInstanceOf[CommittedStateError.CannotSeed])
      )
  }

  test("an unhydrated node cannot derive transitions (combine) until hydrated") {
    for {
      src     <- source(5)
      cSrc    <- src.committed
      fresh   <- freshNode
      seeded  <- fresh.setCommitted(ord(5), state(5), Some(cSrc.breadcrumb))
      attempt <- fresh.advanceWork(seeded.breadcrumb, ToyState.view.entries(state(6))).attempt
    } yield expect(attempt.left.exists(_.isInstanceOf[CommittedStateError.BreadcrumbUnresolvable]))
  }

  test("hydration is verify-gated: matching contents install, tampered contents are rejected") {
    for {
      src  <- source(5)
      cSrc <- src.committed
      contents = cSrc.catalogContents.getOrElse(throw new RuntimeException("source must be hydrated"))

      fresh <- freshNode
      _     <- fresh.setCommitted(ord(5), state(5), Some(cSrc.breadcrumb))

      forged = contents.copy(hot = contents.hot.map { case (o, _) => o -> cSrc.roots.mptRoot })
      rejected <- fresh.hydrate(forged)

      hydrated <- fresh.hydrate(contents).flatMap(IO.fromEither(_))

      // after hydration the node derives the SAME next commitment as the source
      _     <- src.setCommitted(ord(6), state(6))
      cSrc6 <- src.committed
      c6    <- fresh.setCommitted(ord(6), state(6))
    } yield
      expect.all(
        rejected.left.exists(_.isInstanceOf[CommittedStateError.HydrationRootMismatch]),
        hydrated.isHydrated,
        c6.roots == cSrc6.roots
      )
  }

  test("journal restart: a seeded cell hydrates immediately from its own persisted catalog") {
    for {
      journal <- CatalogJournal.inMemory[IO]
      src     <- source(5, journal.some) // writes through to the journal on every transition
      cSrc    <- src.committed

      // 'restart': a new cell over the SAME journal, seeded from the attested breadcrumb
      restarted <- CommittedState.make[IO, ToyState](state(0), journal, config)
      seeded    <- restarted.setCommitted(ord(5), state(5), Some(cSrc.breadcrumb))

      // and it can continue producing transitions in lock-step with the source
      _     <- src.setCommitted(ord(6), state(6))
      cSrc6 <- src.committed
      c6    <- restarted.setCommitted(ord(6), state(6))
    } yield
      expect.all(
        seeded.isHydrated,
        seeded.roots == cSrc.roots,
        c6.roots == cSrc6.roots
      )
  }

  test("LevelDB journal: sealed-epoch roots survive a process restart and re-hydrate the cell") {
    import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
    import java.nio.file.attribute.BasicFileAttributes
    import cats.effect.Resource

    def deleteRecursively(path: Path): IO[Unit] = IO {
      Files.walkFileTree(
        path,
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.delete(file); FileVisitResult.CONTINUE
          }
          override def postVisitDirectory(dir: Path, exc: java.io.IOException): FileVisitResult = {
            Files.delete(dir); FileVisitResult.CONTINUE
          }
        }
      )
    }.void

    val tmpDir = Resource.make(IO(Files.createTempDirectory("committed_journal_")))(deleteRecursively)

    tmpDir.use { dir =>
      for {
        // first 'process': run the chain over a LevelDB journal, then close it
        cSrc <- CatalogJournal.levelDb[IO](dir).use { journal =>
          source(5, journal.some).flatMap(_.committed)
        }
        // second 'process': fresh cell, SAME db path -- the journal alone hydrates the seed
        seeded <- CatalogJournal.levelDb[IO](dir).use { journal =>
          CommittedState
            .make[IO, ToyState](state(0), journal, config)
            .flatMap(_.setCommitted(ord(5), state(5), Some(cSrc.breadcrumb)))
        }
      } yield expect.all(seeded.isHydrated, seeded.roots == cSrc.roots)
    }
  }

  test("journal contents track sealing: hot window pruned, level-1 entries accumulate") {
    for {
      journal <- CatalogJournal.inMemory[IO]
      _       <- source(5, journal.some) // epochSize=2: epochs 0 and 1 sealed, hot = {4}
      c       <- journal.contents
      (hot, level1) = c
    } yield expect.all(hot.keySet == Set(4L), level1.keySet == Set(0L, 1L))
  }

  test("combine-side validation: the honest parent advances; a forged breadcrumb is rejected") {
    for {
      src  <- source(3)
      cSrc <- src.committed

      next <- src.advanceWork(cSrc.breadcrumb, ToyState.view.entries(state(4)))
      // the emitted breadcrumb must equal what setCommitted then derives
      c4 <- src.setCommitted(ord(4), state(4))

      // forged at the committed ordinal: the follower's transition check fires
      forged = c4.breadcrumb.copy(roots = c4.roots.copy(mptRoot = cSrc.roots.mptRoot))
      rejected <- src.advanceWork(forged, ToyState.view.entries(state(5))).attempt

      // forged roots that correspond to NO committed state (a real mptRoot paired with a catalog
      // root that does not recompose from it): not the cell, not the work cache, unreproducible
      // from the journal -- unresolvable, equally fatal. Resolution is by ROOTS, not the claimed
      // ordinal: a populated journal legitimately resolves genuine historical roots (the restart /
      // replay path), so the "unresolvable" case must use roots the node never committed.
      unknown = CommittedBreadcrumb(ord(9), c4.roots.copy(catalogRoot = cSrc.roots.catalogRoot))
      unresolvable <- src.advanceWork(unknown, ToyState.view.entries(state(5))).attempt
    } yield
      expect.all(
        next.ordinal == ord(4),
        next.roots == c4.roots,
        rejected.left.exists(_.isInstanceOf[CommittedStateError.BreadcrumbMismatch]),
        unresolvable.left.exists(_.isInstanceOf[CommittedStateError.BreadcrumbUnresolvable])
      )
  }

  test("setCommitted cross-checks a consensus breadcrumb for the same ordinal") {
    for {
      src  <- source(3)
      cSrc <- src.committed
      // an honest follower derives the same roots the proposer attested
      preview <- src.advanceWork(cSrc.breadcrumb, ToyState.view.entries(state(4)))
      ok      <- src.setCommitted(ord(4), state(4), Some(preview))

      // a forged attested breadcrumb for the NEXT ordinal is caught at commit time
      forged = CommittedBreadcrumb(ord(5), ok.roots)
      bad <- src.setCommitted(ord(5), state(5), Some(forged)).attempt
    } yield
      expect.all(
        ok.roots == preview.roots,
        bad.left.exists(_.isInstanceOf[CommittedStateError.BreadcrumbMismatch])
      )
  }

  test("replay: consecutive advanceWork calls chain through the work cache without touching the cell") {
    for {
      src  <- source(2)
      cSrc <- src.committed

      // fold combine over three future ordinals (the DataApplicationTraverse pattern)
      b3 <- src.advanceWork(cSrc.breadcrumb, ToyState.view.entries(state(3)))
      b4 <- src.advanceWork(b3, ToyState.view.entries(state(4)))
      b5 <- src.advanceWork(b4, ToyState.view.entries(state(5)))

      // the cell is untouched...
      stillAt2 <- src.committed

      // ...and an independent chain that COMMITS each step derives identical breadcrumbs
      other  <- source(5)
      cOther <- other.committed
    } yield
      expect.all(
        stillAt2.ordinal == ord(2),
        b5 == cOther.breadcrumb,
        b4.ordinal == ord(4)
      )
  }
}
