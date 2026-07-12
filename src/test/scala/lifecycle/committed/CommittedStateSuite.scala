package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import weaver.SimpleIOSuite

/**
 * Core invariants of the committed-state cell: canonical roots regardless of construction order,
 * delta-application byte-identical to the full rebuild (the `setCalculatedState` assertion), the
 * documented combined-hash definition over the LIVE catalog, per-ordinal catalog evolution, loud
 * failure on wiring bugs, and ring-buffer eviction.
 */
object CommittedStateSuite extends SimpleIOSuite {
  import ToyFixtures._

  private def ord(n: Long): SnapshotOrdinal = SnapshotOrdinal.unsafeApply(n)

  private val s0 = ToyState(Map("aaa" -> 1, "bbb" -> 2), Map("alpha" -> "x"))
  // vs s0: modifies fiber/aaa, removes fiber/bbb, adds fiber/ccc and registry/beta
  private val s1 = ToyState(Map("aaa" -> 5, "ccc" -> 3), Map("alpha" -> "x", "beta" -> "y"))

  test("root determinism: same logical state in different construction orders yields the same roots") {
    // Small immutable Maps (Map1..Map4) iterate in INSERTION order, so these two genuinely
    // enumerate differently while being equal as maps.
    val a = ToyState(Map("k1" -> 1, "k2" -> 2, "k3" -> 3, "k4" -> 4), Map("r" -> "v"))
    val b = ToyState(Map("k4" -> 4, "k3" -> 3, "k2" -> 2, "k1" -> 1), Map("r" -> "v"))

    for {
      c1 <- mkCommitted(a).flatMap(_.committed)
      c2 <- mkCommitted(b).flatMap(_.committed)
    } yield expect.all(c1.roots == c2.roots, c1.roots.combinedHash == c2.roots.combinedHash)
  }

  test("delta-apply == full rebuild (and equals a fresh genesis at the new state)") {
    for {
      st      <- mkCommitted(s0)
      c1      <- st.setCommitted(ord(1), s1)
      derived <- CommittedCommitment.buildTrie[IO](ToyState.view.entries(s1))
      fresh   <- mkCommitted(s1).flatMap(_.committed)
    } yield
      expect.all(
        c1.roots.mptRoot == derived.rootNode.digest,
        c1.roots.mptRoot == fresh.roots.mptRoot
      )
  }

  test("empty <-> nonempty boundary stays canonical in both directions") {
    for {
      st        <- mkCommitted(ToyState.empty)
      c1        <- st.setCommitted(ord(1), s0)
      c2        <- st.setCommitted(ord(2), ToyState.empty)
      derived   <- CommittedCommitment.buildTrie[IO](ToyState.view.entries(s0))
      emptyTrie <- CommittedCommitment.emptyTrie[IO]
    } yield
      expect.all(
        c1.roots.mptRoot == derived.rootNode.digest,
        c2.roots.mptRoot == emptyTrie.rootNode.digest
      )
  }

  test("combined hash is sha256(rawBytes(mptRoot) ++ rawBytes(liveCatalogRoot))") {
    for {
      st <- mkCommitted(s0)
      c1 <- st.setCommitted(ord(1), s1)
      manual = Hash.fromBytes(Hex(c1.roots.mptRoot.value).toBytes ++ Hex(c1.roots.catalogRoot.value.value).toBytes)
    } yield expect(c1.roots.combinedHash == manual)
  }

  test("the live catalog COMMITS history: same state value at different ordinals -> different catalog roots") {
    for {
      st <- mkCommitted(s0)
      c1 <- st.setCommitted(ord(1), s1)
      c2 <- st.setCommitted(ord(2), s0) // back to the s0 VALUE, but with two ordinals of history
      g  <- mkCommitted(s0).flatMap(_.committed)
    } yield
      expect.all(
        c2.roots.mptRoot == g.roots.mptRoot, // tier 1 is pure in the value
        c2.roots.catalogRoot != g.roots.catalogRoot, // tier 2 commits history
        c1.roots.catalogRoot != c2.roots.catalogRoot, // and evolves every ordinal
        c2.roots.combinedHash != g.roots.combinedHash
      )
  }

  test("hashFor on the steady-state path equals the next transition's combined hash") {
    for {
      st <- mkCommitted(s0)
      h  <- st.hashFor(s1, None) // cell at genesis = parent of ordinal 1
      c1 <- st.setCommitted(ord(1), s1)
    } yield expect(h == c1.roots.combinedHash)
  }

  test("re-committing the same ordinal is idempotent for the same value and refused for a different one") {
    for {
      st     <- mkCommitted(s0)
      c1     <- st.setCommitted(ord(1), s1)
      again  <- st.setCommitted(ord(1), s1)
      forged <- st.setCommitted(ord(1), s0).attempt
    } yield
      expect.all(
        again.roots == c1.roots,
        forged.left.exists(_.isInstanceOf[CommittedStateError.CommitRewrite])
      )
  }

  test("a lying CommittedView.delta is a wiring bug: setCommitted raises RootDivergence") {
    val lyingView: CommittedView[ToyState] = new CommittedView[ToyState] {
      def entries(s: ToyState) = ToyState.view.entries(s)
      override def delta(prev: ToyState, next: ToyState): CommitDelta = CommitDelta.empty
    }

    for {
      journal <- CatalogJournal.inMemory[IO]
      st <- CommittedState.make[IO, ToyState](s0, journal, CommittedConfig.default)(
        IO.asyncForIO,
        JsonBinaryHasher.deriveFromCodec[IO],
        lyingView
      )
      result <- st.setCommitted(ord(1), s1).attempt
    } yield expect(result.left.exists(_.isInstanceOf[CommittedStateError.RootDivergence]))
  }

  test("recentDeltas is a ring buffer: old ordinals evict, recent ones remain") {
    val states = List(
      ToyState(Map("aaa" -> 1), Map.empty),
      ToyState(Map("aaa" -> 2), Map.empty),
      ToyState(Map("aaa" -> 3), Map.empty),
      ToyState(Map("aaa" -> 4), Map.empty)
    )

    for {
      st <- mkCommitted(s0, CommittedConfig(maxRecentDeltas = 2))
      _  <- states.zipWithIndex.traverse { case (s, i) => st.setCommitted(ord(i.toLong + 1), s) }
      c  <- st.committed
    } yield
      expect.all(
        c.recentDeltas.size == 2,
        c.deltaFor(ord(1)).isEmpty,
        c.deltaFor(ord(2)).isEmpty,
        c.deltaFor(ord(3)).nonEmpty,
        c.deltaFor(ord(4)).nonEmpty
      )
  }

  pureTest("CommitKey grammar: valid keys parse, invalid keys are rejected") {
    expect.all(
      CommitKey.from("fiber/0e2f7c1a-aaaa-bbbb-cccc-000000000001").isRight,
      CommitKey.from("registry/tokens/dag").isRight,
      CommitKey.from("meta/schema.v1").isRight,
      CommitKey.from("").isLeft,
      CommitKey.from("Fiber/abc").isLeft,
      CommitKey.from("fiber//abc").isLeft,
      CommitKey.from("/fiber").isLeft,
      CommitKey.from("fiber/").isLeft,
      CommitKey.from("fiber/_bad").isLeft,
      CommitKey.from("a" * 300).isLeft,
      CommitKey.from(List.fill(17)("a").mkString("/")).isLeft
    )
  }

  pureTest("CommitNamespace prefix is segment-exact (trailing separator)") {
    val ns = CommitNamespace.unsafe("fiber")
    expect.all(
      ns.prefixHex == CommitKey.hexOf("fiber/"),
      CommitKey.unsafe("fiber/abc").toHex.value.startsWith(ns.prefixHex.value),
      !CommitKey.unsafe("fiberx/abc").toHex.value.startsWith(ns.prefixHex.value)
    )
  }
  // ── incrementalTrie mode (CommittedConfig.incrementalTrie = true) ────────────────────────────
  // Roots must be IDENTICAL to full-rebuild mode in every path -- the flag changes how roots are
  // computed, never their value. See CommittedConfig.incrementalTrie for the trust model.

  private val incrementalCfg = CommittedConfig(incrementalTrie = true)

  test("incremental mode: a transition sequence yields roots identical to full-rebuild mode") {
    // exercises upserts, removals, collapse-to-empty (the emptied-trie applyDelta branch), and
    // regrowth from empty
    val states = List(
      s1,
      ToyState(Map("aaa" -> 5), Map.empty),
      ToyState.empty,
      ToyState(Map("zzz" -> 9), Map("gamma" -> "g"))
    )
    def run(cfg: CommittedConfig): IO[List[CommittedRoots]] =
      mkCommitted(s0, cfg).flatMap { st =>
        states.zipWithIndex.traverse { case (s, i) => st.setCommitted(ord(i.toLong + 1), s).map(_.roots) }
      }
    for {
      full <- run(CommittedConfig.default)
      inc  <- run(incrementalCfg)
    } yield expect(full == inc)
  }

  test("incremental mode: hashFor equals full-rebuild hashFor for multiple speculative states") {
    val candidateA = s1
    val candidateB = ToyState(Map("aaa" -> 7), Map("alpha" -> "x"))
    for {
      stFull <- mkCommitted(s0)
      stInc  <- mkCommitted(s0, incrementalCfg)
      fa     <- stFull.hashFor(candidateA, None)
      fb     <- stFull.hashFor(candidateB, None)
      ia     <- stInc.hashFor(candidateA, None)
      ib     <- stInc.hashFor(candidateB, None)
    } yield expect.all(fa == ia, fb == ib)
  }

  test("incremental mode: advanceWork emits the same breadcrumbs, including replay chaining") {
    val s2 = ToyState(Map("aaa" -> 5), Map.empty)
    for {
      stFull <- mkCommitted(s0)
      stInc  <- mkCommitted(s0, incrementalCfg)
      gFull  <- stFull.committed.map(_.breadcrumb)
      gInc   <- stInc.committed.map(_.breadcrumb)
      bFull  <- stFull.advanceWork(gFull, s1)
      bInc   <- stInc.advanceWork(gInc, s1)
      // chain off the work cache: the parent is NOT the cell -- the incremental base is still
      // the cell's trie, correct because the MPT is canonical in the final entry set
      b2Full <- stFull.advanceWork(bFull, s2)
      b2Inc  <- stInc.advanceWork(bInc, s2)
    } yield expect.all(bFull == bInc, b2Full == b2Inc)
  }

  test("incremental mode: the stored trie is STRUCTURALLY identical to a from-scratch build") {
    // not merely digest-equal: proofs are served from the stored trie, so its node structure
    // must match what any verifier rebuilding from entries would produce
    for {
      st      <- mkCommitted(s0, incrementalCfg)
      c1      <- st.setCommitted(ord(1), s1)
      rebuilt <- CommittedCommitment.buildTrie[IO](ToyState.view.entries(s1))
    } yield expect.all(c1.trie == rebuilt, c1.roots.mptRoot == rebuilt.rootNode.digest)
  }

  test("incremental mode trusts the view: a lying delta commits WITHOUT RootDivergence (the trade)") {
    // With the from-scratch cross-check skipped, an unfaithful custom delta is NOT caught at
    // runtime -- the applyDelta == buildTrie property test is the offline guarantee. This test
    // documents the failure mode the flag accepts; it is the reason the default stays false.
    val lyingView: CommittedView[ToyState] = new CommittedView[ToyState] {
      def entries(s: ToyState) = ToyState.view.entries(s)
      override def delta(prev: ToyState, next: ToyState): CommitDelta = CommitDelta.empty
    }
    for {
      journal <- CatalogJournal.inMemory[IO]
      st <- CommittedState.make[IO, ToyState](s0, journal, incrementalCfg)(
        IO.asyncForIO,
        JsonBinaryHasher.deriveFromCodec[IO],
        lyingView
      )
      genesis <- st.committed
      result  <- st.setCommitted(ord(1), s1).attempt
    } yield
      expect.all(
        result.isRight,
        // empty (lying) delta => root unchanged from genesis: committed, silently wrong
        result.exists(_.roots.mptRoot == genesis.roots.mptRoot)
      )
  }
}
