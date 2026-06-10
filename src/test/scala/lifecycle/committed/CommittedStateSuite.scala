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
 * delta-application byte-identical to the full rebuild (the `setCalculatedState` assertion),
 * the documented combined-hash definition, loud failure on wiring bugs, and ring-buffer eviction.
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
      c1 <- CommittedState.make[IO, ToyState](a).flatMap(_.committed)
      c2 <- CommittedState.make[IO, ToyState](b).flatMap(_.committed)
      h1 <- CommittedCommitment.deriveHash[IO, ToyState](a)
      h2 <- CommittedCommitment.deriveHash[IO, ToyState](b)
    } yield expect.all(c1.roots == c2.roots, h1 == h2)
  }

  test("delta-apply == full rebuild (and equals a fresh genesis at the new state)") {
    for {
      st      <- CommittedState.make[IO, ToyState](s0)
      c1      <- st.setCommitted(ord(1), s1)
      derived <- CommittedCommitment.buildTrie[IO](ToyState.view.entries(s1))
      fresh   <- CommittedState.make[IO, ToyState](s1).flatMap(_.committed)
    } yield
      expect.all(
        c1.roots.mptRoot == derived.rootNode.digest,
        c1.roots.mptRoot == fresh.roots.mptRoot
      )
  }

  test("empty <-> nonempty boundary stays canonical in both directions") {
    for {
      st        <- CommittedState.make[IO, ToyState](ToyState.empty)
      c1        <- st.setCommitted(ord(1), s0)
      c2        <- st.setCommitted(ord(2), ToyState.empty)
      derived   <- CommittedCommitment.deriveRoots[IO, ToyState](s0)
      emptyTrie <- CommittedCommitment.emptyTrie[IO]
    } yield
      expect.all(
        c1.roots.mptRoot == derived.mptRoot,
        c2.roots.mptRoot == emptyTrie.rootNode.digest
      )
  }

  test("combined hash is sha256(rawBytes(mptRoot) ++ rawBytes(smtRoot)) over the canonical pair") {
    for {
      roots <- CommittedCommitment.deriveRoots[IO, ToyState](s0)
      h     <- CommittedCommitment.deriveHash[IO, ToyState](s0)
      manual = Hash.fromBytes(Hex(roots.mptRoot.value).toBytes ++ Hex(roots.smtRoot.value.value).toBytes)
    } yield expect.all(h == manual, h == roots.combinedHash)
  }

  test("hashCalculatedState derivation is pure in the value: independent of local history") {
    for {
      st <- CommittedState.make[IO, ToyState](s0)
      _  <- st.setCommitted(ord(1), s1)
      _  <- st.setCommitted(ord(2), s0)
      // a node with history at s0 and a node that has only ever seen s0 agree
      hWithHistory <- st.committed.flatMap(c => CommittedCommitment.deriveHash[IO, ToyState](c.state))
      hFresh       <- CommittedCommitment.deriveHash[IO, ToyState](s0)
    } yield expect(hWithHistory == hFresh)
  }

  test("a lying CommittedView.delta is a wiring bug: setCommitted raises RootDivergence") {
    val lyingView: CommittedView[ToyState] = new CommittedView[ToyState] {
      def entries(s: ToyState) = ToyState.view.entries(s)
      override def delta(prev: ToyState, next: ToyState): CommitDelta = CommitDelta.empty
    }

    for {
      st <- CommittedState.make[IO, ToyState](s0, CommittedState.DefaultMaxRecentDeltas)(
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
      st <- CommittedState.make[IO, ToyState](s0, maxRecentDeltas = 2)
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
}
