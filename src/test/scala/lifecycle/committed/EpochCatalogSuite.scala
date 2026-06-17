package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hex.Hex

import weaver.SimpleIOSuite

/**
 * The two-level epoch rollup: boundary sealing, composition, ancient-ordinal proofs (two
 * fixed-depth inclusions), non-membership at both levels, retention pruning ("serving, not
 * trust"), and contents round-trips.
 */
object EpochCatalogSuite extends SimpleIOSuite {
  import ToyFixtures._

  private def ord(n: Long): SnapshotOrdinal = SnapshotOrdinal.unsafeApply(n)

  private def state(i: Int): ToyState = ToyState(Map("aaa" -> i), Map.empty)

  /** Drive a fresh committed cell through ordinals 1..n (committing state(i) at ordinal i). */
  private def driveTo(n: Int, config: CommittedConfig): IO[(CommittedState[IO, ToyState], Vector[Committed[IO, ToyState]])] =
    for {
      st <- mkCommitted(state(0), config)
      g  <- st.committed
      cs <- (1 to n).toList.traverse(i => st.setCommitted(ord(i.toLong), state(i)))
    } yield (st, g +: cs.toVector)

  private def epochsOf(c: Committed[IO, ToyState]): EpochCatalog[IO] =
    c.catalog.live.map(_.epochs).getOrElse(throw new RuntimeException("expected a hydrated catalog"))

  test("hot epoch fills until the boundary, then seals into level-1 and resets") {
    val config = CommittedConfig(epochSize = 4, sealedEpochRetention = 4)
    for {
      r <- driveTo(5, config)
      (_, cs) = r
      atFour = epochsOf(cs(4)) // catalog holds ordinals 0..3 (epoch 0 complete, not yet sealed)
      atFive = epochsOf(cs(5)) // inserting ordinal 4 sealed epoch 0
    } yield
      expect.all(
        atFour.hotEntries.keySet == Set(0L, 1L, 2L, 3L),
        atFour.level1Entries.isEmpty,
        atFive.hotEntries.keySet == Set(4L),
        atFive.level1Entries.keySet == Set(0L),
        atFive.epochIndex == 1L,
        atFive.sealedTrees.keySet == Set(0L)
      )
  }

  test("the sealed level-1 entry is exactly the pre-seal hot root") {
    val config = CommittedConfig(epochSize = 4)
    for {
      r <- driveTo(5, config)
      (_, cs) = r
      preSealHotRoot <- epochsOf(cs(4)).hotTree.root
      sealedEntry = epochsOf(cs(5)).level1Entries.get(0L)
    } yield expect(sealedEntry.contains(preSealHotRoot.value))
  }

  test("hot-ordinal proof: inclusion verifies to the committed MPT root") {
    val config = CommittedConfig(epochSize = 4)
    for {
      r <- driveTo(3, config)
      (st, cs) = r
      c     <- st.committed
      proof <- c.proveOrdinal(ord(2)).flatMap(IO.fromEither(_))
      result <- OrdinalCatalogProofVerifier
        .verify[IO](c.roots.catalogRoot, proof, config.epochSize)
        .flatMap(IO.fromEither(_))
    } yield expect(result == OrdinalAttestation.CommittedAt(2L, cs(2).roots.mptRoot))
  }

  test("ancient-ordinal proof: two fixed-depth inclusions through level-1 and the sealed epoch tree") {
    val config = CommittedConfig(epochSize = 4)
    for {
      r <- driveTo(7, config) // epoch 0 (ordinals 0..3) sealed; hot = {4,5,6}
      (st, cs) = r
      c     <- st.committed
      proof <- c.proveOrdinal(ord(1)).flatMap(IO.fromEither(_))
      result <- OrdinalCatalogProofVerifier
        .verify[IO](c.roots.catalogRoot, proof, config.epochSize)
        .flatMap(IO.fromEither(_))
    } yield
      expect.all(
        proof.sealedEntry.nonEmpty,
        result == OrdinalAttestation.CommittedAt(1L, cs(1).roots.mptRoot)
      )
  }

  test("non-membership: an uncommitted ordinal is provably absent at both levels") {
    val config = CommittedConfig(epochSize = 2)
    for {
      r <- driveTo(3, config) // catalog holds ordinals 0..2
      (st, _) = r
      c     <- st.committed
      proof <- c.proveOrdinal(ord(999)).flatMap(IO.fromEither(_))
      result <- OrdinalCatalogProofVerifier
        .verify[IO](c.roots.catalogRoot, proof, config.epochSize)
        .flatMap(IO.fromEither(_))
    } yield expect(result == OrdinalAttestation.NotCommitted(999L))
  }

  test("a tampered inclusion is rejected by the verifier") {
    val config = CommittedConfig(epochSize = 4)
    for {
      r <- driveTo(3, config)
      (st, _) = r
      c     <- st.committed
      proof <- c.proveOrdinal(ord(2)).flatMap(IO.fromEither(_))
      tampered = proof.hot match {
        case i: io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleProof.Inclusion =>
          proof.copy(hot = i.copy(value = Hex.fromBytes(i.value.toBytes.map(b => (b ^ 0x01).toByte))))
        case other => proof.copy(hot = other)
      }
      result <- OrdinalCatalogProofVerifier.verify[IO](c.roots.catalogRoot, tampered, config.epochSize)
    } yield expect(result.left.exists(_.isInstanceOf[CommittedProofError.ProofInvalid]))
  }

  test("a wrong epochSize cannot be smuggled past the verifier (keys are recomputed locally)") {
    val config = CommittedConfig(epochSize = 2)
    for {
      r <- driveTo(3, config)
      (st, _) = r
      c      <- st.committed
      proof  <- c.proveOrdinal(ord(999)).flatMap(IO.fromEither(_)) // absence proof, epoch keyed by size 2
      result <- OrdinalCatalogProofVerifier.verify[IO](c.roots.catalogRoot, proof, 3)
    } yield expect(result.isLeft)
  }

  test("retention prunes SERVING of old sealed epochs; committed roots and old proofs stay verifiable") {
    val config = CommittedConfig(epochSize = 2, sealedEpochRetention = 1)
    for {
      r <- driveTo(4, config) // epoch 0 sealed (at transition to 3), retained (only seal so far)
      (st, cs) = r
      cAtFour = cs(4)
      oldProof <- cAtFour.proveOrdinal(ord(0)).flatMap(IO.fromEither(_))

      c5           <- st.setCommitted(ord(5), state(5)) // seals epoch 1 -> retention evicts epoch 0's tree
      freshAttempt <- c5.proveOrdinal(ord(0))

      epochs5 = epochsOf(c5)

      // the proof issued before pruning still verifies against the roots it attested
      oldStillValid <- OrdinalCatalogProofVerifier
        .verify[IO](cAtFour.roots.catalogRoot, oldProof, config.epochSize)
        .flatMap(IO.fromEither(_))
    } yield
      expect.all(
        freshAttempt.left.exists(_.isInstanceOf[CommittedProofError.EpochPruned]),
        epochs5.level1Entries.keySet == Set(0L, 1L), // roots never pruned
        epochs5.sealedTrees.keySet == Set(1L), // serving cache pruned to last K
        oldStillValid == OrdinalAttestation.CommittedAt(0L, cs(0).roots.mptRoot)
      )
  }

  test("catalog contents round-trip: rebuild composes to identical roots") {
    val config = CommittedConfig(epochSize = 2, sealedEpochRetention = 2)
    for {
      r <- driveTo(5, config)
      (st, _) = r
      c <- st.committed
      contents = c.catalogContents.getOrElse(throw new RuntimeException("hydrated catalog expected"))
      rebuilt  <- EpochCatalog.fromContents[IO](config, contents).flatMap(IO.fromEither(_))
      composed <- rebuilt.compose(c.roots.mptRoot)
    } yield expect(composed._2 == c.roots.catalogRoot)
  }

  test("tampered contents are rejected structurally or by root comparison") {
    val config = CommittedConfig(epochSize = 2, sealedEpochRetention = 2)
    for {
      r <- driveTo(5, config)
      (st, _) = r
      c <- st.committed
      contents = c.catalogContents.getOrElse(throw new RuntimeException("hydrated catalog expected"))
      // a sealed epoch whose contents do not reproduce its level-1 root is rejected outright
      tamperedSealed = contents.copy(
        sealedEpochs = contents.sealedEpochs.map { case (e, m) => e -> (m - m.firstKey) }
      )
      structural <- EpochCatalog.fromContents[IO](config, tamperedSealed)
      // a tampered HOT entry rebuilds fine but recomposes to a different root
      tamperedHot = contents.copy(hot = contents.hot.map { case (o, _) => o -> c.roots.mptRoot })
      rebuilt  <- EpochCatalog.fromContents[IO](config, tamperedHot).flatMap(IO.fromEither(_))
      composed <- rebuilt.compose(c.roots.mptRoot)
    } yield
      expect.all(
        structural.left.exists(_.isInstanceOf[CommittedStateError.MalformedCatalogContents]),
        composed._2 != c.roots.catalogRoot
      )
  }
}
