package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{
  MerklePatriciaBatchInclusionVerifier,
  MerklePatriciaVerifier,
  PathNotFound
}
import io.constellationnetwork.metagraph_sdk.crypto.smt.api.SparseMerkleVerifier
import io.constellationnetwork.metagraph_sdk.crypto.smt.{SparseMerkleEntry, SparseMerkleProof}
import io.constellationnetwork.metagraph_sdk.json_logic.core.{BoolValue, JsonLogicValue, MapValue, StrValue}
import io.constellationnetwork.metagraph_sdk.json_logic.ops.AuthDbOps
import io.constellationnetwork.schema.SnapshotOrdinal

import io.circe.Json
import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite

/**
 * Proof-surface tests: single-key / batch / namespace-prefix proofs against the committed MPT root
 * (verified with the existing verifiers AND through the JLVM's `mpt_prefix_verify` hex formats),
 * plus SMT catalog membership (current + historical) and first-class NON-membership.
 */
object CommittedProofSuite extends SimpleIOSuite {
  import ToyFixtures._

  private def ord(n: Long): SnapshotOrdinal = SnapshotOrdinal.unsafeApply(n)

  private val s0 = ToyState(Map("aaa" -> 1, "bbb" -> 2), Map("alpha" -> "x"))
  private val s1 = ToyState(Map("aaa" -> 5, "bbb" -> 2), Map("alpha" -> "x"))
  private val s2 = ToyState(Map("aaa" -> 5, "bbb" -> 9), Map("alpha" -> "x", "beta" -> "y"))

  test("single-key proof verifies against the committed mptRoot; absent key is PathNotFound") {
    for {
      c       <- CommittedState.make[IO, ToyState](s0).flatMap(_.committed)
      proof   <- c.proveKey(CommitKey.unsafe("fiber/aaa")).flatMap(IO.fromEither(_))
      ok      <- MerklePatriciaVerifier.make[IO](c.roots.mptRoot).confirm(proof)
      missing <- c.proveKey(CommitKey.unsafe("fiber/zzz"))
    } yield expect.all(ok.isRight, missing.left.exists(_.isInstanceOf[PathNotFound]))
  }

  test("batch proof covers all requested keys and verifies") {
    val keys = List(CommitKey.unsafe("fiber/aaa"), CommitKey.unsafe("registry/alpha"))
    for {
      c     <- CommittedState.make[IO, ToyState](s0).flatMap(_.committed)
      proof <- c.proveKeys(keys).flatMap(IO.fromEither(_))
      ok    <- MerklePatriciaBatchInclusionVerifier.make[IO](c.roots.mptRoot).confirm(proof)
    } yield expect.all(ok.isRight, proof.paths.toSet == keys.map(_.toHex).toSet)
  }

  test("namespace attestation covers exactly the namespace's keys and verifies") {
    for {
      c     <- CommittedState.make[IO, ToyState](s0).flatMap(_.committed)
      proof <- c.attestNamespace(CommitNamespace.unsafe("fiber")).flatMap(IO.fromEither(_))
      ok    <- MerklePatriciaBatchInclusionVerifier.make[IO](c.roots.mptRoot).confirm(proof)
    } yield expect.all(
      ok.isRight,
      proof.paths.toSet == Set(CommitKey.unsafe("fiber/aaa").toHex, CommitKey.unsafe("fiber/bbb").toHex)
    )
  }

  test("namespace attestation is JLVM mpt_prefix_verify compatible (0x-prefixed hex formats)") {
    val ns = CommitNamespace.unsafe("fiber")
    val entries = ToyState.view.entries(s0).filter { case (k, _) => k.namespace == "fiber" }

    def jlv(json: Json): JsonLogicValue =
      json.as[JsonLogicValue].fold(e => throw new RuntimeException(s"bad bridge: $e"), identity)

    for {
      c     <- CommittedState.make[IO, ToyState](s0).flatMap(_.committed)
      proof <- c.attestNamespace(ns).flatMap(IO.fromEither(_))
      entriesJlv = MapValue(entries.map { case (k, v) => k.toHex.value -> jlv(v) }.toMap)
      args = List[JsonLogicValue](
        StrValue("0x" + c.roots.mptRoot.value),
        StrValue("0x" + ns.prefixHex.value),
        entriesJlv,
        jlv(proof.asJson)
      )
      complete   <- AuthDbOps.mptPrefixVerify[IO](args)
      incomplete <- AuthDbOps.mptPrefixVerify[IO](
        args.updated(2, MapValue(entriesJlv.value - CommitKey.unsafe("fiber/bbb").toHex.value))
      )
    } yield expect.all(complete == Right(BoolValue(true)), incomplete == Right(BoolValue(false)))
  }

  test("SMT catalog: current + historical membership, and NON-membership of an absent ordinal") {
    for {
      st <- CommittedState.make[IO, ToyState](s0)
      c1 <- st.setCommitted(ord(1), s1)
      c2 <- st.setCommitted(ord(2), s2)
      verifier = SparseMerkleVerifier.make[IO]

      currentProof <- c2.proveCatalog(CommitCatalog.CurrentMptName).flatMap(IO.fromEither(_))
      current      <- verifier.verify(c2.roots.smtRoot, currentProof).flatMap(IO.fromEither(_))

      historicalProof <- c2.proveCatalog(CommitCatalog.ordinalName(ord(1))).flatMap(IO.fromEither(_))
      historical      <- verifier.verify(c2.roots.smtRoot, historicalProof).flatMap(IO.fromEither(_))

      absentProof <- c2.proveCatalog(CommitCatalog.ordinalName(ord(999999))).flatMap(IO.fromEither(_))
      absent      <- verifier.verify(c2.roots.smtRoot, absentProof).flatMap(IO.fromEither(_))
    } yield expect.all(
      current.value match {
        case SparseMerkleEntry.Present(_, value) => value.sameElements(CommitCatalog.rootValueBytes(c2.roots.mptRoot))
        case _                                   => false
      },
      historical.value match {
        case SparseMerkleEntry.Present(_, value) => value.sameElements(CommitCatalog.rootValueBytes(c1.roots.mptRoot))
        case _                                   => false
      },
      absentProof.isInstanceOf[SparseMerkleProof.Absence],
      absent.value match {
        case SparseMerkleEntry.Absent(_) => true
        case _                           => false
      }
    )
  }
}
