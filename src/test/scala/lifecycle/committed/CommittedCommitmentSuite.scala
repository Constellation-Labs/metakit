package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.IO

import scala.collection.immutable.SortedMap

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * Pins the content-hash rule (docs/content-hash.md) on the committed state-dict: entry values reach
 * the MPT leaves through `JsonBinaryHasher` -> `JsonBinaryCodec.serialize`, which drops null OBJECT
 * fields (and preserves array nulls) before RFC 8785 canonicalization. Therefore a `CommittedView`
 * that projects `Option = None` leaves as explicit nulls commits to the SAME root as one that omits
 * the fields -- adding optional fields to a projection never changes prior roots.
 */
object CommittedCommitmentSuite extends SimpleIOSuite with Checkers {

  private def entries(pairs: (String, Json)*): SortedMap[CommitKey, Json] =
    SortedMap.from(pairs.map { case (k, v) => CommitKey.unsafe(k) -> v })

  test("explicit-null entry fields commit to the same MPT root as absent fields") {
    val withNulls = entries(
      "fiber/aaa"      -> Json.obj("count" -> 1.asJson, "metadata" -> Json.Null),
      "registry/alpha" -> Json.obj("v" -> "x".asJson, "note" -> Json.Null, "inner" -> Json.obj("opt" -> Json.Null))
    )
    val withoutNulls = entries(
      "fiber/aaa"      -> Json.obj("count" -> 1.asJson),
      "registry/alpha" -> Json.obj("v" -> "x".asJson, "inner" -> Json.obj())
    )

    for {
      trieWith    <- CommittedCommitment.buildTrie[IO](withNulls)
      trieWithout <- CommittedCommitment.buildTrie[IO](withoutNulls)
    } yield expect.same(trieWithout.rootNode.digest, trieWith.rootNode.digest)
  }

  test("array nulls in entry values are significant (different root)") {
    val a = entries("fiber/aaa" -> Json.obj("xs" -> Json.arr(1.asJson, Json.Null, 3.asJson)))
    val b = entries("fiber/aaa" -> Json.obj("xs" -> Json.arr(1.asJson, 3.asJson)))

    for {
      trieA <- CommittedCommitment.buildTrie[IO](a)
      trieB <- CommittedCommitment.buildTrie[IO](b)
    } yield expect(trieA.rootNode.digest != trieB.rootNode.digest)
  }

  test("applyDelta with null-containing upserts matches the full rebuild over dropped-equivalent entries") {
    val base = entries("fiber/aaa" -> Json.obj("count" -> 1.asJson))
    val upsertWithNull = Json.obj("count" -> 2.asJson, "metadata" -> Json.Null)
    val upsertAbsent = Json.obj("count" -> 2.asJson)

    for {
      trie0 <- CommittedCommitment.buildTrie[IO](base)
      viaDelta <- CommittedCommitment.applyDelta[IO](
        trie0,
        CommitDelta(entries("fiber/aaa" -> upsertWithNull), scala.collection.immutable.SortedSet.empty)
      )
      rebuilt <- CommittedCommitment.buildTrie[IO](entries("fiber/aaa" -> upsertAbsent))
    } yield expect.same(rebuilt.rootNode.digest, viaDelta.rootNode.digest)
  }
  test("PROPERTY: applyDelta(buildTrie(prev), structuralDiff) == buildTrie(next), structurally") {
    // The offline guarantee behind CommittedConfig.incrementalTrie: for ANY pair of entry sets,
    // applying the default structural diff to the previous trie reproduces the from-scratch trie
    // EXACTLY (node structure, not just digest). Uses the same diff the default
    // CommittedView.delta computes.
    val keyGen: Gen[String] =
      Gen.chooseNum(0, 9999).map(n => f"fiber/k$n%04d")
    val entriesGen: Gen[SortedMap[CommitKey, Json]] =
      Gen
        .chooseNum(0, 40)
        .flatMap(n => Gen.listOfN(n, Gen.zip(keyGen, Gen.chooseNum(0, 1000))))
        .map(kvs => SortedMap.from(kvs.map { case (k, v) => CommitKey.unsafe(k) -> Json.obj("count" -> v.asJson) }))

    implicit val showEntries: cats.Show[(SortedMap[CommitKey, Json], SortedMap[CommitKey, Json])] =
      cats.Show.fromToString

    forall(Gen.zip(entriesGen, entriesGen)) {
      case (prev, next) =>
        val upserts = next.filter { case (k, v) => !prev.get(k).contains(v) }
        val removes = scala.collection.immutable.SortedSet.from(prev.keySet.diff(next.keySet))
        val delta = CommitDelta(upserts, removes)
        for {
          prevTrie <- CommittedCommitment.buildTrie[IO](prev)
          applied  <- CommittedCommitment.applyDelta[IO](prevTrie, delta)
          rebuilt  <- CommittedCommitment.buildTrie[IO](next)
        } yield
          expect.all(
            applied == rebuilt,
            applied.rootNode.digest == rebuilt.rootNode.digest
          )
    }
  }
}
