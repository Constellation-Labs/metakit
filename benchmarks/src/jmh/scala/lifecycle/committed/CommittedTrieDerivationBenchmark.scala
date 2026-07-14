package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import java.util.concurrent.TimeUnit

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.collection.immutable.{SortedMap, SortedSet}

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

/**
 * The per-ordinal trie costs behind [[CommittedConfig.incrementalTrie]].
 *
 * One accepted ordinal pays, in FULL-REBUILD mode (the default):
 *   - `advanceWork`: 1x [[fullRebuild]]
 *   - `hashFor`: 1x [[fullRebuild]] (per speculative state)
 *   - `transition`: 1x [[incrementalApply]] + 1x [[fullRebuild]] (the divergence assert)
 *
 * and in INCREMENTAL mode: 3x [[incrementalApply]] (each O(churn) in trie work) plus, with the
 * default structural `CommittedView.delta`, 1-3x [[structuralDiff]] (O(state) map compare --
 * collapsing THAT to O(churn) requires an application-supplied delta, e.g. combiner-driven).
 *
 * Run: `sbt "benchmarks/Jmh/run -i 5 -wi 3 -f1 .*CommittedTrieDerivation.*"`
 */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx8G"))
@Warmup(iterations = 3)
@Measurement(iterations = 5)
class CommittedTrieDerivationBenchmark {

  implicit val hasher: JsonBinaryHasher[IO] = JsonBinaryHasher.deriveFromCodec[IO]

  @Param(Array("1000", "10000", "50000"))
  var size: Int = _

  @Param(Array("16", "256"))
  var churn: Int = _

  var prevEntries: SortedMap[CommitKey, Json] = _
  var nextEntries: SortedMap[CommitKey, Json] = _
  var prevTrie: MerklePatriciaTrie = _
  var delta: CommitDelta = _

  private def entry(i: Int, v: Int): (CommitKey, Json) =
    CommitKey.unsafe(f"fiber/k$i%08d") -> Json.obj("count" -> v.asJson)

  @Setup(Level.Trial)
  def setup(): Unit = {
    prevEntries = SortedMap.from((0 until size).map(i => entry(i, i)))
    // churn/2 value-updates on existing keys, churn/2 fresh inserts, churn/4 removes
    val updates = (0 until churn / 2).map(i => entry(i * (size / (churn / 2 + 1)).max(1), -i))
    val inserts = (0 until churn / 2).map(i => entry(size + i, i))
    val removeKeys = (0 until churn / 4).map(i => entry(i * 2 + 1, 0)._1).filter(prevEntries.contains)
    nextEntries = (prevEntries -- removeKeys) ++ updates ++ inserts
    prevTrie = CommittedCommitment.buildTrie[IO](prevEntries).unsafeRunSync()
    val upserts = nextEntries.filter { case (k, v) => !prevEntries.get(k).contains(v) }
    val removes = SortedSet.from(prevEntries.keySet.diff(nextEntries.keySet))
    delta = CommitDelta(upserts, removes)
  }

  /** What every full-rebuild site pays: O(state) trie construction from the entry set. */
  @Benchmark
  def fullRebuild(bh: Blackhole): Unit =
    bh.consume(CommittedCommitment.buildTrie[IO](nextEntries).unsafeRunSync())

  /** What the incremental mode pays per site: O(churn) path rewrites on the persistent trie. */
  @Benchmark
  def incrementalApply(bh: Blackhole): Unit =
    bh.consume(CommittedCommitment.applyDelta[IO](prevTrie, delta).unsafeRunSync())

  /** The default structural diff (CommittedView.delta): the remaining O(state) map compare. */
  @Benchmark
  def structuralDiff(bh: Blackhole): Unit = {
    val upserts = nextEntries.filter { case (k, v) => !prevEntries.get(k).contains(v) }
    val removes = SortedSet.from(prevEntries.keySet.diff(nextEntries.keySet))
    bh.consume(CommitDelta(upserts, removes))
  }
}
