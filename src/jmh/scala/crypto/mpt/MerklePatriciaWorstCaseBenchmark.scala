package crypto.mpt

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaProver
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class MerklePatriciaWorstCaseBenchmark {

  type F[A] = IO[A]

  @Param(Array("1000", "10000"))
  var numEntries: Int = _

  private var entriesWithCommonPrefix: List[(Hash, String)] = _
  private var entriesRandom: List[(Hash, String)] = _
  private var trieCommonPrefix: MerklePatriciaTrie = _
  private var trieRandom: MerklePatriciaTrie = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    // Worst case: all keys share a long common prefix (forces deep tree)
    val commonPrefixProgram: F[(List[(Hash, String)], MerklePatriciaTrie)] = for {
      entries <- (1 to numEntries).toList.traverse { i =>
        // Keys with very long common prefix
        val key = s"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa_${"%010d".format(i)}"
        JsonBinaryHasher[F].computeDigest(key).map(_ -> s"value_$i")
      }
      trie <- MerklePatriciaTrie.make[F, String](entries.toMap)
    } yield (entries, trie)

    // Best/average case: random keys (balanced tree)
    val randomProgram: F[(List[(Hash, String)], MerklePatriciaTrie)] = for {
      entries <- (1 to numEntries).toList.traverse { i =>
        JsonBinaryHasher[F].computeDigest(s"entry_$i").map(_ -> s"value_$i")
      }
      trie <- MerklePatriciaTrie.make[F, String](entries.toMap)
    } yield (entries, trie)

    val (commonEntries, commonTrie) = commonPrefixProgram.unsafeRunSync()
    val (randomEntries, randomTrie) = randomProgram.unsafeRunSync()

    entriesWithCommonPrefix = commonEntries
    entriesRandom = randomEntries
    trieCommonPrefix = commonTrie
    trieRandom = randomTrie
  }

  @Benchmark
  def createTrieWorstCase(bh: Blackhole): Unit = {
    val result = MerklePatriciaTrie.make[F, String](entriesWithCommonPrefix.toMap).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def createTrieRandomCase(bh: Blackhole): Unit = {
    val result = MerklePatriciaTrie.make[F, String](entriesRandom.toMap).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def proofGenerationWorstCase(bh: Blackhole): Unit = {
    val prover = MerklePatriciaProver.make[F](trieCommonPrefix)
    val sampleIndices = Random.shuffle((0 until numEntries).toList).take(10)

    val proofs = sampleIndices.traverse { idx =>
      val (digest, _) = entriesWithCommonPrefix(idx)
      prover.attestDigest(digest).flatMap(IO.fromEither(_))
    }.unsafeRunSync()

    bh.consume(proofs)
  }

  @Benchmark
  def proofGenerationRandomCase(bh: Blackhole): Unit = {
    val prover = MerklePatriciaProver.make[F](trieRandom)
    val sampleIndices = Random.shuffle((0 until numEntries).toList).take(10)

    val proofs = sampleIndices.traverse { idx =>
      val (digest, _) = entriesRandom(idx)
      prover.attestDigest(digest).flatMap(IO.fromEither(_))
    }.unsafeRunSync()

    bh.consume(proofs)
  }

  @Benchmark
  def measureTreeDepth(bh: Blackhole): Unit = {
    // Approximate tree depth by measuring proof sizes
    val proverCommon = MerklePatriciaProver.make[F](trieCommonPrefix)
    val proverRandom = MerklePatriciaProver.make[F](trieRandom)

    val commonProofSize = {
      val (digest, _) = entriesWithCommonPrefix.head
      proverCommon.attestDigest(digest).flatMap(IO.fromEither(_)).unsafeRunSync().witness.size
    }

    val randomProofSize = {
      val (digest, _) = entriesRandom.head
      proverRandom.attestDigest(digest).flatMap(IO.fromEither(_)).unsafeRunSync().witness.size
    }

    bh.consume((commonProofSize, randomProofSize))
  }
}