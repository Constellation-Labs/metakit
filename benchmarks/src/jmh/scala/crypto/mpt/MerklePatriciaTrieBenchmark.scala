package crypto.mpt

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaProver, MerklePatriciaVerifier}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.deriveFromCodec
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hex.Hex

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.util.concurrent.TimeUnit
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx8G"))
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class MerklePatriciaTrieBenchmark {

  type F[A] = IO[A]

  @Param(Array("1000", "10000", "100000"))
  var numEntries: Int = _

  @Param(Array("10", "50", "100"))
  var numProofsToVerify: Int = _

  private var entries: List[(Hex, String)] = _
  private var trie: MerklePatriciaTrie = _
  private var verifier: MerklePatriciaVerifier[F] = _
  private var prover: MerklePatriciaProver[F] = _
  private var randomIndices: List[Int] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val program: F[(List[(Hex, String)], MerklePatriciaTrie)] = for {
      entriesList <- (1 to numEntries).toList.traverse { i =>
        JsonBinaryHasher[F].computeDigest(s"entry_$i").map(d => Hex(d.value) -> s"value_$i")
      }
      trieInstance <- MerklePatriciaTrie.make[F, String](entriesList.toMap)
    } yield (entriesList, trieInstance)

    val (entriesList, trieInstance) = program.unsafeRunSync()

    entries = entriesList
    trie = trieInstance
    verifier = MerklePatriciaVerifier.make[F](trie.rootNode.digest)
    prover = MerklePatriciaProver.make[F](trie)
    randomIndices = Random.shuffle((0 until numEntries).toList).take(numProofsToVerify)
  }

  @Benchmark
  def createTrie(bh: Blackhole): Unit = {
    val result = MerklePatriciaTrie.make[F, String](entries.toMap).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def generateProofs(bh: Blackhole): Unit = {
    val proofs = randomIndices.traverse { idx =>
      val (digest, _) = entries(idx)
      prover.attestPath(digest).flatMap(IO.fromEither(_))
    }.unsafeRunSync()
    bh.consume(proofs)
  }

  @Benchmark
  def verifyProofs(bh: Blackhole): Unit = {
    val results = randomIndices.traverse { idx =>
      val (digest, _) = entries(idx)
      for {
        proof <- prover.attestPath(digest).flatMap(IO.fromEither(_))
        result <- verifier.confirm(proof)
      } yield result
    }.unsafeRunSync()
    bh.consume(results)
  }

  @Benchmark
  def endToEnd(bh: Blackhole): Unit = {
    val program: F[List[Either[String, Unit]]] = for {
      newTrie <- MerklePatriciaTrie.make[F, String](entries.toMap)
      newVerifier = MerklePatriciaVerifier.make[F](newTrie.rootNode.digest)
      newProver = MerklePatriciaProver.make[F](newTrie)
      results <- randomIndices.traverse { idx =>
        val (digest, _) = entries(idx)
        for {
          proof <- newProver.attestPath(digest).flatMap(IO.fromEither(_))
          result <- newVerifier.confirm(proof).map(_.leftMap(_.toString))
        } yield result
      }
    } yield results

    val results = program.unsafeRunSync()
    bh.consume(results)
  }

  @Benchmark
  def proofSizeMeasurement(bh: Blackhole): Unit = {
    val proofSizes = randomIndices.map { idx =>
      val (digest, _) = entries(idx)
      val proof = prover.attestPath(digest).flatMap(IO.fromEither(_)).unsafeRunSync()
      proof.witness.size
    }
    bh.consume(proofSizes)
  }

  @Benchmark
  def batchVerifyParallel(bh: Blackhole): Unit = {
    val results = randomIndices.parTraverse { idx =>
      val (digest, _) = entries(idx)
      for {
        proof <- prover.attestPath(digest).flatMap(IO.fromEither(_))
        result <- verifier.confirm(proof)
      } yield result
    }.unsafeRunSync()
    bh.consume(results)
  }

  @Benchmark
  def lookupOnly(bh: Blackhole): Unit = {
    val results = randomIndices.map { idx =>
      val (digest, _) = entries(idx)
      // Simulates pure lookup without proof generation
      trie.rootNode.digest == digest
    }
    bh.consume(results)
  }
}