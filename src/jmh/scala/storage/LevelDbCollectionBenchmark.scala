package storage

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.storage.Collection
import io.constellationnetwork.metagraph_sdk.storage.impl.LevelDbCollection

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(value = 1, jvmArgs = Array("-Xms2G", "-Xmx2G"))
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class LevelDbCollectionBenchmark {

  type F[A] = IO[A]

  @Param(Array("100", "1000", "10000", "100000"))
  var numEntries: Int = _

  @Param(Array("10", "50", "100", "500"))
  var batchSize: Int = _

  private var tempDir: Path = _
  private var collection: Collection[F, String, TestData] = _
  private var releaseDb: F[Unit] = _
  private var testData: List[(String, TestData)] = _
  private var randomKeys: List[String] = _
  private var randomBatches: List[List[(String, TestData)]] = _

  case class TestData(id: Int, value: String, timestamp: Long)

  object TestData {
    import io.circe.{Decoder, Encoder}
    import io.circe.generic.semiauto._

    implicit val encoder: Encoder[TestData] = deriveEncoder[TestData]
    implicit val decoder: Decoder[TestData] = deriveDecoder[TestData]
  }

  @Setup(Level.Trial)
  def setup(): Unit = {
    tempDir = Files.createTempDirectory("leveldb-benchmark")

    testData = (1 to numEntries).toList.map { i =>
      val key = f"key_$i%08d"
      val data = TestData(i, s"value_$i", System.currentTimeMillis())
      (key, data)
    }

    randomKeys = Random.shuffle(testData.map(_._1)).take(batchSize)

    randomBatches = testData
      .grouped(batchSize)
      .take(10)
      .toList

    val dbResource = LevelDbCollection.make[F, String, TestData](
      path = tempDir,
      compactionInterval = 1.hour
    )

    val (coll, release) = dbResource.allocated.unsafeRunSync()
    collection = coll
    releaseDb = release

    val _ = testData.traverse { case (key, value) =>
      collection.put(key, value)
    }.unsafeRunSync()
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = {
    releaseDb.unsafeRunSync()

    def deleteRecursively(path: Path): Unit = {
      if (Files.isDirectory(path)) {
        Files.list(path).forEach(deleteRecursively)
      }
      val _ = Files.deleteIfExists(path)
    }
    deleteRecursively(tempDir)
  }

  @Benchmark
  def singlePut(bh: Blackhole): Unit = {
    val key = s"bench_${Random.nextInt()}"
    val value = TestData(Random.nextInt(), s"bench_value_${Random.nextInt()}", System.currentTimeMillis())
    val result = collection.put(key, value).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def singleGet(bh: Blackhole): Unit = {
    val key = randomKeys(Random.nextInt(randomKeys.length))
    val result = collection.get(key).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def singleContains(bh: Blackhole): Unit = {
    val key = randomKeys(Random.nextInt(randomKeys.length))
    val result = collection.contains(key).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def singleRemove(bh: Blackhole): Unit = {
    val key = s"temp_${Random.nextInt()}"
    val value = TestData(Random.nextInt(), "temp_value", System.currentTimeMillis())
    val result = (for {
      _ <- collection.put(key, value)
      _ <- collection.remove(key)
    } yield ()).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def batchPut(bh: Blackhole): Unit = {
    val batch = randomBatches(Random.nextInt(randomBatches.length))
    val result = collection.putBatch(batch).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def batchGet(bh: Blackhole): Unit = {
    val keys = randomKeys.take(batchSize)
    val result = collection.getBatch(keys).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def batchRemove(bh: Blackhole): Unit = {
    val batch = (1 to batchSize).toList.map { i =>
      val key = s"temp_batch_${Random.nextInt()}_$i"
      val value = TestData(i, s"temp_value_$i", System.currentTimeMillis())
      (key, value)
    }
    val result = (for {
      _ <- collection.putBatch(batch)
      _ <- collection.removeBatch(batch.map(_._1))
    } yield ()).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def getWithFilter(bh: Blackhole): Unit = {
    val threshold = numEntries / 2
    val result = collection.getWithFilter { (_, value) =>
      value.id > threshold
    }.unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def sequentialReadWrite(bh: Blackhole): Unit = {
    val key = s"seq_${Random.nextInt()}"
    val value = TestData(Random.nextInt(), s"seq_value_${Random.nextInt()}", System.currentTimeMillis())
    val result = (for {
      _ <- collection.put(key, value)
      retrieved <- collection.get(key)
      _ <- collection.remove(key)
    } yield retrieved).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def mixedOperations(bh: Blackhole): Unit = {
    val key1 = randomKeys(Random.nextInt(randomKeys.length))
    val key2 = s"mixed_${Random.nextInt()}"
    val value = TestData(Random.nextInt(), "mixed_value", System.currentTimeMillis())

    val result = (for {
      exists <- collection.contains(key1)
      _ <- collection.put(key2, value)
      retrieved <- collection.get(key1)
      _ <- collection.remove(key2)
    } yield (exists, retrieved)).unsafeRunSync()
    bh.consume(result)
  }

  @Benchmark
  def dump(bh: Blackhole): Unit = {
    val result = collection.dump.unsafeRunSync()
    bh.consume(result)
  }
}