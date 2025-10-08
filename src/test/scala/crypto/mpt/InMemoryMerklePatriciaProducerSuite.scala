package crypto.mpt

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.impl.InMemoryMerklePatriciaProducer
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hex.Hex

import io.circe.Json
import io.circe.syntax._
import weaver._

object InMemoryMerklePatriciaProducerSuite extends SimpleIOSuite {

  def testData: IO[Map[Hex, Json]] =
    List("key1", "key2", "key3").zipWithIndex.traverse {
      case (key, idx) =>
        key.computeDigest.map(hash => Hex(hash.value) -> s"value${idx + 1}".asJson)
    }
      .map(_.toMap)

  test("create empty producer and insert data") {
    for {
      data     <- testData
      producer <- InMemoryMerklePatriciaProducer.make[IO]()

      // Initially empty
      entries1 <- producer.entries
      _        <- expect(entries1.isEmpty).pure[IO]

      // Insert data
      insertResult <- producer.insert(data)
      _            <- expect(insertResult.isRight).pure[IO]

      // Check entries
      entries2 <- producer.entries
      _        <- expect(entries2 == data).pure[IO]

      // Build trie
      buildResult <- producer.build
      _           <- expect(buildResult.isRight).pure[IO]
    } yield success
  }

  test("create producer with initial data") {
    for {
      data     <- testData
      producer <- InMemoryMerklePatriciaProducer.make[IO](data)

      // Check initial entries
      entries <- producer.entries
      _       <- expect(entries == data).pure[IO]

      // Build trie
      buildResult <- producer.build
      _           <- expect(buildResult.isRight).pure[IO]
    } yield success
  }

  test("update existing entry") {
    for {
      data     <- testData
      producer <- InMemoryMerklePatriciaProducer.make[IO](data)

      // Update existing key
      key1Hex      <- "key1".computeDigest.map(hash => Hex(hash.value))
      updateResult <- producer.update(key1Hex, "updated_value1".asJson)
      _            <- expect(updateResult.isRight).pure[IO]

      // Check updated value
      entries <- producer.entries
      _       <- expect(entries(key1Hex) == "updated_value1".asJson).pure[IO]
    } yield success
  }

  test("update non-existing entry should fail") {
    for {
      data     <- testData
      producer <- InMemoryMerklePatriciaProducer.make[IO](data)

      // Try to update non-existing key
      nonExistingHash <- "non_existing".computeDigest.map(hash => Hex(hash.value))
      updateResult    <- producer.update(nonExistingHash, "value".asJson)
      _               <- expect(updateResult.isLeft).pure[IO]
    } yield success
  }

  test("remove existing entries") {
    for {
      data     <- testData
      producer <- InMemoryMerklePatriciaProducer.make[IO](data)

      // Remove some keys
      key1Hash     <- "key1".computeDigest.map(hash => Hex(hash.value))
      key2Hash     <- "key2".computeDigest.map(hash => Hex(hash.value))
      key3Hash     <- "key3".computeDigest.map(hash => Hex(hash.value))
      removeResult <- producer.remove(List(key1Hash, key3Hash))
      _            <- expect(removeResult.isRight).pure[IO]

      // Check remaining entries
      entries <- producer.entries
      _       <- expect(entries.size == 1).pure[IO]
      _       <- expect(entries.contains(key2Hash)).pure[IO]
    } yield success
  }

  test("clear all entries") {
    for {
      data     <- testData
      producer <- InMemoryMerklePatriciaProducer.make[IO](data)

      // Clear all
      _ <- producer.clear

      // Check entries are empty
      entries <- producer.entries
      _       <- expect(entries.isEmpty).pure[IO]
    } yield success
  }

  test("batch insert and remove") {
    for {
      producer <- InMemoryMerklePatriciaProducer.make[IO]()

      // Batch insert
      batch1Hash <- "batch1".computeDigest.map(hash => Hex(hash.value))
      batch2Hash <- "batch2".computeDigest.map(hash => Hex(hash.value))
      batch3Hash <- "batch3".computeDigest.map(hash => Hex(hash.value))
      batchData = Map(
        batch1Hash -> "value1".asJson,
        batch2Hash -> "value2".asJson,
        batch3Hash -> "value3".asJson
      )
      insertResult <- producer.insert(batchData)
      _            <- expect(insertResult.isRight).pure[IO]

      entries1 <- producer.entries
      _        <- expect(entries1.size == 3).pure[IO]

      // Batch remove
      removeResult <- producer.remove(List(batch1Hash, batch3Hash))
      _            <- expect(removeResult.isRight).pure[IO]

      entries2 <- producer.entries
      _        <- expect(entries2.size == 1).pure[IO]
      _        <- expect(entries2.contains(batch2Hash)).pure[IO]
    } yield success
  }

  test("build caches trie when clean") {
    for {
      data     <- testData
      producer <- InMemoryMerklePatriciaProducer.make[IO](data)

      // Build first time
      trie1 <- producer.build
      _     <- expect(trie1.isRight).pure[IO]

      // Build again without changes - should use cache
      trie2 <- producer.build
      _     <- expect(trie2.isRight).pure[IO]
      _     <- expect(trie1 == trie2).pure[IO]

      // Insert new data
      key4Hash <- "key4".computeDigest.map(hash => Hex(hash.value))
      _        <- producer.insert(Map(key4Hash -> "value4".asJson))

      // Build after change - should rebuild
      trie3 <- producer.build
      _     <- expect(trie3.isRight).pure[IO]
      _     <- expect(trie1 != trie3).pure[IO]
    } yield success
  }

  test("getProver returns working prover") {
    for {
      data     <- testData
      producer <- InMemoryMerklePatriciaProducer.make[IO](data)

      // Get prover
      prover <- producer.getProver

      // Test attestation
      key1Hash <- "key1".computeDigest.map(hash => Hex(hash.value))
      proof    <- prover.attestPath(key1Hash)
      _        <- expect(proof.isRight).pure[IO]
    } yield success
  }
}
