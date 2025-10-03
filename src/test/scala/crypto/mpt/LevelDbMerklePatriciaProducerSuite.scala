package crypto.mpt

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.impl.LevelDbMerklePatriciaProducer
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash

import io.circe.Json
import io.circe.syntax._
import weaver._

object LevelDbMerklePatriciaProducerSuite extends SimpleIOSuite {

  def testData: IO[Map[Hash, Json]] =
    List("key1", "key2", "key3").zipWithIndex.traverse {
      case (key, idx) =>
        key.computeDigest.map(hash => hash -> s"value${idx + 1}".asJson)
    }
      .map(_.toMap)

  def tempDbPath: Resource[IO, Path] =
    Resource.make(
      IO(Files.createTempDirectory("mpt-leveldb-test"))
    )(deleteRecursively)

  private def deleteRecursively(path: Path): IO[Unit] = IO {
    Files.walkFileTree(
      path,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      }
    )
  } *> IO.unit

  test("create empty LevelDB producer and insert data") {
    tempDbPath.use { dbPath =>
      LevelDbMerklePatriciaProducer.make[IO](dbPath).use { producer =>
        for {
          data <- testData
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
    }
  }

  test("create LevelDB producer with initial data") { _ =>
    tempDbPath.use { dbPath =>
      for {
        data <- testData
        result <- LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            // Check initial entries
            entries <- producer.entries
            _       <- expect(entries == data).pure[IO]

            // Build trie
            buildResult <- producer.build
            _           <- expect(buildResult.isRight).pure[IO]
          } yield success
        }
      } yield result
    }
  }

  test("persist data across producer instances") { _ =>
    tempDbPath.use { dbPath =>
      for {
        data     <- testData
        key4Hash <- "key4".computeDigest
        // First producer - insert data
        result1 <- LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            _       <- producer.insert(Map(key4Hash -> "value4".asJson))
            entries <- producer.entries
          } yield entries
        }

        // Second producer - load existing data
        result2 <- LevelDbMerklePatriciaProducer.load[IO](dbPath).use { producer =>
          producer.entries
        }

        _ <- expect(result1 == result2).pure[IO]
        _ <- expect(result2.size == 4).pure[IO]
        _ <- expect(result2.contains(key4Hash)).pure[IO]
      } yield success
    }
  }

  test("load fails on empty database") { _ =>
    tempDbPath.use { dbPath =>
      // Create empty database
      LevelDbMerklePatriciaProducer.make[IO](dbPath).use(_ => IO.unit)

      // Try to load empty database
      LevelDbMerklePatriciaProducer.load[IO](dbPath).use(_ => IO.unit).attempt.map { result =>
        expect(result.isLeft)
      }
    }
  }

  test("update existing entry in LevelDB") { _ =>
    tempDbPath.use { dbPath =>
      for {
        data     <- testData
        key1Hash <- "key1".computeDigest
        result <- LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            // Update existing key
            updateResult <- producer.update(key1Hash, "updated_value1".asJson)
            _            <- expect(updateResult.isRight).pure[IO]

            // Check updated value
            entries <- producer.entries
            _       <- expect(entries(key1Hash) == "updated_value1".asJson).pure[IO]
          } yield success
        }
      } yield result
    }
  }

  test("remove entries from LevelDB") { _ =>
    tempDbPath.use { dbPath =>
      for {
        data     <- testData
        key1Hash <- "key1".computeDigest
        key2Hash <- "key2".computeDigest
        key3Hash <- "key3".computeDigest
        result <- LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            // Remove some keys
            removeResult <- producer.remove(List(key1Hash, key3Hash))
            _            <- expect(removeResult.isRight).pure[IO]

            // Check remaining entries
            entries <- producer.entries
            _       <- expect(entries.size == 1).pure[IO]
            _       <- expect(entries.contains(key2Hash)).pure[IO]
          } yield success
        }
      } yield result
    }
  }

  test("clear all entries in LevelDB") { _ =>
    tempDbPath.use { dbPath =>
      for {
        data <- testData
        result <- LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            // Clear all
            _ <- producer.clear

            // Check entries are empty
            entries <- producer.entries
            _       <- expect(entries.isEmpty).pure[IO]

            // Cannot build empty trie
            buildResult <- producer.build
            _           <- expect(buildResult.isLeft).pure[IO]
          } yield success
        }
      } yield result
    }
  }

  test("atomic batch operations") { _ =>
    tempDbPath.use { dbPath =>
      LevelDbMerklePatriciaProducer.make[IO](dbPath).use { producer =>
        for {
          batch1Hash <- "batch1".computeDigest
          batch2Hash <- "batch2".computeDigest
          batch3Hash <- "batch3".computeDigest
          // Batch insert
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
    }
  }

  test("build caches trie when clean") { _ =>
    tempDbPath.use { dbPath =>
      for {
        data     <- testData
        key4Hash <- "key4".computeDigest
        result <- LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            // Build first time
            trie1 <- producer.build
            _     <- expect(trie1.isRight).pure[IO]

            // Build again without changes - should use cache
            trie2 <- producer.build
            _     <- expect(trie2.isRight).pure[IO]
            _     <- expect(trie1 == trie2).pure[IO]

            // Insert new data
            _ <- producer.insert(Map(key4Hash -> "value4".asJson))

            // Build after change - should rebuild
            trie3 <- producer.build
            _     <- expect(trie3.isRight).pure[IO]
            _     <- expect(trie1 != trie3).pure[IO]
          } yield success
        }
      } yield result
    }
  }

  test("getProver returns working prover") { _ =>
    tempDbPath.use { dbPath =>
      for {
        data     <- testData
        key1Hash <- "key1".computeDigest
        result <- LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            // Get prover (should build trie if needed)
            prover <- producer.getProver

            // Test attestation
            proof <- prover.attestDigest(key1Hash)
            _     <- expect(proof.isRight).pure[IO]
          } yield success
        }
      } yield result
    }
  }
}
