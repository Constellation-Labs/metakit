package crypto.mpt

import java.io.IOException
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes

import cats.effect.{IO, Resource}
import cats.syntax.all._

import weaver._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaVerifier
import io.constellationnetwork.metagraph_sdk.crypto.mpt.impl.{
  InMemoryMerklePatriciaProducer,
  LevelDbMerklePatriciaProducer
}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash

import io.circe.Json
import io.circe.syntax._

object ProducerProverIntegrationSuite extends SimpleIOSuite {

  def testData: IO[Map[Hash, Json]] =
    List("key1", "key2", "key3").zipWithIndex.traverse { case (key, idx) =>
      key.computeDigest.map(hash => hash -> s"value${idx + 1}".asJson)
    }.map(_.toMap)

  def tempDbPath: Resource[IO, Path] =
    Resource.make(
      IO(Files.createTempDirectory("mpt-integration-test"))
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

  test("InMemory producer with prover integration") { _ =>
    testData.flatMap { data =>
      for {
        producer <- InMemoryMerklePatriciaProducer.make[IO](data)

        // Build trie
        _ <- producer.build

        // Get prover from producer
        prover <- producer.getProver

        // Generate proofs for all keys
        proofs <- data.keys.toList.traverse { key =>
          prover.attestDigest(key).map(key -> _)
        }

        // Verify all proofs with the trie root
        trieResult <- producer.build
        trie <- IO.fromEither(trieResult)
        verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
        verifyResults <- proofs.traverse {
          case (key, Right(proof)) =>
            verifier.confirm(proof).map(r => key -> r.isRight)
          case (key, Left(err)) =>
            IO.pure(key -> false)
        }

        // All proofs should verify
        _ <- expect(verifyResults.forall(_._2)).pure[IO]

        // Test non-existing key
        nonExistingHash <- "non_existing".computeDigest
        nonExistingProof <- prover.attestDigest(nonExistingHash)
        _ <- expect(nonExistingProof.isLeft).pure[IO]
      } yield success
    }
  }

  test("LevelDb producer with prover integration") { _ =>
    testData.flatMap { data =>
      tempDbPath.use { dbPath =>
        LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            // Add more data
            key4Hash <- "key4".computeDigest
            _ <- producer.insert(Map(key4Hash -> "value4".asJson))

            // Build trie
            trieResult <- producer.build
            trie <- IO.fromEither(trieResult)

            // Get prover from producer
            prover <- producer.getProver

            // Generate and verify proof
            proof <- prover.attestDigest(key4Hash)
            _ <- expect(proof.isRight).pure[IO]

            verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
            verifyResult <- proof.traverse(p => verifier.confirm(p))
            _ <- expect(verifyResult.isRight).pure[IO]
            _ <- verifyResult.traverse(r => expect(r.isRight).pure[IO])
          } yield success
        }
      }
    }
  }

  test("Prover reuses cached trie efficiently") { _ =>
    testData.flatMap { data =>
      tempDbPath.use { dbPath =>
        LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
          for {
            // Build initial trie
            trieResult <- producer.build
            trie <- IO.fromEither(trieResult)

            // Get prover - should use cached trie
            prover <- producer.getProver

            // Generate multiple proofs - all should use same cached trie
            key1Hash <- "key1".computeDigest
            key2Hash <- "key2".computeDigest
            key3Hash <- "key3".computeDigest

            proof1 <- prover.attestDigest(key1Hash)
            proof2 <- prover.attestDigest(key2Hash)
            proof3 <- prover.attestDigest(key3Hash)

            _ <- expect(proof1.isRight && proof2.isRight && proof3.isRight).pure[IO]

            // All proofs should verify with same root hash
            verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
            result1 <- proof1.traverse(p => verifier.confirm(p))
            result2 <- proof2.traverse(p => verifier.confirm(p))
            result3 <- proof3.traverse(p => verifier.confirm(p))
            _ <- expect(result1.isRight && result2.isRight && result3.isRight).pure[IO]
            _ <- result1.traverse(r => expect(r.isRight).pure[IO])
            _ <- result2.traverse(r => expect(r.isRight).pure[IO])
            _ <- result3.traverse(r => expect(r.isRight).pure[IO])
          } yield success
        }
      }
    }
  }

  test("Producer state consistency after operations") { _ =>
    testData.flatMap { data =>
      for {
        producer <- InMemoryMerklePatriciaProducer.make[IO]()

        // Insert initial data
        _ <- producer.insert(data)
        trie1 <- producer.build
        root1 = trie1.map(_.rootNode.digest)

        // Update existing entry
        key1Hash <- "key1".computeDigest
        _ <- producer.update(key1Hash, "updated".asJson)
        trie2 <- producer.build
        root2 = trie2.map(_.rootNode.digest)

        // Roots should differ after update
        _ <- expect(root1 != root2).pure[IO]

        // Remove entry
        key2Hash <- "key2".computeDigest
        _ <- producer.remove(List(key2Hash))
        trie3 <- producer.build
        root3 = trie3.map(_.rootNode.digest)

        // Root should change again
        _ <- expect(root2 != root3).pure[IO]

        // Clear and rebuild
        _ <- producer.clear
        _ <- producer.insert(data)
        trie4 <- producer.build
        root4 = trie4.map(_.rootNode.digest)

        // Should match original root after clear and same insert
        _ <- expect(root1 == root4).pure[IO]
      } yield success
    }
  }

  test("Persistence across restarts") { _ =>
    testData.flatMap { data =>
      tempDbPath.use { dbPath =>
        for {
          session1Hash <- "session1".computeDigest

          // First session - create and populate
          firstSession <- LevelDbMerklePatriciaProducer.make[IO](dbPath, data).use { producer =>
            for {
              _ <- producer.insert(Map(session1Hash -> "data1".asJson))
              trieResult <- producer.build
              trie <- IO.fromEither(trieResult)
              prover <- producer.getProver
              proof <- prover.attestDigest(session1Hash)
            } yield (proof, trie.rootNode.digest)
          }

          // Second session - load and verify
          secondSession <- LevelDbMerklePatriciaProducer.load[IO](dbPath).use { producer =>
            for {
              entries <- producer.entries
              _ <- expect(entries.contains(session1Hash)).pure[IO]

              trieResult <- producer.build
              trie <- IO.fromEither(trieResult)
              prover <- producer.getProver
              proof <- prover.attestDigest(session1Hash)
            } yield (proof, trie.rootNode.digest)
          }

          (proof1, root1) = firstSession
          (proof2, root2) = secondSession

          // Both sessions should produce valid proofs
          _ <- expect(proof1.isRight && proof2.isRight).pure[IO]

          // Roots should match (same data)
          _ <- expect(root1 == root2).pure[IO]

          // Both proofs should verify
          verifier = MerklePatriciaVerifier.make[IO](root1)
          result1 <- proof1.traverse(p => verifier.confirm(p))
          result2 <- proof2.traverse(p => verifier.confirm(p))
          _ <- expect(result1.isRight && result2.isRight).pure[IO]
        } yield success
      }
    }
  }
}