package crypto.merkle

import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.{MerkleProducer, MerkleProver, MerkleVerifier}
import io.constellationnetwork.metagraph_sdk.crypto.merkle.impl.LevelDbMerkleProducer
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}

import io.circe.syntax._
import org.scalacheck.Gen
import weaver.IOSuite
import weaver.scalacheck.Checkers

/**
 * Property-based tests verifying merkle operations work correctly with arbitrary-length arrays
 */
object MerkleArbitraryLengthSuite extends IOSuite with Checkers {

  override def maxParallelism = 1

  override type Res = Path

  override def sharedResource: Resource[IO, Res] =
    Resource.make(
      IO(Files.createTempDirectory("merkle-arbitrary-test"))
    )(path =>
      IO {
        if (Files.exists(path)) {
          Files
            .walk(path)
            .sorted(java.util.Comparator.reverseOrder())
            .forEach(Files.delete(_))
        }
      }
    )

  // Generator for various sized lists
  val sizeGen: Gen[Int] = Gen.frequency(
    (60, Gen.choose(1, 100)), // 60% small
    (30, Gen.choose(100, 500)), // 30% medium
    (10, Gen.choose(500, 1000)) // 10% large
  )

  test("MerkleTree handles arbitrary sized inputs") {
    forall(sizeGen) { size =>
      for {
        data <- IO((1 to size).toList.map(i => s"item-$i"))
        tree <- MerkleTree.create[IO, String](data)
      } yield expect(tree.leafDigestIndex.size == size)
    }
  }

  test("InMemoryProducer handles arbitrary operations") { _ =>
    val genOps = for {
      initialSize <- Gen.choose(1, 100)
      updateCount <- Gen.choose(0, Math.min(10, initialSize))
      removeCount <- Gen.choose(0, Math.min(5, initialSize / 4))
    } yield (initialSize, updateCount, removeCount)

    forall(genOps) {
      case (initialSize, updateCount, removeCount) =>
        for {
          initialLeaves <- (1 to initialSize).toList.traverse(i => MerkleNode.Leaf[IO](s"initial-$i".asJson))
          producer      <- MerkleProducer.inMemory[IO](initialLeaves)

          // Update some indices
          _ <- (0 until updateCount).toList.traverse { i =>
            MerkleNode.Leaf[IO](s"updated-$i".asJson).flatMap(leaf => producer.update(i % initialSize, leaf))
          }

          // Remove some indices from the start
          _ <- (0 until removeCount).toList.traverse { _ =>
            producer.leaves.flatMap { currentLeaves =>
              if (currentLeaves.nonEmpty) producer.remove(0)
              else IO.unit
            }
          }

          // Build and verify
          treeResult <- producer.build
          leaves     <- producer.leaves
          finalSize = initialSize - removeCount
        } yield
          expect(treeResult.isRight || finalSize == 0) &&
          expect(leaves.size == finalSize)
    }
  }

  test("LevelDbProducer handles arbitrary sized inputs") { tempPath =>
    forall(Gen.choose(10, 500)) { size =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"arbitrary_$randSuffix")
        leaves <- (1 to size).toList.traverse(i => MerkleNode.Leaf[IO](i.asJson))
        result <- LevelDbMerkleProducer.make[IO](dbPath, leaves).use { producer =>
          for {
            storedLeaves <- producer.leaves
            treeResult   <- producer.build
          } yield
            expect(storedLeaves.size == size) &&
            expect(treeResult.isRight)
        }
      } yield result
    }
  }

  test("MerkleProver and MerkleVerifier work with arbitrary trees") {
    forall(sizeGen) { size =>
      for {
        data <- IO((1 to size).toList.map(i => s"data-$i"))
        tree <- MerkleTree.create[IO, String](data)
        prover = MerkleProver.make[IO](tree)
        verifier = MerkleVerifier.make[IO](tree.rootNode.digest)

        // Test proof at middle index
        idx = size / 2
        proof <- prover.attestIndex(idx)
        verified <- proof match {
          case Right(p) => verifier.confirm(p)
          case Left(_)  => IO.pure(false)
        }
      } yield expect(verified)
    }
  }
}
