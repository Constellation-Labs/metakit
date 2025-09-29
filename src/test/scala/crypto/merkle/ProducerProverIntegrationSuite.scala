package crypto.merkle

import java.nio.file.{Files, Path}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.MerkleNode
import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.{MerkleProducer, MerkleProver, MerkleVerifier}
import io.constellationnetwork.metagraph_sdk.crypto.merkle.impl.{CollectionMerkleProver, LevelDbMerkleProducer}

import io.circe.syntax._
import org.scalacheck.Gen
import weaver.MutableIOSuite
import weaver.scalacheck.Checkers

object ProducerProverIntegrationSuite extends MutableIOSuite with Checkers {
  type Res = Path

  def sharedResource: Resource[IO, Res] =
    Resource.make(
      IO(Files.createTempDirectory("integration-test"))
    )(path =>
      IO {
        Files
          .walk(path)
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(Files.delete(_))
      }
    )

  private def cleanDb(path: Path): IO[Unit] = IO {
    if (Files.exists(path)) {
      Files
        .walk(path)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(Files.delete(_))
    }
  }

  test("InMemoryProducer integrates with proof generation") { _ =>
    forall(Gen.choose(3, 10)) { leafCount =>
      for {
        leaves   <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](i.asJson))
        producer <- MerkleProducer.inMemory[IO](leaves)

        treeResult <- producer.build
        tree = treeResult.toOption.get

        prover = MerkleProver.make[IO](tree)
        targetIdx = leafCount / 2
        proof <- prover.attestIndex(targetIdx)

        verifier = MerkleVerifier.make[IO](tree.rootNode.digest)
        verifyResult <- verifier.confirm(proof.toOption.get)
      } yield expect(treeResult.isRight) && expect(proof.isRight) && expect(verifyResult)
    }
  }

  test("LevelDbProducer.getProver provides working prover") { tempPath =>
    forall(Gen.listOfN(4, Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"test1_$randSuffix")
        _      <- cleanDb(dbPath)
        leaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        result <- LevelDbMerkleProducer.make[IO](dbPath, leaves).use { producer =>
          for {
            prover <- producer.getProver
            proof  <- prover.attestIndex(1)
            tree   <- producer.build
            rootHash = tree.toOption.get.rootNode.digest
            verifier = MerkleVerifier.make[IO](rootHash)
            verifyResult <- verifier.confirm(proof.toOption.get)
          } yield expect(proof.isRight) && expect(tree.isRight) && expect(verifyResult)
        }
      } yield result
    }
  }

  test("Producer updates affect prover proofs") { tempPath =>
    val genUpdate = for {
      initialStrings <- Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))
      updateString   <- Gen.alphaStr.suchThat(s => s.nonEmpty && s != initialStrings(1))
    } yield (initialStrings, updateString)

    forall(genUpdate) { case (initialStrings, updateString) =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"test2_$randSuffix")
        _             <- cleanDb(dbPath)
        initialLeaves <- initialStrings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        updatedLeaf   <- MerkleNode.Leaf[IO](updateString.asJson)
        result <- LevelDbMerkleProducer.make[IO](dbPath, initialLeaves).use { producer =>
          for {
            prover1 <- producer.getProver
            proof1  <- prover1.attestIndex(0)
            tree1   <- producer.build
            rootHash1 = tree1.toOption.get.rootNode.digest

            _ <- producer.update(1, updatedLeaf)

            prover2 <- producer.getProver
            proof2  <- prover2.attestIndex(0)
            tree2   <- producer.build
            rootHash2 = tree2.toOption.get.rootNode.digest

            verifier1 = MerkleVerifier.make[IO](rootHash1)
            verifier2 = MerkleVerifier.make[IO](rootHash2)
            verify1 <- verifier1.confirm(proof1.toOption.get)

            verify2 <- verifier2.confirm(proof2.toOption.get)

            crossVerify <- verifier2.confirm(proof1.toOption.get)
          } yield expect(rootHash1 != rootHash2) &&
          expect(proof1 != proof2) &&
          expect(verify1) &&
          expect(verify2) &&
          expect(!crossVerify)
        }
      } yield result
    }
  }

  test("CollectionMerkleProver works with LevelDbProducer storage") { tempPath =>
    forall(Gen.choose(5, 10)) { leafCount =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"test3_$randSuffix")
        _      <- cleanDb(dbPath)
        leaves <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](s"leaf-$i".asJson))
        result <- LevelDbMerkleProducer.make[IO](dbPath, leaves).use { producer =>
          val collectionProver = CollectionMerkleProver.make[IO](producer.leavesStore)
          val targetIdx = leafCount / 2

          for {
            tree <- producer.build
            rootHash = tree.toOption.get.rootNode.digest

            producerProver  <- producer.getProver
            producerProof   <- producerProver.attestIndex(targetIdx)
            collectionProof <- collectionProver.attestIndex(targetIdx)

            verifier = MerkleVerifier.make[IO](rootHash)
            verifyProducer   <- verifier.confirm(producerProof.toOption.get)
            verifyCollection <- verifier.confirm(collectionProof.toOption.get)
          } yield expect(producerProof.isRight) &&
          expect(collectionProof.isRight) &&
          expect(producerProof == collectionProof) &&
          expect(verifyProducer) &&
          expect(verifyCollection)
        }
      } yield result
    }
  }

  test("Producer handles append and prepend with prover integration") { tempPath =>
    val genAppendPrepend = for {
      middle <- Gen.alphaNumStr.suchThat(_.nonEmpty)
      first  <- Gen.alphaNumStr.suchThat(_.nonEmpty)
      last   <- Gen.alphaNumStr.suchThat(_.nonEmpty)
    } yield (first, middle, last)

    forall(genAppendPrepend) { case (first, middle, last) =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"test4_$randSuffix")
        _             <- cleanDb(dbPath)
        initialLeaves <- List(middle).traverse(s => MerkleNode.Leaf[IO](s.asJson))
        firstLeaf     <- MerkleNode.Leaf[IO](first.asJson)
        lastLeaf      <- MerkleNode.Leaf[IO](last.asJson)
        result <- LevelDbMerkleProducer.make[IO](dbPath, initialLeaves).use { producer =>
          for {
            _ <- producer.prepend(List(firstLeaf))
            _ <- producer.append(List(lastLeaf))

            prover <- producer.getProver
            tree   <- producer.build
            rootHash = tree.toOption.get.rootNode.digest

            leaves <- producer.leaves
            proofs <- (0 to 2).toList.traverse(prover.attestIndex)

            verifier = MerkleVerifier.make[IO](rootHash)
            verifications <- proofs.traverse { proof =>
              verifier.confirm(proof.toOption.get)
            }
          } yield expect(leaves.map(_.data) == List(first, middle, last).map(_.asJson)) &&
          expect(proofs.forall(_.isRight)) &&
          expect(verifications.forall(identity))
        }
      } yield result
    }
  }

  test("Producer handles remove with prover integration") { tempPath =>
    forall(Gen.choose(8, 12)) { leafCount =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"test5_$randSuffix")
        _      <- cleanDb(dbPath)
        leaves <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](i.asJson))
        removeIdx1 = leafCount / 3
        removeIdx2 = leafCount / 2
        result <- LevelDbMerkleProducer.make[IO](dbPath, leaves).use { producer =>
          for {
            _ <- producer.remove(removeIdx1)
            _ <- producer.remove(removeIdx2 - 1)

            prover <- producer.getProver
            tree   <- producer.build
            rootHash = tree.toOption.get.rootNode.digest

            currentLeaves <- producer.leaves
            expectedSize = leafCount - 2
            proofs <- (0 until expectedSize).toList.traverse(prover.attestIndex)

            verifier = MerkleVerifier.make[IO](rootHash)
            verifications <- proofs.traverse { proof =>
              verifier.confirm(proof.toOption.get)
            }
          } yield expect(currentLeaves.size == expectedSize) &&
          expect(proofs.forall(_.isRight)) &&
          expect(verifications.forall(identity))
        }
      } yield result
    }
  }

  test("Producer caching improves performance for repeated prover access") { tempPath =>
    forall(Gen.choose(15, 25)) { leafCount =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"test6_$randSuffix")
        _      <- cleanDb(dbPath)
        leaves <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](i.asJson))
        result <- LevelDbMerkleProducer.make[IO](dbPath, leaves).use { producer =>
          for {
            _ <- producer.build

            prover1 <- producer.getProver
            prover2 <- producer.getProver
            prover3 <- producer.getProver

            targetIdx = leafCount / 4
            proof1 <- prover1.attestIndex(targetIdx)
            proof2 <- prover2.attestIndex(targetIdx)
            proof3 <- prover3.attestIndex(targetIdx)
          } yield expect(proof1.isRight) &&
          expect(proof1 == proof2) &&
          expect(proof2 == proof3)
        }
      } yield result
    }
  }

  test("Different producer types generate compatible proofs") { tempPath =>
    forall(Gen.listOfN(4, Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"test7_$randSuffix")
        _                <- cleanDb(dbPath)
        leaves           <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        inMemoryProducer <- MerkleProducer.inMemory[IO](leaves)

        levelDbTree <- LevelDbMerkleProducer.make[IO](dbPath, leaves).use { producer =>
          producer.build
        }

        inMemoryTree <- inMemoryProducer.build
      } yield expect(inMemoryTree.isRight) &&
      expect(levelDbTree.isRight) &&
      expect(inMemoryTree.toOption.get.rootNode.digest == levelDbTree.toOption.get.rootNode.digest)
    }
  }

  test("MerkleProducer.levelDb and loadLevelDb convenience methods work correctly") { tempPath =>
    forall(Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        dbPath = tempPath.resolve(s"test8_$randSuffix")
        _      <- cleanDb(dbPath)
        leaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))

        // Create using MerkleProducer.levelDb
        tree1 <- MerkleProducer.levelDb[IO](dbPath, leaves).use { producer =>
          for {
            _             <- producer.append(leaves) // Double the leaves
            tree          <- producer.build
            currentLeaves <- producer.leaves
          } yield (tree, currentLeaves.size)
        }

        // Load using MerkleProducer.loadLevelDb
        tree2 <- MerkleProducer.loadLevelDb[IO](dbPath).use { producer =>
          for {
            loadedLeaves <- producer.leaves
            tree         <- producer.build
          } yield (tree, loadedLeaves.size)
        }

      } yield expect(tree1._1.isRight) &&
      expect(tree2._1.isRight) &&
      expect(tree1._2 == strings.size * 2) &&
      expect(tree2._2 == strings.size * 2) &&
      expect(tree1._1.toOption.get.rootNode.digest == tree2._1.toOption.get.rootNode.digest)
    }
  }
}
