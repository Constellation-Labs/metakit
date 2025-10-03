package crypto.merkle

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.MerkleNode
import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.{InvalidIndex, TreeBuildError}
import io.constellationnetwork.metagraph_sdk.crypto.merkle.impl.LevelDbMerkleProducer

import io.circe.syntax._
import org.scalacheck.Gen
import weaver.IOSuite
import weaver.scalacheck.Checkers

object LevelDbMerkleProducerSuite extends IOSuite with Checkers {

  override def maxParallelism = 1

  override type Res = Path

  override def sharedResource: Resource[IO, Res] =
    for {
      randSuffix <- Resource.eval(IO(scala.util.Random.alphanumeric.take(10).mkString))
      tmpDbPath  <- Resource.make(IO(Files.createTempDirectory(s"leveldb_merkle_${randSuffix}")))(deleteRecursively)
    } yield tmpDbPath

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

  test("creates producer with initial leaves") { tempPath =>
    forall(Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        initialLeaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test1_$randSuffix"), initialLeaves).use { producer =>
          for {
            leaves <- producer.leaves
          } yield expect(leaves.size == 3) && expect(leaves == initialLeaves)
        }
      } yield result
    }
  }

  test("persists leaves across sessions") { tempPath =>
    val genPersist = for {
      initialStrings <- Gen.listOfN(2, Gen.alphaStr.suchThat(_.nonEmpty))
      appendString   <- Gen.alphaStr.suchThat(_.nonEmpty)
    } yield (initialStrings, appendString)

    forall(genPersist) {
      case (initialStrings, appendString) =>
        for {
          randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
          dbPath = tempPath.resolve(s"test2_$randSuffix")
          initialLeaves <- initialStrings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          thirdLeaf     <- MerkleNode.Leaf[IO](appendString.asJson)

          _ <- LevelDbMerkleProducer.make[IO](dbPath, initialLeaves).use { producer =>
            for {
              _      <- producer.append(List(thirdLeaf))
              leaves <- producer.leaves
            } yield expect(leaves.size == 3)
          }

          result <- LevelDbMerkleProducer.load[IO](dbPath).use { producer =>
            for {
              leaves <- producer.leaves
            } yield
              expect(leaves.size == 3) &&
              expect(leaves.map(_.data) == (initialStrings :+ appendString).map(_.asJson))
          }
        } yield result
    }
  }

  test("builds tree correctly") { tempPath =>
    forall(Gen.choose(1, 10)) { leafCount =>
      for {
        randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        initialLeaves <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](i.asJson))
        result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test3_$randSuffix"), initialLeaves).use { producer =>
          for {
            result <- producer.build
          } yield expect(result.isRight) && expect(result.toOption.get.leafDigestIndex.size == leafCount)
        }
      } yield result
    }
  }

  test("updates leaf at index") { tempPath =>
    val genInput = for {
      size         <- Gen.choose(2, 10)
      strings      <- Gen.listOfN(size, Gen.alphaStr.suchThat(_.nonEmpty))
      updateIdx    <- Gen.choose(0, size - 1)
      updateString <- Gen.alphaStr.suchThat(_.nonEmpty).suchThat(_ != strings(updateIdx))
    } yield (strings, updateIdx, updateString)

    forall(genInput) {
      case (strings, updateIdx, updateString) =>
        for {
          randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
          initialLeaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          updatedLeaf   <- MerkleNode.Leaf[IO](updateString.asJson)
          result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test4_$randSuffix"), initialLeaves).use { producer =>
            for {
              updateResult <- producer.update(updateIdx, updatedLeaf)
              leaves       <- producer.leaves
            } yield expect(updateResult.isRight) && expect(leaves(updateIdx).data == updateString.asJson)
          }
        } yield result
    }
  }

  test("returns error for invalid index on update") { tempPath =>
    val genInvalidUpdate = for {
      size         <- Gen.choose(1, 5)
      strings      <- Gen.listOfN(size, Gen.alphaStr.suchThat(_.nonEmpty))
      invalidIdx   <- Gen.choose(size + 1, size + 10)
      updateString <- Gen.alphaStr.suchThat(_.nonEmpty)
    } yield (strings, invalidIdx, updateString)

    forall(genInvalidUpdate) {
      case (strings, invalidIdx, updateString) =>
        for {
          randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
          initialLeaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          xLeaf         <- MerkleNode.Leaf[IO](updateString.asJson)
          result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test5_$randSuffix"), initialLeaves).use { producer =>
            for {
              updateResult <- producer.update(invalidIdx, xLeaf)
            } yield
              expect(updateResult.isLeft) &&
              expect(updateResult.left.toOption.get.isInstanceOf[InvalidIndex])
          }
        } yield result
    }
  }

  test("appends new leaves") { tempPath =>
    val genAppend = for {
      initialSize    <- Gen.choose(1, 5)
      appendSize     <- Gen.choose(1, 5)
      initialStrings <- Gen.listOfN(initialSize, Gen.alphaStr.suchThat(_.nonEmpty))
      appendStrings  <- Gen.listOfN(appendSize, Gen.alphaStr.suchThat(_.nonEmpty))
    } yield (initialStrings, appendStrings)

    forall(genAppend) {
      case (initialStrings, appendStrings) =>
        for {
          randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
          initialLeaves <- initialStrings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          newLeaves     <- appendStrings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test6_$randSuffix"), initialLeaves).use { producer =>
            for {
              _      <- producer.append(newLeaves)
              leaves <- producer.leaves
            } yield
              expect(leaves.size == initialStrings.size + appendStrings.size) &&
              expect(leaves.map(_.data) == (initialStrings ++ appendStrings).map(_.asJson))
          }
        } yield result
    }
  }

  test("prepends new leaves") { tempPath =>
    val genPrepend = for {
      initialSize    <- Gen.choose(1, 5)
      prependSize    <- Gen.choose(1, 5)
      initialStrings <- Gen.listOfN(initialSize, Gen.alphaStr.suchThat(_.nonEmpty))
      prependStrings <- Gen.listOfN(prependSize, Gen.alphaStr.suchThat(_.nonEmpty))
    } yield (initialStrings, prependStrings)

    forall(genPrepend) {
      case (initialStrings, prependStrings) =>
        for {
          randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
          initialLeaves <- initialStrings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          newLeaves     <- prependStrings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test7_$randSuffix"), initialLeaves).use { producer =>
            for {
              _      <- producer.prepend(newLeaves)
              leaves <- producer.leaves
            } yield
              expect(leaves.size == initialStrings.size + prependStrings.size) &&
              expect(leaves.map(_.data) == (prependStrings ++ initialStrings).map(_.asJson))
          }
        } yield result
    }
  }

  test("removes leaf at index") { tempPath =>
    val genRemove = for {
      size      <- Gen.choose(2, 10)
      strings   <- Gen.listOfN(size, Gen.alphaStr.suchThat(_.nonEmpty))
      removeIdx <- Gen.choose(0, size - 1)
    } yield (strings, removeIdx)

    forall(genRemove) {
      case (strings, removeIdx) =>
        for {
          randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
          initialLeaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          expectedData = strings.patch(removeIdx, Nil, 1)
          result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test8_$randSuffix"), initialLeaves).use { producer =>
            for {
              removeResult <- producer.remove(removeIdx)
              leaves       <- producer.leaves
            } yield
              expect(removeResult.isRight) &&
              expect(leaves.size == strings.size - 1) &&
              expect(leaves.map(_.data) == expectedData.map(_.asJson))
          }
        } yield result
    }
  }

  test("returns error for invalid index on remove") { tempPath =>
    val genInvalidRemove = for {
      size       <- Gen.choose(1, 5)
      strings    <- Gen.listOfN(size, Gen.alphaStr.suchThat(_.nonEmpty))
      invalidIdx <- Gen.choose(size + 1, size + 10)
    } yield (strings, invalidIdx)

    forall(genInvalidRemove) {
      case (strings, invalidIdx) =>
        for {
          randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
          initialLeaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test9_$randSuffix"), initialLeaves).use { producer =>
            for {
              removeResult <- producer.remove(invalidIdx)
            } yield
              expect(removeResult.isLeft) &&
              expect(removeResult.left.toOption.get.isInstanceOf[InvalidIndex])
          }
        } yield result
    }
  }

  test("handles empty producer build error") { tempPath =>
    for {
      randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
      result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test10_$randSuffix"), List.empty).use { producer =>
        for {
          result <- producer.build
        } yield
          expect(result.isLeft) &&
          expect(result.left.toOption.get.isInstanceOf[TreeBuildError])
      }
    } yield result
  }

  test("getProver returns cached tree when available") { tempPath =>
    forall(Gen.choose(2, 5)) { leafCount =>
      for {
        randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        initialLeaves <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](i.asJson))
        result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test11_$randSuffix"), initialLeaves).use { producer =>
          for {
            _       <- producer.build
            prover1 <- producer.getProver
            prover2 <- producer.getProver

            proof1 <- prover1.attestIndex(0)
            proof2 <- prover2.attestIndex(0)
          } yield
            expect(proof1.isRight) &&
            expect(proof2.isRight) &&
            expect(proof1 == proof2)
        }
      } yield result
    }
  }

  test("getProver rebuilds tree when dirty") { tempPath =>
    val genUpdate = for {
      size         <- Gen.choose(2, 5)
      strings      <- Gen.listOfN(size, Gen.alphaStr.suchThat(_.nonEmpty))
      updateIdx    <- Gen.choose(0, size - 1)
      updateString <- Gen.alphaStr.suchThat(_.nonEmpty).suchThat(_ != strings(updateIdx))
    } yield (strings, updateIdx, updateString)

    forall(genUpdate) {
      case (strings, updateIdx, updateString) =>
        for {
          randSuffix    <- IO(scala.util.Random.alphanumeric.take(10).mkString)
          initialLeaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
          updatedLeaf   <- MerkleNode.Leaf[IO](updateString.asJson)
          result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test12_$randSuffix"), initialLeaves).use { producer =>
            for {
              prover1 <- producer.getProver
              proof1  <- prover1.attestIndex(0)

              _ <- producer.update(updateIdx, updatedLeaf)

              prover2 <- producer.getProver
              proof2  <- prover2.attestIndex(0)
            } yield
              expect(proof1.isRight) &&
              expect(proof2.isRight) &&
              expect(proof1 != proof2)
          }
        } yield result
    }
  }

  test("batch operations are efficient") { tempPath =>
    forall(Gen.choose(50, 100)) { leafCount =>
      for {
        randSuffix <- IO(scala.util.Random.alphanumeric.take(10).mkString)
        manyLeaves <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](s"leaf-$i".asJson))
        removeIdx = leafCount / 2
        result <- LevelDbMerkleProducer.make[IO](tempPath.resolve(s"test13_$randSuffix"), manyLeaves).use { producer =>
          for {
            _      <- producer.remove(removeIdx)
            leaves <- producer.leaves
          } yield
            expect(leaves.size == leafCount - 1) &&
            expect(!leaves.exists(_.data.asString.exists(_.contains(s"leaf-${removeIdx + 1}")))) &&
            expect(leaves(removeIdx).data.asString.exists(_.contains(s"leaf-${removeIdx + 2}")))
        }
      } yield result
    }
  }
}
