package storage

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import cats.data.NonEmptyList
import cats.effect.{IO, Resource}

import io.constellationnetwork.metagraph_sdk.storage.Collection
import io.constellationnetwork.metagraph_sdk.storage.impl.LevelDbCollection

import shared.Generators._
import weaver.IOSuite
import weaver.scalacheck.Checkers

object LevelDbCollectionSuite extends IOSuite with Checkers {

  override def maxParallelism = 1

  override type Res = Collection[IO, Int, String]

  override def sharedResource: Resource[IO, Res] =
    for {
      randSuffix <- Resource.eval(IO(scala.util.Random.alphanumeric.take(10).mkString))
      tmpDbFile  <- Resource.make(IO(Files.createTempDirectory(s"leveldb_${randSuffix}")))(deleteRecursively)
      store      <- LevelDbCollection.make[IO, Int, String](tmpDbFile)
    } yield store

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

  test("put stores a value and allows it to be removed successfully") { store =>
    for {
      _      <- store.put(1, "A")
      value1 <- store.get(1)
      _      <- store.remove(1)
      value2 <- store.get(1)
    } yield expect(value1.contains("A") && value2.isEmpty)
  }

  test("contains confirms presence of a value after it is stored") { store =>
    for {
      _        <- store.put(2, "B")
      contains <- store.contains(2)
    } yield expect(contains)
  }

  test("getWithFilter returns only values that match the specified filter") { store =>
    for {
      _      <- store.putBatch(List((3, "C"), (4, "D")))
      values <- store.getWithFilter((_, v) => v == "C")
      expected = List((3, "C"))
    } yield expect(values == expected)
  }

  test("getUnsafe returns a value when it exists") { store =>
    for {
      _     <- store.put(5, "E")
      value <- store.getUnsafe(5)
    } yield expect(value == "E")
  }

  test("getUnsafe throws an exception when the requested value does not exist") { store =>
    for {
      result <- store.getUnsafe(42).attempt
    } yield result match {
      case Left(e: NoSuchElementException) => expect(e.getMessage == s"Element not found. id=42")
      case _                               => failure("Exception not caught")
    }
  }

  test("getBatch retrieves a batch of key-value pairs and filters them correctly") { store =>
    for {
      _        <- store.putBatch(List((6, "F"), (7, "G")))
      values   <- store.getBatch(List(6, 7))
      filtered <- store.getWithFilter((_, v) => v == "F")
    } yield expect(values.toSet == Set((6, Some("F")), (7, Some("G")))) && expect(filtered == List((6, "F")))
  }

  test("putBatch stores key-value pairs and getBatch retrieves them correctly") { store =>
    for {
      kvPairs <- IO.fromOption(kvListGenUniqueKeys(999, 1000).sample.map(NonEmptyList.fromListUnsafe))(
        new RuntimeException("Failed to generate key-value list")
      )
      (keys, _) = kvPairs.toList.unzip
      _      <- store.putBatch(kvPairs.toList)
      actual <- store.getBatch(keys).map(_.flatMap { case (i, maybeStr) => maybeStr.map(str => (i, str)) })
      expected = kvPairs.toList
    } yield expect(actual == expected)
  }

  test("removeBatch removes a batch of keys and confirms they no longer exist") { store =>
    for {
      kvPairs <- IO.fromOption(kvListGenUniqueKeys(999, 2000).sample.map(NonEmptyList.fromListUnsafe))(
        new RuntimeException("Failed to generate key-value list")
      )
      (keys, _) = kvPairs.toList.unzip
      _      <- store.putBatch(kvPairs.toList)
      _      <- store.removeBatch(keys)
      actual <- store.getBatch(keys).map(_.flatMap { case (i, maybeStr) => maybeStr.map(str => (i, str)) })
    } yield expect(actual.isEmpty)
  }
}
