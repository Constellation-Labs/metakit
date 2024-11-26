package storage

import cats.data.NonEmptyList
import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.storage.Collection
import io.constellationnetwork.metagraph_sdk.storage.impl.RefMapCollection

import generators.kvListGenUniqueKeys
import weaver._
import weaver.scalacheck.Checkers

object RefMapCollectionSuite extends SimpleIOSuite with Checkers {

  private val storeIO: IO[Collection[IO, Int, String]] = RefMapCollection.make[IO, Int, String]

  test("put stores values, and get retrieves them correctly") {
    forall(kvListGenUniqueKeys(100).map(NonEmptyList.fromListUnsafe)) { kvPairs =>
      for {
        store  <- storeIO
        _      <- kvPairs.traverse(pair => store.put(pair._1, pair._2))
        actual <- kvPairs.traverse(pair => store.get(pair._1))
        expected = kvPairs.map(_._2).toList
      } yield expect(actual.toList.flatten == expected)
    }
  }

  test("getBatch retrieves multiple values correctly") {
    forall(kvListGenUniqueKeys(100).map(NonEmptyList.fromListUnsafe)) { kvPairs =>
      for {
        store  <- storeIO
        _      <- kvPairs.traverse(pair => store.put(pair._1, pair._2))
        actual <- store.getBatch(kvPairs.map(_._1).toList)
        expected = kvPairs.map(pair => (pair._1, Some(pair._2))).toList
      } yield expect(actual == expected)
    }
  }

  test("remove deletes a value successfully") {
    for {
      store <- storeIO
      _     <- store.put(1, "A")
      _     <- store.remove(1)
      value <- store.get(1)
    } yield expect(value.isEmpty)
  }

  test("putBatch stores and removeBatch deletes a batch of values correctly") {
    forall(kvListGenUniqueKeys(100).map(NonEmptyList.fromListUnsafe)) { kvPairs =>
      for {
        store              <- storeIO
        _                  <- store.putBatch(kvPairs.toList)
        valuesBeforeDelete <- store.getBatch(kvPairs.map(_._1).toList)
        _                  <- store.removeBatch(kvPairs.map(_._1).toList)
        valuesAfterDelete  <- store.getBatch(kvPairs.map(_._1).toList)
        expectedBeforeDelete = kvPairs.map(pair => pair._1 -> Some(pair._2)).toList
        expectedAfterDelete = kvPairs.map(pair => pair._1 -> None).toList
      } yield expect(valuesBeforeDelete == expectedBeforeDelete).and(expect(valuesAfterDelete == expectedAfterDelete))
    }
  }

  test("contains confirms presence of a value after storing it") {
    for {
      store    <- storeIO
      _        <- store.put(1, "A")
      contains <- store.contains(1)
    } yield expect(contains)
  }

  test("getWithFilter retrieves only values matching the specified filter") {
    for {
      store  <- storeIO
      _      <- store.put(1, "A")
      _      <- store.put(2, "B")
      values <- store.getWithFilter((_, v) => v == "A")
      expected = List((1, "A"))
    } yield expect(values == expected)
  }

  test("getUnsafe returns a value when it exists") {
    for {
      store <- storeIO
      _     <- store.put(1, "A")
      value <- store.getUnsafe(1)
    } yield expect(value == "A")
  }

  test("getUnsafe throws an exception when the requested value does not exist") {
    for {
      store  <- storeIO
      result <- store.getUnsafe(42).attempt
    } yield result match {
      case Left(e: NoSuchElementException) => expect(e.getMessage == s"Element not found. id=42")
      case _                               => failure("Exception not caught")
    }
  }

  test("dump returns all stored values") {
    for {
      store  <- storeIO
      _      <- store.put(1, "A")
      _      <- store.put(2, "B")
      values <- store.dump
      expected = List((1, "A"), (2, "B"))
    } yield expect(values == expected)
  }
}
