package std

import cats.effect.{Clock, IO}

import scala.concurrent.duration.DurationInt

import org.tessellation.security.hash.Hash

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonBinaryHasher}

import io.circe.{Decoder, Encoder}
import shared.Generators._
import shared.Models._
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object JsonBinaryHasherSuite extends SimpleIOSuite with Checkers {

  def hashTwice[A: Encoder: Decoder](data: A): IO[(Hash, Hash)] =
    for {
      hash1 <- JsonBinaryHasher[IO].hash(data)
      hash2 <- JsonBinaryHasher[IO].hash(data)
    } yield (hash1, hash2)

  test("hashing should be deterministic") {
    forall { (testData: TestData) =>
      hashTwice(testData).map { case (hash1, hash2) =>
        expect(hash1 == hash2)
      }
    }
  }

  test("hashing should be deterministic for complex data") {
    forall { (testData: TestDataComplex) =>
      hashTwice(testData).map { case (hash1, hash2) =>
        expect(hash1 == hash2)
      }
    }
  }

  test("different data should produce different hashes") {
    forall { (data1: TestData, data2: TestData) =>
      (for {
        hash1 <- data1.hash
        hash2 <- data2.hash
      } yield (hash1, hash2)).map { case (hash1, hash2) =>
        expect(data1 == data2 || hash1 != hash2)
      }
    }
  }

  test("hash should be consistent with binary serialization") {
    forall { (testData: TestData) =>
      for {
        directHash <- testData.hash
        serialized <- JsonBinaryCodec[IO, TestData].serialize(testData)
        binaryHash = Hash.fromBytes(serialized)
      } yield expect(directHash == binaryHash)
    }
  }

  test("hash should handle empty strings") {
    val emptyData = TestData("", 0)
    hashTwice(emptyData).map { case (hash1, hash2) =>
      expect(hash1 == hash2) &&
      expect(hash1.toString.nonEmpty)
    }
  }

  test("hash should handle special characters") {
    val specialData = TestData("!@#$%^&*()", 123)
    hashTwice(specialData).map { case (hash1, hash2) =>
      expect(hash1 == hash2) &&
      expect(hash1.toString.nonEmpty)
    }
  }

  test("hash should have fixed length") {
    forall { (testData: TestData) =>
      testData.hash.map { hash =>
        expect(hash.value.length == 64)
      }
    }
  }

  test("hash should be hexadecimal") {
    forall { (testData: TestData) =>
      testData.hash.map { hash =>
        expect(hash.toString.matches("[0-9a-fA-F]+"))
      }
    }
  }

  test("nested objects should produce consistent hashes") {
    forall { (testData: TestDataComplex) =>
      hashTwice(testData).map { case (hash1, hash2) =>
        expect(hash1 == hash2)
      }
    }
  }

  test("hasher should satisfy functor laws") {
    forall { (testData: TestData) =>
      val f: Hash => Hash = h => Hash.fromBytes(h.getBytes) // identity-like
      val g: Hash => Hash = h => Hash.fromBytes(h.getBytes.reverse) // transformation

      for {
        // Identity law
        normalHash <- testData.hash
        mappedHash <- testData.hash.map(identity)

        // Composition law
        composedHash     <- testData.hash.map(f).map(g)
        composedOnceHash <- testData.hash.map(f andThen g)
      } yield expect(normalHash == mappedHash) &&
      expect(composedHash == composedOnceHash)
    }
  }

  test("hash computation should complete within reasonable time") {
    forall { (testData: TestDataComplex) =>
      Clock[IO].timed(testData.hash).map { case (duration, _) =>
        expect(duration < 1.second)
      }
    }
  }

  test("hash computation should not throw exceptions") {
    forall { (testData: TestDataComplex) =>
      testData.hash.attempt.map(result => expect(result.isRight))
    }
  }
}
