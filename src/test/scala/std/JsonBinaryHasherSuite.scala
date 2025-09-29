package std

import java.nio.file.{Files, Paths}

import cats.effect.{Clock, IO}

import scala.concurrent.duration.DurationInt

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonBinaryHasher}
import io.constellationnetwork.security.hash.Hash

import io.circe.{parser, Decoder, Encoder, Json}
import shared.Generators._
import shared.Models._
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object JsonBinaryHasherSuite extends SimpleIOSuite with Checkers {

  private val InputDir = "src/test/resources/input"

  private def readFile(path: String): IO[Json] =
    IO.fromEither(parser.parse(new String(Files.readAllBytes(Paths.get(path)), "UTF-8")))

  private def runTest(filename: String, expected: String): IO[Expectations] =
    for {
      input      <- readFile(s"$InputDir/$filename")
      hashActual <- input.computeDigest
      hashExpected = Hash(expected)
    } yield expect.same(hashExpected, hashActual)

  test("arrays.json should produce a known hash") {
    runTest("arrays.json", "099601b171cafed97c333f8878d68e7f8c8f795412adb34b2fdcf0e7c7beac42")
  }

  test("french.json should produce a known hash") {
    runTest("french.json", "d99d0ebdcb0033cb858cfa830ae46bc0fb3309413b271f1da828c89901a27ed5")
  }

  test("structures.json should produce a known hash") {
    runTest("structures.json", "5ec3c256c54c1d54cdc095c0cabd2950354645b4f9dc938fcc2645e26b48847f")
  }

  test("values.json should produce a known hash") {
    runTest("values.json", "2d5e01a318d0f0879ab568c4be289c8b1f64ef8921a53c6277d5e069978baacb")
  }

  test("weird.json should produce a known hash") {
    runTest("weird.json", "0f539f49254f75f1ab71c4a7b067e0ce38bad1bb5b04d7e06b2dd2a368cec666")
  }

  def hashTwice[A: Encoder: Decoder](data: A): IO[(Hash, Hash)] =
    for {
      hash1 <- JsonBinaryHasher[IO].computeDigest(data)
      hash2 <- JsonBinaryHasher[IO].computeDigest(data)
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
        hash1 <- data1.computeDigest
        hash2 <- data2.computeDigest
      } yield (hash1, hash2)).map { case (hash1, hash2) =>
        expect(data1 == data2 || hash1 != hash2)
      }
    }
  }

  test("hash should be consistent with binary serialization") {
    forall { (testData: TestData) =>
      for {
        directHash <- testData.computeDigest
        serialized <- JsonBinaryCodec[IO, TestData].serialize(testData)
        binaryHash = Hash.fromBytes(serialized)
      } yield expect.same(directHash, binaryHash)
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
      testData.computeDigest.map { hash =>
        expect(hash.value.length == 64)
      }
    }
  }

  test("hash should be hexadecimal") {
    forall { (testData: TestData) =>
      testData.computeDigest.map { hash =>
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
        normalHash <- testData.computeDigest
        mappedHash <- testData.computeDigest.map(identity)

        // Composition law
        composedHash     <- testData.computeDigest.map(f).map(g)
        composedOnceHash <- testData.computeDigest.map(f andThen g)
      } yield expect(normalHash == mappedHash) &&
      expect(composedHash == composedOnceHash)
    }
  }

  test("hash computation should complete within reasonable time") {
    forall { (testData: TestDataComplex) =>
      Clock[IO].timed(testData.computeDigest).map { case (duration, _) =>
        expect(duration < 1.second)
      }
    }
  }

  test("hash computation should not throw exceptions") {
    forall { (testData: TestDataComplex) =>
      testData.computeDigest.attempt.map(result => expect(result.isRight))
    }
  }
}
