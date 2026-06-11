package std

import java.nio.file.{Files, Paths}

import cats.effect.{Clock, IO}

import scala.concurrent.duration.DurationInt

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonBinaryHasher}
import io.constellationnetwork.security.hash.Hash

import io.circe.{Decoder, Encoder, Json, parser}
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

  // arrays.json contains an object field `"10": null`. The codec drops null OBJECT fields BEFORE
  // canonicalizing (schema-evolution-safe — lets you add optional fields without changing the hash of
  // prior data; matches ottochain-sdk `drop-nulls.ts` and released metakit 1.7.0's null-dropping). The
  // canonical form is therefore [56,{"1":[],"d":true}] (the null `"10"` removed, keys sorted).
  test("arrays.json should produce a known hash") {
    runTest("arrays.json", "060ba9d4be65e7b773f67328b6fd6a5360f8f66ef88d57351dbc6e39b46f2ea9")
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
      hashTwice(testData).map {
        case (hash1, hash2) =>
          expect(hash1 == hash2)
      }
    }
  }

  test("hashing should be deterministic for complex data") {
    forall { (testData: TestDataComplex) =>
      hashTwice(testData).map {
        case (hash1, hash2) =>
          expect(hash1 == hash2)
      }
    }
  }

  test("different data should produce different hashes") {
    forall { (data1: TestData, data2: TestData) =>
      (for {
        hash1 <- data1.computeDigest
        hash2 <- data2.computeDigest
      } yield (hash1, hash2)).map {
        case (hash1, hash2) =>
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
    hashTwice(emptyData).map {
      case (hash1, hash2) =>
        expect(hash1 == hash2) &&
        expect(hash1.toString.nonEmpty)
    }
  }

  test("hash should handle special characters") {
    val specialData = TestData("!@#$%^&*()", 123)
    hashTwice(specialData).map {
      case (hash1, hash2) =>
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
      hashTwice(testData).map {
        case (hash1, hash2) =>
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
        composedOnceHash <- testData.computeDigest.map(f.andThen(g))
      } yield
        expect(normalHash == mappedHash) &&
        expect(composedHash == composedOnceHash)
    }
  }

  test("hash computation should complete within reasonable time") {
    forall { (testData: TestDataComplex) =>
      Clock[IO].timed(testData.computeDigest).map {
        case (duration, _) =>
          expect(duration < 1.second)
      }
    }
  }

  test("hash computation should not throw exceptions") {
    forall { (testData: TestDataComplex) =>
      testData.computeDigest.attempt.map(result => expect(result.isRight))
    }
  }

  // --- Content-hash rule: dropNulls before RFC 8785 (docs/content-hash.md) ---

  test("explicit-null object fields hash identically to absent fields") {
    for {
      withNulls    <- IO.fromEither(parser.parse("""{"a":1,"b":null,"c":{"d":null,"e":2},"f":[1,null,3]}"""))
      withoutNulls <- IO.fromEither(parser.parse("""{"a":1,"c":{"e":2},"f":[1,null,3]}"""))
      hashWith     <- withNulls.computeDigest
      hashWithout  <- withoutNulls.computeDigest
    } yield expect.same(hashWithout, hashWith)
  }

  test("absent ≡ null holds through Option fields (None vs decoded-from-absent)") {
    // The sender omits the optional field entirely; the receiver decodes to None
    // (encoded back as null by circe). Both must hash to the SAME digest.
    val withNone = TestDataComplex("test", 42, None)
    for {
      digestNone <- withNone.computeDigest
      // hash the circe encoding WITH its explicit null, as raw Json
      jsonWithNull <- IO.pure(
        Json.obj(
          "id"     -> Json.fromString("test"),
          "value"  -> Json.fromInt(42),
          "nested" -> Json.Null
        )
      )
      // and the same object with the field absent
      jsonAbsent     <- IO.pure(Json.obj("id" -> Json.fromString("test"), "value" -> Json.fromInt(42)))
      digestWithNull <- jsonWithNull.computeDigest
      digestAbsent   <- jsonAbsent.computeDigest
    } yield expect.same(digestAbsent, digestWithNull) && expect.same(digestAbsent, digestNone)
  }

  test("array nulls are PRESERVED (removing one changes the hash)") {
    for {
      withArrayNull <- IO.fromEither(parser.parse("""{"xs":[1,null,3]}"""))
      withoutNull   <- IO.fromEither(parser.parse("""{"xs":[1,3]}"""))
      hash1         <- withArrayNull.computeDigest
      hash2         <- withoutNull.computeDigest
    } yield expect(hash1 != hash2)
  }

  test("nulls nested inside array elements' objects are still dropped") {
    for {
      a     <- IO.fromEither(parser.parse("""{"xs":[{"k":1,"opt":null},null]}"""))
      b     <- IO.fromEither(parser.parse("""{"xs":[{"k":1},null]}"""))
      hashA <- a.computeDigest
      hashB <- b.computeDigest
    } yield expect.same(hashB, hashA)
  }
}
