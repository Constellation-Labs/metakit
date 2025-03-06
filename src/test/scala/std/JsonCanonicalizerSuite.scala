package std

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId

import org.tessellation.security.hex.Hex

import io.constellationnetwork.metagraph_sdk.std.JsonCanonicalizer.JsonPrinterEncodeOps

import io.circe.{Json, parser}
import shared.Generators._
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object JsonCanonicalizerSuite extends SimpleIOSuite with Checkers {

  private val InputDir = "src/test/resources/input"
  private val OutputDir = "src/test/resources/output"

  private def readFile(path: String): IO[Json] =
    IO.fromEither(parser.parse(new String(Files.readAllBytes(Paths.get(path)), "UTF-8")))

  private def runTest(filename: String): IO[Expectations] =
    for {
      inputJson         <- readFile(s"$InputDir/$filename")
      expectedJson      <- readFile(s"$OutputDir/$filename")
      actualCanonical   <- inputJson.toCanonical.map(_.value)
      expectedCanonical <- expectedJson.noSpaces.pure[F]
    } yield expect.same(expectedCanonical, actualCanonical)

  test("arrays.json should be canonicalized correctly") {
    runTest("arrays.json")
  }

  test("french.json should be canonicalized correctly") {
    runTest("french.json")
  }

  test("structures.json should be canonicalized correctly") {
    runTest("structures.json")
  }

  test("values.json should be canonicalized correctly") {
    runTest("values.json")
  }

  test("weird.json should be canonicalized correctly") {
    def readHexFile(path: String): IO[Array[Byte]] =
      IO {
        val hexStr = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8)
        hexStr.replaceAll("\\s+", "").grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
      }

    for {
      inputJson     <- readFile(s"$InputDir/weird.json")
      expectedBytes <- readHexFile(s"$OutputDir/weird.hex").map(Hex.fromBytes(_))
      actualBytes   <- inputJson.toCanonical.map(_.value.getBytes(StandardCharsets.UTF_8)).map(Hex.fromBytes(_))
    } yield expect.same(expectedBytes, actualBytes)
  }

  test("canonicalization should be idempotent") {
    forall(complexJsonGen) { json =>
      for {
        firstPass        <- json.toCanonical
        intermediateJson <- IO.fromEither(parser.parse(firstPass.value))
        secondPass       <- intermediateJson.toCanonical
      } yield expect.same(firstPass, secondPass)
    }
  }

  test("canonicalization should preserve JSON equality") {
    forall(complexJsonGen) { json =>
      for {
        canonicalized <- json.toCanonical
        actualJson    <- IO.fromEither(parser.parse(canonicalized.value))
      } yield expect.same(json, actualJson)
    }
  }

  test("canonicalization should produce deterministic output") {
    forall(complexJsonGen) { json =>
      for {
        result1 <- json.toCanonical
        result2 <- json.toCanonical
      } yield expect.same(result1, result2)
    }
  }
}
