package std

import java.nio.file.{Files, Paths}

import cats.effect.IO

import org.tessellation.security.hex.Hex

import io.constellationnetwork.metagraph_sdk.std.JsonCanonicalizer.JsonPrinterEncodeOps

import io.circe.parser.parse
import io.circe.{parser, Json}
import shared.Generators._
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object JsonCanonicalizerSuite extends SimpleIOSuite with Checkers {

  private val InputDir = "src/test/resources/input"
  private val OutputDir = "src/test/resources/results"

  private def readFile(path: String): IO[Array[Byte]] = IO.blocking {
    val content = new String(Files.readAllBytes(Paths.get(path)), "UTF-8")
    val parsedJson = parser.parse(content).getOrElse(Json.Null)
    parsedJson.noSpaces.getBytes("UTF-8")
  }

  private def runTest(filename: String): IO[Expectations] =
    for {
      rawInput  <- readFile(s"$InputDir/$filename")
      expected  <- readFile(s"$OutputDir/$filename")
      inputJson <- IO.fromEither(parse(new String(rawInput)))
      result    <- inputJson.asCanonicalJson
      hexExpected = Hex.fromBytes(expected)
      hexActual = Hex.fromBytes(result.noSpaces.getBytes("UTF-8"))
    } yield expect.same(hexExpected, hexActual)

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
    runTest("weird.json")
  }

  test("canonicalization should be idempotent") {
    forall(complexJsonGen) { json =>
      (for {
        firstPass  <- json.asCanonicalJson
        secondPass <- firstPass.asCanonicalJson
      } yield expect(firstPass == secondPass))
    }
  }

  test("canonicalization should preserve JSON equality") {
    forall(complexJsonGen) { json =>
      for {
        canonicalized <- json.asCanonicalJson
        parsed        <- IO.fromEither(parse(canonicalized.noSpaces))
      } yield expect(parsed == json)
    }
  }

  test("canonicalization should produce deterministic output") {
    forall(complexJsonGen) { json =>
      for {
        result1 <- json.asCanonicalJson
        result2 <- json.asCanonicalJson
      } yield expect(result1.noSpaces == result2.noSpaces)
    }
  }
}
