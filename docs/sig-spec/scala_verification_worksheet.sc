// IntelliJ IDEA Scala Worksheet for cross-language signature verification
// To use: Open in IntelliJ, ensure "Use compile server" is checked in worksheet settings

import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import derevo.cats.show
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import io.circe.{Json, parser}
import io.constellationnetwork.currency.dataApplication.DataUpdate
import io.constellationnetwork.metagraph_sdk.crypto.SignatureProtocol
import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonBinaryHasher}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.metagraph_sdk.std.JsonCanonicalizer.JsonPrinterStringEncodeOps
import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
import fs2.io.file.Path

@derive(encoder, decoder, show)
case class TestData(id: String, value: Int)

@derive(encoder, decoder, show) 
case class TestDataUpdate(id: String, value: Int) extends DataUpdate

// IntelliJ runs from project root, so we need to check if test_vectors.json exists
// First, let's see where we are running from
val currentDir = System.getProperty("user.dir")
println(s"Current working directory: $currentDir")

// Try different path options relative to where IntelliJ might be running
val paths = List(
  "test_vectors.json",
  "docs/sig-spec/test_vectors.json",
  "git/metakit/docs/sig-spec/test_vectors.json",
  "../metakit/docs/sig-spec/test_vectors.json"
)

// Find the first path that exists
val foundPath = paths.find(p => java.nio.file.Files.exists(java.nio.file.Paths.get(p)))

val testVectorsPath = foundPath match {
  case Some(p) =>
    println(s"Found test_vectors.json at: $p")
    Path(p)
  case None =>
    println("Failed to find test_vectors.json in any of the expected locations:")
    paths.foreach(p => println(s"  - $p"))
    println("Defaulting to docs/sig-spec/test_vectors.json")
    Path("docs/sig-spec/test_vectors.json")
}

// Helper functions
def verifySignature(hash: Hash, signatureHex: String, publicKeyHex: String)(implicit sp: SecurityProvider[IO]): IO[Boolean] = {
  val parseSignatureProof: IO[SignatureProof] = IO {
    val publicKeyData = publicKeyHex.substring(2) // Remove "04" prefix
    val id = Id(Hex(publicKeyData))
    val signature = Signature(Hex(signatureHex))
    SignatureProof(id, signature)
  }

  parseSignatureProof
    .flatTap(_ => IO.println("    Python signature components parsed successfully"))
    .flatMap(proof => SignatureProtocol.verifier[IO].confirm(hash, proof))
    .handleErrorWith { e =>
      IO.println(s"    Signature verification failed: ${e.getMessage}").as(false)
    }
}

def loadTestVectors(path: Path): IO[Option[Json]] = IO {
  val javaPath = java.nio.file.Paths.get(path.toString)
  println(s"Looking for test vectors at: ${javaPath.toAbsolutePath}")
  
  if (java.nio.file.Files.exists(javaPath)) {
    val content = new String(java.nio.file.Files.readAllBytes(javaPath))
    parser.parse(content) match {
      case Right(json) => 
        println("✓ Test vectors loaded successfully")
        Some(json)
      case Left(error) =>
        println(s"✗ Failed to parse test_vectors.json: $error")
        None
    }
  } else {
    println("✗ test_vectors.json not found.")
    println("Run python_verification_example.py first to generate test vectors.")
    val parentDir = javaPath.getParent
    if (parentDir != null && java.nio.file.Files.exists(parentDir)) {
      println(s"Files in ${parentDir}:")
      import scala.jdk.CollectionConverters._
      java.nio.file.Files.list(parentDir).iterator().asScala.take(10).foreach { p =>
        println(s"  - ${p.getFileName}")
      }
    }
    None
  }
}

def verifyTestVector(vector: Json, index: Int)(implicit sp: SecurityProvider[IO]): IO[Unit] = {
  val testType = vector.hcursor.get[String]("type").getOrElse("Unknown")
  val dataJson = vector.hcursor.get[Json]("data").getOrElse(Json.Null)
  val expectedCanonical = vector.hcursor.get[String]("canonical_json").getOrElse("")
  val expectedHashHex = vector.hcursor.get[String]("sha256_hash_hex").getOrElse("")
  val signatureHex = vector.hcursor.get[String]("signature_hex").getOrElse("")
  val publicKeyHex = vector.hcursor.get[String]("public_key_hex").getOrElse("")

  for {
    _ <- IO.println(s"Verifying Test Vector ${index + 1} ($testType):")
    _ <- testType match {
      case "TestData" =>
        dataJson.as[TestData] match {
          case Right(testData) =>
            for {
              canonical <- testData.toCanonicalString(cats.MonadThrow[IO], implicitly)
              hash <- testData.computeDigest(cats.MonadThrow[IO], JsonBinaryCodec[IO, TestData])
              pythonSigVerified <- verifySignature(hash, signatureHex, publicKeyHex)
              _ <- IO {
                println(s"  ✓ Canonical JSON matches: ${canonical == expectedCanonical}")
                println(s"  ✓ SHA-256 hash matches: ${hash.value == expectedHashHex}")
                println(s"  ✓ Python signature verified: $pythonSigVerified")
              }
            } yield ()
          case Left(error) =>
            IO.println(s"  ✗ Failed to parse TestData: $error")
        }
        
      case "TestDataUpdate" =>
        dataJson.as[TestDataUpdate] match {
          case Right(testUpdate) =>
            for {
              canonical <- testUpdate.toCanonicalString(cats.MonadThrow[IO], implicitly)
              hash <- testUpdate.computeDigest(cats.MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
              pythonSigVerified <- verifySignature(hash, signatureHex, publicKeyHex)
              _ <- IO {
                println(s"  ✓ Canonical JSON matches: ${canonical == expectedCanonical}")
                println(s"  ✓ SHA-256 hash matches: ${hash.value == expectedHashHex}")
                println(s"  ✓ Python signature verified: $pythonSigVerified")
              }
            } yield ()
          case Left(error) =>
            IO.println(s"  ✗ Failed to parse TestDataUpdate: $error")
        }
        
      case _ =>
        IO.println(s"  ✗ Unknown test type: $testType")
    }
    _ <- IO.println("")
  } yield ()
}

// Main verification program
val verificationProgram: Resource[IO, IO[Unit]] = {
  SecurityProvider.forAsync[IO].map { implicit sp =>
    for {
      _ <- IO.println("=== Scala: Verifying Python Signatures ===\n")
      testVectorsJson <- loadTestVectors(testVectorsPath)
      _ <- testVectorsJson match {
        case Some(json) =>
          json.asArray match {
            case Some(vectors) =>
              vectors.zipWithIndex.traverse_ { case (vector, index) =>
                verifyTestVector(vector, index)
              }
            case None =>
              IO.println("Failed to parse test vectors as array")
          }
        case None =>
          IO.unit
      }
    } yield ()
  }
}

// Execute the program
verificationProgram.use(identity).unsafeRunSync()