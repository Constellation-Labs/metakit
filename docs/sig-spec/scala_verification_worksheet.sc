import cats.effect.{Async, IO, Resource}
import cats.effect.std.Console
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import cats.{Applicative, FlatMap, MonadThrow}
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
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

@derive(encoder, decoder, show)
case class TestData(id: String, value: Int)

@derive(encoder, decoder, show)
case class TestDataUpdate(id: String, value: Int) extends DataUpdate

sealed trait VerificationResult
case class VerificationSuccess(
                                canonicalMatches: Boolean,
                                hashMatches: Boolean,
                                signatureValid: Boolean
                              ) extends VerificationResult
case class VerificationFailure(error: String) extends VerificationResult

case class TestVector(
                       source: String,
                       testType: String,
                       dataJson: Json,
                       expectedCanonical: String,
                       expectedHashHex: String,
                       signatureHex: String,
                       publicKeyHex: String
                     )

trait FileLoader[F[_]] {
  def findTestVectorsPath(paths: List[String]): F[Option[Path]]
  def loadJson(path: Path): F[Option[Json]]
}

trait SignatureVerifier[F[_]] {
  def verifySignature(hash: Hash, signatureHex: String, publicKeyHex: String): F[Boolean]
}

trait VectorProcessor[F[_]] {
  def parseVector(json: Json): F[TestVector]
  def verifyTestData(data: TestData, expected: TestVector): F[VerificationResult]
  def verifyTestDataUpdate(data: TestDataUpdate, expected: TestVector): F[VerificationResult]
}

object FileLoader {
  def apply[F[_]: Async](files: Files[F]): FileLoader[F] = new FileLoader[F] {
    def findTestVectorsPath(paths: List[String]): F[Option[Path]] =
      paths.traverse { p =>
        files.exists(Path(p)).map(exists => if (exists) Some(Path(p)) else None)
      }.map(_.flatten.headOption)

    def loadJson(path: Path): F[Option[Json]] =
      files.readUtf8(path)
        .compile
        .string
        .map(parser.parse(_).toOption)
        .handleError(_ => None)
  }
}

object SignatureVerifier {
  def apply[F[_]: Async](implicit sp: SecurityProvider[F]): SignatureVerifier[F] =
    new SignatureVerifier[F] {
      def verifySignature(hash: Hash, signatureHex: String, publicKeyHex: String): F[Boolean] = {
        val parseSignatureProof = Async[F].catchNonFatal {
          val publicKeyData = publicKeyHex.substring(2)
          val id = Id(Hex(publicKeyData))
          val signature = Signature(Hex(signatureHex))
          SignatureProof(id, signature)
        }

        parseSignatureProof
          .flatMap(SignatureProtocol.verifier[F].confirm(hash, _))
          .handleError(_ => false)
      }
    }
}

object VectorProcessor {
  def apply[F[_]: MonadThrow](
                               signatureVerifier: SignatureVerifier[F]
                             ): VectorProcessor[F] = new VectorProcessor[F] {

    def parseVector(json: Json): F[TestVector] = MonadThrow[F].fromEither {
      val cursor = json.hcursor
      for {
        source <- cursor.get[String]("source")
        testType <- cursor.get[String]("type")
        dataJson <- cursor.get[Json]("data")
        expectedCanonical <- cursor.get[String]("canonical_json")
        expectedHashHex <- cursor.get[String]("sha256_hash_hex")
        signatureHex <- cursor.get[String]("signature_hex")
        publicKeyHex <- cursor.get[String]("public_key_hex")
      } yield TestVector(
        source, testType, dataJson, expectedCanonical,
        expectedHashHex, signatureHex, publicKeyHex
      )
    }

    def verifyTestData(data: TestData, expected: TestVector): F[VerificationResult] =
      for {
        canonical <- data.toCanonicalString(MonadThrow[F], implicitly)
        hash <- data.computeDigest(MonadThrow[F], JsonBinaryCodec[F, TestData])
        sigValid <- signatureVerifier.verifySignature(hash, expected.signatureHex, expected.publicKeyHex)
      } yield VerificationSuccess(
        canonicalMatches = canonical == expected.expectedCanonical,
        hashMatches = hash.value == expected.expectedHashHex,
        signatureValid = sigValid
      )

    def verifyTestDataUpdate(data: TestDataUpdate, expected: TestVector): F[VerificationResult] =
      for {
        canonical <- data.toCanonicalString(MonadThrow[F], implicitly)
        hash <- data.computeDigest(MonadThrow[F], JsonBinaryCodec[F, TestDataUpdate])
        sigValid <- signatureVerifier.verifySignature(hash, expected.signatureHex, expected.publicKeyHex)
      } yield VerificationSuccess(
        canonicalMatches = canonical == expected.expectedCanonical,
        hashMatches = hash.value == expected.expectedHashHex,
        signatureValid = sigValid
      )
  }
}

class VerificationProgram[F[_]: MonadThrow: Logger](
                                                     fileLoader: FileLoader[F],
                                                     vectorProcessor: VectorProcessor[F]
                                                   ) {

  val testVectorPaths = List(
    "test_vectors.json",
    "docs/sig-spec/test_vectors.json",
    "git/metakit/docs/sig-spec/test_vectors.json",
    "../metakit/docs/sig-spec/test_vectors.json"
  )

  def verifyVector(vector: Json, index: Int): F[Unit] =
    vectorProcessor.parseVector(vector).flatMap { tv =>
      val result = tv.testType match {
        case "TestData" =>
          tv.dataJson.as[TestData].fold(
            err => VerificationFailure(s"Failed to parse TestData: $err").pure[F],
            data => vectorProcessor.verifyTestData(data, tv)
          )
        case "TestDataUpdate" =>
          tv.dataJson.as[TestDataUpdate].fold(
            err => VerificationFailure(s"Failed to parse TestDataUpdate: $err").pure[F],
            data => vectorProcessor.verifyTestDataUpdate(data, tv)
          )
        case other =>
          VerificationFailure(s"Unknown test type: $other").pure[F]
      }

      result.flatMap {
        case VerificationSuccess(canonical, hash, sig) =>
          Logger[F].info(s"Test Vector ${index + 1} (${tv.source} ${tv.testType}):") *>
            Logger[F].info(s"  ✓ Canonical JSON matches: $canonical") *>
            Logger[F].info(s"  ✓ SHA-256 hash matches: $hash") *>
            Logger[F].info(s"  ✓ Signature verified: $sig")
        case VerificationFailure(err) =>
          Logger[F].error(s"✗ Test Vector ${index + 1} failed: $err")
      }
    }

  def run: F[Unit] = for {
    _ <- Logger[F].info("=== Scala: Cross-Language Signature Verification ===")

    maybePath <- fileLoader.findTestVectorsPath(testVectorPaths)

    _ <- maybePath.fold(
      Logger[F].error("Failed to find test_vectors.json in any expected location") *>
        testVectorPaths.traverse_(p => Logger[F].info(s"  - $p"))
    )(path => Logger[F].info(s"Found test_vectors.json at: $path"))

    _ <- maybePath.traverse_ { path =>
      fileLoader.loadJson(path).flatMap {
        case Some(json) =>
          json.asArray.fold(
            Logger[F].error("Failed to parse test vectors as array")
          )(vectors =>
            vectors.zipWithIndex.traverse_ { case (vector, index) =>
              verifyVector(vector, index)
            }
          )
        case None =>
          Logger[F].error("Failed to load test vectors JSON")
      }
    }
  } yield ()
}

val program: Resource[IO, IO[Unit]] =
  SecurityProvider.forAsync[IO].map { implicit sp =>
    implicit val logger: Logger[IO] = Slf4jLogger.getLogger[IO]
    val files: Files[IO] = Files[IO]

    val fileLoader = FileLoader[IO](files)
    val signatureVerifier = SignatureVerifier[IO]
    val vectorProcessor = VectorProcessor[IO](signatureVerifier)

    new VerificationProgram[IO](
      fileLoader,
      vectorProcessor
    ).run
  }

program.use(identity).unsafeRunSync()