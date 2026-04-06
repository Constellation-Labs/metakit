package std

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.implicits._

import io.constellationnetwork.metagraph_sdk.crypto.curves.Secp256r1
import io.constellationnetwork.metagraph_sdk.crypto.{MetagraphSignatureProof, SignatureProtocol, SigningScheme}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.{KeyPairGenerator, SecurityProvider}

import io.circe.syntax._
import shared.Generators._
import shared.Models.TestDataUpdate
import weaver._
import weaver.scalacheck._

object MetagraphSignatureProtocolSuite extends SimpleIOSuite with Checkers {
  private val securityProviderResource: Resource[IO, SecurityProvider[IO]] = SecurityProvider.forAsync[IO]

  test("schemeProver with Secp256k1Rfc8785V1 should create valid proofs") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair <- KeyPairGenerator.makeKeyPair[IO]
          hash    <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof   <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256k1Rfc8785V1).attest(hash, keypair)
        } yield
          expect(proof.scheme === SigningScheme.Secp256k1Rfc8785V1.some) &&
          expect(proof.id.hex.value.nonEmpty) &&
          expect(proof.signature.value.value.nonEmpty)
      }
    }
  }

  test("schemeProver with Secp256r1Rfc8785V1 should create valid proofs") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair <- Secp256r1.generateKeyPair[IO]
          hash    <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof   <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256r1Rfc8785V1).attest(hash, keypair)
        } yield
          expect(proof.scheme === SigningScheme.Secp256r1Rfc8785V1.some) &&
          expect(proof.id.hex.value.nonEmpty) &&
          expect(proof.signature.value.value.nonEmpty)
      }
    }
  }

  test("secp256r1 sign and verify round-trip via schemeProver/schemeVerifier") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair  <- Secp256r1.generateKeyPair[IO]
          hash     <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof    <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256r1Rfc8785V1).attest(hash, keypair)
          verified <- SignatureProtocol.schemeVerifier[IO](SigningScheme.Secp256r1Rfc8785V1).confirm(hash, proof)
        } yield expect(verified)
      }
    }
  }

  test("secp256k1 sign and verify round-trip via schemeProver/schemeVerifier") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair  <- KeyPairGenerator.makeKeyPair[IO]
          hash     <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof    <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256k1Rfc8785V1).attest(hash, keypair)
          verified <- SignatureProtocol.schemeVerifier[IO](SigningScheme.Secp256k1Rfc8785V1).confirm(hash, proof)
        } yield expect(verified)
      }
    }
  }

  test("autoVerifier should verify secp256r1 proofs") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair  <- Secp256r1.generateKeyPair[IO]
          hash     <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof    <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256r1Rfc8785V1).attest(hash, keypair)
          verified <- SignatureProtocol.autoVerifier[IO].confirm(hash, proof)
        } yield expect(verified)
      }
    }
  }

  test("autoVerifier should verify secp256k1 proofs") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair  <- KeyPairGenerator.makeKeyPair[IO]
          hash     <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof    <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256k1Rfc8785V1).attest(hash, keypair)
          verified <- SignatureProtocol.autoVerifier[IO].confirm(hash, proof)
        } yield expect(verified)
      }
    }
  }

  test("autoVerifier should verify proofs with scheme=None (defaults to k1)") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair  <- KeyPairGenerator.makeKeyPair[IO]
          hash     <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof    <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256k1Rfc8785V1).attest(hash, keypair)
          noneProof = proof.copy(scheme = none)
          verified <- SignatureProtocol.autoVerifier[IO].confirm(hash, noneProof)
        } yield expect(verified)
      }
    }
  }

  test("MetagraphSignedProducer with secp256r1 should create valid Signed[A]") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair <- Secp256r1.generateKeyPair[IO]
          producer = SignatureProtocol.proveSignedWithScheme[IO, TestDataUpdate](SigningScheme.Secp256r1Rfc8785V1)
          signed <- producer.create(data, keypair)
        } yield
          expect(signed.value == data) &&
          expect(signed.proofs.size == 1)
      }
    }
  }

  test("MetagraphSignedEvaluator with constant resolver should verify secp256r1 proofs") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        val resolver = SignatureProtocol.constantSchemeResolver[IO](SigningScheme.Secp256r1Rfc8785V1)
        for {
          keypair  <- Secp256r1.generateKeyPair[IO]
          producer  = SignatureProtocol.proveSignedWithScheme[IO, TestDataUpdate](SigningScheme.Secp256r1Rfc8785V1)
          signed   <- producer.create(data, keypair)
          evaluator = SignatureProtocol.verifySignedWithScheme[IO, TestDataUpdate](resolver)
          result   <- evaluator.inspect(signed)
        } yield
          result match {
            case Right(validProofs) => expect(validProofs.size == 1)
            case Left(_)            => failure("Valid secp256r1 signature was incorrectly rejected")
          }
      }
    }
  }

  test("MetagraphSignedEvaluator should detect tampered data with secp256r1") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { originalData =>
        val resolver = SignatureProtocol.constantSchemeResolver[IO](SigningScheme.Secp256r1Rfc8785V1)
        for {
          keypair  <- Secp256r1.generateKeyPair[IO]
          producer  = SignatureProtocol.proveSignedWithScheme[IO, TestDataUpdate](SigningScheme.Secp256r1Rfc8785V1)
          signed   <- producer.create(originalData, keypair)
          tampered  = signed.copy(value = originalData.copy(value = originalData.value + 1))
          evaluator = SignatureProtocol.verifySignedWithScheme[IO, TestDataUpdate](resolver)
          result   <- evaluator.inspect(tampered)
        } yield
          result match {
            case Right(_)            => failure("Tampered data was incorrectly verified")
            case Left(invalidProofs) => expect(invalidProofs.size == 1)
          }
      }
    }
  }

  test("MetagraphSignedProducer batch sign with secp256r1") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        val resolver = SignatureProtocol.constantSchemeResolver[IO](SigningScheme.Secp256r1Rfc8785V1)
        for {
          kp1 <- Secp256r1.generateKeyPair[IO]
          kp2 <- Secp256r1.generateKeyPair[IO]
          kp3 <- Secp256r1.generateKeyPair[IO]

          keypairs = NonEmptyList.of(kp1, kp2, kp3)
          producer = SignatureProtocol.proveSignedWithScheme[IO, TestDataUpdate](SigningScheme.Secp256r1Rfc8785V1)
          signed <- producer.batchSign(data, keypairs)

          evaluator = SignatureProtocol.verifySignedWithScheme[IO, TestDataUpdate](resolver)
          result <- evaluator.inspect(signed)
        } yield
          result match {
            case Right(validProofs) =>
              expect(signed.proofs.size == 3) &&
              expect(validProofs.size == 3)
            case Left(_) => failure("Valid secp256r1 signatures were incorrectly rejected")
          }
      }
    }
  }

  test("createWithProofs should return MetagraphSignatureProof with scheme metadata") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair <- Secp256r1.generateKeyPair[IO]
          producer = SignatureProtocol.proveSignedWithScheme[IO, TestDataUpdate](SigningScheme.Secp256r1Rfc8785V1)
          result  <- producer.createWithProofs(data, keypair)
          (signed, proofs) = result
        } yield
          expect(signed.proofs.size == 1) &&
          expect(proofs.size == 1) &&
          expect(proofs.head.scheme === SigningScheme.Secp256r1Rfc8785V1.some)
      }
    }
  }

  test("MetagraphSignatureProof should convert to/from tessellation SignatureProof") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair <- Secp256r1.generateKeyPair[IO]
          hash    <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof   <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256r1Rfc8785V1).attest(hash, keypair)
          tessProof = proof.toTessellation
          backToMeta = MetagraphSignatureProof.fromTessellation(tessProof)
        } yield
          expect(tessProof.id === proof.id) &&
          expect(tessProof.signature === proof.signature) &&
          expect(backToMeta.scheme.isEmpty)
      }
    }
  }

  test("SigningScheme circe round-trip") {
    IO {
      SigningScheme.all.foldMap { scheme =>
        val json = scheme.asJson
        val decoded = json.as[SigningScheme]
        expect(decoded === Right(scheme))
      }
    }
  }

  test("MetagraphSignatureProof circe codec with scheme present") {
    securityProviderResource.use { implicit s =>
      for {
        keypair <- Secp256r1.generateKeyPair[IO]
        hash    <- "test".getBytes.pure[IO].map(io.constellationnetwork.security.hash.Hash.fromBytes)
        proof   <- SignatureProtocol.schemeProver[IO](SigningScheme.Secp256r1Rfc8785V1).attest(hash, keypair)
        json     = proof.asJson
        decoded  = json.as[MetagraphSignatureProof]
      } yield
        expect(decoded.isRight) &&
        expect(decoded.map(_.scheme) === Right(SigningScheme.Secp256r1Rfc8785V1.some))
    }
  }

  test("MetagraphSignatureProof circe codec with scheme absent decodes as None") {
    securityProviderResource.use { implicit s =>
      for {
        keypair <- KeyPairGenerator.makeKeyPair[IO]
        hash    <- "test".getBytes.pure[IO].map(io.constellationnetwork.security.hash.Hash.fromBytes)
        proof   <- SignatureProtocol.prover[IO].attest(hash, keypair)
        metaProof = MetagraphSignatureProof.fromTessellation(proof)
        json      = metaProof.asJson
        decoded   = json.as[MetagraphSignatureProof]
      } yield
        expect(decoded.isRight) &&
        expect(decoded.map(_.scheme) === Right(none[SigningScheme])) &&
        expect(!json.hcursor.downField("scheme").succeeded, "scheme field should be absent from JSON")
    }
  }
}
