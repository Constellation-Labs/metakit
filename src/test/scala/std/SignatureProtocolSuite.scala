package std

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.{IO, Resource}
import cats.implicits._

import io.constellationnetwork.metagraph_sdk.crypto.SignatureProtocol
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.{KeyPairGenerator, SecurityProvider}

import shared.Generators._
import shared.Models.TestDataUpdate
import weaver._
import weaver.scalacheck._

object SignatureProtocolSuite extends SimpleIOSuite with Checkers {
  private val securityProviderResource: Resource[IO, SecurityProvider[IO]] = SecurityProvider.forAsync[IO]

  test("SignatureProver should create proofs with valid signatures") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair <- KeyPairGenerator.makeKeyPair[IO]
          hash    <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof   <- SignatureProtocol.prover[IO].attest(hash, keypair)
        } yield expect(proof.id.hex.value.nonEmpty && proof.signature.value.value.nonEmpty)
      }
    }
  }

  test("SignedJsonProducer should create signed objects using JsonBinaryCodec") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair <- KeyPairGenerator.makeKeyPair[IO]
          producer = SignatureProtocol.proveSigned[IO, TestDataUpdate]
          signed <- producer.create(data, keypair)
        } yield
          expect(signed.value == data) &&
          expect(signed.proofs.size == 1) &&
          expect(signed.proofs.head.id.hex.value.nonEmpty) &&
          expect(signed.proofs.head.signature.value.value.nonEmpty)
      }
    }
  }

  test("SignedJsonProducer should create signed objects using custom serializer") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        val customSerializer: TestDataUpdate => IO[Array[Byte]] =
          td => IO.pure(s"${td.id}:${td.value}".getBytes)

        for {
          keypair <- KeyPairGenerator.makeKeyPair[IO]
          producer = SignatureProtocol.customProveSigned[IO, TestDataUpdate](customSerializer)
          signed <- producer.create(data, keypair)
        } yield
          expect(signed.value == data) &&
          expect(signed.proofs.size == 1) &&
          expect(signed.proofs.head.id.hex.value.nonEmpty) &&
          expect(signed.proofs.head.signature.value.value.nonEmpty)
      }
    }
  }

  test("Signature proofs should be verifiable after creation") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair  <- KeyPairGenerator.makeKeyPair[IO]
          hash     <- data.computeDigest(MonadThrow[IO], JsonBinaryCodec[IO, TestDataUpdate])
          proof    <- SignatureProtocol.prover[IO].attest(hash, keypair)
          verified <- SignatureProtocol.verifier[IO].confirm(hash, proof)
        } yield expect(verified)
      }
    }
  }

  test("SignedJsonEvaluator should verify proofs correctly") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair <- KeyPairGenerator.makeKeyPair[IO]
          producer = SignatureProtocol.proveSigned[IO, TestDataUpdate]
          signed <- producer.create(data, keypair)
          evaluator = SignatureProtocol.verifySigned[IO, TestDataUpdate]
          result <- evaluator.inspect(signed)
        } yield
          result match {
            case Right(validProofs) => expect(validProofs.size == 1)
            case Left(_)            => failure("Valid signature was incorrectly rejected")
          }
      }
    }
  }

  test("SignedJsonEvaluator should identify tampered data") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { originalData =>
        for {
          keypair <- KeyPairGenerator.makeKeyPair[IO]
          producer = SignatureProtocol.proveSigned[IO, TestDataUpdate]
          signed <- producer.create(originalData, keypair)
          tamperedData = originalData.copy(value = originalData.value + 1)
          signedTampered = signed.copy(value = tamperedData)
          evaluator = SignatureProtocol.verifySigned[IO, TestDataUpdate]
          result <- evaluator.inspect(signedTampered)
        } yield
          result match {
            case Right(_)            => failure("Tampered data was incorrectly verified")
            case Left(invalidProofs) => expect(invalidProofs.size == 1)
          }
      }
    }
  }

  test("Custom verifier should work with byte serialization") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        val customSerializer: TestDataUpdate => IO[Array[Byte]] = td => IO.pure(s"${td.id}:${td.value}".getBytes)

        for {
          keypair <- KeyPairGenerator.makeKeyPair[IO]
          producer = SignatureProtocol.customProveSigned[IO, TestDataUpdate](customSerializer)
          signed <- producer.create(data, keypair)
          evaluator = SignatureProtocol.customVerifySigned[IO, TestDataUpdate](customSerializer)
          result <- evaluator.inspect(signed)
        } yield
          result match {
            case Right(_) => success
            case Left(_)  => failure("Valid signature was rejected")
          }
      }
    }
  }

  test("Adding signature to an existing signed object") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair1 <- KeyPairGenerator.makeKeyPair[IO]
          keypair2 <- KeyPairGenerator.makeKeyPair[IO]

          producer = SignatureProtocol.proveSigned[IO, TestDataUpdate]
          signed              <- producer.create(data, keypair1)
          signedWithTwoProofs <- producer.addSignature(signed, keypair2)

          evaluator = SignatureProtocol.verifySigned[IO, TestDataUpdate]
          result <- evaluator.inspect(signedWithTwoProofs)
        } yield
          result match {
            case Right(validProofs) =>
              expect(signedWithTwoProofs.proofs.size == 2) &&
              expect(validProofs.size == 2)
            case Left(_) => failure("Valid signatures were incorrectly rejected")
          }
      }
    }
  }

  test("Batch signing should create signed object with multiple valid proofs") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair1 <- KeyPairGenerator.makeKeyPair[IO]
          keypair2 <- KeyPairGenerator.makeKeyPair[IO]
          keypair3 <- KeyPairGenerator.makeKeyPair[IO]

          keypairs = NonEmptyList.of(keypair1, keypair2, keypair3)
          producer = SignatureProtocol.proveSigned[IO, TestDataUpdate]
          signedWithMultipleProofs <- producer.batchSign(data, keypairs)

          evaluator = SignatureProtocol.verifySigned[IO, TestDataUpdate]
          result <- evaluator.inspect(signedWithMultipleProofs)
        } yield
          result match {
            case Right(validProofs) =>
              expect(signedWithMultipleProofs.proofs.size == 3) &&
              expect(validProofs.size == 3)
            case Left(_) => failure("Valid signatures were incorrectly rejected")
          }
      }
    }
  }

  test("Multiple proofs should all be verified correctly") {
    securityProviderResource.use { implicit s =>
      forall(genTestDataUpdate) { data =>
        for {
          keypair1 <- KeyPairGenerator.makeKeyPair[IO]
          keypair2 <- KeyPairGenerator.makeKeyPair[IO]
          keypair3 <- KeyPairGenerator.makeKeyPair[IO]

          producer = SignatureProtocol.proveSigned[IO, TestDataUpdate]
          signed1 <- producer.create(data, keypair1)
          signed2 <- producer.addSignature(signed1, keypair2)
          signed3 <- producer.addSignature(signed2, keypair3)

          evaluator = SignatureProtocol.verifySigned[IO, TestDataUpdate]
          result <- evaluator.inspect(signed3)
        } yield
          result match {
            case Right(validProofs) =>
              expect(validProofs.size == 3)
            case Left(_) => failure("Valid signatures were incorrectly rejected")
          }
      }
    }
  }
}
