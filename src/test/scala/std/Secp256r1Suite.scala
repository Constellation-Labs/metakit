package std

import cats.effect.{IO, Resource}

import io.constellationnetwork.metagraph_sdk.crypto.curves.Secp256r1
import io.constellationnetwork.security.SecurityProvider

import weaver._

object Secp256r1Suite extends SimpleIOSuite {
  private val securityProviderResource: Resource[IO, SecurityProvider[IO]] = SecurityProvider.forAsync[IO]

  test("generateKeyPair should produce a valid EC key pair on secp256r1") {
    securityProviderResource.use { implicit s =>
      for {
        keyPair <- Secp256r1.generateKeyPair[IO]
      } yield
        expect(keyPair.getPublic != null) &&
        expect(keyPair.getPrivate != null) &&
        expect(keyPair.getPublic.getAlgorithm == "EC")
    }
  }

  test("sign and verify should round-trip successfully") {
    securityProviderResource.use { implicit s =>
      val data = "hello world".getBytes("UTF-8")
      for {
        keyPair  <- Secp256r1.generateKeyPair[IO]
        sigBytes <- Secp256r1.sign[IO](data, keyPair.getPrivate)
        result   <- Secp256r1.verify[IO](data, sigBytes, keyPair.getPublic)
      } yield expect(result)
    }
  }

  test("verify should reject tampered data") {
    securityProviderResource.use { implicit s =>
      val data = "hello world".getBytes("UTF-8")
      val tampered = "hello tampered".getBytes("UTF-8")
      for {
        keyPair  <- Secp256r1.generateKeyPair[IO]
        sigBytes <- Secp256r1.sign[IO](data, keyPair.getPrivate)
        result   <- Secp256r1.verify[IO](tampered, sigBytes, keyPair.getPublic)
      } yield expect(!result)
    }
  }

  test("verify should reject wrong public key") {
    securityProviderResource.use { implicit s =>
      val data = "hello world".getBytes("UTF-8")
      for {
        keyPair1 <- Secp256r1.generateKeyPair[IO]
        keyPair2 <- Secp256r1.generateKeyPair[IO]
        sigBytes <- Secp256r1.sign[IO](data, keyPair1.getPrivate)
        result   <- Secp256r1.verify[IO](data, sigBytes, keyPair2.getPublic)
      } yield expect(!result)
    }
  }

  test("publicKeyToId and idToPublicKey should round-trip") {
    securityProviderResource.use { implicit s =>
      for {
        keyPair   <- Secp256r1.generateKeyPair[IO]
        id         = Secp256r1.publicKeyToId(keyPair.getPublic)
        recovered <- Secp256r1.idToPublicKey[IO](id)
      } yield expect(keyPair.getPublic.getEncoded.sameElements(recovered.getEncoded))
    }
  }

  test("publicKeyToId should produce 128-char hex (64-byte raw EC point)") {
    securityProviderResource.use { implicit s =>
      for {
        keyPair <- Secp256r1.generateKeyPair[IO]
        id       = Secp256r1.publicKeyToId(keyPair.getPublic)
      } yield expect(id.hex.value.length == 128, s"Expected 128 hex chars, got ${id.hex.value.length}")
    }
  }

  test("signToSignature should produce a non-empty Signature") {
    securityProviderResource.use { implicit s =>
      val data = "test message".getBytes("UTF-8")
      for {
        keyPair   <- Secp256r1.generateKeyPair[IO]
        signature <- Secp256r1.signToSignature[IO](data, keyPair.getPrivate)
      } yield expect(signature.value.value.nonEmpty)
    }
  }
}
