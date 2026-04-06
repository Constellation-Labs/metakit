package io.constellationnetwork.metagraph_sdk.crypto.curves

import java.security.spec.{ECGenParameterSpec, X509EncodedKeySpec}
import java.security.{KeyPairGenerator => JKeyPairGenerator, Signature => JSignature, _}

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.signature.signature.Signature
import io.constellationnetwork.security.{SecureRandom, SecurityProvider}

import io.estatico.newtype.ops._

object Secp256r1 {

  val CurveName: String = "secp256r1"
  val SignatureAlgorithm: String = "SHA256withECDSA"

  val PublicKeyHexPrefix: String = "3059301306072a8648ce3d020106082a8648ce3d03010703420004"

  def generateKeyPair[F[_]: Async: SecurityProvider]: F[KeyPair] =
    for {
      ecSpec <- Async[F].delay(new ECGenParameterSpec(CurveName))
      keyGen <- Async[F].delay(JKeyPairGenerator.getInstance("EC", SecurityProvider[F].provider))
      secureRandom <- SecureRandom.get[F]
      _       <- Async[F].delay(keyGen.initialize(ecSpec, secureRandom))
      keyPair <- Async[F].delay(keyGen.generateKeyPair())
    } yield keyPair

  def sign[F[_]: Async: SecurityProvider](data: Array[Byte], privateKey: PrivateKey): F[Array[Byte]] =
    for {
      sig <- Async[F].delay(JSignature.getInstance(SignatureAlgorithm, SecurityProvider[F].provider))
      secureRandom <- SecureRandom.get[F]
      _      <- Async[F].delay(sig.initSign(privateKey, secureRandom))
      signed <- Async[F].blocking {
        sig.update(data)
        sig.sign()
      }
    } yield signed

  def verify[F[_]: Async: SecurityProvider](
    data: Array[Byte],
    sigBytes: Array[Byte],
    publicKey: PublicKey
  ): F[Boolean] =
    for {
      sig    <- Async[F].delay(JSignature.getInstance(SignatureAlgorithm, SecurityProvider[F].provider))
      _      <- Async[F].delay(sig.initVerify(publicKey))
      result <- Async[F].blocking {
        sig.update(data)
        sig.verify(sigBytes)
      }
    } yield result

  def publicKeyToId(publicKey: PublicKey): Id = {
    val hex: String = Hex.fromBytes(publicKey.getEncoded).coerce
    Id(hex.stripPrefix(PublicKeyHexPrefix).coerce[Hex])
  }

  def idToPublicKey[F[_]: Async: SecurityProvider](id: Id): F[PublicKey] = {
    val prefixed: Hex = (PublicKeyHexPrefix + id.hex.coerce[String]).coerce[Hex]
    val encodedBytes = prefixed.toBytes
    for {
      spec <- Async[F].delay(new X509EncodedKeySpec(encodedBytes))
      kf   <- Async[F].delay(KeyFactory.getInstance("EC", SecurityProvider[F].provider))
      pk   <- Async[F].delay(kf.generatePublic(spec))
    } yield pk
  }

  def signToSignature[F[_]: Async: SecurityProvider](data: Array[Byte], privateKey: PrivateKey): F[Signature] =
    sign(data, privateKey).map(bytes => Signature(Hex.fromBytes(bytes)))
}
