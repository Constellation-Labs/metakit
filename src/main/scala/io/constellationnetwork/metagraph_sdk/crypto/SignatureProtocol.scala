package io.constellationnetwork.metagraph_sdk.crypto

import java.nio.charset.StandardCharsets
import java.security.KeyPair

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonBinaryHasher}
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.key.ops.PublicKeyOps
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
import io.constellationnetwork.security.signature.{Signed, Signing}

import io.circe.{Decoder, Encoder}
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait SignatureProver[F[_]] {
  def attest(msg: Hash, keypair: KeyPair): F[SignatureProof]
}

trait SignatureVerifier[F[_]] {
  def confirm(msg: Hash, proof: SignatureProof): F[Boolean]
}

class SignedJsonProducer[F[_]: Async: SecurityProvider, A](serde: Either[A => F[Array[Byte]], JsonBinaryCodec[F, A]]) {

  def create(value: A, keypair: KeyPair): F[Signed[A]] =
    serde match {
      case Left(toBytes) =>
        for {
          bytes <- toBytes(value)
          hash  <- Hash.fromBytes(bytes).pure[F]
          proof <- SignatureProtocol.prover.attest(hash, keypair)
          proofSet = NonEmptySet.one(proof)
        } yield Signed(value, proofSet)

      case Right(codec) =>
        for {
          hash  <- JsonBinaryHasher[F].computeDigest(value)(codec)
          proof <- SignatureProtocol.prover.attest(hash, keypair)
          proofSet = NonEmptySet.one(proof)
        } yield Signed(value, proofSet)
    }

  def addSignature(signed: Signed[A], keypair: KeyPair): F[Signed[A]] =
    serde match {
      case Left(toBytes) =>
        for {
          bytes <- toBytes(signed.value)
          hash  <- Hash.fromBytes(bytes).pure[F]
          proof <- SignatureProtocol.prover.attest(hash, keypair)
        } yield signed.addProof(proof)

      case Right(codec) =>
        for {
          hash  <- JsonBinaryHasher[F].computeDigest(signed.value)(codec)
          proof <- SignatureProtocol.prover.attest(hash, keypair)
        } yield signed.addProof(proof)
    }

  def batchSign(value: A, keypairs: NonEmptyList[KeyPair]): F[Signed[A]] =
    keypairs.head match {
      case firstKeypair =>
        for {
          initialSigned <- create(value, firstKeypair)
          result <- keypairs.tail.foldLeftM(initialSigned) { (acc, keypair) =>
            addSignature(acc, keypair)
          }
        } yield result
    }
}

class SignedJsonEvaluator[F[_]: Async: SecurityProvider, A](serde: Either[A => F[Array[Byte]], JsonBinaryCodec[F, A]]) {

  def inspect(signed: Signed[A]): F[Either[NonEmptySet[SignatureProof], NonEmptySet[SignatureProof]]] =
    signed.proofs.toNonEmptyList.toList.traverse { proof =>
      serde match {
        case Left(toBytes) =>
          for {
            bytes  <- toBytes(signed.value)
            hash   <- Hash.fromBytes(bytes).pure[F]
            result <- SignatureProtocol.verifier.confirm(hash, proof)
          } yield proof -> result

        case Right(codec) =>
          for {
            hash   <- JsonBinaryHasher[F].computeDigest(signed.value)(codec)
            result <- SignatureProtocol.verifier.confirm(hash, proof)
          } yield proof -> result
      }
    }.map {
      _.collect {
        case (proof, false) =>
          proof
      }.toNel
        .map(_.toNes)
        .toLeft(signed.proofs)
    }
}

object SignatureProtocol {
  type InvalidProofs = NonEmptySet[SignatureProof]
  type ValidProofs = NonEmptySet[SignatureProof]

  def prover[F[_]: Async: SecurityProvider]: SignatureProver[F] = (msg: Hash, keypair: KeyPair) =>
    for {
      id        <- keypair.getPublic.toId.pure[F]
      msgBytes  <- msg.value.getBytes(StandardCharsets.UTF_8).pure[F]
      sigBytes  <- Signing.signData(msgBytes)(keypair.getPrivate)
      signature <- Signature(Hex.fromBytes(sigBytes)).pure[F]
    } yield SignatureProof(id, signature)

  def verifier[F[_]: Async: SecurityProvider]: SignatureVerifier[F] = (msg: Hash, proof: SignatureProof) =>
    (for {
      publicKey <- proof.id.hex.toPublicKey
      msgBytes  <- msg.value.getBytes(StandardCharsets.UTF_8).pure[F]
      sigBytes  <- proof.signature.value.toBytes.pure[F]
      result    <- Signing.verifySignature(msgBytes, sigBytes)(publicKey)
    } yield result).handleErrorWith { err =>
      Slf4jLogger.getLogger[F].error(err)(s"Failed to verify signature with id: ${proof.id.show}").as(false)
    }

  def proveSigned[F[_]: Async: SecurityProvider, A: Encoder: Decoder](implicit codec: JsonBinaryCodec[F, A]): SignedJsonProducer[F, A] =
    new SignedJsonProducer[F, A](codec.asRight)

  def verifySigned[F[_]: Async: SecurityProvider, A: Encoder: Decoder](implicit codec: JsonBinaryCodec[F, A]): SignedJsonEvaluator[F, A] =
    new SignedJsonEvaluator[F, A](codec.asRight)

  def customProveSigned[F[_]: Async: SecurityProvider, A](toBytes: A => F[Array[Byte]]): SignedJsonProducer[F, A] =
    new SignedJsonProducer[F, A](toBytes.asLeft)

  def customVerifySigned[F[_]: Async: SecurityProvider, A](toBytes: A => F[Array[Byte]]): SignedJsonEvaluator[F, A] =
    new SignedJsonEvaluator[F, A](toBytes.asLeft)

}
