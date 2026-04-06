package io.constellationnetwork.metagraph_sdk.crypto

import java.nio.charset.StandardCharsets
import java.security.KeyPair

import cats.Applicative
import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.curves.Secp256r1
import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonBinaryHasher}
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex
import io.constellationnetwork.security.key.ops.PublicKeyOps
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}
import io.constellationnetwork.security.signature.{Signed, Signing}

import org.typelevel.log4cats.slf4j.Slf4jLogger

trait SignatureProver[F[_]] {
  def attest(msg: Hash, keypair: KeyPair): F[SignatureProof]
}

trait SignatureVerifier[F[_]] {
  def confirm(msg: Hash, proof: SignatureProof): F[Boolean]
}

trait MetagraphSignatureProver[F[_]] {
  def attest(msg: Hash, keypair: KeyPair): F[MetagraphSignatureProof]
}

trait MetagraphSignatureVerifier[F[_]] {
  def confirm(msg: Hash, proof: MetagraphSignatureProof): F[Boolean]
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

  def proveSigned[F[_]: Async: SecurityProvider, A](implicit codec: JsonBinaryCodec[F, A]): SignedJsonProducer[F, A] =
    new SignedJsonProducer[F, A](codec.asRight)

  def verifySigned[F[_]: Async: SecurityProvider, A](implicit codec: JsonBinaryCodec[F, A]): SignedJsonEvaluator[F, A] =
    new SignedJsonEvaluator[F, A](codec.asRight)

  def customProveSigned[F[_]: Async: SecurityProvider, A](toBytes: A => F[Array[Byte]]): SignedJsonProducer[F, A] =
    new SignedJsonProducer[F, A](toBytes.asLeft)

  def customVerifySigned[F[_]: Async: SecurityProvider, A](toBytes: A => F[Array[Byte]]): SignedJsonEvaluator[F, A] =
    new SignedJsonEvaluator[F, A](toBytes.asLeft)

  // --- Scheme-aware API ---

  def schemeProver[F[_]: Async: SecurityProvider](scheme: SigningScheme): MetagraphSignatureProver[F] =
    scheme match {
      case SigningScheme.Secp256k1Rfc8785V1 => secp256k1Prover[F]
      case SigningScheme.Secp256r1Rfc8785V1 => secp256r1Prover[F]
    }

  def schemeVerifier[F[_]: Async: SecurityProvider](scheme: SigningScheme): MetagraphSignatureVerifier[F] =
    scheme match {
      case SigningScheme.Secp256k1Rfc8785V1 => secp256k1Verifier[F]
      case SigningScheme.Secp256r1Rfc8785V1 => secp256r1Verifier[F]
    }

  def autoVerifier[F[_]: Async: SecurityProvider]: MetagraphSignatureVerifier[F] =
    (msg: Hash, proof: MetagraphSignatureProof) =>
      schemeVerifier[F](proof.scheme.getOrElse(SigningScheme.Secp256k1Rfc8785V1)).confirm(msg, proof)

  def proveSignedWithScheme[F[_]: Async: SecurityProvider, A](
    scheme: SigningScheme
  )(implicit codec: JsonBinaryCodec[F, A]): MetagraphSignedProducer[F, A] =
    new MetagraphSignedProducer[F, A](codec.asRight, scheme)

  def verifySignedWithScheme[F[_]: Async: SecurityProvider, A](
    resolver: SignatureProof => F[Option[SigningScheme]]
  )(implicit codec: JsonBinaryCodec[F, A]): MetagraphSignedEvaluator[F, A] =
    new MetagraphSignedEvaluator[F, A](codec.asRight, resolver)

  def constantSchemeResolver[F[_]: Applicative](scheme: SigningScheme): SignatureProof => F[Option[SigningScheme]] =
    _ => scheme.some.pure[F]

  // --- Private scheme implementations ---

  private def secp256k1Prover[F[_]: Async: SecurityProvider]: MetagraphSignatureProver[F] =
    (msg: Hash, keypair: KeyPair) =>
      for {
        id        <- keypair.getPublic.toId.pure[F]
        msgBytes  <- msg.value.getBytes(StandardCharsets.UTF_8).pure[F]
        sigBytes  <- Signing.signData(msgBytes)(keypair.getPrivate)
        signature <- Signature(Hex.fromBytes(sigBytes)).pure[F]
      } yield MetagraphSignatureProof(id, signature, SigningScheme.Secp256k1Rfc8785V1.some)

  private def secp256r1Prover[F[_]: Async: SecurityProvider]: MetagraphSignatureProver[F] =
    (msg: Hash, keypair: KeyPair) =>
      for {
        id        <- Secp256r1.publicKeyToId(keypair.getPublic).pure[F]
        msgBytes  <- msg.value.getBytes(StandardCharsets.UTF_8).pure[F]
        sigBytes  <- Secp256r1.sign(msgBytes, keypair.getPrivate)
        signature <- Signature(Hex.fromBytes(sigBytes)).pure[F]
      } yield MetagraphSignatureProof(id, signature, SigningScheme.Secp256r1Rfc8785V1.some)

  private def secp256k1Verifier[F[_]: Async: SecurityProvider]: MetagraphSignatureVerifier[F] =
    (msg: Hash, proof: MetagraphSignatureProof) =>
      (for {
        publicKey <- proof.id.hex.toPublicKey
        msgBytes  <- msg.value.getBytes(StandardCharsets.UTF_8).pure[F]
        sigBytes  <- proof.signature.value.toBytes.pure[F]
        result    <- Signing.verifySignature(msgBytes, sigBytes)(publicKey)
      } yield result).handleErrorWith { err =>
        Slf4jLogger.getLogger[F].error(err)(s"Failed to verify k1 signature with id: ${proof.id.show}").as(false)
      }

  private def secp256r1Verifier[F[_]: Async: SecurityProvider]: MetagraphSignatureVerifier[F] =
    (msg: Hash, proof: MetagraphSignatureProof) =>
      (for {
        publicKey <- Secp256r1.idToPublicKey(proof.id)
        msgBytes  <- msg.value.getBytes(StandardCharsets.UTF_8).pure[F]
        sigBytes  <- proof.signature.value.toBytes.pure[F]
        result    <- Secp256r1.verify(msgBytes, sigBytes, publicKey)
      } yield result).handleErrorWith { err =>
        Slf4jLogger.getLogger[F].error(err)(s"Failed to verify r1 signature with id: ${proof.id.show}").as(false)
      }
}
