package io.constellationnetwork.metagraph_sdk.crypto

import java.security.KeyPair

import cats.data.{NonEmptyList, NonEmptySet}
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonBinaryHasher}
import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed
import io.constellationnetwork.security.signature.signature.SignatureProof

class MetagraphSignedProducer[F[_]: Async: SecurityProvider, A](
  serde: Either[A => F[Array[Byte]], JsonBinaryCodec[F, A]],
  scheme: SigningScheme
) {

  private val metagraphProver: MetagraphSignatureProver[F] = SignatureProtocol.schemeProver[F](scheme)

  def create(value: A, keypair: KeyPair): F[Signed[A]] =
    for {
      hash  <- computeHash(value)
      proof <- metagraphProver.attest(hash, keypair)
      proofSet = NonEmptySet.one(proof.toTessellation)
    } yield Signed(value, proofSet)

  def createWithProofs(value: A, keypair: KeyPair): F[(Signed[A], NonEmptyList[MetagraphSignatureProof])] =
    for {
      hash  <- computeHash(value)
      proof <- metagraphProver.attest(hash, keypair)
      proofSet = NonEmptySet.one(proof.toTessellation)
    } yield (Signed(value, proofSet), NonEmptyList.one(proof))

  def addSignature(signed: Signed[A], keypair: KeyPair): F[Signed[A]] =
    for {
      hash  <- computeHash(signed.value)
      proof <- metagraphProver.attest(hash, keypair)
    } yield signed.addProof(proof.toTessellation)

  def batchSign(value: A, keypairs: NonEmptyList[KeyPair]): F[Signed[A]] =
    for {
      initialSigned <- create(value, keypairs.head)
      result <- keypairs.tail.foldLeftM(initialSigned) { (acc, keypair) =>
        addSignature(acc, keypair)
      }
    } yield result

  private def computeHash(value: A): F[Hash] =
    serde match {
      case Left(toBytes) => toBytes(value).map(Hash.fromBytes)
      case Right(codec)  => JsonBinaryHasher[F].computeDigest(value)(codec)
    }
}

class MetagraphSignedEvaluator[F[_]: Async: SecurityProvider, A](
  serde: Either[A => F[Array[Byte]], JsonBinaryCodec[F, A]],
  schemeResolver: SignatureProof => F[Option[SigningScheme]]
) {

  private val autoVerifier: MetagraphSignatureVerifier[F] = SignatureProtocol.autoVerifier[F]

  def inspect(signed: Signed[A]): F[Either[NonEmptySet[SignatureProof], NonEmptySet[SignatureProof]]] =
    signed.proofs.toNonEmptyList.toList.traverse { proof =>
      for {
        hash   <- computeHash(signed.value)
        scheme <- schemeResolver(proof)
        mProof  = MetagraphSignatureProof(proof.id, proof.signature, scheme)
        result <- autoVerifier.confirm(hash, mProof)
      } yield proof -> result
    }.map {
      _.collect { case (proof, false) => proof }
        .toNel
        .map(_.toNes)
        .toLeft(signed.proofs)
    }

  private def computeHash(value: A): F[Hash] =
    serde match {
      case Left(toBytes) => toBytes(value).map(Hash.fromBytes)
      case Right(codec)  => JsonBinaryHasher[F].computeDigest(value)(codec)
    }
}
