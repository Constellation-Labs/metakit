package io.constellationnetwork.metagraph_sdk.std

import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.security.SecurityProvider
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.signature.SignatureProof
import io.constellationnetwork.security.signature.{signature, Signed}

object SignatureValidator {

  /**
   * Validates signature proofs for a signed object using a specified serialization scheme.
   *
   * @param signed The signed object containing the value and proofs to validate
   * @param serializationScheme Specifies how to serialize the object for hashing:
   *   - Left: Custom function to convert the object to bytes
   *   - Right: Use JsonBinaryCodec for serialization
   * @return Either invalid proofs (Left) or valid proofs (Right)
   */
  private def validateSignatures[F[_]: Async: SecurityProvider: JsonBinaryHasher, A](
    signed:              Signed[A],
    serializationScheme: Either[A => F[Array[Byte]], JsonBinaryCodec[F, A]]
  ): F[Either[NonEmptySet[SignatureProof], NonEmptySet[SignatureProof]]] =
    signed.proofs.toNonEmptyList.toList
      .traverse { proof =>
        serializationScheme match {
          case Left(toBytes) =>
            for {
              bytes <- toBytes(signed.value)
              hash = Hash.fromBytes(bytes)
              result <- signature.verifySignatureProof(hash, proof)
            } yield proof -> result

          case Right(codec) =>
            for {
              hash   <- JsonBinaryHasher[F].computeDigest(signed.value)(codec)
              result <- signature.verifySignatureProof(hash, proof)
            } yield proof -> result
        }
      }
      .map {
        _.collect { case (proof, false) =>
          proof
        }.toNel
          .map(_.toNes)
          .toLeft(signed.proofs)
      }

  def validateWithCodec[F[_]: Async: SecurityProvider: JsonBinaryHasher, A](
    signed: Signed[A]
  )(implicit codec: JsonBinaryCodec[F, A]): F[Either[NonEmptySet[SignatureProof], NonEmptySet[SignatureProof]]] =
    validateSignatures(signed, Right(codec))

  def validateWithCustom[F[_]: Async: SecurityProvider: JsonBinaryHasher, A](
    signed:  Signed[A],
    toBytes: A => F[Array[Byte]]
  ): F[Either[NonEmptySet[SignatureProof], NonEmptySet[SignatureProof]]] =
    validateSignatures(signed, Left(toBytes))
}
