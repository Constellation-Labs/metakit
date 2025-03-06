package io.constellationnetwork.metagraph_sdk.std

import cats.MonadThrow
import cats.syntax.functor._

import io.constellationnetwork.security.hash.Hash

trait JsonBinaryHasher[F[_]] {
  def computeDigest[A](data: A)(implicit codec: JsonBinaryCodec[F, A]): F[Hash]
}

object JsonBinaryHasher {
  def apply[F[_]](implicit ev: JsonBinaryHasher[F]): JsonBinaryHasher[F] = ev

  implicit def deriveFromCodec[F[_]: MonadThrow]: JsonBinaryHasher[F] =
    new JsonBinaryHasher[F] {

      def computeDigest[A](data: A)(implicit codec: JsonBinaryCodec[F, A]): F[Hash] =
        codec.serialize(data).map(Hash.fromBytes)
    }

  implicit class HasherOps[F[_], A](private val _v: A) extends AnyVal {

    def computeDigest(implicit mt: MonadThrow[F], codec: JsonBinaryCodec[F, A]): F[Hash] =
      JsonBinaryHasher[F].computeDigest(_v)
  }
}
