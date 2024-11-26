package io.constellationnetwork.metagraph_sdk.std

import cats.Functor
import cats.implicits.toFunctorOps

import org.tessellation.security.hash.Hash

trait JsonBinaryHasher[F[_]] {
  def hash[A](data: A): F[Hash]
}

object JsonBinaryHasher {
  def apply[F[_]](implicit ev: JsonBinaryHasher[F]): JsonBinaryHasher[F] = ev

  implicit class FromJsonBinaryCodec[F[_]: Functor, A](obj: A)(implicit bin: JsonBinaryCodec[F, A]) {
    def hash: F[Hash] = bin.serialize(obj).map(Hash.fromBytes)
  }
}
