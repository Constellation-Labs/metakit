package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import cats.MonadThrow

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.impl.{
  OptimizedMerklePatriciaProducer,
  SimpleMerklePatriciaProducer
}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

import io.circe.Encoder

trait MerklePatriciaProducer[F[_]] {
  def create[A: Encoder](data: Map[Hash, A]): F[MerklePatriciaTrie]

  def insert[A: Encoder](
    current: MerklePatriciaTrie,
    data:    Map[Hash, A]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]]

  def remove(current: MerklePatriciaTrie, data: List[Hash]): F[Either[MerklePatriciaError, MerklePatriciaTrie]]
}

object MerklePatriciaProducer {
  def apply[F[_]](implicit producer: MerklePatriciaProducer[F]): MerklePatriciaProducer[F] = producer

  def make[F[_]: JsonBinaryHasher: MonadThrow]: MerklePatriciaProducer[F] =
    new OptimizedMerklePatriciaProducer[F]

  def simple[F[_]: JsonBinaryHasher: MonadThrow]: MerklePatriciaProducer[F] =
    new SimpleMerklePatriciaProducer[F]
}

sealed trait MerklePatriciaError extends Throwable
case class InvalidData(message: String) extends MerklePatriciaError
case class OperationError(message: String) extends MerklePatriciaError
