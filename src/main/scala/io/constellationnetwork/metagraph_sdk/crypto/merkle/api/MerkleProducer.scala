package io.constellationnetwork.metagraph_sdk.crypto.merkle.api

import java.nio.file.Path

import cats.MonadThrow
import cats.effect.{Async, Resource, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.impl.{InMemoryMerkleProducer, LevelDbMerkleProducer, StatelessMerkleProducer}
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher

trait MerkleProducer[F[_]] {
  def create(leaves: List[MerkleNode.Leaf]): F[MerkleTree]

  def append(
    current: MerkleTree,
    leaves: List[MerkleNode.Leaf]
  ): F[MerkleTree]

  def prepend(
    current: MerkleTree,
    leaves: List[MerkleNode.Leaf]
  ): F[MerkleTree]

  def update(
    current: MerkleTree,
    index: Int,
    leaf: MerkleNode.Leaf
  ): F[Either[MerkleProducerError, MerkleTree]]

  def remove(
    current: MerkleTree,
    index: Int
  ): F[Either[MerkleProducerError, MerkleTree]]

  def getProver(tree: MerkleTree): F[MerkleProver[F]]
}

trait StatefulMerkleProducer[F[_]] {
  def leaves: F[List[MerkleNode.Leaf]]
  def build: F[Either[TreeBuildError, MerkleTree]]
  def update(index: Int, leaf: MerkleNode.Leaf): F[Either[MerkleProducerError, Unit]]
  def append(leaves: List[MerkleNode.Leaf]): F[Unit]
  def prepend(leaves: List[MerkleNode.Leaf]): F[Unit]
  def remove(index: Int): F[Either[MerkleProducerError, Unit]]
  def getProver: F[MerkleProver[F]]
}

object MerkleProducer {
  def apply[F[_]](implicit producer: MerkleProducer[F]): MerkleProducer[F] = producer

  def make[F[_]: JsonBinaryHasher: MonadThrow]: MerkleProducer[F] = stateless[F]

  def stateless[F[_]: JsonBinaryHasher: MonadThrow]: MerkleProducer[F] =
    new StatelessMerkleProducer[F]

  /**
   * Create an in-memory producer with caching support
   *
   * @param initial Initial leaves
   * @return Producer with in-memory storage and caching
   */
  def inMemory[F[_]: Sync: JsonBinaryHasher](
    initial: List[MerkleNode.Leaf] = List.empty
  ): F[StatefulMerkleProducer[F]] =
    InMemoryMerkleProducer.make[F](initial).widen[StatefulMerkleProducer[F]]

  /**
   * Create a LevelDB-backed persistent producer instance
   *
   * @param dbPath Path to the LevelDB database directory
   * @param initial Initial leaf nodes (only used if database is empty)
   * @return Producer with persistent storage
   */
  def levelDb[F[_]: Async: JsonBinaryHasher](
    dbPath: Path,
    initial: List[MerkleNode.Leaf] = List.empty
  ): Resource[F, StatefulMerkleProducer[F]] =
    LevelDbMerkleProducer.make[F](dbPath, initial).widen[StatefulMerkleProducer[F]]

  /**
   * Load an existing LevelDB-backed producer instance
   *
   * @param dbPath Path to the existing LevelDB database directory
   * @return Producer connected to existing persistent storage
   */
  def loadLevelDb[F[_]: Async: JsonBinaryHasher](
    dbPath: Path
  ): Resource[F, StatefulMerkleProducer[F]] =
    LevelDbMerkleProducer.load[F](dbPath).widen[StatefulMerkleProducer[F]]
}

sealed trait MerkleProducerError extends Throwable

case class InvalidIndex(index: Int, size: Int) extends MerkleProducerError {
  override def getMessage: String = s"Invalid index $index, size is $size"
}

case class TreeBuildError(message: String) extends MerkleProducerError {
  override def getMessage: String = message
}
