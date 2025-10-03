package io.constellationnetwork.metagraph_sdk.crypto.mpt.api

import java.nio.file.Path

import cats.MonadThrow
import cats.effect.{Async, Resource, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.impl.{
  InMemoryMerklePatriciaProducer,
  LevelDbMerklePatriciaProducer,
  StatelessMerklePatriciaProducer
}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash

import io.circe.{Encoder, Json}

trait MerklePatriciaProducer[F[_]] {
  def create[A: Encoder](data: Map[Hash, A]): F[MerklePatriciaTrie]

  def insert[A: Encoder](
    current: MerklePatriciaTrie,
    data: Map[Hash, A]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]]

  def remove(
    current: MerklePatriciaTrie,
    keys: List[Hash]
  ): F[Either[MerklePatriciaError, MerklePatriciaTrie]]

  def getProver(trie: MerklePatriciaTrie): F[MerklePatriciaProver[F]]
}

trait StatefulMerklePatriciaProducer[F[_]] {
  def entries: F[Map[Hash, Json]]
  def build: F[Either[MerklePatriciaError, MerklePatriciaTrie]]
  def insert[A: Encoder](data: Map[Hash, A]): F[Either[MerklePatriciaError, Unit]]
  def update[A: Encoder](key: Hash, value: A): F[Either[MerklePatriciaError, Unit]]
  def remove(keys: List[Hash]): F[Either[MerklePatriciaError, Unit]]
  def clear: F[Unit]
  def getProver: F[MerklePatriciaProver[F]]
}

object MerklePatriciaProducer {
  def apply[F[_]](implicit producer: MerklePatriciaProducer[F]): MerklePatriciaProducer[F] = producer

  def make[F[_]: JsonBinaryHasher: MonadThrow]: MerklePatriciaProducer[F] = stateless[F]

  def stateless[F[_]: JsonBinaryHasher: MonadThrow]: MerklePatriciaProducer[F] =
    new StatelessMerklePatriciaProducer[F]

  /**
   * Create an in-memory producer with caching support
   *
   * @param initial Initial entries
   * @return Producer with in-memory storage and caching
   */
  def inMemory[F[_]: Sync: JsonBinaryHasher](
    initial: Map[Hash, Json] = Map.empty
  ): F[StatefulMerklePatriciaProducer[F]] =
    InMemoryMerklePatriciaProducer.make[F](initial).widen[StatefulMerklePatriciaProducer[F]]

  /**
   * Create a LevelDB-backed persistent producer instance
   *
   * @param dbPath Path to the LevelDB database directory
   * @param initial Initial entries (only used if database is empty)
   * @return Producer with persistent storage
   */
  def levelDb[F[_]: Async: JsonBinaryHasher](
    dbPath: Path,
    initial: Map[Hash, Json] = Map.empty
  ): Resource[F, StatefulMerklePatriciaProducer[F]] =
    LevelDbMerklePatriciaProducer.make[F](dbPath, initial).widen[StatefulMerklePatriciaProducer[F]]

  /**
   * Load an existing LevelDB-backed producer instance
   *
   * @param dbPath Path to the existing LevelDB database directory
   * @return Producer connected to existing persistent storage
   */
  def loadLevelDb[F[_]: Async: JsonBinaryHasher](
    dbPath: Path
  ): Resource[F, StatefulMerklePatriciaProducer[F]] =
    LevelDbMerklePatriciaProducer.load[F](dbPath).widen[StatefulMerklePatriciaProducer[F]]
}

sealed trait MerklePatriciaError extends Throwable
case class InvalidData(message: String) extends MerklePatriciaError
case class OperationError(message: String) extends MerklePatriciaError
