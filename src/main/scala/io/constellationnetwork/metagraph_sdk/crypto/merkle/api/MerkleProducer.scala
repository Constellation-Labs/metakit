package io.constellationnetwork.metagraph_sdk.crypto.merkle.api

import java.nio.file.Path

import cats.effect.{Ref, Resource, Sync}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.impl.{
  InMemoryMerkleProducer,
  LevelDbMerkleProducer,
  SimpleMerkleProducer
}
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher

/**
 * Type class for building and modifying Merkle trees
 */
trait MerkleProducer[F[_]] {

  /**
   * Get current leaves in the tree
   *
   * @return List of leaf nodes
   */
  def leaves: F[List[MerkleNode.Leaf]]

  /**
   * Build a Merkle tree from current leaves
   *
   * @return Built Merkle tree or error
   */
  def build: F[Either[TreeBuildError, MerkleTree]]

  /**
   * Update a leaf at a specific index
   *
   * @param index Index to update
   * @param leaf New leaf node
   * @return Unit if successful, error if index invalid
   */
  def update(index: Int, leaf: MerkleNode.Leaf): F[Either[MerkleProducerError, Unit]]

  /**
   * Append leaves to the end of the tree
   *
   * @param leaves Leaves to append
   */
  def append(leaves: List[MerkleNode.Leaf]): F[Unit]

  /**
   * Prepend leaves to the start of the tree
   *
   * @param leaves Leaves to prepend
   */
  def prepend(leaves: List[MerkleNode.Leaf]): F[Unit]

  /**
   * Remove a leaf at a specific index
   *
   * @param index Index to remove
   * @return Unit if successful, error if index invalid
   */
  def remove(index: Int): F[Either[MerkleProducerError, Unit]]
}

object MerkleProducer {
  def apply[F[_]](implicit producer: MerkleProducer[F]): MerkleProducer[F] = producer

  /**
   * Create an in-memory cached producer instance
   *
   * @param initial Initial leaf nodes
   * @return Producer with in-memory caching
   */
  def make[F[_]: Sync: JsonBinaryHasher](
    initial: List[MerkleNode.Leaf]
  ): F[MerkleProducer[F]] =
    inMemory(initial)

  /**
   * Create a simple producer instance
   *
   * @param initial Initial leaf nodes
   * @return Simple producer without caching
   */
  def simple[F[_]: Sync: JsonBinaryHasher](
    initial: List[MerkleNode.Leaf]
  ): F[MerkleProducer[F]] =
    Ref
      .of[F, Vector[MerkleNode.Leaf]](Vector.from(initial))
      .map(new SimpleMerkleProducer[F](_))

  /**
   * Create an in-memory cached producer instance
   *
   * @param initial Initial leaf nodes
   * @return Producer with in-memory caching
   */
  def inMemory[F[_]: Sync: JsonBinaryHasher](
    initial: List[MerkleNode.Leaf]
  ): F[MerkleProducer[F]] =
    InMemoryMerkleProducer.make[F](initial).widen

  /**
   * Create a LevelDB-backed persistent producer instance
   *
   * @param dbPath Path to the LevelDB database directory
   * @param initial Initial leaf nodes (only used if database is empty)
   * @return Producer with persistent storage
   */
  def levelDb[F[_]: Sync: JsonBinaryHasher](
    dbPath:  Path,
    initial: List[MerkleNode.Leaf] = List.empty
  ): Resource[F, LevelDbMerkleProducer[F]] =
    LevelDbMerkleProducer.make(dbPath, initial)

  /**
   * Load an existing LevelDB-backed producer instance
   *
   * @param dbPath Path to the existing LevelDB database directory
   * @return Producer connected to existing persistent storage
   */
  def loadLevelDb[F[_]: Sync: JsonBinaryHasher](
    dbPath: Path
  ): Resource[F, LevelDbMerkleProducer[F]] =
    LevelDbMerkleProducer.load(dbPath)

  /**
   * Provides syntax extensions for more ergonomic tree building
   *
   * Import xyz.kd5ujc.accumulators.merkle.api.MerkleProducer.syntax._ to use these extensions
   */
  object syntax {

    implicit class MerkleLeafOps(val leaves: List[MerkleNode.Leaf]) extends AnyVal {

      /**
       * Build a new Merkle tree from these leaves
       *
       * @return Built Merkle tree
       */
      def buildTree[F[_]: Sync: JsonBinaryHasher]: F[Either[TreeBuildError, MerkleTree]] =
        make[F](leaves).flatMap(_.build)
    }
  }
}

sealed trait MerkleProducerError extends Throwable

case class InvalidIndex(index: Int, size: Int) extends MerkleProducerError {
  override def getMessage: String = s"Invalid index $index, size is $size"
}

case class TreeBuildError(message: String) extends MerkleProducerError {
  override def getMessage: String = message
}
