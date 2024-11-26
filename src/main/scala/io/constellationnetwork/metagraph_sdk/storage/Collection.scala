package io.constellationnetwork.metagraph_sdk.storage

import cats.data.OptionT
import cats.implicits.showInterpolator
import cats.{MonadThrow, Show}

trait Collection[F[_], Key, Value] extends CollectionReader[F, Key, Value] with CollectionWriter[F, Key, Value]

trait CollectionReader[F[_], Key, Value] {

  def get(id: Key): F[Option[Value]]

  def getBatch(ids: List[Key]): F[List[(Key, Option[Value])]]

  def contains(id: Key): F[Boolean]

  def dump: F[List[(Key, Value)]] =
    getWithFilter((_, _) => true)

  def getWithFilter(cond: (Key, Value) => Boolean): F[List[(Key, Value)]]

  def getUnsafe(id: Key)(implicit monadThrow: MonadThrow[F], showKey: Show[Key]): F[Value] =
    OptionT(get(id)).getOrElseF(monadThrow.raiseError(new NoSuchElementException(show"Element not found. id=$id")))
}

trait CollectionWriter[F[_], Key, Value] {
  def put(id: Key, t: Value): F[Unit]

  def remove(id: Key): F[Unit]

  def putBatch(updates: List[(Key, Value)]): F[Unit]

  def removeBatch(deletions: List[Key]): F[Unit]
}
