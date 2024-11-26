package io.constellationnetwork.metagraph_sdk.std

import cats.effect.{Ref, Sync}
import cats.syntax.functor._

import scala.collection.mutable

trait CircularBuffer[F[_], A] {
  def put(items: List[A]): F[Unit]
  def list: F[List[A]]
  def contains(item:  A): F[Boolean]
  def contains(items: List[A]): F[List[Boolean]]
}

object CircularBuffer {

  def make[F[_]: Sync, A](size: Int): F[CircularBuffer[F, A]] =
    Ref.of[F, (mutable.Buffer[A], Int)]((mutable.Buffer.empty[A], 0)).map { ref =>
      new CircularBuffer[F, A] {
        override def put(items: List[A]): F[Unit] =
          ref.update { case (buffer, writePos) =>
            items.foldLeft((buffer, writePos)) { case ((buf, pos), item) =>
              if (buf.size < size) {
                buf.append(item)
              } else {
                buf.update(pos, item)
              }

              (buf, (pos + 1) % size)
            }
          }

        override def list: F[List[A]] =
          ref.get.map(_._1.toList)

        override def contains(item: A): F[Boolean] =
          ref.get.map { case (buffer, _) =>
            buffer.contains(item)
          }

        override def contains(items: List[A]): F[List[Boolean]] =
          ref.get.map { case (buffer, _) =>
            items.map(buffer.contains(_))
          }
      }
    }
}
