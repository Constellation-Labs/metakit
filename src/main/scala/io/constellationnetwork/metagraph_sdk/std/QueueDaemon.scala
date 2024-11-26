package io.constellationnetwork.metagraph_sdk.std

import cats.effect.Sync
import cats.effect.std.Queue
import cats.syntax.applicativeError._

import fs2.Stream
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

abstract class QueueDaemon[F[_]: Sync, A] {

  val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass(this.getClass)

  val queue: Queue[F, A]

  def process(a: A): F[Unit]

  def run: F[Unit] =
    Stream
      .fromQueueUnterminated(queue)
      .evalMap { elem =>
        process(elem)
          .handleErrorWith(logger.error(_)("Uncaught exception during queue processing"))
      }
      .compile
      .drain
}
