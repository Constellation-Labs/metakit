package io.constellationnetwork.metagraph_sdk.lifecycle

import cats.effect.Async
import cats.effect.std.AtomicCell
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.std.Checkpoint

import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait CheckpointService[F[_], S] {
  def get: F[Checkpoint[S]]
  def set(checkpoint: Checkpoint[S]): F[Boolean]
  def evalModify[E](f: Checkpoint[S] => F[Either[E, Checkpoint[S]]]): F[Either[E, Checkpoint[S]]]
}

object CheckpointService {

  def make[F[_]: Async, S](state: S): F[CheckpointService[F, S]] =
    AtomicCell[F]
      .of(Checkpoint.genesis(state))
      .map { atomicCell =>
        new CheckpointService[F, S] {

          private val logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromClass(CheckpointService.getClass)

          override def get: F[Checkpoint[S]] =
            atomicCell.get

          override def set(checkpoint: Checkpoint[S]): F[Boolean] =
            atomicCell
              .set(checkpoint)
              .as(true)
              .handleErrorWith(logger.error(_)(s"Checkpoint set failed") >> false.pure[F])

          override def evalModify[E](f: Checkpoint[S] => F[Either[E, Checkpoint[S]]]): F[Either[E, Checkpoint[S]]] =
            atomicCell.evalModify { currentState =>
              f(currentState).map {
                case result @ Right(updatedState) => (updatedState, result)
                case result @ Left(_)             => (currentState, result)
              }
            }
        }
      }
}
