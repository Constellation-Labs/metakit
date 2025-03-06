package io.constellationnetwork.metagraph_sdk.lifecycle

import cats.Monad
import cats.syntax.foldable._

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.security.signature.Signed

abstract class CombinerService[F[_]: Monad, TX <: DataUpdate, PUB <: DataOnChainState, PRV <: DataCalculatedState] {

  def foldLeft(
    previous: DataState[PUB, PRV],
    batch:    List[Signed[TX]]
  )(implicit ctx: L0NodeContext[F]): F[DataState[PUB, PRV]] =
    batch.foldLeftM(previous) { (acc, update) =>
      insert(acc, update)
    }

  def insert(
    previous: DataState[PUB, PRV],
    update:   Signed[TX]
  )(implicit ctx: L0NodeContext[F]): F[DataState[PUB, PRV]]
}
