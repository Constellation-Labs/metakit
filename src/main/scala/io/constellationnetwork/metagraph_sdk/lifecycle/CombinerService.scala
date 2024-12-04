package io.constellationnetwork.metagraph_sdk.lifecycle

import cats.Monad
import cats.syntax.foldable._

import org.tessellation.currency.dataApplication._
import org.tessellation.security.signature.Signed

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
