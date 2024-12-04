package io.constellationnetwork.metagraph_sdk.lifecycle

import cats.data.NonEmptyList
import cats.syntax.functor._
import cats.syntax.parallel._
import cats.{Monad, Parallel}

import org.tessellation.currency.dataApplication._
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import org.tessellation.security.signature.Signed

abstract class ValidationService[
  F[_]: Monad: Parallel,
  TX <: DataUpdate,
  PUB <: DataOnChainState,
  PRV <: DataCalculatedState
] {

  def validateUpdate(
    update: TX
  )(implicit ctx: L1NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]]

  def validateSignedUpdate(
    current:      DataState[PUB, PRV],
    signedUpdate: Signed[TX]
  )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]]

  def validateDataParallel(
    current: DataState[PUB, PRV],
    batch:   NonEmptyList[Signed[TX]]
  )(implicit ctx: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    val validator = validateSignedUpdate(current, _)

    batch
      .parTraverse(validator)
      .map(_.reduce)
  }

  def validateData(
    current: DataState[PUB, PRV],
    batch:   NonEmptyList[Signed[TX]]
  )(implicit ctx: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] = {
    val validator = validateSignedUpdate(current, _)

    batch
      .traverse(validator)
      .map(_.reduce)
  }
}
