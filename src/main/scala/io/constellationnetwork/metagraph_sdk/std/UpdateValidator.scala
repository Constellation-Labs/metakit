package io.constellationnetwork.metagraph_sdk.std

import org.tessellation.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr

trait UpdateValidator[F[_], U, T] {
  def verify(state: T, update: U): F[DataApplicationValidationErrorOr[Unit]]
}
