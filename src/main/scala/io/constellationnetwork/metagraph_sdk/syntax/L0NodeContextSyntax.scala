package io.constellationnetwork.metagraph_sdk.syntax

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all._

import org.tessellation.currency.dataApplication.{DataApplicationValidationError, DataOnChainState, L0NodeContext}
import org.tessellation.currency.schema.currency.CurrencyIncrementalSnapshot
import org.tessellation.schema.SnapshotOrdinal
import org.tessellation.security.Hashed

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

trait L0NodeContextSyntax {

  implicit class L0ContextOps[F[_]: Sync](ctx: L0NodeContext[F]) {

    def getOnChainState[PUB <: DataOnChainState](implicit
      json2bin: JsonBinaryCodec[F, PUB]
    ): F[Either[DataApplicationValidationError, PUB]] =
      EitherT(getLatestCurrencySnapshot).flatMapF { snapshot =>
        snapshot.dataApplication
          .toRight(L0Errors.L0CtxCouldNotGetLatestState: DataApplicationValidationError)
          .flatTraverse { part =>
            json2bin
              .deserialize(part.onChainState)
              .map(_.leftMap(_ => L0Errors.L0CtxFailedToDecodeState: DataApplicationValidationError))
          }
      }.value

    def getLatestCurrencySnapshot: F[Either[DataApplicationValidationError, CurrencyIncrementalSnapshot]] =
      EitherT
        .fromOptionF[F, DataApplicationValidationError, Hashed[CurrencyIncrementalSnapshot]](
          ctx.getLastCurrencySnapshot,
          L0Errors.L0CtxCouldNotGetLatestCurrencySnapshot
        )
        .map(_.signed.value)
        .value

    def getCurrencySnapshotAt(
      ordinal: SnapshotOrdinal
    ): F[Either[DataApplicationValidationError, CurrencyIncrementalSnapshot]] =
      EitherT
        .fromOptionF[F, DataApplicationValidationError, Hashed[CurrencyIncrementalSnapshot]](
          ctx.getCurrencySnapshot(ordinal),
          L0Errors.L0CtxCouldNotGetLatestGlobalSnapshot
        )
        .map(_.signed.value)
        .value
  }
}

object L0Errors {

  @derive(decoder, encoder)
  case object L0CtxCouldNotGetLatestCurrencySnapshot extends DataApplicationValidationError {
    val message = "Failed to retrieve latest currency snapshot from L0 node context!"
  }

  @derive(decoder, encoder)
  case object L0CtxCouldNotGetLatestState extends DataApplicationValidationError {
    val message = "Failed to retrieve latest state from L0 node context!"
  }

  @derive(decoder, encoder)
  case object L0CtxFailedToDecodeState extends DataApplicationValidationError {
    val message = "An error was encountered while decoding the state from L0 node context"
  }

  @derive(decoder, encoder)
  case object L0CtxCouldNotGetLatestGlobalSnapshot extends DataApplicationValidationError {
    val message = "Failed to retrieve latest global snapshot from L0 node context!"
  }
}
