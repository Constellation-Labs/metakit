package io.constellationnetwork.metagraph_sdk.syntax

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all._

import org.tessellation.currency.dataApplication.{DataApplicationValidationError, DataOnChainState, L1NodeContext}
import org.tessellation.currency.schema.currency.CurrencyIncrementalSnapshot
import org.tessellation.schema.GlobalIncrementalSnapshot
import org.tessellation.security.Hashed

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

trait L1NodeContextSyntax {

  implicit class L1ContextOps[F[_]: Sync](ctx: L1NodeContext[F]) {

    def getLatestGlobalSnapshot: F[Either[DataApplicationValidationError, GlobalIncrementalSnapshot]] =
      EitherT
        .fromOptionF[F, DataApplicationValidationError, Hashed[GlobalIncrementalSnapshot]](
          ctx.getLastGlobalSnapshot,
          L1Errors.L1CtxCouldNotGetLatestGlobalSnapshot
        )
        .map(_.signed.value)
        .value

    def getOnChainState[PUB <: DataOnChainState](implicit
      json2bin: JsonBinaryCodec[F, PUB]
    ): F[Either[DataApplicationValidationError, PUB]] =
      EitherT(getLatestCurrencySnapshot)
        .flatMapF { snapshot =>
          snapshot.dataApplication
            .toRight(L1Errors.L1CtxCouldNotGetLatestState: DataApplicationValidationError)
            .traverse { part =>
              json2bin
                .deserialize(part.onChainState)
                .map(_.leftMap(_ => L1Errors.L1CtxFailedToDecodeState: DataApplicationValidationError))
            }
        }
        .value
        .map(_.flatten)

    def getLatestCurrencySnapshot: F[Either[DataApplicationValidationError, CurrencyIncrementalSnapshot]] =
      EitherT
        .fromOptionF[F, DataApplicationValidationError, Hashed[CurrencyIncrementalSnapshot]](
          ctx.getLastCurrencySnapshot,
          L1Errors.L1CtxCouldNotGetLatestCurrencySnapshot
        )
        .map(_.signed.value)
        .value
  }
}

object L1Errors {

  @derive(decoder, encoder)
  case object L1CtxCouldNotGetLatestCurrencySnapshot extends DataApplicationValidationError {
    val message = "Failed to retrieve latest currency snapshot from L1 node context!"
  }

  @derive(decoder, encoder)
  case object L1CtxCouldNotGetLatestState extends DataApplicationValidationError {
    val message = "Failed to retrieve latest state from L1 node context!"
  }

  @derive(decoder, encoder)
  case object L1CtxFailedToDecodeState extends DataApplicationValidationError {
    val message = "An error was encountered while decoding the state from L1 node context"
  }

  @derive(decoder, encoder)
  case object L1CtxCouldNotGetLatestGlobalSnapshot extends DataApplicationValidationError {
    val message = "Failed to retrieve latest global snapshot from L1 node context!"
  }
}
