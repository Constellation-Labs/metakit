package io.constellationnetwork.metagraph_sdk.syntax

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.reflect.ClassTag

import io.constellationnetwork.currency.dataApplication.DataUpdate
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationBlock
import io.constellationnetwork.currency.schema.currency.{CurrencyIncrementalSnapshot, DataApplicationPart}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec.JsonBinaryDecodeOps
import io.constellationnetwork.security.signature.Signed

import io.circe.{Decoder, Encoder}

trait CurrencyIncrementalSnapshotSyntax {

  implicit class CurrencyIncrementalSnapshotOps[F[_]: Sync](cis: CurrencyIncrementalSnapshot)(
    implicit ue: Encoder[DataUpdate],
    ud: Decoder[DataUpdate]
  ) {

    def countUpdates: F[Long] = getBlocks.map(_.map(_.dataTransactions.size.toLong).sum)

    def getBlocks: F[List[Signed[DataApplicationBlock]]] =
      getPart.flatMap {
        _.blocks.traverse { bytes =>
          bytes.fromBinary[Signed[DataApplicationBlock]].flatMap(Sync[F].fromEither)
        }
      }

    def getPart: F[DataApplicationPart] =
      Sync[F].fromOption(cis.dataApplication, new RuntimeException(s"Failed to access Data Application Part"))

    def getSignedUpdates[U <: DataUpdate: ClassTag]: F[List[Signed[U]]] =
      getBlocks.map {
        _.flatMap { signDataBlock =>
          DataUpdate.getDataUpdates(signDataBlock.dataTransactions.toList).collect {
            case Signed(u: U, p) =>
              Signed(u, p)
          }
        }
      }
  }
}
