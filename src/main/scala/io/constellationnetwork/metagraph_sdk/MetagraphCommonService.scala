package io.constellationnetwork.metagraph_sdk

import cats.effect.Async
import cats.syntax.functor._

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationBlock
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec._
import io.constellationnetwork.security.signature.Signed

import io.circe.{Decoder, Encoder}
import org.http4s.EntityDecoder
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder

abstract class MetagraphCommonService[F[
  _
], TX <: DataUpdate, PUB <: DataOnChainState, PRV <: DataCalculatedState, Context](implicit
  txEncoder:  Encoder[TX],
  pubEncoder: Encoder[PUB],
  prvEncoder: Encoder[PRV],
  txDecoder:  Decoder[TX],
  pubDecoder: Decoder[PUB],
  prvDecoder: Decoder[PRV],
  async:      Async[F]
) extends DataApplicationSharedContextualOps[F, TX, PUB, PRV, Context] {

  implicit def dataUpdateEncoder: Encoder[DataUpdate] = txEncoder.contramap(_.asInstanceOf[TX])

  implicit def dataUpdateDecoder: Decoder[DataUpdate] = txDecoder.widen

  val signedDataEntityDecoder: EntityDecoder[F, Signed[TX]] = circeEntityDecoder

  def serializeState(state: PUB): F[Array[Byte]] =
    state.toBinary

  def deserializeState(bytes: Array[Byte]): F[Either[Throwable, PUB]] =
    bytes.fromBinary[PUB]

  def serializeCalculatedState(calculatedState: PRV): F[Array[Byte]] =
    calculatedState.toBinary

  def deserializeCalculatedState(bytes: Array[Byte]): F[Either[Throwable, PRV]] =
    bytes.fromBinary[PRV]

  def serializeUpdate(update: TX): F[Array[Byte]] =
    update.toBinary

  def deserializeUpdate(bytes: Array[Byte]): F[Either[Throwable, TX]] =
    bytes.fromBinary[TX]

  def serializeBlock(block: Signed[DataApplicationBlock]): F[Array[Byte]] =
    block.toBinary

  def deserializeBlock(bytes: Array[Byte]): F[Either[Throwable, Signed[DataApplicationBlock]]] =
    bytes.fromBinary[Signed[DataApplicationBlock]]

  def dataEncoder: Encoder[TX] = txEncoder

  def dataDecoder: Decoder[TX] = txDecoder

  def calculatedStateEncoder: Encoder[PRV] = prvEncoder

  def calculatedStateDecoder: Decoder[PRV] = prvDecoder
}
