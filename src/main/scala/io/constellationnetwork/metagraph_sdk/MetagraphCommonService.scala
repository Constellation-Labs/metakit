package io.constellationnetwork.metagraph_sdk

import cats.effect.Async

import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock
import org.tessellation.currency.dataApplication.{DataCalculatedState, DataOnChainState, DataUpdate}
import org.tessellation.security.signature.Signed

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec

import io.circe.{Decoder, Encoder}
import org.http4s.EntityDecoder
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder

abstract class MetagraphCommonService[F[_], TX <: DataUpdate, PUB <: DataOnChainState, PRV <: DataCalculatedState](
  implicit
  txEncoder:     Encoder[TX],
  txDecoder:     Decoder[TX],
  prvEncoder:    Encoder[PRV],
  prvDecoder:    Decoder[PRV],
  txBinCodec:    JsonBinaryCodec[F, TX],
  pubBinCodec:   JsonBinaryCodec[F, PUB],
  prvBinCodec:   JsonBinaryCodec[F, PRV],
  blockBinCodec: JsonBinaryCodec[F, Signed[DataApplicationBlock]],
  async:         Async[F]
) {

  val signedDataEntityDecoder: EntityDecoder[F, Signed[TX]] = circeEntityDecoder

  def serializeState(state: PUB): F[Array[Byte]] =
    pubBinCodec.serialize(state)

  def deserializeState(bytes: Array[Byte]): F[Either[Throwable, PUB]] =
    pubBinCodec.deserialize(bytes)

  def serializeCalculatedState(calculatedState: PRV): F[Array[Byte]] =
    prvBinCodec.serialize(calculatedState)

  def deserializeCalculatedState(bytes: Array[Byte]): F[Either[Throwable, PRV]] =
    prvBinCodec.deserialize(bytes)

  def serializeUpdate(update: TX): F[Array[Byte]] =
    txBinCodec.serialize(update)

  def deserializeUpdate(bytes: Array[Byte]): F[Either[Throwable, TX]] =
    txBinCodec.deserialize(bytes)

  def serializeBlock(block: Signed[DataApplicationBlock]): F[Array[Byte]] =
    blockBinCodec.serialize(block)

  def deserializeBlock(bytes: Array[Byte]): F[Either[Throwable, Signed[DataApplicationBlock]]] =
    blockBinCodec.deserialize(bytes)

  def dataEncoder: Encoder[TX] = txEncoder

  def dataDecoder: Decoder[TX] = txDecoder

  def calculatedStateEncoder: Encoder[PRV] = prvEncoder

  def calculatedStateDecoder: Decoder[PRV] = prvDecoder
}
