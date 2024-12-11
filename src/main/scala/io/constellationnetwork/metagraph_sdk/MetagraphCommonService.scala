package io.constellationnetwork.metagraph_sdk

import cats.effect.Async
import cats.syntax.functor._

import org.tessellation.currency.dataApplication._
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock
import org.tessellation.security.signature.Signed

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec.{simpleJsonDeserialization, simpleJsonSerialization}

import io.circe.{Decoder, Encoder}
import org.http4s.EntityDecoder
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder

abstract class MetagraphCommonService[F[_], TX <: DataUpdate, PUB <: DataOnChainState, PRV <: DataCalculatedState](
  implicit
  txEncoder:       Encoder[TX],
  txDecoder:       Decoder[TX],
  prvEncoder:      Encoder[PRV],
  prvDecoder:      Decoder[PRV],
  val txBinCodec:  JsonBinaryCodec[F, TX],
  val pubBinCodec: JsonBinaryCodec[F, PUB],
  val prvBinCodec: JsonBinaryCodec[F, PRV],
  async:           Async[F]
) {

  val signedDataEntityDecoder: EntityDecoder[F, Signed[TX]] = circeEntityDecoder

  implicit val dataBlockCodec: JsonBinaryCodec[F, Signed[DataApplicationBlock]] =
    new JsonBinaryCodec[F, Signed[DataApplicationBlock]] {
      implicit def dataEncoder: Encoder[DataUpdate] = txEncoder.contramap(_.asInstanceOf[TX])

      implicit def dataDecoder: Decoder[DataUpdate] = txDecoder.widen

      override def serialize(obj: Signed[DataApplicationBlock]): F[Array[Byte]] =
        simpleJsonSerialization(obj)

      override def deserialize(bytes: Array[Byte]): F[Either[Throwable, Signed[DataApplicationBlock]]] =
        simpleJsonDeserialization(bytes)
    }

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
    dataBlockCodec.serialize(block)

  def deserializeBlock(bytes: Array[Byte]): F[Either[Throwable, Signed[DataApplicationBlock]]] =
    dataBlockCodec.deserialize(bytes)

  def dataEncoder: Encoder[TX] = txEncoder

  def dataDecoder: Decoder[TX] = txDecoder

  def calculatedStateEncoder: Encoder[PRV] = prvEncoder

  def calculatedStateDecoder: Decoder[PRV] = prvDecoder
}
