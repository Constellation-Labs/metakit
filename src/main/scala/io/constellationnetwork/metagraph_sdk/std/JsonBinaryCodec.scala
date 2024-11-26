package io.constellationnetwork.metagraph_sdk.std

import java.nio.charset.StandardCharsets

import cats.effect.Sync
import cats.implicits.{toFlatMapOps, toFunctorOps}

import org.tessellation.currency.dataApplication.DataUpdate
import org.tessellation.currency.dataApplication.dataApplication.DataApplicationBlock
import org.tessellation.currency.schema.currency.CurrencyIncrementalSnapshot
import org.tessellation.schema.ID.Id
import org.tessellation.security.signature.Signed

import io.circe.jawn.JawnParser
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Printer}
import org.bouncycastle.util.encoders.Base64

trait JsonBinaryCodec[F[_], A] {
  def serialize(content:   A): F[Array[Byte]]
  def deserialize(content: Array[Byte]): F[Either[Throwable, A]]
}

object JsonBinaryCodec {

  private val printer = Printer(dropNullValues = true, indent = "", sortKeys = true)

  def apply[F[_], A](implicit ev: JsonBinaryCodec[F, A]): JsonBinaryCodec[F, A] = ev

  def simpleJsonSerialization[F[_]: Sync, A: Encoder](content: A): F[Array[Byte]] =
    Sync[F].delay(content.asJson.printWith(printer).getBytes("UTF-8"))

  def simpleJsonDeserialization[F[_]: Sync, A: Decoder](content: Array[Byte]): F[Either[Throwable, A]] =
    Sync[F].delay(JawnParser(false).decodeByteArray[A](content))

  def serializeDataUpdate[F[_]: Sync, U <: DataUpdate: Encoder](content: U): F[Array[Byte]] = for {
    jsonBytes    <- simpleJsonSerialization(content)
    base64String <- Sync[F].delay(Base64.toBase64String(jsonBytes))
    prefixedString = s"\u0019Constellation Signed Data:\n${base64String.length}\n$base64String"
  } yield prefixedString.getBytes("UTF-8")

  def deserializeDataUpdate[F[_]: Sync, U <: DataUpdate: Decoder](
    bytes: Array[Byte]
  ): F[Either[Throwable, DataUpdate]] = for {
    base64String <- Sync[F].pure(new String(bytes, StandardCharsets.UTF_8).split("\n").drop(2).mkString)
    jsonBytes    <- Sync[F].delay(Base64.decode(base64String))
    result       <- simpleJsonDeserialization(jsonBytes)
  } yield result

  implicit def dataBlockBinaryCodec[F[_]: Sync](implicit
    updEnc: Encoder[DataUpdate],
    updDec: Decoder[DataUpdate]
  ): JsonBinaryCodec[F, Signed[DataApplicationBlock]] =
    new JsonBinaryCodec[F, Signed[DataApplicationBlock]] {

      override def serialize(obj: Signed[DataApplicationBlock]): F[Array[Byte]] =
        simpleJsonSerialization(obj)

      override def deserialize(bytes: Array[Byte]): F[Either[Throwable, Signed[DataApplicationBlock]]] =
        simpleJsonDeserialization(bytes)
    }

  implicit def currencyIncrementalSnapshotCodec[F[_]: Sync]: JsonBinaryCodec[F, CurrencyIncrementalSnapshot] =
    new JsonBinaryCodec[F, CurrencyIncrementalSnapshot] {

      override def serialize(obj: CurrencyIncrementalSnapshot): F[Array[Byte]] =
        simpleJsonSerialization(obj)

      override def deserialize(bytes: Array[Byte]): F[Either[Throwable, CurrencyIncrementalSnapshot]] =
        simpleJsonDeserialization(bytes)
    }

  implicit def idCodec[F[_]: Sync]: JsonBinaryCodec[F, Id] =
    new JsonBinaryCodec[F, Id] {

      override def serialize(obj: Id): F[Array[Byte]] =
        simpleJsonSerialization(obj)

      override def deserialize(bytes: Array[Byte]): F[Either[Throwable, Id]] =
        simpleJsonDeserialization(bytes)
    }
}
