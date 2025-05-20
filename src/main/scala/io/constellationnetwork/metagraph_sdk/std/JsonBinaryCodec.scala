package io.constellationnetwork.metagraph_sdk.std

import java.nio.charset.StandardCharsets

import cats.MonadThrow
import cats.data.EitherT
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataUpdate
import io.constellationnetwork.metagraph_sdk.models.CanonicalJson
import io.constellationnetwork.metagraph_sdk.models.CanonicalJson._
import io.constellationnetwork.metagraph_sdk.std.JsonCanonicalizer.JsonPrinterEncodeOps

import io.circe.jawn.JawnParser
import io.circe.{Decoder, Encoder}
import org.bouncycastle.util.encoders.Base64

trait JsonBinaryCodec[F[_], A] {
  def serialize(content: A): F[Array[Byte]]
  def deserialize(bytes: Array[Byte]): F[Either[Throwable, A]]
}

object JsonBinaryCodec {
  def apply[F[_], A](implicit ev: JsonBinaryCodec[F, A]): JsonBinaryCodec[F, A] = ev

  def fromBinary[F[_], A](bytes: Array[Byte])(implicit codec: JsonBinaryCodec[F, A]): F[Either[Throwable, A]] =
    codec.deserialize(bytes)

  implicit def derive[F[_]: MonadThrow, A: Encoder: Decoder]: JsonBinaryCodec[F, A] =
    new JsonBinaryCodec[F, A] {

      def serialize(content: A): F[Array[Byte]] =
        for {
          str   <- content.toCanonical
          bytes <- str.toBinary(jsonBinaryCodecForCanonical[F])
        } yield bytes

      def deserialize(bytes: Array[Byte]): F[Either[Throwable, A]] =
        (for {
          canonical <- EitherT(bytes.fromBinary[CanonicalJson](jsonBinaryCodecForCanonical[F]))
          parsed    <- EitherT.fromEither[F].apply[Throwable, A](JawnParser(false).decode[A](canonical.value))
        } yield parsed).value
    }

  implicit def deriveDataUpdate[F[_]: MonadThrow, U <: DataUpdate: Encoder: Decoder]: JsonBinaryCodec[F, U] =
    new JsonBinaryCodec[F, U] {

      def serialize(content: U): F[Array[Byte]] =
        for {
          str          <- content.toCanonical
          bytes        <- str.toBinary(jsonBinaryCodecForCanonical[F])
          base64String <- Base64.toBase64String(bytes).pure[F]
          prefixedString = s"\u0019Constellation Signed Data:\n${base64String.length}\n$base64String"
          result <- prefixedString.getBytes("UTF-8").pure[F]
        } yield result

      def deserialize(bytes: Array[Byte]): F[Either[Throwable, U]] =
        (for {
          str <- EitherT.right[Throwable](new String(bytes, StandardCharsets.UTF_8).pure[F])

          _ <- EitherT.cond[F](
            str.startsWith("\u0019Constellation Signed Data:\n"),
            (),
            new IllegalArgumentException("Invalid format: Missing prefix")
          )

          parts = str.split("\n", 3)
          _ <- EitherT.cond[F](
            parts.length >= 3,
            (),
            new IllegalArgumentException("Invalid format: Missing parts")
          )

          base64Part = parts(2)
          decodedBytes <- EitherT.fromEither[F](
            Either
              .catchNonFatal(Base64.decode(base64Part))
              .leftMap(e => new IllegalArgumentException(s"Invalid Base64: ${e.getMessage}"): Throwable)
          )

          canonical <- EitherT(decodedBytes.fromBinary[CanonicalJson](jsonBinaryCodecForCanonical[F]))
          parsed    <- EitherT.fromEither[F].apply[Throwable, U](JawnParser(false).decode[U](canonical.value))
        } yield parsed).value
    }

  implicit class JsonBinaryEncodeOps[F[_], A](private val _v: A) extends AnyVal {

    def toBinary(implicit codec: JsonBinaryCodec[F, A]): F[Array[Byte]] =
      codec.serialize(_v)
  }

  implicit class JsonBinaryDecodeOps[F[_]](private val _v: Array[Byte]) extends AnyVal {

    def fromBinary[A](implicit codec: JsonBinaryCodec[F, A]): F[Either[Throwable, A]] =
      codec.deserialize(_v)
  }
}
