package io.constellationnetwork.metagraph_sdk.std

import java.nio.charset.StandardCharsets

import cats.MonadThrow
import cats.syntax.all._

import org.tessellation.currency.dataApplication.DataUpdate

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
          bytes <- str.getBytes("UTF-8").pure[F]
        } yield bytes

      def deserialize(bytes: Array[Byte]): F[Either[Throwable, A]] =
        for {
          str    <- new String(bytes, "UTF-8").pure[F]
          result <- JawnParser(false).decode[A](str).pure[F]
        } yield result
    }

  implicit def deriveDataUpdate[F[_]: MonadThrow, U <: DataUpdate: Encoder: Decoder]: JsonBinaryCodec[F, U] =
    new JsonBinaryCodec[F, U] {

      def serialize(content: U): F[Array[Byte]] =
        for {
          str          <- content.toCanonical
          bytes        <- str.getBytes("UTF-8").pure[F]
          base64String <- Base64.toBase64String(bytes).pure[F]
          prefixedString = s"\u0019Constellation Signed Data:\n${base64String.length}\n$base64String"
          result <- prefixedString.getBytes("UTF-8").pure[F]
        } yield result

      def deserialize(bytes: Array[Byte]): F[Either[Throwable, U]] =
        for {
          str          <- new String(bytes, StandardCharsets.UTF_8).pure[F]
          base64String <- str.split("\n").drop(2).mkString.pure[F]
          bytes        <- Base64.decode(base64String).pure[F]
          jsonStr      <- new String(bytes, "UTF-8").pure[F]
          result       <- JawnParser(false).decode[U](jsonStr).pure[F]
        } yield result
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
