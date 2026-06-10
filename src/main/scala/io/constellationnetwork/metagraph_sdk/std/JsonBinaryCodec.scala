package io.constellationnetwork.metagraph_sdk.std

import java.nio.charset.StandardCharsets

import cats.MonadThrow
import cats.data.EitherT
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication.DataUpdate
import io.constellationnetwork.metagraph_sdk.models.CanonicalJson
import io.constellationnetwork.metagraph_sdk.models.CanonicalJson._

import io.circe.jawn.JawnParser
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}
import org.bouncycastle.util.encoders.Base64

trait JsonBinaryCodec[F[_], A] {
  def serialize(content: A): F[Array[Byte]]
  def deserialize(bytes: Array[Byte]): F[Either[Throwable, A]]
}

/**
 * Exception thrown when JSON nesting depth exceeds the configured limit.
 *
 * This protects against stack overflow attacks from maliciously crafted
 * deeply nested JSON payloads.
 */
case class MaxDepthExceededException(depth: Int, limit: Int)
    extends RuntimeException(s"JSON nesting depth $depth exceeds maximum allowed depth of $limit")

object JsonBinaryCodec {
  def apply[F[_], A](implicit ev: JsonBinaryCodec[F, A]): JsonBinaryCodec[F, A] = ev

  def fromBinary[F[_], A](bytes: Array[Byte])(implicit codec: JsonBinaryCodec[F, A]): F[Either[Throwable, A]] =
    codec.deserialize(bytes)

  /**
   * Default maximum nesting depth for JSON structures.
   * This value balances practical use cases against DoS protection.
   */
  val DefaultMaxDepth: Int = 64

  /**
   * Computes the maximum nesting depth of a JSON value.
   * Uses tail-recursive trampolined approach with explicit work list.
   *
   * @param json The JSON value to measure
   * @return The maximum nesting depth (0 for primitives, 1+ for containers)
   */
  private def jsonDepth(json: Json): Int = {
    import scala.annotation.tailrec

    @tailrec
    def loop(work: List[(Json, Int)], maxDepth: Int): Int =
      work match {
        case Nil => maxDepth
        case (current, depth) :: rest =>
          val newMax = math.max(maxDepth, depth)
          val children = current.arrayOrObject(
            List.empty[(Json, Int)],
            arr => arr.toList.map(elem => (elem, depth + 1)),
            obj => obj.toIterable.toList.map { case (_, v) => (v, depth + 1) }
          )
          loop(children ::: rest, newMax)
      }

    loop(List((json, 0)), 0)
  }

  /**
   * Validates that JSON depth does not exceed the specified limit.
   *
   * @param json The JSON value to validate
   * @param maxDepth Maximum allowed nesting depth
   * @return Either an error or the validated JSON
   */
  private def validateDepth(json: Json, maxDepth: Int): Either[Throwable, Json] = {
    val depth = jsonDepth(json)
    if (depth > maxDepth) Left(MaxDepthExceededException(depth, maxDepth))
    else Right(json)
  }

  /**
   * Recursively removes null values from JSON objects.
   *
   * This ensures that Option[T] fields encoded as null by Circe are omitted
   * from the canonical representation, matching the behavior of JSON serializers
   * that omit undefined/null fields (e.g., TypeScript's JSON.stringify with
   * replacer, or Circe's dropNullValues printer).
   *
   * This is important for signature compatibility: the sender may omit optional
   * fields entirely, while the receiver's decoder fills them with None (encoded
   * as null). Without dropping nulls, the re-encoded JSON would differ from the
   * original, causing signature verification to fail.
   *
   * Note: null values inside arrays are preserved to maintain index positions.
   * Only object field values that are null are removed.
   */
  private def dropNulls(json: Json): Json =
    json.arrayOrObject(
      json,
      arr => Json.fromValues(arr.map(dropNulls)),
      obj =>
        Json.fromJsonObject(
          JsonObject.fromIterable(
            obj.toIterable.collect {
              case (k, v) if !v.isNull => k -> dropNulls(v)
            }
          )
        )
    )

  implicit def derive[F[_]: MonadThrow, A: Encoder: Decoder]: JsonBinaryCodec[F, A] =
    deriveWithMaxDepth[F, A](DefaultMaxDepth)

  /**
   * Derives a JsonBinaryCodec with a custom maximum depth limit.
   *
   * @param maxDepth Maximum allowed JSON nesting depth for deserialization
   */
  def deriveWithMaxDepth[F[_]: MonadThrow, A: Encoder: Decoder](maxDepth: Int): JsonBinaryCodec[F, A] =
    new JsonBinaryCodec[F, A] {

      def serialize(content: A): F[Array[Byte]] =
        for {
          json  <- dropNulls(content.asJson).pure[F]
          str   <- JsonCanonicalizer.canonicalizeJson[F](json)
          bytes <- str.toBinary(jsonBinaryCodecForCanonical[F])
        } yield bytes

      def deserialize(bytes: Array[Byte]): F[Either[Throwable, A]] =
        (for {
          canonical <- EitherT(bytes.fromBinary[CanonicalJson](jsonBinaryCodecForCanonical[F]))
          json      <- EitherT.fromEither[F](JawnParser(false).parse(canonical.value))
          _         <- EitherT.fromEither[F](validateDepth(json, maxDepth))
          parsed    <- EitherT.fromEither[F].apply[Throwable, A](json.as[A].leftMap(_.fillInStackTrace()))
        } yield parsed).value
    }

  implicit def deriveDataUpdate[F[_]: MonadThrow, U <: DataUpdate: Encoder: Decoder]: JsonBinaryCodec[F, U] =
    deriveDataUpdateWithMaxDepth[F, U](DefaultMaxDepth)

  /**
   * Derives a DataUpdate JsonBinaryCodec with a custom maximum depth limit.
   *
   * @param maxDepth Maximum allowed JSON nesting depth for deserialization
   */
  def deriveDataUpdateWithMaxDepth[F[_]: MonadThrow, U <: DataUpdate: Encoder: Decoder](maxDepth: Int): JsonBinaryCodec[F, U] =
    new JsonBinaryCodec[F, U] {

      def serialize(content: U): F[Array[Byte]] =
        for {
          json         <- dropNulls(content.asJson).pure[F]
          str          <- JsonCanonicalizer.canonicalizeJson[F](json)
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
          json      <- EitherT.fromEither[F](JawnParser(false).parse(canonical.value))
          _         <- EitherT.fromEither[F](validateDepth(json, maxDepth))
          parsed    <- EitherT.fromEither[F].apply[Throwable, U](json.as[U].leftMap(_.fillInStackTrace()))
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
