package io.constellationnetwork.metagraph_sdk

import cats.MonadThrow
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.functor._

import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonCanonicalizer}

import io.circe.parser.parse
import io.circe.{Decoder, Encoder, Json}
import io.estatico.newtype.macros.newtype

package object models {

  @newtype
  case class CanonicalJson(value: String)

  object CanonicalJson {

    /**
     * RAW RFC 8785 canonicalization — does NOT drop null object fields.
     *
     * This is the value-layer primitive (nulls are preserved as real values).
     * Do NOT use it to produce bytes for typed-content hashing or signing:
     * those surfaces must apply the drop-nulls rule first. Route typed
     * content through `JsonBinaryCodec.serialize` / `JsonBinaryHasher`
     * instead. See `docs/content-hash.md`.
     */
    def from[F[_]: MonadThrow, A: Encoder](content: A): F[CanonicalJson] =
      JsonCanonicalizer.make[F].canonicalize(content)

    /**
     * RAW RFC 8785 canonicalization — does NOT drop null object fields.
     * See [[from]] for when this is (and is not) appropriate.
     */
    def fromJson[F[_]: MonadThrow](json: Json): F[CanonicalJson] =
      JsonCanonicalizer.make[F].canonicalize(json)

    def fromJsonString[F[_]: MonadThrow](jsonString: String): F[CanonicalJson] =
      parse(jsonString).fold(
        err => MonadThrow[F].raiseError(err),
        json => fromJson(json)
      )

    def parseJson[F[_]: MonadThrow](canonical: CanonicalJson): F[Json] =
      parse(canonical.value).liftTo[F]

    implicit val encoder: Encoder[CanonicalJson] =
      Encoder.encodeJson.contramap(cj => parse(cj.value).getOrElse(Json.Null))

    implicit val decoder: Decoder[CanonicalJson] = Decoder.decodeJson.emap { json =>
      json.noSpaces.asRight[String].map(CanonicalJson(_))
    }

    implicit def jsonBinaryCodecForCanonical[F[_]: MonadThrow]: JsonBinaryCodec[F, CanonicalJson] =
      new JsonBinaryCodec[F, CanonicalJson] {

        def serialize(content: CanonicalJson): F[Array[Byte]] =
          content.value.getBytes("UTF-8").pure[F]

        def deserialize(bytes: Array[Byte]): F[Either[Throwable, CanonicalJson]] =
          new String(bytes, "UTF-8").pure[F].map { str =>
            parse(str).map(_ => CanonicalJson(str)).leftMap(identity)
          }
      }
  }
}
