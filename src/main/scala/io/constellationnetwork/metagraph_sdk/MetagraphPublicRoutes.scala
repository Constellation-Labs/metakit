package io.constellationnetwork.metagraph_sdk

import cats.effect.Concurrent
import cats.implicits.{toBifunctorOps, toFlatMapOps, toFunctorOps}

import io.constellationnetwork.currency.dataApplication.DataApplicationValidationError
import io.constellationnetwork.metagraph_sdk.MetagraphPublicRoutes.{DataApp404, StringError}
import io.constellationnetwork.routes.internal.{InternalUrlPrefix, PublicRoutes}

import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive
import eu.timepit.refined.auto._
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.CORS
import org.http4s.{HttpRoutes, Response}

abstract class MetagraphPublicRoutes[F[_]: Concurrent] extends Http4sDsl[F] with PublicRoutes[F] {

  override lazy val public: HttpRoutes[F] =
    CORS.policy
      .withAllowCredentials(false)
      .httpRoutes(routes)

  protected val routes: HttpRoutes[F]

  override protected def prefixPath: InternalUrlPrefix = "/"

  protected def prepareResponse[T: Encoder](result: Either[DataApplicationValidationError, T]): F[Response[F]] =
    result match {
      case Left(ex: DataApp404) => NotFound(ex.message)
      case Left(ex)             => BadRequest(ex.message)
      case Right(value)         => Ok(value.asJson)
    }

  implicit class dataAppErrorResponseOps[T: Encoder](result: F[Either[DataApplicationValidationError, T]]) {
    def toResponse: F[Response[F]] = result.flatMap(prepareResponse(_))
  }

  implicit class throwableResponseOps[T: Encoder](result: F[Either[Throwable, T]]) {

    def toResponse: F[Response[F]] = result
      .map(_.leftMap(e => StringError(e.getMessage)))
      .flatMap(prepareResponse(_))
  }
}

object MetagraphPublicRoutes {

  @derive(decoder, encoder)
  case class StringError(message: String) extends DataApplicationValidationError

  @derive(decoder, encoder)
  case class DataApp404(message: String) extends DataApplicationValidationError
}
