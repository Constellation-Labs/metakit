package io.constellationnetwork.metagraph_sdk.crypto

import cats.Order
import cats.syntax.all._

import io.circe.{Decoder, Encoder}

sealed trait SigningScheme {
  def label: String
}

object SigningScheme {

  case object Secp256k1Rfc8785V1 extends SigningScheme {
    val label: String = "secp256k1_rfc8785_v1"
  }

  case object Secp256r1Rfc8785V1 extends SigningScheme {
    val label: String = "secp256r1_rfc8785_v1"
  }

  val all: List[SigningScheme] = List(Secp256k1Rfc8785V1, Secp256r1Rfc8785V1)

  def fromLabel(s: String): Option[SigningScheme] = all.find(_.label === s)

  implicit val encoder: Encoder[SigningScheme] = Encoder.encodeString.contramap(_.label)

  implicit val decoder: Decoder[SigningScheme] = Decoder.decodeString.emap(s =>
    fromLabel(s).toRight(s"Unknown signing scheme: $s")
  )

  implicit val order: Order[SigningScheme] = Order.by(_.label)
}
