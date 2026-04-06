package io.constellationnetwork.metagraph_sdk.crypto

import cats.Order
import cats.syntax.all._

import io.constellationnetwork.schema.ID.Id
import io.constellationnetwork.security.signature.signature.{Signature, SignatureProof}

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

case class MetagraphSignatureProof(
  id: Id,
  signature: Signature,
  scheme: Option[SigningScheme]
) {

  def toTessellation: SignatureProof =
    SignatureProof(id, signature)
}

object MetagraphSignatureProof {

  def fromTessellation(proof: SignatureProof): MetagraphSignatureProof =
    MetagraphSignatureProof(proof.id, proof.signature, none)

  def fromTessellation(proof: SignatureProof, scheme: SigningScheme): MetagraphSignatureProof =
    MetagraphSignatureProof(proof.id, proof.signature, scheme.some)

  implicit val encoder: Encoder[MetagraphSignatureProof] = Encoder.instance { proof =>
    val base = Json.obj(
      "id"        -> proof.id.asJson,
      "signature" -> proof.signature.asJson
    )
    proof.scheme.fold(base)(s => base.deepMerge(Json.obj("scheme" -> s.asJson)))
  }

  implicit val decoder: Decoder[MetagraphSignatureProof] = Decoder.instance { c =>
    for {
      id        <- c.downField("id").as[Id]
      signature <- c.downField("signature").as[Signature]
      scheme    <- c.downField("scheme").as[Option[SigningScheme]]
    } yield MetagraphSignatureProof(id, signature, scheme)
  }

  implicit val order: Order[MetagraphSignatureProof] = Order.by(p => (p.id, p.signature))
}
