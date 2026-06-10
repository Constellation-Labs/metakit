package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import io.constellationnetwork.currency.dataApplication.DataOnChainState

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/**
 * The on-chain state type `CommittedApp.makeL0` actually registers with tessellation: the dev's
 * own `PUB` plus the [[CommittedBreadcrumb]] for the snapshot being produced.
 *
 * This is what makes the breadcrumb CORRECT BY CONSTRUCTION: the service's on-chain
 * encoder/decoder and `combine` are typed over `CommittedOnChain[PUB]`, and only the committed
 * layer ever constructs the wrapper -- the dev's combiner sees and returns plain `PUB`, so the
 * breadcrumb can neither be omitted nor forged from application code. A proposer that tampers
 * with it produces an artifact honest validators cannot reproduce (their own `combine` emits the
 * locally derived breadcrumb), so the proposal cannot gather a majority; and the next round's
 * `combine` re-validates the persisted breadcrumb against the local committed roots
 * ([[CommittedStateError.BreadcrumbMismatch]]).
 */
final case class CommittedOnChain[PUB <: DataOnChainState](inner: PUB, breadcrumb: CommittedBreadcrumb) extends DataOnChainState

object CommittedOnChain {

  implicit def encoder[PUB <: DataOnChainState: Encoder]: Encoder[CommittedOnChain[PUB]] =
    (s: CommittedOnChain[PUB]) =>
      Json.obj(
        "inner"      -> s.inner.asJson,
        "breadcrumb" -> s.breadcrumb.asJson
      )

  implicit def decoder[PUB <: DataOnChainState: Decoder]: Decoder[CommittedOnChain[PUB]] = (c: HCursor) =>
    for {
      inner      <- c.downField("inner").as[PUB]
      breadcrumb <- c.downField("breadcrumb").as[CommittedBreadcrumb]
    } yield CommittedOnChain(inner, breadcrumb)
}
