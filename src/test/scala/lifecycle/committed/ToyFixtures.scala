package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.IO

import scala.collection.immutable.SortedMap

import io.constellationnetwork.currency.dataApplication.{DataCalculatedState, DataOnChainState, DataUpdate}

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}

/**
 * A toy application state with a `CommittedView`, plus tessellation-typed wrappers for exercising
 * `CommittedApp.makeL0`. Lives in the production package so suites can reach the package-private
 * assembly internals (`CommittedState.make`) for white-box tests.
 */
object ToyFixtures {

  /**
   * Shared committed-state constructor for the non-bootstrap suites: wires a fresh empty in-memory
   * journal. An empty journal cannot recompose any attested catalog root, so this behaves exactly
   * like the old journal-less default for every seed/unhydrated test.
   */
  def mkCommitted(s: ToyState, config: CommittedConfig = CommittedConfig.default): IO[CommittedState[IO, ToyState]] =
    CatalogJournal.inMemory[IO].flatMap(CommittedState.make[IO, ToyState](s, _, config))

  final case class ToyState(fibers: Map[String, Int], registry: Map[String, String])

  object ToyState {
    val empty: ToyState = ToyState(Map.empty, Map.empty)

    implicit val view: CommittedView[ToyState] = new CommittedView[ToyState] {

      def entries(s: ToyState): SortedMap[CommitKey, Json] =
        SortedMap.from(
          s.fibers.map { case (id, v) => CommitKey.unsafe(s"fiber/$id") -> Json.obj("count" -> v.asJson) } ++
          s.registry.map { case (n, v) => CommitKey.unsafe(s"registry/$n") -> Json.fromString(v) }
        )
    }

    implicit val encoder: Encoder[ToyState] = Encoder.forProduct2("fibers", "registry")(s => (s.fibers, s.registry))
    implicit val decoder: Decoder[ToyState] = Decoder.forProduct2("fibers", "registry")(ToyState.apply)
  }

  final case class ToyTx(value: Int) extends DataUpdate

  object ToyTx {
    implicit val encoder: Encoder[ToyTx] = Encoder.forProduct1("value")(_.value)
    implicit val decoder: Decoder[ToyTx] = Decoder.forProduct1("value")(ToyTx(_))
  }

  final case class ToyPub(updateCount: Int) extends DataOnChainState

  object ToyPub {
    implicit val encoder: Encoder[ToyPub] = Encoder.forProduct1("updateCount")(_.updateCount)
    implicit val decoder: Decoder[ToyPub] = Decoder.forProduct1("updateCount")(ToyPub(_))
  }

  final case class ToyPrv(state: ToyState) extends DataCalculatedState

  object ToyPrv {
    implicit val encoder: Encoder[ToyPrv] = Encoder.forProduct1("state")(_.state)
    implicit val decoder: Decoder[ToyPrv] = Decoder.forProduct1("state")(ToyPrv(_))

    implicit val view: CommittedView[ToyPrv] = new CommittedView[ToyPrv] {
      def entries(s: ToyPrv): SortedMap[CommitKey, Json] = ToyState.view.entries(s.state)
    }
  }
}
