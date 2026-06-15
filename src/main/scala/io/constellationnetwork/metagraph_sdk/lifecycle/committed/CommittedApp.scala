package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all._
import cats.{Applicative, Parallel}

import scala.reflect.ClassTag

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.currency.schema.currency.CurrencyIncrementalSnapshot
import io.constellationnetwork.metagraph_sdk.MetagraphCommonService
import io.constellationnetwork.metagraph_sdk.lifecycle.{CombinerService, ValidationService}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.Hashed
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import io.circe.{Decoder, Encoder}
import org.http4s.HttpRoutes

/**
 * The single assembly path for a state-root-committed metagraph L0 data application.
 *
 * [[makeL0]] is the ONLY way to wire a `CommittedState` into a `BaseDataApplicationL0Service` (the
 * `CommittedState` constructor and `make` are package-private), which makes the commitment correct
 * by construction:
 *
 *   - the service's on-chain type is [[CommittedOnChain]]`[PUB]` -- makeL0 owns the on-chain
 *     serializers, so EVERY snapshot's on-chain state carries the constant breadcrumb
 *     `(ordinal, mptRoot, catalogRoot)`; the dev cannot omit it, and `combine` (below) rejects a
 *     forged one.
 *   - `combine` validates the incoming breadcrumb against the locally committed roots
 *     ([[CommittedStateError.BreadcrumbMismatch]] -- the follower-side transition check
 *     `catalogRoot_N = insert(catalogRoot_(N-1), ordinal:<N-1> -> mptRoot_(N-1))`), runs the dev
 *     combiner on the UNWRAPPED state, and emits the next breadcrumb derived from the local
 *     catalog.
 *   - `hashCalculatedState` is `sha256(mptRoot || liveCatalogRoot)` with the catalog root sourced
 *     per tessellation's call ordering (steady-state transition vs O(1) bootstrap from the latest
 *     signed snapshot's breadcrumb) -- see `CommittedState.hashFor`.
 *   - `setCalculatedState` routes through `CommittedState.setCommitted` (delta-apply +
 *     full-rebuild assert + epoch advance; or breadcrumb seed on bootstrap).
 *   - the `/committed/...` routes and any `extraRoutes` read the SAME private cell.
 */
object CommittedApp {

  def makeL0[
    F[+_]: Async: Parallel,
    TX <: DataUpdate: Encoder: Decoder: ClassTag,
    PUB <: DataOnChainState: Encoder: Decoder: ClassTag,
    PRV <: DataCalculatedState: Encoder: Decoder: ClassTag: CommittedView
  ](
    genesisState: DataState[PUB, PRV],
    combiner: CombinerService[F, TX, PUB, PRV],
    validator: ValidationService[F, TX, PUB, PRV],
    journal: CatalogJournal[F],
    extraRoutes: Option[CommittedReader[F, PRV] => HttpRoutes[F]] = None,
    config: CommittedConfig = CommittedConfig.default,
    onConsensusResult: Option[(CommittedReader[F, PRV], Hashed[CurrencyIncrementalSnapshot]) => F[Unit]] = None
  ): F[BaseDataApplicationL0Service[F]] = {
    implicit val wrappedEncoder: Encoder[CommittedOnChain[PUB]] = CommittedOnChain.encoder[PUB]
    implicit val wrappedDecoder: Decoder[CommittedOnChain[PUB]] = CommittedOnChain.decoder[PUB]
    val view = CommittedView[PRV]

    for {
      committedState   <- CommittedState.make[F, PRV](genesisState.calculated, journal, config)
      genesisCommitted <- committedState.committed
      genesisData = DataState(
        CommittedOnChain(genesisState.onChain, genesisCommitted.breadcrumb),
        genesisState.calculated,
        genesisState.sharedArtifacts
      )
    } yield {

      /*
       * The latest SIGNED snapshot's on-chain breadcrumb, when a context (and snapshot) exists.
       * Consensus-attested: the on-chain bytes are part of the signed artifact.
       */
      def contextBreadcrumb(implicit context: L0NodeContext[F]): F[Option[CommittedBreadcrumb]] =
        Option(context)
          .flatTraverse(_.getLastCurrencySnapshot.handleError(_ => none))
          .flatMap {
            _.flatTraverse { hashed =>
              hashed.signed.value.dataApplication.flatTraverse { part =>
                JsonBinaryCodec
                  .fromBinary[F, CommittedOnChain[PUB]](part.onChainState)
                  .map(_.toOption.map(_.breadcrumb).filter(_.ordinal == hashed.signed.value.ordinal))
              }
            }
          }

      BaseDataApplicationL0Service[F, TX, CommittedOnChain[PUB], PRV](
        new MetagraphCommonService[F, TX, CommittedOnChain[PUB], PRV, L0NodeContext[F]]
          with DataApplicationL0Service[F, TX, CommittedOnChain[PUB], PRV] {

          override def genesis: DataState[CommittedOnChain[PUB], PRV] = genesisData

          /** Forward the consensus result to the dev hook (if any), handing it the committed reader. */
          override def onSnapshotConsensusResult(
            snapshot: Hashed[CurrencyIncrementalSnapshot]
          )(implicit A: Applicative[F]): F[Unit] =
            onConsensusResult.traverse_(f => f(committedState, snapshot))

          override def validateData(
            state: DataState[CommittedOnChain[PUB], PRV],
            updates: NonEmptyList[Signed[TX]]
          )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
            validator.validateData(DataState(state.onChain.inner, state.calculated, state.sharedArtifacts), updates)

          override def combine(
            state: DataState[CommittedOnChain[PUB], PRV],
            updates: List[Signed[TX]]
          )(implicit context: L0NodeContext[F]): F[DataState[CommittedOnChain[PUB], PRV]] =
            for {
              combined <- combiner.foldLeft(DataState(state.onChain.inner, state.calculated, state.sharedArtifacts), updates)
              next     <- committedState.advanceWork(state.onChain.breadcrumb, view.entries(combined.calculated))
            } yield DataState(CommittedOnChain(combined.onChain, next), combined.calculated, combined.sharedArtifacts)

          override def getCalculatedState(implicit context: L0NodeContext[F]): F[(SnapshotOrdinal, PRV)] =
            committedState.committed.map(c => c.ordinal -> c.state)

          override def setCalculatedState(ordinal: SnapshotOrdinal, state: PRV)(implicit context: L0NodeContext[F]): F[Boolean] =
            contextBreadcrumb.flatMap(bc => committedState.setCommitted(ordinal, state, bc)).as(true)

          override def hashCalculatedState(state: PRV)(implicit context: L0NodeContext[F]): F[Hash] =
            contextBreadcrumb.flatMap(bc => committedState.hashFor(state, bc))

          override def routes(implicit context: L0NodeContext[F]): HttpRoutes[F] =
            new CommittedRoutes[F, PRV](committedState).public <+>
            extraRoutes.fold(HttpRoutes.empty[F])(f => f(committedState))
        }
      )
    }
  }
}
