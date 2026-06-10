package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.Parallel
import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all._

import scala.reflect.ClassTag

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.metagraph_sdk.MetagraphCommonService
import io.constellationnetwork.metagraph_sdk.lifecycle.{CombinerService, ValidationService}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.signature.Signed

import io.circe.{Decoder, Encoder}
import org.http4s.HttpRoutes

/**
 * The single assembly path for a state-root-committed metagraph L0 data application.
 *
 * [[makeL0]] is the ONLY way to wire a `CommittedState` into a `BaseDataApplicationL0Service`
 * (the `CommittedState` constructor and `make` are package-private), which makes the commitment
 * correct by construction:
 *
 *   - `hashCalculatedState` is the PURE derivation `CommittedCommitment.deriveHash` -- a function
 *     of the state value alone (see that object's scaladoc for why the catalog in the hash is the
 *     canonical single-entry one).
 *   - `setCalculatedState` routes through `CommittedState.setCommitted`, which applies the view's
 *     delta incrementally and ASSERTS the resulting trie root equals the value-derived root,
 *     raising [[CommittedStateError.RootDivergence]] (failing the snapshot loudly) on any mismatch
 *     -- a divergence is a wiring bug, never something to paper over.
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
    extraRoutes: Option[CommittedReader[F, PRV] => HttpRoutes[F]] = None,
    maxRecentDeltas: Int = CommittedState.DefaultMaxRecentDeltas
  ): F[BaseDataApplicationL0Service[F]] =
    CommittedState.make[F, PRV](genesisState.calculated, maxRecentDeltas).map { committedState =>
      BaseDataApplicationL0Service[F, TX, PUB, PRV](
        new MetagraphCommonService[F, TX, PUB, PRV, L0NodeContext[F]] with DataApplicationL0Service[F, TX, PUB, PRV] {

          override def genesis: DataState[PUB, PRV] = genesisState

          override def validateData(
            state: DataState[PUB, PRV],
            updates: NonEmptyList[Signed[TX]]
          )(implicit context: L0NodeContext[F]): F[DataApplicationValidationErrorOr[Unit]] =
            validator.validateData(state, updates)

          override def combine(
            state: DataState[PUB, PRV],
            updates: List[Signed[TX]]
          )(implicit context: L0NodeContext[F]): F[DataState[PUB, PRV]] =
            combiner.foldLeft(state, updates)

          override def getCalculatedState(implicit context: L0NodeContext[F]): F[(SnapshotOrdinal, PRV)] =
            committedState.committed.map(c => c.ordinal -> c.state)

          override def setCalculatedState(ordinal: SnapshotOrdinal, state: PRV)(implicit
            context: L0NodeContext[F]
          ): F[Boolean] =
            committedState.setCommitted(ordinal, state).as(true)

          override def hashCalculatedState(state: PRV)(implicit context: L0NodeContext[F]): F[Hash] =
            CommittedCommitment.deriveHash[F, PRV](state)

          override def routes(implicit context: L0NodeContext[F]): HttpRoutes[F] =
            new CommittedRoutes[F, PRV](committedState).public <+>
            extraRoutes.fold(HttpRoutes.empty[F])(f => f(committedState))
        }
      )
    }
}
