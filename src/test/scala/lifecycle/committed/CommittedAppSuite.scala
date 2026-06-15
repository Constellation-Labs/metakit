package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.currency.dataApplication._
import io.constellationnetwork.currency.dataApplication.dataApplication.DataApplicationValidationErrorOr
import io.constellationnetwork.metagraph_sdk.lifecycle.{CombinerService, ValidationService}
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.signature.Signed

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.http4s._
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.implicits._
import weaver.SimpleIOSuite

/**
 * End-to-end over the single assembly path: `CommittedApp.makeL0` produces a
 * `BaseDataApplicationL0Service` whose on-chain type carries the breadcrumb (emitted by `combine`,
 * validated against the local commitment), whose `hashCalculatedState` commits the live catalog,
 * and whose drop-in `/committed/...` routes serve atomically-consistent reads.
 */
object CommittedAppSuite extends SimpleIOSuite {
  import ToyFixtures._

  // The committed service reads the node context defensively; tests run without one.
  implicit private val ctx: L0NodeContext[IO] = null

  private def ord(n: Long): SnapshotOrdinal = SnapshotOrdinal.unsafeApply(n)

  private val s0 = ToyState(Map("aaa" -> 1, "bbb" -> 2), Map("alpha" -> "x"))
  private val s1 = ToyState(Map("aaa" -> 5, "ccc" -> 3), Map("alpha" -> "x", "beta" -> "y"))

  private val combiner: CombinerService[IO, ToyTx, ToyPub, ToyPrv] =
    new CombinerService[IO, ToyTx, ToyPub, ToyPrv] {

      def insert(previous: DataState[ToyPub, ToyPrv], update: Signed[ToyTx])(
        implicit ctx: L0NodeContext[IO]
      ): IO[DataState[ToyPub, ToyPrv]] =
        previous.copy(onChain = ToyPub(previous.onChain.updateCount + 1)).pure[IO]
    }

  private val validator: ValidationService[IO, ToyTx, ToyPub, ToyPrv] =
    new ValidationService[IO, ToyTx, ToyPub, ToyPrv] {

      def validateUpdate(update: ToyTx)(implicit ctx: L1NodeContext[IO]): IO[DataApplicationValidationErrorOr[Unit]] =
        ().validNec[DataApplicationValidationError].pure[IO]

      def validateSignedUpdate(current: DataState[ToyPub, ToyPrv], signedUpdate: Signed[ToyTx])(
        implicit ctx: L0NodeContext[IO]
      ): IO[DataApplicationValidationErrorOr[Unit]] =
        ().validNec[DataApplicationValidationError].pure[IO]
    }

  private def makeService: IO[BaseDataApplicationL0Service[IO]] =
    CatalogJournal.inMemory[IO].flatMap { j =>
      CommittedApp.makeL0[IO, ToyTx, ToyPub, ToyPrv](
        DataState(ToyPub(0), ToyPrv(s0)),
        combiner,
        validator,
        journal = j
      )
    }

  private def get(service: BaseDataApplicationL0Service[IO], path: String): IO[Response[IO]] =
    service.routes.orNotFound.run(Request[IO](Method.GET, Uri.unsafeFromString(path)))

  test("genesis on-chain state carries the genesis breadcrumb (correct by construction)") {
    for {
      service  <- makeService
      expected <- mkCommitted(s0).flatMap(_.committed)
    } yield
      service.genesis.onChain match {
        case CommittedOnChain(ToyPub(0), breadcrumb) =>
          expect.all(
            breadcrumb.ordinal == SnapshotOrdinal.MinValue,
            breadcrumb.roots.mptRoot == expected.roots.mptRoot,
            breadcrumb.roots.catalogRoot == expected.roots.catalogRoot
          )
        case other => failure(s"genesis onChain is not a CommittedOnChain with the dev PUB inside: $other")
      }
  }

  test("combine emits the next breadcrumb; the dev combiner sees only the inner PUB") {
    for {
      service <- makeService
      out     <- service.combine(service.genesis, List.empty)
      // committing the same transition produces the same breadcrumb
      reference <- mkCommitted(s0).flatMap(st => st.setCommitted(ord(1), s0))
    } yield
      out.onChain match {
        case CommittedOnChain(ToyPub(0), breadcrumb) =>
          expect.all(breadcrumb.ordinal == ord(1), breadcrumb.roots == reference.roots)
        case other => failure(s"combine did not emit a CommittedOnChain: $other")
      }
  }

  test("combine resolves the genesis breadcrumb after the cell advanced (tessellation re-run; the e2e stall)") {
    // tessellation interleaves combine and setCalculatedState and RE-RUNS combine(parent=genesis)
    // for the first incremental AFTER setCalculatedState has advanced the cell. Reproduce that: advance
    // the cell to ordinal 1 WITHOUT going through combine (so the work cache never holds the genesis
    // breadcrumb), then combine from genesis. Only the empty-catalog fallback in resolveCatalog can
    // resolve the genesis breadcrumb here (the cell is past it, work is empty, and the journal — which
    // now records ordinal 0 — recomposes to a non-empty catalog). Without it: BreadcrumbUnresolvable
    // and the metagraph stalls at ordinal 1.
    for {
      service <- makeService
      _       <- service.setCalculatedState(ord(1), ToyPrv(s0))
      rerun   <- service.combine(service.genesis, List.empty).attempt
    } yield expect(rerun.isRight)
  }

  test("combine rejects a forged incoming breadcrumb (follower-side transition validation)") {
    for {
      service <- makeService
      honest = service.genesis
      forgedBreadcrumb = honest.onChain match {
        case CommittedOnChain(inner, b: CommittedBreadcrumb) =>
          CommittedOnChain(
            inner.asInstanceOf[ToyPub],
            b.copy(roots = b.roots.copy(mptRoot = io.constellationnetwork.security.hash.Hash.empty))
          )
        case other => throw new RuntimeException(s"unexpected onChain: $other")
      }
      result <- service.combine(honest.copy(onChain = forgedBreadcrumb), List.empty).attempt
    } yield expect(result.left.exists(_.isInstanceOf[CommittedStateError.BreadcrumbMismatch]))
  }

  test("hashCalculatedState commits the live catalog: the transition hash, not a pure-value hash") {
    for {
      service  <- makeService
      h        <- service.hashCalculatedState(ToyPrv(s1))
      expected <- mkCommitted(s0).flatMap(_.hashFor(s1, None))
    } yield expect(h == expected)
  }

  test("setCalculatedState commits through the cell; getCalculatedState reads it back") {
    for {
      service <- makeService
      ok      <- service.setCalculatedState(ord(1), ToyPrv(s1))
      result  <- service.getCalculatedState
    } yield expect.all(ok, result._1 == ord(1), result._2 == ToyPrv(s1))
  }

  test("GET /committed/root reflects the committed ordinal, roots, and consensus hash") {
    for {
      service   <- makeService
      _         <- service.setCalculatedState(ord(1), ToyPrv(s1))
      res       <- get(service, "/committed/root")
      json      <- res.as[Json]
      reference <- mkCommitted(s0).flatMap(st => st.setCommitted(ord(1), s1))
    } yield
      expect.all(
        res.status == Status.Ok,
        json.hcursor.downField("ordinal").as[SnapshotOrdinal] == Right(ord(1)),
        json.hcursor.downField("calculatedStateHash").as[String] == Right(reference.roots.combinedHash.value),
        json.hcursor.downField("hydrated").as[Boolean] == Right(true)
      )
  }

  test("GET /committed/proof/<key> proves a key; invalid keys are 400, absent keys 404") {
    for {
      service <- makeService
      found   <- get(service, "/committed/proof/fiber/aaa")
      json    <- found.as[Json]
      missing <- get(service, "/committed/proof/fiber/zzz")
      invalid <- get(service, "/committed/proof/FIBER/aaa")
    } yield
      expect.all(
        found.status == Status.Ok,
        json.hcursor.downField("key").as[String] == Right("fiber/aaa"),
        json.hcursor.downField("proof").succeeded,
        missing.status == Status.NotFound,
        invalid.status == Status.BadRequest
      )
  }

  test("POST /committed/proofs returns one batch proof for many keys") {
    for {
      service <- makeService
      req = Request[IO](Method.POST, uri"/committed/proofs")
        .withEntity(Json.obj("keys" -> List("fiber/aaa", "registry/alpha").asJson))
      res  <- service.routes.orNotFound.run(req)
      json <- res.as[Json]
    } yield
      expect.all(
        res.status == Status.Ok,
        json.hcursor.downField("proof").downField("paths").as[List[String]].exists(_.size == 2)
      )
  }

  test("GET /committed/proof-prefix/<ns> returns the namespace attestation") {
    for {
      service <- makeService
      res     <- get(service, "/committed/proof-prefix/fiber")
      json    <- res.as[Json]
    } yield
      expect.all(
        res.status == Status.Ok,
        json.hcursor.downField("namespace").as[String] == Right("fiber"),
        json.hcursor.downField("proof").downField("paths").as[List[String]].exists(_.size == 2)
      )
  }

  test("GET /committed/proof-ordinal/:ordinal serves a verifiable catalog attestation") {
    for {
      service <- makeService
      _       <- service.setCalculatedState(ord(1), ToyPrv(s1))
      res     <- get(service, "/committed/proof-ordinal/0")
      json    <- res.as[Json]
      proof   <- IO.fromEither(json.hcursor.downField("proof").as[OrdinalCatalogProof])
      root    <- IO.fromEither(json.hcursor.downField("catalogRoot").as[io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleRoot])
      attestation <- OrdinalCatalogProofVerifier
        .verify[IO](root, proof, CommittedConfig.DefaultEpochSize)
        .flatMap(IO.fromEither(_))
      genesisRoots <- mkCommitted(s0).flatMap(_.committed)
    } yield
      expect.all(
        res.status == Status.Ok,
        attestation == OrdinalAttestation.CommittedAt(0L, genesisRoots.roots.mptRoot)
      )
  }

  test("GET /committed/delta/:ordinal serves retained deltas and 404s evicted ones") {
    for {
      service <- makeService
      _       <- service.setCalculatedState(ord(1), ToyPrv(s1))
      hit     <- get(service, "/committed/delta/1")
      delta   <- hit.as[StateDelta]
      miss    <- get(service, "/committed/delta/999")
    } yield expect.all(hit.status == Status.Ok, delta.ordinal == ord(1), miss.status == Status.NotFound)
  }

  test("GET /committed/snapshot is a valid replication seed; GET /committed/catalog matches it") {
    for {
      service  <- makeService
      _        <- service.setCalculatedState(ord(1), ToyPrv(s1))
      res      <- get(service, "/committed/snapshot")
      snapshot <- res.as[CommittedSnapshot]
      catRes   <- get(service, "/committed/catalog")
      contents <- catRes.as[CatalogContents]
      replica  <- CommittedReplica.fromSnapshot[IO](snapshot).flatMap(IO.fromEither(_))
    } yield
      expect.all(
        res.status == Status.Ok,
        replica.ordinal == ord(1),
        replica.roots == snapshot.roots,
        contents == snapshot.catalog
      )
  }

  test("CommittedOnChain round-trips through the service's on-chain codec") {
    for {
      service <- makeService
      bytes   <- service.serializeState(service.genesis.onChain)
      decoded <- service.deserializeState(bytes)
    } yield expect(decoded == Right(service.genesis.onChain))
  }
}
