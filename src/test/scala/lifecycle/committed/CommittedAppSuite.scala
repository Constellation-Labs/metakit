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
 * `BaseDataApplicationL0Service` whose calculated-state surface is the committed cell --
 * `hashCalculatedState` is the pure derivation, `setCalculatedState` commits (and asserts), and the
 * drop-in `/committed/...` routes serve atomically-consistent reads.
 */
object CommittedAppSuite extends SimpleIOSuite {
  import ToyFixtures._

  // The committed service never touches the node context; tests run without one.
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
    CommittedApp.makeL0[IO, ToyTx, ToyPub, ToyPrv](
      DataState(ToyPub(0), ToyPrv(s0)),
      combiner,
      validator
    )

  private def get(service: BaseDataApplicationL0Service[IO], path: String): IO[Response[IO]] =
    service.routes.orNotFound.run(Request[IO](Method.GET, Uri.unsafeFromString(path)))

  test("hashCalculatedState is the pure two-tier derivation") {
    for {
      service  <- makeService
      h        <- service.hashCalculatedState(ToyPrv(s1))
      expected <- CommittedCommitment.deriveHash[IO, ToyPrv](ToyPrv(s1))
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
      service      <- makeService
      _            <- service.setCalculatedState(ord(1), ToyPrv(s1))
      res          <- get(service, "/committed/root")
      json         <- res.as[Json]
      expectedHash <- CommittedCommitment.deriveHash[IO, ToyPrv](ToyPrv(s1))
    } yield
      expect.all(
        res.status == Status.Ok,
        json.hcursor.downField("ordinal").as[SnapshotOrdinal] == Right(ord(1)),
        json.hcursor.downField("calculatedStateHash").as[String] == Right(expectedHash.value)
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

  test("GET /committed/delta/:ordinal serves retained deltas and 404s evicted ones") {
    for {
      service <- makeService
      _       <- service.setCalculatedState(ord(1), ToyPrv(s1))
      hit     <- get(service, "/committed/delta/1")
      delta   <- hit.as[StateDelta]
      miss    <- get(service, "/committed/delta/999")
    } yield expect.all(hit.status == Status.Ok, delta.ordinal == ord(1), miss.status == Status.NotFound)
  }

  test("GET /committed/snapshot is a valid replication seed") {
    for {
      service  <- makeService
      _        <- service.setCalculatedState(ord(1), ToyPrv(s1))
      res      <- get(service, "/committed/snapshot")
      snapshot <- res.as[CommittedSnapshot]
      replica  <- CommittedReplica.fromSnapshot[IO](snapshot).flatMap(IO.fromEither(_))
    } yield expect.all(res.status == Status.Ok, replica.ordinal == ord(1), replica.roots == snapshot.roots)
  }
}
