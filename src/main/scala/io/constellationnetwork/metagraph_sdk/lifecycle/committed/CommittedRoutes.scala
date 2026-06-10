package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.effect.Async
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.MetagraphPublicRoutes
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaProofError, PathNotFound}
import io.constellationnetwork.schema.SnapshotOrdinal

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, HCursor, Json}
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.{HttpRoutes, Response, Uri}

/**
 * Drop-in read-only routes over a [[CommittedReader]]. Every handler performs exactly ONE read of
 * the committed cell and derives its whole response (roots + proof + metadata) from that single
 * [[Committed]] value, so each response is internally consistent.
 *
 *   - `GET  /committed/root`               -> { ordinal, mptRoot, smtRoot, calculatedStateHash }
 *   - `GET  /committed/proof/<key...>`     -> single-key inclusion proof (key segments in the path)
 *   - `POST /committed/proofs`             -> batch proof; body `{ "keys": ["ns/a", ...] }`
 *   - `GET  /committed/proof-prefix/<ns...>` -> complete prefix attestation for a namespace
 *   - `GET  /committed/delta/:ordinal`     -> the [[StateDelta]] for that ordinal (404 once evicted)
 *   - `GET  /committed/snapshot`           -> the [[CommittedSnapshot]] replication fallback
 */
final class CommittedRoutes[F[_]: Async, S](reader: CommittedReader[F, S])(implicit view: CommittedView[S])
    extends MetagraphPublicRoutes[F] {

  private def pathString(path: Uri.Path): String =
    path.segments.map(_.decoded()).mkString("/")

  private def proofError(err: MerklePatriciaProofError): F[Response[F]] =
    err match {
      case PathNotFound(p) => NotFound(Json.obj("error" -> s"key not found: $p".asJson))
      case other           => BadRequest(Json.obj("error" -> other.getMessage.asJson))
    }

  protected val routes: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root / "committed" / "root" =>
      reader.committed.flatMap { c =>
        CommittedCommitment.canonicalCatalogRoot[F](c.roots.mptRoot).flatMap { canonical =>
          Ok(
            Json.obj(
              "ordinal"             -> c.ordinal.asJson,
              "mptRoot"             -> c.roots.mptRoot.asJson,
              "smtRoot"             -> c.roots.smtRoot.asJson,
              "calculatedStateHash" -> CommittedRoots.combine(c.roots.mptRoot, canonical).asJson
            )
          )
        }
      }

    case GET -> Root / "committed" / "snapshot" =>
      reader.committed.flatMap(c => Ok(c.snapshot.asJson))

    case GET -> Root / "committed" / "delta" / LongVar(ordinal) if ordinal >= 0L =>
      reader.committed.flatMap { c =>
        c.deltaFor(SnapshotOrdinal.unsafeApply(ordinal)) match {
          case Some(delta) => Ok(delta.asJson)
          case None =>
            NotFound(Json.obj("error" -> s"no delta retained for ordinal $ordinal; fall back to /committed/snapshot".asJson))
        }
      }

    case GET -> "committed" /: "proof" /: keyPath =>
      CommitKey.from(pathString(keyPath)) match {
        case Left(err) => BadRequest(Json.obj("error" -> err.getMessage.asJson))
        case Right(key) =>
          reader.committed.flatMap { c =>
            c.proveKey(key).flatMap {
              case Left(err) => proofError(err)
              case Right(proof) =>
                Ok(
                  Json.obj(
                    "ordinal" -> c.ordinal.asJson,
                    "mptRoot" -> c.roots.mptRoot.asJson,
                    "key"     -> key.asJson,
                    "keyHex"  -> key.toHex.asJson,
                    "proof"   -> proof.asJson
                  )
                )
            }
          }
      }

    case req @ POST -> Root / "committed" / "proofs" =>
      req.decode[CommittedRoutes.BatchProofRequest] { body =>
        body.keys.traverse(CommitKey.from) match {
          case Left(err) => BadRequest(Json.obj("error" -> err.getMessage.asJson))
          case Right(keys) =>
            reader.committed.flatMap { c =>
              c.proveKeys(keys).flatMap {
                case Left(err) => proofError(err)
                case Right(proof) =>
                  Ok(
                    Json.obj(
                      "ordinal" -> c.ordinal.asJson,
                      "mptRoot" -> c.roots.mptRoot.asJson,
                      "keys"    -> keys.asJson,
                      "proof"   -> proof.asJson
                    )
                  )
              }
            }
        }
      }

    case GET -> "committed" /: "proof-prefix" /: nsPath =>
      CommitNamespace.from(pathString(nsPath)) match {
        case Left(err) => BadRequest(Json.obj("error" -> err.getMessage.asJson))
        case Right(ns) =>
          reader.committed.flatMap { c =>
            c.attestNamespace(ns).flatMap {
              case Left(err) => proofError(err)
              case Right(proof) =>
                Ok(
                  Json.obj(
                    "ordinal"   -> c.ordinal.asJson,
                    "mptRoot"   -> c.roots.mptRoot.asJson,
                    "namespace" -> ns.value.asJson,
                    "prefixHex" -> ns.prefixHex.asJson,
                    "proof"     -> proof.asJson
                  )
                )
            }
          }
      }
  }
}

object CommittedRoutes {

  final case class BatchProofRequest(keys: List[String])

  object BatchProofRequest {

    implicit val decoder: Decoder[BatchProofRequest] = (c: HCursor) => c.downField("keys").as[List[String]].map(BatchProofRequest(_))
  }
}
