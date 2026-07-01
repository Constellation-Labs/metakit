package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleRoot
import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.InMemorySparseMerkleTree
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax._
import io.circe.{Json, JsonObject, Printer}

/**
 * Generator for the cross-language ORDINAL-CATALOG conformance vectors
 * (`src/test/resources/conformance/ordinal_catalog_test_vectors.json`).
 *
 * Builds one deterministic [[EpochCatalog]] (small `epochSize` so sealing happens quickly), then for
 * a spread of ordinals — hot inclusions, sealed/ancient inclusions, an epoch boundary, and an absent
 * ordinal — serves the real [[OrdinalCatalogProof]] and RUNS THE REFERENCE VERIFIER
 * ([[OrdinalCatalogProofVerifier.verify]]) to produce `expected`. A handful of negative cases mutate
 * a good proof's JSON and record the reference verifier's error outcome (at component granularity).
 * Every `expected` is produced by the Scala reference, never hand-computed. Re-run with:
 *
 *   sbt "Test/runMain io.constellationnetwork.metagraph_sdk.lifecycle.committed.OrdinalCatalogVectorGenerator"
 */
object OrdinalCatalogVectorGenerator extends IOApp {

  private val OutPath = "src/test/resources/conformance/ordinal_catalog_test_vectors.json"

  private val config = CommittedConfig(epochSize = 4, sealedEpochRetention = 8)

  /** Deterministic per-ordinal MPT root: root(n) = sha256("mpt:n"). */
  private def mptRoot(n: Long): Hash = Hash.fromBytes(s"mpt:$n".getBytes(StandardCharsets.UTF_8))

  /** Highest committed ordinal. epochSize 4 -> epoch 0 = {0,1,2,3} sealed, hot epoch 1 = {4,5,6}. */
  private val LastOrdinal = 6L
  private val CurrentMpt = mptRoot(9999)

  private def buildCatalog: IO[(EpochCatalog[IO], InMemorySparseMerkleTree[IO], SparseMerkleRoot)] =
    for {
      cat0 <- EpochCatalog.empty[IO](config)
      cat  <- (0L to LastOrdinal).toList.foldLeftM(cat0)((c, o) => c.advance(o, mptRoot(o)).map(_._1))
      composed <- cat.compose(CurrentMpt)
    } yield (cat, composed._1, composed._2)

  private def attestationJson(att: OrdinalAttestation): Json = att match {
    case OrdinalAttestation.CommittedAt(ordinal, mptRoot) =>
      Json.obj("type" := "CommittedAt", "ordinal" := ordinal, "mptRoot" := mptRoot.value)
    case OrdinalAttestation.NotCommitted(ordinal) =>
      Json.obj("type" := "NotCommitted", "ordinal" := ordinal)
  }

  private def errorJson(err: CommittedProofError): Json = err match {
    case CommittedProofError.WrongProofKey(component, _, _) =>
      Json.obj("error" := "WrongProofKey", "component" := component)
    case CommittedProofError.ProofInvalid(component, _) =>
      Json.obj("error" := "ProofInvalid", "component" := component)
    case _: CommittedProofError.MalformedOrdinalProof =>
      Json.obj("error" := "MalformedOrdinalProof")
    case other =>
      Json.obj("error" := other.getClass.getSimpleName)
  }

  /** A positive/absent case: serve the proof, run the reference verifier, emit the tuple. */
  private def positiveCase(
    cat: EpochCatalog[IO],
    top: InMemorySparseMerkleTree[IO],
    catalogRoot: SparseMerkleRoot,
    ordinal: Long,
    note: String
  ): IO[Json] =
    for {
      proof <- cat.proveOrdinal(ordinal, top).flatMap(IO.fromEither(_))
      att   <- OrdinalCatalogProofVerifier.verify[IO](catalogRoot, proof, config.epochSize).flatMap(IO.fromEither(_))
      _     <- IO.println(f"[positive] ordinal=$ordinal%6d  -> ${attestationJson(att).noSpaces}")
    } yield Json.fromJsonObject(
      JsonObject.fromIterable(
        List("ordinal" := ordinal, "note" := note, "proof" -> proof.asJson, "expected" -> attestationJson(att))
      )
    )

  private def flipFirstHexChar(s: String): String = {
    val c = s.head
    (if (c == '0') '1' else '0').toString + s.tail
  }

  /** A negative case: mutate a good proof's JSON, run the reference verifier, record the error. */
  private def negativeCase(
    cat: EpochCatalog[IO],
    top: InMemorySparseMerkleTree[IO],
    catalogRoot: SparseMerkleRoot,
    ordinal: Long,
    note: String
  )(mutate: Json => Json): IO[Json] =
    for {
      proof <- cat.proveOrdinal(ordinal, top).flatMap(IO.fromEither(_))
      mutatedJson = mutate(proof.asJson)
      mutated <- IO.fromEither(mutatedJson.as[OrdinalCatalogProof])
      result  <- OrdinalCatalogProofVerifier.verify[IO](catalogRoot, mutated, config.epochSize)
      expected <- result match {
        case Left(err) => IO.pure(errorJson(err))
        case Right(att) =>
          IO.raiseError(new RuntimeException(s"negative case ordinal=$ordinal expected an error but verified: ${attestationJson(att).noSpaces}"))
      }
      _ <- IO.println(f"[negative] ordinal=$ordinal%6d  -> ${expected.noSpaces}  ($note)")
    } yield Json.fromJsonObject(
      JsonObject.fromIterable(
        List("ordinal" := ordinal, "note" := note, "proof" -> mutatedJson, "expected" -> expected)
      )
    )

  private def modField(json: Json, field: String, f: Json => Json): Json =
    json.hcursor.downField(field).withFocus(f).top.getOrElse(json)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      built <- buildCatalog
      (cat, top, catalogRoot) = built

      positives <- List(
        (0L, "sealed epoch 0, first ordinal"),
        (1L, "sealed epoch 0 (two-level inclusion via sealedEntry)"),
        (3L, "sealed epoch 0, last ordinal of the epoch"),
        (4L, "epoch boundary: first ordinal of hot epoch 1"),
        (5L, "hot epoch 1 inclusion (sealedEntry null)"),
        (6L, "hot epoch 1, last ordinal"),
        (999L, "absent ordinal: epoch never sealed and not hot -> NotCommitted")
      ).traverse { case (o, note) => positiveCase(cat, top, catalogRoot, o, note) }

      negatives <- List(
        // Tamper a hot inclusion's value -> the SMT fold fails on component "hot".
        negativeCase(cat, top, catalogRoot, 5L, "tampered hot inclusion value -> ProofInvalid(hot)") { j =>
          modField(j, "hot", hot => modField(hot, "value", v => Json.fromString(flipFirstHexChar(v.asString.get))))
        },
        // Replace the hot proof's key with a different ordinal's key -> WrongProofKey on "hot".
        negativeCase(cat, top, catalogRoot, 5L, "wrong key on hot proof -> WrongProofKey(hot)") { j =>
          modField(j, "hot", hot => modField(hot, "key", _ => Json.fromString(CommitCatalog.ordinalKey(6L).value)))
        },
        // Drop sealedEntry for a sealed ordinal -> MalformedOrdinalProof.
        negativeCase(cat, top, catalogRoot, 1L, "missing sealedEntry for a sealed ordinal -> MalformedOrdinalProof") { j =>
          modField(j, "sealedEntry", _ => Json.Null)
        }
      ).sequence

      doc = Json.fromJsonObject(
        JsonObject.fromIterable(
          List(
            "description" := (
              "Ordinal-catalog attestation cross-language test vectors. Scala (metakit) is the reference; " +
              "every implementation must reproduce `expected` EXACTLY: run OrdinalCatalogProofVerifier over " +
              "`proof` against the shared `catalogRoot` with the given `epochSize`, and match the attestation " +
              "({type:CommittedAt,ordinal,mptRoot} | {type:NotCommitted,ordinal}) or the error " +
              "({error, component?}) at component granularity. All roots/keys are raw lowercase hex (no 0x). " +
              "Generated by `sbt \"Test/runMain " +
              "io.constellationnetwork.metagraph_sdk.lifecycle.committed.OrdinalCatalogVectorGenerator\"` — " +
              "expected values are PRODUCED BY RUNNING the Scala verifier, never hand-computed."
            ),
            "version"     := "1.0.0",
            "epochSize"   := config.epochSize,
            "catalogRoot" := catalogRoot.value.value,
            "cases"       := Json.fromValues(positives ++ negatives)
          )
        )
      )
      rendered = Printer.spaces2.copy(colonLeft = " ").print(doc) + "\n"
      _ <- IO(Files.write(Paths.get(OutPath), rendered.getBytes(StandardCharsets.UTF_8)))
      _ <- IO.println(s"\nWrote $OutPath")
    } yield ExitCode.Success
}
