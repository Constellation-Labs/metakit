package crypto.mpt

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaProver, MerklePatriciaVerifier}
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import io.circe.{Json, Printer, parser}
import weaver.SimpleIOSuite

/**
 * Builds the chain-derived KAT fixtures for the sealed `{type, path, witness}` proof wire format
 * (`docs/mpt-spec/test-sealed-proofs.json`, consumed by the reference verifiers -- see the js
 * harness). Every proof is emitted by the REAL prover (`MerklePatriciaProver.provePath`) over a
 * small deterministic trie; nothing is hand-written. The five cases cover the tagged Inclusion
 * shape plus one absence proof per divergence family that the fixture freezes for external
 * implementations: branch-missing-nibble, other-leaf, extension-divergence, and the empty trie.
 * (The fourth terminal condition, path-exhausted-at-branch, shares the Branch terminal encoding
 * of branch-missing-nibble; it is exercised in [[MerklePatriciaAbsenceSuite]].)
 */
object MptSpecFixtures {

  val FixturePath = "docs/mpt-spec/test-sealed-proofs.json"

  final case class BuiltCase(name: String, root: Hash, proof: MerklePatriciaProof, json: Json)

  private def trieOf(entries: List[(String, String)]): IO[MerklePatriciaTrie] =
    if (entries.isEmpty) MerklePatriciaNode.Branch[IO](Map.empty).map(MerklePatriciaTrie(_))
    else MerklePatriciaTrie.make[IO, String](entries.map { case (k, v) => Hex(k) -> v }.toMap)

  private def buildCase(
    name: String,
    entries: List[(String, String)],
    query: String,
    record: Option[String]
  ): IO[BuiltCase] =
    for {
      trie  <- trieOf(entries)
      proof <- MerklePatriciaProver.make[IO](trie).provePath(Hex(query)).flatMap(IO.fromEither(_))
      _ <- IO.raiseUnless(record.isDefined == proof.isInstanceOf[MerklePatriciaProof.Inclusion])(
        new RuntimeException(s"$name: unexpected proof arm for query $query: $proof")
      )
    } yield {
      val fields =
        List(
          "name"     -> Json.fromString(name),
          "trie"     -> Json.obj(entries.map { case (k, v) => k -> Json.fromString(v) }: _*),
          "rootHash" -> trie.rootNode.digest.asJson
        ) ++
        record.map(r => "record" -> Json.fromString(r)).toList :+
        ("proof" -> proof.asJson)
      BuiltCase(name, trie.rootNode.digest, proof, Json.obj(fields: _*))
    }

  /** Fixed keys/values per case; the queried path; `Some(record)` iff inclusion is expected. */
  val cases: IO[List[BuiltCase]] = List(
    ("inclusion-tagged", List("a1" -> "va", "b2" -> "vb"), "a1", Option("va")),
    ("absence-branch-missing-nibble", List("a1" -> "va", "b2" -> "vb"), "c3", Option.empty[String]),
    ("absence-other-leaf", List("abcd" -> "solo"), "abce", Option.empty[String]),
    ("absence-extension-divergence", List("abcd" -> "v1", "abce" -> "v2"), "ab12", Option.empty[String]),
    ("absence-empty-trie", List.empty[(String, String)], "deadbeef", Option.empty[String])
  ).traverse((buildCase _).tupled)

  val rendered: IO[String] = cases.map { built =>
    val doc = Json.obj(
      "description" -> Json.fromString(
        "KAT fixtures for the sealed MerklePatriciaProof wire format {type, path, witness}: every proof is " +
        "produced by the real Scala prover (MerklePatriciaProver.provePath) over the listed trie entries " +
        "(key hex -> string value) and byte-pinned by MptSpecFixtureSuite. An Inclusion leaf's dataDigest " +
        "equals sha256(JCS(record)). Regenerate with `sbt \"Test/runMain crypto.mpt.MptSpecFixtureGenerator\"`; " +
        "never edit by hand. Legacy inclusion-only consumers keep test-proof.json."
      ),
      "cases" -> Json.arr(built.map(_.json): _*)
    )
    Printer.spaces2.print(doc) + "\n"
  }
}

/**
 * Fixture-as-golden: the committed `docs/mpt-spec/test-sealed-proofs.json` must byte-match what
 * the CURRENT prover and codecs produce, and each committed proof must verify. Any wire-format
 * drift (encoder field order, digest discipline, prover walk) fails here before it can silently
 * strand the external reference verifiers.
 */
object MptSpecFixtureSuite extends SimpleIOSuite {

  test("every fixture proof verifies against its own root via the real verifier") {
    for {
      built <- MptSpecFixtures.cases
      failed <- built
        .traverse(c => MerklePatriciaVerifier.make[IO](c.root).confirm(c.proof).map(c.name -> _))
        .map(_.collect { case (name, Left(err)) => s"$name: $err" })
    } yield expect.same(List.empty[String], failed)
  }

  test("the committed fixture file byte-matches the prover's current output") {
    for {
      generated <- MptSpecFixtures.rendered
      committed <- IO(new String(Files.readAllBytes(Paths.get(MptSpecFixtures.FixturePath)), StandardCharsets.UTF_8))
    } yield expect.same(generated, committed)
  }

  test("the committed fixture proofs decode to exactly what the prover emits") {
    for {
      built     <- MptSpecFixtures.cases
      committed <- IO(new String(Files.readAllBytes(Paths.get(MptSpecFixtures.FixturePath)), StandardCharsets.UTF_8))
      decoded <- IO.fromEither(
        parser
          .parse(committed)
          .flatMap(_.hcursor.downField("cases").as[List[Json]])
          .flatMap(_.traverse(_.hcursor.downField("proof").as[MerklePatriciaProof]))
      )
    } yield expect.same(built.map(_.proof), decoded)
  }
}
