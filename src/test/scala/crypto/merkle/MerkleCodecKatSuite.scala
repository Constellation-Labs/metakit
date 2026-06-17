package crypto.merkle

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleCommitment, MerkleInclusionProof, MerkleNode, MerkleTree}
import io.constellationnetwork.security.hash.Hash

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite

/**
 * Wire-format KATs (golden field-name / discriminator pins) for the consensus-HASHED merkle codecs.
 *
 * These serializations are hashed into Merkle roots, so a field rename / reorder / discriminator
 * change silently re-hashes => a binary-compat break that would diverge every committed root. Each
 * case pins the EXACT ordered key list (and ADT `type` discriminator) of a fixed instance and asserts
 * `decode(encode(x)) == x`. A drift fails HERE, before it can change a root. This suite is also the
 * byte-identity gate for any hand-rolled -> derived codec swap: the ordered-key + round-trip pins must
 * stay green across the swap.
 */
object MerkleCodecKatSuite extends SimpleIOSuite {

  private val h1: Hash = Hash("aa" * 32)
  private val h2: Hash = Hash("bb" * 32)

  private def keys(j: Json): List[String] = j.asObject.toList.flatMap(_.keys.toList)
  private def at(j: Json, field: String): Json = j.hcursor.downField(field).focus.getOrElse(Json.Null)

  // --- MerkleCommitment (case-class codecs are derived; ADT discriminator is custom {type,contents}) ---

  pureTest("MerkleCommitment.Leaf wire keys = [dataDigest]") {
    expect(keys(MerkleCommitment.Leaf(h1).asJson) == List("dataDigest"))
  }

  pureTest("MerkleCommitment.Internal wire keys = [leftDigest, rightDigest]") {
    expect(keys(MerkleCommitment.Internal(h1, h2).asJson) == List("leftDigest", "rightDigest"))
  }

  pureTest("MerkleCommitment ADT = {type, contents}, type in {Leaf, Internal}, round-trips") {
    val leaf: MerkleCommitment = MerkleCommitment.Leaf(h1)
    val internal: MerkleCommitment = MerkleCommitment.Internal(h1, h2)
    val jl = leaf.asJson
    val ji = internal.asJson
    expect(keys(jl) == List("type", "contents"))
      .and(expect(at(jl, "type") == Json.fromString("Leaf")))
      .and(expect(at(ji, "type") == Json.fromString("Internal")))
      .and(expect(decode[MerkleCommitment](jl.noSpaces) == Right(leaf)))
      .and(expect(decode[MerkleCommitment](ji.noSpaces) == Right(internal)))
  }

  // --- MerkleInclusionProof (custom witness format: array of {digest, side}; side is a byte) ---

  pureTest("MerkleInclusionProof keys = [leafDigest, witness]; witness elem = [digest, side]; round-trips") {
    val proof = MerkleInclusionProof.create(h1, Seq((h2, MerkleInclusionProof.LeftSide))).toEither.toOption.get
    val j = proof.asJson
    val w0 = j.hcursor.downField("witness").downN(0).focus.getOrElse(Json.Null)
    expect(keys(j) == List("leafDigest", "witness"))
      .and(expect(keys(w0) == List("digest", "side")))
      .and(expect(decode[MerkleInclusionProof](j.noSpaces) == Right(proof)))
  }

  // --- MerkleNode + MerkleTree (built via the digest-computing smart apply) ---

  test("MerkleNode.Leaf/Internal bare + ADT: key order + discriminator + round-trip") {
    for {
      leaf     <- MerkleNode.Leaf[IO](Json.fromString("payload"))
      internal <- MerkleNode.Internal[IO](leaf, None)
    } yield {
      val leafBare = (leaf: MerkleNode.Leaf).asJson
      val internalBare = (internal: MerkleNode.Internal).asJson
      val leafAdt = (leaf: MerkleNode).asJson
      val internalAdt = (internal: MerkleNode).asJson
      expect(keys(leafBare) == List("data", "digest"))
        .and(expect(keys(internalBare) == List("left", "right", "digest")))
        .and(expect(keys(leafAdt) == List("type", "contents")))
        .and(expect(at(leafAdt, "type") == Json.fromString("Leaf")))
        .and(expect(at(internalAdt, "type") == Json.fromString("Internal")))
        .and(expect(decode[MerkleNode](leafAdt.noSpaces) == Right(leaf)))
        .and(expect(decode[MerkleNode](internalAdt.noSpaces) == Right(internal)))
    }
  }

  test("MerkleTree keys = [rootNode, leafDigestIndex]; index elem = [digest, index]; round-trip") {
    for {
      tree <- MerkleTree.create[IO, String](List("a", "b", "c"))
    } yield {
      val j = tree.asJson
      val idx0 = j.hcursor.downField("leafDigestIndex").downN(0).focus.getOrElse(Json.Null)
      expect(keys(j) == List("rootNode", "leafDigestIndex"))
        .and(expect(keys(idx0) == List("digest", "index")))
        .and(expect(decode[MerkleTree](j.noSpaces) == Right(tree)))
    }
  }
}
