package crypto.mpt

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite

/**
 * Wire-format KATs (golden field-name / discriminator pins) for the consensus-HASHED MPT codecs.
 *
 * Same contract as MerkleCodecKatSuite: each case pins the EXACT ordered key list + ADT `type`
 * discriminator of a fixed instance and asserts decode(encode(x)) == x, so a field rename / reorder /
 * discriminator change fails HERE before it can re-hash an MPT root. All MPT codecs stay HAND-ROLLED
 * (not derived): the commitment/node Leaf/Extension fields are `Seq[Nibble]`, whose custom
 * `Nibble.nibbleSeqEncoder` is ambiguous with circe's generic `encodeSeq` under magnolia derivation
 * (the hand-rolled encoders pass it explicitly). This suite guards those hand-rolled field-names /
 * discriminators against drift.
 */
object MerklePatriciaCodecKatSuite extends SimpleIOSuite {

  private val h1: Hash = Hash("aa" * 32)
  private val h2: Hash = Hash("bb" * 32)
  private val nA: Nibble = Nibble.unsafe('a')
  private val nB: Nibble = Nibble.unsafe('b')
  private val n1: Nibble = Nibble.unsafe('1')

  private def keys(j: Json): List[String] = j.asObject.toList.flatMap(_.keys.toList)
  private def at(j: Json, field: String): Json = j.hcursor.downField(field).focus.getOrElse(Json.Null)

  // --- MerklePatriciaCommitment (hand-rolled codecs; Seq[Nibble] blocks clean derivation) ---

  pureTest("MPT commitment wire keys: Leaf/Branch/Extension") {
    expect(keys(MerklePatriciaCommitment.Leaf(Seq(nA), h1).asJson) == List("remaining", "dataDigest"))
      .and(expect(keys(MerklePatriciaCommitment.Branch(Map(nA -> h1, nB -> h2)).asJson) == List("pathsDigest")))
      .and(expect(keys(MerklePatriciaCommitment.Extension(Seq(n1), h1).asJson) == List("shared", "childDigest")))
  }

  pureTest("MPT commitment ADT = {type, contents}, type discriminates, round-trips") {
    val cases: List[(MerklePatriciaCommitment, String)] = List(
      MerklePatriciaCommitment.Leaf(Seq(nA), h1)      -> "Leaf",
      MerklePatriciaCommitment.Branch(Map(nA -> h1))  -> "Branch",
      MerklePatriciaCommitment.Extension(Seq(n1), h1) -> "Extension"
    )
    cases.foldLeft(success) {
      case (acc, (c, tag)) =>
        val j = c.asJson
        acc
          .and(expect(keys(j) == List("type", "contents")))
          .and(expect(at(j, "type") == Json.fromString(tag)))
          .and(expect(decode[MerklePatriciaCommitment](j.noSpaces) == Right(c)))
    }
  }

  // --- MerklePatriciaInclusionProof (path + witness; ADT-list witness; convertible) ---

  pureTest("MPT inclusion proof wire keys = [path, witness]; round-trips") {
    val proof = MerklePatriciaInclusionProof(
      Hex("abcd"),
      List(MerklePatriciaCommitment.Leaf(Seq(nA), h1), MerklePatriciaCommitment.Branch(Map(nA -> h1, nB -> h2)))
    )
    val j = proof.asJson
    expect(keys(j) == List("path", "witness"))
      .and(expect(decode[MerklePatriciaInclusionProof](j.noSpaces) == Right(proof)))
  }

  // --- MerklePatriciaProof (sealed inclusion-or-absence ADT; the light-client wire contract) ---

  pureTest("MPT sealed proof wire = {type, path, witness}; both tags round-trip") {
    val witness: List[MerklePatriciaCommitment] =
      List(MerklePatriciaCommitment.Leaf(Seq(nA), h1), MerklePatriciaCommitment.Branch(Map(nA -> h1, nB -> h2)))
    val cases: List[(MerklePatriciaProof, String)] = List(
      MerklePatriciaProof.Inclusion(MerklePatriciaInclusionProof(Hex("abcd"), witness)) -> "Inclusion",
      MerklePatriciaProof.Absence(Hex("abcd"), witness)                                 -> "Absence"
    )
    cases.foldLeft(success) {
      case (acc, (p, tag)) =>
        val j = p.asJson
        acc
          .and(expect(keys(j) == List("type", "path", "witness")))
          .and(expect(at(j, "type") == Json.fromString(tag)))
          .and(expect(decode[MerklePatriciaProof](j.noSpaces) == Right(p)))
    }
  }

  pureTest("MPT sealed Inclusion encoding = legacy {path, witness} shape + type tag") {
    val legacy = MerklePatriciaInclusionProof(
      Hex("abcd"),
      List(MerklePatriciaCommitment.Extension(Seq(n1), h2), MerklePatriciaCommitment.Leaf(Seq(nA), h1))
    )
    val tagged = (MerklePatriciaProof.Inclusion(legacy): MerklePatriciaProof).asJson
    expect(at(tagged, "path") == at(legacy.asJson, "path"))
      .and(expect(at(tagged, "witness") == at(legacy.asJson, "witness")))
  }

  // --- MerklePatriciaNode + Trie (nodes built via the digest-computing smart apply) ---

  test("MPT node Leaf/Branch/Extension bare + ADT: key order + discriminator + round-trip") {
    for {
      leaf   <- MerklePatriciaNode.Leaf[IO](Seq(nA), Json.fromString("payload"))
      branch <- MerklePatriciaNode.Branch[IO](Map(nA -> leaf))
      ext    <- MerklePatriciaNode.Extension[IO](Seq(n1), branch)
    } yield {
      val leafBare = (leaf: MerklePatriciaNode.Leaf).asJson
      val branchBare = (branch: MerklePatriciaNode.Branch).asJson
      val extBare = (ext: MerklePatriciaNode.Extension).asJson
      val leafAdt = (leaf: MerklePatriciaNode).asJson
      val branchAdt = (branch: MerklePatriciaNode).asJson
      val extAdt = (ext: MerklePatriciaNode).asJson
      expect(keys(leafBare) == List("remaining", "data", "digest"))
        .and(expect(keys(branchBare) == List("paths", "digest")))
        .and(expect(keys(extBare) == List("shared", "child", "digest")))
        .and(expect(at(leafAdt, "type") == Json.fromString("Leaf")))
        .and(expect(at(branchAdt, "type") == Json.fromString("Branch")))
        .and(expect(at(extAdt, "type") == Json.fromString("Extension")))
        .and(expect(keys(leafAdt) == List("type", "contents")))
        .and(expect(decode[MerklePatriciaNode](leafAdt.noSpaces) == Right(leaf)))
        .and(expect(decode[MerklePatriciaNode](branchAdt.noSpaces) == Right(branch)))
        .and(expect(decode[MerklePatriciaNode](extAdt.noSpaces) == Right(ext)))
    }
  }

  test("MPT trie wire keys = [rootNode]; round-trip") {
    for {
      leaf <- MerklePatriciaNode.Leaf[IO](Seq(nA), Json.fromString("payload"))
    } yield {
      val trie = MerklePatriciaTrie(leaf)
      val j = trie.asJson
      expect(keys(j) == List("rootNode"))
        .and(expect(decode[MerklePatriciaTrie](j.noSpaces) == Right(trie)))
    }
  }
}
