package crypto.smt

import cats.syntax.eq._

import io.constellationnetwork.metagraph_sdk.crypto.smt._
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax.EncoderOps
import weaver.SimpleIOSuite

/**
 * Wire-format KATs (golden field-name / discriminator pins) for the consensus-HASHED SMT codecs.
 *
 * Same contract as MerkleCodecKatSuite: each case pins the EXACT ordered key list + ADT `type`
 * discriminator of a fixed instance and asserts decode(encode(x)) == x, so a field rename / reorder /
 * discriminator change fails HERE before it can re-hash an SMT root. The Hash-only types
 * (SparseMerkleSibling, SparseMerkleCommitment.Leaf/Internal, SparseMerkleRoot) are DERIVED
 * (byte-identical); SparseMerkleProof + AbsenceWitness stay hand-rolled (custom Array[Byte]->Hex value
 * codec + case-object ADT). Round-trip for the Array[Byte]-bearing proof uses the type's `Eq` (Array
 * has no structural ==). SparseMerkleEntry has no codec (verified-outcome type), so it is not covered.
 */
object SparseMerkleCodecKatSuite extends SimpleIOSuite {

  private val h1: Hash = Hash("aa" * 32)
  private val h2: Hash = Hash("bb" * 32)
  private val hx: Hex = Hex.fromBytes(Array[Byte](1, 2))

  private def keys(j: Json): List[String] = j.asObject.toList.flatMap(_.keys.toList)
  private def at(j: Json, field: String): Json = j.hcursor.downField(field).focus.getOrElse(Json.Null)

  pureTest("SparseMerkleSibling wire keys = [digest]; round-trips") {
    val s = SparseMerkleSibling(h1)
    expect(keys(s.asJson) == List("digest"))
      .and(expect(decode[SparseMerkleSibling](s.asJson.noSpaces) == Right(s)))
  }

  pureTest("SparseMerkleRoot wire keys = [value]; round-trips") {
    val r = SparseMerkleRoot(h1)
    expect(keys(r.asJson) == List("value"))
      .and(expect(decode[SparseMerkleRoot](r.asJson.noSpaces) == Right(r)))
  }

  pureTest("SparseMerkleCommitment Leaf/Internal keys + ADT {type,contents} + round-trip") {
    val leaf: SparseMerkleCommitment = SparseMerkleCommitment.Leaf(h1, h2)
    val internal: SparseMerkleCommitment = SparseMerkleCommitment.Internal(h1, h2)
    expect(keys(SparseMerkleCommitment.Leaf(h1, h2).asJson) == List("position", "valueDigest"))
      .and(expect(keys(SparseMerkleCommitment.Internal(h1, h2).asJson) == List("left", "right")))
      .and(expect(keys(leaf.asJson) == List("type", "contents")))
      .and(expect(at(leaf.asJson, "type") == Json.fromString("Leaf")))
      .and(expect(at(internal.asJson, "type") == Json.fromString("Internal")))
      .and(expect(decode[SparseMerkleCommitment](leaf.asJson.noSpaces) == Right(leaf)))
      .and(expect(decode[SparseMerkleCommitment](internal.asJson.noSpaces) == Right(internal)))
  }

  pureTest("AbsenceWitness Default/OtherLeaf: discriminator + keys + round-trip") {
    val default: AbsenceWitness = AbsenceWitness.Default
    val other: AbsenceWitness = AbsenceWitness.OtherLeaf(hx, h1)
    expect(keys(default.asJson) == List("type"))
      .and(expect(at(default.asJson, "type") == Json.fromString("Default")))
      .and(expect(keys(other.asJson) == List("type", "occupyingKey", "occupyingDataDigest")))
      .and(expect(at(other.asJson, "type") == Json.fromString("OtherLeaf")))
      .and(expect(decode[AbsenceWitness](default.asJson.noSpaces) == Right(default)))
      .and(expect(decode[AbsenceWitness](other.asJson.noSpaces) == Right(other)))
  }

  pureTest("SparseMerkleProof Inclusion/Absence: keys + discriminator + round-trip (Eq)") {
    val incl: SparseMerkleProof =
      SparseMerkleProof.Inclusion(hx, Hex.fromBytes(Array[Byte](1, 2, 3)), h2, List(SparseMerkleSibling(h1)))
    val abs: SparseMerkleProof =
      SparseMerkleProof.Absence(hx, AbsenceWitness.Default, List(SparseMerkleSibling(h1)))
    expect(keys(incl.asJson) == List("type", "key", "value", "valueDigest", "siblings"))
      .and(expect(at(incl.asJson, "type") == Json.fromString("Inclusion")))
      .and(expect(keys(abs.asJson) == List("type", "key", "witness", "siblings")))
      .and(expect(at(abs.asJson, "type") == Json.fromString("Absence")))
      .and(expect(decode[SparseMerkleProof](incl.asJson.noSpaces).exists(_ === incl)))
      .and(expect(decode[SparseMerkleProof](abs.asJson.noSpaces).exists(_ === abs)))
  }
}
