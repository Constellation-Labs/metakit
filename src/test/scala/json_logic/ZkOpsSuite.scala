package json_logic

import java.util.HexFormat

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.zk.merkle.{Fr, PoseidonMerkleTree}
import io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.{CryptoOps, HexBytes}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

/**
 * End-to-end tests for the JLVM ZK / crypto opcodes (`poseidon`, `pmt_verify`, `groth16_verify`,
 * `ecvrf_verify`) and the shared [[HexBytes]] codec. Each opcode is exercised both directly (through
 * [[CryptoOps]]) and end-to-end through the evaluator, with a positive and a negative case, plus a
 * worked policy-over-verified-facts contract combining `groth16_verify` and `pmt_verify`.
 */
object ZkOpsSuite extends SimpleIOSuite {

  private val hex = HexFormat.of()

  // 0x-prefixed lowercase, 32-byte zero-padded hex of a BigInt (an Fr field element).
  private def fr(v: BigInt): String = HexBytes.encodeFr(v).fold(throw _, identity)

  private def evalExpr(exprJson: String, dataJson: String = "{}"): IO[Either[JsonLogicException, JsonLogicValue]] =
    for {
      expr <- IO.fromEither(parser.parse(exprJson).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse(dataJson).flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield out

  // ===========================================================================
  // HexBytes codec
  // ===========================================================================

  pureTest("HexBytes.parseBytes accepts a well-formed even-length lowercase hex string") {
    expect(HexBytes.parseBytes("0xdeadbeef", Some(4), "x").map(_.toList) == Right(List(0xde, 0xad, 0xbe, 0xef).map(_.toByte)))
  }

  pureTest("HexBytes rejects malformed hex (uppercase / missing prefix / odd length)") {
    expect(HexBytes.parseBytes("0xDEAD", Some(2), "x").isLeft) && // uppercase
    expect(HexBytes.parseBytes("deadbeef", Some(4), "x").isLeft) && // no 0x prefix
    expect(HexBytes.parseBytes("0xabc", None, "x").isLeft) && // odd nibble count
    expect(HexBytes.parseBytes("0xzz", Some(1), "x").isLeft) // non-hex chars
  }

  pureTest("HexBytes rejects wrong width") {
    expect(HexBytes.parseBytes("0xdead", Some(4), "x").isLeft) &&
    expect(HexBytes.parseBytes("0xdeadbeef", Some(2), "x").isLeft)
  }

  pureTest("HexBytes.parseFr accepts a canonical 32-byte field element and rejects non-canonical") {
    val canonical = fr(BigInt(123456789))
    val nonCanonical = "0x" + "f" * 64 // 2^256 - 1, > modulus
    expect(HexBytes.parseFr(canonical, "x") == Right(BigInt(123456789))) &&
    expect(HexBytes.parseFr(nonCanonical, "x").isLeft) &&
    expect(HexBytes.parseFr("0x01", "x").isLeft) // wrong width (1 byte, not 32)
  }

  pureTest("HexBytes round-trips encode/parse for Fr") {
    val v = Poseidon.hash(Seq(BigInt(7), BigInt(8)))
    expect(HexBytes.parseFr(fr(v), "x") == Right(v))
  }

  // ===========================================================================
  // poseidon
  // ===========================================================================

  // Known circomlib vector: poseidon([1, 2]).
  private val poseidon_1_2: BigInt =
    BigInt("115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a", 16)

  test("poseidon([0x01, 0x02]) matches the known circomlib vector") {
    evalExpr(
      """{"poseidon":["0x0000000000000000000000000000000000000000000000000000000000000001","0x0000000000000000000000000000000000000000000000000000000000000002"]}"""
    )
      .map(r => expect(r == Right(StrValue(fr(poseidon_1_2)))))
  }

  pureTest("poseidon direct op: single-arg and multi-arg both valid; matches primitive") {
    val single = CryptoOps.poseidon(List(StrValue(fr(BigInt(1)))))
    val multi = CryptoOps.poseidon(List(StrValue(fr(BigInt(1))), StrValue(fr(BigInt(2)))))
    expect(single == Right(StrValue(fr(Poseidon.hash(Seq(BigInt(1))))))) &&
    expect(multi == Right(StrValue(fr(poseidon_1_2))))
  }

  test("poseidon rejects a non-canonical / malformed input (Result error, no crash)") {
    val nonCanonical = "0x" + "f" * 64
    evalExpr(s"""{"poseidon":["$nonCanonical"]}""").map { r =>
      // A malformed input surfaces as a JsonLogicException, not a thrown exception.
      expect(r.isLeft)
    }
  }

  // ===========================================================================
  // pmt_verify
  // ===========================================================================

  private val MerkleDepth = 8
  private def commitment(seed: Long): BigInt = Poseidon.hash(Seq(BigInt(seed).mod(Fr.R), BigInt(seed + 1).mod(Fr.R)))

  private val merkleTree: PoseidonMerkleTree = {
    val entries = List(BigInt(3) -> commitment(10), BigInt(42) -> commitment(20), BigInt(170) -> commitment(30))
    PoseidonMerkleTree.fromLeaves(MerkleDepth, entries)
  }
  private val merklePos = BigInt(42)
  private val merkleLeaf = commitment(20)
  private val merkleProof = merkleTree.inclusionProof(merklePos)

  private def merkleExpr(root: BigInt, leaf: BigInt, index: BigInt, siblings: Vector[BigInt]): String = {
    val sibs = siblings.map(s => "\"" + fr(s) + "\"").mkString("[", ",", "]")
    s"""{"pmt_verify":["${fr(root)}","${fr(leaf)}",$index,$sibs]}"""
  }

  test("pmt_verify returns true for a real inclusion proof") {
    evalExpr(merkleExpr(merkleTree.root, merkleLeaf, merklePos, merkleProof.siblings))
      .map(r => expect(r == Right(BoolValue(true))))
  }

  test("pmt_verify returns false when a sibling is tampered") {
    val tampered = merkleProof.siblings.updated(0, commitment(999))
    evalExpr(merkleExpr(merkleTree.root, merkleLeaf, merklePos, tampered))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("pmt_verify returns false when the leaf is tampered") {
    evalExpr(merkleExpr(merkleTree.root, commitment(123), merklePos, merkleProof.siblings))
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("pmt_verify errors on a malformed argument shape") {
    // Missing the siblings array entirely.
    evalExpr(s"""{"pmt_verify":["${fr(merkleTree.root)}","${fr(merkleLeaf)}",42]}""")
      .map(r => expect(r.isLeft))
  }

  // ===========================================================================
  // groth16_verify (real SP1 fixture)
  // ===========================================================================

  final private case class Groth16Fixture(vkey: String, publicValues: String, proofBytes: String)

  private val groth16: Groth16Fixture = {
    val raw = {
      val src = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/zk/sp1-groth16-premium.json"), "UTF-8")
      try src.mkString
      finally src.close()
    }
    val cur = parser.parse(raw).fold(throw _, identity).hcursor
    def field(n: String): String = cur.get[String](n).fold(throw _, identity)
    Groth16Fixture(field("vkey"), field("publicValues"), field("proofBytes"))
  }

  // Flip the lowest bit of the last byte of a 0x hex string.
  private def flipLastByte(hex0: String): String = {
    val bytes = HexBytes.parseBytes(hex0, None, "x").fold(throw _, identity)
    bytes(bytes.length - 1) = (bytes(bytes.length - 1) ^ 0x01).toByte
    HexBytes.encodeBytes(bytes)
  }

  test("groth16_verify returns true for the real SP1 proof fixture") {
    evalExpr(s"""{"groth16_verify":["${groth16.vkey}","${groth16.publicValues}","${groth16.proofBytes}"]}""")
      .map(r => expect(r == Right(BoolValue(true))))
  }

  test("groth16_verify returns false when a proof byte is flipped") {
    evalExpr(s"""{"groth16_verify":["${groth16.vkey}","${groth16.publicValues}","${flipLastByte(groth16.proofBytes)}"]}""")
      .map(r => expect(r == Right(BoolValue(false))))
  }

  test("groth16_verify errors on a wrong-width vkey (not 32 bytes)") {
    evalExpr(s"""{"groth16_verify":["0xdead","${groth16.publicValues}","${groth16.proofBytes}"]}""")
      .map(r => expect(r.isLeft))
  }

  // ===========================================================================
  // ecvrf_verify (RFC 9381 vector)
  // ===========================================================================

  final private case class VrfVector(verificationKey: String, message: String, pi: String, beta: String)

  private val vrfVector: VrfVector = {
    val raw = {
      val src = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/vrf/VrfEd25519.json"), "UTF-8")
      try src.mkString
      finally src.close()
    }
    // Use the second vector (non-empty one-byte message "72") for a non-trivial alpha.
    val arr = parser.parse(raw).fold(throw _, identity).asArray.get
    val v = arr(1).hcursor
    VrfVector(
      verificationKey = v.downField("outputs").get[String]("verificationKey").fold(throw _, identity),
      message = v.downField("inputs").get[String]("message").fold(throw _, identity),
      pi = v.downField("outputs").get[String]("pi").fold(throw _, identity),
      beta = v.downField("outputs").get[String]("beta").fold(throw _, identity)
    )
  }

  private def vrfExpr(pk: String, alpha: String, proof: String): String =
    s"""{"ecvrf_verify":["0x$pk","0x$alpha","0x$proof"]}"""

  test("ecvrf_verify returns {valid:true, beta:<hash>} for the RFC 9381 vector") {
    evalExpr(vrfExpr(vrfVector.verificationKey, vrfVector.message, vrfVector.pi)).map { r =>
      expect(
        r == Right(
          MapValue(Map("valid" -> BoolValue(true), "beta" -> StrValue("0x" + vrfVector.beta)))
        )
      )
    }
  }

  test("ecvrf_verify returns {valid:false, beta:null} for a tampered proof") {
    val tampered = {
      val bytes = hex.parseHex(vrfVector.pi)
      bytes(0) = (bytes(0) ^ 0xff).toByte
      hex.formatHex(bytes)
    }
    evalExpr(vrfExpr(vrfVector.verificationKey, vrfVector.message, tampered)).map { r =>
      expect(r == Right(MapValue(Map("valid" -> BoolValue(false), "beta" -> NullValue))))
    }
  }

  test("ecvrf_verify errors on a wrong-width public key") {
    evalExpr(vrfExpr("dead", vrfVector.message, vrfVector.pi)).map(r => expect(r.isLeft))
  }

  // ===========================================================================
  // Worked example: policy over verified facts
  // ===========================================================================

  // Access is granted only when BOTH the SP1 proof verifies AND the Merkle inclusion holds.
  private def policyExpr(groth16Proof: String, merkleSiblings: Vector[BigInt]): String = {
    val sibs = merkleSiblings.map(s => "\"" + fr(s) + "\"").mkString("[", ",", "]")
    s"""
       |{"if":[
       |  {"and":[
       |    {"groth16_verify":["${groth16.vkey}","${groth16.publicValues}","$groth16Proof"]},
       |    {"pmt_verify":["${fr(merkleTree.root)}","${fr(merkleLeaf)}",$merklePos,$sibs]}
       |  ]},
       |  "granted",
       |  "denied"
       |]}
       |""".stripMargin
  }

  test("worked example: grants when both the Groth16 and Merkle proofs are valid") {
    evalExpr(policyExpr(groth16.proofBytes, merkleProof.siblings))
      .map(r => expect(r == Right(StrValue("granted"))))
  }

  test("worked example: denies when the Groth16 proof is bad") {
    evalExpr(policyExpr(flipLastByte(groth16.proofBytes), merkleProof.siblings))
      .map(r => expect(r == Right(StrValue("denied"))))
  }

  test("worked example: denies when the Merkle path is bad") {
    val badSiblings = merkleProof.siblings.updated(0, commitment(7777))
    evalExpr(policyExpr(groth16.proofBytes, badSiblings))
      .map(r => expect(r == Right(StrValue("denied"))))
  }
}
