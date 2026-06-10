package io.constellationnetwork.metagraph_sdk.json_logic.ops

import cats.syntax.either._
import cats.syntax.traverse._

import io.constellationnetwork.metagraph_sdk.crypto.vrf.MiraclEcVrf25519
import io.constellationnetwork.metagraph_sdk.crypto.zk.Sp1Groth16Verifier
import io.constellationnetwork.metagraph_sdk.crypto.zk.merkle.{PoseidonMerkleProof, PoseidonMerkleTree}
import io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon
import io.constellationnetwork.metagraph_sdk.json_logic.core._

/**
 * Pure, deterministic implementations of the ZK / crypto opcodes, expressed as the JLVM's
 * `verify` / `hash` precompiles (the EVM-precompile model: the VM runs in the clear, these
 * opcodes are pure functions over already-verified crypto primitives).
 *
 * Every function returns `Either[JsonLogicException, JsonLogicValue]` and NEVER throws to the
 * caller: malformed inputs (bad hex, wrong width, non-canonical field element, wrong arg count or
 * type) all map to a `JsonLogicException`. The underlying primitives are consumed as-is; this layer
 * only handles encoding (via [[HexBytes]]) and argument shape.
 *
 * Encoding convention (see [[HexBytes]]): all byte / field arguments and returns are lowercase,
 * `0x`-prefixed, big-endian, fixed-width hex strings, modelled as a validated special-case of
 * `StrValue`.
 */
object CryptoOps {

  // ---------------------------------------------------------------------------
  // poseidon: variadic field elements -> Fr hash (32B hex).
  // ---------------------------------------------------------------------------

  /** Largest input width (t) for which circomlib constants are bundled (t = #inputs + 1). */
  private val PoseidonMaxInputs: Int = 16

  def poseidon(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] = {
    val hexArgs: Either[JsonLogicException, List[String]] = values match {
      case Nil => JsonLogicException("poseidon: requires at least one field element").asLeft
      // Accept either variadic hex args or a single array of hex args.
      case ArrayValue(arr) :: Nil if arr.nonEmpty => arr.traverse(expectStr("poseidon input"))
      case _                                      => values.traverse(expectStr("poseidon input"))
    }

    for {
      hexes <- hexArgs
      _ <- Either.cond(
        hexes.nonEmpty,
        (),
        JsonLogicException("poseidon: requires at least one field element")
      )
      _ <- Either.cond(
        hexes.length <= PoseidonMaxInputs,
        (),
        JsonLogicException(s"poseidon: supports at most $PoseidonMaxInputs inputs, got ${hexes.length}")
      )
      inputs <- hexes.zipWithIndex.traverse { case (h, i) => HexBytes.parseFr(h, s"poseidon input[$i]") }
      digest = Poseidon.hash(inputs)
      out <- HexBytes.encodeFr(digest)
    } yield StrValue(out)
  }

  // ---------------------------------------------------------------------------
  // pmt_verify: [root, leaf, index, [siblings...]] -> bool.
  // ---------------------------------------------------------------------------

  def pmtVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case rootV :: leafV :: indexV :: ArrayValue(siblingsV) :: Nil =>
        // Any malformed component (bad hex / non-canonical / negative or out-of-range index)
        // is a Result error; a well-formed-but-wrong proof simply verifies to `false`.
        for {
          rootHex <- expectStr("pmt_verify root")(rootV)
          leafHex <- expectStr("pmt_verify leaf")(leafV)
          root    <- HexBytes.parseFr(rootHex, "pmt_verify root")
          leaf    <- HexBytes.parseFr(leafHex, "pmt_verify leaf")
          index   <- expectIndex("pmt_verify index")(indexV)
          siblings <- siblingsV.zipWithIndex.traverse {
            case (s, i) =>
              expectStr(s"pmt_verify sibling[$i]")(s).flatMap(HexBytes.parseFr(_, s"pmt_verify sibling[$i]"))
          }
          depth = siblings.length
          _ <- Either.cond(
            index < (BigInt(1) << depth),
            (),
            JsonLogicException(s"pmt_verify: index $index out of range for depth $depth")
          )
          proof = PoseidonMerkleProof(index, siblings.toVector)
        } yield BoolValue(PoseidonMerkleTree.verifyInclusion(leaf, proof, root))
      case _ =>
        JsonLogicException(
          s"pmt_verify: expected [rootHex, leafHex, index, [siblingHex...]], got $values"
        ).asLeft
    }

  // ---------------------------------------------------------------------------
  // groth16_verify: [vkey(32B), publicValues(arbitrary), proof] -> bool.
  // ---------------------------------------------------------------------------

  def groth16Verify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case vkeyV :: pubV :: proofV :: Nil =>
        for {
          vkeyHex  <- expectStr("groth16_verify vkey")(vkeyV)
          pubHex   <- expectStr("groth16_verify publicValues")(pubV)
          proofHex <- expectStr("groth16_verify proof")(proofV)
          vkey     <- HexBytes.parseBytes(vkeyHex, Some(32), "groth16_verify vkey")
          pub      <- HexBytes.parseBytes(pubHex, None, "groth16_verify publicValues")
          proof    <- HexBytes.parseBytes(proofHex, None, "groth16_verify proof")
        } yield
          // Right(()) -> true, Left(_) -> false (a malformed-but-well-typed proof is simply invalid).
          BoolValue(Sp1Groth16Verifier.verify(vkey, pub, proof).isRight)
      case _ =>
        JsonLogicException(
          s"groth16_verify: expected [vkeyHex, publicValuesHex, proofHex], got $values"
        ).asLeft
    }

  // ---------------------------------------------------------------------------
  // ecvrf_verify: [pk, alpha, proof] -> {"valid": bool, "beta": hexOrNull}.
  // ---------------------------------------------------------------------------

  private val vrf: MiraclEcVrf25519 = MiraclEcVrf25519.default

  def ecVrfVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case pkV :: alphaV :: proofV :: Nil =>
        for {
          pkHex    <- expectStr("ecvrf_verify pk")(pkV)
          alphaHex <- expectStr("ecvrf_verify alpha")(alphaV)
          proofHex <- expectStr("ecvrf_verify proof")(proofV)
          // pk is a 32-byte point; proof is 80 bytes; alpha is arbitrary-length message bytes.
          pk    <- HexBytes.parseBytes(pkHex, Some(MiraclEcVrf25519.PointBytes), "ecvrf_verify pk")
          alpha <- HexBytes.parseBytes(alphaHex, None, "ecvrf_verify alpha")
          proof <- HexBytes.parseBytes(proofHex, Some(MiraclEcVrf25519.ProofBytes), "ecvrf_verify proof")
          valid = vrf.vrfVerify(pk, alpha, proof)
          beta <-
            if (valid) {
              vrf.vrfProofToHash(proof) match {
                case Some(b) => StrValue(HexBytes.encodeBytes(b)): JsonLogicValue
                case None    => NullValue: JsonLogicValue // valid proof should always yield beta; defensive
              }
            }.asRight[JsonLogicException]
            else (NullValue: JsonLogicValue).asRight[JsonLogicException]
        } yield MapValue(Map("valid" -> BoolValue(valid), "beta" -> beta))
      case _ =>
        JsonLogicException(
          s"ecvrf_verify: expected [pkHex, alphaHex, proofHex], got $values"
        ).asLeft
    }

  // ---------------------------------------------------------------------------
  // Shared argument helpers.
  // ---------------------------------------------------------------------------

  private def expectStr(role: String)(v: JsonLogicValue): Either[JsonLogicException, String] =
    v match {
      case StrValue(s) => s.asRight
      case other       => JsonLogicException(s"$role: expected a hex string, got ${other.tag}").asLeft
    }

  private def expectIndex(role: String)(v: JsonLogicValue): Either[JsonLogicException, BigInt] =
    v match {
      case IntValue(i) if i >= 0 => i.asRight
      case IntValue(i)           => JsonLogicException(s"$role: must be non-negative, got $i").asLeft
      case other                 => JsonLogicException(s"$role: expected a non-negative integer, got ${other.tag}").asLeft
    }
}
