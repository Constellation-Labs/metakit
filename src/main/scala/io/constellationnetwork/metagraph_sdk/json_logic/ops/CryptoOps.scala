package io.constellationnetwork.metagraph_sdk.json_logic.ops

import java.math.BigInteger
import java.security.MessageDigest

import cats.syntax.either._
import cats.syntax.traverse._

import io.constellationnetwork.metagraph_sdk.crypto.bls.Bls12381
import io.constellationnetwork.metagraph_sdk.crypto.vrf.MiraclEcVrf25519
import io.constellationnetwork.metagraph_sdk.crypto.zk.merkle.{PoseidonMerkleProof, PoseidonMerkleTree}
import io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon
import io.constellationnetwork.metagraph_sdk.crypto.zk.{Bn254, Groth16Verifier, Sp1Groth16Verifier}
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

  /**
   * Largest input count for which circomlib constants are bundled (t = #inputs + 1).
   * Derived from [[Poseidon]]'s constants so the opcode cap can never exceed what
   * the primitive supports (a larger cap would let inputs through to an internal
   * `require`, escaping the evaluator as a raw exception).
   */
  private val PoseidonMaxInputs: Int = Poseidon.MaxInputs

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
      // Defense-in-depth: the inputs are fully pre-validated above, but a future
      // validation gap must degrade to a JLVM error, never escape evaluate().
      digest <- Either
        .catchNonFatal(Poseidon.hash(inputs))
        .leftMap(e => JsonLogicException(s"poseidon: ${e.getMessage}"))
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
          // Error-vs-false discipline (lockstep with Rust op_groth16_verify):
          //   Right(())            -> true
          //   Left("ENCODING: ..") -> hard opcode error (malformed, non-canonical proof bytes)
          //   any other Left(_)    -> false (well-formed but cryptographically invalid)
          result <- Sp1Groth16Verifier.verify(vkey, pub, proof) match {
            case Right(()) => BoolValue(true).asRight
            case Left(e) if e.startsWith(Groth16Verifier.EncodingErrorPrefix) =>
              JsonLogicException(s"groth16_verify: $e").asLeft
            case Left(_) => BoolValue(false).asRight
          }
        } yield result
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

  // ===========================================================================
  // SECOND WAVE: BN254 (alt_bn128) curve ops, BLS12-381 signatures, Schnorr.
  // ===========================================================================

  private def bigInteger(v: BigInt): BigInteger = v.bigInteger

  // Build an on-curve Bn254.G1 from a parsed (x, y); reject off-curve points.
  // The all-zero point (0,0) is the EVM point-at-infinity and is on-curve.
  private def g1OnCurve(coords: (BigInt, BigInt), role: String): Either[JsonLogicException, Bn254.G1] = {
    val (x, y) = coords
    val p = Bn254.G1(bigInteger(x), bigInteger(y))
    Either.cond(p.isOnCurve, p, JsonLogicException(s"$role: point is not on the BN254 curve"))
  }

  // Build a valid Bn254.G2 from a parsed (xReal, xImag, yReal, yImag). Two-step
  // validation (mirrored byte-for-byte by the Rust `g2_on_curve`):
  //   1. curve membership (`isOnCurve`);
  //   2. order-r subgroup membership (`isInGroup`). BN254 G2 has a non-trivial
  //      cofactor, so an on-curve point may lie OUTSIDE the order-r subgroup.
  //      Such a point is not a valid pairing input (it breaks the soundness
  //      assumptions of the Groth16 check), so we reject it as malformed --
  //      identical handling to the off-curve case (a JsonLogicException). G1 is
  //      prime-order (cofactor 1), so on-curve already implies subgroup
  //      membership and `g1OnCurve` needs no analogous check.
  private def g2OnCurve(coords: (BigInt, BigInt, BigInt, BigInt), role: String): Either[JsonLogicException, Bn254.G2] = {
    val (xr, xi, yr, yi) = coords
    val p = Bn254.G2(bigInteger(xr), bigInteger(xi), bigInteger(yr), bigInteger(yi))
    for {
      _ <- Either.cond(p.isOnCurve, (), JsonLogicException(s"$role: point is not on the BN254 G2 curve"))
      _ <- Either.cond(p.isInGroup, (), JsonLogicException(s"$role: point is not in the BN254 G2 order-r subgroup"))
    } yield p
  }

  private def encodeG1(p: Bn254.G1): Either[JsonLogicException, String] =
    HexBytes.encodeG1(BigInt(p.x), BigInt(p.y))

  // ---------------------------------------------------------------------------
  // bn254_add: [aHex(64B), bHex(64B)] -> 64B G1 (EIP-196 ecAdd).
  // ---------------------------------------------------------------------------

  def bn254Add(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case aV :: bV :: Nil =>
        for {
          aHex <- expectStr("bn254_add a")(aV)
          bHex <- expectStr("bn254_add b")(bV)
          aC   <- HexBytes.parseG1(aHex, "bn254_add a")
          bC   <- HexBytes.parseG1(bHex, "bn254_add b")
          a    <- g1OnCurve(aC, "bn254_add a")
          b    <- g1OnCurve(bC, "bn254_add b")
          out  <- encodeG1(a.add(b))
        } yield StrValue(out)
      case _ =>
        JsonLogicException(s"bn254_add: expected [aHex(64B), bHex(64B)], got $values").asLeft
    }

  // ---------------------------------------------------------------------------
  // bn254_mul: [pHex(64B), sHex(32B)] -> 64B G1 (EIP-196 ecMul).
  // ---------------------------------------------------------------------------

  def bn254Mul(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case pV :: sV :: Nil =>
        for {
          pHex <- expectStr("bn254_mul point")(pV)
          sHex <- expectStr("bn254_mul scalar")(sV)
          pC   <- HexBytes.parseG1(pHex, "bn254_mul point")
          // Scalar is any 256-bit value; Bn254.G1.multiply reduces it mod R.
          s   <- HexBytes.parseScalar(sHex, "bn254_mul scalar")
          p   <- g1OnCurve(pC, "bn254_mul point")
          out <- encodeG1(p.multiply(bigInteger(s)))
        } yield StrValue(out)
      case _ =>
        JsonLogicException(s"bn254_mul: expected [pointHex(64B), scalarHex(32B)], got $values").asLeft
    }

  // ---------------------------------------------------------------------------
  // bn254_pairing: [[g1Hex(64B), g2Hex(128B)], ...] -> bool (EIP-197).
  //   true iff product of e(g1_i, g2_i) == 1; empty input -> true.
  // ---------------------------------------------------------------------------

  def bn254Pairing(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] = {
    // Accept the natural EIP-197 shape (a single array of [g1, g2] pairs) as well
    // as variadic pairs. Disambiguate the single-pair case `[[g1, g2]]` (which
    // parses to one ArrayValue wrapping one pair) from `[g1, g2]` by only
    // unwrapping the outer array when every element is itself an array (a pair).
    val rawPairs: List[JsonLogicValue] = values match {
      case ArrayValue(arr) :: Nil if arr.forall(_.isInstanceOf[ArrayValue]) => arr
      case other                                                            => other
    }

    for {
      pairs <- rawPairs.zipWithIndex.traverse {
        case (ArrayValue(g1Hex :: g2Hex :: Nil), i) =>
          for {
            g1H <- expectStr(s"bn254_pairing[$i].g1")(g1Hex)
            g2H <- expectStr(s"bn254_pairing[$i].g2")(g2Hex)
            g1C <- HexBytes.parseG1(g1H, s"bn254_pairing[$i].g1")
            g2C <- HexBytes.parseG2(g2H, s"bn254_pairing[$i].g2")
            g1  <- g1OnCurve(g1C, s"bn254_pairing[$i].g1")
            g2  <- g2OnCurve(g2C, s"bn254_pairing[$i].g2")
          } yield (g1, g2)
        case (other, i) =>
          JsonLogicException(s"bn254_pairing[$i]: expected [g1Hex(64B), g2Hex(128B)], got $other").asLeft
      }
    } yield BoolValue(Bn254.pairingProductIsOne(pairs))
  }

  // ---------------------------------------------------------------------------
  // bls_verify: [pkHex(48B G1), msgHex, sigHex(96B G2)] -> bool.
  //   Eth2 / IETF ProofOfPossession ciphersuite (BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_).
  // ---------------------------------------------------------------------------

  def blsVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case pkV :: msgV :: sigV :: Nil =>
        for {
          pkHex  <- expectStr("bls_verify pk")(pkV)
          msgHex <- expectStr("bls_verify msg")(msgV)
          sigHex <- expectStr("bls_verify sig")(sigV)
          pk     <- HexBytes.parseBytes(pkHex, Some(Bls12381.PublicKeyBytes), "bls_verify pk")
          msg    <- HexBytes.parseBytes(msgHex, None, "bls_verify msg")
          sig    <- HexBytes.parseBytes(sigHex, Some(Bls12381.SignatureBytes), "bls_verify sig")
        } yield BoolValue(Bls12381.verify(pk, msg, sig))
      case _ =>
        JsonLogicException(s"bls_verify: expected [pkHex(48B), msgHex, sigHex(96B)], got $values").asLeft
    }

  // ---------------------------------------------------------------------------
  // bls_aggregate_verify: [[pkHex(48B), ...], msgHex, aggSigHex(96B)] -> bool.
  //   SAME-message N-of-N aggregation (threshold / multisig case) via the Eth2
  //   ProofOfPossession fastAggregateVerify.
  // ---------------------------------------------------------------------------

  def blsAggregateVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case ArrayValue(pksV) :: msgV :: aggSigV :: Nil =>
        for {
          _      <- Either.cond(pksV.nonEmpty, (), JsonLogicException("bls_aggregate_verify: at least one public key required"))
          msgHex <- expectStr("bls_aggregate_verify msg")(msgV)
          sigHex <- expectStr("bls_aggregate_verify aggSig")(aggSigV)
          pks <- pksV.zipWithIndex.traverse {
            case (pkV, i) =>
              expectStr(s"bls_aggregate_verify pk[$i]")(pkV)
                .flatMap(HexBytes.parseBytes(_, Some(Bls12381.PublicKeyBytes), s"bls_aggregate_verify pk[$i]"))
          }
          msg    <- HexBytes.parseBytes(msgHex, None, "bls_aggregate_verify msg")
          aggSig <- HexBytes.parseBytes(sigHex, Some(Bls12381.SignatureBytes), "bls_aggregate_verify aggSig")
        } yield BoolValue(Bls12381.fastAggregateVerify(pks, msg, aggSig))
      case _ =>
        JsonLogicException(
          s"bls_aggregate_verify: expected [[pkHex(48B), ...], msgHex, aggSigHex(96B)], got $values"
        ).asLeft
    }

  // ---------------------------------------------------------------------------
  // schnorr_verify: [pkHex(64B G1), msgHex, proofHex(96B)] -> bool.
  //   Schnorr proof of knowledge / signature on BN254 G1. Convention:
  //     proof    = R(64B) || s(32B)
  //     generator G = (1, 2) (the alt_bn128 G1 base point)
  //     challenge c = SHA256(R || pk || msg) mod r   (r = BN254 group order)
  //     accept iff  s*G == R + c*pk
  // ---------------------------------------------------------------------------

  /** The BN254 G1 generator (1, 2), matching Besu's `AltBn128Point.g1()`. */
  private val SchnorrGenerator: Bn254.G1 = Bn254.G1(BigInteger.ONE, BigInteger.valueOf(2))

  def schnorrVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case pkV :: msgV :: proofV :: Nil =>
        for {
          pkHex    <- expectStr("schnorr_verify pk")(pkV)
          msgHex   <- expectStr("schnorr_verify msg")(msgV)
          proofHex <- expectStr("schnorr_verify proof")(proofV)
          pkC      <- HexBytes.parseG1(pkHex, "schnorr_verify pk")
          msg      <- HexBytes.parseBytes(msgHex, None, "schnorr_verify msg")
          // proof = R(64B) || s(32B) -> total 96 bytes.
          proof <- HexBytes.parseBytes(proofHex, Some(HexBytes.G1Bytes + HexBytes.ScalarBytes), "schnorr_verify proof")
          rBytes = proof.slice(0, HexBytes.G1Bytes)
          sBytes = proof.slice(HexBytes.G1Bytes, HexBytes.G1Bytes + HexBytes.ScalarBytes)
          rC <- HexBytes.parseG1(HexBytes.encodeBytes(rBytes), "schnorr_verify R")
          s = BigInt(1, sBytes)
          pk <- g1OnCurve(pkC, "schnorr_verify pk")
          r  <- g1OnCurve(rC, "schnorr_verify R")
          // SOUNDNESS: reject the identity / point-at-infinity public key.
          // BN254 G1 is prime-order (cofactor 1), so on-curve => in-subgroup
          // EXCEPT for the identity O = (0,0). With pk = O the verification
          // equation `s*G == R + c*pk` collapses to `s*G == R`, which an
          // attacker satisfies for ANY message by choosing s and setting
          // R = s*G -- a universal forgery. The identity pk is correct-WIDTH
          // but cryptographically invalid, so this is `false`, NOT an error
          // (malformed-width inputs still error above, as before).
          pkIsIdentity = pk.x.signum == 0 && pk.y.signum == 0
        } yield
          if (pkIsIdentity) BoolValue(false)
          else {
            // c = SHA256(R || pk || msg) mod groupOrder
            val pkBytes = HexBytes.parseBytes(pkHex, Some(HexBytes.G1Bytes), "schnorr_verify pk").toOption.get
            val digest = MessageDigest.getInstance("SHA-256").digest(rBytes ++ pkBytes ++ msg)
            val c = BigInt(1, digest).mod(BigInt(Bn254.R))
            // accept iff s*G == R + c*pk
            val lhs = SchnorrGenerator.multiply(bigInteger(s.mod(BigInt(Bn254.R))))
            val rhs = r.add(pk.multiply(bigInteger(c)))
            BoolValue(lhs.x == rhs.x && lhs.y == rhs.y)
          }
      case _ =>
        JsonLogicException(s"schnorr_verify: expected [pkHex(64B), msgHex, proofHex(96B)], got $values").asLeft
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
