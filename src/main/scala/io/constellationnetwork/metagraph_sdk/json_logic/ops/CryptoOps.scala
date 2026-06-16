package io.constellationnetwork.metagraph_sdk.json_logic.ops

import java.math.BigInteger
import java.security.MessageDigest

import cats.syntax.either._
import cats.syntax.traverse._

import io.constellationnetwork.metagraph_sdk.crypto.bls.{Bls12381, BlsBackend}
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
  //
  //   OPTIONAL BACKEND: both bls opcodes are gated on BlsBackend (the BC 1.85
  //   `org.bouncycastle.crypto.bls` jars are build/test-time unmanaged deps, never published).
  //   When the backend is absent the opcodes return a deterministic Left — they never throw.
  //   The gate runs FIRST so nothing below it can touch Bls12381 on a backend-less classpath.
  // ---------------------------------------------------------------------------

  private def requireBlsBackend(op: String): Either[JsonLogicException, Unit] =
    Either.cond(BlsBackend.isAvailable, (), JsonLogicException(BlsBackend.unavailableMessage(op)))

  def blsVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case pkV :: msgV :: sigV :: Nil =>
        for {
          _      <- requireBlsBackend("bls_verify")
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
          _      <- requireBlsBackend("bls_aggregate_verify")
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

  // ===========================================================================
  // SIGMA PROTOCOLS (classical, no-trusted-setup, Ergo / EIP-11 family).
  //
  // The Σ-protocol family is built from two atomic leaves over BN254 G1, both
  // using the SAME conventions as `schnorr_verify` above (generator (1,2), the
  // SHA256(transcript) mod R Fiat-Shamir hash family, the `0x`-fixed-width hex
  // codec, and the on-curve / identity rejection + error-vs-false discipline of
  // `groth16_verify` / `schnorr_verify`):
  //
  //   - DLog  (`proveDlog`):    knowledge of `x` s.t. `pk = x·G`.  This is the
  //     Schnorr leaf; `prove_dlog_verify` is a first-class ALIAS over
  //     `schnorrVerify` so the sigma-leaf naming is available standalone.
  //   - DHTuple (`proveDHTuple`): knowledge of `w` s.t. `u = g^w ∧ v = h^w`
  //     (a Diffie–Hellman / DDH tuple).  `prove_dhtuple_verify` is a NEW
  //     standalone leaf.
  //
  // STANDALONE vs DEFERRED-TREE. `prove_dlog_verify` / `prove_dhtuple_verify`
  // are the standalone SINGLE-leaf Σ-guards (one statement, one proof, accept
  // iff the leaf verifies). Composing several of them with the JLVM `or` /
  // `some` is CRYPTOGRAPHICALLY UNSOUND for OR / threshold: each standalone
  // proof carries its own independently-derived Fiat-Shamir challenge, so there
  // is no challenge-splitting and therefore no hiding of which leaf the prover
  // actually knows. The sound CAND / COR / CTHRESHOLD composition is the
  // DEFERRED recursive `sigma_verify` tree (see docs/sigma-verify.md), which
  // reuses the two commitment-recovery helpers below. The helpers are extracted
  // here, NOT inside the standalone verifiers (which recompute inline, exactly
  // like `schnorrVerify`), so the future tree can share one audited copy.
  // ===========================================================================

  /**
   * SHA256(bytes) reduced mod the BN254 group order R — the Fiat-Shamir hash family shared with
   * `schnorr_verify`. The caller is responsible for the transcript byte layout (the LOAD-BEARING
   * correctness choice: see `proveDhTupleVerify` for the strong-FS binding).
   */
  private def fiatShamirChallenge(transcript: Array[Byte]): BigInt =
    BigInt(1, MessageDigest.getInstance("SHA-256").digest(transcript)).mod(BigInt(Bn254.R))

  /**
   * DLog commitment recovery: from a verified Schnorr / DLog transcript with public key `pk`,
   * challenge `e` and response `z`, recover the prover's commitment
   * {{{ a = z·G − e·pk }}}
   * For an honest transcript (`z = r + e·x`, `pk = x·G`) this equals the original `a = r·G = R`,
   * since `z·G − e·pk = (r + e·x)·G − e·(x·G) = r·G`.
   *
   * This is NOT used by the standalone `schnorr_verify` / `prove_dlog_verify` (which check the
   * algebraic equation `s·G == R + c·pk` directly); it is the bottom-up reconstruction primitive
   * the deferred `sigma_verify` tree needs (the tree is GIVEN the per-leaf `(e, z)` after top-down
   * challenge propagation and must rebuild the leaf commitment to fold it into the root transcript).
   *
   * Subtraction on the curve is `a + (−e)·pk`, i.e. point-negate `pk` (negate the y-coordinate mod
   * P) and add. `G` is the BN254 generator (1,2). The scalars are reduced mod R by `multiply`.
   *
   * Visibility: public (not `private[ops]`) so the deferred-tree unit tests can pin the
   * `z·G − e·pk == R` round-trip directly; it is harmless pure curve arithmetic with no
   * argument-shape / hex handling (unlike the opcode entry points).
   */
  def dlogComputeCommitment(pk: Bn254.G1, e: BigInt, z: BigInt): Bn254.G1 = {
    val zG = SchnorrGenerator.multiply(bigInteger(z))
    val ePk = pk.multiply(bigInteger(e))
    // −ePk: negate the affine y-coordinate modulo P (the all-zero identity negates to itself).
    val negEPk =
      if (ePk.isInfinity) ePk
      else Bn254.G1(ePk.x, Bn254.P.subtract(ePk.y))
    zG.add(negEPk)
  }

  /**
   * DHTuple commitment recovery for one base `base` (either `g` or `h`) of a DDH tuple, given the
   * corresponding image `image` (`u` for `g`, `v` for `h`), challenge `e` and response `z`:
   * {{{ a = z·base − e·image }}}
   * For an honest transcript (`z = r + e·w`, `image = base^w`) this equals the original commitment
   * `a = r·base`. As with [[dlogComputeCommitment]] this is the bottom-up reconstruction primitive
   * for the deferred `sigma_verify` tree, NOT used by the standalone `prove_dhtuple_verify` (which
   * checks `z·g == a1 + e·u ∧ z·h == a2 + e·v` directly). Public for the same test-seam reason as
   * [[dlogComputeCommitment]].
   */
  def dhtupleComputeCommitment(base: Bn254.G1, image: Bn254.G1, e: BigInt, z: BigInt): Bn254.G1 = {
    val zBase = base.multiply(bigInteger(z))
    val eImg = image.multiply(bigInteger(e))
    val negEImg =
      if (eImg.isInfinity) eImg
      else Bn254.G1(eImg.x, Bn254.P.subtract(eImg.y))
    zBase.add(negEImg)
  }

  // ---------------------------------------------------------------------------
  // prove_dlog_verify: [pkHex(64B G1), msgHex, proofHex(96B)] -> bool.
  //   First-class sigma-leaf ALIAS for `schnorr_verify` (identical inputs and
  //   semantics: the DLog Σ-leaf, proof of knowledge of `x` with `pk = x·G`).
  //   Standalone single-key guard; see the SIGMA PROTOCOLS note above for why
  //   it must NOT be composed by JLVM `or` for an OR/threshold policy.
  // ---------------------------------------------------------------------------

  def proveDlogVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    schnorrVerify(values).leftMap(e => JsonLogicException(e.getMessage.replace("schnorr_verify", "prove_dlog_verify")))

  // ---------------------------------------------------------------------------
  // prove_dhtuple_verify: [gHex(64B), hHex(64B), uHex(64B), vHex(64B), msgHex, proofHex(160B)] -> bool.
  //   DDH / Diffie–Hellman-tuple Σ-leaf on BN254 G1. Statement (g,h,u,v) ∈ G1⁴,
  //   claim ∃w. u = g^w ∧ v = h^w. Convention:
  //     proof    = a1(64B) || a2(64B) || z(32B)   (total 160 bytes)
  //     a1 = g^r, a2 = h^r, z = r + e·w
  //     STRONG Fiat-Shamir: e = SHA256(g‖h‖u‖v‖a1‖a2‖msg) mod R
  //     accept iff  z·g == a1 + e·u  AND  z·h == a2 + e·v
  //
  //   STRONG-FS IS THE LOAD-BEARING CORRECTNESS POINT. The challenge MUST bind
  //   the FULL statement (g,h,u,v) AND BOTH commitments (a1,a2) AND the message.
  //   A weak transcript that omits any of these is forgeable (the prover could
  //   adaptively choose a commitment after seeing the challenge, or rebind the
  //   statement / message); see docs/sigma-verify.md and the cited weak-FS
  //   attack class (SPL ZK-ElGamal, Trail of Bits "Weak Fiat-Shamir Attacks").
  // ---------------------------------------------------------------------------

  /** Total proof width: a1(64B) || a2(64B) || z(32B). */
  private val DhTupleProofBytes: Int = HexBytes.G1Bytes + HexBytes.G1Bytes + HexBytes.ScalarBytes

  def proveDhTupleVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case gV :: hV :: uV :: vV :: msgV :: proofV :: Nil =>
        for {
          gHex     <- expectStr("prove_dhtuple_verify g")(gV)
          hHex     <- expectStr("prove_dhtuple_verify h")(hV)
          uHex     <- expectStr("prove_dhtuple_verify u")(uV)
          vHex     <- expectStr("prove_dhtuple_verify v")(vV)
          msgHex   <- expectStr("prove_dhtuple_verify msg")(msgV)
          proofHex <- expectStr("prove_dhtuple_verify proof")(proofV)
          gC       <- HexBytes.parseG1(gHex, "prove_dhtuple_verify g")
          hC       <- HexBytes.parseG1(hHex, "prove_dhtuple_verify h")
          uC       <- HexBytes.parseG1(uHex, "prove_dhtuple_verify u")
          vC       <- HexBytes.parseG1(vHex, "prove_dhtuple_verify v")
          msg      <- HexBytes.parseBytes(msgHex, None, "prove_dhtuple_verify msg")
          // proof = a1(64B) || a2(64B) || z(32B) -> total 160 bytes.
          proof <- HexBytes.parseBytes(proofHex, Some(DhTupleProofBytes), "prove_dhtuple_verify proof")
          a1Bytes = proof.slice(0, HexBytes.G1Bytes)
          a2Bytes = proof.slice(HexBytes.G1Bytes, HexBytes.G1Bytes * 2)
          zBytes = proof.slice(HexBytes.G1Bytes * 2, DhTupleProofBytes)
          a1C <- HexBytes.parseG1(HexBytes.encodeBytes(a1Bytes), "prove_dhtuple_verify a1")
          a2C <- HexBytes.parseG1(HexBytes.encodeBytes(a2Bytes), "prove_dhtuple_verify a2")
          z = BigInt(1, zBytes)
          g  <- g1OnCurve(gC, "prove_dhtuple_verify g")
          h  <- g1OnCurve(hC, "prove_dhtuple_verify h")
          u  <- g1OnCurve(uC, "prove_dhtuple_verify u")
          v  <- g1OnCurve(vC, "prove_dhtuple_verify v")
          a1 <- g1OnCurve(a1C, "prove_dhtuple_verify a1")
          a2 <- g1OnCurve(a2C, "prove_dhtuple_verify a2")
          // SOUNDNESS: reject the identity / point-at-infinity on ANY of the four statement
          // points. BN254 G1 is prime-order (cofactor 1), so on-curve => in-subgroup EXCEPT for
          // O = (0,0). An identity base (g or h) makes the corresponding equation collapse to
          // `z·O == a + e·image`, i.e. `O == a + e·image`, which an attacker satisfies by
          // choosing the matching commitment a freely (a universal forgery for that coordinate);
          // an identity image (u or v) similarly degenerates the hiding of w. These are
          // correct-WIDTH but cryptographically invalid, so `false`, NOT an error (malformed-width
          // inputs still error above). a1 / a2 may legitimately be the identity (r ≡ 0), so they
          // are NOT rejected here — they are still bound into the transcript below.
          stmtHasIdentity = g.isInfinity || h.isInfinity || u.isInfinity || v.isInfinity
        } yield
          if (stmtHasIdentity) BoolValue(false)
          else {
            // STRONG Fiat-Shamir: bind the full statement AND both commitments AND the message.
            // Re-encode each point to its canonical fixed-width 64-byte form (parseG1 already
            // validated width, so .get is safe) so the transcript is layout-deterministic.
            def fixed(role: String, hex: String): Array[Byte] =
              HexBytes.parseBytes(hex, Some(HexBytes.G1Bytes), role).toOption.get
            val transcript =
              fixed("g", gHex) ++ fixed("h", hHex) ++ fixed("u", uHex) ++ fixed("v", vHex) ++
              a1Bytes ++ a2Bytes ++ msg
            val e = fiatShamirChallenge(transcript)
            val zr = bigInteger(z.mod(BigInt(Bn254.R)))
            // accept iff z·g == a1 + e·u  AND  z·h == a2 + e·v
            val lhs1 = g.multiply(zr)
            val rhs1 = a1.add(u.multiply(bigInteger(e)))
            val lhs2 = h.multiply(zr)
            val rhs2 = a2.add(v.multiply(bigInteger(e)))
            val ok = lhs1.x == rhs1.x && lhs1.y == rhs1.y && lhs2.x == rhs2.x && lhs2.y == rhs2.y
            BoolValue(ok)
          }
      case _ =>
        JsonLogicException(
          s"prove_dhtuple_verify: expected [gHex(64B), hHex(64B), uHex(64B), vHex(64B), msgHex, proofHex(160B)], got $values"
        ).asLeft
    }

  // ===========================================================================
  // sigma_verify: the RECURSIVE CDS Σ-protocol proposition verifier (Phase 2).
  //
  //   {"sigma_verify": [ <proposition>, <proof>, <messageHex> ]} -> bool
  //
  // This is the sound CAND / COR / CTHRESHOLD composition of the DLog / DHTuple
  // leaves above (the Ergo `SigSerializer` / `verifySignature` "Verifier Steps
  // 1-6" restated for BN254 G1). It reuses `dlogComputeCommitment` /
  // `dhtupleComputeCommitment` for the bottom-up commitment reconstruction. See
  // docs/sigma-verify.md for the full rationale.
  //
  // CROWN-JEWEL CORRECTNESS TARGETS (audit these first):
  //   1. STRONG Fiat-Shamir. The root challenge binds the WHOLE statement (every
  //      leaf's points, the tree shape, threshold params), EVERY reconstructed
  //      commitment, AND the message, under a FROZEN canonical byte layout (see
  //      `serializeTree`). A transcript that omits any statement/commitment is
  //      forgeable (the SPL ZK-ElGamal / Trail-of-Bits weak-FS class). Nothing
  //      the prover controls is left out of the hash.
  //   2. CDS challenge-splitting. OR = XOR over fixed-width 32-byte challenges;
  //      THRESHOLD(k,n) = GF(2^8) Shamir, degree (n-k), constant term = parent
  //      challenge, evaluated byte-wise across the 32 lanes (exactly Ergo). The
  //      verifier RECOMPUTES the relations and rejects any inconsistency.
  //   3. Commitments are RECONSTRUCTED, never trusted from the proof, so a forged
  //      response necessarily changes the hashed commitment and breaks step 6.
  //
  // ERROR-VS-FALSE (lockstep with the leaves): malformed (bad hex/width, off-curve
  // point, structurally invalid tree, k<=0 or k>n, prop/proof shape mismatch,
  // duplicate/out-of-range threshold index, wrong polynomial degree) => hard
  // JsonLogicException. Well-formed-but-cryptographically-wrong (root hash !=
  // root challenge, OR challenges do not XOR, threshold does not interpolate,
  // identity statement point) => `false`.
  // ===========================================================================

  /**
   * FROZEN canonical serialization for the strong-FS transcript (docs/sigma-verify.md §4).
   *
   * NORMATIVE BYTE LAYOUT (pre-order, children in array order — array order is part of the
   * statement; reordering children changes the proposition):
   *
   *   - Node tag: ONE fixed byte per kind — dlog=0x00, dhtuple=0x01, and=0x02, or=0x03,
   *     threshold=0x04.
   *   - Threshold k AND every child-count: fixed-width 4-byte big-endian (`encodeUInt(_, 4)`),
   *     so the structure (arity, k) is itself bound — an attacker cannot re-bracket the tree.
   *   - Points (pk, g, h, u, v, and the RECONSTRUCTED commitments a / a1 / a2): the canonical
   *     64-byte big-endian `x‖y` form (`HexBytes.encodeG1`), the SAME fixed-width encoding the
   *     leaf opcodes already use. No compression, no variable width.
   *
   *   dlog      := 0x00 ‖ pk(64) ‖ a(64)
   *   dhtuple   := 0x01 ‖ g(64) ‖ h(64) ‖ u(64) ‖ v(64) ‖ a1(64) ‖ a2(64)
   *   and       := 0x02 ‖ nChildren(4) ‖ child_0 ‖ … ‖ child_{n-1}
   *   or        := 0x03 ‖ nChildren(4) ‖ child_0 ‖ … ‖ child_{n-1}
   *   threshold := 0x04 ‖ k(4) ‖ nChildren(4) ‖ child_0 ‖ … ‖ child_{n-1}
   *
   * Root challenge := SHA256( DomainSep ‖ serializeTree(root) ‖ message ) mod R, with
   * DomainSep = ascii("sigma_verify:v1") (separates this hash family from the per-leaf
   * `schnorr_verify` / `prove_dhtuple_verify` transcripts so a leaf proof can never be
   * replayed as a 1-node tree proof and vice-versa).
   */
  private object Sigma {

    // One fixed tag byte per node kind (part of the bound transcript).
    val TagDlog: Byte = 0x00
    val TagDhTuple: Byte = 0x01
    val TagAnd: Byte = 0x02
    val TagOr: Byte = 0x03
    val TagThreshold: Byte = 0x04

    /** Domain separator for the sigma_verify root hash (distinct from the leaf transcripts). */
    val DomainSep: Array[Byte] = "sigma_verify:v1".getBytes("US-ASCII")

    /** Fixed challenge width in bytes (32-byte big-endian, reduced mod R). */
    val ChallengeBytes: Int = HexBytes.ScalarBytes // 32

    // --- Parsed PROPOSITION tree (statement only; no challenges/responses). ---
    sealed trait PropNode
    final case class PropDlog(pk: Bn254.G1, pkBytes: Array[Byte]) extends PropNode
    final case class PropDhTuple(
      g: Bn254.G1,
      h: Bn254.G1,
      u: Bn254.G1,
      v: Bn254.G1,
      gBytes: Array[Byte],
      hBytes: Array[Byte],
      uBytes: Array[Byte],
      vBytes: Array[Byte]
    ) extends PropNode
    final case class PropAnd(children: List[PropNode]) extends PropNode
    final case class PropOr(children: List[PropNode]) extends PropNode
    final case class PropThreshold(k: Int, children: List[PropNode]) extends PropNode

    // --- Parsed PROOF tree (per-node challenge `e`; per-leaf response `z`). ---
    sealed trait ProofNode { def e: Array[Byte] }
    final case class ProofDlog(e: Array[Byte], z: BigInt) extends ProofNode
    final case class ProofDhTuple(e: Array[Byte], z: BigInt) extends ProofNode
    final case class ProofAnd(e: Array[Byte], children: List[ProofNode]) extends ProofNode
    final case class ProofOr(e: Array[Byte], children: List[ProofNode]) extends ProofNode
    final case class ProofThreshold(e: Array[Byte], k: Int, children: List[ProofNode]) extends ProofNode
  }

  /** SHA256 of a byte string (no mod), used for the GF-independent transcript hash convention. */
  private def sha256Bytes(bytes: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bytes)

  // ---------------------------------------------------------------------------
  // GF(2^8) Shamir arithmetic for the CTHRESHOLD challenge split (Ergo / AES field).
  //
  // The challenge is a 32-byte array; threshold interpolation is performed BYTE-WISE
  // (32 independent GF(2^8) lanes), exactly as Ergo's `GF2_192_Poly` reduced to the
  // byte field. Field = GF(2^8) with the AES reduction polynomial x^8+x^4+x^3+x+1
  // (0x11b). Indices are the child positions 1..n (0 is reserved for the parent
  // challenge = the polynomial's value at 0).
  // ---------------------------------------------------------------------------

  /** GF(2^8) multiply (Russian-peasant, AES reduction poly 0x11b). Pure, fixed 8-round fold. */
  private def gfMul(a0: Int, b0: Int): Int = {
    // Fold over the 8 bits of b; (acc product, shifting a). Subtraction/addition in GF(2^m) is XOR.
    val (p, _, _) = (0 until 8).foldLeft((0, a0 & 0xff, b0 & 0xff)) {
      case ((prod, a, b), _) =>
        val nextProd = if ((b & 1) != 0) prod ^ a else prod
        val shifted = (a << 1) & 0xff
        val nextA = if ((a & 0x80) != 0) shifted ^ 0x1b else shifted // reduce by 0x11b's low byte
        (nextProd, nextA, b >> 1)
    }
    p & 0xff
  }

  /** GF(2^8) multiplicative inverse via Fermat (a^254 = a^-1 for a != 0). gfInv(0) = 0. */
  private def gfInv(a: Int): Int =
    if ((a & 0xff) == 0) 0
    else {
      // a^254: square-and-multiply over the 8 bits of the exponent 254 = 0b11111110.
      val (result, _) = (0 until 8).foldLeft((1, a & 0xff)) {
        case ((acc, base), bit) =>
          val nextAcc = if (((254 >> bit) & 1) != 0) gfMul(acc, base) else acc
          (nextAcc, gfMul(base, base))
      }
      result & 0xff
    }

  /**
   * Lagrange evaluation in GF(2^8): given sample points `(xs(i), ys(i))` (all `xs` DISTINCT),
   * return the interpolating polynomial evaluated at `xEval`. Used to (a) reconstruct the
   * degree-`(n-k)` threshold polynomial from `(0, parentChallenge)` + the first `n-k` child
   * points, and (b) check the remaining `k` child points lie on it. Pure GF(2^8) arithmetic.
   *
   * Caller guarantees `xs` are pairwise distinct (duplicate/out-of-range indices are rejected
   * upstream as a hard error), so every `(x_i - x_j)` is non-zero and invertible.
   */
  private def gfLagrangeEval(xs: Array[Int], ys: Array[Int], xEval: Int): Int =
    xs.indices.foldLeft(0) { (acc, i) =>
      // basis_i(xEval) = ∏_{j!=i} (xEval - xs_j) / (xs_i - xs_j); subtraction == XOR in GF(2^m).
      val (num, den) = xs.indices.foldLeft((1, 1)) {
        case ((nm, dn), j) =>
          if (j == i) (nm, dn)
          else (gfMul(nm, xEval ^ xs(j)), gfMul(dn, xs(i) ^ xs(j)))
      }
      acc ^ gfMul(ys(i), gfMul(num, gfInv(den)))
    } & 0xff

  // ---------------------------------------------------------------------------
  // sigma_verify entry point + recursive verifier.
  // ---------------------------------------------------------------------------

  def sigmaVerify(values: List[JsonLogicValue]): Either[JsonLogicException, JsonLogicValue] =
    values match {
      case propV :: proofV :: msgV :: Nil =>
        for {
          msgHex <- expectStr("sigma_verify message")(msgV)
          msg    <- HexBytes.parseBytes(msgHex, None, "sigma_verify message")
          prop   <- parsePropNode(propV, "sigma_verify.proposition")
          proof  <- parseProofNode(proofV, "sigma_verify.proof")
          // STRUCTURAL shape agreement (prop vs proof) is a hard error (encoding fault), checked
          // as we go below. The cryptographic outcome (true/false) is computed in verifyTree.
          result <- verifyTree(prop, proof, msg)
        } yield BoolValue(result)
      case _ =>
        JsonLogicException(
          s"sigma_verify: expected [proposition, proof, messageHex], got $values"
        ).asLeft
    }

  // --- Proposition parsing (statement only). Malformed => hard error. ---

  private def sigmaField(role: String, m: Map[String, JsonLogicValue], key: String): Either[JsonLogicException, JsonLogicValue] =
    m.get(key).toRight(JsonLogicException(s"$role: missing required field '$key'"))

  private def sigmaPoint(role: String, m: Map[String, JsonLogicValue], key: String): Either[JsonLogicException, (Bn254.G1, Array[Byte])] =
    for {
      v   <- sigmaField(role, m, key)
      hex <- expectStr(s"$role.$key")(v)
      c   <- HexBytes.parseG1(hex, s"$role.$key")
      p   <- g1OnCurve(c, s"$role.$key")
      // Canonical fixed-width 64-byte re-encoding for the transcript (parseG1 validated width).
      bytes <- HexBytes.parseBytes(hex, Some(HexBytes.G1Bytes), s"$role.$key")
    } yield (p, bytes)

  private def sigmaChildrenValues(role: String, m: Map[String, JsonLogicValue]): Either[JsonLogicException, List[JsonLogicValue]] =
    sigmaField(role, m, "children").flatMap {
      case ArrayValue(arr) if arr.nonEmpty => arr.asRight
      case ArrayValue(_)                   => JsonLogicException(s"$role: 'children' must be a non-empty array").asLeft
      case other                           => JsonLogicException(s"$role: 'children' must be an array, got ${other.tag}").asLeft
    }

  private def sigmaInt(role: String, m: Map[String, JsonLogicValue], key: String): Either[JsonLogicException, Int] =
    sigmaField(role, m, key).flatMap {
      case IntValue(i) if i >= 0 && i <= Int.MaxValue => i.toInt.asRight
      case IntValue(i)                                => JsonLogicException(s"$role.$key: out of range: $i").asLeft
      case other                                      => JsonLogicException(s"$role.$key: expected an integer, got ${other.tag}").asLeft
    }

  private def parsePropNode(v: JsonLogicValue, role: String): Either[JsonLogicException, Sigma.PropNode] =
    v match {
      case MapValue(m) =>
        sigmaField(role, m, "type").flatMap(expectStr(s"$role.type")).flatMap {
          case "dlog" =>
            sigmaPoint(role, m, "pk").map { case (pk, b) => Sigma.PropDlog(pk, b) }
          case "dhtuple" =>
            for {
              g  <- sigmaPoint(role, m, "g")
              h  <- sigmaPoint(role, m, "h")
              u  <- sigmaPoint(role, m, "u")
              vv <- sigmaPoint(role, m, "v")
            } yield Sigma.PropDhTuple(g._1, h._1, u._1, vv._1, g._2, h._2, u._2, vv._2)
          case "and" =>
            for {
              cs       <- sigmaChildrenValues(role, m)
              children <- cs.zipWithIndex.traverse { case (c, i) => parsePropNode(c, s"$role.and[$i]") }
            } yield Sigma.PropAnd(children)
          case "or" =>
            for {
              cs       <- sigmaChildrenValues(role, m)
              children <- cs.zipWithIndex.traverse { case (c, i) => parsePropNode(c, s"$role.or[$i]") }
            } yield Sigma.PropOr(children)
          case "threshold" =>
            for {
              k        <- sigmaInt(role, m, "k")
              cs       <- sigmaChildrenValues(role, m)
              children <- cs.zipWithIndex.traverse { case (c, i) => parsePropNode(c, s"$role.threshold[$i]") }
              n = children.length
              // Structural validity: 1 <= k <= n. A degree-(n-k) polynomial must exist, and the
              // n child indices 1..n must be valid GF(2^8) field elements (n <= 255).
              _ <- Either.cond(k >= 1, (), JsonLogicException(s"$role.threshold: k must be >= 1, got $k"))
              _ <- Either.cond(k <= n, (), JsonLogicException(s"$role.threshold: k ($k) > number of children ($n)"))
              _ <- Either.cond(n <= 255, (), JsonLogicException(s"$role.threshold: at most 255 children (GF(2^8) indices), got $n"))
            } yield Sigma.PropThreshold(k, children)
          case other =>
            JsonLogicException(s"$role: unknown node type '$other'").asLeft
        }
      case other =>
        JsonLogicException(s"$role: expected a proposition node object, got ${other.tag}").asLeft
    }

  // --- Proof parsing (per-node challenge + per-leaf response). Malformed => hard error. ---

  private def sigmaChallenge(role: String, m: Map[String, JsonLogicValue]): Either[JsonLogicException, Array[Byte]] =
    for {
      v   <- sigmaField(role, m, "e")
      hex <- expectStr(s"$role.e")(v)
      // Challenge is a fixed 32-byte big-endian value, reduced mod R (canonicity not required:
      // the verifier compares it byte-wise against the recomputed challenge, also reduced mod R).
      bytes <- HexBytes.parseBytes(hex, Some(Sigma.ChallengeBytes), s"$role.e")
    } yield bytes

  private def sigmaResponse(role: String, m: Map[String, JsonLogicValue]): Either[JsonLogicException, BigInt] =
    for {
      v   <- sigmaField(role, m, "z")
      hex <- expectStr(s"$role.z")(v)
      z   <- HexBytes.parseScalar(hex, s"$role.z")
    } yield z

  private def parseProofNode(v: JsonLogicValue, role: String): Either[JsonLogicException, Sigma.ProofNode] =
    v match {
      case MapValue(m) =>
        for {
          e   <- sigmaChallenge(role, m)
          typ <- sigmaField(role, m, "type").flatMap(expectStr(s"$role.type"))
          node <- typ match {
            case "dlog"    => sigmaResponse(role, m).map(z => Sigma.ProofDlog(e, z))
            case "dhtuple" => sigmaResponse(role, m).map(z => Sigma.ProofDhTuple(e, z))
            case "and" =>
              for {
                cs       <- sigmaChildrenValues(role, m)
                children <- cs.zipWithIndex.traverse { case (c, i) => parseProofNode(c, s"$role.and[$i]") }
              } yield Sigma.ProofAnd(e, children)
            case "or" =>
              for {
                cs       <- sigmaChildrenValues(role, m)
                children <- cs.zipWithIndex.traverse { case (c, i) => parseProofNode(c, s"$role.or[$i]") }
              } yield Sigma.ProofOr(e, children)
            case "threshold" =>
              for {
                k        <- sigmaInt(role, m, "k")
                cs       <- sigmaChildrenValues(role, m)
                children <- cs.zipWithIndex.traverse { case (c, i) => parseProofNode(c, s"$role.threshold[$i]") }
              } yield Sigma.ProofThreshold(e, k, children)
            case other =>
              JsonLogicException(s"$role: unknown node type '$other'").asLeft
          }
        } yield node
      case other =>
        JsonLogicException(s"$role: expected a proof node object, got ${other.tag}").asLeft
    }

  /**
   * The recursive verifier (Ergo Verifier Steps 1-6). Returns:
   *   Left(JsonLogicException) -> MALFORMED (prop/proof shape mismatch, off-curve, identity
   *                               base, bad threshold degree/index) — a hard encoding fault;
   *   Right(false)             -> well-formed but cryptographically INVALID;
   *   Right(true)              -> accept.
   *
   * Flow: (1) recursively check the CDS challenge-split relations top-down and reconstruct every
   * leaf commitment bottom-up; (2) serialize the WHOLE tree (statement points + reconstructed
   * commitments) under the frozen layout; (3) hash (domain-separated, mod R) and accept iff it
   * equals the ROOT challenge carried in the proof.
   */
  private def verifyTree(prop: Sigma.PropNode, proof: Sigma.ProofNode, msg: Array[Byte]): Either[JsonLogicException, Boolean] =
    for {
      // Step 3 (CDS split) + Step 4 (commitment reconstruction), folded into one recursive walk.
      // Returns (structurallyOk?, serializedBytes). The boolean carries the WELL-FORMED-but-wrong
      // verdict (identity base, OR XOR mismatch, threshold non-interpolation); a structural fault
      // short-circuits as Left.
      walk <- verifyNode(prop, proof, "sigma_verify")
      (cryptoOk, serialized) = walk
    } yield
      if (!cryptoOk) false
      else {
        // Steps 5-6: STRONG Fiat-Shamir over (DomainSep ‖ canonical tree ‖ message), mod R,
        // compared against the ROOT challenge. Both sides reduced mod R (the proof's root e is
        // a 32-byte value, the recomputed one is SHA256 mod R), so compare as BigInt mod R.
        val recomputedRoot = BigInt(1, sha256Bytes(Sigma.DomainSep ++ serialized ++ msg)).mod(BigInt(Bn254.R))
        val claimedRoot = BigInt(1, proof.e).mod(BigInt(Bn254.R))
        recomputedRoot == claimedRoot
      }

  /**
   * One recursive node visit. The PARENT challenge `proof.e` is the node's own propagated
   * challenge (set by the parent's split, or the root challenge at the top). This call:
   *   - reconstructs the leaf commitment(s) from `(e, z)` (Step 4) and serializes the subtree
   *     (Step 5) under the frozen layout; and
   *   - for connectives, CHECKS the child challenges satisfy the CDS relation (Step 3) and
   *     recurses into each child with the child's own carried challenge.
   * Returns `(cryptoOk, serializedBytes)`: `cryptoOk = false` is a well-formed-but-wrong verdict
   * that propagates up (so the whole proof is `false`); `Left` is a structural/encoding fault.
   *
   * Prop/proof SHAPE MISMATCH (different `type`, different child counts) is a hard error.
   */
  private def verifyNode(
    prop: Sigma.PropNode,
    proof: Sigma.ProofNode,
    role: String
  ): Either[JsonLogicException, (Boolean, Array[Byte])] =
    (prop, proof) match {
      // --- DLog leaf: reconstruct a = z·G − e·pk, serialize 0x00 ‖ pk ‖ a. ---
      case (Sigma.PropDlog(pk, pkBytes), Sigma.ProofDlog(e, z)) =>
        // SOUNDNESS: reject the identity pk (universal forgery, mirrors schnorr_verify). The
        // commitment is reconstructed from the challenge reduced mod R (same as the leaf opcode).
        if (pk.isInfinity) (false, Array.emptyByteArray).asRight
        else {
          val eScalar = BigInt(1, e).mod(BigInt(Bn254.R))
          val a = dlogComputeCommitment(pk, eScalar, z.mod(BigInt(Bn254.R)))
          for {
            aBytes <- encodeG1Bytes(a, s"$role.dlog.a")
          } yield (true, Array(Sigma.TagDlog) ++ pkBytes ++ aBytes)
        }

      // --- DHTuple leaf: reconstruct a1 = z·g − e·u, a2 = z·h − e·v; serialize 0x01 ‖ g‖h‖u‖v‖a1‖a2. ---
      case (
            Sigma.PropDhTuple(g, h, u, vv, gB, hB, uB, vB),
            Sigma.ProofDhTuple(e, z)
          ) =>
        // SOUNDNESS: reject identity on any statement point (g/h base => collapse; u/v image =>
        // degenerate hiding), identical to prove_dhtuple_verify. The single shared response z is
        // used for BOTH coordinate reconstructions (the DDH leaf has one witness, one response).
        if (g.isInfinity || h.isInfinity || u.isInfinity || vv.isInfinity) (false, Array.emptyByteArray).asRight
        else {
          val eScalar = BigInt(1, e).mod(BigInt(Bn254.R))
          val zr = z.mod(BigInt(Bn254.R))
          val a1 = dhtupleComputeCommitment(g, u, eScalar, zr)
          val a2 = dhtupleComputeCommitment(h, vv, eScalar, zr)
          for {
            a1Bytes <- encodeG1Bytes(a1, s"$role.dhtuple.a1")
            a2Bytes <- encodeG1Bytes(a2, s"$role.dhtuple.a2")
          } yield (true, Array(Sigma.TagDhTuple) ++ gB ++ hB ++ uB ++ vB ++ a1Bytes ++ a2Bytes)
        }

      // --- CAND: every child challenge MUST equal the node challenge (Step 3, AND rule). ---
      case (Sigma.PropAnd(pChildren), Sigma.ProofAnd(e, prChildren)) =>
        for {
          _ <- Either.cond(
            pChildren.length == prChildren.length,
            (),
            JsonLogicException(s"$role.and: proposition/proof child count mismatch (${pChildren.length} vs ${prChildren.length})")
          )
          // AND copies the parent challenge to each child; the proof must reflect that exactly.
          childChallengesOk = prChildren.forall(c => constantTimeEq(c.e, e))
          walked <- pChildren.zip(prChildren).zipWithIndex.traverse {
            case ((pc, prc), i) => verifyNode(pc, prc, s"$role.and[$i]")
          }
          allOk = childChallengesOk && walked.forall(_._1)
          body = walked.foldLeft(Array.emptyByteArray)((acc, w) => acc ++ w._2)
        } yield (allOk, Array(Sigma.TagAnd) ++ uint32(pChildren.length) ++ body)

      // --- COR: child challenges MUST XOR to the node challenge (Step 3, OR rule = CDS XOR). ---
      case (Sigma.PropOr(pChildren), Sigma.ProofOr(e, prChildren)) =>
        for {
          _ <- Either.cond(
            pChildren.length == prChildren.length,
            (),
            JsonLogicException(s"$role.or: proposition/proof child count mismatch (${pChildren.length} vs ${prChildren.length})")
          )
          // CDS OR: ⊕ eᵢ == e_parent over the fixed-width 32-byte challenges. This is the binding
          // that makes simulating ALL branches impossible — the free challenges cannot be made to
          // XOR to the FS-derived root unless the prover can invert the hash.
          xorOk = constantTimeEq(xorBytes(prChildren.map(_.e), Sigma.ChallengeBytes), e)
          walked <- pChildren.zip(prChildren).zipWithIndex.traverse {
            case ((pc, prc), i) => verifyNode(pc, prc, s"$role.or[$i]")
          }
          allOk = xorOk && walked.forall(_._1)
          body = walked.foldLeft(Array.emptyByteArray)((acc, w) => acc ++ w._2)
        } yield (allOk, Array(Sigma.TagOr) ++ uint32(pChildren.length) ++ body)

      // --- CTHRESHOLD(k,n): child challenges are P(1..n) for a degree-(n-k) GF(2^8) poly P,
      //     P(0) = node challenge. Verify byte-wise interpolation (Step 3, threshold rule). ---
      case (Sigma.PropThreshold(pk_, pChildren), Sigma.ProofThreshold(e, prk, prChildren)) =>
        for {
          _ <- Either.cond(
            pk_ == prk,
            (),
            JsonLogicException(s"$role.threshold: proposition/proof k mismatch ($pk_ vs $prk)")
          )
          _ <- Either.cond(
            pChildren.length == prChildren.length,
            (),
            JsonLogicException(s"$role.threshold: proposition/proof child count mismatch (${pChildren.length} vs ${prChildren.length})")
          )
          n = pChildren.length
          // child index i (1..n) is the GF(2^8) evaluation point; 0 is the parent challenge.
          interpOk = thresholdInterpolates(e, prChildren.map(_.e), pk_, n)
          walked <- pChildren.zip(prChildren).zipWithIndex.traverse {
            case ((pc, prc), i) => verifyNode(pc, prc, s"$role.threshold[$i]")
          }
          allOk = interpOk && walked.forall(_._1)
          body = walked.foldLeft(Array.emptyByteArray)((acc, w) => acc ++ w._2)
        } yield (allOk, Array(Sigma.TagThreshold) ++ uint32(pk_) ++ uint32(n) ++ body)

      // --- Any other (prop, proof) pairing is a structural shape mismatch: hard error. ---
      case (p, pr) =>
        JsonLogicException(s"$role: proposition/proof node-type mismatch (${nodeKind(p)} vs ${nodeKind(pr)})").asLeft
    }

  /**
   * CTHRESHOLD interpolation check (Step 3, byte-wise GF(2^8)). The `n` child challenges must be
   * the evaluations `P(1), …, P(n)` of a polynomial `P` of degree `(n-k)` over GF(2^8) with
   * `P(0) = parentChallenge`, computed independently in each of the 32 byte-lanes (exactly Ergo,
   * which treats the challenge as a coefficient vector and interpolates per lane).
   *
   * Method: the polynomial has degree `(n-k)` => it is fully determined by `(n-k+1)` points. Use
   * `(0, parentChallenge)` plus the FIRST `(n-k)` child points as the defining set, then verify
   * the remaining `k` child points lie on the interpolant. (Equivalently: any `(n-k+1)`-subset
   * determines `P`; the other points must be consistent. Choosing `0` + the first `(n-k)` matches
   * the prover, which fixes `P(0)` and the `(n-k)` simulated child challenges and DERIVES the `k`
   * real ones as `P(i)`.) When `k == n` the polynomial is the constant `parentChallenge`, so
   * EVERY child challenge must equal the parent — the CAND-like degenerate case.
   *
   * `false` (not error) on mismatch: a well-formed proof whose shares simply do not interpolate.
   */
  private def thresholdInterpolates(parentE: Array[Byte], childEs: List[Array[Byte]], k: Int, n: Int): Boolean = {
    val degree = n - k // polynomial degree; (degree + 1) points define it
    val childArr = childEs.toArray
    // Defining points: x = 0 (parent), then child indices 1..(n-k). Indices are DISTINCT by
    // construction (0,1,2,…), so the Lagrange denominators are all invertible.
    val knownCount = degree + 1
    val xs = Array.tabulate(knownCount)(i => i) // 0,1,...,degree  (child j sits at x=j+1)
    // Each of the 32 byte-lanes must independently interpolate. Per lane: build the (degree+1)
    // defining y-values [P(0)=parent, child_0, …, child_{degree-1}] and verify the remaining
    // children (x = degree+1 .. n) lie on the interpolant. `forall` short-circuits like the loop.
    (0 until Sigma.ChallengeBytes).forall { lane =>
      val ys = Array.tabulate(knownCount) { j =>
        if (j == 0) parentE(lane) & 0xff // P(0) = parent challenge
        else childArr(j - 1)(lane) & 0xff // child (j-1) sits at x = j
      }
      // Remaining (unconstrained) children: indices degree .. n-1, i.e. x = degree+1 .. n.
      (degree until n).forall { c =>
        (childArr(c)(lane) & 0xff) == gfLagrangeEval(xs, ys, c + 1)
      }
    }
  }

  /** Node-kind label for shape-mismatch error messages. */
  private def nodeKind(n: Sigma.PropNode): String = n match {
    case _: Sigma.PropDlog      => "dlog"
    case _: Sigma.PropDhTuple   => "dhtuple"
    case _: Sigma.PropAnd       => "and"
    case _: Sigma.PropOr        => "or"
    case _: Sigma.PropThreshold => "threshold"
  }

  private def nodeKind(n: Sigma.ProofNode): String = n match {
    case _: Sigma.ProofDlog      => "dlog"
    case _: Sigma.ProofDhTuple   => "dhtuple"
    case _: Sigma.ProofAnd       => "and"
    case _: Sigma.ProofOr        => "or"
    case _: Sigma.ProofThreshold => "threshold"
  }

  /** Re-encode a reconstructed G1 commitment to its canonical 64-byte big-endian bytes. */
  private def encodeG1Bytes(p: Bn254.G1, role: String): Either[JsonLogicException, Array[Byte]] =
    encodeG1(p).flatMap(hex => HexBytes.parseBytes(hex, Some(HexBytes.G1Bytes), role))

  /** Fixed 4-byte big-endian encoding of a non-negative count / threshold k (bounds the structure). */
  private def uint32(v: Int): Array[Byte] =
    Array((v >>> 24).toByte, (v >>> 16).toByte, (v >>> 8).toByte, v.toByte)

  /** XOR a list of equal-width byte arrays into one `width`-byte array (the CDS OR fold). */
  private def xorBytes(arrays: List[Array[Byte]], width: Int): Array[Byte] =
    Array.tabulate(width)(i => arrays.foldLeft(0)((acc, a) => acc ^ a(i)).toByte)

  /** Length-checked, data-independent byte equality (no early-exit timing leak across challenges). */
  private def constantTimeEq(a: Array[Byte], b: Array[Byte]): Boolean =
    a.length == b.length && a.indices.foldLeft(0)((diff, i) => diff | (a(i) ^ b(i))) == 0

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
