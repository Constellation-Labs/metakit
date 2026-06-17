package json_logic

import java.math.BigInteger

import cats.syntax.either._

import io.constellationnetwork.metagraph_sdk.crypto.zk.Bn254
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.{CryptoOps, HexBytes}

import org.scalacheck.{Arbitrary, Gen}
import weaver.scalacheck.{CheckConfig, Checkers}
import weaver.{Expectations, SimpleIOSuite, SourceLocation}

/**
 * TOTALITY (no-throw) suite for the pure `CryptoOps` crypto-opcode layer.
 *
 * GOAL (purity/safety, NOT correctness): every public `CryptoOps` opcode must be TOTAL — on ANY
 * input it returns `Either[JsonLogicException, JsonLogicValue]` (a `Left` for malformed, a `Right`
 * for a value) and NEVER throws. A thrown exception here would escape the `Either` error channel
 * into the consensus combiner (block-poisoning / node-crash risk), since these ops are pure (return
 * `Either`, not `F[_]`).
 *
 * METHOD: feed a large battery of RANDOM + STRUCTURED-ADVERSARIAL inputs to each opcode and assert
 * the call did not THROW. The "did it throw?" probe is `Either.catchNonFatal(op)`:
 *   - if the opcode returned normally (a `Left` OR a `Right`), `catchNonFatal` yields `Right(either)`
 *     -> PASS (the opcode handled the input through its error channel, as required);
 *   - if the opcode THREW, `catchNonFatal` yields `Left(throwable)` -> FAIL (totality violation).
 * We do NOT assert the true/false/error OUTCOME (that is the job of the conformance suites); we only
 * assert no-throw. Correctness / behavior is pinned UNCHANGED by SigmaOpsSuite / SigmaVerifySuite /
 * ZkVectorConformanceSuite / GasVectorConformanceSuite / SigmaVectorGen.
 *
 * Adversarial input classes covered (per opcode and via the shared value generator): empty args,
 * wrong arity, non-string / non-hex values, odd-length hex, wrong-width hex, off-curve / identity
 * points, huge arrays, deeply-nested + malformed sigma proposition / proof trees, non-canonical
 * scalars (z = z + R), 32-byte challenges (vs the 31-byte domain), duplicate keys, and random raw
 * bytes. `minimumSuccessful` keeps the random sweep substantial.
 */
object CryptoOpsTotalitySuite extends SimpleIOSuite with Checkers {

  private val R: BigInt = BigInt(Bn254.R)
  private val g1: Bn254.G1 = Bn254.G1(BigInteger.ONE, BigInteger.valueOf(2))

  // Run a healthy number of random cases per property.
  override def checkConfig: CheckConfig = super.checkConfig.withMinimumSuccessful(300)

  // ===========================================================================
  // The single no-throw assertion. `thunk` is the opcode call (by-name so the
  // potential throw happens INSIDE catchNonFatal, not at the call site).
  // ===========================================================================

  private def assertNoThrow(
    label: String
  )(thunk: => Either[JsonLogicException, JsonLogicValue])(implicit loc: SourceLocation): Expectations =
    Either.catchNonFatal(thunk) match {
      case Right(_) => success // returned Left or Right — TOTAL
      case Left(t)  => failure(s"$label THREW (totality violation): $t")
    }

  // No-throw probe for the two typed pure-curve helpers (return a point, not an Either).
  private def assertNoThrowAny[A](label: String)(thunk: => A)(implicit loc: SourceLocation): Expectations =
    Either.catchNonFatal(thunk) match {
      case Right(_) => success
      case Left(t)  => failure(s"$label THREW (totality violation): $t")
    }

  // Apply a no-throw probe across a whole battery of arg-lists for one opcode.
  private def batchNoThrow(label: String, op: List[JsonLogicValue] => Either[JsonLogicException, JsonLogicValue])(
    batteries: List[(String, List[JsonLogicValue])]
  )(implicit loc: SourceLocation): Expectations =
    batteries.foldLeft(success) {
      case (acc, (note, args)) => acc.and(assertNoThrow(s"$label [$note]")(op(args)))
    }

  // ===========================================================================
  // Generators: JsonLogicValue (well-formed + malformed), hex strings, sigma trees.
  // ===========================================================================

  // Hex-ish strings spanning every malformation class the parsers must survive.
  private val hexStringGen: Gen[String] = Gen.oneOf(
    Gen.const("0x"), // empty body
    Gen.const("0x00"),
    Gen.const("0xabc"), // odd-length nibble
    Gen.const("0xZZ"), // non-hex chars
    Gen.const("not-hex-at-all"), // missing 0x prefix
    Gen.const("0X00"), // uppercase prefix
    Gen.const("0xDEADBEEF"), // uppercase digits
    Gen.const("0x" + "00" * 32), // 32-byte (Fr / scalar / challenge widths)
    Gen.const("0x" + "ff" * 32), // 32-byte all-ones (>= R as a scalar)
    Gen.const("0x" + "00" * 31), // 31-byte (the sigma challenge width)
    Gen.const("0x" + "00" * 64), // 64-byte (G1 / identity)
    Gen.const("0x" + "00" * 128), // 128-byte (G2 / identity)
    Gen.const("0x" + "11" * 64), // 64-byte off-curve-ish pattern
    Gen.const(HexBytes.encodeG1(BigInt(1), BigInt(1)).getOrElse("0x")), // genuine off-curve point (1,1)
    Gen.const(HexBytes.encodeBytes(Array.fill[Byte](96)(0x01))), // 96-byte (schnorr proof width)
    Gen.const(HexBytes.encodeBytes(Array.fill[Byte](160)(0x02))), // 160-byte (dhtuple proof width)
    Gen.choose(0, 200).map(n => "0x" + "ab" * n), // arbitrary even width
    Gen.choose(0, 401).map(n => "0x" + ("a" * n)) // arbitrary (often odd) nibble count
  )

  // A bounded random JsonLogicValue: primitives, nested arrays/maps, hex strings, sigma-ish keys.
  private def valueGen(depth: Int): Gen[JsonLogicValue] =
    if (depth <= 0)
      Gen.oneOf(
        Gen.const(NullValue: JsonLogicValue),
        Arbitrary.arbitrary[Boolean].map(b => BoolValue(b): JsonLogicValue),
        Gen.choose(-1000000L, 1000000L).map(i => IntValue(BigInt(i)): JsonLogicValue),
        hexStringGen.map(s => StrValue(s): JsonLogicValue),
        Gen.alphaNumStr.map(s => StrValue(s): JsonLogicValue)
      )
    else
      Gen.frequency(
        5 -> valueGen(0),
        2 -> Gen.choose(0, 6).flatMap(n => Gen.listOfN(n, valueGen(depth - 1))).map(l => ArrayValue(l): JsonLogicValue),
        2 -> Gen
          .choose(0, 5)
          .flatMap(n =>
            Gen.listOfN(n, Gen.zip(Gen.oneOf("type", "k", "children", "pk", "g", "h", "u", "v", "e", "z", "x"), valueGen(depth - 1)))
          )
          .map(kvs => MapValue(kvs.toMap): JsonLogicValue)
      )

  private val argsGen: Gen[List[JsonLogicValue]] =
    Gen.choose(0, 7).flatMap(n => Gen.listOfN(n, valueGen(3)))

  // A random RAW sigma-ish node tree (often malformed): random type tags, random/absent children,
  // random k, hex e/z/points. Exercises parsePropNode / parseProofNode / boundProofShape / verifyTree
  // against deeply-nested + structurally-broken trees without ever throwing.
  private def sigmaNodeGen(depth: Int): Gen[JsonLogicValue] = {
    val typeGen = Gen.oneOf("dlog", "dhtuple", "and", "or", "threshold", "xor", "", "DLOG")
    val leafFields = for {
      t  <- typeGen
      pk <- hexStringGen
      e  <- hexStringGen
      z  <- hexStringGen
    } yield
      MapValue(
        Map(
          "type" -> StrValue(t),
          "pk"   -> StrValue(pk),
          "g"    -> StrValue(pk),
          "h"    -> StrValue(pk),
          "u"    -> StrValue(pk),
          "v"    -> StrValue(pk),
          "e"    -> StrValue(e),
          "z"    -> StrValue(z)
        )
      ): JsonLogicValue
    if (depth <= 0) leafFields
    else
      Gen.frequency(
        3 -> leafFields,
        2 -> (for {
          t  <- typeGen
          k  <- Gen.choose(-2, 8)
          e  <- hexStringGen
          n  <- Gen.choose(0, 5)
          cs <- Gen.listOfN(n, sigmaNodeGen(depth - 1))
        } yield
          MapValue(
            Map(
              "type"     -> StrValue(t),
              "k"        -> IntValue(BigInt(k)),
              "e"        -> StrValue(e),
              "children" -> ArrayValue(cs)
            )
          ): JsonLogicValue)
      )
  }

  // ---------------------------------------------------------------------------
  // Per-opcode RANDOM sweeps (the wide net) — every op over a fully random arg list.
  // ---------------------------------------------------------------------------

  private val allOps: List[(String, List[JsonLogicValue] => Either[JsonLogicException, JsonLogicValue])] = List(
    "poseidon"             -> CryptoOps.poseidon,
    "pmt_verify"           -> CryptoOps.pmtVerify,
    "groth16_verify"       -> CryptoOps.groth16Verify,
    "ecvrf_verify"         -> CryptoOps.ecVrfVerify,
    "bn254_add"            -> CryptoOps.bn254Add,
    "bn254_mul"            -> CryptoOps.bn254Mul,
    "bn254_pairing"        -> CryptoOps.bn254Pairing,
    "bls_verify"           -> CryptoOps.blsVerify,
    "bls_aggregate_verify" -> CryptoOps.blsAggregateVerify,
    "schnorr_verify"       -> CryptoOps.schnorrVerify,
    "prove_dlog_verify"    -> CryptoOps.proveDlogVerify,
    "prove_dhtuple_verify" -> CryptoOps.proveDhTupleVerify,
    "sigma_verify"         -> CryptoOps.sigmaVerify
  )

  test("every CryptoOps opcode is total over fully-random arg lists (never throws)") {
    forall(argsGen) { args =>
      allOps.foldLeft(success) {
        case (acc, (label, op)) => acc.and(assertNoThrow(label)(op(args)))
      }
    }
  }

  test("sigma_verify is total over random [prop, proof, msg] sigma trees (never throws)") {
    forall(Gen.zip(sigmaNodeGen(4), sigmaNodeGen(4), hexStringGen)) {
      case (prop, proof, msg) =>
        assertNoThrow("sigma_verify(tree)")(CryptoOps.sigmaVerify(List(prop, proof, StrValue(msg))))
    }
  }

  test("verify opcodes are total over random hex triples (arity-3 shapes, never throw)") {
    forall(Gen.zip(hexStringGen, hexStringGen, hexStringGen)) {
      case (a, b, c) =>
        val triple = List[JsonLogicValue](StrValue(a), StrValue(b), StrValue(c))
        success
          .and(assertNoThrow("groth16_verify")(CryptoOps.groth16Verify(triple)))
          .and(assertNoThrow("ecvrf_verify")(CryptoOps.ecVrfVerify(triple)))
          .and(assertNoThrow("bls_verify")(CryptoOps.blsVerify(triple)))
          .and(assertNoThrow("schnorr_verify")(CryptoOps.schnorrVerify(triple)))
          .and(assertNoThrow("prove_dlog_verify")(CryptoOps.proveDlogVerify(triple)))
    }
  }

  test("bn254_add / bn254_mul are total over random hex pairs (never throw)") {
    forall(Gen.zip(hexStringGen, hexStringGen)) {
      case (a, b) =>
        val pair = List[JsonLogicValue](StrValue(a), StrValue(b))
        success
          .and(assertNoThrow("bn254_add")(CryptoOps.bn254Add(pair)))
          .and(assertNoThrow("bn254_mul")(CryptoOps.bn254Mul(pair)))
    }
  }

  // The two public pure-curve commitment-recovery helpers (typed args; no hex/shape handling, but
  // adversarial scalars/points must not throw — e.g. identity points, z >= R, e >= R, negative).
  test("dlog/dhtuple commitment-recovery helpers are total over adversarial scalars/points") {
    // `forall` needs a `Show` for the generated type; `Bn254.G1` has none, so we generate
    // Show-able SELECTORS (point-kind index 0..3, two scalar-kind indices) and build the points /
    // scalars inside the body. Point kinds: generator, identity (point-at-infinity), off-curve, a
    // small multiple. Scalar kinds: 0, ==R, >R, >>R, negative.
    def point(kind: Int): Bn254.G1 = (kind % 4) match {
      case 0 => g1
      case 1 => Bn254.G1(BigInteger.ZERO, BigInteger.ZERO) // identity / point-at-infinity
      case 2 => Bn254.G1(BigInteger.ONE, BigInteger.ONE) // off-curve
      case _ => g1.multiply(BigInteger.valueOf(7L)) // a small multiple
    }
    def scalar(kind: Int): BigInt = (kind % 5) match {
      case 0 => BigInt(0)
      case 1 => R // == R
      case 2 => R + 1 // > R
      case 3 => R * 3 + 7 // well over R
      case _ => BigInt(-5) // negative
    }
    forall(Gen.zip(Gen.choose(0, 3), Gen.choose(0, 3), Gen.choose(0, 4), Gen.choose(0, 4))) {
      case (pk, ik, ek, zk) =>
        val (p, img, e, z) = (point(pk), point(ik), scalar(ek), scalar(zk))
        assertNoThrowAny("dlogComputeCommitment")(CryptoOps.dlogComputeCommitment(p, e, z))
          .and(assertNoThrowAny("dhtupleComputeCommitment")(CryptoOps.dhtupleComputeCommitment(p, img, e, z)))
    }
  }

  // ---------------------------------------------------------------------------
  // STRUCTURED-ADVERSARIAL battery (fixed, deterministic) — the specific hostile
  // shapes called out in the totality brief, pinned per opcode.
  // ---------------------------------------------------------------------------

  private val offCurveHex: String = HexBytes.encodeG1(BigInt(1), BigInt(1)).getOrElse("0x")
  private val identityG1: String = "0x" + "00" * 64
  private val identityG2: String = "0x" + "00" * 128
  private val validG1: String = HexBytes.encodeG1(BigInt(g1.x), BigInt(g1.y)).getOrElse("0x")
  private val nonCanonical32: String = HexBytes.encodeUInt(R + 1, 32).getOrElse("0x") // >= R, still 32 bytes
  private val challenge31: String = "0x" + "01" * 31
  private val challenge32: String = "0x" + "01" * 32 // wrong width vs the 31-byte challenge domain
  private val hugeArray: JsonLogicValue = ArrayValue(List.fill(5000)(StrValue("0x00")))
  private val deepNest: JsonLogicValue = (0 until 200).foldLeft(StrValue("0x00"): JsonLogicValue)((acc, _) => ArrayValue(List(acc)))

  private def s(str: String): JsonLogicValue = StrValue(str)
  private def i(n: Int): JsonLogicValue = IntValue(BigInt(n))

  pureTest("poseidon: structured-adversarial inputs never throw") {
    batchNoThrow("poseidon", CryptoOps.poseidon)(
      List(
        "empty"            -> Nil,
        "non-string"       -> List(i(1), BoolValue(true)),
        "odd-hex"          -> List(s("0xabc")),
        "non-hex"          -> List(s("0xZZ")),
        "wrong-width"      -> List(s("0x00")),
        "huge-array"       -> List(hugeArray),
        "nested-array-arg" -> List(deepNest),
        "over-cap"         -> List(ArrayValue(List.fill(64)(s("0x" + "00" * 32)))),
        "map-arg"          -> List(MapValue(Map("a" -> s("0x00"))))
      )
    )
  }

  pureTest("pmt_verify: structured-adversarial inputs never throw") {
    batchNoThrow("pmt_verify", CryptoOps.pmtVerify)(
      List(
        "empty"          -> Nil,
        "wrong-arity"    -> List(s(validG1)),
        "bad-index-type" -> List(s("0x" + "00" * 32), s("0x" + "00" * 32), s("not-an-int"), ArrayValue(Nil)),
        "negative-index" -> List(s("0x" + "00" * 32), s("0x" + "00" * 32), i(-1), ArrayValue(Nil)),
        "huge-siblings"  -> List(s("0x" + "00" * 32), s("0x" + "00" * 32), i(0), hugeArray),
        "bad-hex-root"   -> List(s("0xZZ"), s("0x" + "00" * 32), i(0), ArrayValue(Nil)),
        "non-array-sib"  -> List(s("0x" + "00" * 32), s("0x" + "00" * 32), i(0), s("0x00"))
      )
    )
  }

  pureTest("groth16_verify: structured-adversarial inputs never throw") {
    batchNoThrow("groth16_verify", CryptoOps.groth16Verify)(
      List(
        "empty"       -> Nil,
        "wrong-arity" -> List(s("0x00"), s("0x00")),
        "non-string"  -> List(i(1), i(2), i(3)),
        "odd-hex"     -> List(s("0xabc"), s("0x00"), s("0x00")),
        "bad-vkey-w"  -> List(s("0x00"), s("0x00"), s("0x00")),
        "raw-bytes"   -> List(s("0x" + "de" * 32), s("0x" + "ad" * 16), s("0x" + "be" * 64))
      )
    )
  }

  pureTest("ecvrf_verify: structured-adversarial inputs never throw") {
    batchNoThrow("ecvrf_verify", CryptoOps.ecVrfVerify)(
      List(
        "empty"       -> Nil,
        "wrong-arity" -> List(s("0x00")),
        "non-string"  -> List(BoolValue(true), i(2), NullValue),
        "wrong-width" -> List(s("0x00"), s("0x00"), s("0x00")),
        "odd-hex"     -> List(s("0xabc"), s("0xabc"), s("0xabc")),
        "raw-bytes"   -> List(s("0x" + "11" * 32), s("0x" + "22" * 10), s("0x" + "33" * 80))
      )
    )
  }

  pureTest("bn254_add / bn254_mul: structured-adversarial inputs never throw") {
    batchNoThrow("bn254_add", CryptoOps.bn254Add)(
      List(
        "empty"      -> Nil,
        "arity"      -> List(s(validG1)),
        "off-curve"  -> List(s(offCurveHex), s(validG1)),
        "identity"   -> List(s(identityG1), s(identityG1)),
        "non-string" -> List(i(1), i(2)),
        "wrong-w"    -> List(s("0x00"), s("0x00")),
        "odd-hex"    -> List(s("0xabc"), s("0xabc"))
      )
    ).and(
      batchNoThrow("bn254_mul", CryptoOps.bn254Mul)(
        List(
          "empty"          -> Nil,
          "off-curve"      -> List(s(offCurveHex), s("0x" + "00" * 32)),
          "identity-point" -> List(s(identityG1), s(nonCanonical32)),
          "huge-scalar"    -> List(s(validG1), s("0x" + "ff" * 32)),
          "wrong-w-scalar" -> List(s(validG1), s("0x00")),
          "non-string"     -> List(BoolValue(false), i(0))
        )
      )
    )
  }

  pureTest("bn254_pairing: structured-adversarial inputs never throw") {
    batchNoThrow("bn254_pairing", CryptoOps.bn254Pairing)(
      List(
        "empty-args"     -> Nil,
        "empty-pairs"    -> List(ArrayValue(Nil)),
        "single-pair"    -> List(ArrayValue(List(s(validG1), s(identityG2)))),
        "off-curve-g1"   -> List(ArrayValue(List(ArrayValue(List(s(offCurveHex), s(identityG2)))))),
        "ragged-pair"    -> List(ArrayValue(List(ArrayValue(List(s(validG1)))))),
        "non-array-pair" -> List(ArrayValue(List(s("0x00")))),
        "huge"           -> List(ArrayValue(List.fill(2000)(ArrayValue(List(s(validG1), s(identityG2)))))),
        "wrong-width-g2" -> List(ArrayValue(List(ArrayValue(List(s(validG1), s("0x00"))))))
      )
    )
  }

  pureTest("bls_verify / bls_aggregate_verify: structured-adversarial inputs never throw") {
    batchNoThrow("bls_verify", CryptoOps.blsVerify)(
      List(
        "empty"       -> Nil,
        "arity"       -> List(s("0x00"), s("0x00")),
        "non-string"  -> List(i(1), i(2), i(3)),
        "wrong-width" -> List(s("0x00"), s("0x00"), s("0x00")),
        "odd-hex"     -> List(s("0xabc"), s("0xabc"), s("0xabc")),
        "raw-bytes"   -> List(s("0x" + "11" * 48), s("0x" + "22" * 7), s("0x" + "33" * 96))
      )
    ).and(
      batchNoThrow("bls_aggregate_verify", CryptoOps.blsAggregateVerify)(
        List(
          "empty"        -> Nil,
          "empty-pks"    -> List(ArrayValue(Nil), s("0x00"), s("0x00")),
          "non-array"    -> List(s("0x00"), s("0x00"), s("0x00")),
          "huge-pks"     -> List(hugeArray, s("0x00"), s("0x" + "33" * 96)),
          "bad-pk-width" -> List(ArrayValue(List(s("0x00"))), s("0x00"), s("0x" + "33" * 96)),
          "non-string"   -> List(ArrayValue(List(i(1))), i(2), i(3))
        )
      )
    )
  }

  pureTest("schnorr_verify / prove_dlog_verify: structured-adversarial inputs never throw") {
    val batteries = List(
      "empty"         -> Nil,
      "arity"         -> List(s(validG1), s("0x00")),
      "non-string"    -> List(i(1), i(2), i(3)),
      "off-curve-pk"  -> List(s(offCurveHex), s("0x00"), s("0x" + "00" * 96)),
      "identity-pk"   -> List(s(identityG1), s("0x00"), s("0x" + "00" * 96)),
      "wrong-w-proof" -> List(s(validG1), s("0x00"), s("0xdead")),
      "non-canonical" -> List(s(validG1), s("0x00"), s("0x" + "00" * 64 + nonCanonical32.stripPrefix("0x"))),
      "odd-hex-msg"   -> List(s(validG1), s("0xabc"), s("0x" + "00" * 96)),
      "raw-bytes"     -> List(s(validG1), s("0x" + "ab" * 20), s(HexBytes.encodeBytes(Array.fill[Byte](96)(0x07))))
    )
    batchNoThrow("schnorr_verify", CryptoOps.schnorrVerify)(batteries)
      .and(batchNoThrow("prove_dlog_verify", CryptoOps.proveDlogVerify)(batteries))
  }

  pureTest("prove_dhtuple_verify: structured-adversarial inputs never throw") {
    batchNoThrow("prove_dhtuple_verify", CryptoOps.proveDhTupleVerify)(
      List(
        "empty"         -> Nil,
        "arity"         -> List(s(validG1), s(validG1), s(validG1)),
        "non-string"    -> List(i(1), i(2), i(3), i(4), i(5), i(6)),
        "off-curve"     -> List(s(offCurveHex), s(validG1), s(validG1), s(validG1), s("0x00"), s("0x" + "00" * 160)),
        "identity-base" -> List(s(identityG1), s(validG1), s(validG1), s(validG1), s("0x00"), s("0x" + "00" * 160)),
        "wrong-w-proof" -> List(s(validG1), s(validG1), s(validG1), s(validG1), s("0x00"), s("0xdead")),
        "non-canonical" -> List(
          s(validG1),
          s(validG1),
          s(validG1),
          s(validG1),
          s("0x00"),
          s("0x" + "00" * 128 + nonCanonical32.stripPrefix("0x"))
        ),
        "odd-hex" -> List(s("0xabc"), s("0xabc"), s("0xabc"), s("0xabc"), s("0xabc"), s("0xabc"))
      )
    )
  }

  pureTest("sigma_verify: structured-adversarial trees never throw") {
    def node(json: (String, JsonLogicValue)*): JsonLogicValue = MapValue(json.toMap)
    val dlogProp = node("type" -> s("dlog"), "pk" -> s(validG1))
    val dlogProof = node("type" -> s("dlog"), "e" -> s(challenge31), "z" -> s("0x" + "00" * 32))

    batchNoThrow("sigma_verify", CryptoOps.sigmaVerify)(
      List(
        "empty"             -> Nil,
        "arity"             -> List(dlogProp, dlogProof),
        "non-map-prop"      -> List(s("0x00"), dlogProof, s("0x00")),
        "unknown-type"      -> List(node("type" -> s("xor"), "pk" -> s(validG1)), dlogProof, s("0x00")),
        "missing-fields"    -> List(node("type" -> s("dlog")), node("type" -> s("dlog"), "e" -> s(challenge31)), s("0x00")),
        "off-curve-pk"      -> List(node("type" -> s("dlog"), "pk" -> s(offCurveHex)), dlogProof, s("0x00")),
        "identity-pk"       -> List(node("type" -> s("dlog"), "pk" -> s(identityG1)), dlogProof, s("0x00")),
        "32-byte-challenge" -> List(dlogProp, node("type" -> s("dlog"), "e" -> s(challenge32), "z" -> s("0x" + "00" * 32)), s("0x00")),
        "non-canonical-z"   -> List(dlogProp, node("type" -> s("dlog"), "e" -> s(challenge31), "z" -> s(nonCanonical32)), s("0x00")),
        "shape-mismatch"    -> List(dlogProp, node("type" -> s("dhtuple"), "e" -> s(challenge31), "z" -> s("0x" + "00" * 32)), s("0x00")),
        "child-mismatch" -> List(
          node("type" -> s("and"), "children" -> ArrayValue(List(dlogProp, dlogProp))),
          node("type" -> s("and"), "e"        -> s(challenge31), "children" -> ArrayValue(List(dlogProof))),
          s("0x00")
        ),
        "threshold-k>n" -> List(
          node("type" -> s("threshold"), "k" -> i(5), "children" -> ArrayValue(List(dlogProp, dlogProp))),
          node("type" -> s("threshold"), "k" -> i(5), "e"        -> s(challenge31), "children" -> ArrayValue(List(dlogProof, dlogProof))),
          s("0x00")
        ),
        "threshold-k<=0" -> List(
          node("type" -> s("threshold"), "k" -> i(0), "children" -> ArrayValue(List(dlogProp))),
          node("type" -> s("threshold"), "k" -> i(0), "e"        -> s(challenge31), "children" -> ArrayValue(List(dlogProof))),
          s("0x00")
        ),
        "huge-mismatched-proof" -> List(
          dlogProp,
          node("type" -> s("or"), "e" -> s(challenge31), "children" -> ArrayValue(List.fill(5000)(dlogProof))),
          s("0x00")
        ),
        "deeply-nested" -> List(
          (0 until 80).foldLeft(dlogProp)((acc, _) => node("type" -> s("and"), "children" -> ArrayValue(List(acc)))),
          (0 until 80).foldLeft(dlogProof)((acc, _) =>
            node("type" -> s("and"), "e" -> s(challenge31), "children" -> ArrayValue(List(acc)))
          ),
          s("0x00")
        ),
        "bad-msg-hex" -> List(dlogProp, dlogProof, s("0xZZ"))
      )
    )
  }
}
