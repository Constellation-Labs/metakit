package json_logic

import java.math.BigInteger
import java.security.{MessageDigest, SecureRandom}

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.zk.Bn254
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasConfig, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.ops.{CryptoOps, HexBytes}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

/**
 * Adversarial end-to-end tests for `sigma_verify` — the recursive CDS Σ-protocol proposition
 * verifier (ring + threshold signatures) on BN254 G1 (docs/sigma-verify.md, Phase 2).
 *
 * The suite carries a full OFF-CHAIN CDS PROVER (the dual of the verifier, where most of the
 * subtlety lives): for a proposition whose witnesses the prover knows, it builds real transcripts
 * for known leaves and HVZK-SIMULATES the unknown branches, splitting the FS root challenge by
 * the tree structure (XOR for OR, GF(2^8) Shamir for THRESHOLD). The prover and verifier MUST
 * agree byte-for-byte on the strong-FS canonical serialization and the CDS split — if they
 * disagree, the bug is in one of those two surfaces (never weaken a soundness test to hide it).
 *
 * Round-trip TRUE cases prove completeness; the SOUNDNESS NEGATIVES (forge-by-simulating-all,
 * known-too-few-threshold-witnesses, wrong message, tampered response/commitment, off-by-one
 * threshold degree, duplicate/out-of-range share index) prove the dangerous-bug surface is closed.
 */
object SigmaVerifySuite extends SimpleIOSuite {

  // ===========================================================================
  // Shared crypto scaffolding (mirrors CryptoOps.Sigma + the leaf opcodes).
  // ===========================================================================

  private val R: BigInt = BigInt(Bn254.R)
  private val g1: Bn254.G1 = Bn254.G1(BigInteger.ONE, BigInteger.valueOf(2)) // generator (1,2)

  // Deterministic RNG so the suite is reproducible (the PROVER is allowed randomness; the
  // verifier is not). A fixed seed keeps failures repeatable.
  private val rng: SecureRandom = {
    val r = SecureRandom.getInstance("SHA1PRNG")
    r.setSeed(0x5191a0_deadbeefL)
    r
  }
  private def randScalar(): BigInt = BigInt(1, { val b = new Array[Byte](32); rng.nextBytes(b); b }).mod(R)

  private def encG1(p: Bn254.G1): String = HexBytes.encodeG1(BigInt(p.x), BigInt(p.y)).fold(throw _, identity)
  private def g1Bytes(p: Bn254.G1): Array[Byte] = HexBytes.parseBytes(encG1(p), Some(64), "g1").fold(throw _, identity)
  // Response `z` is a curve scalar -> emit reduced mod R (32B).
  private def hex32(v: BigInt): String = HexBytes.encodeUInt(v.mod(R), 32).fold(throw _, identity)

  // CHALLENGE DOMAIN (audit finding #1): challenges are 31-byte (248-bit) values. `2^248 < R`, so
  // a challenge is ALWAYS a canonical Fr element — the byte↔scalar map is a bijection. The SAME
  // 31 bytes are the GF(2^8)/XOR object AND, taken directly (no mod R), the Fr scalar. The width
  // MUST match CryptoOps.Sigma.ChallengeBytes exactly.
  private val ChallengeBytes: Int = 31
  // A random 31-byte challenge (a free/simulated CDS challenge). Drawn as 31 raw bytes so it is
  // always < 2^248 (the injective domain) — NOT `mod R`, which could exceed 2^248.
  private def randChallenge(): BigInt = BigInt(1, { val b = new Array[Byte](ChallengeBytes); rng.nextBytes(b); b })
  // Emit a 31-byte challenge as fixed-width hex (the proof's `e` field). The verifier parses
  // exactly 31 bytes; values are < 2^248 by construction so they fit.
  private def hexChallenge(v: BigInt): String = HexBytes.encodeUInt(v, ChallengeBytes).fold(throw _, identity)
  private def sha256(bytes: Array[Byte]): Array[Byte] = MessageDigest.getInstance("SHA-256").digest(bytes)
  // The single SHA256->challenge rule (finding #1): low-order 31 bytes of the digest, as a BigInt.
  private def low31(bytes: Array[Byte]): BigInt = BigInt(1, sha256(bytes).takeRight(ChallengeBytes))

  // Frozen serialization constants — MUST match CryptoOps.Sigma exactly.
  private val TagDlog: Byte = 0x00
  private val TagDhTuple: Byte = 0x01
  private val TagAnd: Byte = 0x02
  private val TagOr: Byte = 0x03
  private val TagThreshold: Byte = 0x04
  private val DomainSep: Array[Byte] = "sigma_verify:v1".getBytes("US-ASCII")
  private def uint32(v: Int): Array[Byte] = Array((v >>> 24).toByte, (v >>> 16).toByte, (v >>> 8).toByte, v.toByte)
  // The RAW 31 bytes of a challenge — the object GF/XOR operate over (and, directly, the Fr scalar).
  private def challengeBytes(e: BigInt): Array[Byte] =
    HexBytes.parseBytes(hexChallenge(e), Some(ChallengeBytes), "e").fold(throw _, identity)

  // ===========================================================================
  // Prover model: a proposition the prover can build a (real|simulated) proof for.
  //
  // Each node carries enough to (a) hold the simulated/real transcript and (b) emit BOTH the
  // proposition JSON and the proof JSON. `commitment` is fixed BEFORE the root challenge (the
  // strong-FS input); responses/sub-challenges are filled AFTER the root challenge is hashed.
  // ===========================================================================

  // A statement leaf with the prover's chosen witness (or None when the prover does NOT know it,
  // forcing a simulated branch — used to drive the soundness negatives).
  sealed trait Prop
  final case class Dlog(x: Option[BigInt], pk: Bn254.G1) extends Prop
  final case class DhTuple(w: Option[BigInt], g: Bn254.G1, h: Bn254.G1, u: Bn254.G1, v: Bn254.G1) extends Prop
  final case class And(children: List[Prop]) extends Prop
  final case class Or(children: List[Prop]) extends Prop
  final case class Threshold(k: Int, children: List[Prop]) extends Prop

  // Convenience constructors.
  private def dlogKnown(x: BigInt): Dlog = Dlog(Some(x.mod(R)), g1.multiply(x.bigInteger))
  private def dlogUnknown(): Dlog = { val x = randScalar(); Dlog(None, g1.multiply(x.bigInteger)) }
  private def dhKnown(w: BigInt, gScalar: BigInt, hScalar: BigInt): DhTuple = {
    val g = g1.multiply(gScalar.bigInteger); val h = g1.multiply(hScalar.bigInteger)
    DhTuple(Some(w.mod(R)), g, h, g.multiply(w.bigInteger), h.multiply(w.bigInteger))
  }

  // Whether the prover can produce a REAL proof for a subtree (it knows enough witnesses).
  private def satisfiable(p: Prop): Boolean = p match {
    case Dlog(x, _)             => x.isDefined
    case DhTuple(w, _, _, _, _) => w.isDefined
    case And(cs)                => cs.forall(satisfiable)
    case Or(cs)                 => cs.exists(satisfiable)
    case Threshold(k, cs)       => cs.count(satisfiable) >= k
  }

  // ----- Proposition JSON (what goes on chain) -----
  private def propJson(p: Prop): String = p match {
    case Dlog(_, pk)            => s"""{"type":"dlog","pk":"${encG1(pk)}"}"""
    case DhTuple(_, g, h, u, v) => s"""{"type":"dhtuple","g":"${encG1(g)}","h":"${encG1(h)}","u":"${encG1(u)}","v":"${encG1(v)}"}"""
    case And(cs)                => s"""{"type":"and","children":[${cs.map(propJson).mkString(",")}]}"""
    case Or(cs)                 => s"""{"type":"or","children":[${cs.map(propJson).mkString(",")}]}"""
    case Threshold(k, cs)       => s"""{"type":"threshold","k":$k,"children":[${cs.map(propJson).mkString(",")}]}"""
  }

  // ----- Stage 1: pick commitments (real nonce / simulated (e,z)) BEFORE the root hash. -----
  //
  // A "PreProof" mirrors the proposition and holds the fixed commitment(s) (the strong-FS input),
  // the witness (for real leaves), and a `sat` flag (whether this subtree is satisfiable). `e` and
  // `z` start at -1 and are filled in stage 3 after challenge propagation. The whole prover is
  // self-contained in this tree (no parallel walk over Prop), so `setChallenge` dispatches on the
  // PreProof alone — no large tuple-cross-product match.
  //
  // For a SIMULATED leaf the (e, z) are picked NOW (random) and `a = computeCommitment(stmt,e,z)`
  // (the HVZK simulator); they are kept fixed thereafter. For a REAL leaf only the nonce `r` is
  // chosen now (a = r·G), and z is computed once the propagated challenge is known.
  sealed trait PreProof {
    var e: BigInt = BigInt(-1)
    def sat: Boolean
    def serializeWithCommitments: Array[Byte]
  }
  final case class PreDlog(pk: Bn254.G1, sat: Boolean, witness: Option[BigInt], r: BigInt, a: Bn254.G1) extends PreProof {
    var z: BigInt = BigInt(-1)
    def serializeWithCommitments: Array[Byte] = Array(TagDlog) ++ g1Bytes(pk) ++ g1Bytes(a)
  }
  final case class PreDh(
    g: Bn254.G1,
    h: Bn254.G1,
    u: Bn254.G1,
    v: Bn254.G1,
    sat: Boolean,
    witness: Option[BigInt],
    r: BigInt,
    a1: Bn254.G1,
    a2: Bn254.G1
  ) extends PreProof {
    var z: BigInt = BigInt(-1)
    def serializeWithCommitments: Array[Byte] =
      Array(TagDhTuple) ++ g1Bytes(g) ++ g1Bytes(h) ++ g1Bytes(u) ++ g1Bytes(v) ++ g1Bytes(a1) ++ g1Bytes(a2)
  }
  final case class PreAnd(children: List[PreProof], sat: Boolean) extends PreProof {
    def serializeWithCommitments: Array[Byte] =
      Array(TagAnd) ++ uint32(children.length) ++ children.flatMap(_.serializeWithCommitments).toArray
  }
  final case class PreOr(children: List[PreProof], sat: Boolean) extends PreProof {
    def serializeWithCommitments: Array[Byte] =
      Array(TagOr) ++ uint32(children.length) ++ children.flatMap(_.serializeWithCommitments).toArray
  }
  final case class PreThr(k: Int, children: List[PreProof], sat: Boolean) extends PreProof {
    def serializeWithCommitments: Array[Byte] =
      Array(TagThreshold) ++ uint32(k) ++ uint32(children.length) ++ children.flatMap(_.serializeWithCommitments).toArray
  }

  // Commit phase. A subtree is committed REAL iff it is satisfiable; otherwise SIMULATED.
  // Commit a subtree. `mustSimulate` = the parent decided this WHOLE subtree is a simulated branch
  // (its node challenge will be a free point in the parent's split). When false, the subtree is on
  // the real path: it is committed REAL (deferred challenge) and chooses EXACTLY the minimal set of
  // real children — 1 for OR, k for THRESHOLD — force-simulating the rest (incl. any SURPLUS
  // satisfiable children beyond the k needed). This keeps the split's free/derived slot counts
  // exact: a real OR has 1 derived + (n-1) free children; a real THRESHOLD has k derived + (n-k)
  // free children.
  private def commit(p: Prop, mustSimulate: Boolean = false): PreProof =
    if (mustSimulate || !satisfiable(p)) simulateForced(p, randChallenge()) // simulated branch, free node e (31B)
    else
      p match {
        case Dlog(xOpt, pk) => val r = randScalar(); PreDlog(pk, sat = true, xOpt, r, g1.multiply(r.bigInteger))
        case DhTuple(wOpt, g, h, u, v) =>
          val r = randScalar(); PreDh(g, h, u, v, sat = true, wOpt, r, g.multiply(r.bigInteger), h.multiply(r.bigInteger))
        case And(cs) =>
          // AND: every child must be proven for real (no hiding); recurse real into all.
          PreAnd(cs.map(c => commit(c, mustSimulate = false)), sat = true)
        case Or(cs) =>
          // OR: exactly ONE real child (first satisfiable); the rest are simulated branches.
          val realIdx = cs.indexWhere(satisfiable)
          PreOr(cs.zipWithIndex.map { case (c, i) => commit(c, mustSimulate = i != realIdx) }, sat = true)
        case Threshold(k, cs) =>
          // THRESHOLD: exactly k real children (first k satisfiable); the rest are simulated.
          val realIdxs = cs.zipWithIndex.collect { case (c, i) if satisfiable(c) => i }.take(k).toSet
          PreThr(k, cs.zipWithIndex.map { case (c, i) => commit(c, mustSimulate = !realIdxs.contains(i)) }, sat = true)
      }

  /**
   * HVZK-simulate a WHOLE subtree to a DICTATED node challenge `e` (the parent's split chose it),
   * producing a fully consistent simulated proof with no witness used. This is the CDS simulator
   * for any unsatisfiable subtree (and for the simulated branches under OR/THRESHOLD):
   *   - leaf: pick random z, a = computeCommitment(stmt, e, z); fix (e, z).
   *   - AND: copy e to every child (each simulated forced to e).
   *   - OR: simulate n-1 children with FREE challenges, force the last to e XOR (⊕ others).
   *   - THRESHOLD(k,n): simulate n-k children FREE; interpolate the degree-(n-k) poly through
   *     (0, e) + those n-k points; force the remaining k children to P(index+1).
   * The resulting node `.e` equals the dictated `e`, so the verifier's relations hold by
   * construction and only the FS-root check can fail (which is the whole point of soundness).
   */
  private def simulateForced(p: Prop, e: BigInt): PreProof = p match {
    case Dlog(_, pk) =>
      val z = randScalar()
      val node = PreDlog(pk, sat = false, None, BigInt(-1), CryptoOps.dlogComputeCommitment(pk, e, z))
      node.e = e; node.z = z; node
    case DhTuple(_, g, h, u, v) =>
      val z = randScalar()
      val node = PreDh(
        g,
        h,
        u,
        v,
        sat = false,
        None,
        BigInt(-1),
        CryptoOps.dhtupleComputeCommitment(g, u, e, z),
        CryptoOps.dhtupleComputeCommitment(h, v, e, z)
      )
      node.e = e; node.z = z; node
    case And(cs) =>
      val node = PreAnd(cs.map(c => simulateForced(c, e)), sat = false); node.e = e; node
    case Or(cs) =>
      // Force the LAST child to absorb the XOR balance; the rest get free challenges.
      val frees = cs.dropRight(1).map(c => simulateForced(c, randChallenge()))
      val lastE = frees.foldLeft(e)((acc, c) => acc ^ c.e)
      val node = PreOr(frees :+ simulateForced(cs.last, lastE), sat = false); node.e = e; node
    case Threshold(k, cs) =>
      val n = cs.length
      val degree = n - k
      // First `degree` children free; interpolate P through (0,e)+them; force the rest to P(i+1).
      val freeIdxs = (0 until degree).toSet
      val freeChildren: Map[Int, PreProof] = freeIdxs.map(i => i -> simulateForced(cs(i), randChallenge())).toMap
      val xs = (0 :: freeIdxs.toList.sorted.map(_ + 1)).toArray
      val children = (0 until n).toList.map { i =>
        if (freeIdxs.contains(i)) freeChildren(i)
        else {
          val forced = BigInt(
            1,
            Array.tabulate(ChallengeBytes) { lane =>
              val ys = (0 :: freeIdxs.toList.sorted).zipWithIndex.map {
                case (fi, pos) =>
                  if (pos == 0) challengeBytes(e)(lane) & 0xff
                  else challengeBytes(freeChildren(fi).e)(lane) & 0xff
              }.toArray
              gfLagrange(xs, ys, i + 1).toByte
            }
          ) // RAW (NOT mod R): challenge byte-string P(i+1)
          simulateForced(cs(i), forced)
        }
      }
      val node = PreThr(k, children, sat = false); node.e = e; node
  }

  // GF(2^8) helpers for the threshold prover (must match CryptoOps' field exactly).
  private def gfMul(a0: Int, b0: Int): Int = {
    val (p, _, _) = (0 until 8).foldLeft((0, a0 & 0xff, b0 & 0xff)) {
      case ((prod, a, b), _) =>
        val np = if ((b & 1) != 0) prod ^ a else prod
        val sh = (a << 1) & 0xff
        val na = if ((a & 0x80) != 0) sh ^ 0x1b else sh
        (np, na, b >> 1)
    }
    p & 0xff
  }
  private def gfInv(a: Int): Int =
    if ((a & 0xff) == 0) 0
    else
      (0 until 8)
        .foldLeft((1, a & 0xff)) {
          case ((acc, base), bit) =>
            (if (((254 >> bit) & 1) != 0) gfMul(acc, base) else acc, gfMul(base, base))
        }
        ._1 & 0xff
  private def gfLagrange(xs: Array[Int], ys: Array[Int], xEval: Int): Int =
    xs.indices.foldLeft(0) { (acc, i) =>
      val (num, den) = xs.indices.foldLeft((1, 1)) {
        case ((nm, dn), j) =>
          if (j == i) (nm, dn) else (gfMul(nm, xEval ^ xs(j)), gfMul(dn, xs(i) ^ xs(j)))
      }
      acc ^ gfMul(ys(i), gfMul(num, gfInv(den)))
    } & 0xff

  // ----- Stage 3: propagate the (now-known) node challenge down + fill responses. -----
  //
  //   - DLog/DhTuple REAL leaf: e := propagated, z := r + e·witness.
  //     SIMULATED leaf: keeps its pre-chosen (e, z) — the parent's split MUST have handed down
  //     exactly that e (the OR/THRESHOLD math below guarantees it), so we assert equality.
  //   - AND: copy e to every child.
  //   - OR: real child = first satisfiable; its e = node_e XOR (⊕ simulated children's fixed e's).
  //   - THRESHOLD(k,n): real children = first k satisfiable; the (n-k) simulated children hold
  //     FIXED e's; with (0, node_e) those (n-k+1) points define a degree-(n-k) polynomial P; each
  //     real child's e = P(its index+1), computed byte-wise across the 31 GF(2^8) lanes.
  private def setChallenge(pp: PreProof, e: BigInt): Unit =
    if (!pp.sat) {
      // A fully-simulated subtree is already internally consistent (built by simulateForced with a
      // free node challenge). The parent's split MUST have handed down exactly that free challenge,
      // so we only ASSERT equality (no recursion needed — its responses are already fixed). With the
      // 31-byte injective challenge domain (finding #1) challenges are never mod-R-reduced, so this
      // is a plain byte-value equality.
      require(pp.e == e, s"simulated subtree challenge mismatch: ${pp.e} vs $e")
    } else
      pp match {
        case d: PreDlog => d.e = e; d.z = (d.r + e * d.witness.get).mod(R)
        case d: PreDh   => d.e = e; d.z = (d.r + e * d.witness.get).mod(R)
        case a: PreAnd =>
          a.e = e
          a.children.foreach(c => setChallenge(c, e)) // AND: copy down to every (real) child
        case o: PreOr =>
          o.e = e
          // Exactly one real child (committed sat=true); the rest are simulated with FIXED e's.
          val realIdx = o.children.indexWhere(_.sat)
          val xorSim = o.children.zipWithIndex.foldLeft(BigInt(0)) {
            case (acc, (c, i)) => if (i == realIdx) acc else acc ^ c.e
          }
          o.children.zipWithIndex.foreach {
            case (c, i) => setChallenge(c, if (i == realIdx) e ^ xorSim else c.e)
          }
        case t: PreThr =>
          t.e = e
          val n = t.children.length
          val degree = n - t.k
          // Exactly k real children (committed sat=true); the (n-k) simulated children hold fixed e's.
          val realIdxs = t.children.zipWithIndex.collect { case (c, i) if c.sat => i }.toSet
          val simIdxs = (0 until n).filterNot(realIdxs.contains).toList
          require(simIdxs.length == degree, s"threshold prover: expected $degree simulated children, got ${simIdxs.length}")
          // Defining points: x = 0 (node challenge) + (simIdx+1, sim child's fixed e). Interpolate
          // the degree-(n-k) polynomial byte-wise and assign each real child's e = P(its index+1).
          val xs = (0 :: simIdxs.map(_ + 1)).toArray
          val realE: Map[Int, BigInt] = realIdxs.map { ri =>
            val bytes = Array.tabulate(ChallengeBytes) { lane =>
              val ys = (0 :: simIdxs).zipWithIndex.map {
                case (si, pos) =>
                  if (pos == 0) challengeBytes(e)(lane) & 0xff
                  else challengeBytes(t.children(si).e)(lane) & 0xff
              }.toArray
              gfLagrange(xs, ys, ri + 1).toByte
            }
            ri -> BigInt(1, bytes) // 31-byte challenge P(ri+1) (the injective domain; no mod R)
          }.toMap
          t.children.zipWithIndex.foreach {
            case (c, i) => setChallenge(c, if (realIdxs.contains(i)) realE(i) else c.e)
          }
      }

  // ----- Proof JSON emission. `e` = 31-byte challenge; `z` = canonical (< R) scalar response. -----
  private def proofJson(pp: PreProof): String = pp match {
    case d: PreDlog => s"""{"type":"dlog","e":"${hexChallenge(d.e)}","z":"${hex32(d.z)}"}"""
    case d: PreDh   => s"""{"type":"dhtuple","e":"${hexChallenge(d.e)}","z":"${hex32(d.z)}"}"""
    case a: PreAnd  => s"""{"type":"and","e":"${hexChallenge(a.e)}","children":[${a.children.map(proofJson).mkString(",")}]}"""
    case o: PreOr   => s"""{"type":"or","e":"${hexChallenge(o.e)}","children":[${o.children.map(proofJson).mkString(",")}]}"""
    case t: PreThr =>
      s"""{"type":"threshold","e":"${hexChallenge(t.e)}","k":${t.k},"children":[${t.children.map(proofJson).mkString(",")}]}"""
  }

  /** Full prover: commit -> strong-FS root over the committed tree -> split -> responses -> JSON. */
  private def prove(prop: Prop, m: Array[Byte]): (String, String) = {
    val pp = commit(prop)
    // Root challenge = low31(SHA256(DomainSep ‖ committed-tree ‖ msg)) — the injective 31-byte
    // domain (finding #1), NOT `mod R`. The verifier recomputes the same low31 and compares bytes.
    val rootChallenge = low31(DomainSep ++ pp.serializeWithCommitments ++ m)
    setChallenge(pp, rootChallenge)
    (propJson(prop), proofJson(pp))
  }

  // ===========================================================================
  // Evaluation harness.
  // ===========================================================================

  private def sigmaExpr(propJ: String, proofJ: String, msgHex: String): String =
    s"""{"sigma_verify":[$propJ,$proofJ,"$msgHex"]}"""

  private def evalSigma(propJ: String, proofJ: String, msgHex: String): IO[Either[JsonLogicException, JsonLogicValue]] =
    for {
      expr <- IO.fromEither(parser.parse(sigmaExpr(propJ, proofJ, msgHex)).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse("{}").flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield out

  private val msg: Array[Byte] = "authorize sigma".getBytes("UTF-8")
  private val msgHex: String = HexBytes.encodeBytes(msg)

  // ===========================================================================
  // ROUND-TRIP TRUE (completeness).
  // ===========================================================================

  test("round-trip true: single dlog leaf") {
    val prop = dlogKnown(BigInt("12345678901234567890"))
    val (pj, prf) = prove(prop, msg)
    evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
  }

  test("round-trip true: single dhtuple leaf") {
    val prop = dhKnown(BigInt("999888777666555"), BigInt(3), BigInt(5))
    val (pj, prf) = prove(prop, msg)
    evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
  }

  test("round-trip true: AND(dlog, dhtuple)") {
    val prop = And(List(dlogKnown(BigInt(7)), dhKnown(BigInt(11), BigInt(2), BigInt(9))))
    val (pj, prf) = prove(prop, msg)
    evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
  }

  // OR / ring over n in {2,3,5}, each branch in turn as the real (known) one.
  private def ringWithKnownAt(n: Int, knownIdx: Int): Or =
    Or((0 until n).map(i => if (i == knownIdx) dlogKnown(randScalar()) else dlogUnknown()).toList)

  List(2, 3, 5).foreach { n =>
    (0 until n).foreach { knownIdx =>
      test(s"round-trip true: OR ring n=$n, real branch = $knownIdx (hiding)") {
        val prop = ringWithKnownAt(n, knownIdx)
        val (pj, prf) = prove(prop, msg)
        evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
      }
    }
  }

  // THRESHOLD k-of-n for several (k, n).
  private def thresholdKnown(k: Int, n: Int, knownCount: Int): Threshold =
    Threshold(k, (0 until n).map(i => if (i < knownCount) dlogKnown(randScalar()) else dlogUnknown()).toList)

  List((2, 3), (3, 5), (1, 4), (4, 4), (2, 5)).foreach {
    case (k, n) =>
      test(s"round-trip true: THRESHOLD $k-of-$n (exactly k witnesses known)") {
        val prop = thresholdKnown(k, n, knownCount = k)
        val (pj, prf) = prove(prop, msg)
        evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
      }
  }

  test("round-trip true: THRESHOLD 2-of-3 with MORE than k known (still picks exactly k real)") {
    val prop = thresholdKnown(2, 3, knownCount = 3)
    val (pj, prf) = prove(prop, msg)
    evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
  }

  // Nested trees.
  test("round-trip true: nested AND of ORs — (A or B) and (C or D)") {
    val prop = And(
      List(
        Or(List(dlogKnown(randScalar()), dlogUnknown())),
        Or(List(dlogUnknown(), dlogKnown(randScalar())))
      )
    )
    val (pj, prf) = prove(prop, msg)
    evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
  }

  test("round-trip true: nested THRESHOLD 2-of-3 of dlogs inside an AND with a dhtuple") {
    val prop = And(
      List(
        thresholdKnown(2, 3, knownCount = 2),
        dhKnown(BigInt(13), BigInt(4), BigInt(7))
      )
    )
    val (pj, prf) = prove(prop, msg)
    evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
  }

  test("round-trip true: OR of (AND, THRESHOLD) — outer ring hides which composite was used") {
    val prop = Or(
      List(
        And(List(dlogKnown(randScalar()), dlogKnown(randScalar()))), // real composite branch
        Threshold(2, List(dlogUnknown(), dlogUnknown(), dlogUnknown())) // simulated composite branch
      )
    )
    val (pj, prf) = prove(prop, msg)
    evalSigma(pj, prf, msgHex).map(r => expect(r == Right(BoolValue(true))))
  }

  // ===========================================================================
  // CHALLENGE-DOMAIN INJECTIVITY (finding #1: 31-byte challenge ↔ Fr scalar bijection).
  // ===========================================================================

  pureTest("finding #1: the 31-byte challenge domain is INJECTIVE into Fr (no e vs e+R alias)") {
    // The whole point of the 31-byte width: 2^248 < R, so EVERY 31-byte challenge is a distinct,
    // canonical Fr element and the byte->scalar map is a bijection. Concretely:
    //   (a) the max 31-byte value is < R, so challenge-as-scalar never wraps;
    //   (b) low31(SHA256(...)) is always < 2^248 (it drops the digest's top byte);
    //   (c) the previous aliasing pair (e, e+R) cannot both be 31-byte challenges, because
    //       e+R >= R > 2^248 needs at least 32 bytes -> it is rejected at the width gate.
    val twoPow248: BigInt = BigInt(1) << (8 * ChallengeBytes) // 2^248
    val maxChallenge: BigInt = twoPow248 - 1 // all-0xff 31-byte value
    // (a) injective domain sits strictly below R.
    val rangeOk = expect(twoPow248 < R, s"2^248 must be < R for injectivity; R=$R")
      .and(expect(maxChallenge < R, "the largest 31-byte challenge must be a canonical Fr element"))
    // (b) low31 of an arbitrary digest is always a 31-byte value (< 2^248).
    val sampleDigests = (0 until 64).map(i => low31(BigInt(i).toByteArray ++ DomainSep))
    val low31Ok = expect(sampleDigests.forall(c => c >= 0 && c < twoPow248), "low31 must land in [0, 2^248)")
    // (c) the classic alias e and e+R: at most ONE is representable as a 31-byte challenge.
    //     For e in [0, 2^248), e is a valid challenge but e+R needs >= 32 bytes (e+R >= R > 2^248).
    val e = maxChallenge
    val aliasBlocked = expect(
      HexBytes.encodeUInt(e + R, ChallengeBytes).isLeft,
      "e+R must NOT fit in 31 bytes — so it can never collide with the 31-byte challenge e"
    )
    rangeOk.and(low31Ok).and(aliasBlocked)
  }

  // ===========================================================================
  // DoS BOUND (finding #2: proof must mirror the gas-charged proposition; reject oversized fast).
  // ===========================================================================

  test("finding #2: a TINY proposition with a HUGE mismatched proof is rejected fast (DoS bound)") {
    // The proposition is a single dlog leaf (gas-charged for ONE node). The proof is a deeply
    // nested / wide AND tree of thousands of nodes — if the verifier parsed the whole proof before
    // shape-matching, that is unpaid work. With the structural bound, the proof's node count/depth
    // exceeds the proposition's (1 node) and is rejected as a hard error BEFORE any curve work.
    val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
    // Build a wide OR proof with 5000 children — vastly more than the proposition's single node.
    val child = s"""{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}"""
    val hugeProof = s"""{"type":"or","e":"${hexChallenge(BigInt(1))}","children":[${List.fill(5000)(child).mkString(",")}]}"""
    evalSigma(prop, hugeProof, msgHex).map(r => expect(r.isLeft))
  }

  test("finding #2: a deeply-NESTED proof beyond the proposition depth is rejected (DoS depth cap)") {
    // Proposition: a single dlog leaf (depth 1). Proof: a chain of nested AND nodes 200 deep — far
    // beyond the proposition's depth. Rejected by the depth bound before deep traversal.
    val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
    val e = hexChallenge(BigInt(1))
    val leaf = s"""{"type":"dlog","e":"$e","z":"${hex32(BigInt(1))}"}"""
    // Nest 200 ANDs, innermost wrapping the leaf.
    val nested = (0 until 200).foldLeft(leaf) { (inner, _) =>
      s"""{"type":"and","e":"$e","children":[$inner]}"""
    }
    evalSigma(prop, nested, msgHex).map(r => expect(r.isLeft))
  }

  // ===========================================================================
  // SOUNDNESS NEGATIVES (the dangerous-bug surface).
  // ===========================================================================

  test("soundness: OR where the prover knows NO witness, forging by simulating ALL branches => false") {
    // Simulate BOTH branches: each child gets free (e_i, z_i); their XOR is some random value that
    // CANNOT equal the FS root (which is fixed by the commitments the simulation produced). The
    // strong-FS binding is exactly what makes this unforgeable.
    val a = dlogUnknown(); val b = dlogUnknown()
    val prop = Or(List(a, b))
    val ca = commit(a).asInstanceOf[PreDlog] // both simulated
    val cb = commit(b).asInstanceOf[PreDlog]
    // Assemble an OR proof whose node challenge = XOR of the two simulated child challenges (so the
    // XOR relation HOLDS) — but that XOR is NOT the FS root, so step 6 must reject.
    val orE = ca.e ^ cb.e // 31-byte XOR; the verifier checks ⊕ child e == node e over bytes
    val proofJ =
      s"""{"type":"or","e":"${hexChallenge(orE)}","children":[${proofJson(ca)},${proofJson(cb)}]}"""
    evalSigma(propJson(prop), proofJ, msgHex).map(r => expect(r == Right(BoolValue(false))))
  }

  test("soundness: OR forgery that instead matches the FS root but BREAKS the XOR relation => false") {
    // Dual attack: set the OR node challenge = the real FS root (so step 6's root check could pass),
    // but then the two free simulated child challenges do NOT XOR to it -> the XOR relation fails.
    val a = dlogUnknown(); val b = dlogUnknown()
    val prop = Or(List(a, b))
    val ca = commit(a).asInstanceOf[PreDlog]
    val cb = commit(b).asInstanceOf[PreDlog]
    // Build the would-be root over the committed tree, then claim it as the OR challenge while
    // leaving the children's (random, fixed) challenges untouched (they will not XOR to it). The
    // root is the 31-byte low31 the verifier recomputes (finding #1), emitted as a 31-byte challenge.
    val orNode = PreOr(List(ca, cb), sat = false)
    val root = low31(DomainSep ++ orNode.serializeWithCommitments ++ msg)
    val proofJ = s"""{"type":"or","e":"${hexChallenge(root)}","children":[${proofJson(ca)},${proofJson(cb)}]}"""
    evalSigma(propJson(prop), proofJ, msgHex).map(r => expect(r == Right(BoolValue(false))))
  }

  test("soundness: THRESHOLD 2-of-3 with only k-1 = 1 real witness (rest HVZK-simulated) => false") {
    // FINDING #5 rebuild. The earlier version built the proof's children from FRESH unrelated
    // `dlogUnknown()` statements, so a rejection could have been a statement/commitment MISMATCH
    // rather than threshold-interpolation unsoundness. Here every child is proven AGAINST THE
    // PROPOSITION'S ACTUAL public key: the k-1 known children carry REAL transcripts (real nonce
    // r, real response z), and the remaining n-(k-1) children are HVZK-SIMULATED on the SAME pks
    // (a = computeCommitment(pk, e, z) for free (e, z)). So the statements/commitments are all
    // internally consistent and the ONLY thing missing is the kth witness — isolating the
    // discriminator to the CDS interpolation (degree / P(0)) soundness, exactly as intended.
    val k = 2 // n = 3 children below (2-of-3)
    val xKnown = randScalar()
    val knownLeaf = dlogKnown(xKnown) // index 0: the one real witness
    val sim1 = dlogUnknown(); val sim2 = dlogUnknown() // indices 1,2: pks whose witness is unknown
    val prop = Threshold(k, List(knownLeaf, sim1, sim2))

    // Child 0: REAL — pick a nonce, commit a = r·G, defer the response.
    val r0 = randScalar()
    val c0 = PreDlog(knownLeaf.pk, sat = true, knownLeaf.x, r0, g1.multiply(r0.bigInteger))
    // Children 1,2: HVZK-SIMULATED on the proposition's REAL pks with FREE challenges.
    val c1 = simulateForced(sim1, randChallenge()).asInstanceOf[PreDlog]
    val c2 = simulateForced(sim2, randChallenge()).asInstanceOf[PreDlog]

    // Use the REAL FS root as the threshold node challenge (so step-6's root check would PASS),
    // isolating the failure to interpolation. With (k-1) real children the prover has only ONE
    // free DOF on the polynomial besides (0, root) and the (n-k+1)=2 simulated free points: the
    // degree is n-k = 1, so a line is fixed by (0, root) + the FIRST simulated point; the SECOND
    // simulated point and the real child's derived challenge generically do NOT both lie on it.
    val thrNode = PreThr(k, List(c0, c1, c2), sat = false)
    val root = low31(DomainSep ++ thrNode.serializeWithCommitments ++ msg)
    // Derive child 0's challenge as the line through (0, root) + the FIRST simulated child (index
    // 1, evaluation point x=2) WOULD assign it at x=1 (so c0's transcript is self-consistent for
    // its own challenge). The verifier still rejects: the SECOND simulated child (index 2, x=3) is
    // over-determined relative to that line and breaks the per-lane interpolation check.
    val e0 = {
      val bytes = Array.tabulate(ChallengeBytes) { lane =>
        val ys = Array(challengeBytes(root)(lane) & 0xff, challengeBytes(c1.e)(lane) & 0xff)
        gfLagrange(Array(0, 2), ys, 1).toByte // P(1) for the real child at index 0 (x=1)
      }
      BigInt(1, bytes)
    }
    c0.e = e0; c0.z = (c0.r + e0 * xKnown).mod(R)
    val proofJ =
      s"""{"type":"threshold","e":"${hexChallenge(root)}","k":$k,"children":[${proofJson(c0)},${proofJson(c1)},${proofJson(c2)}]}"""
    evalSigma(propJson(prop), proofJ, msgHex).map(r => expect(r == Right(BoolValue(false))))
  }

  test("soundness: wrong message => false (strong-FS binds the message)") {
    val prop = And(List(dlogKnown(randScalar()), dlogKnown(randScalar())))
    val (pj, prf) = prove(prop, msg)
    val otherMsg = HexBytes.encodeBytes("a DIFFERENT message".getBytes("UTF-8"))
    evalSigma(pj, prf, otherMsg).map(r => expect(r == Right(BoolValue(false))))
  }

  test("soundness: tampered response (z flipped) on a valid AND => false") {
    val prop = And(List(dlogKnown(randScalar()), dhKnown(BigInt(17), BigInt(2), BigInt(5))))
    val (pj, prf) = prove(prop, msg)
    // Flip the last hex nibble of the FIRST child's z; the reconstructed commitment changes,
    // so the recomputed root no longer matches the (untouched) root challenge.
    val tampered = {
      val i = prf.indexOf("\"z\":\"0x")
      val zStart = i + 6
      val zEnd = prf.indexOf("\"", zStart + 2)
      val orig = prf.substring(zStart, zEnd)
      val flipped = orig.dropRight(1) + (if (orig.last == '0') '1' else '0')
      prf.substring(0, zStart) + flipped + prf.substring(zEnd)
    }
    evalSigma(pj, tampered, msgHex).map(r => expect(r == Right(BoolValue(false))))
  }

  // Flip the last hex nibble of the FIRST 0x-hex value occurring at/after `from` (a "...":"0x...."
  // field). Robust to field width; returns the tampered proof string.
  private def flipHexAt(s: String, from: Int): String = {
    val valStart = s.indexOf("\"0x", from) + 1 // position of the opening quote's next char ('0')
    val valEnd = s.indexOf("\"", valStart) // closing quote
    val orig = s.substring(valStart, valEnd)
    val flipped = orig.dropRight(1) + (if (orig.last == '0') '1' else '0')
    s.substring(0, valStart) + flipped + s.substring(valEnd)
  }

  test("soundness: tampered commitment via tampered root challenge => false") {
    // Flip a nibble of the ROOT challenge in the proof: the verifier reconstructs the commitment
    // from the (unchanged) response but compares against the tampered root -> mismatch -> false.
    val prop = dlogKnown(randScalar())
    val (pj, prf) = prove(prop, msg)
    val tampered = flipHexAt(prf, prf.indexOf("\"e\":")) // the leaf's own e == the root challenge
    evalSigma(pj, tampered, msgHex).map(r => expect(r == Right(BoolValue(false))))
  }

  test("soundness: AND with one child challenge != parent (broken copy relation) => false") {
    // AND requires every child e == node e. Corrupt the FIRST CHILD's e on an otherwise-valid AND
    // proof -> the copy relation fails -> false.
    val prop = And(List(dlogKnown(randScalar()), dlogKnown(randScalar())))
    val (_, validProof) = prove(prop, msg)
    val childEIdx = validProof.indexOf("\"e\":", validProof.indexOf("\"children\""))
    val tampered = flipHexAt(validProof, childEIdx)
    evalSigma(propJson(prop), tampered, msgHex).map(r => expect(r == Right(BoolValue(false))))
  }

  // ===========================================================================
  // STRUCTURAL / ENCODING ERRORS (hard JsonLogicException, never false).
  // ===========================================================================

  test("error: off-curve statement point => hard error") {
    val offCurve = HexBytes.encodeG1(BigInt(1), BigInt(1)).fold(throw _, identity) // (1,1) not on curve
    val prop = s"""{"type":"dlog","pk":"$offCurve"}"""
    val proof = s"""{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(2))}"}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: unknown node type => hard error") {
    val prop = s"""{"type":"xor","children":[{"type":"dlog","pk":"${encG1(g1)}"}]}"""
    val proof =
      s"""{"type":"xor","e":"${hexChallenge(BigInt(1))}","children":[{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(
          BigInt(1)
        )}"}]}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: threshold k > n => hard error") {
    val prop = s"""{"type":"threshold","k":5,"children":[{"type":"dlog","pk":"${encG1(g1)}"},{"type":"dlog","pk":"${encG1(g1)}"}]}"""
    val proof =
      s"""{"type":"threshold","e":"${hexChallenge(BigInt(1))}","k":5,"children":[{"type":"dlog","e":"${hexChallenge(
          BigInt(1)
        )}","z":"${hex32(
          BigInt(1)
        )}"},{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}]}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: threshold k <= 0 => hard error") {
    val prop = s"""{"type":"threshold","k":0,"children":[{"type":"dlog","pk":"${encG1(g1)}"}]}"""
    val proof = s"""{"type":"threshold","e":"${hexChallenge(BigInt(1))}","k":0,"children":[{"type":"dlog","e":"${hexChallenge(
        BigInt(1)
      )}","z":"${hex32(
        BigInt(1)
      )}"}]}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: proposition / proof shape mismatch (dlog vs dhtuple) => hard error") {
    val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
    val proof = s"""{"type":"dhtuple","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: proposition / proof child-count mismatch => hard error") {
    val prop = s"""{"type":"and","children":[{"type":"dlog","pk":"${encG1(g1)}"},{"type":"dlog","pk":"${encG1(g1)}"}]}"""
    val proof =
      s"""{"type":"and","e":"${hexChallenge(BigInt(1))}","children":[{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(
          BigInt(1)
        )}"}]}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: missing required field (pk) => hard error") {
    val prop = """{"type":"dlog"}"""
    val proof = s"""{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: wrong-width challenge in proof => hard error") {
    val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
    val proof = s"""{"type":"dlog","e":"0xdead","z":"${hex32(BigInt(1))}"}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: 32-byte challenge in proof => hard error (challenge domain is 31 bytes, finding #1)") {
    // A challenge supplied at the OLD 32-byte width must now be rejected as wrong-width: the
    // injective challenge domain is exactly 31 bytes. Pins the width change at the boundary.
    val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
    val thirtyTwoByteE = HexBytes.encodeUInt(BigInt(1), 32).fold(throw _, identity)
    val proof = s"""{"type":"dlog","e":"$thirtyTwoByteE","z":"${hex32(BigInt(1))}"}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r.isLeft))
  }

  test("error: malformed message hex => hard error") {
    val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
    val proof = s"""{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}"""
    evalSigma(prop, proof, "0xZZ").map(r => expect(r.isLeft))
  }

  test("error: wrong arity (2 args) => hard error") {
    val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
    val exprJson = s"""{"sigma_verify":[$prop,"$msgHex"]}"""
    for {
      expr <- IO.fromEither(parser.parse(exprJson).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse("{}").flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield expect(out.isLeft)
  }

  // ----- identity statement point (well-formed but a forgery vector) -> false -----

  test("false: identity dlog pk (universal-forgery vector) => false, not error") {
    val identity = "0x" + "0" * 128
    val prop = s"""{"type":"dlog","pk":"$identity"}"""
    val proof = s"""{"type":"dlog","e":"${hexChallenge(BigInt(123))}","z":"${hex32(BigInt(456))}"}"""
    evalSigma(prop, proof, msgHex).map(r => expect(r == Right(BoolValue(false))))
  }

  // ===========================================================================
  // GAS.
  // ===========================================================================

  test("gas: sigma_verify charges base + per-leaf + per-node from the proposition shape") {
    // Proposition: AND( dlog, dhtuple ) => 1 node + 1 dlog leaf + 1 dhtuple leaf.
    val prop = And(List(dlogKnown(randScalar()), dhKnown(BigInt(21), BigInt(2), BigInt(3))))
    val (pj, prf) = prove(prop, msg)
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val cfg = GasConfig.Default
    for {
      expr <- IO.fromEither(parser.parse(sigmaExpr(pj, prf, msgHex)).flatMap(_.as[JsonLogicExpression]))
      res  <- evaluator.evaluateWithGas(expr, MapValue.empty, None, GasLimit.Unlimited, cfg).flatMap(IO.fromEither)
    } yield {
      val expected =
        cfg.sigmaVerify.amount + cfg.depthPenalty(1L).amount +
        cfg.sigmaVerifyPerDlogLeaf.amount + cfg.sigmaVerifyPerDhtupleLeaf.amount + cfg.sigmaVerifyPerNode.amount
      expect(res.value == BoolValue(true)) &&
      expect(res.gasUsed.amount == expected, s"expected $expected got ${res.gasUsed.amount}")
    }
  }

  test("gas: sigma_verify runs out of gas under a tight limit (DoS bound) before curve work") {
    val prop = Threshold(2, List(dlogKnown(randScalar()), dlogKnown(randScalar()), dlogUnknown()))
    val (pj, prf) = prove(prop, msg)
    val evaluator = JsonLogicEvaluator.tailRecursive[IO]
    val cfg = GasConfig.Default
    // Tight limit below even the base cost: must exhaust before any scalar mul.
    val tight = GasLimit(cfg.sigmaVerify.amount - 1L)
    for {
      expr <- IO.fromEither(parser.parse(sigmaExpr(pj, prf, msgHex)).flatMap(_.as[JsonLogicExpression]))
      res  <- evaluator.evaluateWithGas(expr, MapValue.empty, None, tight, cfg)
    } yield expect(res.isLeft)
  }

  // ===========================================================================
  // Worked example: an m-of-n mint-policy guard evaluated end-to-end.
  // ===========================================================================

  test("worked example: 2-of-3 issuer threshold gate returns 'authorized' for a valid proof") {
    val prop = thresholdKnown(2, 3, knownCount = 2)
    val (pj, prf) = prove(prop, msg)
    val gate = s"""{"if":[${sigmaExpr(pj, prf, msgHex)},"authorized","rejected"]}"""
    for {
      expr <- IO.fromEither(parser.parse(gate).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse("{}").flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield expect(out == Right(StrValue("authorized")))
  }
}
