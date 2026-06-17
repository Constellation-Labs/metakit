package json_logic

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.security.{MessageDigest, SecureRandom}

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import io.constellationnetwork.metagraph_sdk.crypto.zk.Bn254
import io.constellationnetwork.metagraph_sdk.json_logic.core.JsonLogicValue.showJsonLogicValue
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.ops.{CryptoOps, HexBytes}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.parser
import weaver.SimpleIOSuite

/**
 * SCALA-REFERENCE VECTOR GENERATOR for the three Σ-protocol opcodes
 * (`prove_dlog_verify`, `prove_dhtuple_verify`, `sigma_verify`).
 *
 * This is the vector-generation mechanism for the cross-language parity work: metakit (Scala) is the
 * REFERENCE, so every `expected` value emitted here is whatever the REAL `CryptoOps` verifier
 * returns when run over the generated proposition/proof/message — never a hand-asserted constant.
 * The generated cases land in the exact `shared/zk_opcode_test_vectors.json` schema (one
 * `{category, cases:[...]}` block per opcode, each case carrying `expr` + `data` + either `expected`
 * (a JSON-string of the result) OR `error:true`), so the Rust `zk_differential.rs` harness consumes
 * them unchanged.
 *
 * The Σ provers (leaf DLog/DHTuple + the recursive CDS tree prover with HVZK branch simulation) are
 * lifted from `SigmaOpsSuite` / `SigmaVerifySuite`. A fixed-seed RNG makes the emitted vectors
 * deterministic and reproducible.
 *
 * Running this weaver suite (a) emits the JSON fragment to the path below and (b) ASSERTS that every
 * generated case's captured `expected`/`error` actually matches what the verifier produces — so the
 * generator is self-checking (it is the metakit-side conformance runner over the augmented file at
 * generation time).
 */
object SigmaVectorGen extends SimpleIOSuite {

  // Outputs land under the gitignored build dir so running the generator never
  // dirties the source tree. The emitted vectors are then MERGED (by the parity
  // workflow) into shared/zk_opcode_test_vectors.json; the KATs are baked into the
  // Rust serialization byte-identity test. Re-run to regenerate / re-validate.
  private val OutPath = "target/sigma_vectors_generated.json"
  private val KatPath = "target/sigma_serialization_kats.json"

  // ===========================================================================
  // Shared crypto scaffolding (mirrors CryptoOps.Sigma + the leaf opcodes).
  // ===========================================================================

  private val R: BigInt = BigInt(Bn254.R)
  private val g1: Bn254.G1 = Bn254.G1(BigInteger.ONE, BigInteger.valueOf(2)) // generator (1,2)

  private val RngSeed: Long = 0x516d61_5645435L // "sigmaVEC"
  // A FRESH SHA1PRNG, seeded immediately after construction (deterministic).
  // NOTE: SHA1PRNG.setSeed SUPPLEMENTS the existing seed rather than resetting,
  // so `reseed()` must build a BRAND-NEW instance to give a repeatable stream.
  private def freshRng(): SecureRandom = {
    val r = SecureRandom.getInstance("SHA1PRNG")
    r.setSeed(RngSeed) // "sigmaVEC" fixed seed -> reproducible vectors
    r
  }
  // Reset the RNG so vector/KAT generation is DETERMINISTIC and independent of
  // test-execution order (weaver may run the KAT and vector-gen tests in any
  // order; both reseed before drawing). This makes the committed vectors + the
  // baked KAT bytes byte-reproducible by re-running the generator.
  private def reseed(): Unit = rng = freshRng()

  private var rng: SecureRandom = freshRng()
  private def randScalar(): BigInt = BigInt(1, { val b = new Array[Byte](32); rng.nextBytes(b); b }).mod(R)

  private def encG1(p: Bn254.G1): String = HexBytes.encodeG1(BigInt(p.x), BigInt(p.y)).toOption.get
  private def g1Bytes(p: Bn254.G1): Array[Byte] = HexBytes.parseBytes(encG1(p), Some(64), "g1").toOption.get
  private def hex32(v: BigInt): String = HexBytes.encodeUInt(v.mod(R), 32).toOption.get
  // A NON-CANONICAL response scalar (audit finding #4): `v + R`, emitted as a RAW 32-byte value (no
  // mod R). For any canonical response `v < R`, `v + R < 2R < 2^255` still fits in 32 bytes, so the
  // proof keeps its fixed width but the response is now `>= R` -> requireCanonicalScalar HARD-errors.
  private def hex32NonCanonical(v: BigInt): String = HexBytes.encodeUInt(v.mod(R) + R, 32).toOption.get
  private def sha256(bytes: Array[Byte]): Array[Byte] = MessageDigest.getInstance("SHA-256").digest(bytes)
  private def sha256Big(bytes: Array[Byte]): BigInt = BigInt(1, sha256(bytes))

  // CHALLENGE DOMAIN (audit finding #1): challenges are 31-byte (248-bit) values (the injective
  // -into-Fr domain; 2^248 < R). MUST match CryptoOps.Sigma.ChallengeBytes exactly.
  private val ChallengeBytes: Int = 31
  // A random 31-byte challenge (a free/simulated CDS challenge) — < 2^248 by construction.
  private def randChallenge(): BigInt = BigInt(1, { val b = new Array[Byte](ChallengeBytes); rng.nextBytes(b); b })
  // Emit a 31-byte challenge as fixed-width hex (the proof `e` field).
  private def hexChallenge(v: BigInt): String = HexBytes.encodeUInt(v, ChallengeBytes).toOption.get
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
  private def challengeBytes(e: BigInt): Array[Byte] = HexBytes.parseBytes(hexChallenge(e), Some(ChallengeBytes), "e").toOption.get

  // ===========================================================================
  // LEAF provers (from SigmaOpsSuite).
  // ===========================================================================

  // Schnorr / DLog: proof = R(64B) || s(32B); c = SHA256(R || pk || msg) mod R; s = k + c*x mod R.
  private def schnorrProve(x: BigInt, msg: Array[Byte], k: BigInt): (String, String) = {
    val pk = g1.multiply(x.bigInteger)
    val rPoint = g1.multiply(k.bigInteger)
    val c = sha256Big(g1Bytes(rPoint) ++ g1Bytes(pk) ++ msg).mod(R)
    val s = (k + c * x).mod(R)
    val sHex = hex32(s)
    val proof = "0x" + HexBytes.encodeBytes(g1Bytes(rPoint)).substring(2) + sHex.substring(2)
    (encG1(pk), proof)
  }

  // DHTuple: proof = a1(64B) || a2(64B) || z(32B); e = SHA256(g‖h‖u‖v‖a1‖a2‖msg) mod R; z = r + e*w.
  private def dhChallenge(g: Bn254.G1, h: Bn254.G1, u: Bn254.G1, v: Bn254.G1, a1: Bn254.G1, a2: Bn254.G1, msg: Array[Byte]): BigInt =
    sha256Big(g1Bytes(g) ++ g1Bytes(h) ++ g1Bytes(u) ++ g1Bytes(v) ++ g1Bytes(a1) ++ g1Bytes(a2) ++ msg).mod(R)

  private def dhProofBytes(a1: Bn254.G1, a2: Bn254.G1, z: BigInt): String =
    "0x" + HexBytes.encodeBytes(g1Bytes(a1)).substring(2) +
    HexBytes.encodeBytes(g1Bytes(a2)).substring(2) + hex32(z).substring(2)

  private def dhTupleProve(
    w: BigInt,
    r: BigInt,
    gScalar: BigInt,
    hScalar: BigInt,
    msg: Array[Byte]
  ): (Bn254.G1, Bn254.G1, Bn254.G1, Bn254.G1, String) = {
    val g = g1.multiply(gScalar.bigInteger)
    val h = g1.multiply(hScalar.bigInteger)
    val u = g.multiply(w.bigInteger)
    val v = h.multiply(w.bigInteger)
    val a1 = g.multiply(r.bigInteger)
    val a2 = h.multiply(r.bigInteger)
    val e = dhChallenge(g, h, u, v, a1, a2, msg)
    val z = (r + e * w).mod(R)
    (g, h, u, v, dhProofBytes(a1, a2, z))
  }

  // ===========================================================================
  // TREE prover (from SigmaVerifySuite): commit -> strong-FS root -> split -> responses -> JSON.
  // ===========================================================================

  sealed trait Prop
  final case class Dlog(x: Option[BigInt], pk: Bn254.G1) extends Prop
  final case class DhTuple(w: Option[BigInt], g: Bn254.G1, h: Bn254.G1, u: Bn254.G1, v: Bn254.G1) extends Prop
  final case class And(children: List[Prop]) extends Prop
  final case class Or(children: List[Prop]) extends Prop
  final case class Threshold(k: Int, children: List[Prop]) extends Prop

  private def dlogKnown(x: BigInt): Dlog = Dlog(Some(x.mod(R)), g1.multiply(x.bigInteger))
  private def dlogUnknown(): Dlog = { val x = randScalar(); Dlog(None, g1.multiply(x.bigInteger)) }
  private def dhKnown(w: BigInt, gScalar: BigInt, hScalar: BigInt): DhTuple = {
    val g = g1.multiply(gScalar.bigInteger); val h = g1.multiply(hScalar.bigInteger)
    DhTuple(Some(w.mod(R)), g, h, g.multiply(w.bigInteger), h.multiply(w.bigInteger))
  }

  private def satisfiable(p: Prop): Boolean = p match {
    case Dlog(x, _)             => x.isDefined
    case DhTuple(w, _, _, _, _) => w.isDefined
    case And(cs)                => cs.forall(satisfiable)
    case Or(cs)                 => cs.exists(satisfiable)
    case Threshold(k, cs)       => cs.count(satisfiable) >= k
  }

  private def propJson(p: Prop): String = p match {
    case Dlog(_, pk)            => s"""{"type":"dlog","pk":"${encG1(pk)}"}"""
    case DhTuple(_, g, h, u, v) => s"""{"type":"dhtuple","g":"${encG1(g)}","h":"${encG1(h)}","u":"${encG1(u)}","v":"${encG1(v)}"}"""
    case And(cs)                => s"""{"type":"and","children":[${cs.map(propJson).mkString(",")}]}"""
    case Or(cs)                 => s"""{"type":"or","children":[${cs.map(propJson).mkString(",")}]}"""
    case Threshold(k, cs)       => s"""{"type":"threshold","k":$k,"children":[${cs.map(propJson).mkString(",")}]}"""
  }

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

  private def commit(p: Prop, mustSimulate: Boolean = false): PreProof =
    if (mustSimulate || !satisfiable(p)) simulateForced(p, randChallenge()) // 31-byte free node e (finding #1)
    else
      p match {
        case Dlog(xOpt, pk) => val r = randScalar(); PreDlog(pk, sat = true, xOpt, r, g1.multiply(r.bigInteger))
        case DhTuple(wOpt, g, h, u, v) =>
          val r = randScalar(); PreDh(g, h, u, v, sat = true, wOpt, r, g.multiply(r.bigInteger), h.multiply(r.bigInteger))
        case And(cs) => PreAnd(cs.map(c => commit(c, mustSimulate = false)), sat = true)
        case Or(cs) =>
          val realIdx = cs.indexWhere(satisfiable)
          PreOr(cs.zipWithIndex.map { case (c, i) => commit(c, mustSimulate = i != realIdx) }, sat = true)
        case Threshold(k, cs) =>
          val realIdxs = cs.zipWithIndex.collect { case (c, i) if satisfiable(c) => i }.take(k).toSet
          PreThr(k, cs.zipWithIndex.map { case (c, i) => commit(c, mustSimulate = !realIdxs.contains(i)) }, sat = true)
      }

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
      val frees = cs.dropRight(1).map(c => simulateForced(c, randChallenge()))
      val lastE = frees.foldLeft(e)((acc, c) => acc ^ c.e)
      val node = PreOr(frees :+ simulateForced(cs.last, lastE), sat = false); node.e = e; node
    case Threshold(k, cs) =>
      val n = cs.length
      val degree = n - k
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
          )
          simulateForced(cs(i), forced)
        }
      }
      val node = PreThr(k, children, sat = false); node.e = e; node
  }

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

  private def setChallenge(pp: PreProof, e: BigInt): Unit =
    if (!pp.sat) {
      require(pp.e == e, s"simulated subtree challenge mismatch: ${pp.e} vs $e") // 31-byte domain: no mod R
    } else
      pp match {
        case d: PreDlog => d.e = e; d.z = (d.r + e * d.witness.get).mod(R)
        case d: PreDh   => d.e = e; d.z = (d.r + e * d.witness.get).mod(R)
        case a: PreAnd =>
          a.e = e
          a.children.foreach(c => setChallenge(c, e))
        case o: PreOr =>
          o.e = e
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
          val realIdxs = t.children.zipWithIndex.collect { case (c, i) if c.sat => i }.toSet
          val simIdxs = (0 until n).filterNot(realIdxs.contains).toList
          require(simIdxs.length == degree, s"threshold prover: expected $degree simulated children, got ${simIdxs.length}")
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
            ri -> BigInt(1, bytes)
          }.toMap
          t.children.zipWithIndex.foreach {
            case (c, i) => setChallenge(c, if (realIdxs.contains(i)) realE(i) else c.e)
          }
      }

  private def proofJson(pp: PreProof): String = pp match {
    case d: PreDlog => s"""{"type":"dlog","e":"${hexChallenge(d.e)}","z":"${hex32(d.z)}"}"""
    case d: PreDh   => s"""{"type":"dhtuple","e":"${hexChallenge(d.e)}","z":"${hex32(d.z)}"}"""
    case a: PreAnd  => s"""{"type":"and","e":"${hexChallenge(a.e)}","children":[${a.children.map(proofJson).mkString(",")}]}"""
    case o: PreOr   => s"""{"type":"or","e":"${hexChallenge(o.e)}","children":[${o.children.map(proofJson).mkString(",")}]}"""
    case t: PreThr =>
      s"""{"type":"threshold","e":"${hexChallenge(t.e)}","k":${t.k},"children":[${t.children.map(proofJson).mkString(",")}]}"""
  }

  private def prove(prop: Prop, m: Array[Byte]): (String, String) = {
    val pp = commit(prop)
    val rootChallenge = low31(DomainSep ++ pp.serializeWithCommitments ++ m) // 31-byte root (finding #1)
    setChallenge(pp, rootChallenge)
    (propJson(prop), proofJson(pp))
  }

  // ===========================================================================
  // Case model + JSON emission in the shared schema.
  // ===========================================================================

  private val msg: Array[Byte] = "authorize sigma".getBytes("UTF-8")
  private val msgHex: String = HexBytes.encodeBytes(msg)

  // Run an expr through the REAL evaluator (the metakit reference), returning either the rendered
  // result string (Show) or None if evaluation failed (raised or Left).
  private def evalReal(exprJson: String): Either[String, String] =
    (for {
      expr <- IO.fromEither(parser.parse(exprJson).flatMap(_.as[JsonLogicExpression]))
      data <- IO.fromEither(parser.parse("{}").flatMap(_.as[JsonLogicValue]))
      out  <- JsonLogicEvaluator.tailRecursive[IO].evaluate(expr, data, None)
    } yield out).attempt.map {
      case Left(raised)        => Left(s"raised: ${raised.getMessage}")
      case Right(Left(err))    => Left(s"eval-err: ${err.getMessage}")
      case Right(Right(value)) => Right(showJsonLogicValue.show(value))
    }
      .unsafeRunSync()

  // One emitted vector case.
  final private case class Case(expr: String, expected: Option[String], error: Boolean, note: String)

  // JSON-escape a raw expr string for embedding as a JSON string value.
  private def jsonStr(s: String): String = {
    val sb = new StringBuilder("\"")
    s.foreach {
      case '"'  => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case c    => sb.append(c)
    }
    sb.append("\"").toString
  }

  private def renderCase(c: Case): String = {
    val parts = scala.collection.mutable.ListBuffer[String]()
    parts += s"""        "expr": ${jsonStr(c.expr)}"""
    parts += s"""        "data": "{}""""
    if (c.error) parts += """        "error": true"""
    else parts += s"""        "expected": ${jsonStr(c.expected.get)}"""
    parts += s"""        "note": ${jsonStr(c.note)}"""
    "      {\n" + parts.mkString(",\n") + "\n      }"
  }

  private def renderCategory(category: String, cases: List[Case]): String =
    s"""    {
       |      "category": "$category",
       |      "cases": [
       |${cases.map(renderCase).mkString(",\n")}
       |      ]
       |    }""".stripMargin

  // Build a VALUE case: run it, capture the actual rendered result, and assert it matches `want`.
  private def valueCase(expr: String, want: String, note: String): (Case, Boolean, String) =
    evalReal(expr) match {
      case Right(got) if got == want => (Case(expr, Some(got), error = false, note), true, "")
      case Right(got)                => (Case(expr, Some(got), error = false, note), false, s"[$note] wanted $want got $got")
      case Left(err) => (Case(expr, Some(want), error = false, note), false, s"[$note] wanted value $want but ERRORED: $err")
    }

  // Build an ERROR case: run it, assert it actually errors.
  private def errorCase(expr: String, note: String): (Case, Boolean, String) =
    evalReal(expr) match {
      case Left(_)    => (Case(expr, None, error = true, note), true, "")
      case Right(got) => (Case(expr, None, error = true, note), false, s"[$note] wanted ERROR but got value $got")
    }

  private def dlogExpr(pk: String, msgH: String, proof: String): String =
    s"""{"prove_dlog_verify":["$pk","$msgH","$proof"]}"""
  private def dhExpr(g: String, h: String, u: String, v: String, msgH: String, proof: String): String =
    s"""{"prove_dhtuple_verify":["$g","$h","$u","$v","$msgH","$proof"]}"""
  private def sigmaExpr(propJ: String, proofJ: String, msgH: String): String =
    s"""{"sigma_verify":[$propJ,$proofJ,"$msgH"]}"""

  // ---------------------------------------------------------------------------
  // prove_dlog_verify cases.
  // ---------------------------------------------------------------------------

  private def dlogCases(): List[(Case, Boolean, String)] = {
    val x = BigInt("123456789012345678901234567890").mod(R)
    val k = BigInt("987654321098765432109876543210").mod(R)
    val (pk, proof) = schnorrProve(x, msg, k)

    // generated extra valid
    val x2 = randScalar(); val k2 = randScalar()
    val (pk2, proof2) = schnorrProve(x2, msg, k2)

    // tampered proof: flip last byte of s
    val tampered = {
      val b = HexBytes.parseBytes(proof, Some(96), "p").toOption.get
      b(95) = (b(95) ^ 0x01).toByte
      HexBytes.encodeBytes(b)
    }
    // wrong message
    val otherMsgHex = HexBytes.encodeBytes("a different message".getBytes("UTF-8"))
    // identity pk forgery vector -> false
    val sForge = BigInt(123456789)
    val rForge = g1.multiply(sForge.bigInteger)
    val forgeProof = "0x" + HexBytes.encodeBytes(g1Bytes(rForge)).substring(2) + hex32(sForge).substring(2)
    val identityPk = "0x" + "0" * 128
    // off-curve pk (1,1)
    val offCurve = HexBytes.encodeG1(BigInt(1), BigInt(1)).toOption.get
    // NON-CANONICAL response s (finding #4): an otherwise-valid proof whose s is replaced by s + R
    // (still 32 bytes, but >= R). `s` and `s + R` are congruent mod R and verify identically, so
    // accepting the raw 32-byte form would make the proof malleable -> requireCanonicalScalar errors.
    val nonCanonicalProof = {
      val pkP = g1.multiply(x.bigInteger)
      val rPoint = g1.multiply(k.bigInteger)
      val c = sha256Big(g1Bytes(rPoint) ++ g1Bytes(pkP) ++ msg).mod(R)
      val s = (k + c * x).mod(R)
      "0x" + HexBytes.encodeBytes(g1Bytes(rPoint)).substring(2) + hex32NonCanonical(s).substring(2)
    }

    List(
      valueCase(dlogExpr(pk, msgHex, proof), "true", "known-answer valid DLog/Schnorr proof -> true"),
      valueCase(dlogExpr(pk2, msgHex, proof2), "true", "generated valid DLog proof (fresh witness/nonce) -> true"),
      valueCase(dlogExpr(pk, msgHex, tampered), "false", "tampered response s (last byte flipped) -> false"),
      valueCase(dlogExpr(pk, otherMsgHex, proof), "false", "wrong message (FS binds msg) -> false"),
      valueCase(dlogExpr(identityPk, msgHex, forgeProof), "false", "identity pk universal-forgery vector -> false (not error)"),
      errorCase(dlogExpr(pk, msgHex, nonCanonicalProof), "non-canonical response s = s + R (>= R) -> error (finding #4)"),
      errorCase(dlogExpr(offCurve, msgHex, proof), "off-curve pk (1,1) not on y^2=x^3+3 -> error"),
      errorCase(dlogExpr(pk, msgHex, "0xdead"), "wrong-width proof (2 bytes, not 96) -> error"),
      errorCase(dlogExpr(pk, "0xZZ", proof), "malformed message hex -> error")
    )
  }

  // ---------------------------------------------------------------------------
  // prove_dhtuple_verify cases.
  // ---------------------------------------------------------------------------

  private def dhtupleCases(): List[(Case, Boolean, String)] = {
    val w = BigInt("111122223333444455556666777788889999").mod(R)
    val r = BigInt("424242424242424242424242424242424242").mod(R)
    val (g, h, u, v, proof) = dhTupleProve(w, r, BigInt(3), BigInt(5), msg)
    val (gH, hH, uH, vH) = (encG1(g), encG1(h), encG1(u), encG1(v))

    // generated extra valid
    val w2 = randScalar(); val r2 = randScalar()
    val (g2, h2, u2, v2, proof2) = dhTupleProve(w2, r2, BigInt(7), BigInt(11), msg)

    // tampered z
    val tamperedZ = {
      val b = HexBytes.parseBytes(proof, Some(160), "p").toOption.get
      b(159) = (b(159) ^ 0x01).toByte
      HexBytes.encodeBytes(b)
    }
    // non-DH-tuple: v' = h^(w+7) -> no single witness -> false
    val vWrong = encG1(h.multiply((w + BigInt(7)).bigInteger))
    // wrong message
    val otherMsgHex = HexBytes.encodeBytes("authorize a different compose".getBytes("UTF-8"))
    // identity base g=O -> false; identity image v=O -> false
    val identity = "0x" + "0" * 128
    // off-curve
    val offCurve = HexBytes.encodeG1(BigInt(1), BigInt(1)).toOption.get
    // NON-CANONICAL response z (finding #4): the valid proof's a1‖a2 kept, but z replaced by z + R
    // (still 32 bytes, >= R). Congruent mod R -> verifies identically, so accepting it would make the
    // proof bytes malleable; requireCanonicalScalar HARD-errors instead.
    val nonCanonicalProof = {
      val a1 = g.multiply(r.bigInteger)
      val a2 = h.multiply(r.bigInteger)
      val e = dhChallenge(g, h, u, v, a1, a2, msg)
      val z = (r + e * w).mod(R)
      "0x" + HexBytes.encodeBytes(g1Bytes(a1)).substring(2) +
      HexBytes.encodeBytes(g1Bytes(a2)).substring(2) + hex32NonCanonical(z).substring(2)
    }

    List(
      valueCase(dhExpr(gH, hH, uH, vH, msgHex, proof), "true", "known-answer valid DH-tuple proof -> true"),
      valueCase(dhExpr(encG1(g2), encG1(h2), encG1(u2), encG1(v2), msgHex, proof2), "true", "generated valid DH-tuple proof -> true"),
      valueCase(dhExpr(gH, hH, uH, vH, msgHex, tamperedZ), "false", "tampered response z -> false"),
      valueCase(dhExpr(gH, hH, uH, vWrong, msgHex, proof), "false", "non-DH-tuple (v=h^(w+7), w' != w) -> false"),
      valueCase(dhExpr(gH, hH, uH, vH, otherMsgHex, proof), "false", "wrong message (strong-FS binds msg) -> false"),
      valueCase(dhExpr(identity, hH, uH, vH, msgHex, proof), "false", "identity base g=O forgery vector -> false (not error)"),
      valueCase(dhExpr(gH, hH, uH, identity, msgHex, proof), "false", "identity image v=O degenerate-hiding -> false (not error)"),
      errorCase(dhExpr(gH, hH, uH, vH, msgHex, nonCanonicalProof), "non-canonical response z = z + R (>= R) -> error (finding #4)"),
      errorCase(dhExpr(offCurve, hH, uH, vH, msgHex, proof), "off-curve statement point -> error"),
      errorCase(dhExpr(gH, hH, uH, vH, msgHex, "0xdead"), "wrong-width proof -> error"),
      errorCase(s"""{"prove_dhtuple_verify":["$gH","$hH","$uH"]}""", "wrong arity -> error")
    )
  }

  // ---------------------------------------------------------------------------
  // sigma_verify cases (the BYTE-CONTRACT). Each valid proof is generated by the
  // CDS prover; soundness negatives are constructed forgeries; malformed -> error.
  // ---------------------------------------------------------------------------

  private def sigmaCases(): List[(Case, Boolean, String)] = {
    val cases = scala.collection.mutable.ListBuffer[(Case, Boolean, String)]()

    // --- single dlog leaf ---
    {
      val prop = dlogKnown(BigInt("12345678901234567890"))
      val (pj, prf) = prove(prop, msg)
      cases += valueCase(sigmaExpr(pj, prf, msgHex), "true", "single dlog leaf, valid -> true")
    }
    // --- single dhtuple leaf ---
    {
      val prop = dhKnown(BigInt("999888777666555"), BigInt(3), BigInt(5))
      val (pj, prf) = prove(prop, msg)
      cases += valueCase(sigmaExpr(pj, prf, msgHex), "true", "single dhtuple leaf, valid -> true")
    }
    // --- AND(dlog, dhtuple) ---
    {
      val prop = And(List(dlogKnown(BigInt(7)), dhKnown(BigInt(11), BigInt(2), BigInt(9))))
      val (pj, prf) = prove(prop, msg)
      cases += valueCase(sigmaExpr(pj, prf, msgHex), "true", "AND(dlog,dhtuple), valid -> true")
    }
    // --- OR-ring n=2 (real branch 0) ---
    {
      val prop = Or(List(dlogKnown(randScalar()), dlogUnknown()))
      val (pj, prf) = prove(prop, msg)
      cases += valueCase(sigmaExpr(pj, prf, msgHex), "true", "OR ring n=2 (real branch 0, hiding) -> true")
    }
    // --- OR-ring n=3 (real branch 2) ---
    {
      val prop = Or(List(dlogUnknown(), dlogUnknown(), dlogKnown(randScalar())))
      val (pj, prf) = prove(prop, msg)
      cases += valueCase(sigmaExpr(pj, prf, msgHex), "true", "OR ring n=3 (real branch 2, hiding) -> true")
    }
    // --- THRESHOLD 2-of-3 ---
    {
      val prop = Threshold(2, List(dlogKnown(randScalar()), dlogKnown(randScalar()), dlogUnknown()))
      val (pj, prf) = prove(prop, msg)
      cases += valueCase(sigmaExpr(pj, prf, msgHex), "true", "THRESHOLD 2-of-3 (exactly k witnesses) -> true")
    }
    // --- THRESHOLD 1-of-4 ---
    {
      val prop = Threshold(1, List(dlogKnown(randScalar()), dlogUnknown(), dlogUnknown(), dlogUnknown()))
      val (pj, prf) = prove(prop, msg)
      cases += valueCase(sigmaExpr(pj, prf, msgHex), "true", "THRESHOLD 1-of-4 (exactly k witnesses) -> true")
    }
    // --- nested AND of ORs ---
    {
      val prop = And(
        List(
          Or(List(dlogKnown(randScalar()), dlogUnknown())),
          Or(List(dlogUnknown(), dlogKnown(randScalar())))
        )
      )
      val (pj, prf) = prove(prop, msg)
      cases += valueCase(sigmaExpr(pj, prf, msgHex), "true", "nested (A or B) and (C or D), valid -> true")
    }

    // --- SOUNDNESS: forge-all-simulated OR (XOR holds but != FS root) -> false ---
    {
      val a = dlogUnknown(); val b = dlogUnknown()
      val prop = Or(List(a, b))
      val ca = commit(a).asInstanceOf[PreDlog]
      val cb = commit(b).asInstanceOf[PreDlog]
      val orE = ca.e ^ cb.e // 31-byte XOR (finding #1)
      val proofJ = s"""{"type":"or","e":"${hexChallenge(orE)}","children":[${proofJson(ca)},${proofJson(cb)}]}"""
      cases += valueCase(
        sigmaExpr(propJson(prop), proofJ, msgHex),
        "false",
        "soundness: OR forged by simulating ALL branches (XOR ok, != FS root) -> false"
      )
    }
    // --- SOUNDNESS: OR matches FS root but breaks XOR -> false ---
    {
      val a = dlogUnknown(); val b = dlogUnknown()
      val prop = Or(List(a, b))
      val ca = commit(a).asInstanceOf[PreDlog]
      val cb = commit(b).asInstanceOf[PreDlog]
      val orNode = PreOr(List(ca, cb), sat = false)
      val root = low31(DomainSep ++ orNode.serializeWithCommitments ++ msg) // 31-byte root (finding #1)
      val proofJ = s"""{"type":"or","e":"${hexChallenge(root)}","children":[${proofJson(ca)},${proofJson(cb)}]}"""
      cases += valueCase(
        sigmaExpr(propJson(prop), proofJ, msgHex),
        "false",
        "soundness: OR matches FS root but breaks XOR relation -> false"
      )
    }
    // --- SOUNDNESS: THRESHOLD 2-of-3 with only k-1=1 witness -> false (finding #2 rebuild) ---
    {
      // Mirrors the GOOD direct unit test (SigmaVerifySuite "THRESHOLD 2-of-3 with only k-1 = 1 real
      // witness"). The EARLIER generator version built the proof children from FRESH unrelated
      // `dlogUnknown()` statements, so the `false` could have been a statement/commitment MISMATCH
      // rather than threshold-interpolation unsoundness. Here every child is proven AGAINST THE
      // PROPOSITION'S ACTUAL pubkeys: the one known child carries a REAL transcript and the other two
      // are HVZK-simulated on the SAME pks, isolating the discriminator to the CDS interpolation.
      val k = 2 // n = 3 (2-of-3)
      val xKnown = randScalar()
      val knownLeaf = dlogKnown(xKnown) // index 0: the one real witness
      val sim1 = dlogUnknown(); val sim2 = dlogUnknown() // indices 1,2: pks whose witness is unknown
      val prop = Threshold(k, List(knownLeaf, sim1, sim2))

      // Child 0: REAL — nonce r0, commit a = r0·G, defer the response.
      val r0 = randScalar()
      val c0 = PreDlog(knownLeaf.pk, sat = true, knownLeaf.x, r0, g1.multiply(r0.bigInteger))
      // Children 1,2: HVZK-SIMULATED on the proposition's REAL pks with FREE challenges.
      val c1 = simulateForced(sim1, randChallenge()).asInstanceOf[PreDlog]
      val c2 = simulateForced(sim2, randChallenge()).asInstanceOf[PreDlog]

      // Use the REAL FS root as the threshold node challenge (so step-6's root check would PASS),
      // isolating the failure to interpolation. degree = n-k = 1, so a line is fixed by (0, root) +
      // the FIRST simulated point; the SECOND simulated point + the real child generically do NOT
      // both lie on it, so the per-lane interpolation check rejects.
      val thrNode = PreThr(k, List(c0, c1, c2), sat = false)
      val root = low31(DomainSep ++ thrNode.serializeWithCommitments ++ msg) // 31-byte root (finding #1)
      // Derive child 0's challenge as P(1) for the line through (0, root) + sim child 1 (x=2), so c0's
      // own transcript is self-consistent; the verifier still rejects on the over-determined sim 2.
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
      cases += valueCase(
        sigmaExpr(propJson(prop), proofJ, msgHex),
        "false",
        "soundness: THRESHOLD 2-of-3 with only k-1 real witness (rest HVZK-simulated on real pks) -> false"
      )
    }
    // --- SOUNDNESS: wrong message -> false ---
    {
      val prop = And(List(dlogKnown(randScalar()), dlogKnown(randScalar())))
      val (pj, prf) = prove(prop, msg)
      val otherMsg = HexBytes.encodeBytes("a DIFFERENT message".getBytes("UTF-8"))
      cases += valueCase(sigmaExpr(pj, prf, otherMsg), "false", "soundness: wrong message (strong-FS binds msg) -> false")
    }
    // --- SOUNDNESS: tampered response z on a valid AND -> false ---
    {
      val prop = And(List(dlogKnown(randScalar()), dhKnown(BigInt(17), BigInt(2), BigInt(5))))
      val (pj, prf) = prove(prop, msg)
      val tampered = {
        val i = prf.indexOf("\"z\":\"0x")
        val zStart = i + 6
        val zEnd = prf.indexOf("\"", zStart + 2)
        val orig = prf.substring(zStart, zEnd)
        val flipped = orig.dropRight(1) + (if (orig.last == '0') '1' else '0')
        prf.substring(0, zStart) + flipped + prf.substring(zEnd)
      }
      cases += valueCase(sigmaExpr(pj, tampered, msgHex), "false", "soundness: tampered response z on a valid AND -> false")
    }
    // --- false: identity dlog pk (forgery vector) -> false (not error) ---
    {
      val identity = "0x" + "0" * 128
      val prop = s"""{"type":"dlog","pk":"$identity"}"""
      val proof = s"""{"type":"dlog","e":"${hexChallenge(BigInt(123))}","z":"${hex32(BigInt(456))}"}"""
      cases += valueCase(sigmaExpr(prop, proof, msgHex), "false", "identity dlog pk universal-forgery vector -> false (not error)")
    }

    // --- ERROR: non-canonical leaf response z = z + R (>= R) on an otherwise-valid leaf (finding #4) ---
    {
      // Build a genuinely-valid single dlog leaf (real witness, real root-derived challenge), then
      // swap ONLY the leaf response z for z + R. The challenge stays the valid 31-byte root, so the
      // ONLY defect is the non-canonical 32-byte response -> requireCanonicalScalar HARD-errors
      // (it never reaches the FS check). Pins the canonical-response rule for the sigma tree leaf.
      val prop = dlogKnown(BigInt("12345678901234567890"))
      val pp = commit(prop)
      val rootChallenge = low31(DomainSep ++ pp.serializeWithCommitments ++ msg) // 31-byte root (finding #1)
      setChallenge(pp, rootChallenge)
      val d = pp.asInstanceOf[PreDlog]
      val proofJ = s"""{"type":"dlog","e":"${hexChallenge(d.e)}","z":"${hex32NonCanonical(d.z)}"}"""
      cases += errorCase(sigmaExpr(propJson(prop), proofJ, msgHex), "non-canonical leaf response z = z + R (>= R) -> error (finding #4)")
    }

    // --- ERROR: off-curve statement point ---
    {
      val offCurve = HexBytes.encodeG1(BigInt(1), BigInt(1)).toOption.get
      val prop = s"""{"type":"dlog","pk":"$offCurve"}"""
      val proof = s"""{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(2))}"}"""
      cases += errorCase(sigmaExpr(prop, proof, msgHex), "off-curve statement point -> error")
    }
    // --- ERROR: tiny proposition + HUGE mismatched proof -> hard error (finding #2 DoS bound) ---
    {
      // Mirrors the direct DoS test (SigmaVerifySuite "a TINY proposition with a HUGE mismatched
      // proof is rejected fast"). The proposition is ONE dlog leaf; the proof is a wide OR of
      // thousands of children — far exceeding the proposition's node count. boundProofShape rejects
      // it (hard error) BEFORE any hex/curve work, having walked only O(maxNodes) of the proof. Pins
      // the structural DoS bound cross-language (the gas DoS bound is pinned by the gas vectors).
      val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
      val child = s"""{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}"""
      val hugeProof = s"""{"type":"or","e":"${hexChallenge(BigInt(1))}","children":[${List.fill(5000)(child).mkString(",")}]}"""
      cases += errorCase(sigmaExpr(prop, hugeProof, msgHex), "tiny proposition + huge mismatched proof -> error (DoS bound, finding #2)")
    }
    // --- ERROR: unknown node type ---
    {
      val prop = s"""{"type":"xor","children":[{"type":"dlog","pk":"${encG1(g1)}"}]}"""
      val proof = s"""{"type":"xor","e":"${hexChallenge(BigInt(1))}","children":[{"type":"dlog","e":"${hexChallenge(
          BigInt(1)
        )}","z":"${hex32(
          BigInt(1)
        )}"}]}"""
      cases += errorCase(sigmaExpr(prop, proof, msgHex), "unknown node type -> error")
    }
    // --- ERROR: threshold k > n ---
    {
      val prop = s"""{"type":"threshold","k":5,"children":[{"type":"dlog","pk":"${encG1(g1)}"},{"type":"dlog","pk":"${encG1(g1)}"}]}"""
      val proof = s"""{"type":"threshold","e":"${hexChallenge(BigInt(1))}","k":5,"children":[{"type":"dlog","e":"${hexChallenge(
          BigInt(1)
        )}","z":"${hex32(
          BigInt(1)
        )}"},{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}]}"""
      cases += errorCase(sigmaExpr(prop, proof, msgHex), "threshold k > n -> error")
    }
    // --- ERROR: prop/proof shape mismatch (dlog vs dhtuple) ---
    {
      val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
      val proof = s"""{"type":"dhtuple","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}"""
      cases += errorCase(sigmaExpr(prop, proof, msgHex), "proposition/proof shape mismatch -> error")
    }
    // --- ERROR: child-count mismatch ---
    {
      val prop = s"""{"type":"and","children":[{"type":"dlog","pk":"${encG1(g1)}"},{"type":"dlog","pk":"${encG1(g1)}"}]}"""
      val proof = s"""{"type":"and","e":"${hexChallenge(BigInt(1))}","children":[{"type":"dlog","e":"${hexChallenge(
          BigInt(1)
        )}","z":"${hex32(
          BigInt(1)
        )}"}]}"""
      cases += errorCase(sigmaExpr(prop, proof, msgHex), "proposition/proof child-count mismatch -> error")
    }
    // --- ERROR: wrong-width challenge in proof ---
    {
      val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
      val proof = s"""{"type":"dlog","e":"0xdead","z":"${hex32(BigInt(1))}"}"""
      cases += errorCase(sigmaExpr(prop, proof, msgHex), "wrong-width challenge in proof -> error")
    }
    // --- ERROR: malformed message hex ---
    {
      val prop = s"""{"type":"dlog","pk":"${encG1(g1)}"}"""
      val proof = s"""{"type":"dlog","e":"${hexChallenge(BigInt(1))}","z":"${hex32(BigInt(1))}"}"""
      cases += errorCase(sigmaExpr(prop, proof, "0xZZ"), "malformed message hex -> error")
    }

    cases.toList
  }

  // ===========================================================================
  // The generator test: emit + self-check.
  // ===========================================================================

  // SERIALIZATION KATs: for a VALID proof, the verifier's `serializeTree` (over
  // RECONSTRUCTED commitments) equals the prover's `serializeWithCommitments`
  // (over the SAME commitments). Emit (proposition, proof, message, serializedHex)
  // for a few fixed trees so the Rust side can pin its `verify_node` output bytes
  // DIRECTLY against the Scala byte layout (the frozen-serialization contract,
  // independent of the true/false outcome). This is the docs/sigma-verify.md §4
  // "serialization-only vector set".
  private def emitKats(): Unit = {
    // Fixed-witness trees so the bytes are reproducible.
    val katProps: List[(String, Prop)] = List(
      "dlog_leaf"        -> dlogKnown(BigInt("12345678901234567890")),
      "dhtuple_leaf"     -> dhKnown(BigInt("999888777666555"), BigInt(3), BigInt(5)),
      "and_dlog_dhtuple" -> And(List(dlogKnown(BigInt(7)), dhKnown(BigInt(11), BigInt(2), BigInt(9)))),
      "threshold_2of3"   -> Threshold(2, List(dlogKnown(BigInt(101)), dlogKnown(BigInt(202)), dlogKnown(BigInt(303))))
    )
    val entries = katProps.map {
      case (name, prop) =>
        // commit deterministically (seeded RNG), hash the root, fill responses.
        val pp = commit(prop)
        val rootChallenge = low31(DomainSep ++ pp.serializeWithCommitments ++ msg) // 31-byte root (finding #1)
        setChallenge(pp, rootChallenge)
        val serializedHex = HexBytes.encodeBytes(pp.serializeWithCommitments)
        // Sanity: the emitted (prop, proof) must verify true under the REAL opcode.
        val v = evalReal(sigmaExpr(propJson(prop), proofJson(pp), msgHex))
        require(v == Right("true"), s"KAT $name expected true, got $v")
        s"""  {
           |    "name": ${jsonStr(name)},
           |    "proposition": ${jsonStr(propJson(prop))},
           |    "proof": ${jsonStr(proofJson(pp))},
           |    "messageHex": ${jsonStr(msgHex)},
           |    "serializedHex": ${jsonStr(serializedHex)}
           |  }""".stripMargin
    }
    val _ = Files.write(Paths.get(KatPath), ("[\n" + entries.mkString(",\n") + "\n]\n").getBytes(StandardCharsets.UTF_8))
  }

  // ONE consolidated test. The KAT + vector passes share the single `rng`, so
  // they MUST run sequentially in a fixed order (not as two concurrent weaver
  // tests, which would interleave the shared RNG and de-stabilise the bytes).
  // `reseed()` at the top then KATs then vectors gives a fully deterministic,
  // reproducible byte stream.
  test("generate sigma opcode vectors + serialization KATs (deterministic, self-checked)") {
    IO {
      reseed()
      emitKats()

      val dlog = dlogCases()
      val dhtuple = dhtupleCases()
      val sigma = sigmaCases()

      val all = dlog ++ dhtuple ++ sigma
      val failures = all.collect { case (_, false, msg) => msg }

      val json =
        s"""[
           |${renderCategory("sigma_dlog", dlog.map(_._1))},
           |${renderCategory("sigma_dhtuple", dhtuple.map(_._1))},
           |${renderCategory("sigma", sigma.map(_._1))}
           |]
           |""".stripMargin

      Files.write(Paths.get(OutPath), json.getBytes(StandardCharsets.UTF_8))

      expect(
        failures.isEmpty,
        s"${failures.length} generated case(s) did not match the reference verifier:\n" + failures.mkString("\n")
      )
    }
  }
}
