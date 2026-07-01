package json_logic

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.security.{MessageDigest, SecureRandom}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.zk.Bn254
import io.constellationnetwork.metagraph_sdk.json_logic.core.{JsonLogicExpression, JsonLogicValue}
import io.constellationnetwork.metagraph_sdk.json_logic.gas.{GasConfig, GasExhaustedException, GasLimit}
import io.constellationnetwork.metagraph_sdk.json_logic.ops.{CryptoOps, HexBytes}
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.syntax._
import io.circe.{Json, JsonObject, Printer, parser}

/**
 * Generator for the cross-language GAS conformance vectors
 * (`src/test/resources/conformance/gas_test_vectors.json`).
 *
 * Every `expected` value in the vector file is PRODUCED BY RUNNING the Scala
 * gas meter (`JsonLogicEvaluator.evaluateWithGas` with `GasConfig.Default`) —
 * never hand-computed. Re-run after any change to the charging contract:
 *
 *   sbt "Test/runMain json_logic.GasVectorGenerator"
 *
 * Each case is evaluated twice:
 *   - once with `GasLimit.Unlimited` to measure the true gas consumption
 *     (used to resolve the relative limit specs `Exact` / `ExactMinus`), and
 *   - once with the declared `gasLimit`, producing `expected`: the exact
 *     `gasUsed` integer on success, or the string "OOG" when the meter raises
 *     `GasExhaustedException`.
 *
 * Any other evaluation failure aborts generation: gas vectors must only
 * contain programs that either succeed or run out of gas.
 */
object GasVectorGenerator extends IOApp {

  private val OutPath = "src/test/resources/conformance/gas_test_vectors.json"

  sealed private trait LimitSpec
  private case class Abs(amount: Long) extends LimitSpec

  /** Resolved to the measured (unlimited) gas consumption: an exactly-at-limit success. */
  private case object Exact extends LimitSpec

  /** Resolved to measured - n: an out-of-gas case n short of the requirement. */
  private case class ExactMinus(n: Long) extends LimitSpec

  final private case class CaseSpec(
    expr: String,
    data: String,
    limit: LimitSpec,
    note: Option[String] = None
  )

  final private case class CategorySpec(category: String, note: Option[String], cases: List[CaseSpec])

  // --- compact CDS prover for the sigma_verify gas fixtures (31-byte challenges, finding #1) -----
  // A condensed copy of the SigmaVerifySuite / SigmaVectorGen prover, emitting a full
  // {"sigma_verify":[prop, proof, msg]} expression for a proposition the prover can satisfy. Fixed
  // seed -> deterministic, reproducible fixtures. Challenges are the 31-byte injective domain.
  private object SigmaProver {
    private val R: BigInt = BigInt(Bn254.R)
    private val g1: Bn254.G1 = Bn254.G1(BigInteger.ONE, BigInteger.valueOf(2))
    private val ChallengeBytes: Int = 31
    private val rng: SecureRandom = {
      val r = SecureRandom.getInstance("SHA1PRNG"); r.setSeed(0x6761_5347_4153L); r
    }
    def randScalar(): BigInt = BigInt(1, { val b = new Array[Byte](32); rng.nextBytes(b); b }).mod(R)
    private def randChallenge(): BigInt = BigInt(1, { val b = new Array[Byte](ChallengeBytes); rng.nextBytes(b); b })
    private def encG1(p: Bn254.G1): String = HexBytes.encodeG1(BigInt(p.x), BigInt(p.y)).toOption.get
    private def g1Bytes(p: Bn254.G1): Array[Byte] = HexBytes.parseBytes(encG1(p), Some(64), "g1").toOption.get
    private def hex32(v: BigInt): String = HexBytes.encodeUInt(v.mod(R), 32).toOption.get
    private def hexChallenge(v: BigInt): String = HexBytes.encodeUInt(v, ChallengeBytes).toOption.get
    private def sha256(b: Array[Byte]): Array[Byte] = MessageDigest.getInstance("SHA-256").digest(b)
    private def low31(b: Array[Byte]): BigInt = BigInt(1, sha256(b).takeRight(ChallengeBytes))
    private def challengeBytes(e: BigInt): Array[Byte] = HexBytes.parseBytes(hexChallenge(e), Some(ChallengeBytes), "e").toOption.get
    private val TagDlog: Byte = 0x00; private val TagDhTuple: Byte = 0x01
    private val TagAnd: Byte = 0x02; private val TagOr: Byte = 0x03; private val TagThreshold: Byte = 0x04
    private val DomainSep: Array[Byte] = "sigma_verify:v1".getBytes("US-ASCII")
    private def uint32(v: Int): Array[Byte] = Array((v >>> 24).toByte, (v >>> 16).toByte, (v >>> 8).toByte, v.toByte)
    private val msg: Array[Byte] = "authorize sigma".getBytes("UTF-8")
    private val msgHex: String = HexBytes.encodeBytes(msg)

    sealed trait Prop
    final case class Dlog(x: Option[BigInt], pk: Bn254.G1) extends Prop
    final case class DhTuple(w: Option[BigInt], g: Bn254.G1, h: Bn254.G1, u: Bn254.G1, v: Bn254.G1) extends Prop
    final case class And(children: List[Prop]) extends Prop
    final case class Or(children: List[Prop]) extends Prop
    final case class Threshold(k: Int, children: List[Prop]) extends Prop

    def dlogKnown(x: BigInt): Dlog = Dlog(Some(x.mod(R)), g1.multiply(x.bigInteger))
    def dlogUnknown(): Dlog = { val x = randScalar(); Dlog(None, g1.multiply(x.bigInteger)) }
    def dhKnown(w: BigInt, gS: BigInt, hS: BigInt): DhTuple = {
      val g = g1.multiply(gS.bigInteger); val h = g1.multiply(hS.bigInteger)
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

    sealed trait Pre { var e: BigInt = BigInt(-1); def sat: Boolean; def ser: Array[Byte] }
    final case class PreDlog(pk: Bn254.G1, sat: Boolean, w: Option[BigInt], r: BigInt, a: Bn254.G1) extends Pre {
      var z: BigInt = BigInt(-1); def ser: Array[Byte] = Array(TagDlog) ++ g1Bytes(pk) ++ g1Bytes(a)
    }
    final case class PreDh(
      g: Bn254.G1,
      h: Bn254.G1,
      u: Bn254.G1,
      v: Bn254.G1,
      sat: Boolean,
      w: Option[BigInt],
      r: BigInt,
      a1: Bn254.G1,
      a2: Bn254.G1
    ) extends Pre {
      var z: BigInt = BigInt(-1)
      def ser: Array[Byte] = Array(TagDhTuple) ++ g1Bytes(g) ++ g1Bytes(h) ++ g1Bytes(u) ++ g1Bytes(v) ++ g1Bytes(a1) ++ g1Bytes(a2)
    }
    final case class PreAnd(children: List[Pre], sat: Boolean) extends Pre {
      def ser: Array[Byte] = Array(TagAnd) ++ uint32(children.length) ++ children.flatMap(_.ser).toArray
    }
    final case class PreOr(children: List[Pre], sat: Boolean) extends Pre {
      def ser: Array[Byte] = Array(TagOr) ++ uint32(children.length) ++ children.flatMap(_.ser).toArray
    }
    final case class PreThr(k: Int, children: List[Pre], sat: Boolean) extends Pre {
      def ser: Array[Byte] = Array(TagThreshold) ++ uint32(k) ++ uint32(children.length) ++ children.flatMap(_.ser).toArray
    }

    private def commit(p: Prop, mustSimulate: Boolean = false): Pre =
      if (mustSimulate || !satisfiable(p)) simulateForced(p, randChallenge())
      else
        p match {
          case Dlog(xOpt, pk) => val r = randScalar(); PreDlog(pk, sat = true, xOpt, r, g1.multiply(r.bigInteger))
          case DhTuple(wOpt, g, h, u, v) =>
            val r = randScalar(); PreDh(g, h, u, v, sat = true, wOpt, r, g.multiply(r.bigInteger), h.multiply(r.bigInteger))
          case And(cs) => PreAnd(cs.map(c => commit(c)), sat = true)
          case Or(cs) =>
            val ri = cs.indexWhere(satisfiable); PreOr(cs.zipWithIndex.map { case (c, i) => commit(c, i != ri) }, sat = true)
          case Threshold(k, cs) =>
            val ris = cs.zipWithIndex.collect { case (c, i) if satisfiable(c) => i }.take(k).toSet
            PreThr(k, cs.zipWithIndex.map { case (c, i) => commit(c, !ris.contains(i)) }, sat = true)
        }
    private def simulateForced(p: Prop, e: BigInt): Pre = p match {
      case Dlog(_, pk) =>
        val z = randScalar(); val n = PreDlog(pk, sat = false, None, BigInt(-1), CryptoOps.dlogComputeCommitment(pk, e, z)); n.e = e;
        n.z = z; n
      case DhTuple(_, g, h, u, v) =>
        val z = randScalar()
        val n = PreDh(
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
        n.e = e; n.z = z; n
      case And(cs) => val n = PreAnd(cs.map(c => simulateForced(c, e)), sat = false); n.e = e; n
      case Or(cs) =>
        val frees = cs.dropRight(1).map(c => simulateForced(c, randChallenge()))
        val lastE = frees.foldLeft(e)((acc, c) => acc ^ c.e)
        val n = PreOr(frees :+ simulateForced(cs.last, lastE), sat = false); n.e = e; n
      case Threshold(k, cs) =>
        val n0 = cs.length; val degree = n0 - k; val freeIdxs = (0 until degree).toSet
        val freeChildren = freeIdxs.map(i => i -> simulateForced(cs(i), randChallenge())).toMap
        val xs = (0 :: freeIdxs.toList.sorted.map(_ + 1)).toArray
        val children = (0 until n0).toList.map { i =>
          if (freeIdxs.contains(i)) freeChildren(i)
          else {
            val forced = BigInt(
              1,
              Array.tabulate(ChallengeBytes) { lane =>
                val ys = (0 :: freeIdxs.toList.sorted).zipWithIndex.map {
                  case (fi, pos) => if (pos == 0) challengeBytes(e)(lane) & 0xff else challengeBytes(freeChildren(fi).e)(lane) & 0xff
                }.toArray
                gfLagrange(xs, ys, i + 1).toByte
              }
            )
            simulateForced(cs(i), forced)
          }
        }
        val n = PreThr(k, children, sat = false); n.e = e; n
    }
    private def gfMul(a0: Int, b0: Int): Int = {
      val (p, _, _) = (0 until 8).foldLeft((0, a0 & 0xff, b0 & 0xff)) {
        case ((prod, a, b), _) =>
          val np = if ((b & 1) != 0) prod ^ a else prod; val sh = (a << 1) & 0xff
          (np, if ((a & 0x80) != 0) sh ^ 0x1b else sh, b >> 1)
      }
      p & 0xff
    }
    private def gfInv(a: Int): Int =
      if ((a & 0xff) == 0) 0
      else
        (0 until 8)
          .foldLeft((1, a & 0xff)) {
            case ((acc, base), bit) => (if (((254 >> bit) & 1) != 0) gfMul(acc, base) else acc, gfMul(base, base))
          }
          ._1 & 0xff
    private def gfLagrange(xs: Array[Int], ys: Array[Int], xEval: Int): Int =
      xs.indices.foldLeft(0) { (acc, i) =>
        val (num, den) = xs.indices.foldLeft((1, 1)) {
          case ((nm, dn), j) => if (j == i) (nm, dn) else (gfMul(nm, xEval ^ xs(j)), gfMul(dn, xs(i) ^ xs(j)))
        }
        acc ^ gfMul(ys(i), gfMul(num, gfInv(den)))
      } & 0xff
    private def setChallenge(pp: Pre, e: BigInt): Unit =
      if (!pp.sat) require(pp.e == e, s"simulated subtree challenge mismatch: ${pp.e} vs $e")
      else
        pp match {
          case d: PreDlog => d.e = e; d.z = (d.r + e * d.w.get).mod(R)
          case d: PreDh   => d.e = e; d.z = (d.r + e * d.w.get).mod(R)
          case a: PreAnd  => a.e = e; a.children.foreach(c => setChallenge(c, e))
          case o: PreOr =>
            o.e = e; val ri = o.children.indexWhere(_.sat)
            val xorSim = o.children.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (c, i)) => if (i == ri) acc else acc ^ c.e }
            o.children.zipWithIndex.foreach { case (c, i) => setChallenge(c, if (i == ri) e ^ xorSim else c.e) }
          case t: PreThr =>
            t.e = e; val n0 = t.children.length; val degree = n0 - t.k
            val ris = t.children.zipWithIndex.collect { case (c, i) if c.sat => i }.toSet
            val simIdxs = (0 until n0).filterNot(ris.contains).toList
            require(simIdxs.length == degree, s"threshold prover: expected $degree simulated, got ${simIdxs.length}")
            val xs = (0 :: simIdxs.map(_ + 1)).toArray
            val realE = ris.map { ri =>
              val bytes = Array.tabulate(ChallengeBytes) { lane =>
                val ys = (0 :: simIdxs).zipWithIndex.map {
                  case (si, pos) => if (pos == 0) challengeBytes(e)(lane) & 0xff else challengeBytes(t.children(si).e)(lane) & 0xff
                }.toArray
                gfLagrange(xs, ys, ri + 1).toByte
              }
              ri -> BigInt(1, bytes)
            }.toMap
            t.children.zipWithIndex.foreach { case (c, i) => setChallenge(c, if (ris.contains(i)) realE(i) else c.e) }
        }
    private def proofJson(pp: Pre): String = pp match {
      case d: PreDlog => s"""{"type":"dlog","e":"${hexChallenge(d.e)}","z":"${hex32(d.z)}"}"""
      case d: PreDh   => s"""{"type":"dhtuple","e":"${hexChallenge(d.e)}","z":"${hex32(d.z)}"}"""
      case a: PreAnd  => s"""{"type":"and","e":"${hexChallenge(a.e)}","children":[${a.children.map(proofJson).mkString(",")}]}"""
      case o: PreOr   => s"""{"type":"or","e":"${hexChallenge(o.e)}","children":[${o.children.map(proofJson).mkString(",")}]}"""
      case t: PreThr =>
        s"""{"type":"threshold","e":"${hexChallenge(t.e)}","k":${t.k},"children":[${t.children.map(proofJson).mkString(",")}]}"""
    }
    def sigmaExpr(prop: Prop): String = {
      val pp = commit(prop)
      val root = low31(DomainSep ++ pp.ser ++ msg)
      setChallenge(pp, root)
      s"""{"sigma_verify":[${propJson(prop)},${proofJson(pp)},"$msgHex"]}"""
    }
  }

  // --- crypto fixtures (verified-true cases lifted from the shared ZK opcode vectors v1.8.0) ----

  private val PoseidonTwoInputs =
    "{\"poseidon\":[\"0x0000000000000000000000000000000000000000000000000000000000000001\",\"0x0000000000000000000000000000000000000000000000000000000000000002\"]}"

  private val PoseidonFourInputs =
    "{\"poseidon\":[\"0x0000000000000000000000000000000000000000000000000000000000000004\",\"0x0000000000000000000000000000000000000000000000000000000000000005\",\"0x0000000000000000000000000000000000000000000000000000000000000006\",\"0x0000000000000000000000000000000000000000000000000000000000000007\"]}"

  private val PmtVerifyEightSiblings =
    "{\"pmt_verify\":[\"0x047e3f50a0bf1da6c86860a77474b1f6ee1a807660c2556f34e046f34155f54f\",\"0x0cc5c2f21d3b979fa5284982d35ceccb66333b93d325333dd780a6a3ced1c5f5\",42,[\"0x166f24e25f67126bbff81d9c8f064c913d2127f180a906dfed933ecf251a56bc\",\"0x2dee93c5a666459646ea7d22cca9e1bcfed71e6951b953611d11dda32ea09d78\",\"0x1445dc1092ecebc0a6001d45b9bd4d85705c66e435abc23436da8d39a6f37d08\",\"0x07f9d837cb17b0d36320ffe93ba52345f1b728571a568265caac97559dbc952a\",\"0x18f43331537ee2af2e3d758d50f72106467c6eea50371dd528d57eb2b856d238\",\"0x1069673dcdb12263df301a6ff584a7ec261a44cb9dc68df067a4774460b1f1e1\",\"0x2098f5fb9e239eab3ceac3f27b81e481dc3124d55ffed523a839ee8446b64864\",\"0x0000000000000000000000000000000000000000000000000000000000000000\"]]}"

  private val SchnorrVerifyValid =
    "{\"schnorr_verify\":[\"0x234403317325635150f68fd4b3403ae81e7513abee02b83478fb37c7ae1b47db0bf44b96107a81ab3b5192e1f2f920eb4fe6a382d6df663e8af2ae97cf8e3ae5\",\"0x617574686f72697a65207472616e73666572\",\"0x18186954bd891097c44985153dfbbee526e9bfa9798037c42e0e23ca023d5c942a9cb437b23ce7ff0d0a1dccabd0ec7619d7d045b3134a98f7d7eb34b3fd9a470ba9e88831ee2663248a9087524a7034a9eef889e7d912f2cfe8798045071663\"]}"

  private val BlsVerifyValid =
    "{\"bls_verify\":[\"0x864350e49b8b46468478af45cfb9c167357c8701e0c7a2dcd401028de506080f632400e2059ab58532eb2b3912078d80\",\"0x636f6e7374656c6c6174696f6e2d736e617073686f742d30783031\",\"0xa816e2440371eea63b85484f0111914874974cfb8f83833b214ba365bc1bc46cfd070d75c8decb6e9d9bcea0e2a2b92214cfe0bed5c00a7702741a2e92186454f76ba5e4e86804908e7a2f38a0f123941b3513bff5a4af6951c6c7a8e61b04ee\"]}"

  private val BlsAggregateVerifyFourKeys =
    "{\"bls_aggregate_verify\":[[\"0x96512b63cded51762b89ba53811524508ad33a3a990306e5e07097c787ad801dfb160ec6959472b9a188cafcc101f282\",\"0xb0de40cd41c728cd90408081b764b7cc40889dd3dbd499f2f6f771455e1ea799ff859f401db923933ca48a695dc6c3f3\",\"0xb0d93cc62d599b3557eb65a9a08519d9b1a96e5090ac9fadf84ccb6e090a2298edf3a552000dfd0338a37dc86ba65a49\",\"0xaa6230be32948f5f5f746fe050250e3c003d0f00827131381e3e8e8cb2d2bd8fe6d37c10b3433074c6dfa5dee04d1cdd\"],\"0x636f6d6d69747465652d726f756e642d37\",\"0xa3f4674d9b713ca0598e394a19c98e5312eafd2b4e3698b41090651332d507d330d5a9e36aa46f8247ec84e1e0302c1c08bdd8f7944dc7a8daa0cb8c07b6c3837015b6c8533247c1c8876102d9650857c00924f9d7999f4df8a2a30af33c48d4\"]}"

  // --- sigma fixtures (valid proofs lifted verbatim from the shared ZK opcode vectors). The gas
  // charge for these opcodes is pre-charged from the proposition SHAPE alone (and the flat leaf
  // base), BEFORE the verifier runs, so the metered gasUsed is the same for any well-formed proof
  // of the same shape — these happen to verify true. prove_dlog/prove_dhtuple are flat; sigma_verify
  // adds per-DLog-leaf (45000) + per-DHTuple-leaf (85000) + per-node (2000) over the tree. -------

  private val ProveDlogVerify =
    "{\"prove_dlog_verify\":[\"0x234403317325635150f68fd4b3403ae81e7513abee02b83478fb37c7ae1b47db0bf44b96107a81ab3b5192e1f2f920eb4fe6a382d6df663e8af2ae97cf8e3ae5\",\"0x617574686f72697a65207369676d61\",\"0x18186954bd891097c44985153dfbbee526e9bfa9798037c42e0e23ca023d5c942a9cb437b23ce7ff0d0a1dccabd0ec7619d7d045b3134a98f7d7eb34b3fd9a470948e25b19635a71ec3ab59cbcada6cdcab12b0976cc2845a7698bea3ee4373e\"]}"

  private val ProveDhtupleVerify =
    "{\"prove_dhtuple_verify\":[\"0x0769bf9ac56bea3ff40232bcb1b6bd159315d84715b8e679f2d355961915abf02ab799bee0489429554fdb7c8d086475319e63b40b9c5b57cdf1ff3dd9fe2261\",\"0x17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa901e0559bacb160664764a357af8a9fe70baa9258e0b959273ffc5718c6d4cc7c\",\"0x0e1c874e45561967a2255d53d8c6444a0d2f859fbfcc586b27dd7dc337c2e10f11f1d07e4d5bc331552d5f59b999550538f781e8a8df3228b9d53db15d8a693d\",\"0x1717922852aa5a8d41e3c07b1ce90113870477c783f9bae370c746025f65ba812b1bb9988842b379971a902c34b7aee5dda67bd19f4b41b44710a4fe88caf9b7\",\"0x617574686f72697a65207369676d61\",\"0x0722a7de245ee113dddc175f0bf85ead53dd5a43f37645627119583ddf8144200591c67a93b6586889248690c06e241da4babfea7d3c41760c4aa882d5f1587e1b18396c26fde54b88bb2a48f24949ea08ba52784050e5528020ca2a41226c0b18a2738401209982d27d737ea00ff07284abed2bd89d71a72332e7f83f6a4e6e085c3a8870fe4557b7ab0b6105d3677b9e19c56fa7fa4d0f52bd25032256477c\"]}"

  // sigma_verify fixtures are GENERATED by a compact CDS prover (the dual of the verifier) so they
  // carry VALID 31-byte challenges (audit finding #1): the old hardcoded 32-byte-challenge proofs
  // would now be rejected at the width gate, breaking gas measurement. The gas charged depends only
  // on the proposition SHAPE (arg 0), but the proof must still evaluate to Right(_) for the meter to
  // report gasUsed — so we emit genuinely-valid proofs. Deterministic (fixed seed) -> reproducible.
  private val SigmaSingleDlog = SigmaProver.sigmaExpr(SigmaProver.dlogKnown(BigInt("12345678901234567890")))
  private val SigmaSingleDhtuple = SigmaProver.sigmaExpr(SigmaProver.dhKnown(BigInt("999888777666555"), BigInt(3), BigInt(5)))
  private val SigmaAnd =
    SigmaProver.sigmaExpr(SigmaProver.And(List(SigmaProver.dlogKnown(BigInt(7)), SigmaProver.dhKnown(BigInt(11), BigInt(2), BigInt(9)))))
  private val SigmaOr3 =
    SigmaProver.sigmaExpr(
      SigmaProver.Or(List(SigmaProver.dlogUnknown(), SigmaProver.dlogUnknown(), SigmaProver.dlogKnown(SigmaProver.randScalar())))
    )
  private val SigmaThreshold2of3 =
    SigmaProver.sigmaExpr(
      SigmaProver.Threshold(
        2,
        List(SigmaProver.dlogKnown(SigmaProver.randScalar()), SigmaProver.dlogKnown(SigmaProver.randScalar()), SigmaProver.dlogUnknown())
      )
    )
  private val SigmaNested =
    SigmaProver.sigmaExpr(
      SigmaProver.And(
        List(
          SigmaProver.Or(List(SigmaProver.dlogKnown(SigmaProver.randScalar()), SigmaProver.dlogUnknown())),
          SigmaProver.Or(List(SigmaProver.dlogUnknown(), SigmaProver.dlogKnown(SigmaProver.randScalar())))
        )
      )
    )

  // --- the corpus -------------------------------------------------------------------------------

  private val Corpus: List[CategorySpec] = List(
    CategorySpec(
      "constants",
      Some("Literals are never charged: constants, array literals and object literals cost 0 gas."),
      List(
        CaseSpec("42", "{}", Abs(1000)),
        CaseSpec("[1, 2, 3]", "{}", Abs(1000), Some("array literal: elements are constants, no op charge")),
        CaseSpec("{\"a\": 1, \"b\": 2}", "{}", Abs(1000), Some("object literal: no op charge"))
      )
    ),
    CategorySpec(
      "simple_ops",
      Some("Single operation over constant args: base(op) + depthPenalty(1) [+ input-scaled term]."),
      List(
        CaseSpec("{\"!\": [true]}", "{}", Abs(1000)),
        CaseSpec("{\"==\": [1, 1]}", "{}", Abs(1000)),
        CaseSpec(
          "{\"max\": [1, 2, 3]}",
          "{}",
          Abs(1000),
          Some("max over a 3-arg list adds sizeCost(3) on top of base + depth")
        ),
        CaseSpec("{\"typeof\": [42]}", "{}", Abs(1000))
      )
    ),
    CategorySpec(
      "control_flow",
      Some(
        "if/let are evaluated lazily but still charge their flat base cost (ifElse = 10) once per node " +
        "at the dispatch site, with NO depth penalty (depth is undefined at the lazy dispatch site; " +
        "see the GasConfig schedule comment). Condition / bindings / taken branch pay for themselves; " +
        "untaken branches pay nothing."
      ),
      List(
        CaseSpec("{\"if\": [true, 1, 2]}", "{}", Abs(1000), Some("constant condition + constant branch: only the if base cost")),
        CaseSpec(
          "{\"if\": [{\">\": [5, 3]}, {\"+\": [1, 2]}, 99]}",
          "{}",
          Abs(1000),
          Some("condition + taken then-branch are charged; untaken else-branch is not")
        ),
        CaseSpec(
          "{\"let\": [{\"a\": 1}, {\"var\": \"a\"}]}",
          "{}",
          Abs(1000),
          Some("constant binding: let base cost + the var lookup")
        ),
        CaseSpec(
          "{\"let\": [{\"a\": {\"+\": [1, 2]}}, {\"+\": [{\"var\": \"a\"}, 1]}]}",
          "{}",
          Abs(1000),
          Some("let base cost + binding expression + result expression")
        )
      )
    ),
    CategorySpec(
      "arithmetic_depth",
      Some("Nested arithmetic: each op charges once with depthPenalty(5 * height-of-op-over-its-args); no subtree re-charge."),
      List(
        CaseSpec("{\"+\": [1, 2]}", "{}", Abs(1000)),
        CaseSpec("{\"+\": [1, {\"+\": [2, 3]}]}", "{}", Abs(1000)),
        CaseSpec("{\"+\": [{\"+\": [1, {\"+\": [2, 3]}]}, 4]}", "{}", Abs(1000), Some("3-deep chain: depth penalties 5/10/15")),
        CaseSpec("{\"*\": [{\"+\": [1, 2]}, {\"-\": [5, 3]}]}", "{}", Abs(1000), Some("two depth-1 children under a depth-2 parent")),
        CaseSpec("{\"pow\": [2, 10]}", "{}", Abs(1000), Some("pow adds |exponent| as an input-scaled term"))
      )
    ),
    CategorySpec(
      "var_paths",
      Some("Variable lookups charge varAccess(2) + #pathSegments once at lookup time (Java String.split('.') segment count)."),
      List(
        CaseSpec("{\"var\": \"x\"}", "{\"x\": 42}", Abs(1000)),
        CaseSpec("{\"var\": \"a.b.c\"}", "{\"a\": {\"b\": {\"c\": 123}}}", Abs(1000), Some("3 path segments")),
        CaseSpec("{\"var\": \"\"}", "{\"x\": 1}", Abs(1000), Some("whole-data access: empty key still counts 1 segment")),
        CaseSpec(
          "{\"var\": [\"missing\", \"fallback\"]}",
          "{}",
          Abs(1000),
          Some("lookup is charged even when the default is substituted")
        )
      )
    ),
    CategorySpec(
      "collections",
      Some("Collection ops add sizeCost(#elements) up front; per-element callback runs charge their own ops against the same counter."),
      List(
        CaseSpec("{\"map\": [[1, 2, 3], {\"+\": [{\"var\": \"\"}, 1]}]}", "{}", Abs(1000)),
        CaseSpec("{\"filter\": [[1, 2, 3, 4], {\">\": [{\"var\": \"\"}, 2]}]}", "{}", Abs(1000)),
        CaseSpec(
          "{\"reduce\": [[1, 2, 3], {\"+\": [{\"var\": \"current\"}, {\"var\": \"accumulator\"}]}, 0]}",
          "{}",
          Abs(1000)
        ),
        CaseSpec(
          "{\"all\": [[1, 2, 3], {\">\": [{\"var\": \"\"}, 0]}]}",
          "{}",
          Abs(1000),
          Some("all evaluates the predicate for EVERY element (no short-circuit), charging each run")
        ),
        CaseSpec(
          "{\"merge\": [[1, 2], [3, 4]]}",
          "{}",
          Abs(1000),
          Some("merge charges an output-scaled residual: sizeCost(#merged elements) after the primitive")
        )
      )
    ),
    CategorySpec(
      "strings",
      Some("cat/join pre-charge the coerced output length from the inputs; split/substr post-charge an output residual."),
      List(
        CaseSpec("{\"cat\": [\"foo\", \"bar\"]}", "{}", Abs(1000), Some("input-scaled: sum of coerced arg lengths")),
        CaseSpec(
          "{\"join\": [[\"a\", \"b\", \"c\"], \"-\"]}",
          "{}",
          Abs(1000),
          Some("input-scaled: element lengths + separator * (n-1)")
        ),
        CaseSpec("{\"split\": [\"a,b,c\", \",\"]}", "{}", Abs(1000), Some("output residual: 2 * #pieces, charged after the primitive")),
        CaseSpec("{\"substr\": [\"hello world\", 0, 5]}", "{}", Abs(1000), Some("output residual: produced string length"))
      )
    ),
    CategorySpec(
      "crypto",
      Some(
        "ZK / crypto opcodes (fixtures lifted from the shared ZK opcode vectors; all verify successfully). " +
        "Per-element components (poseidon per-input, pmt per-sibling, bls-aggregate per-key) are pre-charged from the args."
      ),
      List(
        CaseSpec(PoseidonTwoInputs, "{}", Abs(1_000_000), Some("poseidon: 150 base + 150 per input (2 inputs)")),
        CaseSpec(PoseidonFourInputs, "{}", Abs(1_000_000), Some("poseidon: 150 base + 150 per input (4 inputs)")),
        CaseSpec(PmtVerifyEightSiblings, "{}", Abs(1_000_000), Some("pmt_verify: 200 base + 300 per sibling (8 siblings)")),
        CaseSpec(SchnorrVerifyValid, "{}", Abs(1_000_000), Some("schnorr_verify: flat 45000")),
        CaseSpec(BlsVerifyValid, "{}", Abs(1_000_000), Some("bls_verify: flat 120000")),
        CaseSpec(
          BlsAggregateVerifyFourKeys,
          "{}",
          Abs(1_000_000),
          Some("bls_aggregate_verify: 120000 base + 15000 per public key (4 keys)")
        )
      )
    ),
    CategorySpec(
      "sigma",
      Some(
        "Σ-protocol opcodes (valid proofs from the ZK opcode vectors). The two fixed-arity leaves " +
        "are flat: prove_dlog_verify = 45000, prove_dhtuple_verify = 85000 (plus depthPenalty(1) = 5). " +
        "sigma_verify is the recursive CDS tree verifier: its cost is PRE-CHARGED from the " +
        "proposition-tree SHAPE (arg 0) alone BEFORE any curve arithmetic — the DoS bound — as " +
        "sigmaVerify base (45000) + sigmaVerifyPerDlogLeaf (45000) * #DLog-leaves + " +
        "sigmaVerifyPerDhtupleLeaf (85000) * #DHTuple-leaves + sigmaVerifyPerNode (2000) * #connective-nodes " +
        "(a connective counts as one node INCLUDING the root), plus depthPenalty(1) = 5."
      ),
      List(
        CaseSpec(ProveDlogVerify, "{}", Abs(1_000_000), Some("prove_dlog_verify: flat 45000 + depth 5")),
        CaseSpec(ProveDhtupleVerify, "{}", Abs(1_000_000), Some("prove_dhtuple_verify: flat 85000 + depth 5")),
        CaseSpec(
          SigmaSingleDlog,
          "{}",
          Abs(1_000_000),
          Some("sigma_verify single DLog leaf: base 45000 + 1*45000 (dlog leaf) + depth 5")
        ),
        CaseSpec(
          SigmaSingleDhtuple,
          "{}",
          Abs(1_000_000),
          Some("sigma_verify single DHTuple leaf: base 45000 + 1*85000 (dhtuple leaf) + depth 5")
        ),
        CaseSpec(
          SigmaAnd,
          "{}",
          Abs(1_000_000),
          Some("sigma_verify AND(dlog,dhtuple): base 45000 + 45000 + 85000 + 1 node*2000 + depth 5")
        ),
        CaseSpec(
          SigmaOr3,
          "{}",
          Abs(1_000_000),
          Some("sigma_verify OR ring n=3: base 45000 + 3*45000 (dlog leaves) + 1 node*2000 + depth 5")
        ),
        CaseSpec(
          SigmaThreshold2of3,
          "{}",
          Abs(1_000_000),
          Some("sigma_verify THRESHOLD 2-of-3: base 45000 + 3*45000 (dlog leaves) + 1 node*2000 + depth 5")
        ),
        CaseSpec(
          SigmaNested,
          "{}",
          Abs(1_000_000),
          Some("sigma_verify nested (A or B) and (C or D): base 45000 + 4*45000 + 3 nodes*2000 + depth 5")
        )
      )
    ),
    CategorySpec(
      "hex_conversion",
      Some(
        "hex_to_int decodes a 0x-prefixed hex string to an unsigned big-endian integer. Flat base " +
        "cost 10 (pinned EQUAL to modulo `%`) + depthPenalty; no input-scaled term (fixed-cost " +
        "decode + fold), so the charge is independent of the hex length."
      ),
      List(
        CaseSpec("{\"hex_to_int\": [\"0xff\"]}", "{}", Abs(1000), Some("single byte")),
        CaseSpec("{\"hex_to_int\": [\"0xdeadbeef\"]}", "{}", Abs(1000), Some("4 bytes: same flat cost as 1 byte")),
        CaseSpec("{\"hex_to_int\": [\"0x\"]}", "{}", Abs(1000), Some("empty hex decodes to 0; same flat cost")),
        CaseSpec(
          "{\"%\": [{\"hex_to_int\": [\"0xff\"]}, 10]}",
          "{}",
          Abs(1000),
          Some("hex_to_int feeding modulo: the nested op adds its own depth penalty")
        )
      )
    ),
    CategorySpec(
      "map_mutation",
      Some(
        "Immutable single-key map write (set) and delete (unset). Flat base cost 5 (pinned EQUAL to " +
        "merge) + depthPenalty; the map, key and value are evaluated args and pay for themselves."
      ),
      List(
        CaseSpec("{\"set\": [{\"a\": 1}, \"b\", 2]}", "{}", Abs(1000), Some("add a new key")),
        CaseSpec("{\"set\": [{\"a\": 1}, \"a\", 9]}", "{}", Abs(1000), Some("replace an existing key (last-wins)")),
        CaseSpec(
          "{\"set\": [{}, {\"var\": \"k\"}, {\"var\": \"v\"}]}",
          "{\"k\": \"b\", \"v\": 2}",
          Abs(1000),
          Some("computed key + value: the two var lookups are charged on top of the set base")
        ),
        CaseSpec("{\"unset\": [{\"a\": 1, \"b\": 2}, \"b\"]}", "{}", Abs(1000), Some("delete a present key")),
        CaseSpec("{\"unset\": [{\"a\": 1}, \"z\"]}", "{}", Abs(1000), Some("delete an absent key: no-op, still charged the base"))
      )
    ),
    CategorySpec(
      "oog",
      Some(
        "Out-of-gas behavior. expected = \"OOG\" asserts the meter fails with the distinct gas-exhaustion error; " +
        "an integer asserts success with that exact gasUsed. NOTE the gas-starved var case: the runtime swallows " +
        "a failed lookup into default/null, so a var lookup that cannot afford its charge yields null and consumes 0."
      ),
      List(
        CaseSpec(
          PoseidonTwoInputs,
          "{}",
          Abs(400),
          Some(
            "size-scaled pre-charge OOG: limit covers poseidon base(150)+depth(5) but not the per-input " +
            "term (300); OOG fires BEFORE the permutation runs"
          )
        ),
        CaseSpec("{\"+\": [1, {\"+\": [2, 3]}]}", "{}", Exact, Some("exactly-at-limit: succeeds with gasUsed == gasLimit")),
        CaseSpec("{\"+\": [1, {\"+\": [2, 3]}]}", "{}", ExactMinus(1), Some("one gas short of the requirement")),
        CaseSpec(
          "{\"+\": [{\"*\": [3, 4]}, {\"*\": [5, 6]}]}",
          "{}",
          Abs(20),
          Some("OOG mid-args: first multiplication fits, the second one's pre-charge does not")
        ),
        CaseSpec(
          "{\"cat\": [\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\", \"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\"]}",
          "{}",
          Abs(50),
          Some("cat input-scaled pre-charge OOG: base(5)+depth(5) fit, the 60-char length term does not")
        ),
        CaseSpec(
          "{\"var\": \"x\"}",
          "{\"x\": 42}",
          Abs(2),
          Some(
            "gas-starved var lookup: the lookup needs 3 (varAccess 2 + 1 segment) but only 2 remain; the " +
            "runtime swallows the failure into null and consumes NOTHING (gasUsed = 0, evaluation succeeds)"
          )
        ),
        CaseSpec(
          "{\"map\": [[1, 2, 3, 4, 5], {\"+\": [{\"var\": \"\"}, 1]}]}",
          "{}",
          Abs(55),
          Some("OOG inside a callback run: the third element's `+` pre-charge exhausts the counter")
        ),
        CaseSpec(
          SigmaSingleDlog,
          "{}",
          ExactMinus(1),
          Some(
            "sigma_verify out-of-gas one short of the requirement: the proposition-shape pre-charge " +
            "(base 45000 + 1 dlog leaf 45000 + depth 5) is consumed atomically BEFORE any curve work, " +
            "so a limit of (exact cost - 1) raises OOG before the verifier runs (finding #3 DoS bound)"
          )
        ),
        CaseSpec(
          SigmaThreshold2of3,
          "{}",
          Abs(GasConfig.Default.sigmaVerify.amount - 1L),
          Some(
            "sigma_verify out-of-gas under a tight limit BELOW even the base cost: a tiny limit can " +
            "never afford the proposition-shape pre-charge, so OOG fires before any traversal / scalar " +
            "mul — the DoS bound holds regardless of proof size (finding #3)"
          )
        )
      )
    )
  )

  // --- generation -------------------------------------------------------------------------------

  private val evaluator = JsonLogicEvaluator.tailRecursive[IO]

  private def parse(c: CaseSpec): IO[(JsonLogicExpression, JsonLogicValue)] =
    IO.fromEither(
      (for {
        exprJson <- parser.parse(c.expr).left.map(e => s"expr parse: $e")
        expr     <- exprJson.as[JsonLogicExpression].left.map(e => s"expr decode: $e")
        dataJson <- parser.parse(c.data).left.map(e => s"data parse: $e")
        data     <- dataJson.as[JsonLogicValue].left.map(e => s"data decode: $e")
      } yield (expr, data)).left.map(msg => new RuntimeException(s"${c.expr}: $msg"))
    )

  private def runCase(category: String, c: CaseSpec): IO[Json] =
    for {
      parsed <- parse(c)
      (expr, data) = parsed
      measuredRes <- evaluator.evaluateWithGas(expr, data, None, GasLimit.Unlimited, GasConfig.Default)
      measured <- measuredRes match {
        case Right(r) => IO.pure(r.gasUsed.amount)
        case Left(err) =>
          IO.raiseError(new RuntimeException(s"[$category] ${c.expr}: unlimited run failed: ${err.getMessage}"))
      }
      limit = c.limit match {
        case Abs(n)        => n
        case Exact         => measured
        case ExactMinus(n) => measured - n
      }
      limitedRes <- evaluator.evaluateWithGas(expr, data, None, GasLimit(limit), GasConfig.Default)
      expected <- limitedRes match {
        case Right(r)                       => IO.pure(Json.fromLong(r.gasUsed.amount))
        case Left(_: GasExhaustedException) => IO.pure(Json.fromString("OOG"))
        case Left(err) =>
          IO.raiseError(new RuntimeException(s"[$category] ${c.expr}: limited run failed NON-gas: ${err.getMessage}"))
      }
      _ <- IO.println(f"[$category%-16s] measured=$measured%8d limit=$limit%8d expected=${expected.noSpaces}%8s  ${c.expr.take(60)}")
    } yield
      Json.fromJsonObject(
        JsonObject.fromIterable(
          List("expr" := c.expr, "data" := c.data, "gasLimit" := limit, "expected" -> expected) ++
          c.note.map(n => "note" := n).toList
        )
      )

  private def categoryJson(spec: CategorySpec): IO[Json] =
    spec.cases.traverse(runCase(spec.category, _)).map { cases =>
      Json.fromJsonObject(
        JsonObject.fromIterable(
          List("category" := spec.category) ++
          spec.note.map(n => "note" := n).toList ++
          List("cases" := Json.fromValues(cases))
        )
      )
    }

  override def run(args: List[String]): IO[ExitCode] =
    for {
      categories <- Corpus.traverse(categoryJson)
      doc = Json.fromJsonObject(
        JsonObject.fromIterable(
          List(
            "description" := (
              "JLVM gas-metering cross-language test vectors. Scala (metakit) is the reference gas meter; " +
              "every implementation must reproduce `expected` EXACTLY: the integer gasUsed reported when " +
              "evaluating `expr` against `data` under `gasLimit` with the default gas schedule, or the " +
              "string \"OOG\" when metering must fail with the distinct gas-exhaustion error. The charging " +
              "contract is normative per metakit PR #37: each op consumes exactly once " +
              "base(op) + depthPenalty + inputScaledCost atomically BEFORE the primitive runs, plus an " +
              "output-scaled residual after it for split/merge/flatten/slice/substr only; var lookups " +
              "consume varAccess + #pathSegments at lookup; the lazily-dispatched if/let charge their " +
              "flat base cost once per node at the dispatch site with NO depth penalty (untaken branches " +
              "cost nothing); gasUsed is the gas-counter delta. Generated by " +
              "`sbt \"Test/runMain json_logic.GasVectorGenerator\"` — " +
              "expected values are PRODUCED BY RUNNING the Scala meter, never hand-computed."
            ),
            "version" := "1.2.0",
            "tests" := Json.fromValues(categories)
          )
        )
      )
      rendered = Printer.spaces2.copy(colonLeft = " ").print(doc) + "\n"
      _ <- IO(Files.write(Paths.get(OutPath), rendered.getBytes(StandardCharsets.UTF_8)))
      _ <- IO.println(s"\nWrote $OutPath")
    } yield ExitCode.Success
}
