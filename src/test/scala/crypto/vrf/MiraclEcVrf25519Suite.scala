package crypto.vrf

import java.util.HexFormat

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.vrf.MiraclEcVrf25519

import io.circe.generic.auto._
import io.circe.parser
import weaver.SimpleIOSuite

/**
 * Byte-exact conformance for the MIRACL-based ECVRF-EDWARDS25519-SHA512-TAI
 * (RFC 9381, suite 0x03). The vectors are the OFFICIAL RFC 9381 Appendix B.3
 * test vectors (Examples 16, 17, 18), so a passing run proves the MIRACL
 * implementation generates verification keys, proofs (pi) and outputs (beta)
 * that match the published RFC 9381 standard byte-for-byte -- in particular the
 * 5-point ECVRF_challenge_generation (§5.4.3) with the public key Y hashed first.
 */
object MiraclEcVrf25519Suite extends SimpleIOSuite {

  private val hex = HexFormat.of()
  private val vrf = new MiraclEcVrf25519()

  final case class TestInputs(secretKey: String, message: String)
  final case class TestOutputs(verificationKey: String, pi: String, beta: String)
  final case class TestVector(description: String, inputs: TestInputs, outputs: TestOutputs)

  private val vectors: List[TestVector] = {
    val stream = getClass.getResourceAsStream("/vrf/VrfEd25519.json")
    val json =
      try scala.io.Source.fromInputStream(stream).mkString
      finally stream.close()
    parser.decode[List[TestVector]](json).getOrElse(throw new RuntimeException("Failed to parse VRF test vectors"))
  }

  private def msgBytes(m: String): Array[Byte] =
    if (m.isEmpty) Array.emptyByteArray else hex.parseHex(m)

  vectors.foreach { vector =>
    test(s"VRF - ${vector.description} - derive verification key (byte-exact)") {
      IO {
        val sk = hex.parseHex(vector.inputs.secretKey)
        val expectedVk = hex.parseHex(vector.outputs.verificationKey)
        expect(java.util.Arrays.equals(vrf.getVerificationKey(sk), expectedVk))
      }
    }

    test(s"VRF - ${vector.description} - generate proof pi (byte-exact)") {
      IO {
        val sk = hex.parseHex(vector.inputs.secretKey)
        val expectedPi = hex.parseHex(vector.outputs.pi)
        expect(java.util.Arrays.equals(vrf.vrfProof(sk, msgBytes(vector.inputs.message)), expectedPi))
      }
    }

    test(s"VRF - ${vector.description} - proof to hash beta (byte-exact)") {
      IO {
        val pi = hex.parseHex(vector.outputs.pi)
        val expectedBeta = hex.parseHex(vector.outputs.beta)
        val actual = vrf.vrfProofToHash(pi)
        expect(actual.isDefined).and(expect(java.util.Arrays.equals(actual.get, expectedBeta)))
      }
    }

    test(s"VRF - ${vector.description} - verify proof") {
      IO {
        val vk = hex.parseHex(vector.outputs.verificationKey)
        val pi = hex.parseHex(vector.outputs.pi)
        expect(vrf.vrfVerify(vk, msgBytes(vector.inputs.message), pi))
      }
    }

    test(s"VRF - ${vector.description} - reject tampered proof") {
      IO {
        val vk = hex.parseHex(vector.outputs.verificationKey)
        val pi = hex.parseHex(vector.outputs.pi)
        val tampered = pi.clone()
        tampered(0) = (tampered(0) ^ 0xff).toByte
        expect(!vrf.vrfVerify(vk, msgBytes(vector.inputs.message), tampered))
      }
    }
  }

  test("VRF - roundtrip: prove then verify") {
    IO {
      val seed = new Array[Byte](32)
      new java.security.SecureRandom().nextBytes(seed)
      val vk = vrf.getVerificationKey(seed)
      val message = "Hello, VRF!".getBytes("UTF-8")
      val proof = vrf.vrfProof(seed, message)
      expect(vrf.vrfVerify(vk, message, proof))
    }
  }

  test("VRF - deterministic: same input -> same proof and beta") {
    IO {
      val seed = new Array[Byte](32)
      new java.security.SecureRandom().nextBytes(seed)
      val message = "Deterministic".getBytes("UTF-8")
      val p1 = vrf.vrfProof(seed, message)
      val p2 = vrf.vrfProof(seed, message)
      val b1 = vrf.vrfProofToHash(p1)
      val b2 = vrf.vrfProofToHash(p2)
      expect(java.util.Arrays.equals(p1, p2))
        .and(expect(b1.isDefined && b2.isDefined))
        .and(expect(java.util.Arrays.equals(b1.get, b2.get)))
    }
  }

  test("VRF - wrong message fails verification") {
    IO {
      val seed = new Array[Byte](32)
      new java.security.SecureRandom().nextBytes(seed)
      val vk = vrf.getVerificationKey(seed)
      val proof = vrf.vrfProof(seed, "Hello, VRF!".getBytes("UTF-8"))
      expect(!vrf.vrfVerify(vk, "Wrong message".getBytes("UTF-8"), proof))
    }
  }

  test("VRF - wrong key fails verification") {
    IO {
      val seed1 = new Array[Byte](32)
      val seed2 = new Array[Byte](32)
      val rng = new java.security.SecureRandom()
      rng.nextBytes(seed1); rng.nextBytes(seed2)
      val vk2 = vrf.getVerificationKey(seed2)
      val message = "Hello, VRF!".getBytes("UTF-8")
      val proof = vrf.vrfProof(seed1, message)
      expect(!vrf.vrfVerify(vk2, message, proof))
    }
  }

  test("VRF - proof length 80, beta length 64") {
    IO {
      val seed = new Array[Byte](32)
      new java.security.SecureRandom().nextBytes(seed)
      val proof = vrf.vrfProof(seed, "len".getBytes("UTF-8"))
      val beta = vrf.vrfProofToHash(proof)
      expect(proof.length == 80).and(expect(beta.isDefined)).and(expect(beta.get.length == 64))
    }
  }

  test("VRF - invalid lengths return false / None") {
    IO {
      val seed = new Array[Byte](32)
      new java.security.SecureRandom().nextBytes(seed)
      val vk = vrf.getVerificationKey(seed)
      val msg = "x".getBytes("UTF-8")
      expect(!vrf.vrfVerify(vk, msg, new Array[Byte](79)))
        .and(expect(!vrf.vrfVerify(vk, msg, new Array[Byte](81))))
        .and(expect(!vrf.vrfVerify(new Array[Byte](31), msg, vrf.vrfProof(seed, msg))))
        .and(expect(vrf.vrfProofToHash(new Array[Byte](79)).isEmpty))
    }
  }

  test("VRF - empty message works") {
    IO {
      val seed = new Array[Byte](32)
      new java.security.SecureRandom().nextBytes(seed)
      val vk = vrf.getVerificationKey(seed)
      val proof = vrf.vrfProof(seed, Array.emptyByteArray)
      expect(vrf.vrfVerify(vk, Array.emptyByteArray, proof))
    }
  }
}
