package crypto.zk

import scala.io.Source

import io.constellationnetwork.metagraph_sdk.crypto.zk.Sp1Groth16Verifier

import io.circe.parser.{parse => parseJson}
import weaver.SimpleIOSuite

/**
 * Cross-repo capstone. A SHIELDED-TRANSFER proof — produced in the metakit-sdk Rust/SP1 zkVM
 * (Poseidon note commitments + nullifiers + fixed-depth Poseidon Merkle membership + value
 * conservation, proven for a 2-in/2-out spend and wrapped to Groth16-BN254 on the GPU) — is
 * verified here in metakit's pure-JVM `Sp1Groth16Verifier` (vendored Besu alt_bn128 pairing,
 * no native deps, no Docker).
 *
 * This closes the same loop M3 closed for the JsonLogic program, on a second and independent
 * SP1 program, and demonstrates that the SP1 Groth16 *circuit* verifying key is stable across
 * the SP1 versions in play (the JLVM proof was v6.1.0-era; this shielded proof was produced with
 * SP1 6.2.x). The program vkey is a public input to that circuit; the circuit VK is what the
 * verifier pins.
 *
 * Fixture: `src/test/resources/zk/sp1-groth16-shielded.json`, vendored from
 * metakit-sdk `rust/zk-shielded/script/fixtures/shielded_groth16_fixture.json` (metakit-sdk PR #31).
 */
object ShieldedTransferVerifierSuite extends SimpleIOSuite {

  private def hexToBytes(h0: String): Array[Byte] = {
    val h = if (h0.startsWith("0x")) h0.substring(2) else h0
    require(h.length % 2 == 0, s"odd-length hex string: ${h.length}")
    h.grouped(2).map(x => Integer.parseInt(x, 16).toByte).toArray
  }

  private def flipByte(bytes: Array[Byte], idx: Int): Array[Byte] = {
    val c = bytes.clone()
    c(idx) = (c(idx) ^ 0x01).toByte
    c
  }

  private val (vkey, publicValues, proofBytes) = {
    val raw = {
      val s = Source.fromInputStream(getClass.getResourceAsStream("/zk/sp1-groth16-shielded.json"), "UTF-8")
      try s.mkString
      finally s.close()
    }
    val cur = parseJson(raw).fold(throw _, identity).hcursor
    def field(name: String): String = cur.get[String](name).fold(throw _, identity)
    (hexToBytes(field("vkey")), hexToBytes(field("publicValues")), hexToBytes(field("proofBytes")))
  }

  pureTest("real shielded-transfer SP1 Groth16 proof verifies in pure JVM") {
    Sp1Groth16Verifier.verify(vkey, publicValues, proofBytes) match {
      case Right(_)  => success
      case Left(msg) => failure(s"expected verification to succeed, got: $msg")
    }
  }

  pureTest("tampered shielded proof is rejected") {
    val tampered = flipByte(proofBytes, proofBytes.length - 1)
    expect(Sp1Groth16Verifier.verify(vkey, publicValues, tampered).isLeft)
  }

  pureTest("wrong publicValues is rejected") {
    val tampered = flipByte(publicValues, publicValues.length - 1)
    expect(Sp1Groth16Verifier.verify(vkey, tampered, proofBytes).isLeft)
  }
}
