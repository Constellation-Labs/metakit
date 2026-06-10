package crypto.bls

import io.constellationnetwork.metagraph_sdk.crypto.bls.{Bls12381, BlsBackend}
import io.constellationnetwork.metagraph_sdk.json_logic.core.{ArrayValue, StrValue}
import io.constellationnetwork.metagraph_sdk.json_logic.ops.CryptoOps

import weaver.SimpleIOSuite

/**
 * Contract suite for the OPTIONAL-backend gate ([[BlsBackend]]).
 *
 * metakit's own test classpath carries the vendored BC 1.85 jars, so the backend is PRESENT here
 * (asserted below; the bls categories of `ZkVectorConformanceSuite` and the `ZkOpsWave2Suite` /
 * `Bls12381Suite` known-answer tests then exercise the available path normally). The ABSENT path
 * cannot be reproduced on this classpath — a failed class init is permanent per classloader — so
 * it is covered by driving the gate's probe function directly with the exact linkage errors a
 * backend-less consumer JVM raises, plus pinning the deterministic error message the opcodes
 * surface.
 */
object BlsBackendSuite extends SimpleIOSuite {

  pureTest("backend is available on metakit's own (vendored-jar) classpath") {
    expect(BlsBackend.isAvailable)
  }

  pureTest("probe -> true when backend init succeeds") {
    expect(BlsBackend.probe(() => ()))
  }

  pureTest("probe -> false on NoClassDefFoundError (backend jars absent / BC 1.70 shadowing)") {
    expect(!BlsBackend.probe(() => throw new NoClassDefFoundError("org/bouncycastle/crypto/bls/BLS12_381G1")))
  }

  pureTest("probe -> false on ExceptionInInitializerError (backend class init failed)") {
    expect(!BlsBackend.probe(() => throw new ExceptionInInitializerError(new RuntimeException("boom"))))
  }

  pureTest("probe does NOT swallow non-linkage failures") {
    val thrown =
      try {
        BlsBackend.probe(() => throw new RuntimeException("not a linkage problem"))
        false
      } catch { case _: RuntimeException => true }
    expect(thrown)
  }

  pureTest("unavailable message is the deterministic, environment-independent contract string") {
    expect
      .eql(
        "bls_verify unavailable: BouncyCastle 1.85 BLS backend not on classpath",
        BlsBackend.unavailableMessage("bls_verify")
      )
      .and(
        expect.eql(
          "bls_aggregate_verify unavailable: BouncyCastle 1.85 BLS backend not on classpath",
          BlsBackend.unavailableMessage("bls_aggregate_verify")
        )
      )
  }

  pureTest("opcode arity errors gate AFTER availability: with backend present, malformed args still error normally") {
    // Guards the gate's position in the handler: on this (backend-present) classpath the
    // pre-existing arity/shape errors must be unchanged by the gate.
    val r = CryptoOps.blsVerify(List(StrValue("deadbeef")))
    expect(r.isLeft).and(expect(r.swap.exists(_.getMessage.startsWith("bls_verify: expected"))))
  }

  pureTest("aggregate opcode shape errors unchanged by the gate") {
    val r = CryptoOps.blsAggregateVerify(List(ArrayValue(List.empty), StrValue("00"), StrValue("00")))
    expect(r.isLeft).and(
      expect(
        r.swap.exists(_.getMessage == "bls_aggregate_verify: at least one public key required")
      )
    )
  }

  pureTest("gate constants would match the backend's (sanity: probe target is the real init)") {
    // The probe forces Bls12381's init via SignatureBytes; assert the values it exposes are the
    // wire sizes the opcodes parse against, so the probe target cannot silently drift.
    expect.eql(48, Bls12381.PublicKeyBytes).and(expect.eql(96, Bls12381.SignatureBytes))
  }
}
