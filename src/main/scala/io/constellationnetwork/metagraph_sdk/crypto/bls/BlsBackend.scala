package io.constellationnetwork.metagraph_sdk.crypto.bls

/**
 * Runtime availability gate for the OPTIONAL BouncyCastle 1.85 BLS backend.
 *
 * The eth2-ciphersuite BLS primitive ([[Bls12381]]) compiles against
 * `org.bouncycastle.crypto.bls.*`, which exists ONLY in the (unpublished) BouncyCastle 1.85 line.
 * metakit builds and tests against the sha256-pinned beta jars vendored in `lib/` (sbt's
 * `unmanagedBase`), but unmanaged jars are NEVER part of the published artifact or its POM: the
 * published metakit carries NO BouncyCastle-1.85 linkage requirement.
 *
 * This object is the contract that makes that safe. It probes — exactly once, cached — whether
 * [[Bls12381]] can be initialized on the current classpath:
 *
 *   - Backend present (metakit's own build/tests, or a consumer who vendored the pinned jars, or a
 *     future managed BC 1.85): [[isAvailable]] is `true` and the BLS opcodes work normally.
 *   - Backend absent (any consumer of the published artifact who did not supply the jars; also a
 *     classpath where BC 1.70 shadows 1.85): initializing [[Bls12381]] fails with a
 *     `LinkageError` (`NoClassDefFoundError` / `ExceptionInInitializerError`); the probe catches
 *     it, [[isAvailable]] is `false`, and the JLVM BLS opcodes (`bls_verify` /
 *     `bls_aggregate_verify`) return the DETERMINISTIC error [[unavailableMessage]] as a
 *     `Left(JsonLogicException)` — they never throw and never crash the evaluator.
 *
 * Nothing here references `org.bouncycastle` types, so this class always loads. The [[Bls12381]]
 * methods the gated callers invoke after a successful probe carry no BC types in their signatures
 * either, so linking them is safe once initialization has succeeded.
 */
object BlsBackend {

  /**
   * Whether the BC 1.85 BLS backend is on the classpath. Computed once on first use (per the JVM
   * spec a failed class initialization is permanent for the lifetime of the classloader, so
   * retrying could never succeed anyway).
   */
  lazy val isAvailable: Boolean = probe { () =>
    val _ = Bls12381.SignatureBytes // forces Bls12381 <clinit>, which touches org.bouncycastle.crypto.bls
  }

  /**
   * Runs `forceBackendInit` and classifies the outcome: `true` if it completes, `false` on any
   * `LinkageError` (covers `NoClassDefFoundError` and `ExceptionInInitializerError`). Public so
   * the absence path stays unit-testable without manipulating the classpath; production code uses
   * the cached [[isAvailable]].
   */
  def probe(forceBackendInit: () => Unit): Boolean =
    try {
      forceBackendInit()
      true
    } catch {
      case _: LinkageError => false
    }

  /**
   * The deterministic message the BLS opcodes fail with when the backend is absent. Deliberately
   * environment-independent (no exception details) so the error is byte-stable across consumers.
   */
  def unavailableMessage(op: String): String =
    s"$op unavailable: BouncyCastle 1.85 BLS backend not on classpath"
}
