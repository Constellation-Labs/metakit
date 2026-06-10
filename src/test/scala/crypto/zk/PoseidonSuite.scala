package crypto.zk

import scala.util.Try

import io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon

import weaver.SimpleIOSuite

/**
 * Correctness suite for the circomlib-compatible Poseidon hash over BN254 Fr.
 *
 * The decisive checks are the published test vectors taken verbatim from
 * circomlibjs' own test suite (iden3/circomlibjs `test/poseidon.js`):
 *
 *   poseidon([1, 2]) == 0x115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a
 *   poseidon([1])    == 18586133768512220936620570745912940619677854269274689475585506675881198879027
 *   poseidon([1,2,3,4]) == 0x299c867db6c1fdd79dcefa40e4510b9837e60ebb1ce0663dbaa525df65250465
 *
 * If these pass, our implementation is byte-for-byte interoperable with the
 * Ethereum-ecosystem reference Poseidon.
 */
object PoseidonSuite extends SimpleIOSuite {

  private val expected_1_2: BigInt =
    BigInt("115cc0f5e7d690413df64c6b9662e9cf2a3617f2743245519e19607a4417189a", 16)

  private val expected_1: BigInt =
    BigInt("18586133768512220936620570745912940619677854269274689475585506675881198879027")

  private val expected_1_2_3_4: BigInt =
    BigInt("299c867db6c1fdd79dcefa40e4510b9837e60ebb1ce0663dbaa525df65250465", 16)

  pureTest("published vector: poseidon([1, 2]) matches circomlibjs") {
    expect.same(Poseidon.hash(Seq(BigInt(1), BigInt(2))), expected_1_2)
  }

  pureTest("published vector: poseidon([1]) matches circomlibjs") {
    expect.same(Poseidon.hash(Seq(BigInt(1))), expected_1)
  }

  pureTest("published vector: poseidon([1, 2, 3, 4]) matches circomlibjs") {
    expect.same(Poseidon.hash(Seq(BigInt(1), BigInt(2), BigInt(3), BigInt(4))), expected_1_2_3_4)
  }

  pureTest("hash is deterministic") {
    val a = Poseidon.hash(Seq(BigInt(7), BigInt(42), BigInt(123456789)))
    val b = Poseidon.hash(Seq(BigInt(7), BigInt(42), BigInt(123456789)))
    expect.same(a, b)
  }

  pureTest("output is always a canonical field element in [0, R)") {
    val h = Poseidon.hash(Seq(BigInt(1), BigInt(2)))
    expect(h >= 0) && expect(h < Poseidon.R)
  }

  pureTest("compress(a, b) == hash([a, b])") {
    val a = BigInt("12345678901234567890")
    val b = BigInt("98765432109876543210")
    expect.same(Poseidon.compress(a, b), Poseidon.hash(Seq(a, b)))
  }

  pureTest("compress matches the published 2-input vector for (1, 2)") {
    expect.same(Poseidon.compress(BigInt(1), BigInt(2)), expected_1_2)
  }

  pureTest("inputs >= R are rejected (not silently reduced)") {
    val tooBig = Poseidon.R // exactly the modulus is out of range
    val attempt = Try(Poseidon.hash(Seq(tooBig)))
    expect(attempt.isFailure)
  }

  pureTest("negative inputs are rejected") {
    val attempt = Try(Poseidon.hash(Seq(BigInt(-1))))
    expect(attempt.isFailure)
  }

  pureTest("the maximal in-range input R-1 is accepted and reproduces circomlib") {
    // poseidon([R-1]) over BN254, computed with the circomlib reference
    // permutation (the same path that reproduces the published vectors above).
    // Locks the canonical-boundary behaviour to the exact reference output.
    val rMinus1 = Poseidon.R - 1
    val expected =
      BigInt("3366645945435192953002076803303112651887535928162668198103357554665518664470")
    expect.same(Poseidon.hash(Seq(rMinus1)), expected)
  }

  pureTest("3-input (width t=4) reproduces circomlib reference: poseidon([3,4,5])") {
    // Reference value from the circomlib permutation over BN254 at t = 4.
    val expected =
      BigInt("16070431878087339506657234884858910435593423055199073760739081656581316900759")
    expect.same(Poseidon.hash(Seq(BigInt(3), BigInt(4), BigInt(5))), expected)
  }

  pureTest("empty input is rejected") {
    expect(Try(Poseidon.hash(Seq.empty)).isFailure)
  }

  pureTest("different inputs produce different hashes") {
    val h1 = Poseidon.hash(Seq(BigInt(1), BigInt(2)))
    val h2 = Poseidon.hash(Seq(BigInt(2), BigInt(1)))
    expect(h1 != h2)
  }
}
