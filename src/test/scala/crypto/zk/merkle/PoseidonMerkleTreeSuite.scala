package crypto.zk.merkle

import io.constellationnetwork.metagraph_sdk.crypto.zk.merkle.{Fr, PoseidonMerkleTree}
import io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * Correctness suite for the fixed-depth Poseidon Merkle tree over BN254 Fr.
 *
 * Covers: empty-tree root equals the precomputed zero(depth); inserted leaves yield inclusion proofs that verify
 * against the root; tampering with either the leaf or any path sibling is rejected; absence proofs verify for an unset
 * position and stop verifying once that position is filled; and the root is a pure function of the {position -> leaf}
 * set (insertion-order independence / determinism).
 *
 * Small depths are used so the property tests stay fast while still exercising real left/right path ordering and the
 * zero-subtree convention; a couple of checks pin the default depth (32) explicitly.
 */
object PoseidonMerkleTreeSuite extends SimpleIOSuite with Checkers {

  private val Depth: Int = 8 // capacity 256; enough to span both bit values at every level

  // A note commitment is itself a Poseidon hash; produce canonical, non-zero Fr leaves.
  private def commitment(seed: Long): BigInt = Poseidon.hash(Seq(BigInt(seed).mod(Fr.R), BigInt(seed + 1).mod(Fr.R)))

  private val genPosition: Gen[BigInt] = Gen.choose(0L, (1L << Depth) - 1).map(BigInt(_))
  private val genLeaf: Gen[BigInt] = Gen.choose(1L, 1000000L).map(commitment)

  // ---- zero hashes / empty tree -------------------------------------------------------------------------------------

  pureTest("zero hashes follow zero(0)=0 and zero(i)=compress(zero(i-1), zero(i-1))") {
    val z = PoseidonMerkleTree.zeroHashes(Depth)
    val base = expect.same(z.head, Fr.Zero) && expect.same(z.length, Depth + 1)
    (1 to Depth).foldLeft(base) { (acc, i) =>
      acc && expect.same(z(i), Poseidon.compress(z(i - 1), z(i - 1)))
    }
  }

  pureTest("empty-tree root equals zero(depth)") {
    val tree = PoseidonMerkleTree.empty(Depth)
    expect.same(tree.root, PoseidonMerkleTree.zeroHashes(Depth)(Depth))
  }

  pureTest("default-depth empty tree root equals zero(32)") {
    val tree = PoseidonMerkleTree.empty
    expect.same(tree.depth, PoseidonMerkleTree.DefaultDepth) &&
    expect.same(tree.root, PoseidonMerkleTree.zeroHashes(PoseidonMerkleTree.DefaultDepth)(PoseidonMerkleTree.DefaultDepth))
  }

  pureTest("all zero hashes are canonical field elements in [0, R)") {
    val z = PoseidonMerkleTree.zeroHashes(Depth)
    z.foldLeft(expect(z.nonEmpty))((acc, h) => acc && expect(Fr.isCanonical(h)))
  }

  // ---- inclusion ----------------------------------------------------------------------------------------------------

  pureTest("a single inserted leaf produces an inclusion proof that verifies against the root") {
    val pos = BigInt(173)
    val leaf = commitment(99)
    val tree = PoseidonMerkleTree.empty(Depth).insert(pos, leaf)
    val proof = tree.inclusionProof(pos)
    expect(proof.siblings.length == Depth) &&
    expect(PoseidonMerkleTree.verifyInclusion(leaf, proof, tree.root))
  }

  test("inclusion proofs verify for every inserted leaf (multiple leaves)") {
    val gen = Gen.mapOfN(20, Gen.zip(genPosition, genLeaf))
    forall(gen) { entries =>
      val tree = PoseidonMerkleTree.fromLeaves(Depth, entries)
      val root = tree.root
      val ok = entries.forall {
        case (pos, leaf) =>
          val proof = tree.inclusionProof(pos)
          proof.siblings.length == Depth && PoseidonMerkleTree.verifyInclusion(leaf, proof, root)
      }
      expect(ok)
    }
  }

  pureTest("inserting the same leaf at two positions yields the same leaf but distinct proofs that both verify") {
    val leaf = commitment(7)
    val tree = PoseidonMerkleTree.empty(Depth).insert(BigInt(0), leaf).insert(BigInt(255), leaf)
    val p0 = tree.inclusionProof(BigInt(0))
    val p255 = tree.inclusionProof(BigInt(255))
    expect(p0.siblings != p255.siblings) &&
    expect(PoseidonMerkleTree.verifyInclusion(leaf, p0, tree.root)) &&
    expect(PoseidonMerkleTree.verifyInclusion(leaf, p255, tree.root))
  }

  // ---- tamper rejection ---------------------------------------------------------------------------------------------

  pureTest("a tampered leaf is rejected by inclusion verification") {
    val pos = BigInt(42)
    val leaf = commitment(1)
    val tree = PoseidonMerkleTree.empty(Depth).insert(pos, leaf)
    val proof = tree.inclusionProof(pos)
    val tamperedLeaf = commitment(2)
    expect(!PoseidonMerkleTree.verifyInclusion(tamperedLeaf, proof, tree.root))
  }

  test("a tampered path sibling is rejected by inclusion verification") {
    val gen = for {
      pos      <- genPosition
      leaf     <- genLeaf
      tamperAt <- Gen.choose(0, Depth - 1)
    } yield (pos, leaf, tamperAt)
    forall(gen) {
      case (pos, leaf, tamperAt) =>
        val tree = PoseidonMerkleTree.empty(Depth).insert(pos, leaf)
        val proof = tree.inclusionProof(pos)
        val original = proof.siblings(tamperAt)
        val tampered = Fr.reduce(original + 1) // flip one sibling to a different canonical element
        val bad = proof.copy(siblings = proof.siblings.updated(tamperAt, tampered))
        // If, astronomically, tampering produced the same value, skip; otherwise it must be rejected.
        expect(tampered == original || !PoseidonMerkleTree.verifyInclusion(leaf, bad, tree.root))
    }
  }

  pureTest("a proof for one position does not verify a leaf claimed at a different position's root") {
    val tree = PoseidonMerkleTree.empty(Depth).insert(BigInt(10), commitment(10)).insert(BigInt(11), commitment(11))
    val proofFor10 = tree.inclusionProof(BigInt(10))
    // Claiming position 11's leaf with position 10's path must fail.
    expect(!PoseidonMerkleTree.verifyInclusion(commitment(11), proofFor10, tree.root))
  }

  // ---- absence (nullifier non-membership) ---------------------------------------------------------------------------

  pureTest("an absence proof verifies for an unset position") {
    val tree = PoseidonMerkleTree.empty(Depth).insert(BigInt(5), commitment(5))
    val proof = tree.absenceProof(BigInt(200)) // never set
    expect(proof.siblings.length == Depth) &&
    expect(PoseidonMerkleTree.verifyAbsence(proof, tree.root))
  }

  pureTest("an absence proof captured before an insert no longer verifies against the new root after that insert") {
    val pos = BigInt(200)
    val before = PoseidonMerkleTree.empty(Depth).insert(BigInt(5), commitment(5))
    val absenceProof = before.absenceProof(pos)
    val verifiesBefore = PoseidonMerkleTree.verifyAbsence(absenceProof, before.root)

    val after = before.insert(pos, commitment(123)) // spend the nullifier slot
    val verifiesAfterAgainstNewRoot = PoseidonMerkleTree.verifyAbsence(absenceProof, after.root)

    expect(verifiesBefore) && expect(!verifiesAfterAgainstNewRoot)
  }

  pureTest("after inserting at a position, the fresh absence proof for it fails (it now holds a non-zero leaf)") {
    val pos = BigInt(77)
    val tree = PoseidonMerkleTree.empty(Depth).insert(pos, commitment(77))
    // We can still ask for the raw authentication path; folding the ZERO leaf must NOT reach the root.
    val path = tree.inclusionProof(pos)
    expect(!PoseidonMerkleTree.verifyAbsence(path, tree.root)) &&
    // ... while folding the genuine leaf does.
    expect(PoseidonMerkleTree.verifyInclusion(tree.leafAt(pos), path, tree.root))
  }

  pureTest("absenceProof refuses to be produced for a set position") {
    val pos = BigInt(77)
    val tree = PoseidonMerkleTree.empty(Depth).insert(pos, commitment(77))
    val attempt = scala.util.Try(tree.absenceProof(pos))
    expect(attempt.isFailure)
  }

  pureTest("clearing a position back to zero restores its absence proof") {
    val pos = BigInt(77)
    val tree = PoseidonMerkleTree.empty(Depth).insert(pos, commitment(77)).insert(pos, Fr.Zero)
    expect(!tree.isSet(pos)) &&
    expect.same(tree.leafAt(pos), Fr.Zero) &&
    expect(PoseidonMerkleTree.verifyAbsence(tree.absenceProof(pos), tree.root)) &&
    expect.same(tree.root, PoseidonMerkleTree.empty(Depth).root)
  }

  // ---- determinism / order independence ---------------------------------------------------------------------------

  test("the root is a pure function of the {position -> leaf} set, independent of insertion order") {
    val gen = Gen.mapOfN(16, Gen.zip(genPosition, genLeaf)).map(_.toList)
    forall(gen) { entries =>
      val shuffled = scala.util.Random.shuffle(entries)
      val t1 = PoseidonMerkleTree.fromLeaves(Depth, entries)
      val t2 = PoseidonMerkleTree.fromLeaves(Depth, shuffled)
      expect.same(t1.root, t2.root)
    }
  }

  pureTest("re-inserting the same leaf at the same position is idempotent on the root") {
    val pos = BigInt(123)
    val leaf = commitment(123)
    val once = PoseidonMerkleTree.empty(Depth).insert(pos, leaf)
    val twice = once.insert(pos, leaf)
    expect.same(once.root, twice.root)
  }

  pureTest("overwriting a position changes the leaf and the root deterministically") {
    val pos = BigInt(123)
    val a = PoseidonMerkleTree.empty(Depth).insert(pos, commitment(1))
    val b = a.insert(pos, commitment(2))
    val direct = PoseidonMerkleTree.empty(Depth).insert(pos, commitment(2))
    expect(a.root != b.root) && expect.same(b.root, direct.root) && expect.same(b.leafAt(pos), commitment(2))
  }

  // ---- encoding / validation -------------------------------------------------------------------------------------

  pureTest("the root is always a canonical field element in [0, R)") {
    val tree = PoseidonMerkleTree.empty(Depth).insert(BigInt(1), commitment(1)).insert(BigInt(200), commitment(2))
    expect(Fr.isCanonical(tree.root))
  }

  pureTest("non-canonical leaves are rejected on insert") {
    val tree = PoseidonMerkleTree.empty(Depth)
    expect(scala.util.Try(tree.insert(BigInt(0), Fr.R)).isFailure) &&
    expect(scala.util.Try(tree.insert(BigInt(0), BigInt(-1))).isFailure)
  }

  pureTest("out-of-range positions are rejected") {
    val tree = PoseidonMerkleTree.empty(Depth)
    expect(scala.util.Try(tree.insert(tree.capacity, commitment(1))).isFailure) &&
    expect(scala.util.Try(tree.insert(BigInt(-1), commitment(1))).isFailure) &&
    expect(scala.util.Try(tree.inclusionProof(tree.capacity)).isFailure)
  }
}
