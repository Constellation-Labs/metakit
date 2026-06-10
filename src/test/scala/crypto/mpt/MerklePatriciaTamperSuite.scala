package crypto.mpt

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaProver, MerklePatriciaVerifier}
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{MerklePatriciaCommitment, MerklePatriciaInclusionProof, MerklePatriciaTrie, Nibble}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import weaver.SimpleIOSuite

/**
 * Negative (tamper) tests for the prover/verifier pair. A valid proof must verify; any mutation
 * of the witness - altering a commitment digest, dropping a step, or swapping leaf data - must
 * cause verification to return Left.
 */
object MerklePatriciaTamperSuite extends SimpleIOSuite {

  private val bogusHash: Hash =
    Hash("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")

  /** Build a trie over 16 hashed-key entries and return (trie, list of keys). */
  private def fixture: IO[(MerklePatriciaTrie, List[Hex])] =
    (1 to 16).toList
      .traverse(i => s"entry_$i".computeDigest.map(h => Hex(h.value) -> s"value_$i"))
      .flatMap { pairs =>
        MerklePatriciaTrie.make[IO, String](pairs.toMap).map(trie => (trie, pairs.map(_._1)))
      }

  private def proofFor(trie: MerklePatriciaTrie, key: Hex): IO[MerklePatriciaInclusionProof] =
    MerklePatriciaProver.make[IO](trie).attestPath(key).flatMap(IO.fromEither(_))

  test("an untampered proof verifies") {
    for {
      (trie, keys) <- fixture
      verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      proof  <- proofFor(trie, keys.head)
      result <- verifier.confirm(proof)
    } yield expect(result.isRight)
  }

  test("tamper: mutating a leaf commitment's dataDigest fails verification") {
    for {
      (trie, keys) <- fixture
      verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      proof <- proofFor(trie, keys.head)
      tampered = proof.copy(witness = proof.witness.map {
        case leaf: MerklePatriciaCommitment.Leaf => leaf.copy(dataDigest = bogusHash)
        case other                               => other
      })
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("tamper: mutating a leaf commitment's remaining path fails verification") {
    for {
      (trie, keys) <- fixture
      verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      proof <- proofFor(trie, keys.head)
      tampered = proof.copy(witness = proof.witness.map {
        case leaf: MerklePatriciaCommitment.Leaf =>
          leaf.copy(remaining = leaf.remaining :+ Nibble.unsafe(0x00: Byte))
        case other => other
      })
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("tamper: mutating a branch commitment's child digest fails verification") {
    for {
      (trie, keys) <- fixture
      verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      proof <- proofFor(trie, keys.head)
      tampered = proof.copy(witness = proof.witness.map {
        case branch: MerklePatriciaCommitment.Branch =>
          branch.copy(pathsDigest = branch.pathsDigest.map { case (n, _) => n -> bogusHash })
        case other => other
      })
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("tamper: dropping the first (leaf) witness step fails verification") {
    // Witness is ordered leaf-first; dropping the head removes the terminal leaf commitment.
    for {
      (trie, keys) <- fixture
      verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      deepKey   <- keys.traverse(k => proofFor(trie, k).map(p => k -> p.witness.size)).map(_.maxBy(_._2)._1)
      deepProof <- proofFor(trie, deepKey)
      _ = assert(deepProof.witness.size >= 2)
      tampered = deepProof.copy(witness = deepProof.witness.drop(1))
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("tamper: dropping an intermediate branch step fails verification") {
    for {
      (trie, keys) <- fixture
      verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      // pick the key with the deepest proof so there is an intermediate step to drop
      deepKey <- keys.traverse(k => proofFor(trie, k).map(p => k -> p.witness.size)).map(_.maxBy(_._2)._1)
      proof   <- proofFor(trie, deepKey)
      _ = assert(proof.witness.size >= 2)
      // drop the last witness element (the root-most commitment, an intermediate branch/extension)
      tampered = proof.copy(witness = proof.witness.dropRight(1))
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("tamper: swapping in another path's leaf commitment fails verification") {
    for {
      (trie, keys) <- fixture
      verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      proofA <- proofFor(trie, keys.head)
      proofB <- proofFor(trie, keys(1))
      leafB = proofB.witness.collectFirst { case l: MerklePatriciaCommitment.Leaf => l }.get
      // Replace proofA's leaf commitment (witness head) with proofB's leaf commitment.
      tampered = proofA.copy(witness = leafB :: proofA.witness.tail)
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("tamper: verifying a valid proof against the wrong root fails") {
    for {
      (trie, keys) <- fixture
      proof        <- proofFor(trie, keys.head)
      wrongVerifier = MerklePatriciaVerifier.make[IO](bogusHash)
      result <- wrongVerifier.confirm(proof)
    } yield expect(result.isLeft)
  }

  test("tamper: replacing the entire witness with an empty list fails verification") {
    for {
      (trie, keys) <- fixture
      verifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      proof <- proofFor(trie, keys.head)
      tampered = proof.copy(witness = List.empty[MerklePatriciaCommitment])
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }
}
