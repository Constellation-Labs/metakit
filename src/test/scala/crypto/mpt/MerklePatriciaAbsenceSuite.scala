package crypto.mpt

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt._
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{
  InvalidNodeType,
  MerklePatriciaProver,
  MerklePatriciaVerifier,
  PathNotFound
}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * ABSENCE (non-inclusion) proof coverage: the prover's `provePath` must emit a verifiable
 * [[MerklePatriciaProof.Absence]] wherever the legacy `attestPath` errored, for every divergence
 * shape (branch-missing-nibble, extension divergence, other-leaf, path-exhausted-at-branch, and
 * the EMPTY trie -- the empty-namespace case `mpt_prefix_verify` cannot express); and the
 * verifier must REJECT any absence claim that does not genuinely bind a divergence to the root
 * (tampered root, tampered/truncated witness, present keys, replayed paths).
 */
object MerklePatriciaAbsenceSuite extends SimpleIOSuite with Checkers {

  private val bogusHash: Hash =
    Hash("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")

  private def prover(trie: MerklePatriciaTrie) = MerklePatriciaProver.make[IO](trie)
  private def verifier(trie: MerklePatriciaTrie) = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)

  private def proveAbsent(trie: MerklePatriciaTrie, path: Hex): IO[MerklePatriciaProof.Absence] =
    prover(trie).provePath(path).flatMap(IO.fromEither(_)).flatMap {
      case a: MerklePatriciaProof.Absence => IO.pure(a)
      case other                          => IO.raiseError(new RuntimeException(s"expected Absence, got: $other"))
    }

  /** Two keys diverging at the first nibble -> root Branch{a, b}. */
  private def branchTrie: IO[MerklePatriciaTrie] =
    MerklePatriciaTrie.make[IO, String](Map(Hex("a1") -> "va", Hex("b2") -> "vb"))

  /** Two keys sharing [a,b,c] -> root Extension(shared=[a,b,c], Branch{d, e}). */
  private def extensionTrie: IO[MerklePatriciaTrie] =
    MerklePatriciaTrie.make[IO, String](Map(Hex("abcd") -> "v1", Hex("abce") -> "v2"))

  /** A single key -> root Leaf. */
  private def singleLeafTrie: IO[MerklePatriciaTrie] =
    MerklePatriciaTrie.make[IO, String](Map(Hex("abcd") -> "solo"))

  /** The canonical empty trie: an empty Branch root (what removing every key collapses to). */
  private def emptyTrie: IO[MerklePatriciaTrie] =
    MerklePatriciaNode.Branch[IO](Map.empty).map(MerklePatriciaTrie(_))

  // --- provePath emits Inclusion for present keys (consistency with attestPath) ---

  test("provePath returns Inclusion for a present key, identical to attestPath's proof") {
    for {
      trie      <- branchTrie
      sealedP   <- prover(trie).provePath(Hex("a1")).flatMap(IO.fromEither(_))
      legacy    <- prover(trie).attestPath(Hex("a1")).flatMap(IO.fromEither(_))
      confirmed <- verifier(trie).confirm(sealedP)
    } yield
      expect.all(
        sealedP == MerklePatriciaProof.Inclusion(legacy),
        confirmed.isRight
      )
  }

  // --- each absence shape yields a proof that verifies against the true root ---

  test("absence under a branch (missing nibble) proves and verifies") {
    for {
      trie   <- branchTrie
      proof  <- proveAbsent(trie, Hex("c3"))
      result <- verifier(trie).confirm(proof)
    } yield
      expect.all(
        result.isRight,
        proof.witness.headOption.exists(_.isInstanceOf[MerklePatriciaCommitment.Branch])
      )
  }

  test("absence at a divergent extension (mid-edge mismatch) proves and verifies") {
    for {
      trie   <- extensionTrie
      proof  <- proveAbsent(trie, Hex("ab12"))
      result <- verifier(trie).confirm(proof)
    } yield
      expect.all(
        result.isRight,
        proof.witness.headOption.exists(_.isInstanceOf[MerklePatriciaCommitment.Extension])
      )
  }

  test("absence when the query exhausts inside an extension edge proves and verifies") {
    for {
      trie   <- extensionTrie
      proof  <- proveAbsent(trie, Hex("ab"))
      result <- verifier(trie).confirm(proof)
    } yield expect(result.isRight)
  }

  test("absence at an other-leaf (single-key trie) proves and verifies for sibling/shorter/longer queries") {
    for {
      trie <- singleLeafTrie
      results <- List(Hex("abce"), Hex("ab"), Hex("abcdef")).traverse { q =>
        proveAbsent(trie, q).flatMap(verifier(trie).confirm(_))
      }
    } yield expect(results.forall(_.isRight))
  }

  test("absence in the EMPTY trie proves and verifies (empty-namespace case)") {
    for {
      trie   <- emptyTrie
      proof  <- proveAbsent(trie, Hex("deadbeef"))
      result <- verifier(trie).confirm(proof)
    } yield
      expect.all(
        result.isRight,
        proof.witness == List(MerklePatriciaCommitment.Branch(Map.empty))
      )
  }

  test("absence when the path exhausts at a branch (no value slot) proves and verifies") {
    for {
      trie   <- MerklePatriciaTrie.make[IO, String](Map(Hex("ab") -> "v1", Hex("ac") -> "v2"))
      proof  <- proveAbsent(trie, Hex("a"))
      result <- verifier(trie).confirm(proof)
    } yield expect(result.isRight)
  }

  test("any absent 64-nibble key in a random trie yields a verifiable absence proof") {
    forall(Gen.listOfN(32, Gen.long)) { list =>
      val absentKey = Hex("F" * 64)
      for {
        leafMap <- list.traverse(el => el.computeDigest.map(hash => Hex(hash.value) -> el)).map(_.toMap)
        trie    <- MerklePatriciaTrie.make(leafMap)
        proof   <- proveAbsent(trie, absentKey)
        result  <- verifier(trie).confirm(proof)
      } yield expect(result.isRight)
    }
  }

  // --- rejection: forged or replayed absence claims must not verify ---

  test("absence proof is rejected against a tampered root") {
    for {
      trie   <- branchTrie
      proof  <- proveAbsent(trie, Hex("c3"))
      result <- MerklePatriciaVerifier.make[IO](bogusHash).confirm(proof)
    } yield expect(result.isLeft)
  }

  test("absence proof with a tampered witness commitment is rejected") {
    for {
      trie <- branchTrie
      proof <- proveAbsent(trie, Hex("c3"))
      tampered = proof.copy(witness = proof.witness.map {
        case branch: MerklePatriciaCommitment.Branch =>
          branch.copy(pathsDigest = branch.pathsDigest.map { case (n, _) => n -> bogusHash })
        case other => other
      })
      result <- verifier(trie).confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("absence proof with a truncated witness is rejected") {
    for {
      trie  <- MerklePatriciaTrie.make[IO, String](Map(Hex("ab") -> "v1", Hex("ac") -> "v2"))
      proof <- proveAbsent(trie, Hex("a"))
      _ = assert(proof.witness.size >= 2)
      // Dropping the root-most element breaks the digest chain back to the root.
      truncated = proof.copy(witness = proof.witness.dropRight(1))
      emptied = proof.copy(witness = List.empty)
      r1 <- verifier(trie).confirm(truncated)
      r2 <- verifier(trie).confirm(emptied)
    } yield expect.all(r1.isLeft, r2.isLeft)
  }

  test("absence claim for a PRESENT key is rejected (relabeled and truncated inclusion witness)") {
    for {
      trie      <- branchTrie
      inclusion <- prover(trie).attestPath(Hex("a1")).flatMap(IO.fromEither(_))
      // The full inclusion witness relabeled as absence: terminal leaf MATCHES -> invalid.
      relabeled = MerklePatriciaProof.Absence(Hex("a1"), inclusion.witness)
      // The witness truncated to the branch: terminal branch CONTAINS the next nibble -> invalid.
      truncated = MerklePatriciaProof.Absence(Hex("a1"), inclusion.witness.drop(1))
      r1 <- verifier(trie).confirm(relabeled)
      r2 <- verifier(trie).confirm(truncated)
    } yield expect.all(r1.isLeft, r2.isLeft)
  }

  test("absence proof is not transferable to a different (present) path") {
    for {
      trie  <- extensionTrie
      proof <- proveAbsent(trie, Hex("ab12"))
      replayed = proof.copy(path = Hex("abcd"))
      result <- verifier(trie).confirm(replayed)
    } yield expect(result.isLeft)
  }

  // --- legacy surface: attestPath stays inclusion-only with its historical errors ---

  test("attestPath still errors on absent keys with the legacy error types") {
    for {
      trie1 <- branchTrie
      trie2 <- singleLeafTrie
      trie3 <- extensionTrie
      missingChild <- prover(trie1).attestPath(Hex("c3"))
      otherLeaf    <- prover(trie2).attestPath(Hex("abce"))
      divergent    <- prover(trie3).attestPath(Hex("ab12"))
    } yield
      expect.all(
        missingChild.left.exists(_.isInstanceOf[PathNotFound]),
        otherLeaf.left.exists(_.isInstanceOf[InvalidNodeType]),
        divergent.left.exists(_.isInstanceOf[InvalidNodeType])
      )
  }

  // --- inclusion hardening: the extension prefix is now bound to the queried path ---

  test("forged inclusion proof whose extension edge diverges from the path is rejected") {
    for {
      trie  <- MerklePatriciaTrie.make[IO, String](Map(Hex("1a") -> "va", Hex("1b") -> "vb"))
      proof <- prover(trie).attestPath(Hex("1a")).flatMap(IO.fromEither(_))
      // Same witness, different first nibble: without the extension shared-prefix check this
      // would have verified (the fold consumed the edge without comparing it to the path).
      forged = MerklePatriciaInclusionProof(Hex("2a"), proof.witness)
      genuine <- verifier(trie).confirm(proof)
      result  <- verifier(trie).confirm(forged)
    } yield expect.all(genuine.isRight, result.isLeft)
  }
}
