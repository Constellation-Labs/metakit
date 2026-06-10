package crypto.mpt

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{
  MerklePatriciaBatchInclusionProver,
  MerklePatriciaBatchInclusionVerifier,
  MerklePatriciaProver,
  MerklePatriciaVerifier
}
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{MerklePatriciaBatchInclusionProof, MerklePatriciaTrie}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * Round-trip and negative tests for the batch inclusion prover/verifier (ported from
 * tessellation). A batch proof carries one de-duplicated witness shared across all attested paths.
 */
object MerklePatriciaBatchInclusionSuite extends SimpleIOSuite with Checkers {

  private val bogusHash: Hash =
    Hash("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")

  private def fixture(n: Int): IO[(MerklePatriciaTrie, List[Hex])] =
    (1 to n).toList
      .traverse(i => s"entry_$i".computeDigest.map(h => Hex(h.value) -> s"value_$i"))
      .flatMap(pairs => MerklePatriciaTrie.make[IO, String](pairs.toMap).map(t => (t, pairs.map(_._1))))

  test("batch proof round-trips for all attested paths") {
    for {
      (trie, keys) <- fixture(32)
      prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
      verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      subset = keys.take(10)
      proof  <- prover.attestPaths(subset).flatMap(IO.fromEither(_))
      result <- verifier.confirm(proof)
    } yield
      expect.all(
        proof.paths.toSet == subset.toSet,
        proof.witness.nonEmpty,
        result.isRight
      )
  }

  test("batch proof paths are sorted and the witness is de-duplicated") {
    for {
      (trie, keys) <- fixture(32)
      prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
      subset = keys.take(12)
      proof <- prover.attestPaths(subset).flatMap(IO.fromEither(_))
      // Concatenating the individual witnesses must contain at least as many commitments as the
      // de-duplicated batch witness; shared upper-trie nodes collapse to a single entry.
      singleProver = MerklePatriciaProver.make[IO](trie)
      perPathTotals <- subset.traverse(k => singleProver.attestPath(k).flatMap(IO.fromEither(_)).map(_.witness.size))
      concatenated = perPathTotals.sum
    } yield
      expect.all(
        proof.paths == subset.sorted(Ordering.by[Hex, String](_.value)),
        proof.witness.size <= concatenated,
        proof.witness.distinct.size == proof.witness.size
      )
  }

  test("batch proof of a single path agrees with the single-path verifier") {
    for {
      (trie, keys) <- fixture(16)
      batchProver = MerklePatriciaBatchInclusionProver.make[IO](trie)
      batchVerifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      singleVerifier = MerklePatriciaVerifier.make[IO](trie.rootNode.digest)
      key = keys.head
      batchProof   <- batchProver.attestPaths(List(key)).flatMap(IO.fromEither(_))
      batchResult  <- batchVerifier.confirm(batchProof)
      singleProof  <- MerklePatriciaProver.make[IO](trie).attestPath(key).flatMap(IO.fromEither(_))
      singleResult <- singleVerifier.confirm(singleProof)
    } yield expect(batchResult.isRight && singleResult.isRight)
  }

  test("batch proof JSON round-trips") {
    for {
      (trie, keys) <- fixture(16)
      prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
      proof   <- prover.attestPaths(keys.take(5)).flatMap(IO.fromEither(_))
      decoded <- IO.fromEither(proof.asJson.as[MerklePatriciaBatchInclusionProof])
    } yield expect(decoded == proof)
  }

  test("attesting an empty path list returns Left") {
    for {
      (trie, _) <- fixture(8)
      prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
      result <- prover.attestPaths(List.empty)
    } yield expect(result.isLeft)
  }

  test("attesting a path absent from the trie returns Left") {
    for {
      (trie, keys) <- fixture(8)
      prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
      absent = Hex("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
      result <- prover.attestPaths(keys.take(2) :+ absent)
    } yield expect(result.isLeft)
  }

  test("verifying a batch proof against the wrong root returns Left") {
    for {
      (trie, keys) <- fixture(16)
      prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
      wrongVerifier = MerklePatriciaBatchInclusionVerifier.make[IO](bogusHash)
      proof  <- prover.attestPaths(keys.take(4)).flatMap(IO.fromEither(_))
      result <- wrongVerifier.confirm(proof)
    } yield expect(result.isLeft)
  }

  test("tamper: dropping a commitment from the shared witness fails verification") {
    for {
      (trie, keys) <- fixture(24)
      prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
      verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      proof <- prover.attestPaths(keys.take(8)).flatMap(IO.fromEither(_))
      _ = assert(proof.witness.size >= 2)
      tampered = proof.copy(witness = proof.witness.drop(1))
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("tamper: verifying a path not covered by the witness fails") {
    for {
      (trie, keys) <- fixture(24)
      prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
      verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      proof <- prover.attestPaths(keys.take(4)).flatMap(IO.fromEither(_))
      // Inject a path whose witness was never included.
      tampered = proof.copy(paths = proof.paths :+ keys.last)
      result <- verifier.confirm(tampered)
    } yield expect(result.isLeft)
  }

  test("batch proof round-trips over random subsets (property)") {
    val gen = for {
      size      <- Gen.choose(2, 40)
      subsetLen <- Gen.choose(1, size)
    } yield (size, subsetLen)

    forall(gen) {
      case (size, subsetLen) =>
        for {
          (trie, keys) <- fixture(size)
          prover = MerklePatriciaBatchInclusionProver.make[IO](trie)
          verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
          subset = scala.util.Random.shuffle(keys).take(subsetLen)
          proof  <- prover.attestPaths(subset).flatMap(IO.fromEither(_))
          result <- verifier.confirm(proof)
        } yield expect(result.isRight && proof.paths.toSet == subset.toSet)
    }
  }
}
