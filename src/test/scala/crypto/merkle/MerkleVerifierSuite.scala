package crypto.merkle

import cats.effect.IO
import cats.implicits.toTraverseOps

import io.constellationnetwork.metagraph_sdk.crypto.merkle.MerkleTree
import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.{MerkleProver, MerkleVerifier}

import shared.Generators.nonEmptyStringListGen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleVerifierSuite extends SimpleIOSuite with Checkers {

  test("Check whether a valid proof is verified successfully") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        tree <- MerkleTree.create[IO, String](strings.distinct) // Use distinct to avoid duplicates
        prover = MerkleProver.make[IO](tree)
        verifier = MerkleVerifier.make[IO](tree.rootNode.digest)
        results <- (0 until strings.distinct.size).toList.traverse { idx =>
          for {
            proof   <- prover.attestIndex(idx).flatMap(IO.fromEither)
            outcome <- verifier.confirm(proof)
          } yield outcome
        }
      } yield expect(results.forall(identity))
    }
  }

  test("Check that a valid proof does NOT verify against another tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        tree1 <- MerkleTree.create[IO, String](strings)
        tree2 <- MerkleTree.create[IO, String](List("a", "b", "c"))
        prover1 = MerkleProver.make[IO](tree1)
        verifier2 = MerkleVerifier.make[IO](tree2.rootNode.digest)
        proof   <- prover1.attestIndex(0).flatMap(IO.fromEither)
        outcome <- verifier2.confirm(proof)
      } yield expect(!outcome)
    }
  }
}
