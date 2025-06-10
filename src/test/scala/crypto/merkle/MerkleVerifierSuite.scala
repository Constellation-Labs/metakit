package crypto.merkle

import cats.effect.IO
import cats.implicits.toTraverseOps

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.{MerkleProver, MerkleVerifier}
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}

import io.circe.syntax.EncoderOps
import shared.Generators.nonEmptyStringListGen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleVerifierSuite extends SimpleIOSuite with Checkers {

  test("Check whether a valid proof is verified successfully") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        leaves <- strings.traverse(str => MerkleNode.Leaf(str.asJson))
        tree   <- MerkleTree.create[IO, String](strings)
        prover = MerkleProver.make[IO](tree)
        verifier = MerkleVerifier.make[IO](tree.rootNode.digest)
        proof   <- prover.attestLeaf(leaves.head).flatMap(IO.fromEither)
        outcome <- verifier.confirm(proof)
      } yield expect(outcome)
    }
  }

  test("Check that a valid proof does NOT verify against another tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        leaves <- strings.traverse(str => MerkleNode.Leaf(str.asJson))
        tree1  <- MerkleTree.create[IO, String](strings)
        tree2  <- MerkleTree.create[IO, String](List("a", "b", "c"))
        prover1 = MerkleProver.make[IO](tree1)
        verifier2 = MerkleVerifier.make[IO](tree2.rootNode.digest)
        proofEither <- prover1.attestLeaf(leaves.head)
        proof       <- IO.fromEither(proofEither)
        outcome     <- verifier2.confirm(proof)
      } yield expect(!outcome)
    }
  }
}
