package crypto.merkle

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.MerkleProver
import io.constellationnetwork.metagraph_sdk.crypto.merkle.{MerkleNode, MerkleTree}

import io.circe.syntax.EncoderOps
import shared.Generators.nonEmptyStringListGen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleProverSuite extends SimpleIOSuite with Checkers {

  test("Creating a proof is successful using a Leaf of the tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        leaves <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        tree   <- MerkleTree.create[IO, String](strings)
        prover = MerkleProver.make[IO](tree)
        proofEither <- prover.attestLeaf(leaves.head)
        proof       <- IO.fromEither(proofEither)
      } yield expect(proof.leafDigest == leaves.head.digest)
    }
  }

  test("Creating a proof is successful using a digest of a leaf of the tree") {
    forall(nonEmptyStringListGen(1, 100)) { strings =>
      for {
        leaves <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        tree   <- MerkleTree.create[IO, String](strings)
        prover = MerkleProver.make[IO](tree)
        proofEither <- prover.attestDigest(leaves.head.digest)
        proof       <- IO.fromEither(proofEither)
      } yield expect(proof.leafDigest == leaves.head.digest)
    }
  }

  test("Creating a proof fails when using a Leaf NOT in the tree") {
    forall(nonEmptyStringListGen(2, 100).map(_.distinct)) { strings =>
      for {
        leaves <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        tree   <- MerkleTree.create[IO, String](strings.tail)
        prover = MerkleProver.make[IO](tree)
        proofEither <- prover.attestLeaf(leaves.head)
      } yield expect(proofEither.isLeft)
    }
  }

  test("Creating a proof fails when using a digest of a leaf NOT in the tree") {
    forall(nonEmptyStringListGen(2, 100).map(_.distinct)) { strings =>
      for {
        leaves <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        tree   <- MerkleTree.create[IO, String](strings.tail)
        prover = MerkleProver.make[IO](tree)
        proofEither <- prover.attestDigest(leaves.head.digest)
      } yield expect(proofEither.isLeft)
    }
  }
}
