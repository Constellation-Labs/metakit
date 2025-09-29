package crypto.merkle

import cats.effect.{IO, Ref}
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.merkle.MerkleNode
import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.LeafNotFound
import io.constellationnetwork.metagraph_sdk.crypto.merkle.impl.CollectionMerkleProver
import io.constellationnetwork.metagraph_sdk.storage.CollectionReader
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax._
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object CollectionMerkleProverSuite extends SimpleIOSuite with Checkers {

  def makeTestStorage(leaves: List[MerkleNode.Leaf]): IO[CollectionReader[IO, Int, MerkleNode.Leaf]] = {
    val indexedLeaves = leaves.zipWithIndex.map { case (leaf, idx) => (idx, leaf) }.toMap
    for {
      stateRef <- Ref.of[IO, Map[Int, MerkleNode.Leaf]](indexedLeaves)
    } yield new CollectionReader[IO, Int, MerkleNode.Leaf] {
      def get(key: Int): IO[Option[MerkleNode.Leaf]] = stateRef.get.map(_.get(key))
      def getBatch(keys: List[Int]): IO[List[(Int, Option[MerkleNode.Leaf])]] =
        stateRef.get.map(map => keys.map(k => (k, map.get(k))))
      override def dump: IO[List[(Int, MerkleNode.Leaf)]] = stateRef.get.map(_.toList)
      def contains(id: Int): IO[Boolean] = stateRef.get.map(_.contains(id))
      def getWithFilter(cond: (Int, MerkleNode.Leaf) => Boolean): IO[List[(Int, MerkleNode.Leaf)]] =
        stateRef.get.map(_.toList.filter { case (k, v) => cond(k, v) })
    }
  }

  test("generates proof for existing leaf by digest") {
    forall(Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        leaves  <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)
        leafDigest = leaves.head.digest
        result <- prover.attestDigest(leafDigest)
      } yield expect(result.isRight) && expect(result.toOption.get.leafDigest == leafDigest)
    }
  }

  test("returns error for non-existent digest") {
    forall(Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        leaves  <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)
        fakeDigest = Hash("deadbeef")
        result <- prover.attestDigest(fakeDigest)
      } yield expect(result.isLeft) && expect(result.left.toOption.get.isInstanceOf[LeafNotFound])
    }
  }

  test("generates proof for leaf at index") {
    val genIndexTest = for {
      size      <- Gen.choose(3, 10)
      targetIdx <- Gen.choose(0, size - 1)
    } yield (size, targetIdx)

    forall(genIndexTest) { case (size, targetIdx) =>
      for {
        leaves  <- (1 to size).toList.traverse(i => MerkleNode.Leaf[IO](i.asJson))
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)
        result <- prover.attestIndex(targetIdx)
      } yield expect(result.isRight) &&
      expect(result.toOption.get.leafDigest == leaves(targetIdx).digest)
    }
  }

  test("generates consistent proofs across multiple calls") {
    forall(Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        leaves  <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)

        proof1 <- prover.attestIndex(0)
        proof2 <- prover.attestIndex(0)
      } yield expect(proof1.isRight) &&
      expect(proof2.isRight) &&
      expect(proof1 == proof2)
    }
  }

  test("handles single leaf tree") {
    forall(Gen.alphaStr.suchThat(_.nonEmpty)) { leafData =>
      for {
        singleLeaf <- MerkleNode.Leaf[IO](leafData.asJson)
        leaves = List(singleLeaf)
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)
        result <- prover.attestIndex(0)
      } yield expect(result.isRight) && expect(result.toOption.get.witness.isEmpty)
    }
  }

  test("handles unbalanced tree with 3 leaves") {
    forall(Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        leaves  <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)

        proof0 <- prover.attestIndex(0)
        proof1 <- prover.attestIndex(1)
        proof2 <- prover.attestIndex(2)
      } yield expect(proof0.isRight) &&
      expect(proof1.isRight) &&
      expect(proof2.isRight) &&
      expect(proof0.toOption.get.leafDigest == leaves(0).digest) &&
      expect(proof1.toOption.get.leafDigest == leaves(1).digest) &&
      expect(proof2.toOption.get.leafDigest == leaves(2).digest)
    }
  }

  test("handles larger unbalanced tree") {
    forall(Gen.choose(10, 20)) { leafCount =>
      for {
        leaves  <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](s"item-$i".asJson))
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)

        indices = List(0, leafCount / 2, leafCount - 1)
        proofs <- indices.traverse(prover.attestIndex)

        proofsValid = proofs.forall(_.isRight)
        digestsMatch = proofs.zip(indices).forall { case (proof, idx) =>
          proof.toOption.get.leafDigest == leaves(idx).digest
        }
      } yield expect(proofsValid) && expect(digestsMatch)
    }
  }

  test("attestLeaf delegates to attestDigest") {
    forall(Gen.listOfN(3, Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        leaves <- strings.traverse(s => MerkleNode.Leaf[IO](s.asJson))
        targetLeaf = leaves(1)
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)

        proofByLeaf   <- prover.attestLeaf(targetLeaf)
        proofByDigest <- prover.attestDigest(targetLeaf.digest)
      } yield expect(proofByLeaf.isRight) &&
      expect(proofByDigest.isRight) &&
      expect(proofByLeaf == proofByDigest)
    }
  }

  test("rebuilds tree correctly for each proof") {
    forall(Gen.choose(5, 10)) { leafCount =>
      for {
        leaves  <- (1 to leafCount).toList.traverse(i => MerkleNode.Leaf[IO](i.asJson))
        storage <- makeTestStorage(leaves)
        prover = CollectionMerkleProver.make[IO](storage)

        proofs <- leaves.zipWithIndex.traverse { case (leaf, idx) =>
          prover.attestIndex(idx).map(_.toOption.get -> leaf)
        }

        allDigestsMatch = proofs.forall { case (proof, leaf) =>
          proof.leafDigest == leaf.digest
        }
      } yield expect(proofs.size == leafCount) && expect(allDigestsMatch)
    }
  }

  test("works with empty storage reader") {
    for {
      storage <- makeTestStorage(List.empty)
      prover = CollectionMerkleProver.make[IO](storage)
      result <- prover.attestIndex(0)
    } yield expect(result.isLeft)
  }
}
