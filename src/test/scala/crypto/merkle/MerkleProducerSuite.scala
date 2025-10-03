package crypto.merkle

import cats.effect.IO
import cats.implicits.toTraverseOps

import io.constellationnetwork.metagraph_sdk.crypto.merkle.MerkleNode
import io.constellationnetwork.metagraph_sdk.crypto.merkle.api.{MerkleProducer, TreeBuildError}

import io.circe.syntax.EncoderOps
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleProducerSuite extends SimpleIOSuite with Checkers {

  test("Building with no leaves throws an error") {
    for {
      producer <- MerkleProducer.inMemory[IO](List())
      outcome  <- producer.build
    } yield
      outcome match {
        case Left(TreeBuildError(message)) => expect(message == "Cannot build tree with no leaves")
        case Left(_)                       => failure("Unexpected error type")
        case Right(_)                      => failure("Expecting error but got successful result")
      }
  }

  test("Building a tree with non-empty list is successful") {
    forall(Gen.listOf(Gen.alphaNumStr).suchThat(_.nonEmpty)) { strings =>
      for {
        leaves        <- strings.map(_.asJson).traverse(MerkleNode.Leaf(_))
        producer      <- MerkleProducer.inMemory[IO](leaves)
        outcomeEither <- producer.build
        outcome       <- IO.fromEither(outcomeEither)
      } yield expect(outcome.rootNode.digest.value.nonEmpty)
    }
  }

  test("Appending a value should result in a new tree") {
    for {
      leaves        <- List("one", "two", "three", "four").map(_.asJson).traverse(MerkleNode.Leaf(_))
      newLeaf       <- List("five").map(_.asJson).traverse(MerkleNode.Leaf(_))
      producer      <- MerkleProducer.inMemory[IO](leaves)
      oldTreeEither <- producer.build
      oldTree       <- IO.fromEither(oldTreeEither)
      _             <- producer.append(newLeaf)
      newTreeEither <- producer.build
      newTree       <- IO.fromEither(newTreeEither)
      newLeaves     <- producer.leaves.map(_.map(_.data))
    } yield
      expect(newTree.rootNode.digest.asJson != oldTree.rootNode.asJson) &&
      expect.same(newLeaves, List("one", "two", "three", "four", "five").map(_.asJson))
  }

  test("Prepending a value should result in a new tree") {
    for {
      leaves        <- List("one", "two", "three", "four").map(_.asJson).traverse(MerkleNode.Leaf(_))
      newLeaf       <- List("five").map(_.asJson).traverse(MerkleNode.Leaf(_))
      producer      <- MerkleProducer.inMemory[IO](leaves)
      oldTreeEither <- producer.build
      oldTree       <- IO.fromEither(oldTreeEither)
      _             <- producer.prepend(newLeaf)
      newTreeEither <- producer.build
      newTree       <- IO.fromEither(newTreeEither)
      newLeaves     <- producer.leaves.map(_.map(_.data))
    } yield
      expect(newTree.rootNode.digest.asJson != oldTree.rootNode.asJson) &&
      expect.same(newLeaves, List("five", "one", "two", "three", "four").map(_.asJson))
  }

  test("Updating a value should result in a new tree") {
    for {
      leaves        <- List("one", "two", "three", "four").map(_.asJson).traverse(MerkleNode.Leaf(_))
      newLeaf       <- MerkleNode.Leaf("five".asJson)
      producer      <- MerkleProducer.inMemory[IO](leaves)
      oldTreeEither <- producer.build
      oldTree       <- IO.fromEither(oldTreeEither)
      _             <- producer.update(0, newLeaf).flatMap(IO.fromEither)
      newTreeEither <- producer.build
      newTree       <- IO.fromEither(newTreeEither)
      newLeaves     <- producer.leaves.map(_.map(_.data))
    } yield
      expect(newTree.rootNode.digest.asJson != oldTree.rootNode.asJson) &&
      expect.same(newLeaves, List("five", "two", "three", "four").map(_.asJson))
  }

  test("Removing a value should result in a new tree") {
    for {
      leaves        <- List("one", "two", "three", "four").map(_.asJson).traverse(MerkleNode.Leaf(_))
      producer      <- MerkleProducer.inMemory[IO](leaves)
      oldTreeEither <- producer.build
      oldTree       <- IO.fromEither(oldTreeEither)
      _             <- producer.remove(0).flatMap(IO.fromEither)
      newTreeEither <- producer.build
      newTree       <- IO.fromEither(newTreeEither)
      newLeaves     <- producer.leaves.map(_.map(_.data))
    } yield
      expect(newTree.rootNode.digest.asJson != oldTree.rootNode.asJson) &&
      expect.same(newLeaves, List("two", "three", "four").map(_.asJson))
  }
}
