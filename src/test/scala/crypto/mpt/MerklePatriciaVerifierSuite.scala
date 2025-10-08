package crypto.mpt

import cats.effect.IO
import cats.syntax.applicative._
import cats.syntax.traverse._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaProver, MerklePatriciaVerifier}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerklePatriciaVerifierSuite extends SimpleIOSuite with Checkers {

  test("verifier can confirm an inclusion proof for a path in the trie") {
    forall(Gen.listOfN(32, Gen.long).flatMap { list =>
      Gen.choose(0, list.size - 1).map(index => (list, index))
    }) {
      case (list, randomIndex) =>
        for {
          leafPairs    <- list.traverse(el => el.computeDigest.map(hash => Hex(hash.value) -> el))
          trie         <- MerklePatriciaTrie.make(leafPairs.toMap)
          verifier     <- MerklePatriciaVerifier.make(trie.rootNode.digest).pure[F]
          prover       <- MerklePatriciaProver.make(trie).pure[F]
          proof        <- prover.attestPath(leafPairs(randomIndex)._1).flatMap(IO.fromEither(_))
          resultEither <- verifier.confirm(proof)
        } yield expect(proof.witness.nonEmpty && resultEither.isRight)
    }
  }

  test("verifier fails to confirm an inclusion proof for a fixed root digest") {
    forall(Gen.listOfN(32, Gen.long).flatMap { list =>
      Gen.choose(0, list.size - 1).map(index => (list, index))
    }) {
      case (list, randomIndex) =>
        for {
          leafPairs <- list.traverse(el => el.computeDigest.map(hash => Hex(hash.value) -> el))
          trie      <- MerklePatriciaTrie.make(leafPairs.toMap)
          verifier <- MerklePatriciaVerifier
            .make(Hash("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))
            .pure[F]
          prover       <- MerklePatriciaProver.make(trie).pure[F]
          proof        <- prover.attestPath(leafPairs(randomIndex)._1).flatMap(IO.fromEither(_))
          resultEither <- verifier.confirm(proof)
        } yield expect(resultEither.isLeft)
    }
  }

  test("verifier can handle large trie with 1k entries and verify random subset of leaves") {
    val numEntries = 1000
    val numProofsToVerify = 50

    for {
      entries <- (1 to numEntries).toList.traverse { i =>
        s"entry_$i".computeDigest.map(hash => Hex(hash.value) -> s"value_$i")
      }
      trie     <- MerklePatriciaTrie.make(entries.toMap)
      verifier <- MerklePatriciaVerifier.make(trie.rootNode.digest).pure[F]
      prover   <- MerklePatriciaProver.make(trie).pure[F]

      randomIndices = scala.util.Random.shuffle((0 until numEntries).toList).take(numProofsToVerify)

      verificationResults <- randomIndices.traverse { idx =>
        val (hex, _) = entries(idx)
        for {
          proof        <- prover.attestPath(hex).flatMap(IO.fromEither(_))
          resultEither <- verifier.confirm(proof)
        } yield resultEither.isRight
      }
    } yield
      expect.all(
        verificationResults.forall(identity),
        verificationResults.size == numProofsToVerify
      )
  }
}
