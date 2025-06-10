package crypto.mpt

import cats.effect.IO
import cats.syntax.applicative._
import cats.syntax.traverse._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaProver, MerklePatriciaVerifier}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerklePatriciaVerifierSuite extends SimpleIOSuite with Checkers {

  test("verifier can confirm an inclusion proof for a path in the trie") {
    forall(Gen.listOfN(32, Gen.long).flatMap { list =>
      Gen.choose(0, list.size - 1).map(index => (list, index))
    }) { case (list, randomIndex) =>
      for {
        leafPairs    <- list.traverse(el => el.computeDigest.map(_ -> el))
        trie         <- MerklePatriciaTrie.create(leafPairs.toMap)
        verifier     <- MerklePatriciaVerifier.make(trie.rootNode.digest).pure[F]
        prover       <- MerklePatriciaProver.make(trie).pure[F]
        proof        <- prover.attestDigest(leafPairs(randomIndex)._1).flatMap(IO.fromEither(_))
        resultEither <- verifier.confirm(proof)
      } yield expect(proof.witness.nonEmpty && resultEither.isRight)
    }
  }

  test("verifier fails to confirm an inclusion proof for a fixed root digest") {
    forall(Gen.listOfN(32, Gen.long).flatMap { list =>
      Gen.choose(0, list.size - 1).map(index => (list, index))
    }) { case (list, randomIndex) =>
      for {
        leafPairs <- list.traverse(el => el.computeDigest.map(_ -> el))
        trie      <- MerklePatriciaTrie.create(leafPairs.toMap)
        verifier <- MerklePatriciaVerifier
          .make(Hash("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))
          .pure[F]
        prover       <- MerklePatriciaProver.make(trie).pure[F]
        proof        <- prover.attestDigest(leafPairs(randomIndex)._1).flatMap(IO.fromEither(_))
        resultEither <- verifier.confirm(proof)
      } yield expect(resultEither.isLeft)
    }
  }
}
