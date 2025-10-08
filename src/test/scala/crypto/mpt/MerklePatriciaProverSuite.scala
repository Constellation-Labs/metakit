package crypto.mpt

import cats.effect.IO
import cats.syntax.applicative._
import cats.syntax.traverse._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaProver
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hex.Hex

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerklePatriciaProverSuite extends SimpleIOSuite with Checkers {

  test("prover can produce an inclusion proof for a path in the trie") {
    forall(Gen.listOfN(32, Gen.long).flatMap { list =>
      Gen.choose(0, list.size - 1).map(index => (list, index))
    }) {
      case (list, randomIndex) =>
        for {
          leafPairs <- list.traverse(el => el.computeDigest.map(hash => Hex(hash.value) -> el))
          trie      <- MerklePatriciaTrie.make(leafPairs.toMap)
          prover    <- MerklePatriciaProver.make(trie).pure[F]
          proof     <- prover.attestPath(leafPairs(randomIndex)._1).flatMap(IO.fromEither)
        } yield expect(proof.witness.nonEmpty)
    }
  }

  test("prover fails to produce an inclusion proof for a path not in the trie") {
    forall(Gen.listOfN(32, Gen.long)) { list =>
      for {
        leafMap     <- list.traverse(el => el.computeDigest.map(hash => Hex(hash.value) -> el)).map(_.toMap)
        trie        <- MerklePatriciaTrie.make(leafMap)
        prover      <- MerklePatriciaProver.make(trie).pure[F]
        proofEither <- prover.attestPath(Hex("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))
      } yield expect(proofEither.isLeft)
    }
  }
}
