package crypto.mpt

import cats.effect.{IO, Resource}
import cats.syntax.applicative._
import cats.syntax.traverse._

import xyz.kd5ujc.accumulators.mpt.MerklePatriciaTrie
import xyz.kd5ujc.accumulators.mpt.api.MerklePatriciaProver
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.impl.Blake2b256Hasher
import xyz.kd5ujc.hash.l256

import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerklePatriciaProverSuite extends SimpleIOSuite with Checkers {

  private val hasherResource: Resource[IO, Blake2b256Hasher[IO]] =
    Resource.eval {
      JsonSerializer.forSync[IO].map(implicit json2bin => new Blake2b256Hasher[IO])
    }

  private val toDigest: String => l256 = (str: String) => l256.unsafe(Hex.decode(str))

  test("prover can produce an inclusion proof for a path in the trie") {
    hasherResource.use { implicit hasher =>
      forall(Gen.listOfN(32, Gen.long).flatMap { list =>
        Gen.choose(0, list.size - 1).map(index => (list, index))
      }) {
        case (list, randomIndex) =>
          for {
            leafPairs   <- list.traverse(l => hasher.hash(l).map(_ -> l))
            trie        <- MerklePatriciaTrie.create(leafPairs.toMap)
            prover      <- MerklePatriciaProver.make(trie).pure[F]
            proofEither <- prover.attestDigest(leafPairs(randomIndex)._1)
            proof       <- IO.fromEither(proofEither)
          } yield expect(proof.witness.nonEmpty)
      }
    }
  }

  test("prover fails to produce an inclusion proof for a path not in the trie") {
    hasherResource.use { implicit hasher =>
      forall(Gen.listOfN(32, Gen.long)) { list =>
        for {
          leafMap     <- list.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
          trie        <- MerklePatriciaTrie.create(leafMap)
          prover      <- MerklePatriciaProver.make(trie).pure[F]
          proofEither <- prover.attestDigest(toDigest("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))
          _           <- IO.fromEither(proofEither).attempt
        } yield expect(proofEither.isLeft)
      }
    }
  }
}
