package crypto.merkle

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.crypto.merkle.MerkleTree
import io.constellationnetwork.security.hash.Hash

import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleTreeSuite extends SimpleIOSuite with Checkers {

  test("ensure root of MerkleTree is non-empty for list of non-empty strings") {
    forall(Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        merkleTree <- MerkleTree.create[IO, String](strings)
      } yield expect(merkleTree.rootNode.digest.value.nonEmpty)
    }
  }

  test("the same list of strings produces MerkleTrees with the same root") {
    forall(Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty))) { strings =>
      for {
        merkleTree1 <- MerkleTree.create[IO, String](strings)
        merkleTree2 <- MerkleTree.create[IO, String](strings)
      } yield expect.same(merkleTree1.asJson, merkleTree2.asJson)
    }
  }

  test("different lists of strings produce MerkleTrees with different roots") {
    val distinctNonEmptyLists: Gen[(List[String], List[String])] = for {
      list1 <- Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty))
      list2 <- Gen.nonEmptyListOf(Gen.alphaStr.suchThat(_.nonEmpty)).suchThat(_ != list1)
    } yield (list1, list2)

    forall(distinctNonEmptyLists) { case (strings1, strings2) =>
      for {
        merkleTree1 <- MerkleTree.create[IO, String](strings1)
        merkleTree2 <- MerkleTree.create[IO, String](strings2)
      } yield expect(merkleTree1.asJson != merkleTree2.asJson)
    }
  }

  // to replicate behavior in another environment, make sure to use a hash function that allows for incremental updating
  // ------------------------------------------
  // NodeJS example (using blake2b, similar for sha256)
  // ------------------------------------------
  //  const blake = require('blakejs');
  //
  //  function computeHash(data, prefix) {
  //    const context = blake.blake2bInit(32, null);
  //    blake.blake2bUpdate(context, prefix);
  //    blake.blake2bUpdate(context, data);
  //    return Buffer.from(blake.blake2bFinal(context))
  //  }
  //
  //  const leafPrefix = Buffer.from([0x00]);
  //  const internalPrefix = Buffer.from([0x01]);
  //
  //  const left = { "a": 1 };
  //  const leftBinary = Buffer.from(JSON.stringify(left));
  //  const leftDigest = computeHash(leftBinary, leafPrefix);
  //
  //  const right = { "b": 2 };
  //  const rightBinary = Buffer.from(JSON.stringify(right));
  //  const rightDigest = computeHash(rightBinary, leafPrefix);
  //
  //  const internalHashable = {
  //    "leftDigest": leftDigest.toString('hex'),
  //    "rightDigest": rightDigest.toString('hex')
  //  };
  //  const internalBinary = Buffer.from(JSON.stringify(internalHashable));
  //  const internalDigest = computeHash(internalBinary, internalPrefix)
  //
  //  console.log(internalDigest.toString('hex'))

  test("ensure root of MerkleTree matches expected value for fixed data") {
    for {
      tree <- MerkleTree.create[IO, Json](List(Json.obj("a" -> 1.asJson), Json.obj("b" -> 2.asJson)))
      expectedRootHash = Hash("1f385c4a728d0e3e49cdf53203df3af57804a18aab5247fa9ddf6f943a65c159")
    } yield expect.same(tree.rootNode.digest, expectedRootHash)
  }
}
