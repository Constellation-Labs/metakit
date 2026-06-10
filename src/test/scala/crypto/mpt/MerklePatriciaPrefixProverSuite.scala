package crypto.mpt

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaBatchInclusionVerifier, MerklePatriciaPrefixProver}
import io.constellationnetwork.security.hex.Hex

import weaver.SimpleIOSuite

/**
 * Tests for the prefix prover (ported from tessellation). A prefix proof is a batch proof over
 * every key whose path starts with the requested prefix.
 */
object MerklePatriciaPrefixProverSuite extends SimpleIOSuite {

  // Variable-length, prefix-free keys grouped under distinguishable prefixes.
  private val entries: Map[Hex, String] = Map(
    Hex("ab12") -> "v1",
    Hex("ab34") -> "v2",
    Hex("ab56") -> "v3",
    Hex("ac78") -> "v4",
    Hex("cd90") -> "v5",
    Hex("cdef") -> "v6",
    Hex("ff00") -> "v7"
  )

  private def trieIO: IO[MerklePatriciaTrie] = MerklePatriciaTrie.make[IO, String](entries)

  test("prefix proof collects every key under the prefix and verifies") {
    for {
      trie <- trieIO
      prover = MerklePatriciaPrefixProver.make[IO](trie)
      verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      proof  <- prover.attestPrefix(Hex("ab")).flatMap(IO.fromEither(_))
      result <- verifier.confirm(proof)
    } yield
      expect.all(
        proof.paths.toSet == Set(Hex("ab12"), Hex("ab34"), Hex("ab56")),
        result.isRight
      )
  }

  test("prefix proof for a deeper prefix narrows the matching set") {
    for {
      trie <- trieIO
      prover = MerklePatriciaPrefixProver.make[IO](trie)
      verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      proof  <- prover.attestPrefix(Hex("cde")).flatMap(IO.fromEither(_))
      result <- verifier.confirm(proof)
    } yield
      expect.all(
        proof.paths.toSet == Set(Hex("cdef")),
        result.isRight
      )
  }

  test("empty prefix collects every key in the trie") {
    for {
      trie <- trieIO
      prover = MerklePatriciaPrefixProver.make[IO](trie)
      verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      proof  <- prover.attestPrefix(Hex("")).flatMap(IO.fromEither(_))
      result <- verifier.confirm(proof)
    } yield
      expect.all(
        proof.paths.toSet == entries.keySet,
        result.isRight
      )
  }

  test("prefix matching a single full key verifies") {
    for {
      trie <- trieIO
      prover = MerklePatriciaPrefixProver.make[IO](trie)
      verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      proof  <- prover.attestPrefix(Hex("ff00")).flatMap(IO.fromEither(_))
      result <- verifier.confirm(proof)
    } yield expect(proof.paths.toSet == Set(Hex("ff00")) && result.isRight)
  }

  test("prefix with no matches returns Left") {
    for {
      trie <- trieIO
      prover = MerklePatriciaPrefixProver.make[IO](trie)
      result <- prover.attestPrefix(Hex("99"))
    } yield expect(result.isLeft)
  }

  test("prefix proof over hashed-key entries verifies for a sampled prefix") {
    for {
      pairs <- (1 to 64).toList.traverse { i =>
        import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
        s"item_$i".computeDigest.map(h => Hex(h.value) -> s"value_$i")
      }
      trie <- MerklePatriciaTrie.make[IO, String](pairs.toMap)
      prover = MerklePatriciaPrefixProver.make[IO](trie)
      verifier = MerklePatriciaBatchInclusionVerifier.make[IO](trie.rootNode.digest)
      // Use the first nibble of an existing key as a prefix; at least that key must match.
      somePrefix = Hex(pairs.head._1.value.take(1))
      expectedMatches = pairs.map(_._1).filter(_.value.startsWith(somePrefix.value)).toSet
      proof  <- prover.attestPrefix(somePrefix).flatMap(IO.fromEither(_))
      result <- verifier.confirm(proof)
    } yield
      expect.all(
        proof.paths.toSet == expectedMatches,
        proof.paths.nonEmpty,
        result.isRight
      )
  }
}
