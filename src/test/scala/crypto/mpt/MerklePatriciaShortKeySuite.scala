package crypto.mpt

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaProver, MerklePatriciaVerifier, OperationError}
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{MerklePatriciaTrie, Nibble}
import io.constellationnetwork.security.hex.Hex

import weaver.SimpleIOSuite

/**
 * Coverage for SHORT, VARIABLE-LENGTH and MUTUALLY-PREFIXING keys.
 *
 * The other MPT suites only exercise fixed 64-nibble hash digests, so the extension-split,
 * variable-length-leaf and prefix-collision paths are otherwise untested.
 */
object MerklePatriciaShortKeySuite extends SimpleIOSuite {

  private def buildIncrementally(entries: List[(Hex, String)]): IO[MerklePatriciaTrie] =
    entries match {
      case Nil => IO.raiseError(new RuntimeException("empty entries"))
      case (headKey, headVal) :: tail =>
        for {
          initial <- MerklePatriciaTrie.make[IO, String](Map(headKey -> headVal))
          result <- tail.foldLeftM(initial) { (trie, kv) =>
            io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaProducer
              .stateless[IO]
              .insert(trie, Map(kv._1 -> kv._2))
              .flatMap(IO.fromEither(_))
          }
        } yield result
    }

  test("two single-nibble keys form a branch of two leaves") {
    val entries = Map(Hex("a") -> "va", Hex("b") -> "vb")
    for {
      trie   <- MerklePatriciaTrie.make[IO, String](entries)
      values <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie).traverse(_.data.as[String]))
    } yield expect(values.toSet == Set("va", "vb"))
  }

  test("variable-length keys sharing a prefix are all retrievable") {
    // A prefix-free but mutually-prefixing, variable-length set ("ab.." family + an outlier).
    // None of these keys is a strict prefix of another, so all are representable as leaves.
    val entries = Map(
      Hex("abcd")   -> "v1",
      Hex("abce")   -> "v2",
      Hex("ab12")   -> "v3",
      Hex("abda99") -> "v4"
    )
    for {
      trie   <- MerklePatriciaTrie.make[IO, String](entries)
      values <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie).traverse(_.data.as[String]))
    } yield expect(values.toSet == entries.values.toSet)
  }

  test("prover and verifier round-trip on short variable-length keys") {
    val entries = Map(
      Hex("0a")   -> "alpha",
      Hex("0b")   -> "beta",
      Hex("1234") -> "gamma",
      Hex("12ff") -> "delta"
    )
    for {
      trie     <- MerklePatriciaTrie.make[IO, String](entries)
      prover   <- MerklePatriciaProver.make[IO](trie).pure[IO]
      verifier <- MerklePatriciaVerifier.make[IO](trie.rootNode.digest).pure[IO]
      results <- entries.keys.toList.traverse { k =>
        for {
          proof  <- prover.attestPath(k).flatMap(IO.fromEither(_))
          result <- verifier.confirm(proof)
        } yield result.isRight
      }
    } yield expect(results.forall(identity))
  }

  test("a key that is a strict prefix of an existing key is rejected (no value-at-branch)") {
    // Inserting "ab" when "abcd" exists exhausts the key at a branch node. The producer must
    // surface this as an OperationError (Left), not crash.
    for {
      trie <- MerklePatriciaTrie.make[IO, String](Map(Hex("abcd") -> "v1", Hex("abce") -> "v2"))
      result <- io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaProducer
        .stateless[IO]
        .insert(trie, Map(Hex("ab") -> "prefix"))
    } yield
      expect(result match {
        case Left(_: OperationError) => true
        case _                       => false
      })
  }

  test("a key for which an existing key is a strict prefix is rejected") {
    // The reverse direction: "ab" exists, inserting "abcd" descends past the leaf "ab" and the
    // extension/leaf logic exhausts at the resulting branch. Must be a Left, not a crash.
    for {
      trie <- MerklePatriciaTrie.make[IO, String](Map(Hex("ab") -> "short", Hex("cd") -> "other"))
      result <- io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaProducer
        .stateless[IO]
        .insert(trie, Map(Hex("abcd") -> "long"))
    } yield
      expect(result match {
        case Left(_: OperationError) => true
        case _                       => false
      })
  }

  test("single-nibble keys: extension split at full length stays consistent across orders") {
    // "1a","1b" share "1" -> Extension(shared="1", Branch{a,b}); inserting "2c" splits that
    // extension at its full length (sharedRemaining length 1). Must be order independent.
    val a = Hex("1a") -> "va"
    val b = Hex("1b") -> "vb"
    val c = Hex("2c") -> "vc"
    for {
      o1 <- buildIncrementally(List(a, b, c))
      o2 <- buildIncrementally(List(c, a, b))
    } yield expect(o1.rootNode.digest == o2.rootNode.digest)
  }

  test("empty remaining path at a branch yields a clean error rather than a malformed proof") {
    // Build a branch (two diverging single-nibble keys), then prove a path that lands on the
    // branch with no remaining nibbles. The prover should report a Left, not throw.
    for {
      trie   <- MerklePatriciaTrie.make[IO, String](Map(Hex("ab") -> "v1", Hex("ac") -> "v2"))
      prover <- MerklePatriciaProver.make[IO](trie).pure[IO]
      // "a" leads into the branch but has no further nibble for a leaf -> not found.
      proof <- prover.attestPath(Hex("a"))
    } yield expect(proof.isLeft)
  }

  test("nibble path order for short keys matches direct construction") {
    // Two keys that share a single-nibble prefix produce a config-A branch (no extension because
    // the split happens immediately after the shared nibble forms an extension of length 1).
    val entries = Map(Hex("1a") -> "va", Hex("1b") -> "vb")
    for {
      trie <- MerklePatriciaTrie.make[IO, String](entries)
      // Root must be an Extension(shared = [1], Branch{a,b}) per Patricia compression.
      rootIsExtension = trie.rootNode match {
        case ext: io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaNode.Extension =>
          ext.shared == Seq(Nibble.unsafe('1')) && ext.child.paths.keySet == Set(
            Nibble.unsafe('a'),
            Nibble.unsafe('b')
          )
        case _ => false
      }
    } yield expect(rootIsExtension)
  }
}
