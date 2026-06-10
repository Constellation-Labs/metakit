package crypto.mpt

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.MerklePatriciaTrie
import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaProducer
import io.constellationnetwork.security.hex.Hex

import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * Determinism-focused tests for the Merkle Patricia Trie producer.
 *
 *   - Item 1.3: inserting a key that splits an Extension at its FULL shared length must not
 *     introduce a degenerate zero-length Extension node. Incremental and fresh builds of the
 *     same key-set must agree.
 *   - Item 1.2: the resulting root digest must not depend on insertion order. Several shuffles
 *     of the same key-set, and incremental vs full rebuild, must produce identical roots.
 */
object MerklePatriciaDeterminismSuite extends SimpleIOSuite with Checkers {

  /** Build a trie from a list of (key, value) entries by folding `insert` in the given order. */
  private def buildIncrementally(entries: List[(Hex, String)]): IO[MerklePatriciaTrie] =
    entries match {
      case Nil => IO.raiseError(new RuntimeException("empty entries"))
      case (headKey, headVal) :: tail =>
        for {
          initial <- MerklePatriciaTrie.make[IO, String](Map(headKey -> headVal))
          result <- tail.foldLeftM(initial) { (trie, kv) =>
            MerklePatriciaProducer
              .stateless[IO]
              .insert(trie, Map(kv._1 -> kv._2))
              .flatMap(IO.fromEither(_))
          }
        } yield result
    }

  // ---------------------------------------------------------------------------
  // Item 1.3 - zero-length Extension when splitting an extension at full length
  // ---------------------------------------------------------------------------

  test("inserting a key that splits an Extension at its full shared length is deterministic") {
    // "abcd" and "abce" share prefix "abc" -> Extension(shared = "abc", Branch{d, e}).
    // Inserting "abf0" shares only "ab" with that extension; sharedRemaining = "c" (length 1),
    // so sharedRemaining.tail is empty. The unguarded producer wraps the surviving child Branch
    // in a degenerate Extension("", child), changing the digest.
    //
    // The "split" order below inserts the two extension-forming keys FIRST, then the splitter,
    // which forces the full-length split. The "no-split" order inserts the splitter first, so the
    // extension is never created. Both orders must yield the same root.
    val k1 = Hex("abcd")
    val k2 = Hex("abce")
    val k3 = Hex("abf0")

    val splitOrder = List(k1 -> "v1", k2 -> "v2", k3 -> "v3")
    val noSplitOrder = List(k3 -> "v3", k1 -> "v1", k2 -> "v2")

    for {
      split   <- buildIncrementally(splitOrder)
      noSplit <- buildIncrementally(noSplitOrder)
      fresh   <- MerklePatriciaTrie.make[IO, String](splitOrder.toMap)
    } yield
      expect.all(
        split.rootNode.digest == noSplit.rootNode.digest,
        split.rootNode.digest == fresh.rootNode.digest
      )
  }

  test("splitting an Extension at full length keeps all leaves and a consistent root") {
    // Same split scenario, then verify every key is still present after the incremental build
    // and that the incremental build matches the fresh build exactly (structure + digest).
    val entries = List(
      Hex("abcd1") -> "v1",
      Hex("abce2") -> "v2",
      Hex("abf03") -> "v3",
      Hex("abf14") -> "v4"
    )

    for {
      incremental <- buildIncrementally(entries)
      fresh       <- MerklePatriciaTrie.make[IO, String](entries.toMap)
      leaves = MerklePatriciaTrie.collectLeafNodes(incremental)
      values <- IO.fromEither(leaves.traverse(_.data.as[String]))
    } yield
      expect.all(
        incremental == fresh,
        values.toSet == entries.map(_._2).toSet
      )
  }

  // ---------------------------------------------------------------------------
  // Item 1.2 - insertion-order independence of the root digest
  // ---------------------------------------------------------------------------

  test("root digest is independent of insertion order (example, variable-length keys)") {
    // Variable-length, mutually-prefixing-but-not-equal keys that exercise extension splits at
    // their full shared length. None of these keys is a strict prefix of another, so all are
    // representable as leaves. Before the zero-length-Extension fix this set produced different
    // roots depending on insertion order.
    val entries = List(
      Hex("abcd")   -> "v1",
      Hex("abce")   -> "v2",
      Hex("abf0")   -> "v3",
      Hex("a1cc")   -> "v4",
      Hex("abda99") -> "v5",
      Hex("00aa")   -> "v6"
    )

    val permutations = List(
      entries,
      entries.reverse,
      entries.sortBy(_._1.value),
      entries.sortBy(-_._1.value.length)
    )

    for {
      roots <- permutations.traverse(p => buildIncrementally(p).map(_.rootNode.digest))
    } yield expect(roots.distinct.size == 1)
  }

  test("root digest is independent of insertion order (property over shuffles)") {
    // Generate a set of distinct fixed-length hex keys (fixed length => no key is a strict prefix
    // of another, so every key is representable as a leaf), then build the trie from several
    // random shuffles. All roots must be identical, and must equal the full `create` rebuild.
    val hexChar: Gen[Char] = Gen.oneOf("0123456789abcdef".toList)
    val keyGen: Gen[String] = Gen.listOfN(4, hexChar).map(_.mkString)

    val keysGen: Gen[List[String]] =
      Gen.choose(3, 12).flatMap(n => Gen.listOfN(n, keyGen)).map(_.distinct)

    forall(keysGen) { keys =>
      val entries = keys.zipWithIndex.map { case (k, i) => Hex(k) -> s"value_$i" }

      val shuffles = List(
        entries,
        entries.reverse,
        scala.util.Random.shuffle(entries),
        scala.util.Random.shuffle(entries)
      )

      for {
        fresh    <- MerklePatriciaTrie.make[IO, String](entries.toMap)
        incRoots <- shuffles.traverse(s => buildIncrementally(s).map(_.rootNode.digest))
        allRoots = fresh.rootNode.digest :: incRoots
      } yield expect(allRoots.distinct.size == 1)
    }
  }

  test("removing keys in different orders yields the same root") {
    // Build a 6-key trie, then remove the same 3 keys in two different orders. The resulting root
    // must be identical and equal to building the surviving 3-key set from scratch.
    val all = List(
      Hex("abcd")   -> "v1",
      Hex("abce")   -> "v2",
      Hex("abf0")   -> "v3",
      Hex("a1cc")   -> "v4",
      Hex("abda99") -> "v5",
      Hex("00aa")   -> "v6"
    )
    val toRemove = List(Hex("abce"), Hex("abf0"), Hex("abda99"))
    val survivors = all.filterNot { case (k, _) => toRemove.contains(k) }

    def removeInOrder(order: List[Hex]): IO[MerklePatriciaTrie] =
      MerklePatriciaTrie
        .make[IO, String](all.toMap)
        .flatMap(trie => MerklePatriciaProducer.stateless[IO].remove(trie, order))
        .flatMap(IO.fromEither(_))

    for {
      r1    <- removeInOrder(toRemove)
      r2    <- removeInOrder(toRemove.reverse)
      fresh <- MerklePatriciaTrie.make[IO, String](survivors.toMap)
    } yield
      expect.all(
        r1.rootNode.digest == r2.rootNode.digest,
        r1.rootNode.digest == fresh.rootNode.digest
      )
  }
}
