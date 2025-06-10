package crypto.mpt

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import xyz.kd5ujc.accumulators.mpt.{MerklePatriciaNode, MerklePatriciaTrie, Nibble}
import xyz.kd5ujc.binary.JsonSerializer
import xyz.kd5ujc.hash.impl.Blake2b256Hasher
import xyz.kd5ujc.hash.{Digest, l256}

import io.circe.syntax.EncoderOps
import org.bouncycastle.util.encoders.Hex
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerklePatriciaTrieSuite extends SimpleIOSuite with Checkers {

  private val hasherResource: Resource[IO, Blake2b256Hasher[IO]] =
    Resource.eval {
      JsonSerializer.forSync[IO].map { implicit json2bin =>
        new Blake2b256Hasher[IO]
      }
    }

  private val toDigest: String => l256 = (str: String) => l256.unsafe(Hex.decode(str))
  private val toNibbleSeq: String => Seq[Nibble] = (str: String) => scala.collection.immutable.ArraySeq.from(str.map(Nibble.unsafe))

  test("trie can be encoded and decoded from json") {
    hasherResource.use { implicit hasher =>
      forall(Gen.listOfN(32, Gen.long)) { listLong =>
        for {
          leafMap      <- listLong.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
          trieExpected <- MerklePatriciaTrie.create(leafMap)
          trieActual   <- IO.fromEither(trieExpected.asJson.as[MerklePatriciaTrie])
        } yield expect(trieExpected == trieActual)
      }
    }
  }

  test("root of trie is non-empty") {
    hasherResource.use { implicit hasher =>
      forall(Gen.listOfN(32, Gen.long)) { listLong =>
        for {
          leafMap <- listLong.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
          trie    <- MerklePatriciaTrie.create(leafMap)
        } yield expect(trie.rootNode.digest.value.nonEmpty)
      }
    }
  }

  test("trie from create contains all values in leaves") {
    hasherResource.use { implicit hasher =>
      forall(Gen.listOfN(32, Gen.long)) { listLong =>
        for {
          leafMap    <- listLong.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
          trie       <- MerklePatriciaTrie.create(leafMap)
          listLeaves <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie).traverse(_.data.as[Long]))
          sortedInputSet = SortedSet.from(listLong)
          sortedOutputSet = SortedSet.from(listLeaves)
        } yield expect(sortedInputSet == sortedOutputSet)
      }
    }
  }

  test("trie from insert contains all values in leaves") {
    val gen = for {
      list1 <- Gen.listOfN(32, Gen.long)
      list2 <- Gen.listOfN(32, Gen.long)
    } yield (list1, list2)

    hasherResource.use { implicit hasher =>
      forall(gen) {
        case (list1, list2) =>
          for {
            initMap    <- list1.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
            updMap     <- list2.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
            trie       <- MerklePatriciaTrie.create(initMap)
            trie2      <- MerklePatriciaTrie.insert(trie, updMap).flatMap(IO.fromEither(_))
            listLeaves <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie2).traverse(_.data.as[Long]))
            sortedInputSet = SortedSet.from(list1 ++ list2)
            sortedOutputSet = SortedSet.from(listLeaves)
          } yield expect(sortedInputSet == sortedOutputSet)
      }
    }
  }

  test("trie can remove leaves") {
    val gen = for {
      createList <- Gen.listOfN(32, Gen.long)
      removeList <- Gen.someOf(createList).map(_.toList)
    } yield (createList, removeList)

    hasherResource.use { implicit hasher =>
      forall(gen) {
        case (createList, removeList) =>
          for {
            createMap   <- createList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
            removePaths <- removeList.traverse(hasher.hash(_))
            trie1       <- MerklePatriciaTrie.create(createMap)
            trie2       <- MerklePatriciaTrie.remove(trie1, removePaths).flatMap(IO.fromEither(_))
            listLeaves  <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie2).traverse(_.data.as[Long]))
          } yield expect(listLeaves.forall(!removeList.contains(_)))
      }
    }
  }

  test("updating a trie with an existing path updates the data held by the leaf and changes the root node digest") {
    hasherResource.use { implicit hasher =>
      forall(Gen.long.flatMap(v1 => Gen.long.flatMap(v2 => (v1, v2))).suchThat(g => g._1 != g._2)) {
        case (val1, val2) =>
          for {
            path  <- IO.fromEither(l256.from(Array.fill(32)(1: Byte)).toEither.leftMap(err => new Exception(s"${err.toString}")))
            trie1 <- MerklePatriciaTrie.create[IO, Long](Map(path -> val1))
            trie2 <- MerklePatriciaTrie.insert[IO, Long](trie1, Map(path -> val2)).flatMap(IO.fromEither(_))
            (root1, data1, digest1) <- trie1.rootNode match {
              case MerklePatriciaNode.Leaf(_, _data, _digest) => IO.pure((trie1.rootNode.digest, _data, _digest))
              case _                                          => IO.raiseError(new Exception("unexpected root node found"))
            }
            (root2, data2, digest2) <- trie2.rootNode match {
              case MerklePatriciaNode.Leaf(_, _data, _digest) => IO.pure((trie2.rootNode.digest, _data, _digest))
              case _                                          => IO.raiseError(new Exception("unexpected root node found"))
            }
          } yield expect(root1 != root2 && data1 != data2 && digest1 != digest2)
      }
    }
  }

  test("create produces a trie with a known root digest") {
    hasherResource.use { implicit hasher =>
      for {
        leafMap <- (0 to 31).toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
        trie    <- MerklePatriciaTrie.create[IO, Int](leafMap)
      } yield expect(trie.rootNode.digest == toDigest("fddee3f51499aeb0077ef1ab2dd3756d5083a8912e9a8b83fae2d446e282fd57"))
    }
  }

  test("create then insert produces a trie with a known root digest") {
    hasherResource.use { implicit hasher =>
      for {
        leafMap   <- (0 to 31).toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
        trie      <- MerklePatriciaTrie.create[IO, Int](leafMap)
        newLeaves <- (-31 to -0).toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
        trie2     <- MerklePatriciaTrie.insert(trie, newLeaves).flatMap(IO.fromEither(_))
      } yield expect(trie2.rootNode.digest == toDigest("543e932b3a73fba7572d393d733b0400756e8cad163cc32303598e4ccf6395ed"))
    }
  }

  test("create then remove produces a trie with a known root digest") {
    hasherResource.use { implicit hasher =>
      for {
        leafMap   <- (0 to 31).toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
        trie      <- MerklePatriciaTrie.create[IO, Int](leafMap)
        remLeaves <- (17 to 31).toList.traverse(l => hasher.hash(l))
        trie2     <- MerklePatriciaTrie.remove(trie, remLeaves).flatMap(IO.fromEither(_))
      } yield expect(trie2.rootNode.digest == toDigest("59edbf8dbc3d09a6d4657ab3978971b792318d799e97d100b6a244aa018a2ee9"))
    }
  }

  test("create can produce a fixed simple trie with a single branch and multiple leaves") {
    hasherResource.use { implicit hasher =>
      for {
        content    <- ((1 to 5) ++ (7 to 9)).pure[IO]
        leafMap    <- content.toList.traverse(l => hasher.hash(l).map(_ -> l)).map(_.toMap)
        trieActual <- MerklePatriciaTrie.create[IO, Int](leafMap)
        trieExpected <- for {
          leafNodes <- leafMap.toList.traverse {
            case (digest, data) =>
              val path = Nibble(digest)
              val json = data.asJson
              MerklePatriciaNode.Leaf[IO](path.tail, json).map(leaf => Map(path.head -> leaf))
          }
          mergedLeafNodes = leafNodes.fold(Map.empty)(_ ++ _)
          branchNode <- MerklePatriciaNode.Branch[IO](mergedLeafNodes)
        } yield MerklePatriciaTrie(branchNode)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create produces a fixed complex trie with branches, extensions, and leaves") {
    val leafMap = Map[Digest, String](
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2") -> "are we done yet?",
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1") -> "yet another value",
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF") -> "a value",
      toDigest("1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF") -> "another value"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), "a value".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), "another value".asJson)
          leaf3 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("1"), "yet another value".asJson)
          leaf4 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("2"), "are we done yet?".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x0f: Byte) -> leaf3,
              Nibble.unsafe(0x0d: Byte) -> leaf4
            )
          )
          ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), branch1)
          branch2 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x01: Byte) -> leaf2,
              Nibble.unsafe(0x0a: Byte) -> ext
            )
          )
        } yield MerklePatriciaTrie(branch2)
      } yield expect(trieActual == trieExpected)
    }
  }

  /**
   * For a 2-leaf trie there are two possible configurations (B = Branch, L = Leaf, E = Extension)
   * Config A  | Config B  |
   * ----------|-----------|
   *     B     |     E     |
   *    / \    |     |     |
   *   L   L   |     B     |
   *           |    / \    |
   *           |   L   L   |
   * -----------------------
   */
  test("create produces a 2-leaf trie in configuration A") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> leaf2
            )
          )
        } yield MerklePatriciaTrie(branch1)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create produces a 2-leaf trie in configuration B") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("2"), "leaf 2".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x0a: Byte) -> leaf1,
              Nibble.unsafe(0x0b: Byte) -> leaf2
            )
          )
          ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), branch1)
        } yield MerklePatriciaTrie(ext)
      } yield expect(trieActual == trieExpected)
    }
  }

  /**
   * For a 3-leaf trie there are six possible configurations (B = Branch, L = Leaf, E = Extension)
   * Config A  | Config B  | Config C  | Config D  | Config E  | Config F  |
   * ----------|-----------|-----------|-----------|-----------|-----------|
   *     B     |     E     |      B    |      E    |      B    |      E    |
   *   / | \   |     |     |     / \   |      |    |     / \   |      |    |
   *  L  L  L  |     B     |    B   L  |      B    |    E   L  |      B    |
   *           |   / | \   |   / \     |     / \   |    |      |     / \   |
   *           |  L  L  L  |  L   L    |    B   L  |    B      |    E   L  |
   *           |           |           |   / \     |   / \     |    |      |
   *           |           |           |  L   L    |  L   L    |    B      |
   *           |           |           |           |           |   / \     |
   *           |           |           |           |           |  L   L    |
   * -----------------------------------------------------------------------
   */
  test("create produces a 3-leaf trie in configuration A") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> leaf2,
              Nibble.unsafe(0x0f: Byte) -> leaf3
            )
          )
        } yield MerklePatriciaTrie(branch1)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create produces a 3-leaf trie in configuration B") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x0a: Byte) -> leaf1,
              Nibble.unsafe(0x0b: Byte) -> leaf2,
              Nibble.unsafe(0x0c: Byte) -> leaf3
            )
          )
          ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), branch1)
        } yield MerklePatriciaTrie(ext)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create produces a 3-leaf trie in configuration C") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("AAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x0a: Byte) -> leaf2,
              Nibble.unsafe(0x0f: Byte) -> leaf3
            )
          )
          branch2 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> branch1
            )
          )
        } yield MerklePatriciaTrie(branch2)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create produces a 3-leaf trie in configuration D") {
    val leafMap = Map[Digest, String](
      toDigest("FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("FFAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("FFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x0a: Byte) -> leaf2,
              Nibble.unsafe(0x0f: Byte) -> leaf3
            )
          )
          branch2 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> branch1
            )
          )
          ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FF"), branch2)
        } yield MerklePatriciaTrie(ext)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create produces a 3-leaf trie in configuration E") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("AFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("AFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf2,
              Nibble.unsafe(0x0a: Byte) -> leaf3
            )
          )
          ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FF"), branch1)
          branch2 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> ext
            )
          )
        } yield MerklePatriciaTrie(branch2)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create produces a 3-leaf trie in configuration F") {
    val leafMap = Map[Digest, String](
      toDigest("FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("FFAFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie.create(leafMap)
        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf2,
              Nibble.unsafe(0x0a: Byte) -> leaf3
            )
          )
          ext1 <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FF"), branch1)
          branch2 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> ext1
            )
          )
          ext2 <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FF"), branch2)
        } yield MerklePatriciaTrie(ext2)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create then remove produces a 3-leaf trie in configuration A") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie
          .create(leafMap)
          .flatMap(MerklePatriciaTrie.remove(_, List(toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4"))))
          .flatMap(IO.fromEither(_))

        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> leaf2,
              Nibble.unsafe(0x0f: Byte) -> leaf3
            )
          )
        } yield MerklePatriciaTrie(branch1)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create then remove produces a 3-leaf trie in configuration B") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie
          .create(leafMap)
          .flatMap(trie =>
            MerklePatriciaTrie.remove(trie, List(toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4")))
          )
          .flatMap(IO.fromEither(_))

        trieExpected <- for {
          leaf1 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode.Leaf[IO](toNibbleSeq("3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x0a: Byte) -> leaf1,
              Nibble.unsafe(0x0b: Byte) -> leaf2,
              Nibble.unsafe(0x0c: Byte) -> leaf3
            )
          )
          ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"), branch1)
        } yield MerklePatriciaTrie(ext)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create then remove produces a 3-leaf trie in configuration C") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("AAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie
          .create(leafMap)
          .flatMap(trie =>
            MerklePatriciaTrie.remove(trie, List(toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4")))
          )
          .flatMap(IO.fromEither(_))

        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x0a: Byte) -> leaf2,
              Nibble.unsafe(0x0f: Byte) -> leaf3
            )
          )
          branch2 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> branch1
            )
          )
        } yield MerklePatriciaTrie(branch2)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create then remove produces a 3-leaf trie in configuration D") {
    val leafMap = Map[Digest, String](
      toDigest("FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("FFAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("FFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie
          .create(leafMap)
          .flatMap(MerklePatriciaTrie.remove(_, List(toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4"))))
          .flatMap(IO.fromEither(_))

        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x0a: Byte) -> leaf2,
              Nibble.unsafe(0x0f: Byte) -> leaf3
            )
          )
          branch2 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> branch1
            )
          )
          ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FF"), branch2)
        } yield MerklePatriciaTrie(ext)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create then remove produces a 3-leaf trie in configuration E") {
    val leafMap = Map[Digest, String](
      toDigest("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("AFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("AFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie
          .create(leafMap)
          .flatMap(MerklePatriciaTrie.remove(_, List(toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4"))))
          .flatMap(IO.fromEither(_))

        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf2,
              Nibble.unsafe(0x0a: Byte) -> leaf3
            )
          )
          ext <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FF"), branch1)
          branch2 <- MerklePatriciaNode.Branch[IO](
            Map(
              Nibble.unsafe(0x00: Byte) -> leaf1,
              Nibble.unsafe(0x0a: Byte) -> ext
            )
          )
        } yield MerklePatriciaTrie(branch2)
      } yield expect(trieActual == trieExpected)
    }
  }

  test("create then remove produces a 3-leaf trie in configuration F") {
    val leafMap = Map[Digest, String](
      toDigest("FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      toDigest("FFAFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    hasherResource.use { implicit hasher =>
      for {
        trieActual <- MerklePatriciaTrie
          .create(leafMap)
          .flatMap(MerklePatriciaTrie.remove(_, List(toDigest("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4"))))
          .flatMap(IO.fromEither(_))

        trieExpected <- for {
          leaf1 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"), "leaf 1".asJson)
          leaf2 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"), "leaf 2".asJson)
          leaf3 <- MerklePatriciaNode
            .Leaf[IO](toNibbleSeq("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"), "leaf 3".asJson)
          branch1 <- MerklePatriciaNode.Branch[IO](Map(Nibble.unsafe(0x00: Byte) -> leaf2, Nibble.unsafe(0x0a: Byte) -> leaf3))
          ext1    <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FF"), branch1)
          branch2 <- MerklePatriciaNode.Branch[IO](Map(Nibble.unsafe(0x00: Byte) -> leaf1, Nibble.unsafe(0x0a: Byte) -> ext1))
          ext2    <- MerklePatriciaNode.Extension[IO](toNibbleSeq("FF"), branch2)
        } yield MerklePatriciaTrie(ext2)
      } yield expect(trieActual == trieExpected)
    }
  }
}
