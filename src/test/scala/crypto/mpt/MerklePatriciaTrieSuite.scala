package crypto.mpt

import cats.effect.IO
import cats.syntax.all._

import scala.collection.immutable.SortedSet

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.MerklePatriciaProducer
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{MerklePatriciaNode, MerklePatriciaTrie, Nibble}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher.HasherOps
import io.constellationnetwork.security.hash.Hash

import io.circe.syntax.EncoderOps
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerklePatriciaTrieSuite extends SimpleIOSuite with Checkers {

  test("trie can be encoded and decoded from json") {
    forall(Gen.listOfN(32, Gen.long)) { listLong =>
      for {
        leafMap      <- listLong.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
        trieExpected <- MerklePatriciaTrie.make(leafMap)
        trieActual   <- IO.fromEither(trieExpected.asJson.as[MerklePatriciaTrie])
      } yield expect(trieExpected == trieActual)
    }
  }

  test("root of trie is non-empty") {
    forall(Gen.listOfN(32, Gen.long)) { listLong =>
      for {
        leafMap <- listLong.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
        trie    <- MerklePatriciaTrie.make(leafMap)
      } yield expect(trie.rootNode.digest.value.nonEmpty)
    }
  }

  test("trie from create contains all values in leaves") {
    forall(Gen.listOfN(32, Gen.long)) { listLong =>
      for {
        leafMap    <- listLong.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
        trie       <- MerklePatriciaTrie.make(leafMap)
        listLeaves <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie).traverse(_.data.as[Long]))
        sortedInputSet = SortedSet.from(listLong)
        sortedOutputSet = SortedSet.from(listLeaves)
      } yield expect(sortedInputSet == sortedOutputSet)
    }
  }

  test("trie from insert contains all values in leaves") {
    val gen = for {
      list1 <- Gen.listOfN(32, Gen.long)
      list2 <- Gen.listOfN(32, Gen.long)
    } yield (list1, list2)

    forall(gen) {
      case (list1, list2) =>
        for {
          initMap    <- list1.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
          updMap     <- list2.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
          trie       <- MerklePatriciaTrie.make(initMap)
          trie2      <- MerklePatriciaProducer.stateless[IO].insert(trie, updMap).flatMap(IO.fromEither(_))
          listLeaves <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie2).traverse(_.data.as[Long]))
          sortedInputSet = SortedSet.from(list1 ++ list2)
          sortedOutputSet = SortedSet.from(listLeaves)
        } yield expect(sortedInputSet == sortedOutputSet)
    }
  }

  test("trie can remove leaves") {
    val gen = for {
      createList <- Gen.listOfN(32, Gen.long)
      removeList <- Gen.someOf(createList).map(_.toList)
    } yield (createList, removeList)

    forall(gen) {
      case (createList, removeList) =>
        for {
          createMap   <- createList.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
          removePaths <- removeList.traverse[IO, Hash](el => el.computeDigest)
          trie1       <- MerklePatriciaTrie.make(createMap)
          trie2       <- MerklePatriciaProducer.stateless[IO].remove(trie1, removePaths).flatMap(IO.fromEither(_))
          listLeaves  <- IO.fromEither(MerklePatriciaTrie.collectLeafNodes(trie2).traverse(_.data.as[Long]))
        } yield expect(listLeaves.forall(!removeList.contains(_)))
    }
  }

  test("updating a trie with an existing path updates the data held by the leaf and changes the root node digest") {
    forall(Gen.long.flatMap(v1 => Gen.long.flatMap(v2 => (v1, v2))).suchThat(g => g._1 != g._2)) {
      case (val1, val2) =>
        for {
          path  <- Hash(Array.fill(32)('1').mkString).pure[IO]
          trie1 <- MerklePatriciaTrie.make[IO, Long](Map(path -> val1))
          trie2 <- MerklePatriciaProducer.stateless[IO].insert[Long](trie1, Map(path -> val2)).flatMap(IO.fromEither(_))
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

  test("create produces a trie with a known root digest") {
    for {
      leafMap <- (0 to 31).toList.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
      trie    <- MerklePatriciaTrie.make[IO, Int](leafMap)
    } yield expect(trie.rootNode.digest == Hash("2c225239414a82ea1b72061de98199f90e910106b9e9896bd6df4cc74e6c39a0"))

  }

  test("create then insert produces a trie with a known root digest") {
    for {
      leafMap   <- (0 to 31).toList.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
      trie      <- MerklePatriciaTrie.make[IO, Int](leafMap)
      newLeaves <- (-31 to -0).toList.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
      trie2     <- MerklePatriciaProducer.stateless[IO].insert(trie, newLeaves).flatMap(IO.fromEither(_))
    } yield expect(trie2.rootNode.digest == Hash("f01117b41e875b6f432e12a10b340ddd0cafa077a4b9b82aed688695adf58c45"))
  }

  test("create then remove produces a trie with a known root digest") {
    for {
      leafMap   <- (0 to 31).toList.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
      trie      <- MerklePatriciaTrie.make[IO, Int](leafMap)
      remLeaves <- (17 to 31).toList.traverse[IO, Hash](el => el.computeDigest)
      trie2     <- MerklePatriciaProducer.stateless[IO].remove(trie, remLeaves).flatMap(IO.fromEither(_))
    } yield expect(trie2.rootNode.digest == Hash("dd0c87acf3b891f2461cb9776a1e3376216b156ff0bebdb6b7c1b4c6f8ee9f35"))
  }

  // this test relies on the happenstance that SHA256 hash of each (1,2,3,4,7,8,9) don't share a common prefix
  test("create can produce a fixed simple trie with a single branch and multiple leaves") {
    for {
      content    <- List(1, 2, 3, 5, 7, 8, 9).pure[IO]
      leafMap    <- content.traverse(el => el.computeDigest.map(_ -> el)).map(_.toMap)
      trieActual <- MerklePatriciaTrie.make[IO, Int](leafMap)
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

  test("create produces a fixed complex trie with branches, extensions, and leaves") {
    val leafMap = Map[Hash, String](
      Hash("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2") -> "are we done yet?",
      Hash("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1") -> "yet another value",
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF") -> "a value",
      Hash("1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF") -> "another value"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
        )
        leaf2Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
        )
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("1"))
        leaf4Rem <- IO.fromEither(Nibble.fromHexString("2"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "a value".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "another value".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "yet another value".asJson)
        leaf4    <- MerklePatriciaNode.Leaf[IO](leaf4Rem, "are we done yet?".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x0f: Byte) -> leaf3,
            Nibble.unsafe(0x0d: Byte) -> leaf4
          )
        )
        extShared <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
        )
        ext <- MerklePatriciaNode.Extension[IO](extShared, branch1)
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
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1")
        )
        leaf2Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2")
        )
        leaf1 <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2 <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x00: Byte) -> leaf1,
            Nibble.unsafe(0x0a: Byte) -> leaf2
          )
        )
      } yield MerklePatriciaTrie(branch1)
    } yield expect(trieActual == trieExpected)

  }

  test("create produces a 2-leaf trie in configuration B") {
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(Nibble.fromHexString("1"))
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("2"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x0a: Byte) -> leaf1,
            Nibble.unsafe(0x0b: Byte) -> leaf2
          )
        )
        extRem <- IO.fromEither(Nibble.fromHexString("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))
        ext    <- MerklePatriciaNode.Extension[IO](extRem, branch1)
      } yield MerklePatriciaTrie(ext)
    } yield expect(trieActual == trieExpected)

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
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1")
        )
        leaf2Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2")
        )
        leaf3Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3")
        )
        leaf1 <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2 <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3 <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
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

  test("create produces a 3-leaf trie in configuration B") {
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(Nibble.fromHexString("1"))
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("2"))
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("3"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x0a: Byte) -> leaf1,
            Nibble.unsafe(0x0b: Byte) -> leaf2,
            Nibble.unsafe(0x0c: Byte) -> leaf3
          )
        )
        extRem <- IO.fromEither(Nibble.fromHexString("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))
        ext    <- MerklePatriciaNode.Extension[IO](extRem, branch1)
      } yield MerklePatriciaTrie(ext)
    } yield expect(trieActual == trieExpected)
  }

  test("create produces a 3-leaf trie in configuration C") {
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("AAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1")
        )
        leaf2Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2")
        )
        leaf3Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3")
        )
        leaf1 <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2 <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3 <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
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

  test("create produces a 3-leaf trie in configuration D") {
    val leafMap = Map[Hash, String](
      Hash("FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("FFAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("FFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"))
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"))
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
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
        extRem <- IO.fromEither(Nibble.fromHexString("FF"))
        ext    <- MerklePatriciaNode.Extension[IO](extRem, branch2)
      } yield MerklePatriciaTrie(ext)
    } yield expect(trieActual == trieExpected)

  }

  test("create produces a 3-leaf trie in configuration E") {
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("AFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("AFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1")
        )
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"))
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x00: Byte) -> leaf2,
            Nibble.unsafe(0x0a: Byte) -> leaf3
          )
        )
        extRem <- IO.fromEither(Nibble.fromHexString("FF"))
        ext    <- MerklePatriciaNode.Extension[IO](extRem, branch1)
        branch2 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x00: Byte) -> leaf1,
            Nibble.unsafe(0x0a: Byte) -> ext
          )
        )
      } yield MerklePatriciaTrie(branch2)
    } yield expect(trieActual == trieExpected)

  }

  test("create produces a 3-leaf trie in configuration F") {
    val leafMap = Map[Hash, String](
      Hash("FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("FFAFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("FFAFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3"
    )

    for {
      trieActual <- MerklePatriciaTrie.make(leafMap)
      trieExpected <- for {
        leaf1Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"))
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"))
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x00: Byte) -> leaf2,
            Nibble.unsafe(0x0a: Byte) -> leaf3
          )
        )
        ext1Rem <- IO.fromEither(Nibble.fromHexString("FF"))
        ext1    <- MerklePatriciaNode.Extension[IO](ext1Rem, branch1)
        branch2 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x00: Byte) -> leaf1,
            Nibble.unsafe(0x0a: Byte) -> ext1
          )
        )
        ext2Rem <- IO.fromEither(Nibble.fromHexString("FF"))
        ext2    <- MerklePatriciaNode.Extension[IO](ext2Rem, branch2)
      } yield MerklePatriciaTrie(ext2)
    } yield expect(trieActual == trieExpected)

  }

  test("create then remove produces a 3-leaf trie in configuration A") {
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    for {
      trieActual <- MerklePatriciaTrie
        .make(leafMap)
        .flatMap(
          MerklePatriciaProducer
            .stateless[IO]
            .remove(_, List(Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4")))
        )
        .flatMap(IO.fromEither(_))

      trieExpected <- for {
        leaf1Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1")
        )
        leaf2Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2")
        )
        leaf3Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3")
        )
        leaf1 <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2 <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3 <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
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

  test("create then remove produces a 3-leaf trie in configuration B") {
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    for {
      trieActual <- MerklePatriciaTrie
        .make(leafMap)
        .flatMap(trie =>
          MerklePatriciaProducer
            .stateless[IO]
            .remove(trie, List(Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4")))
        )
        .flatMap(IO.fromEither(_))

      trieExpected <- for {
        leaf1Rem <- IO.fromEither(Nibble.fromHexString("1"))
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("2"))
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("3"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x0a: Byte) -> leaf1,
            Nibble.unsafe(0x0b: Byte) -> leaf2,
            Nibble.unsafe(0x0c: Byte) -> leaf3
          )
        )
        extRem <- IO.fromEither(Nibble.fromHexString("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))
        ext    <- MerklePatriciaNode.Extension[IO](extRem, branch1)
      } yield MerklePatriciaTrie(ext)
    } yield expect(trieActual == trieExpected)
  }

  test("create then remove produces a 3-leaf trie in configuration C") {
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("AAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    for {
      trieActual <- MerklePatriciaTrie
        .make(leafMap)
        .flatMap(trie =>
          MerklePatriciaProducer
            .stateless[IO]
            .remove(trie, List(Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4")))
        )
        .flatMap(IO.fromEither(_))

      trieExpected <- for {
        leaf1Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1")
        )
        leaf2Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2")
        )
        leaf3Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3")
        )
        leaf1 <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2 <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3 <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
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

  test("create then remove produces a 3-leaf trie in configuration D") {
    val leafMap = Map[Hash, String](
      Hash("FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("FFAAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("FFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    for {
      trieActual <- MerklePatriciaTrie
        .make(leafMap)
        .flatMap(
          MerklePatriciaProducer
            .stateless[IO]
            .remove(_, List(Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4")))
        )
        .flatMap(IO.fromEither(_))

      trieExpected <- for {
        leaf1Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"))
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"))
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
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
        extRem <- IO.fromEither(Nibble.fromHexString("FF"))
        ext    <- MerklePatriciaNode.Extension[IO](extRem, branch2)
      } yield MerklePatriciaTrie(ext)
    } yield expect(trieActual == trieExpected)
  }

  test("create then remove produces a 3-leaf trie in configuration E") {
    val leafMap = Map[Hash, String](
      Hash("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("AFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("AFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    for {
      trieActual <- MerklePatriciaTrie
        .make(leafMap)
        .flatMap(
          MerklePatriciaProducer
            .stateless[IO]
            .remove(_, List(Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4")))
        )
        .flatMap(IO.fromEither(_))

      trieExpected <- for {
        leaf1Rem <- IO.fromEither(
          Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1")
        )
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"))
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x00: Byte) -> leaf2,
            Nibble.unsafe(0x0a: Byte) -> leaf3
          )
        )
        extRem <- IO.fromEither(Nibble.fromHexString("FF"))
        ext    <- MerklePatriciaNode.Extension[IO](extRem, branch1)
        branch2 <- MerklePatriciaNode.Branch[IO](
          Map(
            Nibble.unsafe(0x00: Byte) -> leaf1,
            Nibble.unsafe(0x0a: Byte) -> ext
          )
        )
      } yield MerklePatriciaTrie(branch2)
    } yield expect(trieActual == trieExpected)
  }

  test("create then remove produces a 3-leaf trie in configuration F") {
    val leafMap = Map[Hash, String](
      Hash("FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1") -> "leaf 1",
      Hash("FFAFF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2") -> "leaf 2",
      Hash("FFAFFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3") -> "leaf 3",
      Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4") -> "leaf 4"
    )

    for {
      trieActual <- MerklePatriciaTrie
        .make(leafMap)
        .flatMap(
          MerklePatriciaProducer
            .stateless[IO]
            .remove(_, List(Hash("FFAFFAFFFFFFFFFFFFFFFFFFFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD4")))
        )
        .flatMap(IO.fromEither(_))

      trieExpected <- for {
        leaf1Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA1"))
        leaf2Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB2"))
        leaf3Rem <- IO.fromEither(Nibble.fromHexString("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC3"))
        leaf1    <- MerklePatriciaNode.Leaf[IO](leaf1Rem, "leaf 1".asJson)
        leaf2    <- MerklePatriciaNode.Leaf[IO](leaf2Rem, "leaf 2".asJson)
        leaf3    <- MerklePatriciaNode.Leaf[IO](leaf3Rem, "leaf 3".asJson)
        branch1 <- MerklePatriciaNode.Branch[IO](
          Map(Nibble.unsafe(0x00: Byte) -> leaf2, Nibble.unsafe(0x0a: Byte) -> leaf3)
        )
        ext1Rem <- IO.fromEither(Nibble.fromHexString("FF"))
        ext1    <- MerklePatriciaNode.Extension[IO](ext1Rem, branch1)
        branch2 <- MerklePatriciaNode.Branch[IO](
          Map(Nibble.unsafe(0x00: Byte) -> leaf1, Nibble.unsafe(0x0a: Byte) -> ext1)
        )
        ext2Rem <- IO.fromEither(Nibble.fromHexString("FF"))
        ext2    <- MerklePatriciaNode.Extension[IO](ext2Rem, branch2)
      } yield MerklePatriciaTrie(ext2)
    } yield expect(trieActual == trieExpected)
  }

  test("create, insert, then remove produces original root hash") {
    val initialLeafMap = Map[Hash, String](
      Hash("AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD1") -> "initial value 1",
      Hash("BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD2") -> "initial value 2"
    )

    val insertKey = Hash("CFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD3")
    val insertValue = "inserted value"

    for {
      trie1 <- MerklePatriciaTrie.make(initialLeafMap)
      root1 = trie1.rootNode.digest

      trie2 <- MerklePatriciaProducer
        .stateless[IO]
        .insert(trie1, Map(insertKey -> insertValue))
        .flatMap(IO.fromEither(_))
      root2 = trie2.rootNode.digest

      trie3 <- MerklePatriciaProducer.stateless[IO].remove(trie2, List(insertKey)).flatMap(IO.fromEither(_))
      root3 = trie3.rootNode.digest

    } yield expect(root1 == root3 && root1 != root2 && root2 != root3)
  }
}
