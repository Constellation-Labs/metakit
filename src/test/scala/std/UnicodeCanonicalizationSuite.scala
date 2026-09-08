package std

import java.io.IOException

import cats.effect.IO
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.std.{JsonBinaryCodec, JsonBinaryHasher, JsonCanonicalizer}

import io.circe.{Json, parser}
import weaver.SimpleIOSuite

object UnicodeCanonicalizationSuite extends SimpleIOSuite {
  private val canonicalizer = JsonCanonicalizer.make[IO]

  pureTest("comparator strictly orders all adjacent single UTF-16 code units, including surrogates") {
    val comparisons = (0 until 65535).forall { n =>
      val a = n.toChar.toString
      val b = (n + 1).toChar.toString
      JsonCanonicalizer.keyOrdering.compare(a, b) < 0 && JsonCanonicalizer.keyOrdering.compare(b, a) > 0
    }
    expect(comparisons)
  }

  pureTest("raw UTF-16 ordering preserves distinct malformed keys and prefixes") {
    val ordered = List("", "a", "aa", "\ud800", "\ud800a", "\ud800\udc00", "\ud801", "\udbff", "\udc00", "\udfff", "\ue000", "\ufffd")
    expect
      .same(ordered.map(_.toList.map(_.toInt)), ordered.reverse.sorted(JsonCanonicalizer.keyOrdering).map(_.toList.map(_.toInt)))
      .and(expect.same(ordered.size, scala.collection.immutable.SortedSet.from(ordered)(JsonCanonicalizer.keyOrdering).size))
  }

  List(
    "\ud800",
    "\ud801",
    "\udbff",
    "\udc00",
    "\udfff",
    "x\ud800",
    "\ud800x",
    "\udc00\ud800",
    "\ud800\ud800",
    "\ud800\udc00\udc00"
  ).zipWithIndex.foreach {
    case (bad, index) =>
      test(s"JCS rejects malformed key/value/nested input $index before canonical bytes or digest") {
        val inputs = List(
          Json.fromString(bad),
          Json.obj(bad      -> Json.fromInt(1)),
          Json.obj("nested" -> Json.arr(Json.obj("value" -> Json.fromString(bad)))),
          Json.obj("nested" -> Json.arr(Json.obj(bad -> Json.fromInt(1))))
        )
        inputs.traverse { input =>
          for {
            raw    <- canonicalizer.canonicalize(input).attempt
            bytes  <- JsonBinaryCodec.derive[IO, Json].serialize(input).attempt
            digest <- JsonBinaryHasher[IO].computeDigest(input).attempt
          } yield
            expect(raw.swap.exists(_.isInstanceOf[IOException]))
              .and(expect(bytes.swap.exists(_.isInstanceOf[IOException])))
              .and(expect(digest.swap.exists(_.isInstanceOf[IOException])))
        }.map(_.reduce(_.and(_)))
      }
  }

  test("parsed malformed keys cannot collapse into an accepted JCS object") {
    List("{\"\\ud800\":1,\"\\ud801\":2}", "{\"\\ud801\":2,\"\\ud800\":1}").traverse { text =>
      for {
        input  <- IO.fromEither(parser.parse(text))
        result <- canonicalizer.canonicalize(input).attempt
      } yield expect(result.swap.exists(_.isInstanceOf[IOException]))
    }.map(_.reduce(_.and(_)))
  }

  test("all isolated surrogate code units reject as strings and keys, including null-valued raw keys") {
    (0xd800 to 0xdfff).toList.traverse { n =>
      val s = n.toChar.toString
      for {
        value <- canonicalizer.canonicalize(Json.fromString(s)).attempt
        key   <- canonicalizer.canonicalize(Json.obj(s -> Json.Null)).attempt
      } yield value.isLeft && key.isLeft
    }.map(results => expect(results.forall(identity)).and(expect.same(2048, results.size)))
  }

  test("valid surrogate pairs and replacement characters survive unchanged") {
    val strings = List("", "\ufffd", "\ud800\udc00", "\udbff\udfff", "x\ud83d\ude00y", "\ud83d\ude00\ud800\udc00")
    strings.traverse { s =>
      val input = Json.obj(s -> Json.fromString(s))
      for {
        encoded <- canonicalizer.canonicalize(input)
        decoded <- IO.fromEither(parser.parse(encoded.value))
      } yield expect.same(input, decoded)
    }.map(_.reduce(_.and(_)))
  }
}
