package std

import java.nio.charset.StandardCharsets

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryCodec.{JsonBinaryDecodeOps, JsonBinaryEncodeOps}

import org.bouncycastle.util.encoders.Base64
import shared.Generators._
import shared.Models._
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object JsonBinaryCodecSuite extends SimpleIOSuite with Checkers {

  def assertRoundTrip[A](data: A)(implicit codec: JsonBinaryCodec[IO, A]): IO[Expectations] =
    for {
      serialized   <- codec.serialize(data)
      deserialized <- codec.deserialize(serialized)
    } yield expect.same(deserialized, Right(data))

  test("regular codec should round-trip arbitrary TestData") {
    forall { (testData: TestData) =>
      assertRoundTrip(testData)
    }
  }

  test("regular codec should handle empty strings") {
    val testData = TestData("", 0)
    assertRoundTrip(testData)
  }

  test("regular codec should handle special characters") {
    val specialChars = "!@#$%^&*()"
    val testData = TestData(specialChars, 123)
    assertRoundTrip(testData)
  }

  test("regular codec should fail gracefully with invalid input") {
    val invalidBytes = "invalid json".getBytes(StandardCharsets.UTF_8)
    JsonBinaryCodec[IO, TestData].deserialize(invalidBytes).map { result =>
      expect(result.isLeft)
    }
  }

  test("DataUpdate codec should round-trip arbitrary TestDataUpdate") {
    forall { (testUpdate: TestDataUpdate) =>
      assertRoundTrip(testUpdate)
    }
  }

  test("DataUpdate codec should include correct prefix") {
    forall { (testUpdate: TestDataUpdate) =>
      JsonBinaryCodec[IO, TestDataUpdate].serialize(testUpdate).map { serialized =>
        val serializedString = new String(serialized, StandardCharsets.UTF_8)
        expect(serializedString.startsWith("\u0019Constellation Signed Data:\n")) &&
        expect(serializedString.split("\n").length >= 3)
      }
    }
  }

  test("DataUpdate codec should handle Base64 encoding/decoding") {
    forall { (testUpdate: TestDataUpdate) =>
      for {
        serialized <- JsonBinaryCodec[IO, TestDataUpdate].serialize(testUpdate)
        serializedString = new String(serialized, StandardCharsets.UTF_8)
        base64Part = serializedString.split("\n").drop(2).mkString
        _            <- IO(Base64.decode(base64Part)) // Should not throw exception
        deserialized <- JsonBinaryCodec[IO, TestDataUpdate].deserialize(serialized)
      } yield expect.same(deserialized, Right(testUpdate))
    }
  }

  test("extension methods should work for regular data") {
    forall { (testData: TestData) =>
      for {
        serialized   <- testData.toBinary
        deserialized <- serialized.fromBinary[TestData]
      } yield expect.same(deserialized, Right(testData))
    }
  }

  test("extension methods should work for DataUpdate") {
    forall { (testUpdate: TestDataUpdate) =>
      for {
        serialized   <- testUpdate.toBinary
        deserialized <- serialized.fromBinary[TestDataUpdate]
      } yield expect.same(deserialized, Right(testUpdate))
    }
  }

  test("serialization should be deterministic") {
    forall { (testData: TestData) =>
      for {
        bytes1 <- JsonBinaryCodec[IO, TestData].serialize(testData)
        bytes2 <- JsonBinaryCodec[IO, TestData].serialize(testData)
      } yield expect(java.util.Arrays.equals(bytes1, bytes2))
    }
  }

  test("DataUpdate serialization should be deterministic") {
    forall { (testUpdate: TestDataUpdate) =>
      for {
        bytes1 <- JsonBinaryCodec[IO, TestDataUpdate].serialize(testUpdate)
        bytes2 <- JsonBinaryCodec[IO, TestDataUpdate].serialize(testUpdate)
      } yield expect(java.util.Arrays.equals(bytes1, bytes2))
    }
  }

  test("codec should satisfy identity law") {
    forall { (testData: TestData) =>
      for {
        serialized1  <- testData.toBinary
        deserialized <- serialized1.fromBinary[TestData]
        serialized2  <- deserialized.toOption.get.toBinary
      } yield expect(java.util.Arrays.equals(serialized1, serialized2))
    }
  }

  test("codec should drop null values from Option fields") {
    // TestDataComplex has nested: Option[TestData] which becomes null when None
    val dataWithNone = TestDataComplex("test", 42, None)
    val dataWithSome = TestDataComplex("test", 42, Some(TestData("inner", 1)))

    for {
      bytesNone <- dataWithNone.toBinary
      bytesSome <- dataWithSome.toBinary
      strNone = new String(bytesNone, StandardCharsets.UTF_8)
      strSome = new String(bytesSome, StandardCharsets.UTF_8)
    } yield
      // When nested is None, the "nested" key should not appear in the serialized form
      expect(!strNone.contains("nested")) &&
      expect(!strNone.contains("null")) &&
      // When nested has a value, it should appear
      expect(strSome.contains("nested")) &&
      expect(strSome.contains("inner"))
  }

  test("codec should round-trip data with Option fields") {
    forall { (testData: TestDataComplex) =>
      assertRoundTrip(testData)
    }
  }

  test("codec with None produces same bytes as sender without field") {
    // This simulates the signature compatibility scenario:
    // - TypeScript SDK sends JSON without optional field
    // - Scala deserializes to case class with None
    // - Scala re-serializes - should produce same bytes (no null field)
    val dataWithNone = TestDataComplex("test", 42, None)

    for {
      serialized <- dataWithNone.toBinary
      str = new String(serialized, StandardCharsets.UTF_8)
    } yield
      // The serialized form should not contain "nested" key at all
      // This ensures hash compatibility with senders who omit the field
      expect(!str.contains("\"nested\""))
  }

  test("DataUpdate codec should drop null values from Option fields") {
    // Same behavior as regular codec, but for DataUpdate path
    // Note: DataUpdate wraps in prefix + base64, so we decode to check content
    val dataWithNone = TestDataUpdateComplex("test", 42, None)
    val dataWithSome = TestDataUpdateComplex("test", 42, Some("meta"))

    for {
      bytesNone <- dataWithNone.toBinary
      bytesSome <- dataWithSome.toBinary
      // Extract base64 part and decode
      strNoneRaw = new String(bytesNone, StandardCharsets.UTF_8)
      strSomeRaw = new String(bytesSome, StandardCharsets.UTF_8)
      base64None = strNoneRaw.split("\n").drop(2).mkString
      base64Some = strSomeRaw.split("\n").drop(2).mkString
      decodedNone = new String(Base64.decode(base64None), StandardCharsets.UTF_8)
      decodedSome = new String(Base64.decode(base64Some), StandardCharsets.UTF_8)
    } yield
      // When metadata is None, the key should not appear in decoded content
      expect(!decodedNone.contains("metadata")) &&
      expect(!decodedNone.contains("null")) &&
      // When metadata has a value, it should appear
      expect(decodedSome.contains("metadata")) &&
      expect(decodedSome.contains("meta"))
  }

  test("DataUpdate codec should round-trip data with Option fields") {
    forall { (testData: TestDataUpdateComplex) =>
      assertRoundTrip(testData)
    }
  }
}
