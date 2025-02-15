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
}
