package shared

import io.circe.Json
import org.scalacheck.{Arbitrary, Gen}
import shared.Models._

object Generators {

  val nonEmptyAlphaStr: Gen[String] =
    Gen.choose(1, 20).flatMap(n => Gen.listOfN(n, Gen.alphaChar).map(_.mkString))

  val nonEmptyAlphaNumStr: Gen[String] =
    Gen.choose(1, 20).flatMap(n => Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString))

  implicit val arbTestData: Arbitrary[TestData] = Arbitrary(
    for {
      id    <- Gen.stringOf(Gen.alphaNumChar)
      value <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    } yield TestData(id, value)
  )

  implicit val arbTestDataComplex: Arbitrary[TestDataComplex] = Arbitrary(
    for {
      id     <- Gen.stringOf(Gen.alphaNumChar)
      value  <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
      nested <- Gen.option(arbTestData.arbitrary)
    } yield TestDataComplex(id, value, nested)
  )

  implicit val arbTestDataUpdate: Arbitrary[TestDataUpdate] = Arbitrary(
    for {
      id    <- Gen.stringOf(Gen.alphaNumChar)
      value <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    } yield TestDataUpdate(id, value)
  )

  implicit val arbTestDataUpdateComplex: Arbitrary[TestDataUpdateComplex] = Arbitrary(
    for {
      id       <- Gen.stringOf(Gen.alphaNumChar)
      value    <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
      metadata <- Gen.option(nonEmptyAlphaStr)
    } yield TestDataUpdateComplex(id, value, metadata)
  )

  val genTestDataUpdate: Gen[TestDataUpdate] = arbTestDataUpdate.arbitrary

  val jsonValueGen: Gen[Json] = Gen.oneOf(
    nonEmptyAlphaStr.map(Json.fromString),
    Gen.chooseNum(-100, 100).map(Json.fromInt),
    Gen.oneOf(true, false).map(Json.fromBoolean),
    Gen.const(Json.Null)
  )

  val jsonArrayGen: Gen[Json] = Gen.listOf(jsonValueGen).map(Json.arr(_: _*))

  val jsonObjectGen: Gen[Json] = for {
    size   <- Gen.choose(0, 10)
    keys   <- Gen.listOfN(size, nonEmptyAlphaStr).map(_.distinct)
    values <- Gen.listOfN(keys.length, jsonValueGen)
  } yield Json.obj(keys.zip(values).map { case (k, v) => (k, v) }: _*)

  val complexJsonGen: Gen[Json] = for {
    mainValue <- Gen.oneOf(jsonValueGen, jsonArrayGen, jsonObjectGen)
    key       <- nonEmptyAlphaStr
  } yield Json.obj((key, mainValue))

  val kvGen: Gen[(Int, String)] = for {
    key   <- Gen.posNum[Int]
    value <- nonEmptyAlphaStr
  } yield (key, value)

  def kvListGenUniqueKeys(n: Int, start: Int = 1): Gen[List[(Int, String)]] =
    Gen.listOfN(n, nonEmptyAlphaStr).map { values =>
      (start until start + n).toList.zip(values)
    }

  def nonEmptyStringListGen(start: Int, end: Int): Gen[List[String]] = for {
    size <- Gen.chooseNum(start, end)
    list <- Gen.listOfN(size, nonEmptyAlphaStr)
  } yield list
}
