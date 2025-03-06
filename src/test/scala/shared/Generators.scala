package shared

import io.circe.Json
import org.scalacheck.{Arbitrary, Gen}
import shared.Models._

object Generators {

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

  val genTestDataUpdate: Gen[TestDataUpdate] = arbTestDataUpdate.arbitrary

  val jsonValueGen: Gen[Json] = Gen.oneOf(
    Gen.alphaStr.suchThat(_.nonEmpty).map(Json.fromString),
    Gen.chooseNum(-100, 100).map(Json.fromInt),
    Gen.oneOf(true, false).map(Json.fromBoolean),
    Gen.const(Json.Null)
  )

  val jsonArrayGen: Gen[Json] = Gen.listOf(jsonValueGen).map(Json.arr(_: _*))

  val jsonObjectGen: Gen[Json] = for {
    keys   <- Gen.listOf(Gen.alphaStr.suchThat(_.nonEmpty)).map(_.distinct)
    values <- Gen.listOfN(keys.length, jsonValueGen)
  } yield Json.obj(keys.zip(values).map { case (k, v) => (k, v) }: _*)

  val complexJsonGen: Gen[Json] = for {
    mainValue <- Gen.oneOf(jsonValueGen, jsonArrayGen, jsonObjectGen)
    key       <- Gen.alphaStr.suchThat(_.nonEmpty)
  } yield Json.obj((key, mainValue))

  val kvGen: Gen[(Int, String)] = for {
    key   <- Gen.posNum[Int]
    value <- Gen.alphaStr.suchThat(_.nonEmpty)
  } yield (key, value)

  def kvListGenUniqueKeys(n: Int, start: Int = 1): Gen[List[(Int, String)]] =
    Gen.listOfN(n, Gen.alphaStr.suchThat(_.nonEmpty)).map { values =>
      (start until start + n).toList.zip(values)
    }

  def nonEmptyStringListGen(start: Int, end: Int): Gen[List[String]] = for {
    size <- Gen.chooseNum(start, end)
    list <- Gen.listOfN(size, Gen.alphaStr.suchThat(_.nonEmpty))
  } yield list
}
