import org.scalacheck.Gen

package object generators {

  // Key-Value pairs generator
  val kvGen: Gen[(Int, String)] = for {
    key   <- Gen.posNum[Int]
    value <- Gen.alphaStr.suchThat(_.nonEmpty)
  } yield (key, value)

  // Generate a list of unique Key-Value pairs
  def kvListGenUniqueKeys(n: Int, start: Int = 1): Gen[List[(Int, String)]] =
    Gen.listOfN(n, Gen.alphaStr.suchThat(_.nonEmpty)).map { values =>
      (start until start + n).toList.zip(values)
    }

  def nonEmptyStringListGen(start: Int, end: Int): Gen[List[String]] = for {
    size <- Gen.chooseNum(start, end)
    list <- Gen.listOfN(size, Gen.alphaStr.suchThat(_.nonEmpty))
  } yield list
}
