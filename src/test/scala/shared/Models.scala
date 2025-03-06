package shared

import io.constellationnetwork.currency.dataApplication.DataUpdate

import derevo.cats.show
import derevo.circe.magnolia.{decoder, encoder}
import derevo.derive

object Models {

  @derive(encoder, decoder, show)
  case class TestData(id: String, value: Int)

  @derive(encoder, decoder, show)
  case class TestDataComplex(id: String, value: Int, nested: Option[TestData])

  @derive(encoder, decoder, show)
  case class TestDataUpdate(id: String, value: Int) extends DataUpdate

}
