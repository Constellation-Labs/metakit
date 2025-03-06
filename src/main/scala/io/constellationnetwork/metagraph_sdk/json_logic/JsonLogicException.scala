package io.constellationnetwork.metagraph_sdk.json_logic

class JsonLogicException(msg: String) extends Exception(msg)

object JsonLogicException {
  def apply(msg: String): JsonLogicException = new JsonLogicException(msg)
}
