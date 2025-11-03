package io.constellationnetwork.metagraph_sdk.json_logic.syntax

import cats.Monad

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.{JsonLogicRuntime, ResultContext}
import io.constellationnetwork.metagraph_sdk.json_logic.semantics.JsonLogicSemantics

trait SemanticsSyntax {

  implicit class SemanticsSyntaxOps[F[_]: Monad, Result[_]: ResultContext](sem: JsonLogicSemantics[F, Result]) {

    def eval(expr: JsonLogicExpression, ctx: Option[JsonLogicValue] = None): F[Either[JsonLogicException, Result[JsonLogicValue]]] = {
      implicit val implicitSem: JsonLogicSemantics[F, Result] = sem
      JsonLogicRuntime.evaluate(expr, ctx)
    }
  }
}
