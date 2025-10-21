package io.constellationnetwork.metagraph_sdk.json_logic.syntax

import io.constellationnetwork.metagraph_sdk.json_logic.core._

trait ExpressionSyntax {

  implicit class ExpressionSyntaxOps(expr: JsonLogicExpression) {
    def bind(input: JsonLogicValue): (JsonLogicExpression, JsonLogicValue) = expr -> input
  }

  implicit class ListProgramSyntaxOps(exprs: List[JsonLogicExpression]) {
    def bind(inputs: List[JsonLogicValue]): List[(JsonLogicExpression, JsonLogicValue)] = exprs.zip(inputs)
  }
}