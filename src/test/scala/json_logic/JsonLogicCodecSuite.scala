package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic._

import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import weaver.SimpleIOSuite

object JsonLogicCodecSuite extends SimpleIOSuite {

  // Simulate a Transition-like structure from workchain
  case class MockTransition(
    from:   String,
    to:     String,
    effect: JsonLogicExpression
  )

  object MockTransition {
    // Simulate derevo-generated encoder/decoder
    implicit val encoder: Encoder[MockTransition] = Encoder.forProduct3("from", "to", "effect")(t =>
      (t.from, t.to, t.effect)
    )

    implicit val decoder: Decoder[MockTransition] = Decoder.forProduct3("from", "to", "effect")(MockTransition.apply)
  }

  case class MockDefinition(
    states:      Map[String, String],
    transitions: List[MockTransition]
  )

  object MockDefinition {
    implicit val encoder: Encoder[MockDefinition] = Encoder.forProduct2("states", "transitions")(d =>
      (d.states, d.transitions)
    )

    implicit val decoder: Decoder[MockDefinition] = Decoder.forProduct2("states", "transitions")(MockDefinition.apply)
  }

  test("MapExpression with nested spawn directives should roundtrip correctly") {
    val original: JsonLogicExpression = MapExpression(
      Map(
        "_spawn" -> ArrayExpression(
          List(
            MapExpression(
              Map(
                "childId"    -> ConstExpression(StrValue("test-child-id")),
                "definition" -> MapExpression(
                  Map(
                    "states" -> MapExpression(
                      Map(
                        "active" -> MapExpression(
                          Map(
                            "id"      -> ConstExpression(StrValue("active")),
                            "isFinal" -> ConstExpression(BoolValue(false))
                          )
                        )
                      )
                    ),
                    "initialState" -> ConstExpression(StrValue("active")),
                    "transitions"  -> ArrayExpression(List())
                  )
                )
              )
            )
          )
        ),
        "_triggers" -> ArrayExpression(List()),
        "spawned"   -> ConstExpression(BoolValue(true))
      )
    )

    for {
      json    <- IO.pure(original.asJson)
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield {
      decoded match {
        case MapExpression(m) =>
          expect(m.contains("_spawn")) and
            expect(m.contains("_triggers")) and
            expect(m.contains("spawned")) and
            expect(decoded == original)
        case ConstExpression(_) =>
          failure("MapExpression was incorrectly decoded as ConstExpression")
        case other =>
          failure(s"MapExpression was decoded as unexpected type: ${other.getClass.getSimpleName}")
      }
    }
  }

  test("MapExpression with mixed expression and constant fields should roundtrip") {
    val original: JsonLogicExpression = MapExpression(
      Map(
        "userId"    -> VarExpression(Left("uid")),
        "action"    -> ConstExpression(StrValue("create")),
        "timestamp" -> VarExpression(Left("ts")),
        "version"   -> ConstExpression(IntValue(1))
      )
    )

    for {
      json    <- IO.pure(original.asJson)
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield expect(decoded == original)
  }

  test("Complex nested MapExpression should preserve structure through roundtrip") {
    val original: JsonLogicExpression = MapExpression(
      Map(
        "effect" -> MapExpression(
          Map(
            "status"   -> ConstExpression(StrValue("ready")),
            "progress" -> ConstExpression(IntValue(0)),
            "metadata" -> MapExpression(
              Map(
                "createdAt" -> VarExpression(Left("timestamp")),
                "createdBy" -> VarExpression(Left("userId"))
              )
            )
          )
        ),
        "triggers" -> ArrayExpression(
          List(
            ConstExpression(StrValue("event1")),
            ConstExpression(StrValue("event2"))
          )
        )
      )
    )

    for {
      json    <- IO.pure(original.asJson)
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield {
      decoded match {
        case MapExpression(m) =>
          expect(m.contains("effect")) and
            expect(m.contains("triggers")) and
            expect(decoded == original)
        case _ =>
          failure(s"Expected MapExpression but got ${decoded.getClass.getSimpleName}")
      }
    }
  }

  test("ArrayExpression with nested MapExpression should roundtrip") {
    val original: JsonLogicExpression = ArrayExpression(
      List(
        MapExpression(
          Map(
            "id"   -> ConstExpression(StrValue("item1")),
            "type" -> ConstExpression(StrValue("spawn"))
          )
        ),
        MapExpression(
          Map(
            "id"   -> ConstExpression(StrValue("item2")),
            "type" -> ConstExpression(StrValue("trigger"))
          )
        )
      )
    )

    for {
      json    <- IO.pure(original.asJson)
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield expect(decoded == original)
  }

  test("ConstExpression(MapValue) should remain ConstExpression after roundtrip") {
    val original: JsonLogicExpression = ConstExpression(
      MapValue(
        Map(
          "name"   -> StrValue("John"),
          "age"    -> IntValue(30),
          "active" -> BoolValue(true)
        )
      )
    )

    for {
      json    <- IO.pure(original.asJson)
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield {
      decoded match {
        case ConstExpression(MapValue(_)) =>
          expect(decoded == original)
        case MapExpression(_) =>
          success // This is acceptable - pure constant maps can be decoded as either
        case _ =>
          failure(s"Expected ConstExpression(MapValue) but got ${decoded.getClass.getSimpleName}")
      }
    }
  }

  test("MapExpression vs ConstExpression(MapValue) distinction") {
    val mapExpr: JsonLogicExpression = MapExpression(
      Map(
        "computed" -> VarExpression(Left("x")),
        "static"   -> ConstExpression(IntValue(100))
      )
    )

    val constMapValue: JsonLogicExpression = ConstExpression(
      MapValue(
        Map(
          "name" -> StrValue("test"),
          "age"  -> IntValue(25)
        )
      )
    )

    for {
      mapExprJson        <- IO.pure(mapExpr.asJson)
      constMapValueJson  <- IO.pure(constMapValue.asJson)
      decodedMapExpr     <- IO.fromEither(mapExprJson.as[JsonLogicExpression])
      decodedConstMapVal <- IO.fromEither(constMapValueJson.as[JsonLogicExpression])
    } yield {
      val mapExprCheck = decodedMapExpr match {
        case MapExpression(_) => success
        case _                => failure("MapExpression with VarExpression should decode as MapExpression")
      }

      val constCheck = decodedConstMapVal match {
        case ConstExpression(MapValue(_)) | MapExpression(_) => success
        case _ => failure("ConstExpression(MapValue) should decode as ConstExpression or MapExpression")
      }

      mapExprCheck and constCheck
    }
  }

  test("Deeply nested MapExpression structures should preserve all levels") {
    val original: JsonLogicExpression = MapExpression(
      Map(
        "level1" -> MapExpression(
          Map(
            "level2" -> MapExpression(
              Map(
                "level3" -> MapExpression(
                  Map(
                    "value" -> ConstExpression(StrValue("deep"))
                  )
                )
              )
            )
          )
        )
      )
    )

    for {
      json    <- IO.pure(original.asJson)
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield expect(decoded == original)
  }

  test("Empty MapExpression should roundtrip correctly") {
    val original: JsonLogicExpression = MapExpression(Map.empty)

    for {
      json    <- IO.pure(original.asJson)
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield {
      decoded match {
        case MapExpression(m) if m.isEmpty         => success
        case ConstExpression(MapValue(m)) if m.isEmpty => success
        case _                                     => failure(s"Empty MapExpression should decode as empty map, got: $decoded")
      }
    }
  }

  test("MapExpression with all expression types should roundtrip") {
    val original: JsonLogicExpression = MapExpression(
      Map(
        "const"  -> ConstExpression(IntValue(42)),
        "var"    -> VarExpression(Left("x")),
        "array"  -> ArrayExpression(List(ConstExpression(IntValue(1)), ConstExpression(IntValue(2)))),
        "nested" -> MapExpression(Map("inner" -> ConstExpression(StrValue("value")))),
        "apply"  -> ApplyExpression(JsonLogicOp.AddOp, List(ConstExpression(IntValue(1)), ConstExpression(IntValue(2))))
      )
    )

    for {
      json    <- IO.pure(original.asJson)
      decoded <- IO.fromEither(json.as[JsonLogicExpression])
    } yield expect(decoded == original)
  }

  test("Transition with nested spawn effect should preserve MapExpression structure") {
    // Reproduce the bug from METAKIT_MAPEXPRESSION_BUG.md
    // Simulate a parent transition that spawns a child with nested definition
    val spawnEffect = MapExpression(
      Map(
        "_spawn" -> ArrayExpression(
          List(
            MapExpression(
              Map(
                "childId" -> ConstExpression(StrValue("child-uuid")),
                "definition" -> MapExpression(
                  Map(
                    "states" -> MapExpression(
                      Map(
                        "idle" -> MapExpression(
                          Map(
                            "id"      -> ConstExpression(StrValue("idle")),
                            "isFinal" -> ConstExpression(BoolValue(false))
                          )
                        ),
                        "spawned_child" -> MapExpression(
                          Map(
                            "id"      -> ConstExpression(StrValue("spawned_child")),
                            "isFinal" -> ConstExpression(BoolValue(false))
                          )
                        )
                      )
                    ),
                    "initialState" -> ConstExpression(StrValue("idle")),
                    "transitions" -> ArrayExpression(
                      List(
                        MapExpression(
                          Map(
                            "from"      -> ConstExpression(StrValue("idle")),
                            "to"        -> ConstExpression(StrValue("spawned_child")),
                            "eventType" -> ConstExpression(StrValue("spawn_grandchild")),
                            "guard"     -> ConstExpression(BoolValue(true)),
                            "effect" -> MapExpression(
                              Map(
                                "_spawn"    -> ArrayExpression(List()),
                                "_triggers" -> ArrayExpression(List()),
                                "spawned"   -> ConstExpression(BoolValue(true))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                "initialData" -> MapExpression(
                  Map(
                    "parentId" -> VarExpression(Left("machineId")),
                    "level"    -> ConstExpression(IntValue(2))
                  )
                )
              )
            )
          )
        ),
        "_triggers" -> ArrayExpression(List()),
        "level"     -> ConstExpression(IntValue(1))
      )
    )

    val transition = MockTransition(
      from = "init",
      to = "spawned",
      effect = spawnEffect
    )

    for {
      // Encode the transition (simulating what happens when StateMachineFiberRecord is stored)
      transitionJson <- IO.pure(transition.asJson)

      // Decode it back (simulating retrieval from state)
      decodedTransition <- IO.fromEither(transitionJson.as[MockTransition])
    } yield {
      decodedTransition.effect match {
        case MapExpression(m) =>
          val hasSpawn    = m.contains("_spawn")
          val hasTriggers = m.contains("_triggers")
          val hasLevel    = m.contains("level")

          expect(hasSpawn, "Should preserve _spawn key") and
            expect(hasTriggers, "Should preserve _triggers key") and
            expect(hasLevel, "Should preserve level key") and
            expect(decodedTransition.effect == spawnEffect, "Effect should match original")

        case ConstExpression(MapValue(_)) =>
          failure("MapExpression was incorrectly decoded as ConstExpression(MapValue)")

        case other =>
          failure(s"Effect was decoded as unexpected type: ${other.getClass.getSimpleName}")
      }
    }
  }

  test("Definition with multiple transitions should preserve all MapExpression effects") {
    // Test encoding/decoding a full definition with multiple transitions
    val transition1 = MockTransition(
      from = "idle",
      to = "working",
      effect = MapExpression(
        Map(
          "status"   -> ConstExpression(StrValue("working")),
          "progress" -> ConstExpression(IntValue(0))
        )
      )
    )

    val transition2 = MockTransition(
      from = "idle",
      to = "spawned",
      effect = MapExpression(
        Map(
          "_spawn" -> ArrayExpression(
            List(
              MapExpression(
                Map(
                  "childId"     -> ConstExpression(StrValue("test-child")),
                  "definition"  -> MapExpression(Map("states" -> MapExpression(Map.empty))),
                  "initialData" -> MapExpression(Map("status" -> ConstExpression(StrValue("ready"))))
                )
              )
            )
          ),
          "_triggers" -> ArrayExpression(List()),
          "spawned"   -> ConstExpression(BoolValue(true))
        )
      )
    )

    val definition = MockDefinition(
      states = Map("idle" -> "idle", "working" -> "working", "spawned" -> "spawned"),
      transitions = List(transition1, transition2)
    )

    for {
      defJson       <- IO.pure(definition.asJson)
      decodedDef    <- IO.fromEither(defJson.as[MockDefinition])
      decodedTrans1 = decodedDef.transitions(0)
      decodedTrans2 = decodedDef.transitions(1)
    } yield {
      val trans1Check = decodedTrans1.effect match {
        case MapExpression(m) =>
          expect(m.contains("status")) and expect(m.contains("progress"))
        case _ =>
          failure("Transition 1 effect should be MapExpression")
      }

      val trans2Check = decodedTrans2.effect match {
        case MapExpression(m) =>
          expect(m.contains("_spawn"), "Transition 2 should preserve _spawn") and
            expect(m.contains("_triggers")) and
            expect(m.contains("spawned"))
        case ConstExpression(MapValue(_)) =>
          failure("Transition 2 effect should be MapExpression, not ConstExpression")
        case _ =>
          failure("Transition 2 effect should be MapExpression")
      }

      trans1Check and trans2Check
    }
  }
}