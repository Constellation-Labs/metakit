package json_logic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Locale

import cats.effect.IO
import cats.syntax.all._

import scala.collection.immutable.{HashMap, ListMap}

import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.json_logic.gas.GasLimit
import io.constellationnetwork.metagraph_sdk.json_logic.runtime.JsonLogicEvaluator

import io.circe.{Decoder, Json, parser}
import weaver.{Expectations, SimpleIOSuite}

object CanonicalOrderingLocaleSuite extends SimpleIOSuite {
  private val evaluator = JsonLogicEvaluator.tailRecursive[IO]
  private val recursive = JsonLogicEvaluator.recursive[IO]

  private def check(op: String, input: JsonLogicValue, expected: JsonLogicValue): IO[(Expectations, Long)] =
    for {
      expr <- IO.fromEither(parser.parse(s"""{"$op":[{"var":"input"}]}""").flatMap(_.as[JsonLogicExpression]))
      data = MapValue(Map("input" -> input))
      ordinary      <- evaluator.evaluate(expr, data, None)
      direct        <- recursive.evaluate(expr, data, None)
      metered       <- evaluator.evaluateWithGas(expr, data, None, GasLimit.Default)
      directMetered <- recursive.evaluateWithGas(expr, data, None, GasLimit.Default)
      result        <- IO.fromEither(metered)
      directResult  <- IO.fromEither(directMetered)
    } yield
      (
        expect
          .same(Right(expected), ordinary)
          .and(expect.same(Right(expected), direct))
          .and(expect.same(expected, result.value))
          .and(expect.same(expected, directResult.value))
          .and(expect.same(result.gasUsed, directResult.gasUsed)),
        result.gasUsed.amount
      )

  // This expectation is deliberately independent of the production comparator.
  private def expected(op: String, sorted: List[(String, JsonLogicValue)]): JsonLogicValue =
    ArrayValue(op match {
      case "keys"    => sorted.map { case (key, _) => StrValue(key) }
      case "values"  => sorted.map(_._2)
      case "entries" => sorted.map { case (key, value) => ArrayValue(List(StrValue(key), value)) }
      case other     => throw new IllegalArgumentException(s"Unexpected test operator: $other")
    })

  private def checkOrders(
    op: String,
    sorted: List[(String, JsonLogicValue)],
    orders: List[List[(String, JsonLogicValue)]]
  ): IO[Expectations] = {
    val maps = orders.flatMap(entries => List[Map[String, JsonLogicValue]](ListMap.from(entries), HashMap.from(entries)))
    maps.traverse(m => check(op, MapValue(m), expected(op, sorted))).map { results =>
      results.map(_._1).reduce(_.and(_)).and(expect.same(1, results.map(_._2).distinct.size))
    }
  }

  List("keys", "values", "entries").foreach { op =>
    test(s"$op: all 120 insertion orders, ListMap and HashMap, ordinary/direct/gas") {
      val sorted = List("alpha" -> 50, "bravo" -> 10, "charlie" -> 40, "delta" -> 20, "echo" -> 30).map {
        case (key, value) => key -> (IntValue(value): JsonLogicValue)
      }
      val orders = sorted.permutations.toList
      checkOrders(op, sorted, orders).map(_.and(expect.same(120, orders.size)))
    }

    test(s"$op: 128-key maps, every rotation and reversal, ordinary/direct/gas") {
      val sorted = (0 until 128).toList.map { i =>
        f"key-$i%03d" -> (IntValue((i * 53) % 128): JsonLogicValue)
      }
      val orders = sorted.indices.toList.flatMap { i =>
        val rotated = sorted.drop(i) ++ sorted.take(i)
        List(rotated, rotated.reverse)
      }
      checkOrders(op, sorted, orders)
    }

    test(s"$op: empty and singleton objects") {
      List(List.empty[(String, JsonLogicValue)], List("only" -> NullValue)).traverse { rows =>
        checkOrders(op, rows, List(rows))
      }.map(_.reduce(_.and(_)))
    }
  }

  final private case class VectorCase(name: String, op: String, input: Json, expected: Json)
  implicit private val vectorDecoder: Decoder[VectorCase] = Decoder.forProduct4("name", "op", "input", "expected")(VectorCase.apply)
  private val vectors = parser
    .decode[List[VectorCase]](
      new String(Files.readAllBytes(Paths.get("src/test/resources/conformance/ordering_case_vectors.json")), StandardCharsets.UTF_8)
    )
    .fold(throw _, identity)

  vectors.foreach { vector =>
    test(s"cross-language vector: ${vector.name}") {
      for {
        input    <- IO.fromEither(vector.input.as[JsonLogicValue])
        expected <- IO.fromEither(vector.expected.as[JsonLogicValue])
        result   <- check(vector.op, input, expected)
      } yield result._1
    }
  }

  test("locale fork uses the requested JVM default; never mutate the shared JVM locale") {
    IO.pure(sys.props.get("metakit.test.expectedLanguage").fold(expect(vectors.nonEmpty)) { language =>
      expect.same(language, Locale.getDefault.getLanguage)
    })
  }
}
