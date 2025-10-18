package json_logic

import cats.effect.IO

import io.constellationnetwork.metagraph_sdk.json_logic._

import io.circe.parser
import weaver.scalacheck.Checkers
import weaver.{Expectations, SimpleIOSuite}

object ComplexContractsSuite extends SimpleIOSuite with Checkers {

  def parseTestJson(expr: String, data: String): IO[(JsonLogicExpression, JsonLogicValue)] = for {
    expr <- IO.fromEither(parser.parse(expr).flatMap(_.as[JsonLogicExpression]))
    data <- IO.fromEither(parser.parse(data).flatMap(_.as[JsonLogicValue]))
  } yield (expr, data)

  private def testRunner(
    expr: JsonLogicExpression,
    data: JsonLogicValue,
    expected: JsonLogicValue
  ): IO[Expectations] =
    JsonLogicEvaluator
      .tailRecursive[IO]
      .evaluate(expr, data, None)
      .map(actual => expect(actual == expected))

  val escrowContractExpr: String =
    """
      |{"if":[
      |  {"==":[{"var":"method"},"create"]},
      |  {"if":[
      |    {"and":[
      |      {"exists":[{"var":"buyer"},{"var":"seller"},{"var":"amount"},{"var":"arbiter"}]},
      |      {">=":[{"var":"amount"},0]},
      |      {"in":[{"var":"buyer"},{"var":"participants"}]},
      |      {"in":[{"var":"seller"},{"var":"participants"}]},
      |      {"in":[{"var":"arbiter"},{"var":"participants"}]}
      |    ]},
      |    {
      |      "buyer": {"var":"buyer"},
      |      "seller": {"var":"seller"},
      |      "arbiter": {"var":"arbiter"},
      |      "amount": {"var":"amount"},
      |      "status": "pending",
      |      "deposited": 0
      |    },
      |    "Failed create checks"
      |  ]},
      |
      |  {"==":[{"var":"method"},"deposit"]},
      |  {"if":[
      |    {"and":[
      |      {"==":[{"var":"content.status"},"pending"]},
      |      {"==":[{"var":"depositor"},{"var":"content.buyer"}]},
      |      {"==":[{"var":"depositAmount"},{"var":"content.amount"}]},
      |      {"in":[{"var":"depositor"},{"var":"participants"}]}
      |    ]},
      |    {
      |      "buyer": {"var":"content.buyer"},
      |      "seller": {"var":"content.seller"},
      |      "arbiter": {"var":"content.arbiter"},
      |      "amount": {"var":"content.amount"},
      |      "status": "deposited",
      |      "deposited": {"var":"depositAmount"}
      |    },
      |    "Failed deposit checks"
      |  ]},
      |
      |  {"==":[{"var":"method"},"release"]},
      |  {"if":[
      |    {"and":[
      |      {"==":[{"var":"content.status"},"deposited"]},
      |      {"or":[
      |        {"==":[{"var":"releaser"},{"var":"content.buyer"}]},
      |        {"==":[{"var":"releaser"},{"var":"content.arbiter"}]}
      |      ]},
      |      {"in":[{"var":"releaser"},{"var":"participants"}]}
      |    ]},
      |    {
      |      "buyer": {"var":"content.buyer"},
      |      "seller": {"var":"content.seller"},
      |      "arbiter": {"var":"content.arbiter"},
      |      "amount": {"var":"content.amount"},
      |      "status": "completed",
      |      "deposited": {"var":"content.deposited"},
      |      "releasedTo": {"var":"content.seller"}
      |    },
      |    "Failed release checks"
      |  ]},
      |
      |  {"==":[{"var":"method"},"refund"]},
      |  {"if":[
      |    {"and":[
      |      {"==":[{"var":"content.status"},"deposited"]},
      |      {"or":[
      |        {"==":[{"var":"refunder"},{"var":"content.seller"}]},
      |        {"==":[{"var":"refunder"},{"var":"content.arbiter"}]}
      |      ]},
      |      {"in":[{"var":"refunder"},{"var":"participants"}]}
      |    ]},
      |    {
      |      "buyer": {"var":"content.buyer"},
      |      "seller": {"var":"content.seller"},
      |      "arbiter": {"var":"content.arbiter"},
      |      "amount": {"var":"content.amount"},
      |      "status": "refunded",
      |      "deposited": {"var":"content.deposited"},
      |      "refundedTo": {"var":"content.buyer"}
      |    },
      |    "Failed refund checks"
      |  ]},
      |
      |  "Unknown method"
      |]}
      |""".stripMargin

  val votingContractExpr: String =
    """
      |{"if":[
      |  {"==":[{"var":"method"},"create"]},
      |  {"if":[
      |    {"and":[
      |      {"exists":[{"var":"title"},{"var":"options"},{"var":"endTime"}]},
      |      {">":[{"var":"endTime"},{"var":"currentTime"}]}
      |    ]},
      |    {
      |      "title": {"var":"title"},
      |      "options": {"var":"options"},
      |      "voteCount": 0,
      |      "endTime": {"var":"endTime"},
      |      "active": true
      |    },
      |    "Failed create checks"
      |  ]},
      |
      |  {"==":[{"var":"method"},"vote"]},
      |  {"if":[
      |    {"and":[
      |      {"==":[{"var":"content.active"},true]},
      |      {"<":[{"var":"currentTime"},{"var":"content.endTime"}]},
      |      {"in":[{"var":"option"},{"var":"content.options"}]},
      |      {"in":[{"var":"voter"},{"var":"participants"}]}
      |    ]},
      |    {
      |      "title": {"var":"content.title"},
      |      "options": {"var":"content.options"},
      |      "voteCount": {"+":[{"var":"content.voteCount"},1]},
      |      "lastVoter": {"var":"voter"},
      |      "lastOption": {"var":"option"},
      |      "endTime": {"var":"content.endTime"},
      |      "active": {"var":"content.active"}
      |    },
      |    "Failed vote checks"
      |  ]},
      |
      |  {"==":[{"var":"method"},"close"]},
      |  {"if":[
      |    {"and":[
      |      {"==":[{"var":"content.active"},true]},
      |      {">=":[{"var":"currentTime"},{"var":"content.endTime"}]}
      |    ]},
      |    {
      |      "title": {"var":"content.title"},
      |      "options": {"var":"content.options"},
      |      "voteCount": {"var":"content.voteCount"},
      |      "endTime": {"var":"content.endTime"},
      |      "active": false
      |    },
      |    "Failed close checks"
      |  ]},
      |
      |  "Unknown method"
      |]}
      |""".stripMargin

  val marketplaceContractExpr: String =
    """
      |{"if":[
      |  {"==":[{"var":"method"},"createListing"]},
      |  {"if":[
      |    {"and":[
      |      {"exists":[{"var":"itemId"},{"var":"seller"},{"var":"price"},{"var":"description"}]},
      |      {">":[{"var":"price"},0]},
      |      {"in":[{"var":"seller"},{"var":"participants"}]}
      |    ]},
      |    {
      |      "itemId": {"var":"itemId"},
      |      "seller": {"var":"seller"},
      |      "price": {"var":"price"},
      |      "description": {"var":"description"},
      |      "status": "active",
      |      "offerCount": 0
      |    },
      |    "Failed createListing checks"
      |  ]},
      |
      |  {"==":[{"var":"method"},"makeOffer"]},
      |  {"if":[
      |    {"and":[
      |      {"==":[{"var":"content.status"},"active"]},
      |      {"exists":[{"var":"buyer"},{"var":"offerPrice"}]},
      |      {">":[{"var":"offerPrice"},0]},
      |      {"in":[{"var":"buyer"},{"var":"participants"}]},
      |      {"!=":[{"var":"buyer"},{"var":"content.seller"}]}
      |    ]},
      |    {
      |      "itemId": {"var":"content.itemId"},
      |      "seller": {"var":"content.seller"},
      |      "price": {"var":"content.price"},
      |      "description": {"var":"content.description"},
      |      "status": {"var":"content.status"},
      |      "offerCount": {"+":[{"var":"content.offerCount"},1]},
      |      "lastOfferBuyer": {"var":"buyer"},
      |      "lastOfferPrice": {"var":"offerPrice"}
      |    },
      |    "Failed makeOffer checks"
      |  ]},
      |
      |  {"==":[{"var":"method"},"acceptOffer"]},
      |  {"if":[
      |    {"and":[
      |      {"==":[{"var":"content.status"},"active"]},
      |      {"==":[{"var":"acceptor"},{"var":"content.seller"}]},
      |      {"in":[{"var":"acceptor"},{"var":"participants"}]},
      |      {">":[{"var":"content.offerCount"},0]}
      |    ]},
      |    {
      |      "itemId": {"var":"content.itemId"},
      |      "seller": {"var":"content.seller"},
      |      "price": {"var":"content.price"},
      |      "description": {"var":"content.description"},
      |      "status": "sold",
      |      "offerCount": {"var":"content.offerCount"},
      |      "acceptedBuyer": {"var":"content.lastOfferBuyer"},
      |      "acceptedPrice": {"var":"content.lastOfferPrice"}
      |    },
      |    "Failed acceptOffer checks"
      |  ]},
      |
      |  {"==":[{"var":"method"},"cancelListing"]},
      |  {"if":[
      |    {"and":[
      |      {"==":[{"var":"content.status"},"active"]},
      |      {"==":[{"var":"canceller"},{"var":"content.seller"}]},
      |      {"in":[{"var":"canceller"},{"var":"participants"}]}
      |    ]},
      |    {
      |      "itemId": {"var":"content.itemId"},
      |      "seller": {"var":"content.seller"},
      |      "price": {"var":"content.price"},
      |      "description": {"var":"content.description"},
      |      "status": "cancelled",
      |      "offerCount": {"var":"content.offerCount"}
      |    },
      |    "Failed cancelListing checks"
      |  ]},
      |
      |  "Unknown method"
      |]}
      |""".stripMargin

  test("escrow contract: create escrow with valid participants") {
    val dataStr =
      """
        |{
        |  "method": "create",
        |  "buyer": "Alice",
        |  "seller": "Bob",
        |  "arbiter": "Charlie",
        |  "amount": 1000,
        |  "participants": ["Alice", "Bob", "Charlie"]
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "buyer": "Alice",
        |  "seller": "Bob",
        |  "arbiter": "Charlie",
        |  "amount": 1000,
        |  "status": "pending",
        |  "deposited": 0
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(escrowContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("escrow contract: buyer deposits funds") {
    val dataStr =
      """
        |{
        |  "method": "deposit",
        |  "depositor": "Alice",
        |  "depositAmount": 1000,
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "buyer": "Alice",
        |    "seller": "Bob",
        |    "arbiter": "Charlie",
        |    "amount": 1000,
        |    "status": "pending",
        |    "deposited": 0
        |  }
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "buyer": "Alice",
        |  "seller": "Bob",
        |  "arbiter": "Charlie",
        |  "amount": 1000,
        |  "status": "deposited",
        |  "deposited": 1000
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(escrowContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("escrow contract: buyer releases funds to seller") {
    val dataStr =
      """
        |{
        |  "method": "release",
        |  "releaser": "Alice",
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "buyer": "Alice",
        |    "seller": "Bob",
        |    "arbiter": "Charlie",
        |    "amount": 1000,
        |    "status": "deposited",
        |    "deposited": 1000
        |  }
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "buyer": "Alice",
        |  "seller": "Bob",
        |  "arbiter": "Charlie",
        |  "amount": 1000,
        |  "status": "completed",
        |  "deposited": 1000,
        |  "releasedTo": "Bob"
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(escrowContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("escrow contract: arbiter can release funds") {
    val dataStr =
      """
        |{
        |  "method": "release",
        |  "releaser": "Charlie",
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "buyer": "Alice",
        |    "seller": "Bob",
        |    "arbiter": "Charlie",
        |    "amount": 1000,
        |    "status": "deposited",
        |    "deposited": 1000
        |  }
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "buyer": "Alice",
        |  "seller": "Bob",
        |  "arbiter": "Charlie",
        |  "amount": 1000,
        |  "status": "completed",
        |  "deposited": 1000,
        |  "releasedTo": "Bob"
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(escrowContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("escrow contract: seller initiates refund to buyer") {
    val dataStr =
      """
        |{
        |  "method": "refund",
        |  "refunder": "Bob",
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "buyer": "Alice",
        |    "seller": "Bob",
        |    "arbiter": "Charlie",
        |    "amount": 1000,
        |    "status": "deposited",
        |    "deposited": 1000
        |  }
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "buyer": "Alice",
        |  "seller": "Bob",
        |  "arbiter": "Charlie",
        |  "amount": 1000,
        |  "status": "refunded",
        |  "deposited": 1000,
        |  "refundedTo": "Alice"
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(escrowContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("voting contract: create proposal with valid options") {
    val dataStr =
      """
        |{
        |  "method": "create",
        |  "title": "Project Funding",
        |  "options": ["Approve", "Reject", "Defer"],
        |  "currentTime": 1000,
        |  "endTime": 2000
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "title": "Project Funding",
        |  "options": ["Approve", "Reject", "Defer"],
        |  "voteCount": 0,
        |  "endTime": 2000,
        |  "active": true
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(votingContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("voting contract: participant casts vote") {
    val dataStr =
      """
        |{
        |  "method": "vote",
        |  "voter": "Alice",
        |  "option": "Approve",
        |  "currentTime": 1500,
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "title": "Project Funding",
        |    "options": ["Approve", "Reject", "Defer"],
        |    "voteCount": 0,
        |    "endTime": 2000,
        |    "active": true
        |  }
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "title": "Project Funding",
        |  "options": ["Approve", "Reject", "Defer"],
        |  "voteCount": 1,
        |  "lastVoter": "Alice",
        |  "lastOption": "Approve",
        |  "endTime": 2000,
        |  "active": true
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(votingContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("marketplace contract: create item listing") {
    val dataStr =
      """
        |{
        |  "method": "createListing",
        |  "itemId": "item-001",
        |  "seller": "Alice",
        |  "price": 500,
        |  "description": "Vintage bicycle",
        |  "participants": ["Alice", "Bob", "Charlie"]
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "itemId": "item-001",
        |  "seller": "Alice",
        |  "price": 500,
        |  "description": "Vintage bicycle",
        |  "status": "active",
        |  "offerCount": 0
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(marketplaceContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("marketplace contract: buyer makes offer") {
    val dataStr =
      """
        |{
        |  "method": "makeOffer",
        |  "buyer": "Bob",
        |  "offerPrice": 450,
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "itemId": "item-001",
        |    "seller": "Alice",
        |    "price": 500,
        |    "description": "Vintage bicycle",
        |    "status": "active",
        |    "offerCount": 0
        |  }
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "itemId": "item-001",
        |  "seller": "Alice",
        |  "price": 500,
        |  "description": "Vintage bicycle",
        |  "status": "active",
        |  "offerCount": 1,
        |  "lastOfferBuyer": "Bob",
        |  "lastOfferPrice": 450
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(marketplaceContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("marketplace contract: seller accepts offer") {
    val dataStr =
      """
        |{
        |  "method": "acceptOffer",
        |  "acceptor": "Alice",
        |  "offerIndex": 0,
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "itemId": "item-001",
        |    "seller": "Alice",
        |    "price": 500,
        |    "description": "Vintage bicycle",
        |    "status": "active",
        |    "offerCount": 1,
        |    "lastOfferBuyer": "Bob",
        |    "lastOfferPrice": 450
        |  }
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "itemId": "item-001",
        |  "seller": "Alice",
        |  "price": 500,
        |  "description": "Vintage bicycle",
        |  "status": "sold",
        |  "offerCount": 1,
        |  "acceptedBuyer": "Bob",
        |  "acceptedPrice": 450
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(marketplaceContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("marketplace contract: seller cancels listing") {
    val dataStr =
      """
        |{
        |  "method": "cancelListing",
        |  "canceller": "Alice",
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "itemId": "item-001",
        |    "seller": "Alice",
        |    "price": 500,
        |    "description": "Vintage bicycle",
        |    "status": "active",
        |    "offerCount": 0
        |  }
        |}
        |""".stripMargin

    val expectedStr =
      """
        |{
        |  "itemId": "item-001",
        |  "seller": "Alice",
        |  "price": 500,
        |  "description": "Vintage bicycle",
        |  "status": "cancelled",
        |  "offerCount": 0
        |}
        |""".stripMargin

    for {
      expr     <- IO.fromEither(parser.parse(marketplaceContractExpr).flatMap(_.as[JsonLogicExpression]))
      data     <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      expected <- IO.fromEither(parser.parse(expectedStr).flatMap(_.as[JsonLogicValue]))
      result   <- testRunner(expr, data, expected)
    } yield result
  }

  test("escrow contract: fails when buyer tries to release before deposit") {
    val dataStr =
      """
        |{
        |  "method": "release",
        |  "releaser": "Alice",
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "buyer": "Alice",
        |    "seller": "Bob",
        |    "arbiter": "Charlie",
        |    "amount": 1000,
        |    "status": "pending",
        |    "deposited": 0
        |  }
        |}
        |""".stripMargin

    for {
      expr   <- IO.fromEither(parser.parse(escrowContractExpr).flatMap(_.as[JsonLogicExpression]))
      data   <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      result <- testRunner(expr, data, StrValue("Failed release checks"))
    } yield result
  }

  test("voting contract: fails when voting after deadline") {
    val dataStr =
      """
        |{
        |  "method": "vote",
        |  "voter": "Alice",
        |  "option": "Approve",
        |  "currentTime": 2500,
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "title": "Project Funding",
        |    "options": ["Approve", "Reject", "Defer"],
        |    "voteCount": 0,
        |    "endTime": 2000,
        |    "active": true
        |  }
        |}
        |""".stripMargin

    for {
      expr   <- IO.fromEither(parser.parse(votingContractExpr).flatMap(_.as[JsonLogicExpression]))
      data   <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      result <- testRunner(expr, data, StrValue("Failed vote checks"))
    } yield result
  }

  test("marketplace contract: fails when buyer makes offer on own listing") {
    val dataStr =
      """
        |{
        |  "method": "makeOffer",
        |  "buyer": "Alice",
        |  "offerPrice": 450,
        |  "participants": ["Alice", "Bob", "Charlie"],
        |  "content": {
        |    "itemId": "item-001",
        |    "seller": "Alice",
        |    "price": 500,
        |    "description": "Vintage bicycle",
        |    "status": "active",
        |    "offerCount": 0
        |  }
        |}
        |""".stripMargin

    for {
      expr   <- IO.fromEither(parser.parse(marketplaceContractExpr).flatMap(_.as[JsonLogicExpression]))
      data   <- IO.fromEither(parser.parse(dataStr).flatMap(_.as[JsonLogicValue]))
      result <- testRunner(expr, data, StrValue("Failed makeOffer checks"))
    } yield result
  }
}
