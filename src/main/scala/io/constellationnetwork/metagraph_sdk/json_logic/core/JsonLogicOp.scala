package io.constellationnetwork.metagraph_sdk.json_logic.core

import enumeratum.{CirceEnum, _}

sealed abstract class JsonLogicOp(val tag: String) extends EnumEntry

object JsonLogicOp extends Enum[JsonLogicOp] with CirceEnum[JsonLogicOp] {
  val values: IndexedSeq[JsonLogicOp] = findValues
  val knownOperatorTags: Map[String, JsonLogicOp] = JsonLogicOp.values.map(op => op.tag -> op).toMap

  // Control Flow
  case object NoOp extends JsonLogicOp("noop")
  case object IfElseOp extends JsonLogicOp("if")
  case object DefaultOp extends JsonLogicOp("default")
  case object LetOp extends JsonLogicOp("let")

  // Logical Operators
  case object NotOp extends JsonLogicOp("!")
  case object NOp extends JsonLogicOp("!!")
  case object OrOp extends JsonLogicOp("or")
  case object AndOp extends JsonLogicOp("and")

  // Comparison Operators
  case object EqOp extends JsonLogicOp("==")
  case object EqStrictOp extends JsonLogicOp("===")
  case object NEqOp extends JsonLogicOp("!=")
  case object NEqStrictOp extends JsonLogicOp("!==")
  case object Lt extends JsonLogicOp("<")
  case object Leq extends JsonLogicOp("<=")
  case object Gt extends JsonLogicOp(">")
  case object Geq extends JsonLogicOp(">=")

  // Arithmetic Operators
  case object AddOp extends JsonLogicOp("+")
  case object MinusOp extends JsonLogicOp("-")
  case object TimesOp extends JsonLogicOp("*")
  case object DivOp extends JsonLogicOp("/")
  case object ModuloOp extends JsonLogicOp("%")
  case object MaxOp extends JsonLogicOp("max")
  case object MinOp extends JsonLogicOp("min")
  case object AbsOp extends JsonLogicOp("abs")
  case object RoundOp extends JsonLogicOp("round")
  case object FloorOp extends JsonLogicOp("floor")
  case object CeilOp extends JsonLogicOp("ceil")
  case object PowOp extends JsonLogicOp("pow")

  // Array Operations
  case object MapOp extends JsonLogicOp("map")
  case object FilterOp extends JsonLogicOp("filter")
  case object ReduceOp extends JsonLogicOp("reduce")
  case object MergeOp extends JsonLogicOp("merge")
  case object AllOp extends JsonLogicOp("all")
  case object SomeOp extends JsonLogicOp("some")
  case object NoneOp extends JsonLogicOp("none")
  case object FindOp extends JsonLogicOp("find")
  case object CountOp extends JsonLogicOp("count")
  case object InOp extends JsonLogicOp("in")
  case object IntersectOp extends JsonLogicOp("intersect")
  case object UniqueOp extends JsonLogicOp("unique")
  case object SliceOp extends JsonLogicOp("slice")
  case object ReverseOp extends JsonLogicOp("reverse")
  case object FlattenOp extends JsonLogicOp("flatten")

  // String Operations
  case object CatOp extends JsonLogicOp("cat")
  case object SubStrOp extends JsonLogicOp("substr")
  case object LowerOp extends JsonLogicOp("lower")
  case object UpperOp extends JsonLogicOp("upper")
  case object JoinOp extends JsonLogicOp("join")
  case object SplitOp extends JsonLogicOp("split")
  case object TrimOp extends JsonLogicOp("trim")
  case object StartsWithOp extends JsonLogicOp("startsWith")
  case object EndsWithOp extends JsonLogicOp("endsWith")

  // Object/Map Operations
  case object MapValuesOp extends JsonLogicOp("values")
  case object MapKeysOp extends JsonLogicOp("keys")
  case object GetOp extends JsonLogicOp("get")
  case object HasOp extends JsonLogicOp("has")
  case object EntriesOp extends JsonLogicOp("entries")

  // Utility Operations
  case object LengthOp extends JsonLogicOp("length")
  case object ExistsOp extends JsonLogicOp("exists")
  case object MissingNoneOp extends JsonLogicOp("missing")
  case object MissingSomeOp extends JsonLogicOp("missing_some")
  case object TypeOfOp extends JsonLogicOp("typeof")

  // ZK / Crypto Operations (verify/hash precompiles over verified facts)
  case object PoseidonOp extends JsonLogicOp("poseidon")
  case object PmtVerifyOp extends JsonLogicOp("pmt_verify")
  case object Groth16VerifyOp extends JsonLogicOp("groth16_verify")
  case object EcVrfVerifyOp extends JsonLogicOp("ecvrf_verify")

  // ZK / Crypto Operations -- second wave (BN254 curve, BLS12-381, Schnorr)
  case object Bn254AddOp extends JsonLogicOp("bn254_add")
  case object Bn254MulOp extends JsonLogicOp("bn254_mul")
  case object Bn254PairingOp extends JsonLogicOp("bn254_pairing")
  case object BlsVerifyOp extends JsonLogicOp("bls_verify")
  case object BlsAggregateVerifyOp extends JsonLogicOp("bls_aggregate_verify")
  case object SchnorrVerifyOp extends JsonLogicOp("schnorr_verify")

  // ZK / Crypto Operations -- third wave (clear-text authenticated databases: SMT + MPT).
  // Unlike the Poseidon/BN254 verifiers above (field-element hex), these tries store circe JSON;
  // proofs and values are JSON-encoded, while roots/keys/prefixes remain hex.
  case object SmtVerifyOp extends JsonLogicOp("smt_verify")
  case object MptVerifyOp extends JsonLogicOp("mpt_verify")
  case object MptPrefixVerifyOp extends JsonLogicOp("mpt_prefix_verify")
}
