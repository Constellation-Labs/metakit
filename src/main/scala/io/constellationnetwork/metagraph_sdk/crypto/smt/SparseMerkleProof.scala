package io.constellationnetwork.metagraph_sdk.crypto.smt

import cats.Eq
import cats.syntax.eq._

import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe._
import io.circe.syntax.EncoderOps

/**
 * One sibling subtree digest on the authentication path of an [[SparseMerkleProof]].
 *
 * Siblings are recorded TOP-DOWN (root-first): in a proof for position `P`, `siblings(i)` is the digest of the sibling
 * of the internal node at depth `i` (i.e. the child NOT on `P`'s path, selected by the complement of bit `i` of `P`).
 * The number of siblings equals the depth at which the proof terminates (a leaf, an other-leaf, or an empty/default
 * subtree). A verifier folds from the deepest sibling upward (`siblings.reverse`), choosing left/right at each level by
 * bit `i` of `P`.
 *
 * A sibling whose value is `Hash.empty` denotes a collapsed empty (default) subtree at that level -- kept explicit
 * (not omitted) so the sibling list index lines up with the bit index of `P`.
 */
final case class SparseMerkleSibling(digest: Hash)

object SparseMerkleSibling {
  implicit val eq: Eq[SparseMerkleSibling] = Eq.by(_.digest)

  implicit val encoder: Encoder[SparseMerkleSibling] = Encoder.instance(s => Json.obj("digest" -> s.digest.asJson))
  implicit val decoder: Decoder[SparseMerkleSibling] = Decoder.instance(_.downField("digest").as[Hash].map(SparseMerkleSibling(_)))
}

/**
 * Why a key is absent, native to the sparse Merkle tree (no separate "exclusion proof" shape -- absence is the same
 * authentication-path fold as inclusion, differing only in what sits at the terminating slot).
 *
 *   - [[AbsenceWitness.Default]] -- the slot on the queried key's path is a collapsed EMPTY (default-hash) subtree:
 *     nothing occupies it. The fold starts from `Hash.empty`.
 *   - [[AbsenceWitness.OtherLeaf]] -- a DIFFERENT leaf occupies the position the queried key's path leads to (they
 *     share the first `siblings.length`-bit prefix, then the descent halts at that leaf before reaching the queried
 *     position). Carries the occupying leaf's `occupyingKey` (so the verifier recomputes its position and asserts it
 *     differs from the queried position) and its `occupyingDataDigest` (so the verifier recomputes the occupying leaf
 *     digest and folds it up).
 */
sealed trait AbsenceWitness extends Product with Serializable

object AbsenceWitness {
  case object Default extends AbsenceWitness
  final case class OtherLeaf(occupyingKey: Hex, occupyingDataDigest: Hash) extends AbsenceWitness

  implicit val eq: Eq[AbsenceWitness] = Eq.instance {
    case (Default, Default)                     => true
    case (OtherLeaf(k1, d1), OtherLeaf(k2, d2)) => k1 === k2 && d1 === d2
    case _                                      => false
  }

  implicit val encoder: Encoder[AbsenceWitness] = Encoder.instance {
    case Default =>
      Json.obj("type" -> Json.fromString("Default"))
    case OtherLeaf(occupyingKey, occupyingDataDigest) =>
      Json.obj(
        "type"                -> Json.fromString("OtherLeaf"),
        "occupyingKey"        -> occupyingKey.asJson,
        "occupyingDataDigest" -> occupyingDataDigest.asJson
      )
  }

  implicit val decoder: Decoder[AbsenceWitness] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "Default" => Right(Default)
      case "OtherLeaf" =>
        for {
          occupyingKey        <- c.downField("occupyingKey").as[Hex]
          occupyingDataDigest <- c.downField("occupyingDataDigest").as[Hash]
        } yield OtherLeaf(occupyingKey, occupyingDataDigest)
      case other => Left(DecodingFailure(s"Unknown AbsenceWitness type: $other", c.history))
    }
  }
}

/**
 * A native sparse-Merkle-tree proof against an [[SparseMerkleRoot]]. Sealed: a proof is EITHER inclusion OR absence -- there is
 * no third shape, and absence is first-class.
 *
 * Value bytes are carried as a hex string on the wire (no circe `Encoder[Array[Byte]]` in scope; the codebase's
 * convention is `Hex`); the round-trip is byte-exact (`Hex.fromBytes`/`Hex.toBytes`).
 *
 *   - [[SparseMerkleProof.Inclusion]] -- `key` is present with `value`; `valueDigest` is the value digest committed in the leaf
 *     (= `Hash.fromBytes` of the value bytes); `siblings` is the top-down authentication path from the root to the leaf
 *     at the key's position. The verifier (1) asserts `Hash.fromBytes(value) == valueDigest` (mandatory value-binding;
 *     a tampered `value` fails HERE as `ValueBindingFailed`, distinctly from a tampered path which fails the root fold
 *     as `RootMismatch`), then (2) recomputes the leaf digest from `(position, valueDigest)` and folds up to check the
 *     root. `valueDigest` is the SMT analogue of the MPT leaf's `dataDigest`; it is carried explicitly so value-binding
 *     is a distinguishable check.
 *   - [[SparseMerkleProof.Absence]] -- `key` is absent; `witness` explains why ([[AbsenceWitness]]); `siblings` is the top-down
 *     authentication path to the terminating slot.
 */
sealed trait SparseMerkleProof extends Product with Serializable {
  def key: Hex
  def siblings: List[SparseMerkleSibling]
}

object SparseMerkleProof {

  final case class Inclusion(key: Hex, value: Hex, valueDigest: Hash, siblings: List[SparseMerkleSibling]) extends SparseMerkleProof
  final case class Absence(key: Hex, witness: AbsenceWitness, siblings: List[SparseMerkleSibling]) extends SparseMerkleProof

  // `value` is a `Hex` (tessellation) on the wire and in memory — NOT a raw `Array[Byte]`: Hex has a
  // circe codec, structural equality, and is immutable, so the proof needs no custom value codec and
  // no `sameElements` Eq. (The byte-exact value still binds via `Hash.fromBytes(value.toBytes)`.)
  implicit val eq: Eq[SparseMerkleProof] = Eq.instance {
    case (Inclusion(k1, v1, d1, s1), Inclusion(k2, v2, d2, s2)) => k1 === k2 && v1 === v2 && d1 === d2 && s1 === s2
    case (Absence(k1, w1, s1), Absence(k2, w2, s2))             => k1 === k2 && w1 === w2 && s1 === s2
    case _                                                      => false
  }

  implicit val encoder: Encoder[SparseMerkleProof] = Encoder.instance {
    case Inclusion(key, value, valueDigest, siblings) =>
      Json.obj(
        "type"        -> Json.fromString("Inclusion"),
        "key"         -> key.asJson,
        "value"       -> value.asJson,
        "valueDigest" -> valueDigest.asJson,
        "siblings"    -> siblings.asJson
      )
    case Absence(key, witness, siblings) =>
      Json.obj(
        "type"     -> Json.fromString("Absence"),
        "key"      -> key.asJson,
        "witness"  -> witness.asJson,
        "siblings" -> siblings.asJson
      )
  }

  implicit val decoder: Decoder[SparseMerkleProof] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "Inclusion" =>
        for {
          key         <- c.downField("key").as[Hex]
          value       <- c.downField("value").as[Hex]
          valueDigest <- c.downField("valueDigest").as[Hash]
          siblings    <- c.downField("siblings").as[List[SparseMerkleSibling]]
        } yield Inclusion(key, value, valueDigest, siblings)
      case "Absence" =>
        for {
          key      <- c.downField("key").as[Hex]
          witness  <- c.downField("witness").as[AbsenceWitness]
          siblings <- c.downField("siblings").as[List[SparseMerkleSibling]]
        } yield Absence(key, witness, siblings)
      case other => Left(DecodingFailure(s"Unknown SparseMerkleProof type: $other", c.history))
    }
  }
}
