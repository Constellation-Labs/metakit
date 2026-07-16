package io.constellationnetwork.metagraph_sdk.crypto.mpt

import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/**
 * A Merkle Patricia proof against a trusted root digest. Sealed: a proof is EITHER inclusion OR
 * absence -- absence (non-inclusion) is first-class, mirroring [[crypto.smt.SparseMerkleProof]].
 *
 * WIRE FORMAT (KAT-pinned in MerklePatriciaCodecKatSuite and byte-pinned as chain-derived
 * fixtures in `docs/mpt-spec/test-sealed-proofs.json` via MptSpecFixtureSuite). The reference
 * implementation for external light-client verifiers is `docs/mpt-spec/js` (inclusion AND
 * absence, exercised against the fixtures); the other bundled references (go/python/rust/
 * solidity) are inclusion-only pending ports -- they report a valid `Absence` proof as invalid:
 *
 * {{{
 *   { "type": "Inclusion", "path": "<nibble hex>", "witness": [ <commitment>* ] }
 *   { "type": "Absence",   "path": "<nibble hex>", "witness": [ <commitment>* ] }
 * }}}
 *
 * where each commitment is the existing [[MerklePatriciaCommitment]] ADT encoding
 * `{ "type": "Leaf"|"Branch"|"Extension", "contents": {...} }` and `witness` is ordered
 * DEEPEST-FIRST, identical to [[MerklePatriciaInclusionProof]] (verifiers fold `witness.reverse`
 * root-first). The `Inclusion` encoding is byte-identical to the legacy standalone
 * [[MerklePatriciaInclusionProof]] `{path, witness}` shape plus the `type` tag; the legacy codec
 * itself is preserved untouched for existing wire consumers.
 *
 * ABSENCE: the witness is the root-to-divergence commitment chain; its deepest element
 * (`witness.head`) is the TERMINAL commitment at which the queried path cannot continue:
 *
 *   - a terminal `Branch` whose `pathsDigest` lacks the next path nibble, or at which the path is
 *     exhausted (branches carry no value slot in this MPT, so a path ending at a branch is
 *     necessarily absent);
 *   - a terminal `Extension` whose `shared` nibbles are NOT a prefix of the remaining path
 *     (divergence mid-edge, including a remaining path shorter than the edge);
 *   - a terminal `Leaf` whose `remaining` differs from the remaining path (a different key
 *     occupies the position).
 *
 * There is deliberately NO explicit absence-reason tag on the wire: the terminal commitment
 * already carries its own `type` discriminator inside `witness`, and given that type plus the
 * un-consumed path suffix (both recomputed by the verifier from `path` during the fold) exactly
 * one absence condition applies -- the reason is structurally unambiguous, and a redundant tag
 * would be one more field to cross-validate. The Inclusion/Absence `type` tag IS carried because
 * the two cases share an identical field shape and a proof must state its claim explicitly rather
 * than have the verifier decide post-hoc what was proven.
 *
 * Codecs are hand-rolled per this package's convention: the commitment fields are `Seq[Nibble]`,
 * whose custom `Nibble.nibbleSeqEncoder` is ambiguous with circe's generic `encodeSeq` under
 * magnolia derivation (see MerklePatriciaCodecKatSuite).
 */
sealed trait MerklePatriciaProof extends Product with Serializable {
  def path: Hex
  def witness: List[MerklePatriciaCommitment]
}

object MerklePatriciaProof {

  /** Wraps the legacy proof unchanged so pre-existing inclusion consumers keep their type. */
  final case class Inclusion(proof: MerklePatriciaInclusionProof) extends MerklePatriciaProof {
    def path: Hex = proof.path
    def witness: List[MerklePatriciaCommitment] = proof.witness
  }

  final case class Absence(path: Hex, witness: List[MerklePatriciaCommitment]) extends MerklePatriciaProof

  implicit val mpProofEncoder: Encoder[MerklePatriciaProof] = Encoder.instance {
    case Inclusion(proof) =>
      Json.obj(
        "type"    -> Json.fromString("Inclusion"),
        "path"    -> proof.path.asJson,
        "witness" -> proof.witness.asJson
      )
    case Absence(path, witness) =>
      Json.obj(
        "type"    -> Json.fromString("Absence"),
        "path"    -> path.asJson,
        "witness" -> witness.asJson
      )
  }

  implicit val mpProofDecoder: Decoder[MerklePatriciaProof] = Decoder.instance { c =>
    c.downField("type").as[String].flatMap {
      case "Inclusion" =>
        for {
          path    <- c.downField("path").as[Hex]
          witness <- c.downField("witness").as[List[MerklePatriciaCommitment]]
        } yield Inclusion(MerklePatriciaInclusionProof(path, witness))
      case "Absence" =>
        for {
          path    <- c.downField("path").as[Hex]
          witness <- c.downField("witness").as[List[MerklePatriciaCommitment]]
        } yield Absence(path, witness)
      case other => Left(DecodingFailure(s"Unknown MerklePatriciaProof type: $other", c.history))
    }
  }
}
