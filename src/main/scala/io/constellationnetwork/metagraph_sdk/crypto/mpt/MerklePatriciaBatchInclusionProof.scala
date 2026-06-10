package io.constellationnetwork.metagraph_sdk.crypto.mpt

import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/**
 * A batch inclusion proof attesting that several paths are all present in the same trie.
 *
 * The witness is a single, de-duplicated list of node commitments shared across all paths. A
 * verifier reconstructs the per-path witness by walking from the root and, at each step, selecting
 * the commitment in `witness` whose prefixed digest matches the expected child digest.
 */
final case class MerklePatriciaBatchInclusionProof(
  paths: List[Hex],
  witness: List[MerklePatriciaCommitment]
)

object MerklePatriciaBatchInclusionProof {

  implicit val batchProofEncoder: Encoder[MerklePatriciaBatchInclusionProof] =
    (proof: MerklePatriciaBatchInclusionProof) =>
      Json.obj(
        "paths"   -> proof.paths.asJson,
        "witness" -> proof.witness.asJson
      )

  implicit val batchProofDecoder: Decoder[MerklePatriciaBatchInclusionProof] = (c: HCursor) =>
    for {
      paths   <- c.downField("paths").as[List[Hex]]
      witness <- c.downField("witness").as[List[MerklePatriciaCommitment]]
    } yield MerklePatriciaBatchInclusionProof(paths, witness)
}
