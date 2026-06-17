package io.constellationnetwork.metagraph_sdk.json_logic.ops

import cats.MonadThrow
import cats.syntax.all._

import io.constellationnetwork.metagraph_sdk.crypto.mpt.api.{MerklePatriciaBatchInclusionVerifier, MerklePatriciaVerifier}
import io.constellationnetwork.metagraph_sdk.crypto.mpt.{
  MerklePatriciaBatchInclusionProof,
  MerklePatriciaCommitment,
  MerklePatriciaInclusionProof,
  Nibble
}
import io.constellationnetwork.metagraph_sdk.crypto.smt._
import io.constellationnetwork.metagraph_sdk.crypto.smt.api.SparseMerkleVerifier
import io.constellationnetwork.metagraph_sdk.json_logic.core._
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Json}

/**
 * F-effectful implementations of the WAVE 3 JLVM opcodes: verifiers over the two clear-text,
 * authenticated databases -- the Sparse Merkle Tree (`smt_verify`) and the Merkle Patricia Trie
 * (`mpt_verify`, `mpt_prefix_verify`).
 *
 * These differ from the pure wave-1/2 [[CryptoOps]] in two ways:
 *
 *   1. EFFECT. The auth-DB verifiers are `F[_]: MonadThrow: JsonBinaryHasher` (hashing routes
 *      through metakit's `std/JsonBinaryHasher`, which is derived automatically from `MonadThrow[F]`
 *      via `JsonBinaryHasher.deriveFromCodec`). So these handlers run in the JLVM's own `F` -- no
 *      `SyncIO`/`unsafeRun`.
 *
 *   2. ENCODING. The wave-1/2 crypto ops hash BN254 field elements and take everything as fixed-width
 *      hex. These tries instead store circe JSON, so:
 *        - ROOTS / KEYS / PREFIXES are hex (`Hash` / `Hex`), validated via [[HexBytes]] (`0x`-prefixed
 *          lowercase) at the boundary and handed to the primitives as raw tessellation hex.
 *        - PROOFS and VALUES are JSON. A proof arrives as a JSON Logic value, is bridged to circe
 *          `Json` (via the `Encoder[JsonLogicValue]` already defined on the value type), and decoded
 *          to the native proof type through its circe `Decoder`. Values are bridged the same way.
 *
 * Error discipline (mirrors `groth16_verify` / `pmt_verify`): malformed / undecodable input (bad hex,
 * a proof that does not match its declared shape, etc.) is a `Result` error ([[JsonLogicException]]);
 * a WELL-FORMED proof that simply does not verify against the root is a `false` / `valid:false` VALUE,
 * so contracts can branch on it.
 */
object AuthDbOps {

  // ===========================================================================
  // smt_verify: [rootHex, proofJson]
  //   -> {"valid": bool, "included": bool, "key": hex, "value": <json|null>}
  // ===========================================================================

  def smtVerify[F[_]: MonadThrow](values: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
    values match {
      case rootV :: proofV :: Nil =>
        val parsed: Either[JsonLogicException, (SparseMerkleRoot, SparseMerkleProof)] = for {
          rootHex <- expectStr("smt_verify root")(rootV)
          rootRaw <- parseHashHex(rootHex, "smt_verify root")
          proof   <- decodeProof[SparseMerkleProof]("smt_verify proof")(proofV)
        } yield (SparseMerkleRoot(rootRaw), proof)

        parsed match {
          case Left(err) => err.asLeft[JsonLogicValue].pure[F]
          case Right((root, proof)) =>
            SparseMerkleVerifier.make[F].verify(root, proof).map {
              // A well-formed proof that does not verify => valid:false (NOT a Result error).
              case Left(_) =>
                (smtResult(valid = false, included = false, keyHex(proof.key), NullValue): JsonLogicValue).asRight[JsonLogicException]
              case Right(verified) =>
                verified.value match {
                  case SparseMerkleEntry.Present(key, value) =>
                    smtResult(valid = true, included = true, keyHex(key), valueToJlv(value.toBytes)).asRight[JsonLogicException]
                  case SparseMerkleEntry.Absent(key) =>
                    smtResult(valid = true, included = false, keyHex(key), NullValue).asRight[JsonLogicException]
                }
            }
        }
      case _ =>
        JsonLogicException(s"smt_verify: expected [rootHex, proofJson], got $values").asLeft[JsonLogicValue].pure[F]
    }

  // ===========================================================================
  // mpt_verify: [rootHex, keyHex, valueJson, proofJson] -> bool
  //   (true iff key->value is included at root per the proof)
  // ===========================================================================

  def mptVerify[F[_]: MonadThrow](values: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
    values match {
      case rootV :: keyV :: valueV :: proofV :: Nil =>
        val parsed: Either[JsonLogicException, (Hash, Hex, Json, MerklePatriciaInclusionProof)] = for {
          rootHex <- expectStr("mpt_verify root")(rootV)
          rootRaw <- parseHashHex(rootHex, "mpt_verify root")
          keyHexS <- expectStr("mpt_verify key")(keyV)
          keyRaw  <- parseHexHex(keyHexS, "mpt_verify key")
          valueJs <- toCirce(valueV)
          proof   <- decodeProof[MerklePatriciaInclusionProof]("mpt_verify proof")(proofV)
        } yield (rootRaw, keyRaw, valueJs, proof)

        parsed match {
          case Left(err)                          => err.asLeft[JsonLogicValue].pure[F]
          case Right((root, key, valueJs, proof)) =>
            // The proof's path must be exactly the queried key, and the leaf must commit to the
            // queried value (dataDigest = computeDigest(value)). Either binding failing is a `false`
            // VALUE, not an error: the contract should be able to branch on a wrong key/value.
            if (proof.path.value.toLowerCase != key.value.toLowerCase)
              (BoolValue(false): JsonLogicValue).asRight[JsonLogicException].pure[F]
            else
              JsonBinaryHasher[F].computeDigest(valueJs).flatMap { valueDigest =>
                // A single-path inclusion proof carries exactly one Leaf commitment (the prover
                // prepends it at the head of the witness); it must bind the queried value.
                val leafBinds = proof.witness.collectFirst {
                  case MerklePatriciaCommitment.Leaf(_, dataDigest) => dataDigest == valueDigest
                }.getOrElse(false)
                if (!leafBinds) (BoolValue(false): JsonLogicValue).asRight[JsonLogicException].pure[F]
                else
                  MerklePatriciaVerifier
                    .make[F](root)
                    .confirm(proof)
                    .map(res => (BoolValue(res.isRight): JsonLogicValue).asRight[JsonLogicException])
              }
        }
      case _ =>
        JsonLogicException(s"mpt_verify: expected [rootHex, keyHex, valueJson, proofJson], got $values")
          .asLeft[JsonLogicValue]
          .pure[F]
    }

  // ===========================================================================
  // mpt_prefix_verify: [rootHex, prefixHex, entriesJson, batchProofJson] -> bool
  //   (true iff the COMPLETE set of entries under `prefix` == `entriesJson`, all verified at root)
  // ===========================================================================

  /**
   * `entriesJson` is the complete, authenticated set of `{key->value}` pairs claimed to live under
   * the prefix. It is given as a JSON object mapping the (raw, lowercase) key hex to the entry's JSON
   * value.
   *
   * Returns `true` iff `entries` is the COMPLETE, correctly-bound set of `key->value` pairs under
   * `prefix`, all authenticated at `root`. Soundness rests on three cryptographic checks (the
   * `claimedKeys == attestedKeys` set-equality is a cheap well-formedness gate, NOT the soundness
   * boundary):
   *
   *   1. PER-KEY BINDING: the leaf reconstructed for each key's path must commit
   *      `dataDigest == computeDigest(value)` -- this binds each value to ITS key's leaf, so swapping
   *      which value belongs to which key fails (a set-membership test over all leaf digests would
   *      not).
   *   2. BATCH INCLUSION: every attested path reconstructs and verifies against the root.
   *   3. COMPLETENESS: the subtree rooted at the prefix point is traversed over the witness, and
   *      EVERY leaf reachable in it must be an attested terminal. A prover that omits a key under the
   *      prefix leaves a branch child pointing at a node that is not an attested leaf, which fails.
   */
  def mptPrefixVerify[F[_]: MonadThrow](values: List[JsonLogicValue]): F[Either[JsonLogicException, JsonLogicValue]] =
    values match {
      case rootV :: prefixV :: entriesV :: proofV :: Nil =>
        val parsed: Either[JsonLogicException, (Hash, Hex, Map[String, Json], MerklePatriciaBatchInclusionProof)] = for {
          rootHex   <- expectStr("mpt_prefix_verify root")(rootV)
          rootRaw   <- parseHashHex(rootHex, "mpt_prefix_verify root")
          prefixHex <- expectStr("mpt_prefix_verify prefix")(prefixV)
          prefixRaw <- parseHexHex(prefixHex, "mpt_prefix_verify prefix")
          entries   <- expectEntries("mpt_prefix_verify entries")(entriesV)
          proof     <- decodeProof[MerklePatriciaBatchInclusionProof]("mpt_prefix_verify batchProof")(proofV)
        } yield (rootRaw, prefixRaw, entries, proof)

        parsed match {
          case Left(err) => err.asLeft[JsonLogicValue].pure[F]
          case Right((root, prefix, entries, proof)) =>
            val claimedKeys = entries.keySet.map(_.toLowerCase)
            val attestedKeys = proof.paths.map(_.value.toLowerCase).toSet
            val prefixLower = prefix.value.toLowerCase
            // WELL-FORMEDNESS GATE: the claimed key-set must equal the proof's attested path-set, and
            // every attested path must be under the prefix. This is a cheap consistency check on the
            // prover's OWN lists; soundness is enforced by per-key binding + batch inclusion + the
            // completeness traversal below (which do not trust these lists).
            val keySetsMatch = claimedKeys == attestedKeys
            val allUnderPrefix = attestedKeys.forall(_.startsWith(prefixLower))
            if (!keySetsMatch || !allUnderPrefix)
              (BoolValue(false): JsonLogicValue).asRight[JsonLogicException].pure[F]
            else
              // Precompute every witness commitment's prefixed digest once (first occurrence wins,
              // mirroring `findMatchingCommitment`'s `collectFirst`), then run the three soundness
              // checks purely against that digest map.
              witnessDigestMap[F](proof.witness).flatMap { byDigest =>
                // 1. PER-KEY VALUE-BINDING.
                valuesBindPerKey[F](root, entries, byDigest).flatMap {
                  case false => (BoolValue(false): JsonLogicValue).asRight[JsonLogicException].pure[F]
                  case true  =>
                    // 2. BATCH INCLUSION.
                    MerklePatriciaBatchInclusionVerifier.make[F](root).confirm(proof).map { batchRes =>
                      if (batchRes.isLeft) (BoolValue(false): JsonLogicValue).asRight[JsonLogicException]
                      else {
                        // 3. COMPLETENESS: the attested set must be ALL keys under the prefix.
                        val complete = prefixSubtreeComplete(root, prefix, proof, byDigest)
                        (BoolValue(complete): JsonLogicValue).asRight[JsonLogicException]
                      }
                    }
                }
              }
        }
      case _ =>
        JsonLogicException(s"mpt_prefix_verify: expected [rootHex, prefixHex, entriesJson, batchProofJson], got $values")
          .asLeft[JsonLogicValue]
          .pure[F]
    }

  // ---------------------------------------------------------------------------
  // mpt_prefix_verify soundness helpers (per-key binding + completeness).
  //
  // These mirror the batch verifier's root-down reconstruction (matching a witness
  // commitment by its prefixed digest) but operate over a precomputed digest map so
  // the traversal is pure once the digests are known.
  // ---------------------------------------------------------------------------

  /**
   * Map each witness commitment by its prefixed node-commitment digest, keeping the FIRST occurrence
   * of any duplicate digest (mirroring the verifier's `collectFirst`).
   */
  private def witnessDigestMap[F[_]: MonadThrow](
    witness: List[MerklePatriciaCommitment]
  ): F[Map[String, MerklePatriciaCommitment]] =
    witness
      .traverse(c => MerklePatriciaCommitment.commitmentDigest[F](c).map(d => d.value.toLowerCase -> c))
      .map(_.foldLeft(Map.empty[String, MerklePatriciaCommitment]) {
        case (acc, (d, c)) => if (acc.contains(d)) acc else acc + (d -> c)
      })

  /**
   * PER-KEY binding: for EVERY claimed `(keyHex, value)`, the leaf that the KEY's path reconstructs to
   * must commit `dataDigest == computeDigest(value)`. Binds each value to ITS key's leaf -- swapping
   * which value belongs to which key fails. A key whose path does not reconstruct to a leaf (under the
   * trusted root) fails.
   */
  private def valuesBindPerKey[F[_]: MonadThrow](
    root: Hash,
    entries: Map[String, Json],
    byDigest: Map[String, MerklePatriciaCommitment]
  ): F[Boolean] =
    entries.toList.traverse {
      case (keyHex, value) =>
        JsonBinaryHasher[F].computeDigest(value).map { valueDigest =>
          reconstructTerminalLeaf(root, Hex(keyHex), byDigest) match {
            case Some((_, leaf)) => leaf.dataDigest.value.toLowerCase == valueDigest.value.toLowerCase
            case None            => false
          }
        }
    }
      .map(_.forall(identity))

  /**
   * Reconstruct `path` from the witness digest map (the same root-down walk as the batch verifier's
   * `findMatchingCommitment`) and return the terminal `Leaf` commitment it reaches together with that
   * leaf's prefixed node-commitment digest, or `None` if it does not reconstruct to a leaf. The batch
   * verifier separately re-checks the same reconstruction folds to the root.
   */
  private def reconstructTerminalLeaf(
    root: Hash,
    path: Hex,
    byDigest: Map[String, MerklePatriciaCommitment]
  ): Option[(String, MerklePatriciaCommitment.Leaf)] = {
    @scala.annotation.tailrec
    def loop(currentDigest: String, remaining: Seq[Nibble]): Option[(String, MerklePatriciaCommitment.Leaf)] =
      if (remaining.isEmpty) None // path exhausted without reaching a Leaf
      else
        byDigest.get(currentDigest) match {
          case Some(leaf: MerklePatriciaCommitment.Leaf) =>
            if (remaining == leaf.remaining) Some((currentDigest, leaf)) else None
          case Some(ext: MerklePatriciaCommitment.Extension) =>
            if (remaining.startsWith(ext.shared))
              loop(ext.childDigest.value.toLowerCase, remaining.drop(ext.shared.length))
            else None
          case Some(branch: MerklePatriciaCommitment.Branch) =>
            branch.pathsDigest.get(remaining.head) match {
              case Some(child) => loop(child.value.toLowerCase, remaining.tail)
              case None        => None
            }
          case None => None
        }
    loop(root.value.toLowerCase, Nibble(path))
  }

  /**
   * COMPLETENESS: after per-key binding and batch inclusion have passed, require that the attested
   * leaves are ALL the leaves under the prefix.
   *
   *   1. Walk root -> prefix, consuming the prefix nibbles through branches / extensions / a leaf, to
   *      find the subtree-root digest at (or just under) the prefix point. If the prefix selects an
   *      absent branch child, diverges from an extension, or its node is missing from the witness,
   *      there is NO subtree under the prefix -> the only complete attestation is the empty set.
   *   2. Traverse the subtree at that digest over the witness: every Branch requires ALL of its
   *      `pathsDigest` children to be present-and-complete; an Extension requires its single child; a
   *      Leaf must be an ATTESTED terminal (its digest is among the leaves the attested paths
   *      reconstruct to). Any branch child pointing at a node absent from the witness, or at a leaf no
   *      attested path reaches, is an UNATTESTED key under the prefix -> INCOMPLETE.
   *
   * Full-child coverage is enforced only AT OR BELOW the prefix point: nodes above the prefix
   * legitimately have siblings outside the prefix, which are never visited.
   */
  private def prefixSubtreeComplete(
    root: Hash,
    prefix: Hex,
    proof: MerklePatriciaBatchInclusionProof,
    byDigest: Map[String, MerklePatriciaCommitment]
  ): Boolean = {
    // The set of leaf-commitment digests the attested paths actually terminate at.
    val attestedLeafDigests: Set[String] =
      proof.paths.flatMap { p =>
        reconstructTerminalLeaf(root, p, byDigest).map(_._1)
      }.toSet
    // A path that does not reconstruct to a leaf was already rejected by batch inclusion; defensively
    // treat a mismatch (fewer attested leaves than paths) as incomplete.
    if (attestedLeafDigests.size != proof.paths.map(_.value.toLowerCase).toSet.size) false
    else
      walkToPrefix(root.value.toLowerCase, Nibble(prefix), byDigest) match {
        case None                => proof.paths.isEmpty // no subtree under prefix -> only empty set is complete
        case Some(subRootDigest) => subtreeAllLeavesAttested(subRootDigest, byDigest, attestedLeafDigests)
      }
  }

  /**
   * Walk root -> prefix point, returning the subtree-root digest at the prefix, or `None` if the
   * prefix has no reachable subtree (absent branch child, divergent extension, or a missing node).
   */
  @scala.annotation.tailrec
  private def walkToPrefix(
    currentDigest: String,
    remaining: Seq[Nibble],
    byDigest: Map[String, MerklePatriciaCommitment]
  ): Option[String] =
    if (remaining.isEmpty) Some(currentDigest)
    else
      byDigest.get(currentDigest) match {
        case None => None // node on the prefix path is not in the witness
        case Some(branch: MerklePatriciaCommitment.Branch) =>
          branch.pathsDigest.get(remaining.head) match {
            case Some(child) => walkToPrefix(child.value.toLowerCase, remaining.tail, byDigest)
            case None        => None // prefix selects a non-existent branch child
          }
        case Some(ext: MerklePatriciaCommitment.Extension) =>
          if (remaining.startsWith(ext.shared))
            walkToPrefix(ext.childDigest.value.toLowerCase, remaining.drop(ext.shared.length), byDigest)
          else if (ext.shared.startsWith(remaining))
            // Prefix ends MID-extension: the whole subtree below the extension's child is under prefix.
            Some(ext.childDigest.value.toLowerCase)
          else None // prefix diverges from the extension
        case Some(leaf: MerklePatriciaCommitment.Leaf) =>
          // Prefix descends into a single leaf (single-leaf subtree); under prefix iff the leaf's
          // remaining nibbles extend it.
          if (leaf.remaining.startsWith(remaining)) Some(currentDigest) else None
      }

  /**
   * Recursively require that every leaf reachable in the subtree rooted at `digest` is an attested
   * terminal. At a Branch, EVERY `pathsDigest` child must itself be complete (full-child coverage). A
   * node digest absent from the witness, or a leaf not in `attested`, means an unattested key exists
   * under the prefix -> INCOMPLETE.
   */
  private def subtreeAllLeavesAttested(
    digest: String,
    byDigest: Map[String, MerklePatriciaCommitment],
    attested: Set[String]
  ): Boolean =
    byDigest.get(digest) match {
      case None                                   => false // committed child not present in the witness
      case Some(_: MerklePatriciaCommitment.Leaf) => attested.contains(digest)
      case Some(ext: MerklePatriciaCommitment.Extension) =>
        subtreeAllLeavesAttested(ext.childDigest.value.toLowerCase, byDigest, attested)
      case Some(branch: MerklePatriciaCommitment.Branch) =>
        branch.pathsDigest.values.forall(child => subtreeAllLeavesAttested(child.value.toLowerCase, byDigest, attested))
    }

  /** Build the smt_verify result object. */
  private def smtResult(valid: Boolean, included: Boolean, key: String, value: JsonLogicValue): JsonLogicValue =
    MapValue(
      Map(
        "valid"    -> BoolValue(valid),
        "included" -> BoolValue(included),
        "key"      -> StrValue(key),
        "value"    -> value
      )
    )

  /** Render a tessellation `Hex` key as the JLVM's `0x`-prefixed lowercase hex convention. */
  private def keyHex(key: Hex): String = "0x" + key.value.toLowerCase

  /** Bridge raw value bytes (from a verified SMT entry) to a JSON Logic value via circe. */
  private def valueToJlv(value: Array[Byte]): JsonLogicValue =
    io.circe.parser
      .parse(new String(value, java.nio.charset.StandardCharsets.UTF_8))
      .flatMap(_.as[JsonLogicValue])
      .getOrElse(StrValue("0x" + Hex.fromBytes(value).value.toLowerCase))

  /** Bridge a JSON Logic value to circe `Json` (uses the value type's own `Encoder`). */
  private def toCirce(v: JsonLogicValue): Either[JsonLogicException, Json] =
    Right(v.asJson)

  /** Decode a JSON Logic value (bridged to circe) into a native proof type via its `Decoder`. */
  private def decodeProof[A: Decoder](role: String)(v: JsonLogicValue): Either[JsonLogicException, A] =
    v.asJson.as[A].leftMap(df => JsonLogicException(s"$role: undecodable proof JSON (${df.getMessage})"))

  /** `entriesJson` must be an object of `{ keyHex -> valueJson }`. */
  private def expectEntries(role: String)(v: JsonLogicValue): Either[JsonLogicException, Map[String, Json]] =
    v match {
      case MapValue(m) => Right(m.view.mapValues(_.asJson).toMap)
      case other       => Left(JsonLogicException(s"$role: expected a {keyHex -> value} object, got ${other.tag}"))
    }

  private def expectStr(role: String)(v: JsonLogicValue): Either[JsonLogicException, String] =
    v match {
      case StrValue(s) => Right(s)
      case other       => Left(JsonLogicException(s"$role: expected a hex string, got ${other.tag}"))
    }

  /**
   * Validate a `0x`-prefixed lowercase hex string (the JLVM convention) and return a tessellation
   * [[Hash]]. The decoded byte length is unconstrained here; `Hash` is a SHA-256 digest, so a 32-byte
   * root is expected, but a wrong width simply fails the verify (RootMismatch) rather than the parse.
   */
  private def parseHashHex(hex: String, role: String): Either[JsonLogicException, Hash] =
    HexBytes.parseBytes(hex, None, role).map(bytes => Hash(Hex.fromBytes(bytes).value))

  /**
   * Validate a `0x`-prefixed lowercase hex string and return a tessellation [[Hex]] (raw, no `0x`).
   * Keys / prefixes are nibble-granular MPT paths, so an ODD nibble count is allowed (a 1-nibble
   * prefix is legal).
   */
  private def parseHexHex(hex: String, role: String): Either[JsonLogicException, Hex] =
    HexBytes.parseNibbleHex(hex, role).map(body => Hex(body))
}
