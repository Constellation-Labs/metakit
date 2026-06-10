package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.MonadThrow
import cats.data.EitherT

import io.constellationnetwork.metagraph_sdk.crypto.smt.api.SparseMerkleVerifier
import io.constellationnetwork.metagraph_sdk.crypto.smt.{SparseMerkleEntry, SparseMerkleProof, SparseMerkleProofError, SparseMerkleRoot}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

/**
 * The catalog-side attestation of one ordinal against a single trusted catalog root: was an MPT
 * root committed at snapshot ordinal `ordinal`, and if so which one?
 *
 * Structure (see [[EpochCatalog]] for the rollup): the two TOP inclusions surface the hot-epoch
 * and level-1 roots, then
 *   - HOT ordinal: `hot` is an inclusion -> the committed MPT root;
 *   - ANCIENT ordinal: `level1` includes the sealed root of epoch `ordinal / epochSize` and
 *     `sealedEntry` includes the ordinal inside that sealed tree -- two fixed-depth inclusions;
 *   - NON-MEMBERSHIP: `hot` is an absence AND the sealed path terminates in an absence (the epoch
 *     was never sealed, or the ordinal is absent from the sealed tree).
 *
 * [[OrdinalCatalogProofVerifier.verify]] recomputes every key locally (from `ordinal` and the
 * chain's `epochSize`) -- nothing inside the proof chooses which slots are checked, so a prover
 * cannot e.g. prove absence in the wrong epoch tree.
 */
final case class OrdinalCatalogProof(
  ordinal: Long,
  topHot: SparseMerkleProof,
  topSealed: SparseMerkleProof,
  hot: SparseMerkleProof,
  level1: SparseMerkleProof,
  sealedEntry: Option[SparseMerkleProof]
)

object OrdinalCatalogProof {

  implicit val encoder: Encoder[OrdinalCatalogProof] =
    (p: OrdinalCatalogProof) =>
      Json.obj(
        "ordinal"     -> p.ordinal.asJson,
        "topHot"      -> p.topHot.asJson,
        "topSealed"   -> p.topSealed.asJson,
        "hot"         -> p.hot.asJson,
        "level1"      -> p.level1.asJson,
        "sealedEntry" -> p.sealedEntry.asJson
      )

  implicit val decoder: Decoder[OrdinalCatalogProof] = (c: HCursor) =>
    for {
      ordinal     <- c.downField("ordinal").as[Long]
      topHot      <- c.downField("topHot").as[SparseMerkleProof]
      topSealed   <- c.downField("topSealed").as[SparseMerkleProof]
      hot         <- c.downField("hot").as[SparseMerkleProof]
      level1      <- c.downField("level1").as[SparseMerkleProof]
      sealedEntry <- c.downField("sealedEntry").as[Option[SparseMerkleProof]]
    } yield OrdinalCatalogProof(ordinal, topHot, topSealed, hot, level1, sealedEntry)
}

/** The verified outcome of an [[OrdinalCatalogProof]]. */
sealed trait OrdinalAttestation extends Product with Serializable

object OrdinalAttestation {

  /** Ordinal `ordinal` committed MPT root `mptRoot` (proven against the catalog root). */
  final case class CommittedAt(ordinal: Long, mptRoot: Hash) extends OrdinalAttestation

  /** Ordinal `ordinal` is provably NOT committed in the catalog. */
  final case class NotCommitted(ordinal: Long) extends OrdinalAttestation
}

object OrdinalCatalogProofVerifier {

  /**
   * Verify `proof` against the trusted `catalogRoot`. `epochSize` is the chain-wide rollup
   * geometry ([[CommittedConfig.epochSize]]) -- it determines the unique epoch tree the ordinal
   * may live in and MUST come from configuration, never from the prover.
   */
  def verify[F[_]: MonadThrow: JsonBinaryHasher](
    catalogRoot: SparseMerkleRoot,
    proof: OrdinalCatalogProof,
    epochSize: Int
  ): F[Either[CommittedProofError, OrdinalAttestation]] = {
    val verifier = SparseMerkleVerifier.make[F]
    val epoch = CommitCatalog.epochOf(proof.ordinal, epochSize)

    def checked(
      component: String,
      root: SparseMerkleRoot,
      smtProof: SparseMerkleProof,
      expectedKey: Hex
    ): EitherT[F, CommittedProofError, SparseMerkleEntry] =
      if (smtProof.key != expectedKey)
        EitherT.leftT[F, SparseMerkleEntry](
          CommittedProofError.WrongProofKey(component, expectedKey, smtProof.key): CommittedProofError
        )
      else
        EitherT(verifier.verify(root, smtProof))
          .bimap(CommittedProofError.ProofInvalid(component, _): CommittedProofError, _.value)

    def requireSubRoot(component: String, entry: SparseMerkleEntry): EitherT[F, CommittedProofError, SparseMerkleRoot] =
      entry match {
        case SparseMerkleEntry.Present(_, value) =>
          EitherT.rightT[F, CommittedProofError](SparseMerkleRoot(CommitCatalog.rootFromValueBytes(value)))
        case SparseMerkleEntry.Absent(_) =>
          EitherT.leftT[F, SparseMerkleRoot](
            CommittedProofError.MalformedOrdinalProof(s"$component must be an inclusion in the top catalog"): CommittedProofError
          )
      }

    (for {
      hotEntry    <- checked("topHot", catalogRoot, proof.topHot, CommitCatalog.hotEpochsKey)
      hotRoot     <- requireSubRoot("topHot", hotEntry)
      sealedEntry <- checked("topSealed", catalogRoot, proof.topSealed, CommitCatalog.sealedEpochsKey)
      level1Root  <- requireSubRoot("topSealed", sealedEntry)

      hotResult <- checked("hot", hotRoot, proof.hot, CommitCatalog.ordinalKey(proof.ordinal))

      attestation <- hotResult match {
        case SparseMerkleEntry.Present(_, value) =>
          EitherT.rightT[F, CommittedProofError](
            OrdinalAttestation.CommittedAt(proof.ordinal, CommitCatalog.rootFromValueBytes(value)): OrdinalAttestation
          )
        case SparseMerkleEntry.Absent(_) =>
          checked("level1", level1Root, proof.level1, CommitCatalog.epochKey(epoch)).flatMap {
            case SparseMerkleEntry.Absent(_) =>
              // hot-absent AND the ordinal's epoch was never sealed => not committed.
              EitherT.rightT[F, CommittedProofError](
                OrdinalAttestation.NotCommitted(proof.ordinal): OrdinalAttestation
              )
            case SparseMerkleEntry.Present(_, sealedRootBytes) =>
              val sealedRoot = SparseMerkleRoot(CommitCatalog.rootFromValueBytes(sealedRootBytes))
              proof.sealedEntry match {
                case None =>
                  EitherT.leftT[F, OrdinalAttestation](
                    CommittedProofError.MalformedOrdinalProof(
                      s"epoch $epoch is sealed; a sealedEntry proof is required"
                    ): CommittedProofError
                  )
                case Some(entryProof) =>
                  checked("sealedEntry", sealedRoot, entryProof, CommitCatalog.ordinalKey(proof.ordinal)).map {
                    case SparseMerkleEntry.Present(_, value) =>
                      OrdinalAttestation.CommittedAt(proof.ordinal, CommitCatalog.rootFromValueBytes(value)): OrdinalAttestation
                    case SparseMerkleEntry.Absent(_) =>
                      OrdinalAttestation.NotCommitted(proof.ordinal): OrdinalAttestation
                  }
              }
          }
      }
    } yield attestation).value
  }
}

/** Errors of the catalog proof surface (serving and verification). */
sealed abstract class CommittedProofError(message: String) extends RuntimeException(message)

object CommittedProofError {

  /** The node pruned this sealed epoch's contents: it cannot SERVE the proof (others can; the proof stays verifiable). */
  final case class EpochPruned(epoch: Long)
      extends CommittedProofError(
        s"sealed epoch $epoch contents pruned by retention policy on this node; " +
        "fetch the proof from a node retaining the epoch (the catalog root still verifies it)"
      )

  /** The catalog is seeded from a breadcrumb but not hydrated: contents unknown, proofs unavailable. */
  case object CatalogNotHydrated extends CommittedProofError("catalog is breadcrumb-seeded but not hydrated; POST /committed/hydrate first")

  final case class ProofUnavailable(cause: SparseMerkleProofError)
      extends CommittedProofError(s"failed to produce catalog proof: ${cause.getMessage}")

  final case class WrongProofKey(component: String, expected: Hex, got: Hex)
      extends CommittedProofError(s"ordinal proof component '$component' proves key ${got.value}, expected ${expected.value}")

  final case class ProofInvalid(component: String, cause: SparseMerkleProofError)
      extends CommittedProofError(s"ordinal proof component '$component' failed verification: ${cause.getMessage}")

  final case class MalformedOrdinalProof(reason: String) extends CommittedProofError(s"malformed ordinal proof: $reason")
}
