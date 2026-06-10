package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all._

import scala.collection.immutable.SortedMap

import io.constellationnetwork.metagraph_sdk.crypto.smt.impl.InMemorySparseMerkleTree
import io.constellationnetwork.metagraph_sdk.crypto.smt.{SparseMerkleProof, SparseMerkleRoot, SparseMerkleTree}
import io.constellationnetwork.metagraph_sdk.std.JsonBinaryHasher
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

import io.circe._
import io.circe.syntax.EncoderOps

/**
 * The TWO-LEVEL EPOCH ROLLUP behind the live catalog root.
 *
 * Historical `ordinal:<N> -> mptRoot_N` entries are not kept in one ever-growing tree. They are
 * rolled up in epochs of [[CommittedConfig.epochSize]] ordinals:
 *
 *   - the HOT epoch SMT holds the ordinals of the CURRENT epoch;
 *   - at each epoch boundary the hot tree is SEALED: its root becomes the `epoch:<E>` entry of the
 *     LEVEL-1 SMT, and the hot tree resets to empty;
 *   - [[compose]] binds everything into the TOP catalog SMT: `{ current:mpt -> mptRoot, epoch:hot
 *     -> hotRoot, epoch:sealed -> level1Root }`, whose root is the breadcrumb's `catalogRoot`.
 *
 * A proof of an ancient ordinal is therefore two fixed-depth SMT inclusions (level-1 -> sealed
 * epoch tree) under a constant-size top catalog; non-membership is absence in the hot tree plus
 * absence on the sealed path (see [[OrdinalCatalogProof]]).
 *
 * RETENTION: [[sealedTrees]] (the full contents of sealed epochs) is a SERVING cache pruned to the
 * last [[CommittedConfig.sealedEpochRetention]] epochs. Pruning never changes any root --
 * [[level1Entries]] keeps every sealed epoch root forever (32 bytes per ~2^16 ordinals), so proofs
 * issued before pruning remain verifiable; the node merely can no longer SERVE new proofs for
 * pruned epochs ([[CommittedProofError.EpochPruned]]).
 *
 * The value is immutable: [[advance]] returns a new catalog (plus the [[SealEvent]], for journal
 * write-through), sharing all untouched trees structurally.
 */
final case class EpochCatalog[F[_]] private (
  epochSize: Int,
  retention: Int,
  epochIndex: Long,
  hotEntries: SortedMap[Long, Hash],
  hotTree: InMemorySparseMerkleTree[F],
  level1Entries: SortedMap[Long, Hash],
  level1Tree: InMemorySparseMerkleTree[F],
  sealedTrees: SortedMap[Long, EpochCatalog.SealedEpoch[F]]
) {
  import CommitCatalog._

  /**
   * The transition step into ordinal `prevOrdinal + 1`: insert `ordinal:<prevOrdinal> ->
   * prevMptRoot`, sealing the hot epoch first if `prevOrdinal` opens a new epoch. Returns the
   * advanced catalog and the seal event, if one occurred.
   */
  def advance(prevOrdinal: Long, prevMptRoot: Hash)(
    implicit F: Sync[F],
    H: JsonBinaryHasher[F]
  ): F[(EpochCatalog[F], Option[EpochCatalog.SealEvent])] = {
    val targetEpoch = epochOf(prevOrdinal, epochSize)

    val sealedStep: F[(EpochCatalog[F], Option[EpochCatalog.SealEvent])] =
      if (targetEpoch <= epochIndex)
        (this, none[EpochCatalog.SealEvent]).pure[F]
      else
        for {
          hotRoot <- hotTree.root
          newLevel1 <- level1Tree
            .insert(epochKey(epochIndex), rootValueBytes(hotRoot))
            .flatMap(EpochCatalog.requireInMemory[F])
          emptyHot <- InMemorySparseMerkleTree.empty[F]
          retained = sealedTrees + (epochIndex -> EpochCatalog.SealedEpoch(hotEntries, hotTree))
          pruned = retained.drop((retained.size - retention).max(0))
        } yield
          (
            copy(
              epochIndex = targetEpoch,
              hotEntries = SortedMap.empty[Long, Hash],
              hotTree = emptyHot,
              level1Entries = level1Entries + (epochIndex -> hotRoot.value),
              level1Tree = newLevel1,
              sealedTrees = pruned
            ),
            EpochCatalog.SealEvent(epochIndex, hotRoot, hotEntries.keySet.toList).some
          )

    sealedStep.flatMap {
      case (catalog, seal) =>
        catalog.hotTree
          .insert(ordinalKey(prevOrdinal), rootValueBytes(prevMptRoot))
          .flatMap(EpochCatalog.requireInMemory[F])
          .map { newHot =>
            (
              catalog.copy(
                hotEntries = catalog.hotEntries + (prevOrdinal -> prevMptRoot),
                hotTree = newHot
              ),
              seal
            )
          }
    }
  }

  /**
   * Bind the catalog to the current state-dict root: the TOP catalog SMT `{ current:mpt ->
   * mptRoot, epoch:hot -> hotRoot, epoch:sealed -> level1Root }` and its root (the breadcrumb's
   * `catalogRoot`).
   */
  def compose(mptRoot: Hash)(implicit F: Sync[F], H: JsonBinaryHasher[F]): F[(InMemorySparseMerkleTree[F], SparseMerkleRoot)] =
    for {
      hotRoot    <- hotTree.root
      level1Root <- level1Tree.root
      top <- InMemorySparseMerkleTree.make[F](
        Map(
          currentMptKey   -> rootValueBytes(mptRoot),
          hotEpochsKey    -> rootValueBytes(hotRoot),
          sealedEpochsKey -> rootValueBytes(level1Root)
        )
      )
      root <- top.root
    } yield (top, root)

  /** The replication/hydration payload: enough to rebuild this catalog elsewhere. */
  def contents: CatalogContents =
    CatalogContents(
      epochSize = epochSize,
      hot = hotEntries,
      level1 = level1Entries,
      sealedEpochs = sealedTrees.map { case (e, s) => e -> s.entries }
    )

  /**
   * Serve the catalog-side attestation of `ordinal` against the composed catalog root: inclusion
   * via the hot tree or the sealed path, or non-membership at both. `top` must be the tree
   * [[compose]] produced for the CURRENT committed roots.
   */
  def proveOrdinal(ordinal: Long, top: InMemorySparseMerkleTree[F])(
    implicit F: Sync[F]
  ): F[Either[CommittedProofError, OrdinalCatalogProof]] = {
    val epoch = epochOf(ordinal, epochSize)

    def prove(tree: InMemorySparseMerkleTree[F], key: Hex) =
      EitherT(tree.prover.flatMap(_.prove(key)))
        .leftMap(CommittedProofError.ProofUnavailable(_): CommittedProofError)

    val sealedEntryProof: EitherT[F, CommittedProofError, Option[SparseMerkleProof]] =
      if (!level1Entries.contains(epoch))
        EitherT.rightT[F, CommittedProofError](none[SparseMerkleProof])
      else
        sealedTrees.get(epoch) match {
          case None              => EitherT.leftT[F, Option[SparseMerkleProof]](CommittedProofError.EpochPruned(epoch): CommittedProofError)
          case Some(sealedEpoch) => prove(sealedEpoch.tree, ordinalKey(ordinal)).map(_.some)
        }

    (for {
      topHot      <- prove(top, hotEpochsKey)
      topSealed   <- prove(top, sealedEpochsKey)
      hot         <- prove(hotTree, ordinalKey(ordinal))
      level1      <- prove(level1Tree, epochKey(epoch))
      sealedEntry <- sealedEntryProof
    } yield OrdinalCatalogProof(ordinal, topHot, topSealed, hot, level1, sealedEntry)).value
  }
}

object EpochCatalog {

  /** A sealed epoch retained for SERVING: its full `ordinal -> mptRoot` contents plus the frozen tree. */
  final case class SealedEpoch[F[_]](entries: SortedMap[Long, Hash], tree: InMemorySparseMerkleTree[F])

  /** Emitted by [[EpochCatalog.advance]] when an epoch boundary seals the hot tree. */
  final case class SealEvent(epoch: Long, root: SparseMerkleRoot, sealedOrdinals: List[Long])

  /** The empty catalog (genesis: no history yet). */
  def empty[F[_]: Sync: JsonBinaryHasher](config: CommittedConfig): F[EpochCatalog[F]] =
    for {
      hot    <- InMemorySparseMerkleTree.empty[F]
      level1 <- InMemorySparseMerkleTree.empty[F]
    } yield
      EpochCatalog(
        config.epochSize,
        config.sealedEpochRetention,
        0L,
        SortedMap.empty,
        hot,
        SortedMap.empty,
        level1,
        SortedMap.empty
      )

  /**
   * Rebuild a catalog from transported [[CatalogContents]] (hydration / replication). Validates
   * internal consistency: hot ordinals all in one epoch later than every sealed epoch, and each
   * transported sealed epoch's rebuilt root equal to its level-1 entry. The COMMITMENT itself is
   * NOT trusted from here -- callers must compare `compose(...)` against an attested root.
   */
  def fromContents[F[_]: Sync: JsonBinaryHasher](
    config: CommittedConfig,
    contents: CatalogContents
  ): F[Either[CommittedStateError, EpochCatalog[F]]] = {
    val epochSize = contents.epochSize
    val hotEpochs = contents.hot.keySet.map(CommitCatalog.epochOf(_, epochSize))
    val epochIndex = hotEpochs.headOption.getOrElse(contents.level1.lastOption.map(_._1 + 1).getOrElse(0L))

    val structural: Either[CommittedStateError, Unit] =
      if (epochSize <= 0)
        CommittedStateError.MalformedCatalogContents(s"epochSize must be positive: $epochSize").asLeft
      else if (hotEpochs.size > 1)
        CommittedStateError.MalformedCatalogContents(s"hot ordinals span multiple epochs: $hotEpochs").asLeft
      else if (contents.level1.lastOption.exists(_._1 >= epochIndex) && hotEpochs.nonEmpty)
        CommittedStateError.MalformedCatalogContents("level-1 contains the hot epoch or a later one").asLeft
      else if (!contents.sealedEpochs.keySet.subsetOf(contents.level1.keySet))
        CommittedStateError.MalformedCatalogContents("sealed epoch contents without a level-1 root").asLeft
      else ().asRight

    structural.flatTraverse { _ =>
      for {
        hot <- InMemorySparseMerkleTree.make[F](
          contents.hot.map { case (o, h) => CommitCatalog.ordinalKey(o) -> CommitCatalog.rootValueBytes(h) }.toMap
        )
        level1 <- InMemorySparseMerkleTree.make[F](
          contents.level1.map { case (e, h) => CommitCatalog.epochKey(e) -> CommitCatalog.rootValueBytes(h) }.toMap
        )
        rebuiltSealed <- contents.sealedEpochs.toList.traverse {
          case (epoch, entries) =>
            InMemorySparseMerkleTree
              .make[F](entries.map { case (o, h) => CommitCatalog.ordinalKey(o) -> CommitCatalog.rootValueBytes(h) }.toMap)
              .flatMap(tree => tree.root.map(root => (epoch, SealedEpoch(entries, tree), root)))
        }
        mismatch = rebuiltSealed.collectFirst {
          case (epoch, _, root) if !contents.level1.get(epoch).contains(root.value) => epoch
        }
      } yield
        mismatch match {
          case Some(epoch) =>
            (CommittedStateError.MalformedCatalogContents(
              s"sealed epoch $epoch contents do not reproduce its level-1 root"
            ): CommittedStateError).asLeft
          case None =>
            EpochCatalog(
              epochSize,
              config.sealedEpochRetention,
              epochIndex,
              contents.hot,
              hot,
              contents.level1,
              level1,
              SortedMap.from(rebuiltSealed.map { case (e, s, _) => e -> s })
            ).asRight[CommittedStateError]
        }
    }
  }

  private[committed] def requireInMemory[F[_]: Sync](
    tree: SparseMerkleTree[F]
  ): F[InMemorySparseMerkleTree[F]] =
    tree match {
      case t: InMemorySparseMerkleTree[F] => t.pure[F]
      case other =>
        Sync[F].raiseError(CommittedStateError.CatalogImplementationMismatch(other.getClass.getName))
    }
}

/**
 * The transportable catalog payload: everything needed to rebuild an [[EpochCatalog]] -- the hot
 * epoch entries, the level-1 sealed-epoch roots, and (optionally, for serving) retained sealed
 * epochs' full contents. `epochSize` is included because the rollup geometry is part of the
 * commitment.
 */
final case class CatalogContents(
  epochSize: Int,
  hot: SortedMap[Long, Hash],
  level1: SortedMap[Long, Hash],
  sealedEpochs: SortedMap[Long, SortedMap[Long, Hash]]
)

object CatalogContents {

  implicit private val longKeyEncoder: KeyEncoder[Long] = KeyEncoder.encodeKeyLong
  implicit private val longKeyDecoder: KeyDecoder[Long] = KeyDecoder.decodeKeyLong

  implicit private val longHashMapEncoder: Encoder[SortedMap[Long, Hash]] =
    Encoder.encodeMap[Long, Hash].contramap(identity)

  implicit private val longHashMapDecoder: Decoder[SortedMap[Long, Hash]] =
    Decoder.decodeMap[Long, Hash].map(SortedMap.from(_))

  implicit val encoder: Encoder[CatalogContents] =
    (c: CatalogContents) =>
      Json.obj(
        "epochSize"    -> c.epochSize.asJson,
        "hot"          -> c.hot.asJson,
        "level1"       -> c.level1.asJson,
        "sealedEpochs" -> Encoder.encodeMap[Long, SortedMap[Long, Hash]].apply(c.sealedEpochs)
      )

  implicit val decoder: Decoder[CatalogContents] = (c: HCursor) =>
    for {
      epochSize <- c.downField("epochSize").as[Int]
      hot       <- c.downField("hot").as[SortedMap[Long, Hash]]
      level1    <- c.downField("level1").as[SortedMap[Long, Hash]]
      sealedEpochs <- c
        .downField("sealedEpochs")
        .as[Map[Long, SortedMap[Long, Hash]]]
        .map(SortedMap.from(_))
    } yield CatalogContents(epochSize, hot, level1, sealedEpochs)
}
