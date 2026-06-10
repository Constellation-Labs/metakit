package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import java.nio.charset.StandardCharsets

import io.constellationnetwork.metagraph_sdk.crypto.smt.SparseMerkleRoot
import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

/**
 * The catalog key scheme (tier 2 of the commitment) -- the LIVE catalog committed by
 * `hashCalculatedState`.
 *
 * Every tree in the catalog is a SparseMerkleTree keyed by FIXED-length keys: `sha256(name)`
 * rendered as the 64-char lowercase hex `Hex` the SMT consumes; values are the 32 RAW bytes of the
 * committed root digest ([[rootValueBytes]]). Names are `family:qualifier` strings, spread over
 * three levels (see `docs/committed-namespaces.md` for the full composition):
 *
 * TOP catalog (root = the breadcrumb's `catalogRoot`):
 *   - `current:mpt`  -> the CURRENT state-dict MPT root
 *   - `epoch:hot`    -> root of the HOT epoch SMT (this epoch's historical ordinals)
 *   - `epoch:sealed` -> root of the LEVEL-1 SMT (one entry per sealed epoch)
 *
 * LEVEL-1 SMT:
 *   - `epoch:<E>`    -> the sealed root of epoch E (E decimal, no padding)
 *
 * EPOCH SMTs (hot and sealed alike):
 *   - `ordinal:<N>`  -> the MPT root committed at snapshot ordinal N (N decimal, no padding)
 *
 * The TOP scheme is extensible -- any other root a metagraph wants to commit (e.g. a Poseidon
 * shadow root, a sub-registry root) gets its own name family alongside the three above.
 */
object CommitCatalog {

  val CurrentMptName: String = "current:mpt"

  /** TOP catalog name of the hot epoch SMT root. */
  val HotEpochsName: String = "epoch:hot"

  /** TOP catalog name of the level-1 (sealed epochs) SMT root. */
  val SealedEpochsName: String = "epoch:sealed"

  def ordinalName(ordinal: Long): String = s"ordinal:$ordinal"

  def ordinalName(ordinal: SnapshotOrdinal): String = ordinalName(ordinal.value.value)

  /** Level-1 name of sealed epoch `epoch`'s root. */
  def epochName(epoch: Long): String = s"epoch:$epoch"

  /** The epoch an ordinal belongs to: `ordinal / epochSize`. */
  def epochOf(ordinal: Long, epochSize: Int): Long = ordinal / epochSize

  /** Catalog key for `name`: sha256(name) as the 64-char lowercase hex `Hex` (fixed-length SMT key). */
  def catalogKey(name: String): Hex =
    Hex(Hash.fromBytes(name.getBytes(StandardCharsets.UTF_8)).value)

  def currentMptKey: Hex = catalogKey(CurrentMptName)

  def hotEpochsKey: Hex = catalogKey(HotEpochsName)

  def sealedEpochsKey: Hex = catalogKey(SealedEpochsName)

  def ordinalKey(ordinal: Long): Hex = catalogKey(ordinalName(ordinal))

  def ordinalKey(ordinal: SnapshotOrdinal): Hex = ordinalKey(ordinal.value.value)

  def epochKey(epoch: Long): Hex = catalogKey(epochName(epoch))

  /** The SMT leaf value bound to a committed root: the 32 raw bytes of the root digest. */
  def rootValueBytes(root: Hash): Array[Byte] = Hex(root.value).toBytes

  /** The SMT leaf value bound to a committed SUBTREE root (level-1 / epoch roots in the top catalog). */
  def rootValueBytes(root: SparseMerkleRoot): Array[Byte] = rootValueBytes(root.value)

  def rootFromValueBytes(bytes: Array[Byte]): Hash = Hash(Hex.fromBytes(bytes).value)
}
