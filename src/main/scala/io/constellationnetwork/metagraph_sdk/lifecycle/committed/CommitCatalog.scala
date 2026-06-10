package io.constellationnetwork.metagraph_sdk.lifecycle.committed

import java.nio.charset.StandardCharsets

import scala.collection.immutable.SortedMap

import io.constellationnetwork.schema.SnapshotOrdinal
import io.constellationnetwork.security.hash.Hash
import io.constellationnetwork.security.hex.Hex

/**
 * The SMT root-catalog key scheme (tier 2 of the commitment).
 *
 * The catalog is a SparseMerkleTree keyed by FIXED-length keys: `sha256(name)` rendered as the
 * 64-char lowercase hex `Hex` the SMT consumes. Names are `family:qualifier` strings:
 *
 *   - `current:mpt`  -> the CURRENT state-dict MPT root
 *   - `ordinal:<N>`  -> the MPT root committed at snapshot ordinal N (historical; N decimal, no padding)
 *
 * and the scheme is extensible -- any other root a metagraph wants to commit (e.g. a Poseidon
 * shadow root, a sub-registry root) gets its own name family. Values stored in the SMT are the 32
 * RAW bytes of the committed root hash ([[rootValueBytes]]).
 */
object CommitCatalog {

  val CurrentMptName: String = "current:mpt"

  def ordinalName(ordinal: SnapshotOrdinal): String = s"ordinal:${ordinal.value.value}"

  /** Catalog key for `name`: sha256(name) as the 64-char lowercase hex `Hex` (fixed-length SMT key). */
  def catalogKey(name: String): Hex =
    Hex(Hash.fromBytes(name.getBytes(StandardCharsets.UTF_8)).value)

  def currentMptKey: Hex = catalogKey(CurrentMptName)

  def ordinalKey(ordinal: SnapshotOrdinal): Hex = catalogKey(ordinalName(ordinal))

  /** The SMT leaf value bound to a committed root: the 32 raw bytes of the root digest. */
  def rootValueBytes(root: Hash): Array[Byte] = Hex(root.value).toBytes

  def rootFromValueBytes(bytes: Array[Byte]): Hash = Hash(Hex.fromBytes(bytes).value)

  /** The catalog upserts a snapshot at `ordinal` with state-dict root `mptRoot` contributes. */
  def changesFor(ordinal: SnapshotOrdinal, mptRoot: Hash): SortedMap[Hex, Hash] =
    SortedMap(
      currentMptKey       -> mptRoot,
      ordinalKey(ordinal) -> mptRoot
    )
}
