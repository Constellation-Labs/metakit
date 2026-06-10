package io.constellationnetwork.metagraph_sdk.crypto.zk.merkle

import scala.annotation.tailrec

import io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon

/**
 * A FIXED-DEPTH, sparse, incremental Merkle tree over the BN254 / alt_bn128 scalar field (Fr), with all node hashing
 * done by [[Poseidon]] (`compress` for internal nodes). This is the "circuit-friendly" tree backing the zk-JLVM's
 * shielded state: a single position-keyed structure that serves BOTH primitives the design needs.
 *
 * ==Why one position-keyed tree for both use cases==
 *
 * The shielded design needs (a) note-commitment MEMBERSHIP and (b) nullifier NON-membership. A single fixed-depth
 * tree keyed by a leaf position in `[0, 2^depth)` covers both, because absence is first-class here:
 *
 *   - NOTE COMMITMENTS: a note commitment `c = Poseidon(...)` is an Fr leaf inserted at some position `p` (e.g. the
 *     next free slot of an append-only commitment tree). Membership of `c` is an [[inclusionProof]] at `p`.
 *   - NULLIFIERS: a nullifier set is a position-keyed set where "unspent" means the leaf at that position is still the
 *     ZERO leaf and "spent" means a non-zero marker has been inserted there. Non-membership (the spend is allowed) is
 *     an [[absenceProof]]: prove the ZERO leaf at `p` folds to the root. Spending later [[insert]]s a non-zero leaf at
 *     `p`, after which the absence proof no longer verifies.
 *
 * Because absence is "the zero leaf folds to the root" and inclusion is "the claimed leaf folds to the root", the two
 * proofs are the SAME authentication-path shape and the SAME [[PoseidonMerkleTree.verify]] fold -- only the claimed
 * leaf differs ([[Fr.Zero]] for absence). This keeps the in-circuit verifier minimal: ONE fold gadget, no branching
 * on a proof variant. (Contrast the collapsing JMT-style proof on `refactor/crypto-smt`, which has a distinct
 * "terminating witness" for absence; a fixed-depth tree does not need it, which is exactly what we want for a SNARK.)
 *
 * ==Empty-subtree (zero) convention==
 *
 * The canonical empty leaf is [[Fr.Zero]] (`0`). Empty subtrees are summarised by precomputed per-level "zero hashes":
 *
 *   zero(0) = 0                                  // an empty leaf
 *   zero(i) = Poseidon.compress(zero(i-1), zero(i-1))   for 1 <= i <= depth
 *
 * `zero(i)` is the root digest of a perfectly empty subtree of height `i`. The empty-tree root is `zero(depth)`. A
 * sparse internal node whose subtree is entirely empty is never materialised; its digest is read from `zeroHashes(i)`.
 *
 * ==Position bit / left-right convention==
 *
 * A position `p` is a `depth`-bit index. Bit `i` of `p` (LSB-first) selects the child at LEVEL `i`, where level `0`
 * is the BOTTOM (adjacent to the leaf) and level `depth-1` is the TOP (children of the root):
 *
 *   - bit `i == 0` => the path goes LEFT at level `i`; the sibling is the RIGHT child,
 *   - bit `i == 1` => the path goes RIGHT at level `i`; the sibling is the LEFT child.
 *
 * Hashing therefore always preserves left/right order: `parent = compress(leftChildDigest, rightChildDigest)`.
 *
 * ==Determinism==
 *
 * The structure is a pure function of the live `position -> leaf` map (sparse, with structural sharing). The [[root]]
 * depends only on that map, NOT on insertion order: inserting the same set of entries in any order yields the same root.
 *
 * Instances are IMMUTABLE; [[insert]] returns a new tree sharing untouched subtrees with the receiver.
 *
 * @param depth      the fixed tree depth (number of levels of internal nodes; leaves live at the bottom). Capacity is
 *                   `2^depth` positions.
 * @param zeroHashes precomputed empty-subtree hashes, `zeroHashes(i) == zero(i)` for `0 <= i <= depth`.
 * @param nodes      the sparse, materialised non-empty subtree digests, keyed by `(level, index-at-level)`. A leaf is
 *                   `(0, position)`; the root would be `(depth, 0)`. Entries equal to the level's zero hash are pruned.
 * @param leaves     the live, non-zero leaves keyed by position (the logical contents).
 */
final class PoseidonMerkleTree private (
  val depth: Int,
  val zeroHashes: Vector[BigInt],
  private val nodes: Map[(Int, BigInt), BigInt],
  private val leaves: Map[BigInt, BigInt]
) {

  /** The number of positions this tree can hold (`2^depth`). */
  def capacity: BigInt = BigInt(1) << depth

  /** The root commitment of the tree (the empty-tree root `zero(depth)` when no leaves are set). */
  def root: BigInt = nodes.getOrElse((depth, BigInt(0)), zeroHashes(depth))

  /** The leaf currently stored at `position` ([[Fr.Zero]] if the position has never been set). */
  def leafAt(position: BigInt): BigInt = {
    requirePosition(position)
    leaves.getOrElse(position, Fr.Zero)
  }

  /** True iff `position` currently holds a non-zero leaf. */
  def isSet(position: BigInt): Boolean = leaves.contains(position)

  /**
   * Return a NEW tree with `leaf` stored at `position`, recomputing only the digests on `position`'s root-to-leaf path
   * (structural sharing for everything else). `leaf` must be a canonical Fr element; inserting [[Fr.Zero]] clears the
   * position back to the empty-leaf state.
   */
  def insert(position: BigInt, leaf: BigInt): PoseidonMerkleTree = {
    requirePosition(position)
    Fr.requireCanonical(leaf, s"leaf at position $position")

    val newLeaves =
      if (leaf == Fr.Zero) leaves - position
      else leaves.updated(position, leaf)

    // Recompute the path bottom-up, updating (or pruning) one node per level.
    @tailrec
    def climb(level: Int, idx: BigInt, digest: BigInt, acc: Map[(Int, BigInt), BigInt]): Map[(Int, BigInt), BigInt] = {
      val acc1 =
        if (digest == zeroHashes(level)) acc - ((level, idx))
        else acc.updated((level, idx), digest)

      if (level == depth) acc1
      else {
        // bit (level) of `position` decides whether `idx` is the left (0) or right (1) child of its parent.
        val bit = position.testBit(level)
        val siblingIdx = if (bit) idx - 1 else idx + 1
        val siblingDigest = acc1.getOrElse((level, siblingIdx), zeroHashes(level))
        val parentDigest =
          if (bit) Poseidon.compress(siblingDigest, digest) // path node is the RIGHT child
          else Poseidon.compress(digest, siblingDigest) // path node is the LEFT child
        climb(level + 1, idx >> 1, parentDigest, acc1)
      }
    }

    val newNodes = climb(0, position, leaf, nodes)
    new PoseidonMerkleTree(depth, zeroHashes, newNodes, newLeaves)
  }

  /** Insert many `position -> leaf` entries; the result is order-independent (a pure function of the entry map). */
  def insertAll(entries: IterableOnce[(BigInt, BigInt)]): PoseidonMerkleTree =
    entries.iterator.foldLeft(this) { case (t, (p, l)) => t.insert(p, l) }

  /**
   * The authentication path (root-first sibling digests) for `position`. The proof is shape-identical whether the
   * position is set (inclusion) or empty (absence); see [[PoseidonMerkleProof]]. Returns exactly `depth` siblings.
   */
  def proof(position: BigInt): PoseidonMerkleProof = {
    requirePosition(position)

    // Bottom-up collect siblings, then reverse to root-first (top-down) ordering.
    @tailrec
    def collect(level: Int, idx: BigInt, accBottomUp: List[BigInt]): List[BigInt] =
      if (level == depth) accBottomUp
      else {
        val bit = position.testBit(level)
        val siblingIdx = if (bit) idx - 1 else idx + 1
        val siblingDigest = nodes.getOrElse((level, siblingIdx), zeroHashes(level))
        collect(level + 1, idx >> 1, siblingDigest :: accBottomUp)
      }

    // collect builds bottom-first via prepend, so accBottomUp ends up TOP-first already (root-first). Good.
    PoseidonMerkleProof(position, collect(0, position, Nil).toVector)
  }

  /**
   * An INCLUSION proof: the authentication path proving `leafAt(position)` is committed at the [[root]]. Identical in
   * shape to [[proof]]; the verifier folds the claimed leaf and checks it equals the root.
   */
  def inclusionProof(position: BigInt): PoseidonMerkleProof = proof(position)

  /**
   * An ABSENCE proof for `position`: the authentication path proving the ZERO leaf occupies `position` (i.e. nothing
   * non-zero is there). Requires the position to be currently empty -- absence of a SET position is not provable.
   * Identical in shape to [[proof]]; the verifier folds [[Fr.Zero]] and checks it equals the root.
   */
  def absenceProof(position: BigInt): PoseidonMerkleProof = {
    requirePosition(position)
    require(!isSet(position), s"cannot produce an absence proof for position $position: it holds a non-zero leaf")
    proof(position)
  }

  private def requirePosition(position: BigInt): Unit =
    require(
      position >= 0 && position < capacity,
      s"position out of range: must be in [0, 2^$depth) = [0, $capacity); got $position"
    )
}

object PoseidonMerkleTree {

  /** The default fixed depth (capacity `2^32` positions), a common choice for shielded note-commitment trees. */
  val DefaultDepth: Int = 32

  /**
   * Precompute the empty-subtree ("zero") hashes for a tree of the given depth, indexable as `0..depth`:
   *
   *   zero(0) = 0; zero(i) = Poseidon.compress(zero(i-1), zero(i-1)).
   *
   * `zeroHashes(depth)` is the empty-tree root.
   */
  def zeroHashes(depth: Int): Vector[BigInt] = {
    require(depth >= 1, s"depth must be >= 1; got $depth")
    val builder = Vector.newBuilder[BigInt]
    @tailrec
    def loop(level: Int, prev: BigInt): Unit = {
      builder += prev
      if (level < depth) loop(level + 1, Poseidon.compress(prev, prev))
    }
    loop(0, Fr.Zero)
    builder.result()
  }

  /** An empty tree of [[DefaultDepth]] (capacity `2^32`). */
  def empty: PoseidonMerkleTree = empty(DefaultDepth)

  /** An empty tree of the given fixed `depth` (capacity `2^depth`). */
  def empty(depth: Int): PoseidonMerkleTree =
    new PoseidonMerkleTree(depth, zeroHashes(depth), Map.empty, Map.empty)

  /** Build a tree of the given `depth` directly from a `position -> leaf` map (order-independent). */
  def fromLeaves(depth: Int, leaves: IterableOnce[(BigInt, BigInt)]): PoseidonMerkleTree =
    empty(depth).insertAll(leaves)

  /**
   * The fold at the heart of BOTH inclusion and absence verification: fold `leaf` up the authentication path and
   * return the recomputed root. At level `i` (from the bottom), bit `i` of `proof.position` selects left/right:
   *
   *   - bit `i == 0` => path node is the LEFT child:  parent = compress(current, sibling)
   *   - bit `i == 1` => path node is the RIGHT child: parent = compress(sibling, current)
   *
   * `proof.siblings` is root-first, so it is consumed in reverse (bottom-up). Inputs are validated as canonical Fr.
   */
  def computeRoot(leaf: BigInt, proof: PoseidonMerkleProof): BigInt = {
    Fr.requireCanonical(leaf, "leaf")
    proof.siblings.zipWithIndex.foreach { case (s, i) => Fr.requireCanonical(s, s"sibling[$i]") }
    require(
      proof.position >= 0 && proof.position < (BigInt(1) << proof.depth),
      s"proof position out of range for depth ${proof.depth}: ${proof.position}"
    )

    // siblings are root-first; reverse to fold from the leaf upward (bottom level = index 0).
    proof.siblings.reverse.zipWithIndex.foldLeft(leaf) {
      case (current, (sibling, level)) =>
        if (proof.position.testBit(level)) Poseidon.compress(sibling, current) // path node is RIGHT child
        else Poseidon.compress(current, sibling) // path node is LEFT child
    }
  }

  /** Verify an INCLUSION proof: `leaf` is committed at `proof.position` under `root`. */
  def verifyInclusion(leaf: BigInt, proof: PoseidonMerkleProof, root: BigInt): Boolean =
    computeRoot(leaf, proof) == root

  /**
   * Verify an ABSENCE proof: `proof.position` still holds the ZERO leaf under `root` (nothing non-zero occupies it).
   * This is exactly [[verifyInclusion]] with the claimed leaf fixed to [[Fr.Zero]].
   */
  def verifyAbsence(proof: PoseidonMerkleProof, root: BigInt): Boolean =
    verifyInclusion(Fr.Zero, proof, root)
}
