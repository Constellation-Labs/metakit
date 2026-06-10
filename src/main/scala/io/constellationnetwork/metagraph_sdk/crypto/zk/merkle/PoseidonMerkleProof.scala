package io.constellationnetwork.metagraph_sdk.crypto.zk.merkle

import cats.Eq
import cats.syntax.eq._

/**
 * An authentication path against a [[PoseidonMerkleTree]] root, over BN254 Fr.
 *
 * This is the circuit-friendly analogue of the byte/SHA sparse-Merkle proof on the `refactor/crypto-smt`
 * branch, re-expressed for a FIXED-DEPTH tree whose nodes are hashed with [[io.constellationnetwork.metagraph_sdk.crypto.zk.poseidon.Poseidon]]
 * over the alt_bn128 scalar field. Every element here -- `position`, `leaf`, each sibling, and the root --
 * is a canonical field element in `[0, R)` (see [[Fr]]).
 *
 * Unlike the collapsing JMT-style proof (which carries only as many siblings as the descent is deep, plus
 * a terminating witness), this proof is for a tree of FIXED depth `siblings.length`, so it ALWAYS carries
 * exactly `depth` siblings. There is therefore no separate "terminating slot" -- inclusion and absence are
 * the SAME authentication-path fold and differ only in the claimed leaf:
 *
 *   - INCLUSION of a note commitment `c` at `position`: fold the leaf `c` up the path and check it equals the root.
 *   - ABSENCE of `position` (e.g. a nullifier still unspent): fold the ZERO leaf ([[Fr.Zero]]) up the path and
 *     check it equals the root. Proving the zero leaf folds to the root proves nothing non-zero occupies `position`.
 *
 * Sibling ordering convention (root-first / top-down):
 *   - `siblings(0)` is the sibling at the TOP of the tree (the node at depth `0`, i.e. the other child of the root),
 *   - `siblings(depth - 1)` is the sibling adjacent to the leaf (at the BOTTOM).
 *
 * A verifier folds from the BOTTOM up (`siblings.reverse`). At level `i` (counted from the leaf, `i = 0` at the
 * bottom) it inspects bit `i` of `position`:
 *   - bit `i == 0` => the node on the path is the LEFT child: `parent = compress(current, sibling)`,
 *   - bit `i == 1` => the node on the path is the RIGHT child: `parent = compress(sibling, current)`.
 *
 * This left/right rule matches [[PoseidonMerkleTree]]'s insertion descent, where bit `i` of `position` selects the
 * child at level `i` (LSB = deepest level, adjacent to the leaf).
 */
final case class PoseidonMerkleProof(position: BigInt, siblings: Vector[BigInt]) {

  /** The fixed depth of the tree this proof was produced against (= number of siblings). */
  def depth: Int = siblings.length
}

object PoseidonMerkleProof {

  implicit val eq: Eq[PoseidonMerkleProof] =
    Eq.instance((a, b) => a.position === b.position && a.siblings === b.siblings)
}
