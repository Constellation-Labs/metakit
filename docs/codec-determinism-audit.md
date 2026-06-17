# Codec determinism audit — Map/Set ordering in hashed serializations

Date: 2026-06-17. Scope: metakit `crypto/` (merkle, mpt, smt), `lifecycle/committed/`, and
`json_logic/core` codecs. Question raised during the codec-hardening pass: **can the iteration order
of a `Map`/`Set` in a hand-rolled `.asJson` encoder leak into a consensus-hashed root, creating a
cross-Scala/JVM-version fork risk?** (e.g. `MerklePatriciaNode.Branch.paths.toSeq.sortBy(_._1.value)
.toMap.asJson` — the `.toMap` discards the sort, leaving HashMap iteration order).

## Verdict: NO ordering fork risk. The hash path is RFC 8785 canonicalized.

Everything hashed for consensus routes through `JsonBinaryHasher.computeDigest` →
`JsonBinaryCodec.serialize`, which **sorts object keys** (and drops nulls) before hashing. The
`.asJson` encoder's `Map` iteration order is therefore irrelevant to the hashed bytes. The earlier
"MPT Branch HashMap-order" concern is a **false alarm**: the canonicalizer re-sorts at hash time.

## Evidence

1. **The hash path canonicalizes.** `JsonBinaryCodec.serialize`
   (`std/JsonBinaryCodec.scala:135-140`) is `dropNulls(content.asJson)` →
   `JsonCanonicalizer.canonicalizeJson` → bytes. `JsonBinaryHasher.computeDigest`
   (`std/JsonBinaryHasher.scala:22`) delegates to exactly this. The signed-`DataUpdate` variant
   (`:162-170`) does the same before its Constellation prefix.
2. **The canonicalizer sorts object keys with a fixed comparator.**
   `JsonCanonicalizer.encode` emits objects via `TreeOrderedMap.from(obj.toMap)`
   (`std/JsonCanonicalizer.scala:121`), and `TreeOrderedMap.from` is
   `SortedMap.empty(keyOrdering) ++ map` (`:157-158`). `keyOrdering` (`:53-65`) is lexicographic over
   **UTF-16BE bytes** (RFC 8785) — a constant comparator, NOT `Object.hashCode`/`HashMap` order, so it
   is stable across Scala/JVM versions. ⇒ a `Map[K, V]` encoded in any iteration order hashes to the
   same sorted-key bytes.

## What JCS does NOT fix — checked, and safe

RFC 8785 sorts **object keys** but **preserves array order**, and only applies on the
`serialize` path. Two residual classes were scanned:

3. **`Set` encoded as a JSON array (order preserved by JCS).** The only `Set`s that reach a hashed
   serialization are `SortedSet` and so are already deterministically ordered at the source:
   `CommitDelta.removes: SortedSet[CommitKey]` (`lifecycle/committed/CommittedView.scala:41`) and
   `StateDelta.removes: SortedSet[CommitKey]` (`lifecycle/committed/StateDelta.scala:23`). Every other
   `Set[...]` in the tree is an **internal working set** (producer dirty-key/index tracking, batch
   prover `seen`) that is never serialized into a hashed structure. No unsorted `Set`-as-array on a
   hashed path.
4. **Hash paths that BYPASS `serialize` (raw `Hash.fromBytes`).** All hash fixed-order bytes, no
   `Map`/`Set` iteration:
   - `CommitCatalog.scala:55` — `Hash.fromBytes(name.getBytes(UTF_8))` (a single name string).
   - `crypto/smt/SparseMerkleHashing.scala:42,46` — `Hash.fromBytes` of raw key / value bytes.
   - `lifecycle/committed/CommittedRoots.scala:28` — `Hash.fromBytes(mptRootBytes ++ catalogRootBytes)`
     (two roots in a fixed order).

   Consensus-serialized `Map`s in the committed layer are additionally `SortedMap` at the source
   (`CommitDelta.upserts: SortedMap[CommitKey, Json]`), so they are ordered even before JCS.

## Notes (non-blocking)

- **Dead sort, harmless.** `MerklePatriciaNode.Branch` (`crypto/mpt/MerklePatriciaNode.scala:58,66`)
  and the `Branch` commitment build `paths.toSeq.sortBy(_._1.value).toMap` — the `.toMap` discards the
  ordering, but JCS re-sorts at hash time, so the result is correct regardless. The `sortBy` is
  redundant; it could be dropped (or the sorted form encoded as an array) for clarity, but that is a
  cosmetic change, not a consensus fix — and changing the *encoded shape* WOULD change the hash, so it
  must not be done casually.
- **`MapValue` (JLVM).** `json_logic/core/JsonLogicValue.scala` encodes `MapValue` in `Map` order, but
  it hashes through the same canonicalizer, and the JLVM runtime itself reuses `keyOrdering` for
  object-form `let` evaluation (`JsonCanonicalizer.scala:45-51`) — consistent.

## Invariant to preserve (for future hashed content)

1. Hash/sign consensus content ONLY through `JsonBinaryHasher.computeDigest` /
   `JsonBinaryCodec.serialize` — never `Hash.fromBytes(json.noSpaces.getBytes)` or a raw `.asJson`
   digest, so dropNulls + RFC 8785 key-sorting are inherited.
2. Any `Set`/sequence-of-unordered placed in hashed content must be a `SortedSet` or sorted before
   encoding — **JCS does not sort arrays**.
3. A `Map` in hashed content is safe key-order-wise (JCS sorts), but prefer `SortedMap` at the source
   for local determinism + readable golden vectors.

## Conclusion

No determinism fork risk from `Map`/`Set` ordering. The codec-hardening pass (wire-format KATs +
decoder-align round-trip fixes + derive-where-byte-identical) is safe to resume; it is orthogonal to
the hash canonicalization (the KATs pin the `.asJson` field-name/discriminator contract; the hash is
independently canonicalized).

## Appendix: `Array[Byte]` value-type audit

Reviewed every `Array[Byte]` used as a CASE-CLASS FIELD (not a method param / local) across metakit
`crypto`, `lifecycle/committed`, and `json_logic`, asking: does it back a SERIALIZED (circe) type and
thus create a bespoke wire format for a mutable JVM array with no structural equality?

- **Fixed** (had a wire format): `SparseMerkleProof.Inclusion.value` (a circe-coded proof → custom
  `valueEncoder`/`valueDecoder`) and `SparseMerkleEntry.Present.value` (custom `sameElements` Eq) →
  now `Hex`, which has a codec + structural equality + immutability. Wire/hash-compatible; consumers
  convert at the boundary with `.toBytes` / `Hex.fromBytes`. (See the `refactor(smt)` commit.)
- **Left as-is** (appropriate — NO circe codec, never serialized, raw bytes are the natural form):
  - `SparseMerkleNode.Leaf.value` — the in-memory SMT node (hashed via its `SparseMerkleCommitment`,
    not serialized directly).
  - sigma `PropNode`/`ProofNode` byte fields (`pkBytes`, challenge `e`) in `CryptoOps` — the internal
    parsed AST of `sigma_verify` (its wire form is the JsonLogicValue / hex strings); raw bytes for the
    XOR / GF(2^8) / curve operations.
- **ottochain:** no `Array[Byte]` case-class fields.

Rule going forward: `Array[Byte]` is fine for INTERNAL/raw byte buffers, but a SERIALIZED case-class
field should be `Hex` (tessellation) — codec + structural equality + immutability, no bespoke array
wire format.
