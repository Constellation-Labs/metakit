# Committed state: key namespaces and the root catalog

This is the key specification for the `lifecycle/committed` module — the two-tier state-root
commitment (`CommittedView` → MPT state-dict + SMT root catalog) that `CommittedApp.makeL0` wires
into a metagraph L0 data application.

## 1. CommitKey grammar (tier 1: the MPT state dictionary)

A `CommitKey` is a validated, namespaced path into the committed dictionary:

```
key      = segment *( "/" segment )
segment  = seg-head *seg-tail
seg-head = %x61-7A / %x30-39              ; [a-z0-9]
seg-tail = %x61-7A / %x30-39 / "_" / "." / "-"
```

Limits (enforced by `CommitKey.from`):

| limit               | value |
|---------------------|-------|
| max segment length  | 64 characters |
| max segments        | 16 |
| max total length    | 256 characters |
| case                | lowercase only |
| empty segments      | rejected (no leading/trailing/double `/`) |

### MPT path encoding

The MPT path of a key is the **lowercase hex of its UTF-8 bytes** (`CommitKey.toHex`), giving
variable-length, byte-aligned trie paths. Because `/` is a single byte (`0x2f`), the hex of
`"ns/"` is a strict prefix of the hex of every key under `ns` — this is what makes namespace
prefix proofs segment-exact. `CommitNamespace.prefixHex` is the hex of `value + "/"`, so the
prefix attestation for namespace `fiber` can never leak keys under `fiberx/`.

The encoding is directly compatible with the JLVM auth-DB opcodes (`mpt_verify`,
`mpt_prefix_verify`): those take the same lowercase hex with a `0x` prefix added (`"0x" +
key.toHex.value`), values as JSON, and the proof JSON exactly as the metakit provers emit it.
See `AuthDbOps` / `HexBytes.parseNibbleHex` — odd nibble counts are legal there, and our
byte-aligned (even-nibble) paths are a subset. Compatibility is exercised by
`CommittedProofSuite`.

### Reserved top-level namespaces

| namespace          | contents |
|--------------------|----------|
| `fiber/<uuid>`     | state-machine fiber state (uuid lowercase hyphenated) |
| `registry/<name>`  | registry entries |
| `oracle/<id>`      | oracle/script state |
| `meta/...`         | module/system metadata (versioning, schema ids, config digests) |

Applications may add further top-level namespaces, but must not repurpose the reserved ones.

## 2. SMT catalog key scheme (tier 2: the root catalog)

The catalog is a `SparseMerkleTree` of **fixed-length keys**: `sha256(name)` rendered as the
64-char lowercase hex `Hex` (see `CommitCatalog.catalogKey`). Names are `family:qualifier`
strings:

| name           | value (32 raw bytes)                      |
|----------------|-------------------------------------------|
| `current:mpt`  | the CURRENT state-dict MPT root            |
| `ordinal:<N>`  | the MPT root committed at snapshot ordinal N (`<N>` decimal, no padding) |

The stored SMT value is the raw 32-byte root digest (`CommitCatalog.rootValueBytes`). The scheme
is extensible: any other root a metagraph commits (a Poseidon shadow root, a sub-registry root, a
cross-module index) gets its own family name. SMT absence proofs are first-class, so
"ordinal `N` was never committed" is provable against `smtRoot`.

## 3. The commitment and `hashCalculatedState`

Per snapshot the module exposes `commitment = (mptRoot, smtRoot)` and defines the single
consensus-facing hash:

```
calculatedStateHash = sha256( rawBytes(mptRoot) ++ rawBytes(canonicalSmtRoot) )
```

where `canonicalSmtRoot` is the root of the **canonical single-entry catalog**
`{ sha256("current:mpt") → mptRoot }`. Rationale (`CommittedCommitment` scaladoc has the full
version): `hashCalculatedState` must be a *pure function of the state value* — tessellation calls
it on freshly downloaded state with no local history — while the live catalog root depends on the
historical `ordinal:<N>` entries. The canonical catalog keeps the hash pure in the value yet still
binds tier 2's key scheme and hash discipline. Nothing is lost: each historical `ordinal:<N>` root
was `current:mpt` at ordinal N and is anchored by snapshot N's own calculated-state proof. The
live catalog (with history) is a node-local, *verifiable* index served by `/committed/root` and
SMT proofs.

## 4. Routes (all read one cell value)

```
GET  /committed/root                 { ordinal, mptRoot, smtRoot, calculatedStateHash }
GET  /committed/proof/<key...>       single-key inclusion proof
POST /committed/proofs               batch proof, body { "keys": [ "ns/a", ... ] }
GET  /committed/proof-prefix/<ns...> complete prefix attestation for a namespace
GET  /committed/delta/:ordinal       StateDelta (404 after ring-buffer eviction)
GET  /committed/snapshot             CommittedSnapshot (replication fallback)
```

## 5. Why a hierarchy: super-registries and cross-metagraph slices

The namespace hierarchy is the unit of *delegated commitment*. A super-registry can commit
sub-registries as namespaces (`registry/<name>/...`), and later promote a sub-registry to its own
catalog entry without changing key shapes. Cross-metagraph: ML0_B asks ML0_A for
`/committed/proof-prefix/registry/tokens` and verifies the returned complete slice against ML0_A's
`mptRoot` — trust-anchored without trusting the serving node, via gl0's
`CurrencySnapshotMptRoots` → the snapshot's `calculatedStateProof` (= `hashCalculatedState`
above) → `mptRoot`. The same path serves JLVM contracts on ML0_B through `mpt_prefix_verify`.
