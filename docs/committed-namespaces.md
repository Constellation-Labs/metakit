# Committed state: key namespaces, the root catalog, and the on-chain breadcrumb

This is the key specification for the `lifecycle/committed` module — the two-tier state-root
commitment (`CommittedView` → MPT state-dict + epoch-rollup root catalog) that
`CommittedApp.makeL0` wires into a metagraph L0 data application.

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

## 2. Catalog composition (tier 2: the LIVE root catalog, epoch rollup)

The catalog commits the **full root history** — the current state-dict root plus the MPT root of
every past ordinal — without ever growing the consensus-facing artifacts: historical roots are
rolled up in a TWO-LEVEL epoch structure (`EpochCatalog`).

Every catalog tree is a `SparseMerkleTree` keyed by **fixed-length keys**: `sha256(name)` rendered
as the 64-char lowercase hex `Hex` (`CommitCatalog.catalogKey`); stored values are the raw
32 bytes of the committed root digest (`CommitCatalog.rootValueBytes`). Names are
`family:qualifier` strings. The exact composition at ordinal N:

```
catalogRoot_N = SMTroot {                       # the TOP catalog
  sha256("current:mpt")  -> mptRoot_N           # current state-dict root
  sha256("epoch:hot")    -> hotRoot_N           # root of the HOT epoch SMT
  sha256("epoch:sealed") -> level1Root_N        # root of the LEVEL-1 SMT
}

hotRoot_N    = SMTroot { sha256("ordinal:<M>") -> mptRoot_M           # M in the CURRENT epoch, M ≤ N-1 }
level1Root_N = SMTroot { sha256("epoch:<E>")   -> sealedEpochRoot_E   # every SEALED epoch E }
```

with `epoch(M) = M / epochSize` (`CommittedConfig.epochSize`, default **2^16** ordinals;
consensus-critical — every node of a metagraph must run the same value). `<M>`, `<E>` are decimal,
no padding. Empty trees contribute the SMT empty root, so all three TOP entries are always
present. The TOP scheme is extensible: any other root a metagraph commits (a Poseidon shadow
root, a sub-registry root) gets its own name family alongside the three above; SMT absence proofs
make "this family is not committed" provable.

### The per-ordinal transition

```
hot/level1 advance:  insert  ordinal:<N-1> -> mptRoot_(N-1)  into the hot tree,
                     SEALING first if epoch(N-1) > current hot epoch:
                       level1 += epoch:<E_hot> -> hotRoot ; hot := empty
catalogRoot_N      = TOP(mptRoot_N, hotRoot_N, level1Root_N)
```

i.e. the catalog at ordinal N contains the history `ordinal:0 .. ordinal:N-1` plus
`current:mpt -> mptRoot_N`. Followers recompute this transition locally and reject any proposal
whose breadcrumb (below) disagrees (`CommittedStateError.BreadcrumbMismatch`).

### Ordinal proofs

`Committed.proveOrdinal` / `GET /committed/proof-ordinal/:ordinal` produce an
`OrdinalCatalogProof`; `OrdinalCatalogProofVerifier.verify(catalogRoot, proof, epochSize)`
checks it against the single trusted catalog root:

- **hot ordinal** — TOP inclusion of `epoch:hot` + hot-tree inclusion of `ordinal:<M>`;
- **ancient ordinal** — TOP inclusion of `epoch:sealed` + **two fixed-depth inclusions**:
  level-1 inclusion of `epoch:<M/epochSize>` and sealed-epoch-tree inclusion of `ordinal:<M>`;
- **non-membership** — hot-tree ABSENCE of the ordinal **and** absence on the sealed path
  (epoch never sealed, or ordinal absent from the sealed tree).

The verifier recomputes every key from `(ordinal, epochSize)` — the prover cannot route the check
through the wrong tree.

## 3. The commitment, `hashCalculatedState`, and the on-chain breadcrumb

Per snapshot the module commits the pair `roots = (mptRoot, catalogRoot)` and defines the single
consensus-facing hash (`CommittedRoots.combinedHash`):

```
calculatedStateHash = sha256( rawBytes(mptRoot) ++ rawBytes(catalogRoot) )
```

over the **live** catalog — the consensus hash anchors the current state AND its entire root
history.

### The constant on-chain breadcrumb

`CommittedApp.makeL0` registers the service with on-chain type `CommittedOnChain[PUB]` — the
dev's `PUB` plus `CommittedBreadcrumb(ordinal, roots)`. Properties:

- **constant size** — exactly the pair for the snapshot's own ordinal; it never accumulates;
- **correct by construction** — only `combine` (owned by makeL0) builds the wrapper; the dev's
  combiner sees and returns plain `PUB`, so the breadcrumb can be neither omitted nor forged from
  application code;
- **validated** — `combine` checks the incoming state's breadcrumb against the locally committed
  roots before deriving the next one (the follower-side transition check), and a proposer that
  tampers with the emitted breadcrumb produces an artifact honest validators cannot reproduce —
  it cannot gather a majority. `setCalculatedState` additionally cross-checks the locally derived
  roots against the signed snapshot's breadcrumb when one is available for the same ordinal.

### Sourcing the catalog root at hash time (call-ordering soundness)

`hashCalculatedState(state)` needs `catalogRoot`, which is history, not a function of the value.
The implementation (`CommittedState.hashFor`) sources it by tessellation's two call orderings:

1. **Steady state** (`DataApplicationSnapshotAcceptanceManager.accept` /
   `consumeSignedMajorityArtifact`): the hash for snapshot N is computed BEFORE the artifact is
   prepended to snapshot storage (`StateChannelSnapshotService.consume` calls
   `consumeSignedMajorityArtifact` first) and the committed cell sits at N-1 (tessellation
   asserts this via `expectCalculatedStateOrdinal`). The cell is the parent → derive the
   transition locally. Fully self-computed; nothing is trusted.
2. **Bootstrap / download** (`currency-l0 Download.fetchAndSetCalculatedState`): tessellation
   `prepend`s the signed snapshot BEFORE fetching the calculated state, so the latest stored
   snapshot is the one being verified and is AHEAD of the (genesis) cell. The hash uses the
   breadcrumb's attested `catalogRoot` directly: **O(1) bootstrap**, no history replay. Trust is
   the Ethereum-header model — the breadcrumb sits in the majority-SIGNED snapshot, and each
   per-step transition was validated by the then-current validators.
3. **Replay** (`DataApplicationTraverse` folds `combine` over cached snapshots without advancing
   the cell): `combine` derives transitions through a bounded work cache keyed by breadcrumb, so
   consecutive calls chain deterministically; the hash falls back to the most recent derivation
   matching the state's mptRoot.

`setCalculatedState` mirrors this: contiguous ordinal → local transition (+ breadcrumb
cross-check); ordinal jump → **seed** from the attested breadcrumb (verify the downloaded value
reproduces `mptRoot`, adopt `catalogRoot`).

### Hydration (seeded → live)

A breadcrumb-seeded node knows the catalog ROOT but not its contents. It can verify hashes and
serve state-dict (MPT) proofs immediately, but deriving the NEXT transition (combine/propose) and
serving catalog proofs require the contents: the hot epoch (≤ epochSize entries) and the level-1
roots (one per sealed epoch) — bounded, NOT the chain history. Hydration is **verify-gated**
(`CommittedState.hydrate` / `POST /committed/hydrate`): the supplied `CatalogContents` must
recompose to the attested root, so the payload can come from ANY peer
(`GET /committed/catalog`), an operator, or the node's own `CatalogJournal` (the restart path —
the journal is LevelDB-backed via metakit's `LevelDbCollection` and written through on every
transition/seal, so a restarted node re-hydrates locally and immediately).

> FLAGGED (tessellation change, not implemented here): tessellation's download/traverse offers no
> hook for an app to fetch auxiliary data from peers, so fetching `CatalogContents` over HTTP is
> wired by the application (ottochain follow-up) — e.g. a sidecar/daemon hitting a peer's
> `/committed/catalog` and POSTing `/committed/hydrate`. With a first-class "data application
> download hook" in tessellation this becomes automatic. Until hydrated, a freshly bootstrapped
> node can verify but not participate in consensus (combine raises
> `BreadcrumbUnresolvable`/`CatalogNotHydrated`, loudly).

## 4. Retention: serving, not trust

`CommittedConfig.sealedEpochRetention` (node-local; default 4) bounds how many sealed epochs'
full contents a node keeps. Pruning removes only the SERVING cache:

- the level-1 root of every sealed epoch is kept forever (32 bytes per ~2^16 ordinals), so the
  catalog root — and therefore every proof ever issued against any committed root — remains
  verifiable;
- a pruned node answers `proof-ordinal` for that epoch with `410 Gone`
  (`CommittedProofError.EpochPruned`); any node retaining the epoch can serve the proof, and it
  verifies against the same attested roots.

Retention bounds what a node can SERVE, never what the network can TRUST.

## 5. Routes (all reads from one cell value)

```
GET  /committed/root                   { ordinal, mptRoot, catalogRoot, calculatedStateHash, hydrated }
GET  /committed/proof/<key...>         single-key inclusion proof
POST /committed/proofs                 batch proof, body { "keys": [ "ns/a", ... ] }
GET  /committed/proof-prefix/<ns...>   complete prefix attestation for a namespace
GET  /committed/proof-ordinal/:ordinal OrdinalCatalogProof (410 Gone if pruned here)
GET  /committed/catalog                CatalogContents (hydration / replication source)
GET  /committed/delta/:ordinal         StateDelta (404 after ring-buffer eviction)
GET  /committed/snapshot               CommittedSnapshot (replication fallback)
POST /committed/hydrate                install CatalogContents on a seeded cell (verify-gated)
```

## 6. Why a hierarchy: super-registries and cross-metagraph slices

The namespace hierarchy is the unit of *delegated commitment*. A super-registry can commit
sub-registries as namespaces (`registry/<name>/...`), and later promote a sub-registry to its own
catalog entry without changing key shapes. Cross-metagraph: ML0_B asks ML0_A for
`/committed/proof-prefix/registry/tokens` and verifies the returned complete slice against ML0_A's
`mptRoot` — trust-anchored without trusting the serving node, via gl0's
`CurrencySnapshotMptRoots` → the snapshot's `calculatedStateProof` (= `calculatedStateHash`
above) → `mptRoot`. The same path serves JLVM contracts on ML0_B through `mpt_prefix_verify`.
And because the consensus hash now commits the FULL catalog, the same anchor also proves
*historical* facts: "ordinal M committed root X" (or provably did not) via a single
`OrdinalCatalogProof` against the latest signed snapshot.
