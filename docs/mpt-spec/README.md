# Merkle Patricia proof wire spec + reference verifiers

The wire formats served by the committed layer (`GET /committed/proof/<key>`) for proving a
single key's status against the consensus-signed `mptRoot`, plus reference verifier
implementations for external light clients.

## Wire formats

### Sealed proof (current): `{type, path, witness}`

A proof is EITHER inclusion OR absence, stated explicitly by a proof-level `type` tag
(Scala `MerklePatriciaProof`):

```json
{ "type": "Inclusion", "path": "<nibble hex>", "witness": [ <commitment>* ] }
{ "type": "Absence",   "path": "<nibble hex>", "witness": [ <commitment>* ] }
```

`witness` is ordered DEEPEST-FIRST (verifiers fold `witness.reverse` root-first). Each
commitment is `{ "type": "Leaf"|"Branch"|"Extension", "contents": {...} }`:

| Commitment  | `contents`                                    |
|-------------|-----------------------------------------------|
| `Leaf`      | `{ "remaining": "<nibble hex>", "dataDigest": "<sha256 hex>" }` |
| `Branch`    | `{ "pathsDigest": { "<nibble>": "<sha256 hex>", ... } }`        |
| `Extension` | `{ "shared": "<nibble hex>", "childDigest": "<sha256 hex>" }`   |

### Legacy proof: un-tagged `{path, witness}`

The pre-sealed inclusion-only shape. The `Inclusion` encoding is byte-identical to it plus
the `type` tag, so lenient legacy parsers keep working on tagged inclusion responses; a
verifier receiving an un-tagged proof treats it as inclusion.

## Digest discipline

Every commitment binds to its parent by `sha256(typePrefix ++ JCS(contents))`, where JCS is
RFC 8785 canonical JSON of the `contents` object ONLY (no `type` wrapper) and the prefix is
one byte: `Leaf = 0x00`, `Branch = 0x01`, `Extension = 0x02`. A `Leaf.dataDigest` is
`sha256(JCS(record))` — no prefix.

## Verification

Both arms run the SAME root-first fold: start at the trusted root digest with the full
`path` nibbles; at each commitment recompute the prefixed digest and require it to equal the
digest threaded so far; a `Branch` consumes one nibble via `pathsDigest`, an `Extension`
consumes its `shared` run (which MUST be a prefix of the remaining path); a `Leaf` may
appear only as the deepest commitment. The arms differ only in the terminal assertion.

**Inclusion terminal**: the fold ends at a `Leaf` whose `remaining` equals the un-consumed
path (and, if the caller knows the record, `dataDigest = sha256(JCS(record))`).

**Absence terminals**: the deepest commitment must hash to the digest the fold reached AND
structurally refuse the next step — exactly one of:

| # | Terminal    | Condition                                                                 |
|---|-------------|---------------------------------------------------------------------------|
| 1 | `Branch`    | `pathsDigest` lacks the next nibble of the remaining path                  |
| 2 | `Branch`    | the remaining path is EMPTY (this MPT has no branch value slot, so a path ending at a branch is necessarily absent) |
| 3 | `Extension` | `shared` is NOT a prefix of the remaining path (divergence mid-edge, incl. a remaining path shorter than the edge) |
| 4 | `Leaf`      | `remaining` differs from the remaining path (a different key occupies the position) |

A terminal that could continue — or a `Leaf` that MATCHES — proves nothing: reject. There
is deliberately no absence-reason tag on the wire: the terminal commitment's own `type`
plus the recomputed path suffix make the reason structurally unambiguous.

## Reference verifier support matrix

| Reference                | Inclusion | Absence (sealed) |
|--------------------------|-----------|------------------|
| [`js/`](js/) — **THE reference implementation** | yes | yes |
| [`go/`](go/)             | yes       | TODO (port from `js/`) |
| [`python/`](python/)     | yes       | TODO (port from `js/`) |
| [`rust/`](rust/)         | yes       | TODO (port from `js/`) |
| [`solidity/`](solidity/) | yes       | TODO (port from `js/`) |

Until the ports land, an inclusion-only reference handed an `Absence` proof reports it
INVALID — indistinguishable from a tampered proof. Absence-aware light clients must use the
js reference (or the Scala `MerklePatriciaVerifier` itself) as their porting source.

## Fixtures

- `test-sealed-proofs.json` — chain-derived KATs for the sealed format: one tagged
  `Inclusion` plus absence proofs for branch-missing-nibble, other-leaf,
  extension-divergence, and the empty trie. Emitted by the REAL Scala prover and
  byte-pinned by `MptSpecFixtureSuite`; regenerate only on an intentional format change via
  `sbt "Test/runMain crypto.mpt.MptSpecFixtureGenerator"`. The js harness (`cd js && npm
  test`) verifies every case.
- `test-proof.json` — the legacy un-tagged inclusion fixture, kept untouched for
  inclusion-only consumers (also exercised by the js harness).

## Route migration note: `GET /committed/proof/<key>`

The endpoint changed observably when absence proofs shipped:

- **Missing keys: `200` + tagged `Absence`** (previously `404`). Consumers that detected
  absence via the 404 status MUST switch to dispatching on the proof-level `type` tag —
  absence is now an attestable, verifiable fact rather than an error.
- **Present keys: the response carries an added `"type": "Inclusion"` field** on top of the
  legacy `{path, witness}` shape. Lenient parsers are unaffected; strict
  deny-unknown-field decoders must add `type`.

This break rides the existing JAR↔SDK version lock; there is no content negotiation for the
legacy shape.
