# Content-Hash Rule (normative)

Every hashing or signing surface that operates on **typed content** — data
updates, on-chain / calculated state, MPT leaf values, anything whose digest
or signature another party will recompute from a decoded value — MUST derive
its bytes as:

```
bytes = utf8( RFC8785( dropNulls( encode(content) ) ) )
```

where `dropNulls` is `JsonBinaryCodec.dropNulls`:

1. **Recursively remove null-valued OBJECT fields.** `{"a": 1, "b": null}`
   hashes as `{"a": 1}` — at every nesting level.
2. **PRESERVE nulls inside arrays.** `[1, null, 3]` keeps its null; removing
   it would shift indices and change the meaning of positional data.
3. **Then canonicalize** with the RFC 8785 canonicalizer
   (`JsonCanonicalizer`) and take the UTF-8 bytes.

In metakit this is applied internally by both `JsonBinaryCodec` derivations
(`derive` and `deriveDataUpdate`), so every surface routed through
`JsonBinaryCodec.serialize`, `JsonBinaryHasher.computeDigest`, or
`SignatureProtocol.proveSigned`/`verifySigned` is covered automatically —
including the MPT/SMT node digests and the `lifecycle/committed` state-dict
leaves built on top of `JsonBinaryHasher`.

## Why drop nulls?

**Schema evolution.** In circe, `Option[A] = None` encodes as an explicit
`null` field, while a sender that never knew the field omits it entirely.
Dropping null object fields makes `None ≡ absent`:

- Adding an `Option` field to a schema does **not** change the hash of any
  previously committed value (old data decodes with `None`, re-encodes with
  `null`, and the null is dropped — bytes identical to before the field
  existed).
- A client that omits optional fields and a node that re-serializes the
  decoded value with explicit nulls agree on the bytes, so signatures verify.

## The layer distinction (value vs. typed content)

There are TWO canonicalization layers in the stack. Do not conflate them:

| Layer | Role | Null object fields |
|---|---|---|
| **Value canonicalizer** (JLVM: Rust `canonical.rs`, TS evaluator output, Scala `JsonCanonicalizer` / `CanonicalJson.from`) | Deterministic rendering of a *JSON Logic value* — null is a real, first-class value the program may have computed | **preserved** |
| **Typed-content hash** (`JsonBinaryCodec` / `JsonBinaryHasher` and every signing surface) | Bytes whose digest/signature is recomputed from a *decoded typed value* | **dropped** (this document) |

`JsonCanonicalizer` and `CanonicalJson.from`/`fromJson` are therefore raw,
null-preserving primitives. Never feed their output directly into a hash or
signature of typed content — go through the codec.

## Cautionary tale: the 2026-06-10 e2e incident

After the metakit `1.7.0-rc.9 → 1.8.0-rc.1` bump, **every ottochain e2e
scenario failed at step 1** with HTTP 400 from the DL1 `/data` endpoint.

Root cause: metakit 1.8 restored drop-nulls-before-canonicalize in
`JsonBinaryCodec.serialize` — the bytes `DataApplicationRoutes` hashes to
verify signature proofs. The e2e client (and external SDK consumers) signed
over raw RFC 8785 canonical JSON that still contained explicit nulls
(`"parentFiberId": null`, `"initialState": null`, `"metadata"`/`"effect":
null` inside fixture definitions) — a shape deliberately aligned to metakit
1.7's no-drop canonical form. The node dropped the nulls, the client did
not, the hashes diverged, and every signature verification failed
(`InvalidSignature → BadRequest`).

The lesson: the drop-nulls rule is only safe if it is applied **universally
and internally** — by every signer, hasher, and verifier, on every surface,
in every language — never as an opt-in pre-processing step the caller has to
remember.

## Conformance checklist for a new hash/sign surface

- [ ] Bytes come from `JsonBinaryCodec.serialize` (or an equivalent that
      applies `dropNulls` before RFC 8785) — not from `JsonCanonicalizer` /
      `CanonicalJson.from` directly.
- [ ] `hash(value with explicit nulls) == hash(value with those fields
      absent)` is pinned by a test.
- [ ] Array nulls are preserved (pinned by a test).
- [ ] Cross-language consumers (TS `@ottochain/sdk` `dropNulls`, Rust
      `jlvm-core` content hash) produce byte-identical results for a shared
      fixture.
