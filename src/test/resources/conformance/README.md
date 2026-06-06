# JSON Logic cross-language conformance vectors

`json_logic_test_vectors.json` is synced from
`metakit-sdk/shared/json_logic_test_vectors.json` — the cross-language source of
truth for the base JSON Logic VM. Keep it in sync.

The same vectors are executed by:

- Rust: `metakit-sdk/rust/jlvm-core/tests/differential.rs`
- TypeScript: `metakit-sdk/packages/typescript/tests/json-logic-vectors.test.ts`
- Scala (this repo): `src/test/scala/json_logic/SharedVectorConformanceSuite.scala`

All three implementations must agree on every case. When updating semantics or
adding vectors, update the shared file in `metakit-sdk` first, then re-copy it
here so the three suites stay aligned.

Format:

```json
{
  "description": "...",
  "version": "1.0.0",
  "tests": [
    { "category": "...", "cases": [ { "expr": "...", "data": "...", "expected": "..." } ] }
  ]
}
```

`expr`, `data`, and `expected` are JSON-encoded strings (the harness parses them).
The `expected` strings use a specific textual JSON form (spaces after `,` and `:`,
e.g. `[1, 2, 3]` and `{"a": 1, "b": 2}`); the Scala suite renders results in that
same form and additionally performs a structural (numbers-by-value) comparison to
match the Rust/TypeScript oracles.
