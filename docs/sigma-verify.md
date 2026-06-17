# RFC: `sigma_verify` — recursive Σ-protocol proposition verifier

- **Status:** implemented — Phase 0–2 shipped, registered **live** on the default JLVM dispatch (no
  feature flag, no opt-in). **Not yet externally audited** (see §0).
- **Scope:** metakit JLVM (`json_logic`) crypto opcodes
- **Curve:** BN254 (alt_bn128) G1 — same as `schnorr_verify` / `groth16_verify`. We do **not** add
  secp256k1.
- **Depends on (shipped, Phase 0–1):**
  - `schnorr_verify` / `prove_dlog_verify` — the standalone DLog Σ-leaf.
  - `prove_dhtuple_verify` — the standalone DDH / Diffie–Hellman-tuple Σ-leaf.
  - `CryptoOps.dlogComputeCommitment` / `CryptoOps.dhtupleComputeCommitment` — the bottom-up
    commitment-recovery helpers this verifier reuses.
- **Crown-jewel correctness target:** the **strong Fiat-Shamir transcript binding** plus the CDS
  challenge-splitting. An external audit of the FS + CDS surface is **strongly recommended** before
  this opcode guards high-value flows — but it is *not* enforced as a runtime gate (see §0).
- **Maturity:** the whole Σ-protocol opcode family (`prove_dlog_verify`, `prove_dhtuple_verify`,
  `sigma_verify`) is **registered and live** on the default JLVM dispatch — no feature flag, no
  opt-in. It is **not yet externally audited** (see §0).

---

## §0. Status & maturity — live, not yet externally audited

The three Σ-protocol opcodes (`prove_dlog_verify`, `prove_dhtuple_verify`, `sigma_verify`) are
**implemented and registered live** on the default JLVM dispatch in all three implementations
(metakit Scala reference + metakit-sdk Rust + metakit-sdk TypeScript). There is **no feature flag
and no opt-in**: a call dispatches
straight to the verifier in production, in tests, and in the conformance / differential harness
alike. Malformed input is a hard error and a cryptographically-invalid proof returns `false` (the
error-vs-`false` discipline of §5) — the opcodes never silently pass.

**They are not yet covered by an external cryptographic audit.** The FS + CDS surface is exactly the
class that broke on Solana (§6), so an external audit of the strong-Fiat-Shamir transcript and the
CDS challenge-splitting is **strongly recommended before these opcodes guard high-value flows** (mint
policies, asset morphisms). That recommendation is a *deployment / integration* decision, **not** a
runtime gate baked into the VM: the cross-language conformance vectors (§4) are a **necessary but not
sufficient** check, and integrators are responsible for tracking the audit. (An earlier
OFF-by-default runtime flag was removed in favour of this honest maturity note, so the opcode behaves
identically everywhere it runs.)

## 1. Motivation

OttoChain consumes JLVM crypto opcodes as **morphism / mintPolicy guards** via the shipped
`ZkVerify`-morphism pattern: an asset morphism or a mint policy is admissible iff a JSON-Logic
expression over verified facts returns `true`. The Phase 0–1 opcodes already give two *atomic*
Σ-leaves over BN254 G1:

- **DLog** (`prove_dlog_verify` ≡ `schnorr_verify`): knowledge of `x` such that `pk = x·G`.
- **DHTuple** (`prove_dhtuple_verify`): knowledge of `w` such that `u = g^w ∧ v = h^w`.

These are sufficient for **single-key** guards ("this one key signed", "this is a valid DH tuple").
They are **not** sufficient for the policies real applications want:

- *m-of-n* multisig / threshold authorization without revealing **which** signers participated;
- "key A **or** key B may authorize" without revealing **which** one did (ring-style hiding);
- mixed propositions: "`(A and B)` or `(threshold 2 of {C, D, E})`".

This is the classical, no-trusted-setup **Σ-protocol family** as used by Ergo (ErgoTree `SigmaProp`)
and EIP-11: a *proposition tree* whose internal nodes are the connectives `CAND` / `COR` /
`CTHRESHOLD(k)` and whose leaves are DLog / DHTuple statements, proven non-interactively and verified
in one shot. `sigma_verify(proposition, proof, message)` is the recursive verifier for that tree.

### Why not just compose the atomic leaves with JLVM `or` / `some`?

This is the single most important thing in this document, so it leads.

`{"or": [ {"prove_dlog_verify": [...A...]}, {"prove_dlog_verify": [...B...]} ]}` is
**cryptographically UNSOUND as an OR proof.** Each standalone leaf opcode derives its **own**
independent Fiat-Shamir challenge from its **own** transcript and checks its **own** equation in
isolation. For the JLVM `or` to return `true`, the prover must supply **at least one fully valid
standalone proof** — i.e. the prover must *actually know a witness for that disjunct*. There is no
**hiding**: the verifier (and any observer of the public proof bytes) learns exactly which disjunct
was satisfied, because the unsatisfied disjunct simply carries an invalid (or absent) proof.

A real Σ-OR must hide which branch the prover knows. The standard construction (Cramer–Damgård–
Schoenmakers, "CDS") does this by **challenge-splitting**: the *root* challenge is fixed by hashing
the whole tree, and the children's challenges must XOR (for OR) / sum-in-`GF(2^8)` (for threshold) to
their parent's challenge. The prover simulates the branches it does **not** know (picking their
challenges freely and back-solving a valid-looking transcript) and only does real work on the branch
it **does** know — but the simulation is indistinguishable from a real proof, so hiding holds. The
JLVM `or` has no challenge to split and therefore cannot express this. The same argument rules out
`{"some": [...leaves..., k]}` as a threshold proof.

**Rule:** OR / threshold over Σ-statements must go through `sigma_verify` (one challenge, split by
the tree structure). The atomic `prove_dlog_verify` / `prove_dhtuple_verify` opcodes are only ever
sound as *single-leaf* guards, or as conjuncts the verifier separately requires (an *AND* of
independent statements is fine to express as JLVM `and`, because AND needs no hiding — but
`sigma_verify`'s `CAND` is still preferred so the whole tree shares one transcript and one gas
charge). This rule is also stated inline in `CryptoOps.scala` (the SIGMA PROTOCOLS note) and on the
`ProveDlogVerifyOp` / `ProveDhTupleVerifyOp` registry entries.

---

## 2. The verifier algorithm (Ergo "Verifier Steps 1–6")

`sigma_verify(proposition, proof, message)` parses a proposition tree and a matching proof tree,
then runs the non-interactive CDS verifier. The algorithm is the Ergo `SigSerializer` /
`verifySignature` flow, restated for BN254 G1:

1. **Parse** the proposition tree (connectives + leaves) and the proof tree (the per-node challenges
   and per-leaf responses; commitments are **not** carried in the proof — they are reconstructed in
   step 4). Reject structurally malformed input as a **hard error** (see §5, error-vs-false).
2. **Compute the root challenge** that the *whole* proof must be consistent with. In the
   non-interactive setting this is **not** read from the proof — it is **recomputed** at the end
   (step 6) and compared. During top-down propagation we use the challenges carried in the proof.
3. **Top-down challenge propagation.** Starting from the root challenge, push challenges down:
   - **`CAND`** (`AND`): every child gets a **copy** of the parent's challenge `e`.
   - **`COR`** (`OR`): the children's challenges must satisfy `⊕ eᵢ = e_parent` (XOR over the
     challenge bytes). The proof supplies all but the "real" child's challenge directly; the real
     child's challenge is `e_parent ⊕ (⊕ of the simulated children)`. (At *verify* time we are given
     all challenges and simply check the XOR relation.)
   - **`CTHRESHOLD(k)`** over `n` children: challenges are the evaluations of a degree-`(n−k)`
     polynomial over `GF(2^8)` whose constant term is the parent challenge; the proof carries the
     polynomial (equivalently the `n−k` simulated challenges), and each child's challenge is the
     polynomial evaluated at that child's index. Verify the polynomial interpolates the parent
     challenge at `0`.
4. **Bottom-up leaf commitment reconstruction.** For each leaf, given its propagated challenge `e`
   and the response(s) `z` from the proof, **recompute** the commitment from the verification
   equation (do **not** trust a commitment supplied in the proof — recompute it):
   - **DLog leaf** `pk`: `a = z·G − e·pk` via `CryptoOps.dlogComputeCommitment(pk, e, z)`.
   - **DHTuple leaf** `(g, h, u, v)`: `a1 = z·g − e·u` and `a2 = z·h − e·v` via two calls to
     `CryptoOps.dhtupleComputeCommitment(g, u, e, z)` and `(h, v, e, z)`.

   Reconstructing (rather than trusting) the commitment is what makes the *final hash* (step 6) bind
   the responses: a forged `z` produces a different `a`, which changes the recomputed root challenge.
5. **Re-serialize the whole tree.** Walk the tree in a **canonical, deterministic** order and
   serialize: the proposition structure (connective tags, threshold `k`, leaf statement points
   `pk` / `(g,h,u,v)` at fixed 64-byte width) **and** the reconstructed leaf commitments (`a` /
   `a1,a2`). The exact byte layout is normative and frozen (see §4) — any divergence between prover
   and verifier serialization is an `InvalidSignature`-class break.
6. **Hash → compare to the root challenge.** Compute
   `e_root* = low31( SHA256( DomainSep ‖ serialized_tree ‖ message ) )` and accept iff `e_root*`
   equals (byte-for-byte) the root challenge that was propagated in step 3. `DomainSep =
   ascii("sigma_verify:v1")` separates this hash family from the per-leaf transcripts; `low31` is
   the low-order 31 bytes of the digest (the injective challenge domain, see §4). The comparison is
   over the 31 challenge bytes — **no mod-R reduction on either side** (the 31-byte challenge is
   already a canonical Fr element). Mismatch ⇒ `false` (well-formed but cryptographically invalid).

This is **strong Fiat-Shamir**: the hash in step 6 binds the **entire** statement (every leaf's
points, the tree shape, the threshold parameters) **and** every reconstructed commitment **and** the
message. See §6.

---

## 3. `JsonLogicValue` encoding of proposition + proof

`sigma_verify` takes three arguments, all in the existing JLVM value space (no new value type, same
as every other crypto opcode):

```
{"sigma_verify": [ <proposition>, <proof>, <messageHex> ]}
```

- **`<messageHex>`** — a `0x`-prefixed lowercase hex byte string (the bound message), exactly like
  the `msg` argument of `schnorr_verify` / `prove_dhtuple_verify`.

- **`<proposition>`** — a `MapValue` tree. Schema (one map per node, discriminated by `"type"`):

  ```jsonc
  // leaves
  {"type": "dlog",    "pk": "0x..(64B)"}
  {"type": "dhtuple", "g": "0x..(64B)", "h": "0x..", "u": "0x..", "v": "0x..(64B)"}
  // connectives
  {"type": "and", "children": [ <node>, ... ]}
  {"type": "or",  "children": [ <node>, ... ]}
  {"type": "threshold", "k": <int>, "children": [ <node>, ... ]}
  ```

- **`<proof>`** — a `MapValue` tree **mirroring** the proposition's shape, carrying the per-node
  challenge and per-leaf response(s):

  ```jsonc
  // every node carries its propagated challenge (31B — the injective challenge domain, §4a):
  {"e": "0x..(31B)", ...}
  // a leaf additionally carries its response(s) (z is a canonical 32B scalar, < R):
  {"type": "dlog",    "e": "0x..(31B)", "z": "0x..(32B)"}
  {"type": "dhtuple", "e": "0x..(31B)", "z": "0x..(32B)"}
  // a connective additionally carries its children's proofs:
  {"type": "or", "e": "0x..(31B)", "children": [ <proofNode>, ... ]}
  {"type": "threshold", "e": "0x..(31B)", "k": <int>, "children": [ <proofNode>, ... ]}
  ```

  Commitments are **not** carried in the proof — they are reconstructed (§2 step 4). This both
  shrinks the proof and removes a forgery surface (a verifier that trusts a supplied commitment and
  *also* hashes it can be made to accept inconsistent `(a, z)` pairs).

The proposition and proof are **two** parallel trees (clean separation: the proposition is the
on-chain commitment and the proof is the witness), as implemented. This matches Ergo's `SigmaProp`
(proposition) vs `ProverResult` (proof) split; the message-binding and gas model below are written
for it.

---

## 4. Canonical serialization (normative, frozen at implementation)

Determinism is mandatory (this runs inside consensus; see §7). The tree serialization for step 5
MUST be byte-exact across every implementation. Rules (frozen, mirroring the `HexBytes` fixed-width
discipline and the existing `mpt-spec` canonical-JSON rule):

- **Traversal order:** pre-order, children in their **array order** (array order is significant and
  is part of the statement — reordering children changes the proposition).
- **Node tag:** a single fixed byte per connective / leaf kind (`dlog`, `dhtuple`, `and`, `or`,
  `threshold`).
- **Threshold `k`:** fixed-width big-endian encoding.
- **Points:** every G1 point (`pk`, `g`, `h`, `u`, `v`, and the reconstructed `a` / `a1` / `a2`) is
  the canonical 64-byte big-endian `x‖y` form produced by `HexBytes.encodeG1` — the same fixed-width
  encoding the leaf opcodes already use. No compression, no variable width.
- **Challenges:** **31-byte (248-bit) big-endian — the INJECTIVE-into-Fr challenge domain (§4a).**
  Because `2^248 < R` (BN254 `R ≈ 2^253.6`), every 31-byte value is a distinct, canonical Fr element,
  so the byte↔scalar map is a *bijection* — there is no `e` vs `e+R` aliasing. The SAME 31 bytes are
  used both as the CDS / GF(2^8) object (XOR / Shamir, closed in GF(2)^248) **and**, taken directly
  with **no mod-R reduction**, as the Fr scalar in the leaf reconstruction (`z·G − e·pk`). Note:
  challenges are **not** part of the serialized transcript (only the statement points + reconstructed
  commitments are); they live in the proof tree and are checked against the recomputed root.
- **Responses (`z`):** 32-byte big-endian, **canonical** (`< R`) — a response `>= R` is a hard error
  (the canonical-response rule; `z` and `z+R` are congruent mod R, so this removes proof-byte malleability).

### 4a. Challenge domain — the injective byte↔scalar map (normative)

The challenge byte width is **31 bytes, not 32** (the auditor-suggested "smaller challenge byte
width"). This kills a malleability / weakened-CDS-soundness hazard that 32-byte challenges carried:
a 32-byte challenge was used **raw** for the OR-XOR / GF(2^8) split but **reduced mod R** for the
leaf scalar arithmetic, so two distinct raw challenges `e` and `e+R` (both `< 2^256`, since `R <
2^256`) collapsed to the **same** scalar — distinct transcripts, identical algebraic check.

The fix makes the challenge↔scalar map a bijection:

- `CHALLENGE_BYTES = 31`. Every challenge — the root challenge AND every per-leaf / per-node
  challenge in the proof AND every challenge **derived** in the CDS split — is a 31-byte value.
- Challenges are derived from SHA-256 by a **single canonical rule**: the **low-order 31 bytes** of
  the digest (`low31(d) = d[1..32]`, i.e. drop the most-significant byte). The root challenge is
  `e_root = low31( SHA256( DomainSep ‖ serializedTree ‖ message ) )`.
- **CDS XOR (OR)** and **GF(2^8) Shamir (THRESHOLD)** operate on the 31-byte challenges — closed in
  `GF(2)^248` (31 independent byte-lanes for the threshold interpolation, was 32).
- **Scalar use:** the 31-byte challenge is converted **directly** to an Fr element (`BigInt(1, e)`).
  Because `e < 2^248 < R` this needs **no mod-R reduction** — the byte↔scalar map is the bijection,
  so the alias is gone by construction (a challenge `≥ 2^248` is impossible: it would not fit in 31
  bytes, and `e+R ≥ R > 2^248` can never be a 31-byte challenge).
- **Encoding deltas:** per-leaf / per-node challenges are now **31 bytes** (were 32). Responses `z`
  stay canonical 32-byte (`< R`) scalars; commitments stay 64-byte G1. The serialized **transcript** (tags,
  arities, `k`, statement points, reconstructed commitments, message) is **unchanged** — challenges
  were never in it — so the serialization KATs are stable in layout (only the reconstructed
  commitment bytes move, because they depend on the new challenge values).

This serialization is the part most likely to diverge silently between the metakit verifier and the
metakit-sdk prover, so it gets its own conformance vectors (cf. `docs/mpt-spec`, `docs/sig-spec`):
the `sigma_dlog` / `sigma_dhtuple` / `sigma` categories of the shared `zk_opcode_test_vectors.json`
(checked into metakit `src/test/resources/conformance/` and mirrored into metakit-sdk `shared/`) — a
fixed set of `(proposition, proof, message) → bool` cases plus `error` cases. The `sigma` category
**is** the frozen serialization byte-contract: the Rust and TypeScript ports must reproduce every
expected value, and reject every `error` case, identically.

---

## 5. Error-vs-false discipline

Identical discipline to `groth16_verify` / `schnorr_verify` / `prove_dhtuple_verify`:

- **Hard error (`JsonLogicException`)** — *malformed*: bad hex, wrong width, a point off the BN254
  curve, a structurally invalid tree (unknown `"type"`, `threshold` with `k > n` or `k ≤ 0`,
  proposition/proof shape mismatch, missing required field). These are encoding faults, not failed
  proofs.
- **`false`** — *well-formed but cryptographically invalid*: the recomputed root challenge does not
  match, an OR's challenges do not XOR to the parent, a threshold polynomial does not interpolate, a
  leaf with an identity statement point (same forgery vectors the leaf opcodes already reject as
  `false`).

This keeps the opcode's failure mode aligned with the rest of the crypto-opcode family: a block does
not get *poisoned* by a malformed proof being mistaken for a consensus-relevant error.

---

## 6. Strong Fiat-Shamir — the crown-jewel correctness target

The non-interactive security of every Σ-protocol rests entirely on the Fiat-Shamir hash binding
**everything the prover could otherwise adaptively choose**. The classic failure class is a **weak**
Fiat-Shamir transform that omits some algebraic component from the transcript, letting an attacker
fix that component **after** seeing the challenge and forge a proof.

This is not theoretical. Two reference incidents that this design is explicitly written to avoid:

- **Solana / SPL ZK-ElGamal Proof program (2025).** The Fiat-Shamir transcript for the confidential-
  transfer proofs **omitted algebraic components** from the challenge hash. The concrete consequence
  was a **forgeable sigma OR proof**, which bypassed fee validation and allowed forging proofs the
  program accepted as valid (arbitrary mint / withdraw of confidential balances). Disclosed
  2025-04-16 (PoC by *LonelySloth*); a second related report on 2025-06-10 led to the ZK ElGamal
  Proof program being **disabled via feature activation on mainnet** at epoch 805 (2025-06-19). The
  root cause is exactly a weak transcript on a **sigma OR** — the construction this RFC adds.
  (Solana post-mortems, May 2 and June 25 2025; zkSecurity "Phantom Challenge" writeup.)
- **Trail of Bits, "Weak Fiat-Shamir Attacks on Modern Proof Systems."** Systematizes the class:
  proof systems whose Fiat-Shamir hash fails to bind the full statement / public inputs / all
  commitments are forgeable, and the paper demonstrates practical forgeries against several deployed
  systems. The takeaway adopted here: **bind the full statement, all commitments, and the message**,
  every time.

How this RFC bakes in **strong** Fiat-Shamir:

1. **The leaves already do it.** `prove_dhtuple_verify` computes
   `e = SHA256(g ‖ h ‖ u ‖ v ‖ a1 ‖ a2 ‖ msg) mod R` — the full statement *and* both commitments
   *and* the message. `schnorr_verify` / `prove_dlog_verify` bind `R ‖ pk ‖ msg`. The
   `SigmaOpsSuite` proves the binding by negative tests: changing `g`, `u`, `v`, or `msg` flips a
   valid proof to `false`.
2. **The tree hash (step 6) binds the whole tree.** The root challenge is `low31( SHA256(DomainSep ‖
   canonical_tree ‖ message) )` where `canonical_tree` includes **every** leaf's statement points,
   **every** reconstructed commitment, the connective structure, and the threshold parameters.
   Nothing the prover controls is left out of the transcript. The `low31` reduction (§4a) makes the
   root challenge a canonical Fr element with no `e` vs `e+R` aliasing.
3. **Commitments are reconstructed, not trusted** (§2 step 4), so a tampered response necessarily
   changes the hashed commitment and breaks step 6.

**Audit recommendation:** because this is precisely the surface that broke on Solana, the FS + CDS
implementation **should pass an external cryptographic audit** before `sigma_verify` guards
high-value flows (mint policies, asset morphisms). This is a deployment-time recommendation, **not** a
runtime gate — the opcode is registered live everywhere it runs (see §0) — and the conformance
vectors (§4) are a *necessary but not sufficient* check.

---

## 7. Determinism

`sigma_verify` runs inside the metagraph combiner / consensus, so it MUST be a pure deterministic
function of its arguments:

- BN254 G1 arithmetic via `Bn254.G1` (Besu pure-Java, already deterministic and used by every other
  curve opcode).
- `SHA256` for the transcript (deterministic).
- **No randomness in the verifier.** All simulation randomness lives in the *prover* (metakit-sdk,
  off-chain); the verifier only checks relations. The canonical serialization (§4) removes the one
  remaining nondeterminism risk (map/JSON ordering) by fixing a pre-order, array-order,
  fixed-width byte layout rather than hashing a JSON rendering.

---

## 8. Gas model

Consistent with the existing crypto-opcode schedule (`GasMetering.scala`), priced as **per-leaf +
per-node**, pre-charged from the **proposition-tree shape** in the gas-aware layer **before** any curve
arithmetic runs (so out-of-gas is raised before the work):

- **Per-DLog-leaf:** ≈ `proveDlogVerify` (one `dlogComputeCommitment` = 2 muls + 1 add).
- **Per-DHTuple-leaf:** ≈ `proveDhtupleVerify` (two `dhtupleComputeCommitment` = 4 muls + 2 adds).
- **Per-connective node:** a small flat charge (`AND` = challenge copy; `OR` = XOR fold over the
  children; `THRESHOLD` = `GF(2^8)` polynomial interpolation — the threshold node also carries a
  per-child term for the interpolation).
- **One root SHA256** over the serialized tree, charged proportionally to the serialized length
  (which is bounded by the leaf + node counts already charged).

The tree size (#leaves, #nodes, threshold `k`/`n`) is fully determined by the proposition + proof
shape, so — exactly like `bn254_pairing`'s per-pair and `bls_aggregate_verify`'s per-key charges —
the gas-aware layer derives the per-element counts from the (already-evaluated) argument values and
pre-charges them in `getInputScaledCost` before dispatching to the verifier. New `GasConfig` fields:
`sigmaVerify` (base), `sigmaVerifyPerDlogLeaf`, `sigmaVerifyPerDhtupleLeaf`, `sigmaVerifyPerNode`
(and the estimator `baseCost` mapping). This is the DoS bound for the opcode. In addition:

- **Proposition bound (before parse).** The proposition's raw node count and depth are bounded by a
  cheap early-aborting walk with the hard caps **4096 nodes / 64 depth** (`SigmaMaxProofNodes` /
  `SigmaMaxProofDepth`) BEFORE the recursive `parsePropNode` runs, and the gas estimator's
  proposition-shape walk is depth-capped by the same value — so a deeply nested / very wide
  proposition cannot drive unbounded stack/CPU work in either the parser or the pre-charge.
- **Proof bound (before parse).** The proof tree is structurally bounded BEFORE the recursive proof
  parse (hex decode / curve work): its node count and depth must not exceed the proposition's, with
  the same hard backstop caps. A tiny proposition + huge mismatched proof is rejected fast (a hard
  error), having done only a bounded raw-tree walk.
- **Message cap.** The message is capped at **4096 bytes** (`SigmaMaxMessageBytes`, shared by
  `sigma_verify` and `prove_dhtuple_verify`) so it cannot force unbounded hex-decode / SHA-256 work
  outside the Sigma-tree pricing.
- **Canonical node encoding.** Every proposition / proof node rejects fields outside its schema
  (`type`, plus `pk` | `g,h,u,v` | `children` | `k` | `e,z` as applicable). This keeps the raw
  encoding canonical (no ignored bytes for logs / caches / external signing layers) and removes the
  bound-inflation surface (a leaf can no longer carry an ignored `children` field).

---

## 9. Off-chain prover

The verifier is **only** the on-chain `sigma_verify` opcode. The **prover** — keygen, witness
selection, branch simulation (CDS), polynomial construction for threshold, transcript serialization,
proof assembly — lives in **metakit-sdk** (off-chain), the same place the Schnorr / DH-tuple provers
used by `SigmaOpsSuite` would graduate to. The on-chain side never simulates and never holds a
witness; it checks relations only.

---

## 10. Relationship to OttoChain

OttoChain consumes `sigma_verify` exactly like the other verify-opcodes: as a JSON-Logic guard
inside a **morphism** or **mintPolicy**, via the shipped `ZkVerify`-morphism pattern. A mint policy
"any 2 of these 3 issuer keys, without revealing which" becomes a `sigma_verify` over a
`threshold(2, [dlog(A), dlog(B), dlog(C)])` proposition; an asset-compose authorization "key A or key
B" becomes `or([dlog(A), dlog(B)])`. Because the leaves and the helpers are already shipped (Phase
0–1) and re-used unchanged, Phase 2 is purely the recursive CDS layer + canonical serialization +
gas wiring — all shipped. A follow-up external audit of the FS + CDS surface is recommended before
high-value use (§0).

---

## 11. Phasing

- **Phase 0 (DONE):** `prove_dlog_verify` first-class alias; `dlogComputeCommitment` extracted +
  unit-tested.
- **Phase 1 (DONE):** `prove_dhtuple_verify` standalone leaf with strong-FS transcript;
  `dhtupleComputeCommitment` extracted + unit-tested; full `SigmaOpsSuite`.
- **Phase 2 (DONE):** `sigma_verify` recursive CDS verifier — proposition/proof encoding, challenge
  propagation (AND/OR/THRESHOLD), commitment reconstruction, canonical serialization, gas,
  cross-language conformance vectors. Registered live (no runtime gate). **An external audit of the
  FS + CDS surface is recommended before it guards high-value flows (§0).**

---

## References

- Cramer, Damgård, Schoenmakers — *Proofs of Partial Knowledge and Simplified Design of Witness
  Hiding Protocols* (CRYPTO '94) — the CDS OR / threshold construction.
- Ergo Platform — ErgoTree / `SigmaProp`, `SigSerializer` / `verifySignature` ("Verifier Steps 1–6")
  and the AND/OR/THRESHOLD (`GF(2^8)`) challenge-splitting; EIP-11 multisig.
- Solana / SPL ZK-ElGamal Proof program weak-Fiat-Shamir post-mortems (forgeable sigma-OR), May 2
  and June 25 2025; zkSecurity "Phantom Challenge Soundness Bug" writeup.
- Trail of Bits — *Weak Fiat-Shamir Attacks on Modern Proof Systems.*
