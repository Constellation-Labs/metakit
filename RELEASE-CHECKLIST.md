# Release checklist — next release off `dev`

> Prep document only. Nothing in this file has been executed; no tags have
> been created or pushed.

## Recommended version: **v1.8.0**

- `build.sbt` declares `versionScheme := "early-semver"`; the last released
  line is 1.7.x (`v1.7.0`, preceded by `v1.7.0-rc.1`–`rc.9`).
- Scope on `dev` since `v1.7.0` is feature-dominated: ZK opcode waves 1–3
  (poseidon, groth16, ecvrf, bn254, bls, schnorr, smt/mpt auth-DB verify),
  SMT/MPT provers, gas metering contract, RFC-9381-line ECVRF, plus the
  exact-rational numeric unification. New public API, no removals → **minor
  bump**, not patch; not a major.
- The shared cross-language conformance vectors vendored on `dev` are already
  versioned `v1.8.0`, matching this recommendation.
- The numeric-model unification (exact-rational arithmetic, #40/7b2df41)
  changes evaluation *results* for some programs relative to v1.7.0. It is a
  conformance fix toward the cross-language reference, but call it out
  prominently in the notes (done below). If the team treats it as breaking,
  the fallback is `v2.0.0`; the recommendation stays v1.8.0 because the old
  behavior diverged from the documented JSON Logic contract.
- Given the v1.7.0 history (9 RCs), consider cutting `v1.8.0-rc.1` first and
  promoting after ottochain integration passes.

## How publishing works (current config)

- `.github/workflows/release.yml` triggers on **any** tag push (`tags: ["*"]`)
  and runs `sbt ci-release` (sbt-ci-release 1.9.0, sonatype host
  `central.sonatype.com`). The version is derived from the git tag by
  sbt-dynver (`vX.Y.Z` → `X.Y.Z`). Secrets required: `PGP_PASSPHRASE`,
  `PGP_SECRET`, `SONATYPE_USERNAME`, `SONATYPE_PASSWORD` (already configured
  upstream).
- Precedent (v1.7.0): `main` is aligned with `dev` first, then the tag is cut
  on `main` (`64a7ab9 chore: align main with dev for v1.7.0 release`).

## Exact commands (when ready — DO NOT run as part of this PR)

```bash
# 0. preconditions: this PR (chore/bls-jar-provenance) merged; CI green on dev
git fetch upstream

# 1. align main with dev (mirrors the v1.7.0 flow)
git checkout main && git reset --hard upstream/main
git merge --no-ff upstream/dev -m "chore: align main with dev for v1.8.0 release"
git push upstream main

# 2. tag and push — the tag push alone triggers .github/workflows/release.yml
git tag -a v1.8.0 -m "metakit v1.8.0"
git push upstream v1.8.0

# (RC variant: git tag -a v1.8.0-rc.1 -m "metakit v1.8.0-rc.1" && git push upstream v1.8.0-rc.1)

# 3. watch the publish
gh run watch --repo Constellation-Labs/metakit

# 4. GitHub release with the notes below
gh release create v1.8.0 --repo Constellation-Labs/metakit \
  --title "metakit v1.8.0" --notes-file <notes>
```

## Required disclosure: vendored BouncyCastle 1.85 beta jars

Include this paragraph (or equivalent) in the release notes:

> **BouncyCastle 1.85 beta (vendored).** This release ships two unmanaged
> BouncyCastle jars in `lib/` (`bcprov-jdk18on-1.85-SNAPSHOT.jar`,
> `bcpkix-jdk18on-1.85-SNAPSHOT.jar`, OSGi build `1.85.0.20603`, JCE-signed by
> Legion of the Bouncy Castle Inc., snapshot signed 2026-05-31, obtained from
> BC's official beta channel `downloads.bouncycastle.org/betas/`). They are
> required because the BLS12-381 API (`org.bouncycastle.crypto.bls.*`) behind
> metakit's eth2-ciphersuite BLS primitive exists only in the unreleased 1.85
> line. The exact bytes are sha256-pinned in `lib/README.md` and enforced in
> CI. The transitive BC 1.70 artifacts from tessellation-sdk are excluded in
> `build.sbt`, so consumers get the 1.85 classes on the classpath. These jars
> will be replaced with the managed `org.bouncycastle:*:1.85` Maven Central
> artifacts the moment BC publishes a stable 1.85; see `lib/README.md` for the
> migration delta and the deferred source-verification checklist.

Release-blocking sub-items (tracked in `lib/README.md`):

- [ ] Either BC 1.85 stable is out (swap to managed dep before tagging), or
      perform the documented source verification (pinned `bcgit/bc-java`
      commit + `./gradlew clean :core:jar :prov:jar :pkix:jar -x test`,
      per-class content diff) and record the pinned commit in `lib/README.md`.
- [ ] Confirm CI jar-pin step is green on the release commit.

## DRAFT release notes — v1.8.0 (dev since v1.7.0)

### JSON Logic VM: ZK / crypto opcodes (waves 1–3)

- Wave 1: `poseidon` (circomlib-compatible over BN254 Fr), `pmt_verify`
  (Poseidon Merkle tree, fixed depth), `groth16_verify` (pure-JVM SP1
  Groth16-BN254 verifier), `ecvrf_verify` (#28, #21, #22, #24).
- Wave 2: `bn254_add` / `bn254_mul` / `bn254_pairing`, `bls_verify`,
  `bls_aggregate_verify`, `schnorr_verify` (#29).
- Wave 3 auth-DB verify opcodes: `smt_verify`, `mpt_verify`,
  `mpt_prefix_verify` (#30).
- Vendored MIRACL Core (ED25519/BN254/BLS12-381) + MIRACL ECVRF; note BN254 ≠
  alt_bn128 — Besu retained for Groth16 (#25); post-merge hardening: Groth16
  error discipline, Poseidon input cap, MIRACL prune (#36).
- Verified a real shielded-transfer SP1 Groth16 proof in the pure-JVM
  verifier (#26).

### Gas metering

- Gas costs for the ZK/crypto opcodes; each op charged exactly once;
  size-scaled costs pre-charged (#37).
- Shared cross-language gas vectors v1.1.0 + conformance suite (#39).

### Authenticated data structures

- Sparse Merkle Tree with membership + non-membership proofs, LevelDB-backed
  (#18).
- MPT: deterministic roots (zero-length Extension fix) + batch/prefix provers
  (#20).

### Numerics & evaluator parity (behavior change — read this)

- Exact-rational numerics for deterministic cross-language arithmetic
  (7b2df41), completed against the Rust reference (#40). Programs relying on
  v1.7.0 floating-point artifacts may evaluate differently; results now match
  the shared cross-language vectors.
- `get`/`let` base-parity gaps closed vs Rust/TS (#32); `!==` and `all([])`
  aligned with the JSON Logic reference (bc198fe).
- Shared cross-language vector conformance suites: base (82b4406), ZK opcode
  vectors v1.8.0 (#38), gas vectors v1.1.0 (#39).

### Crypto

- BLS12-381 reworked to the eth2 proof-of-possession ciphersuite on
  BouncyCastle 1.85, byte-matching tessellation-bls (#33). See the BC beta
  disclosure above.

### Std / hardening

- JSON depth DoS guard; drop-nulls-before-canonicalize restored (#27); stale
  canonical hash fixed (b87cd1b).
- CI: scalafmt check + tests on PRs and pushes to dev/main (d2820a6); vendored
  BC jar sha256 pin check (this PR).
