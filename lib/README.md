# Vendored BouncyCastle 1.85 beta (DO NOT MERGE TO A PUBLISHED RELEASE AS-IS)

These are **beta / snapshot** BouncyCastle 1.85 jars, vendored because the
BLS12-381 API metakit's eth2-ciphersuite BLS primitive depends on
(`org.bouncycastle.crypto.bls.*` — `BLS12_381BasicScheme`,
`BLS12_381ProofOfPossession`, `BLS12_381Serialization`, `BLS12_381Aggregation`,
…) exists **only** in the 1.85 line and is not yet published as a stable
managed artifact.

## Pinned hashes (enforced in CI)

These exact bytes are pinned; CI (`.github/workflows/ci.yml`, step
"Verify vendored BouncyCastle jar pins") recomputes the sha256 of both jars on
every build and fails on any mismatch. If you intentionally replace a jar,
update this table and the CI step together.

| jar                                | sha256 | provides |
|------------------------------------|--------|----------|
| `bcprov-jdk18on-1.85-SNAPSHOT.jar` | `03b4a7656d6aedcff28626e26c1645f4b9956bf0c6d614ee0fba3241a2ced5b8` | `org.bouncycastle.crypto.bls.*`, JCE provider |
| `bcpkix-jdk18on-1.85-SNAPSHOT.jar` | `8ec6507c99806587ec739f1e1c710226326f81aecfd8dccad4f6c5753a3e5349` | PKIX/CMS (provider companion) |

## Provenance

Copied verbatim from the canonical Constellation BLS reference,
`tessellation-bls/modules/shared/lib/`, which vendors the exact same two beta
jars with the same `build.sbt` hack. metakit's `Bls12381` primitive is a
byte-for-byte port of tessellation-bls `BlsSigner`, so it must run against the
*same* BC build to stay byte-identical.

What the jars themselves say (verified 2026-06-10):

- **Upstream channel**: these are builds from BouncyCastle's official beta
  channel, <https://downloads.bouncycastle.org/betas/>, which publishes
  `bcprov-jdk18on-1.85-SNAPSHOT.jar` / `bcpkix-jdk18on-1.85-SNAPSHOT.jar` as a
  *rolling* snapshot (rebuilt regularly). The exact bytes vendored here may no
  longer be downloadable from that page — hence the sha256 pins above.
- **Build identity**: OSGi `Bundle-Version: 1.85.0.20603` (both jars; bcpkix
  imports `org.bouncycastle.asn1;version="[1.85.0.20603,1.86)"`, so the pair is
  internally consistent). Signature files are dated 2026-05-31; non-signature
  entries use the reproducible-build epoch timestamp (1980-02-01). Built with
  Bnd 7.1.0, `Created-By: 25 (Oracle Corporation)`.
- **Code signature**: both jars are JCE-signed (`META-INF/BCRSA204.{SF,RSA}`)
  by `CN=Legion of the Bouncy Castle Inc., OU=Java Software Code Signing,
  O=Oracle Corporation`, issued by Oracle's `JCE Code Signing CA` (signer cert
  valid 2025-12-17 → 2030-12-17). `jarsigner -verify` reports `jar verified.`
  (the PKIX-path warning is expected: the JCE Code Signing CA root is not in
  the default JDK truststore; stable BC releases from Maven Central warn
  identically).
- **No source commit embedded**: the manifests carry no git commit hash, and
  upstream publishes no `r1rv85`-line tag yet (1.85 is still in development on
  <https://github.com/bcgit/bc-java> `main`). Exact source reproduction is
  therefore deferred — see the release checklist below.

## Release checklist: source verification (deferred)

Building bc-java from source exceeds the time budget for this change, so the
verification that *would* pin these bytes to source is recorded here as a
release-blocking checklist item:

1. Pin the `bcgit/bc-java` `main` commit closest to the snapshot signature
   date (2026-05-31) whose `build.gradle` version is `1.85-SNAPSHOT`, e.g.
   `git rev-list -1 --before=2026-05-31T23:59Z main`.
2. Build the provider + pkix jars from that commit:
   `./gradlew clean :core:jar :prov:jar :pkix:jar -x test`
   (artifacts under `prov/build/libs` and `pkix/build/libs`; `./gradlew
   copyJars` gathers everything into `dist/`).
3. Compare *class contents* against the vendored jars (the vendored jars are
   JCE-signed and Bnd-wrapped, so whole-file sha256 will NOT match a local
   build; diff the extracted `org/bouncycastle/**.class` trees, e.g. with
   `diffoscope` or per-class sha256 lists).
4. Better: skip 1–3 entirely by swapping to the stable managed artifact the
   moment BouncyCastle publishes 1.85 to Maven Central — see below.

## How they are wired in (build.sbt)

sbt auto-adds `<project>/lib/*.jar` to the classpath via `unmanagedBase`. The
`tessellation-sdk` managed dependency pulls BouncyCastle **1.70**
(`bcprov`/`bcpkix`/`bcutil-jdk15on`) transitively; that 1.70 line does **not**
contain the `org.bouncycastle.crypto.bls` package and, left on the classpath,
its `org.bouncycastle.*` classes would shadow the 1.85 ones and the BLS package
would be missing at compile time. So `commonSettings` in `build.sbt` excludes
the transitive 1.70 artifacts:

```scala
excludeDependencies ++= Seq(
  ExclusionRule("org.bouncycastle", "bcprov-jdk15on"),
  ExclusionRule("org.bouncycastle", "bcpkix-jdk15on"),
  ExclusionRule("org.bouncycastle", "bcutil-jdk15on")
)
```

This mirrors the tessellation-bls `build.sbt` hack exactly.

## Migration delta when BC 1.85 is published as a stable managed dep

The swap target is the managed `org.bouncycastle` 1.85 artifacts on Maven
Central, as soon as they exist:

1. Delete `lib/*.jar` (and this README), and remove the jar-pin step from
   `.github/workflows/ci.yml`.
2. Drop the `excludeDependencies` block in `build.sbt`'s `commonSettings`.
3. Add managed `"org.bouncycastle" % "bcprov-jdk18on" % "1.85"` (and
   `"org.bouncycastle" % "bcpkix-jdk18on" % "1.85"`) dependencies (plain `%` —
   these are Java artifacts).

Nothing in `Bls12381.scala` or the BLS tests changes.

## Ciphersuite (the byte-identity contract)

- Scheme: ProofOfPossession (PoP), minimal-pubkey-size (pk ∈ G1 = 48 B, sig ∈ G2 = 96 B).
- Signature DST: `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`
- Proof-of-possession DST: `BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`
- Matches `ethereum/bls12-381-tests` (eth2) and `draft-irtf-cfrg-bls-signature` PoP.
