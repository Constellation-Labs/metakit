# Vendored BouncyCastle 1.85 beta (DO NOT MERGE TO A PUBLISHED RELEASE AS-IS)

These are **beta / snapshot** BouncyCastle 1.85 jars, vendored because the
BLS12-381 API metakit's eth2-ciphersuite BLS primitive depends on
(`org.bouncycastle.crypto.bls.*` — `BLS12_381BasicScheme`,
`BLS12_381ProofOfPossession`, `BLS12_381Serialization`, `BLS12_381Aggregation`,
…) exists **only** in the 1.85 line and is not yet published as a stable
managed artifact.

| jar                                | sha256 prefix | provides |
|------------------------------------|---------------|----------|
| `bcprov-jdk18on-1.85-SNAPSHOT.jar` | (beta)        | `org.bouncycastle.crypto.bls.*`, provider |
| `bcpkix-jdk18on-1.85-SNAPSHOT.jar` | (beta)        | PKIX/CMS (provider companion) |

## Provenance

Copied verbatim from the canonical Constellation BLS reference,
`tessellation-bls/modules/shared/lib/`, which vendors the exact same two beta
jars with the same `build.sbt` hack. metakit's `Bls12381` primitive is a
byte-for-byte port of tessellation-bls `BlsSigner`, so it must run against the
*same* BC build to stay byte-identical.

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

1. Delete `lib/*.jar` (and this README).
2. Drop the `excludeDependencies` block in `build.sbt`'s `commonSettings`.
3. Add a managed `org.bouncycastle %% bcprov-jdk18on % "1.85"` dependency.

Nothing in `Bls12381.scala` or the BLS tests changes.

## Ciphersuite (the byte-identity contract)

- Scheme: ProofOfPossession (PoP), minimal-pubkey-size (pk ∈ G1 = 48 B, sig ∈ G2 = 96 B).
- Signature DST: `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`
- Proof-of-possession DST: `BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`
- Matches `ethereum/bls12-381-tests` (eth2) and `draft-irtf-cfrg-bls-signature` PoP.
