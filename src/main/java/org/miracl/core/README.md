# Vendored: MIRACL Core (Java) — ED25519, BN254, BLS12381

Generated (Apache-2.0) from MIRACL Core's pure-Java implementation. The library
is produced by MIRACL's `java/config64.py` generator, which instantiates the
shared base classes plus one subpackage per selected curve from `@TOKEN@`
templates. Selected curves: **Ed25519**, **BN254**, **BLS12381** (64-bit limb
configuration).

## Provenance

- Source repo: https://github.com/miracl/core
- Clone commit: `5c956834f7147654ef6a8937ec3b1c20a513295a` (2026-01-26, "Remove redundant comments")
- Clone command: `git clone https://github.com/miracl/core /tmp/miracl-core`
- Generated with: `printf '1\n28\n31\n0\n' | python3 java/config64.py`
  (menu indices: 1 = Ed25519, 28 = BN254, 31 = BLS12381; 0 = finish)
- License: Apache-2.0 (per-file headers retained; `LICENSE.txt` in upstream).

The generated package is `org.miracl.core` with curve subpackages
`org.miracl.core.{ED25519,BN254,BLS12381}` plus the shared base classes
(AES, GCM, HASH256/384/512, HMAC, RAND, SHA3, and the unused-but-self-contained
DILITHIUM/KYBER/NHS/SHARE shipped by the generator). No external dependencies —
JDK only. The sources are copied verbatim from the generator output; no edits.

## ⚠️ BN254 is the Naehrig curve, NOT Ethereum's alt_bn128

MIRACL's `BN254` ROM is the **original Barreto–Naehrig BN254** parameter set
(`org.miracl.core.BN254.ROM`), whose base-field prime and group order are:

```
MIRACL BN254 p = 0x2523648240000001ba344d80000000086121000000000013a700000000000013
MIRACL BN254 r = 0x2523648240000001ba344d8000000007ff9f800000000010a10000000000000d
```

Ethereum's `alt_bn128` (EIP-196/197, the curve every SP1/gnark Groth16 proof is
defined over) uses a *different* BN seed and therefore a different field:

```
alt_bn128   p = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
alt_bn128   r = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001
```

These are **incompatible curves**. No MIRACL BN/FP-BN ROM (BN254, BN254CX,
FP256BN, FP512BN, BN462, BN158) reconstructs the alt_bn128 prime. Consequently:

- MIRACL BN254 **cannot** verify SP1 / Ethereum Groth16 proofs.
- The pure-JVM SP1 Groth16 verifier in
  `io.constellationnetwork.metagraph_sdk.crypto.zk` **must keep using the
  vendored Hyperledger Besu `alt_bn128`** (`org.hyperledger.besu.crypto.altbn128`).
- MIRACL's BN254 is vendored only for completeness / generic BN pairing use; it
  is **not** wired into the Groth16 path.

See `MiraclBn254CompatSuite` for the executable proof of this finding
(ROM constants compared, plus the SP1 fixture verified by Besu but with MIRACL's
BN254 explicitly shown to be a different curve).

ED25519 (p = 2^255 − 19) and BLS12381 (standard BLS12-381 p, r) match their
canonical constants exactly and are safe to use as drop-in curves.

## Used by

- `io.constellationnetwork.metagraph_sdk.crypto.vrf.MiraclEcVrf25519` — an
  ECVRF-EDWARDS25519-SHA512-TAI VRF (draft-irtf-cfrg-vrf-10 / RFC 9381,
  suite 0x03) built on MIRACL's ED25519 Edwards arithmetic, byte-compatible
  with tessellation-nakamoto's `EcVrf25519`.
</content>
