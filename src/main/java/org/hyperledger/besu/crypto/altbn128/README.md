# Vendored: Hyperledger Besu alt_bn128 (BN254)

Copied (Apache-2.0) from Hyperledger Besu's pure-Java
`org.hyperledger.besu.crypto.altbn128` — the EIP-196/197 `alt_bn128` precompile
implementation (Ethereum's mainnet BN254 curve). The only change from upstream is
removing Guava (`MoreObjects`) and Tuweni (`Bytes`) usages so these files depend on
the JDK alone (no transitive dependencies).

Source: https://github.com/hyperledger/besu (crypto/algorithms; package
`org.hyperledger.besu.crypto.altbn128`). License: Apache-2.0 (per-file headers retained).

Used by `io.constellationnetwork.metagraph_sdk.crypto.zk` to verify SP1
Groth16-BN254 proofs in pure JVM (no native deps).
