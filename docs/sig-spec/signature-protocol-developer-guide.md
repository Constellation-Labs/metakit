# Constellation Metagraph Standard Signature Protocol Developer Guide

## Overview

The Constellation Metagraph Standard Signature Protocol provides cross-language compatible digital signatures for structured data using RFC 8785 JSON Canonicalization, SHA-256 hashing, and ECDSA signatures on the secp256k1 curve.

## Protocol Flow

```
JSON Data → RFC 8785 Canonicalization → UTF-8 Bytes → SHA-256 Hash → ECDSA Sign
```

### Key Implementation Details

1. **JSON Canonicalization (RFC 8785)**: Standardized JSON formatting with specific key ordering and number formatting
2. **Binary Encoding**: Convert canonical JSON to UTF-8 bytes
3. **SHA-256 Hashing**: Compute 32-byte hash of the UTF-8 bytes
4. **ECDSA Signature**: Sign using the Constellation-specific hex conversion step

## Detailed Signature Creation Steps

### For Regular Data Objects

1. **Start with JSON data**: Take your structured data object containing fields and values

2. **Apply RFC 8785 canonicalization**: Transform the JSON into a deterministic format by sorting object keys in UTF-16BE binary order, removing all whitespace between elements, and formatting numbers without trailing zeros

3. **Convert to UTF-8 bytes**: Take the canonical JSON string and encode it as a UTF-8 byte array

4. **Compute SHA-256 hash**: Feed the UTF-8 byte array into the SHA-256 algorithm to produce a 32-byte hash digest

5. **Convert hash to hex string**: Take each byte of the hash and convert it to a two-character hexadecimal representation, resulting in a 64-character string

6. **Treat hex string as UTF-8 bytes**: Take the hex string and encode it as UTF-8 bytes (not parsing it as hex, but treating the string itself as text)

7. **Apply SHA-512**: Hash these UTF-8 bytes using SHA-512 to produce a 64-byte digest

8. **Truncate to 32 bytes**: Take only the first 32 bytes of the SHA-512 output

9. **Sign with ECDSA**: Use the truncated 32-byte value as input to ECDSA signing with your secp256k1 private key to produce a DER-encoded signature

### For DataUpdate Objects

1. **Steps 1-3 are identical**: Canonicalize JSON and convert to UTF-8 bytes

2. **Base64 encode**: Convert the UTF-8 byte array to a Base64 string

3. **Add Constellation prefix**: Create a new string starting with byte 0x19, followed by the text "Constellation Signed Data:\n", then the length of the Base64 string, a newline, and finally the Base64 string itself

4. **Convert prefixed string to UTF-8 bytes**: Encode this entire prefixed string as UTF-8 bytes

5. **Continue from step 4 above**: Compute SHA-256 hash and proceed with the same hex conversion and signing process

## Reference Implementations

Each implementation demonstrates the complete protocol with cross-language compatibility:

- **Python**: `python/python_verification_example.py` - Uses `rfc8785`, `ecdsa`
- **JavaScript**: `js/javascript_verification_example.js` - Uses `elliptic`, `canonicalize`
- **Go**: `go/main.go` - Uses `btcec`, `cyberphone/json-canonicalization`
- **Rust**: `rust/src/main.rs` - Uses `serde_json_canonicalizer`, `secp256k1`
- **Scala**: `scala_verification_worksheet.sc` - Verifies signatures from other languages

## Cross-Language Verification

### Generate Test Vectors

```bash
# Python
cd python && ./setup_python_env.sh && source venv/bin/activate && python python_verification_example.py

# JavaScript
cd js && ./setup_javascript_env.sh && node javascript_verification_example.js

# Go
cd go && ./setup_go_env.sh && go run main.go

# Rust
cd rust && ./setup_rust_env.sh && cargo run
```

### Verify with Scala

```bash
# Copy test vectors to sig-spec directory
cp {python|js|go|rust}/test_vectors.json .
```

Then verify signatures:
- **IntelliJ IDEA**: Open `scala_verification_worksheet.sc` and click the green run button
- **Online**: Copy your test vectors into the [Scastie worksheet](https://scastie.scala-lang.org/Y4wpfW11QnaoRK2UWHHPTg)

### Expected Output

```
=== Scala: Cross-Language Signature Verification ===
Test Vector 1 (python TestData):
  ✓ Canonical JSON matches: true
  ✓ SHA-256 hash matches: true
  ✓ Signature verified: true
```

## References

- [RFC 8785 - JSON Canonicalization Scheme](https://datatracker.ietf.org/doc/html/rfc8785)
- [RFC 8785 Go Implementation](https://github.com/cyberphone/json-canonicalization/tree/master/go)
- [Constellation Network Metakit Source](https://github.com/Constellation-Labs/metakit)
- [secp256k1 Curve Specification](https://www.secg.org/sec2-v2.pdf)
- [ECDSA Specification (FIPS 186-4)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.186-4.pdf)
- [SHA-256 Specification (FIPS 180-4)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)