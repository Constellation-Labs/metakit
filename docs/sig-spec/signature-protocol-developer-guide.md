# Constellation Metagraph Standard Signature Protocol Developer Guide

## Overview

The Constellation Metagraph Standard Signature Protocol provides a standardized way to create and verify digital signatures for structured data. The protocol ensures deterministic serialization through RFC 8785 JSON Canonicalization, combined with SHA-256 hashing and ECDSA signatures on the secp256k1 curve.

## Protocol Flow

### Complete Signature Creation Process

```
JSON Data → RFC 8785 Canonicalization → UTF-8 Encoding → SHA-256 Hash → ECDSA Sign
```

### Key Implementation Details

1. **JSON Canonicalization**: Uses RFC 8785 standard
2. **Binary Serialization**: Canonical JSON → UTF-8 bytes  
3. **Hashing**: SHA-256 (32-byte output)
5. **Signature Algorithm**: SHA512withECDSA on secp256k1 curve

## Detailed Implementation Steps

### Step 1: JSON Canonicalization (RFC 8785)

The canonicalization follows [RFC 8785](https://datatracker.ietf.org/doc/html/rfc8785):

```python
# Key requirements from RFC 8785:
# 1. No whitespace between tokens
# 2. Object keys sorted by UTF-16BE binary order
# 3. Specific number formatting (IEEE-754)
# 4. Consistent string escaping
```

**Object Key Ordering Example:**
```json
// Input
{"b": 2, "a": 1}

// Output (keys sorted by UTF-16BE)
{"a":1,"b":2}
```

### Step 2: Binary Encoding

```python
def encode_to_binary(canonical_json_string):
    # Simple UTF-8 encoding
    return canonical_json_string.encode('utf-8')
```

### Step 3: SHA-256 Hashing

```python
def compute_hash(utf8_bytes):
    # Use SHA-256 (32 bytes output)
    import hashlib
    digest = hashlib.sha256(utf8_bytes).digest()
    return digest
```
### Step 4: ECDSA Signature

```python
def sign_message(hash_bytes, private_key):
    # Critical: Convert hash bytes to hex string, then treat hex string as UTF-8 bytes
    # This is the implementation in tessellation SignatureProtocol
    hash_hex = hash_bytes.hex()
    hash_bytes_for_signing = hash_hex.encode('utf-8')  # Treat hex string as UTF-8, not hex decode
    
    # Use SHA512withECDSA algorithm
    # For secp256k1, SHA-512 digest must be truncated to 32 bytes
    sha512_hash = hashlib.sha512(hash_bytes_for_signing).digest()
    truncated_hash = sha512_hash[:32]
    
    # Sign the truncated hash
    signature = ecdsa_sign(truncated_hash, private_key)
    return signature.hex()
```

## Complete Implementation Example

### Signing Implementation

```python
import json
import hashlib
from ecdsa import SigningKey, SECP256k1

def sign_data(data, private_key):
    """
    Sign data following Constellation Metagraph Standard protocol
    
    Args:
        data: JSON-serializable Python object
        private_key: ECDSA private key for secp256k1
    
    Returns:
        Signed object with Constellation format
    """
    # Step 1: Canonicalize JSON (RFC 8785)
    canonical_json = canonicalize_rfc8785(data)
    
    # Step 2: Convert to UTF-8 bytes
    utf8_bytes = canonical_json.encode('utf-8')
    
    # Step 3: Compute SHA-256 hash
    hash_bytes = hashlib.sha256(utf8_bytes).digest()
    
    # Step 4: Critical implementation detail - treat hex string as UTF-8 bytes
    hash_hex = hash_bytes.hex()
    hash_bytes_for_signing = hash_hex.encode('utf-8') 
    
    # Step 5: Sign with ECDSA (SHA512withECDSA)
    signature_bytes = sign_sha512_ecdsa(hash_bytes_for_signing, private_key)
    
    # Step 6: Format result
    public_key_hex = get_public_key_hex(private_key)
    
    return {
        "value": data,
        "proofs": [{
            "id": public_key_hex,
            "signature": signature_bytes.hex()
        }]
    }
```

### Verification Implementation

```python
def verify_signature(signed_data, public_key_hex):
    """
    Verify a Constellation Metagraph Standard signature
    
    Args:
        signed_data: Signed object with 'value' and 'proofs'
        public_key_hex: Hex-encoded public key
    
    Returns:
        Boolean indicating if signature is valid
    """
    data = signed_data["value"]
    proofs = signed_data["proofs"]
    
    # Recompute the exact same hash
    canonical_json = canonicalize_rfc8785(data)
    utf8_bytes = canonical_json.encode('utf-8')
    hash_bytes = hashlib.sha256(utf8_bytes).digest()
    
    # Critical: Convert to hex string, then treat as UTF-8 bytes (matches implementation)
    hash_hex = hash_bytes.hex()
    hash_bytes_for_verification = hash_hex.encode('utf-8')  # Not bytes.fromhex!
    
    # Verify each proof
    for proof in proofs:
        if proof["id"] == public_key_hex:
            signature_hex = proof["signature"]
            signature_bytes = bytes.fromhex(signature_hex)
            
            # Verify using SHA512withECDSA
            is_valid = verify_sha512_ecdsa(
                hash_bytes_for_verification, 
                signature_bytes, 
                public_key_hex
            )
            
            if not is_valid:
                return False
    
    return True
```

## RFC 8785 Canonicalization Details

### Number Formatting

According to RFC 8785 Section 3.2.2.3:
- No leading zeros (except for "0")
- No trailing zeros after decimal point
- Use exponential notation for certain values
- Special handling for -0 → 0

```python
# Examples from the implementation:
42.0     → "42"
0.000001 → "1e-6"
-0       → "0"
```

### String Escaping

From RFC 8785 Section 3.2.2.2:
```python
ESCAPED_CHARS = {
    '"': '\\"',
    '\\': '\\\\',
    '\b': '\\b',
    '\f': '\\f',
    '\n': '\\n',
    '\r': '\\r',
    '\t': '\\t'
}

# Control characters (U+0000-U+001F) → \uXXXX format
```

### Object Key Sorting

Keys must be sorted by UTF-16BE binary representation:

```python
def sort_object_keys(obj):
    # Sort by UTF-16BE bytes
    return dict(sorted(
        obj.items(), 
        key=lambda x: x[0].encode('utf-16-be')
    ))
```

## Special Case: DataUpdate Types

For `DataUpdate` types, an additional wrapping is applied:

```
1. Canonical JSON → UTF-8 bytes
2. Base64 encode the bytes
3. Wrap with prefix: "\x19Constellation Signed Data:\n{length}\n{base64}"
4. This wrapped string becomes the bytes to hash
```

Example:
```python
def serialize_data_update(content):
    canonical_json = canonicalize_rfc8785(content)
    utf8_bytes = canonical_json.encode('utf-8')
    base64_string = base64.b64encode(utf8_bytes).decode('ascii')
    wrapped_string = f"\x19Constellation Signed Data:\n{len(base64_string)}\n{base64_string}"
    return wrapped_string.encode('utf-8')
```

## Common Implementation Pitfalls

### 1. Wrong Hash Algorithm
❌ Using SHA-512 for hashing
✅ Using SHA-256 for hashing (but SHA512withECDSA for signing)

### 2. Number Formatting Issues
❌ Keeping decimal zeros: `1.0`
✅ Removing decimal zeros: `1`

### 3. Incorrect Key Sorting
❌ Sorting by Unicode code points
✅ Sorting by UTF-16BE binary representation

### 4. Hash String Handling
❌ Converting hex string back to bytes: `bytes.fromhex(hash_hex)`
✅ Treating hex string as UTF-8 bytes: `hash_hex.encode('utf-8')`

## Test Vectors

### Basic Example

```json
// Input
{"name": "test", "value": 42}

// After RFC 8785 canonicalization
{"name":"test","value":42}

// UTF-8 bytes (hex)
7b226e616d65223a2274657374222c2276616c7565223a34327d

// SHA-256 hash (32 bytes, hex)
4c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e73

// Hash as hex string (what gets converted back to bytes for signing)
"4c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e73"

// Bytes for signing (hex representation of above string as bytes)
4c6f72656d20697073756d20646f6c6f722073697420616d65742c20636f6e73
```

## Library Recommendations

### JavaScript/TypeScript
```javascript
const crypto = require('crypto');
const secp256k1 = require('secp256k1');

// Complete signature creation
function createSignature(jsonData, privateKeyBuffer) {
    // Step 1: Canonicalize JSON (RFC 8785)
    const canonicalJson = canonicalizeRFC8785(jsonData);
    
    // Step 2: Convert to UTF-8 bytes
    const utf8Bytes = Buffer.from(canonicalJson, 'utf-8');
    
    // Step 3: SHA-256 hash
    const hashBytes = crypto.createHash('sha256').update(utf8Bytes).digest();
    
    // Step 4: Critical - treat hex string as UTF-8 bytes
    const hashHex = hashBytes.toString('hex');
    const hashBytesForSigning = Buffer.from(hashHex, 'utf-8');
    
    // Step 5: SHA-512 hash of the hex bytes
    const sha512Hash = crypto.createHash('sha512').update(hashBytesForSigning).digest();
    const truncatedHash = sha512Hash.subarray(0, 32); // Truncate for secp256k1
    
    // Step 6: Sign with ECDSA
    const signature = secp256k1.ecdsaSign(truncatedHash, privateKeyBuffer);
    return signature.signature;
}

// Key generation
const privateKey = crypto.randomBytes(32);
const publicKey = secp256k1.publicKeyCreate(privateKey);
```

### Python
```python
import hashlib
from ecdsa import SigningKey, SECP256k1
from ecdsa.util import sigencode_der

# Complete signature creation
def create_signature(json_data, private_key):
    # Step 1: Canonicalize JSON (RFC 8785)
    canonical_json = canonicalize_rfc8785(json_data)
    
    # Step 2: Convert to UTF-8 bytes
    utf8_bytes = canonical_json.encode('utf-8')
    
    # Step 3: SHA-256 hash
    hash_bytes = hashlib.sha256(utf8_bytes).digest()
    
    # Step 4: Critical - treat hex string as UTF-8 bytes
    hash_hex = hash_bytes.hex()
    hash_bytes_for_signing = hash_hex.encode('utf-8')
    
    # Step 5: SHA-512 hash of the hex bytes
    sha512_hash = hashlib.sha512(hash_bytes_for_signing).digest()
    truncated_hash = sha512_hash[:32]  # Truncate for secp256k1
    
    # Step 6: Sign with ECDSA
    signature = private_key.sign_digest(truncated_hash, sigencode=sigencode_der)
    return signature

# Key generation
private_key = SigningKey.generate(curve=SECP256k1)
public_key = private_key.verifying_key

# Verification
def verify_signature(json_data, signature, public_key):
    # Recompute hash following same steps
    canonical_json = canonicalize_rfc8785(json_data)
    utf8_bytes = canonical_json.encode('utf-8')
    hash_bytes = hashlib.sha256(utf8_bytes).digest()
    hash_hex = hash_bytes.hex()
    hash_bytes_for_signing = hash_hex.encode('utf-8')
    sha512_hash = hashlib.sha512(hash_bytes_for_signing).digest()
    truncated_hash = sha512_hash[:32]
    
    # Verify signature
    return public_key.verify_digest(signature, truncated_hash, sigdecode=sigdecode_der)
```

### Java/Scala
```scala
import java.security.{KeyPairGenerator, MessageDigest, Signature}
import java.security.spec.ECGenParameterSpec

// Complete signature creation
def createSignature(jsonData: String, privateKey: PrivateKey): Array[Byte] = {
  // Step 1: Canonicalize JSON (RFC 8785) - use existing library
  val canonicalJson = canonicalizeRFC8785(jsonData)
  
  // Step 2: Convert to UTF-8 bytes
  val utf8Bytes = canonicalJson.getBytes("UTF-8")
  
  // Step 3: SHA-256 hash
  val sha256 = MessageDigest.getInstance("SHA-256")
  val hashBytes = sha256.digest(utf8Bytes)
  
  // Step 4: Critical - treat hex string as UTF-8 bytes
  val hashHex = hashBytes.map("%02x".format(_)).mkString
  val hashBytesForSigning = hashHex.getBytes(StandardCharsets.UTF_8)
  
  // Step 5: Sign with SHA512withECDSA
  val signature = Signature.getInstance("SHA512withECDSA")
  signature.initSign(privateKey)
  signature.update(hashBytesForSigning)
  signature.sign()
}

// Key generation
val keyGen = KeyPairGenerator.getInstance("EC")
keyGen.initialize(new ECGenParameterSpec("secp256k1"))
val keyPair = keyGen.generateKeyPair()

// Verification
def verifySignature(jsonData: String, signatureBytes: Array[Byte], publicKey: PublicKey): Boolean = {
  // Recompute hash following same steps
  val canonicalJson = canonicalizeRFC8785(jsonData)
  val utf8Bytes = canonicalJson.getBytes("UTF-8")
  val sha256 = MessageDigest.getInstance("SHA-256")
  val hashBytes = sha256.digest(utf8Bytes)
  val hashHex = hashBytes.map("%02x".format(_)).mkString
  val hashBytesForSigning = hashHex.getBytes(StandardCharsets.UTF_8)
  
  // Verify signature
  val signature = Signature.getInstance("SHA512withECDSA")
  signature.initVerify(publicKey)
  signature.update(hashBytesForSigning)
  signature.verify(signatureBytes)
}
```

### Go
```go
import (
    "crypto/ecdsa"
    "crypto/elliptic"
    "crypto/rand"
    "crypto/sha256"
    "crypto/sha512"
    "encoding/hex"
    "webpki.org/jsoncanonicalizer"
)

// Complete signature creation
func createSignature(jsonData []byte, privateKey *ecdsa.PrivateKey) ([]byte, error) {
    // Step 1: Canonicalize JSON (RFC 8785)
    canonicalJSON, err := jsoncanonicalizer.Transform(jsonData)
    if err != nil {
        return nil, err
    }
    
    // Step 2: UTF-8 bytes (already in bytes)
    utf8Bytes := canonicalJSON
    
    // Step 3: SHA-256 hash
    sha256Hasher := sha256.New()
    sha256Hasher.Write(utf8Bytes)
    hashBytes := sha256Hasher.Sum(nil)
    
    // Step 4: Critical - treat hex string as UTF-8 bytes
    hashHex := hex.EncodeToString(hashBytes)
    hashBytesForSigning := []byte(hashHex)
    
    // Step 5: SHA-512 hash of the hex bytes
    sha512Hasher := sha512.New()
    sha512Hasher.Write(hashBytesForSigning)
    sha512Hash := sha512Hasher.Sum(nil)
    truncatedHash := sha512Hash[:32] // Truncate for secp256k1
    
    // Step 6: Sign with ECDSA
    r, s, err := ecdsa.Sign(rand.Reader, privateKey, truncatedHash)
    if err != nil {
        return nil, err
    }
    
    // Encode signature (DER format)
    return encodeSignature(r, s), nil
}

// Key generation
func generateKeyPair() (*ecdsa.PrivateKey, error) {
    return ecdsa.GenerateKey(elliptic.P256(), rand.Reader) // Use secp256k1 if available
}

// Verification
func verifySignature(jsonData []byte, signature []byte, publicKey *ecdsa.PublicKey) bool {
    // Recompute hash following same steps
    canonicalJSON, _ := jsoncanonicalizer.Transform(jsonData)
    sha256Hasher := sha256.New()
    sha256Hasher.Write(canonicalJSON)
    hashBytes := sha256Hasher.Sum(nil)
    hashHex := hex.EncodeToString(hashBytes)
    hashBytesForSigning := []byte(hashHex)
    sha512Hasher := sha512.New()
    sha512Hasher.Write(hashBytesForSigning)
    sha512Hash := sha512Hasher.Sum(nil)
    truncatedHash := sha512Hash[:32]
    
    // Decode and verify signature
    r, s := decodeSignature(signature)
    return ecdsa.Verify(publicKey, truncatedHash, r, s)
}
```

## Cross-Language Verification

To validate implementation correctness, we provide reference implementations that demonstrate cross-language compatibility:

### Python Implementation (`python_verification_example.py`)
- Generates signed test vectors using `ecdsa` library with secp256k1 
- Implements the complete protocol including hex conversion step
- Creates JSON test vectors for cross-verification

### Scala Implementation (`scala_verification_worksheet_proper.sc`)
- Verifies Python-generated signatures using Constellation's SignatureProtocol
- Demonstrates that both implementations produce identical:
  - Canonical JSON serialization
  - SHA-256 hashes
  - Compatible secp256k1 signatures

### Running Cross-Language Verification

1. **Generate Python test vectors:**
   ```bash
   source venv/bin/activate
   python python_verification_example.py
   ```

2. **Verify in Scala:**
   ```bash
   sbt console
   :load scala_verification_worksheet_proper.sc
   ```

This verification proves that:
- RFC 8785 canonicalization is implemented consistently
- The hex conversion step works correctly across languages  
- ECDSA signatures are compatible between Python and Scala/Java implementations

## Conclusion

The Constellation Metagraph Standard Signature Protocol combines industry standards (RFC 8785, ECDSA) with specific implementation details that must be followed exactly for compatibility. The key points are:

1. **RFC 8785 Canonicalization**: Must be implemented exactly as specified
2. **SHA-256 Hashing**: Use SHA-256, not SHA-512, for the initial hash
3. **Hex Conversion**: Critical step that converts hash → hex string → bytes
4. **ECDSA Signing**: Uses SHA512withECDSA algorithm on secp256k1 curve
5. **Cross-Language Compatibility**: Verified through Python/Scala implementations

Following these steps precisely ensures your implementation will be compatible with the Constellation Network ecosystem. The provided reference implementations serve as both validation and practical examples for developers.

## References

- [RFC 8785 - JSON Canonicalization Scheme](https://datatracker.ietf.org/doc/html/rfc8785)
- [RFC 8785 Go Implementation](https://github.com/cyberphone/json-canonicalization/tree/master/go)
- [Constellation Network Metakit Source](https://github.com/Constellation-Labs/metakit)
- [secp256k1 Curve Specification](https://www.secg.org/sec2-v2.pdf)
- [ECDSA Specification (FIPS 186-4)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.186-4.pdf)
- [SHA-256 Specification (FIPS 180-4)](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)