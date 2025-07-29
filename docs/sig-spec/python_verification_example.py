#!/usr/bin/env python3
"""
Python implementation of Constellation Network signature protocol.
Generates test vectors with signatures that can be verified in Scala.

This demonstrates cross-language compatibility of the signature protocol
as specified in the formal specification.
"""

import json
import hashlib
import base64
from typing import Dict, Any, List
from dataclasses import dataclass
from ecdsa import SigningKey, VerifyingKey, SECP256k1
from ecdsa.util import sigdecode_der, sigencode_der


@dataclass
class TestData:
    id: str
    value: int

    def to_dict(self) -> Dict[str, Any]:
        return {"id": self.id, "value": self.value}


@dataclass
class TestDataUpdate:
    id: str
    value: int

    def to_dict(self) -> Dict[str, Any]:
        return {"id": self.id, "value": self.value}


def canonicalize_json_rfc8785(obj: Any) -> str:
    """Canonicalize JSON according to RFC 8785"""
    if obj is None:
        return "null"
    elif isinstance(obj, bool):
        return "true" if obj else "false"
    elif isinstance(obj, (int, float)):
        if obj == int(obj):
            return str(int(obj))
        return json.dumps(float(obj))
    elif isinstance(obj, str):
        return '"' + obj.replace('\\', '\\\\').replace('"', '\\"') + '"'
    elif isinstance(obj, list):
        items = [canonicalize_json_rfc8785(item) for item in obj]
        return "[" + ",".join(items) + "]"
    elif isinstance(obj, dict):
        # Sort by UTF-16BE bytes
        sorted_items = sorted(obj.items(), key=lambda x: x[0].encode('utf-16-be'))
        items = [f'"{k}":{canonicalize_json_rfc8785(v)}' for k, v in sorted_items]
        return "{" + ",".join(items) + "}"
    elif hasattr(obj, 'to_dict'):
        return canonicalize_json_rfc8785(obj.to_dict())
    else:
        raise ValueError(f"Cannot canonicalize object of type {type(obj)}")


def serialize_data(data: Any, is_data_update: bool = False) -> bytes:
    """Serialize data according to Constellation protocol"""
    canonical_json = canonicalize_json_rfc8785(data)
    utf8_bytes = canonical_json.encode('utf-8')
    
    if is_data_update:
        # Add Constellation prefix for DataUpdate
        base64_string = base64.b64encode(utf8_bytes).decode('ascii')
        wrapped_string = f"\x19Constellation Signed Data:\n{len(base64_string)}\n{base64_string}"
        return wrapped_string.encode('utf-8')
    
    return utf8_bytes


def compute_hash(data_bytes: bytes) -> bytes:
    """Compute SHA-256 hash"""
    return hashlib.sha256(data_bytes).digest()


def sign_hash(hash_bytes: bytes, private_key: SigningKey) -> bytes:
    """
    Sign hash using Constellation's protocol:
    1. Convert hash bytes to hex string
    2. Treat hex string as UTF-8 bytes 
    3. Sign with SHA-512 digest, truncated for secp256k1
    """
    hash_hex = hash_bytes.hex()
    hash_bytes_for_signing = hash_hex.encode('utf-8')  # Critical: UTF-8 encode, not hex decode
    sha512_hash = hashlib.sha512(hash_bytes_for_signing).digest()
    truncated_hash = sha512_hash[:32]  # Truncate for secp256k1
    return private_key.sign_digest(truncated_hash, sigencode=sigencode_der)


def create_test_vectors() -> List[Dict[str, Any]]:
    """Create signed test vectors for Scala verification"""
    private_key = SigningKey.generate(curve=SECP256k1)
    public_key = private_key.verifying_key
    
    # Get public key in uncompressed format
    x_coord = public_key.pubkey.point.x()
    y_coord = public_key.pubkey.point.y()
    public_key_hex = f"04{x_coord:064x}{y_coord:064x}"
    
    test_vectors = []
    
    # Test vector 1: Regular TestData
    test_data = TestData(id="cross-verify-001", value=42)
    data_bytes = serialize_data(test_data)
    hash_bytes = compute_hash(data_bytes)
    signature = sign_hash(hash_bytes, private_key)
    
    test_vectors.append({
        "type": "TestData",
        "data": test_data.to_dict(),
        "canonical_json": canonicalize_json_rfc8785(test_data),
        "utf8_bytes_hex": data_bytes.hex(),
        "sha256_hash_hex": hash_bytes.hex(),
        "signature_hex": signature.hex(),
        "public_key_hex": public_key_hex
    })
    
    # Test vector 2: TestDataUpdate with Constellation prefix
    test_update = TestDataUpdate(id="cross-verify-update", value=123)
    update_bytes = serialize_data(test_update, is_data_update=True)
    update_hash_bytes = compute_hash(update_bytes)
    update_signature = sign_hash(update_hash_bytes, private_key)
    
    test_vectors.append({
        "type": "TestDataUpdate",
        "data": test_update.to_dict(),
        "canonical_json": canonicalize_json_rfc8785(test_update),
        "serialized_with_prefix": update_bytes.decode('utf-8'),
        "utf8_bytes_hex": update_bytes.hex(),
        "sha256_hash_hex": update_hash_bytes.hex(),
        "signature_hex": update_signature.hex(),
        "public_key_hex": public_key_hex
    })
    
    return test_vectors


def main():
    """Generate test vectors with signatures for Scala verification"""
    print("=== Python: Generating Signed Test Vectors ===\n")
    
    test_vectors = create_test_vectors()
    
    for i, vector in enumerate(test_vectors, 1):
        print(f"Test Vector {i} ({vector['type']}):")
        print(f"  Data: {json.dumps(vector['data'])}")
        print(f"  Hash: {vector['sha256_hash_hex']}")
        print(f"  Signature: {vector['signature_hex'][:32]}...{vector['signature_hex'][-32:]}")
        print(f"  Public Key: {vector['public_key_hex'][:32]}...{vector['public_key_hex'][-32:]}")
        print()
    
    # Save test vectors for Scala verification
    with open('test_vectors.json', 'w') as f:
        json.dump(test_vectors, f, indent=2)
    
    print("✓ Test vectors saved to test_vectors.json")
    print("✓ Run Scala verification to verify Python signatures")


if __name__ == "__main__":
    main()