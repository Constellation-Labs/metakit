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
import rfc8785


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


def serialize_data(data: Any, is_data_update: bool = False) -> bytes:
    if hasattr(data, 'to_dict'):
        data = data.to_dict()
    
    # Use RFC 8785 canonical JSON serialization (returns bytes)
    utf8_bytes = rfc8785.dumps(data)
    
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
    test_data = TestData(id="python-test-data-001", value=42)
    data_bytes = serialize_data(test_data)
    hash_bytes = compute_hash(data_bytes)
    signature = sign_hash(hash_bytes, private_key)
    
    test_vectors.append({
        "source": "python",
        "type": "TestData",
        "data": test_data.to_dict(),
        "canonical_json": rfc8785.dumps(test_data.to_dict()).decode('utf-8'),
        "utf8_bytes_hex": data_bytes.hex(),
        "sha256_hash_hex": hash_bytes.hex(),
        "signature_hex": signature.hex(),
        "public_key_hex": public_key_hex
    })
    
    # Test vector 2: TestDataUpdate with Constellation prefix
    test_update = TestDataUpdate(id="python-test-update-001", value=123)
    update_bytes = serialize_data(test_update, is_data_update=True)
    update_hash_bytes = compute_hash(update_bytes)
    update_signature = sign_hash(update_hash_bytes, private_key)
    
    test_vectors.append({
        "source": "python",
        "type": "TestDataUpdate",
        "data": test_update.to_dict(),
        "canonical_json": rfc8785.dumps(test_update.to_dict()).decode('utf-8'),
        "utf8_bytes_hex": update_bytes.hex(),
        "sha256_hash_hex": update_hash_bytes.hex(),
        "signature_hex": update_signature.hex(),
        "public_key_hex": public_key_hex
    })
    
    # Test vector 3: Additional TestData with different values
    test_data_2 = TestData(id="python-test-data-002", value=999)
    data_bytes_2 = serialize_data(test_data_2)
    hash_bytes_2 = compute_hash(data_bytes_2)
    signature_2 = sign_hash(hash_bytes_2, private_key)
    
    test_vectors.append({
        "source": "python",
        "type": "TestData",
        "data": test_data_2.to_dict(),
        "canonical_json": rfc8785.dumps(test_data_2.to_dict()).decode('utf-8'),
        "utf8_bytes_hex": data_bytes_2.hex(),
        "sha256_hash_hex": hash_bytes_2.hex(),
        "signature_hex": signature_2.hex(),
        "public_key_hex": public_key_hex
    })
    
    # Test vector 4: Additional TestDataUpdate
    test_update_2 = TestDataUpdate(id="python-test-update-002", value=777)
    update_bytes_2 = serialize_data(test_update_2, is_data_update=True)
    update_hash_bytes_2 = compute_hash(update_bytes_2)
    update_signature_2 = sign_hash(update_hash_bytes_2, private_key)
    
    test_vectors.append({
        "source": "python",
        "type": "TestDataUpdate",
        "data": test_update_2.to_dict(),
        "canonical_json": rfc8785.dumps(test_update_2.to_dict()).decode('utf-8'),
        "utf8_bytes_hex": update_bytes_2.hex(),
        "sha256_hash_hex": update_hash_bytes_2.hex(),
        "signature_hex": update_signature_2.hex(),
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