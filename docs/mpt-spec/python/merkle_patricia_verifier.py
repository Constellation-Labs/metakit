#!/usr/bin/env python3
"""
Merkle Patricia Trie Inclusion Proof Verifier

This module provides verification of Merkle Patricia inclusion proofs
compatible with the Scala implementation in the Metakit project.
Uses RFC 8785 compliant JSON canonicalization.
"""

import hashlib
import json
from typing import Dict, List, Tuple, Optional, Union, Any
from dataclasses import dataclass
from enum import Enum
import rfc8785


class VerificationError(Exception):
    """Base class for verification errors"""
    pass


class InvalidWitness(VerificationError):
    """Raised when witness structure is invalid"""
    pass


class InvalidPath(VerificationError):
    """Raised when path is invalid or not found"""
    pass


class InvalidNodeCommitment(VerificationError):
    """Raised when node commitment verification fails"""
    pass


class NodeType(Enum):
    """Node type identifiers"""
    LEAF = "Leaf"
    BRANCH = "Branch"
    EXTENSION = "Extension"


class NodePrefix:
    """Binary prefixes for node types during hashing"""
    LEAF = bytes([0])
    BRANCH = bytes([1])
    EXTENSION = bytes([2])


class Nibble:
    """Utilities for handling nibbles (4-bit values)"""
    
    @staticmethod
    def from_hex_string(hex_string: str) -> List[int]:
        """Convert hex string to list of nibbles"""
        nibbles = []
        for char in hex_string:
            try:
                nibbles.append(int(char, 16))
            except ValueError:
                raise ValueError(f"Invalid hex character: {char}")
        return nibbles
    
    @staticmethod
    def from_hash(hash_str: str) -> List[int]:
        """Convert hash string to nibbles"""
        return Nibble.from_hex_string(hash_str)
    
    @staticmethod
    def sequence_equals(a: List[int], b: List[int]) -> bool:
        """Check if two nibble sequences are equal"""
        return a == b
    
    @staticmethod
    def sequence_drop(sequence: List[int], count: int) -> List[int]:
        """Drop first count elements from sequence"""
        return sequence[count:]


class JsonBinaryHasher:
    """Hash computation for JSON objects with binary prefixes using RFC 8785"""
    
    @staticmethod
    def compute_digest(json_object: Dict, prefix_bytes: bytes = b"") -> str:
        """Compute SHA-256 hash of JSON object with optional prefix"""
        # Use RFC 8785 canonical JSON serialization (returns bytes)
        canonical_json_bytes = rfc8785.dumps(json_object)
        
        # Combine prefix and JSON bytes
        combined = prefix_bytes + canonical_json_bytes
        
        # Compute SHA-256 hash
        hash_obj = hashlib.sha256()
        hash_obj.update(combined)
        return hash_obj.hexdigest()


@dataclass
class VerificationResult:
    """Result of verification attempt"""
    success: bool
    error: Optional[VerificationError] = None
    remaining_path: Optional[List[int]] = None
    child_digest: Optional[str] = None


class MerklePatriciaVerifier:
    """Verifier for Merkle Patricia inclusion proofs"""
    
    def __init__(self, root_hash: str):
        """
        Initialize verifier with root hash
        
        Args:
            root_hash: The root hash of the Merkle Patricia Trie
        """
        self.root_hash = root_hash
    
    def verify(self, proof: Dict) -> VerificationResult:
        """
        Verify a Merkle Patricia inclusion proof
        
        Args:
            proof: Dictionary containing 'path' and 'witness' fields
            
        Returns:
            VerificationResult indicating success or failure
        """
        try:
            # Parse the path as nibbles
            path_nibbles = Nibble.from_hash(proof['path'])
            
            # Process witness in reverse order (from root to leaf)
            witness = list(reversed(proof['witness']))
            
            # Start verification from root
            current_digest = self.root_hash
            remaining_path = path_nibbles
            index = 0
            
            while index < len(witness):
                commitment = witness[index]
                
                if 'type' not in commitment or 'contents' not in commitment:
                    raise InvalidWitness("Invalid commitment structure")
                
                node_type = commitment['type']
                contents = commitment['contents']
                
                if node_type == NodeType.LEAF.value:
                    # Leaf must be the final node
                    if index != len(witness) - 1:
                        raise InvalidWitness("Leaf must be the final commitment")
                    return self._verify_leaf(contents, current_digest, remaining_path)
                
                elif node_type == NodeType.EXTENSION.value:
                    result = self._verify_extension(contents, current_digest, remaining_path)
                    if not result.success:
                        return result
                    current_digest = contents['childDigest']
                    remaining_path = result.remaining_path
                    index += 1
                
                elif node_type == NodeType.BRANCH.value:
                    result = self._verify_branch(contents, current_digest, remaining_path)
                    if not result.success:
                        return result
                    current_digest = result.child_digest
                    remaining_path = result.remaining_path
                    index += 1
                
                else:
                    raise InvalidWitness(f"Unknown commitment type: {node_type}")
            
            raise InvalidWitness("Proof verification incomplete")
            
        except VerificationError as e:
            return VerificationResult(success=False, error=e)
        except Exception as e:
            return VerificationResult(
                success=False,
                error=InvalidWitness(f"Verification failed: {str(e)}")
            )
    
    def _verify_leaf(self, node_commit: Dict, current_digest: str, 
                     remaining_path: List[int]) -> VerificationResult:
        """Verify a leaf commitment"""
        # Compute the expected digest for this leaf commitment
        commitment_json = {
            'remaining': node_commit['remaining'],
            'dataDigest': node_commit['dataDigest']
        }
        
        computed_digest = JsonBinaryHasher.compute_digest(
            commitment_json, NodePrefix.LEAF
        )
        
        # Verify digest matches
        if computed_digest != current_digest:
            return VerificationResult(
                success=False,
                error=InvalidNodeCommitment("Invalid leaf commitment digest")
            )
        
        # Parse remaining path from commitment
        commitment_remaining = Nibble.from_hex_string(node_commit['remaining'])
        
        # Verify remaining path matches
        if not Nibble.sequence_equals(remaining_path, commitment_remaining):
            return VerificationResult(
                success=False,
                error=InvalidNodeCommitment("Path mismatch at leaf")
            )
        
        return VerificationResult(success=True)
    
    def _verify_extension(self, node_commit: Dict, current_digest: str,
                          remaining_path: List[int]) -> VerificationResult:
        """Verify an extension commitment"""
        # Compute the expected digest for this extension commitment
        commitment_json = {
            'shared': node_commit['shared'],
            'childDigest': node_commit['childDigest']
        }
        
        computed_digest = JsonBinaryHasher.compute_digest(
            commitment_json, NodePrefix.EXTENSION
        )
        
        # Verify digest matches
        if computed_digest != current_digest:
            return VerificationResult(
                success=False,
                error=InvalidNodeCommitment("Invalid extension commitment digest")
            )
        
        # Parse shared path
        shared_nibbles = Nibble.from_hex_string(node_commit['shared'])
        
        # Verify path starts with shared prefix
        for i, nibble in enumerate(shared_nibbles):
            if i >= len(remaining_path) or remaining_path[i] != nibble:
                return VerificationResult(
                    success=False,
                    error=InvalidPath("Path does not match extension shared prefix")
                )
        
        # Update remaining path
        new_remaining_path = Nibble.sequence_drop(remaining_path, len(shared_nibbles))
        
        return VerificationResult(
            success=True,
            remaining_path=new_remaining_path
        )
    
    def _verify_branch(self, node_commit: Dict, current_digest: str,
                       remaining_path: List[int]) -> VerificationResult:
        """Verify a branch commitment"""
        if not remaining_path:
            return VerificationResult(
                success=False,
                error=InvalidPath("No remaining path at branch")
            )
        
        # Get next nibble in path
        next_nibble = remaining_path[0]
        nibble_key = format(next_nibble, 'x')
        
        # Check if branch has path for this nibble
        if 'pathsDigest' not in node_commit or nibble_key not in node_commit['pathsDigest']:
            return VerificationResult(
                success=False,
                error=InvalidPath(f"Path not found in branch: {nibble_key}")
            )
        
        # Compute the expected digest for this branch commitment
        commitment_json = {
            'pathsDigest': node_commit['pathsDigest']
        }
        
        computed_digest = JsonBinaryHasher.compute_digest(
            commitment_json, NodePrefix.BRANCH
        )
        
        # Verify digest matches
        if computed_digest != current_digest:
            return VerificationResult(
                success=False,
                error=InvalidNodeCommitment("Invalid branch commitment digest")
            )
        
        # Get child digest for next step
        child_digest = node_commit['pathsDigest'][nibble_key]
        
        # Update remaining path
        new_remaining_path = remaining_path[1:]
        
        return VerificationResult(
            success=True,
            child_digest=child_digest,
            remaining_path=new_remaining_path
        )


def verify_proof_from_file(filepath: str) -> VerificationResult:
    """
    Load and verify a proof from a JSON file
    
    Args:
        filepath: Path to JSON file containing proof and root hash
        
    Returns:
        VerificationResult
    """
    with open(filepath, 'r') as f:
        data = json.load(f)
    
    verifier = MerklePatriciaVerifier(data['rootHash'])
    return verifier.verify(data['proof'])


if __name__ == "__main__":
    # Example usage
    import sys
    import os
    
    # Try to load test proof if available
    test_file = os.path.join(os.path.dirname(__file__), '..', 'test-proof.json')
    if os.path.exists(test_file):
        print("Verifying test proof...")
        result = verify_proof_from_file(test_file)
        if result.success:
            print("✓ Proof is valid!")
        else:
            print(f"✗ Proof is invalid: {result.error}")
            sys.exit(1)
    else:
        print("Test proof file not found")