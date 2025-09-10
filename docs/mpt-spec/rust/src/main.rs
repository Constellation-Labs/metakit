use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use serde_json_canonicalizer::to_vec;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

#[derive(Debug)]
enum VerificationError {
    InvalidWitness(String),
    InvalidPath(String),
    InvalidNodeCommitment(String),
}

impl std::fmt::Display for VerificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VerificationError::InvalidWitness(msg) => write!(f, "InvalidWitness: {}", msg),
            VerificationError::InvalidPath(msg) => write!(f, "InvalidPath: {}", msg),
            VerificationError::InvalidNodeCommitment(msg) => {
                write!(f, "InvalidNodeCommitment: {}", msg)
            }
        }
    }
}

impl std::error::Error for VerificationError {}

// Node type constants
const NODE_TYPE_LEAF: &str = "Leaf";
const NODE_TYPE_BRANCH: &str = "Branch";
const NODE_TYPE_EXTENSION: &str = "Extension";

// Node prefixes for hashing
const LEAF_PREFIX: &[u8] = &[0];
const BRANCH_PREFIX: &[u8] = &[1];
const EXTENSION_PREFIX: &[u8] = &[2];

/// Nibble utilities
struct Nibble;

impl Nibble {
    fn from_hex_string(hex_str: &str) -> Result<Vec<u8>> {
        let mut nibbles = Vec::with_capacity(hex_str.len());
        for ch in hex_str.chars() {
            let nibble = ch
                .to_digit(16)
                .ok_or_else(|| anyhow!("Invalid hex character: {}", ch))?;
            nibbles.push(nibble as u8);
        }
        Ok(nibbles)
    }

    fn sequence_equals(a: &[u8], b: &[u8]) -> bool {
        a == b
    }

    fn sequence_drop(sequence: &[u8], count: usize) -> Vec<u8> {
        if count >= sequence.len() {
            Vec::new()
        } else {
            sequence[count..].to_vec()
        }
    }
}

/// JSON Binary Hasher with RFC 8785 canonicalization
struct JsonBinaryHasher;

impl JsonBinaryHasher {
    fn compute_digest(json_object: &Value, prefix_bytes: &[u8]) -> Result<String> {
        // Use RFC 8785 canonical JSON serialization
        let canonical_json = to_vec(json_object)?;

        // Combine prefix and JSON bytes
        let mut combined = Vec::with_capacity(prefix_bytes.len() + canonical_json.len());
        combined.extend_from_slice(prefix_bytes);
        combined.extend_from_slice(&canonical_json);

        // Compute SHA-256 hash
        let mut hasher = Sha256::new();
        hasher.update(&combined);
        let result = hasher.finalize();

        Ok(hex::encode(result))
    }
}

/// Verification result
#[derive(Debug)]
struct VerificationResult {
    success: bool,
    error: Option<VerificationError>,
    remaining_path: Option<Vec<u8>>,
    child_digest: Option<String>,
}

impl VerificationResult {
    fn success() -> Self {
        VerificationResult {
            success: true,
            error: None,
            remaining_path: None,
            child_digest: None,
        }
    }

    fn failure(error: VerificationError) -> Self {
        VerificationResult {
            success: false,
            error: Some(error),
            remaining_path: None,
            child_digest: None,
        }
    }

    fn with_remaining_path(mut self, path: Vec<u8>) -> Self {
        self.remaining_path = Some(path);
        self
    }

    fn with_child_digest(mut self, digest: String) -> Self {
        self.child_digest = Some(digest);
        self
    }
}

/// Merkle Patricia Verifier
struct MerklePatriciaVerifier {
    root_hash: String,
}

impl MerklePatriciaVerifier {
    fn new(root_hash: String) -> Self {
        MerklePatriciaVerifier { root_hash }
    }

    fn verify(&self, proof: &Value) -> VerificationResult {
        // Parse the path as nibbles
        let path_str = match proof.get("path").and_then(|p| p.as_str()) {
            Some(p) => p,
            None => {
                return VerificationResult::failure(VerificationError::InvalidWitness(
                    "Missing or invalid path".to_string(),
                ))
            }
        };

        let path_nibbles = match Nibble::from_hex_string(path_str) {
            Ok(nibbles) => nibbles,
            Err(e) => {
                return VerificationResult::failure(VerificationError::InvalidPath(e.to_string()))
            }
        };

        // Get witness array
        let witness_raw = match proof.get("witness").and_then(|w| w.as_array()) {
            Some(w) => w,
            None => {
                return VerificationResult::failure(VerificationError::InvalidWitness(
                    "Missing or invalid witness".to_string(),
                ))
            }
        };

        // Process witness in reverse order (from root to leaf)
        let mut witness: Vec<&Value> = witness_raw.iter().rev().collect();

        // Start verification from root
        let mut current_digest = self.root_hash.clone();
        let mut remaining_path = path_nibbles;
        let mut index = 0;

        while index < witness.len() {
            let commitment = witness[index];

            let node_type = match commitment.get("type").and_then(|t| t.as_str()) {
                Some(t) => t,
                None => {
                    return VerificationResult::failure(VerificationError::InvalidWitness(
                        "Missing commitment type".to_string(),
                    ))
                }
            };

            let contents = match commitment.get("contents") {
                Some(c) => c,
                None => {
                    return VerificationResult::failure(VerificationError::InvalidWitness(
                        "Missing commitment contents".to_string(),
                    ))
                }
            };

            match node_type {
                NODE_TYPE_LEAF => {
                    // Leaf must be the final node
                    if index != witness.len() - 1 {
                        return VerificationResult::failure(VerificationError::InvalidWitness(
                            "Leaf must be the final commitment".to_string(),
                        ));
                    }
                    return self.verify_leaf(contents, &current_digest, &remaining_path);
                }
                NODE_TYPE_EXTENSION => {
                    let result = self.verify_extension(contents, &current_digest, &remaining_path);
                    if !result.success {
                        return result;
                    }
                    current_digest = contents
                        .get("childDigest")
                        .and_then(|d| d.as_str())
                        .unwrap_or("")
                        .to_string();
                    remaining_path = result.remaining_path.unwrap_or_default();
                    index += 1;
                }
                NODE_TYPE_BRANCH => {
                    let result = self.verify_branch(contents, &current_digest, &remaining_path);
                    if !result.success {
                        return result;
                    }
                    current_digest = result.child_digest.unwrap_or_default();
                    remaining_path = result.remaining_path.unwrap_or_default();
                    index += 1;
                }
                _ => {
                    return VerificationResult::failure(VerificationError::InvalidWitness(
                        format!("Unknown commitment type: {}", node_type),
                    ))
                }
            }
        }

        VerificationResult::failure(VerificationError::InvalidWitness(
            "Proof verification incomplete".to_string(),
        ))
    }

    fn verify_leaf(
        &self,
        node_commit: &Value,
        current_digest: &str,
        remaining_path: &[u8],
    ) -> VerificationResult {
        // Compute the expected digest for this leaf commitment
        let commitment_json = serde_json::json!({
            "remaining": node_commit.get("remaining"),
            "dataDigest": node_commit.get("dataDigest")
        });

        let computed_digest = match JsonBinaryHasher::compute_digest(&commitment_json, LEAF_PREFIX)
        {
            Ok(digest) => digest,
            Err(e) => {
                return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                    e.to_string(),
                ))
            }
        };

        // Verify digest matches
        if computed_digest != current_digest {
            return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                "Invalid leaf commitment digest".to_string(),
            ));
        }

        // Parse remaining path from commitment
        let remaining_str = match node_commit.get("remaining").and_then(|r| r.as_str()) {
            Some(r) => r,
            None => {
                return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                    "Invalid remaining path in leaf".to_string(),
                ))
            }
        };

        let commitment_remaining = match Nibble::from_hex_string(remaining_str) {
            Ok(nibbles) => nibbles,
            Err(e) => {
                return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                    e.to_string(),
                ))
            }
        };

        // Verify remaining path matches
        if !Nibble::sequence_equals(remaining_path, &commitment_remaining) {
            return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                "Path mismatch at leaf".to_string(),
            ));
        }

        VerificationResult::success()
    }

    fn verify_extension(
        &self,
        node_commit: &Value,
        current_digest: &str,
        remaining_path: &[u8],
    ) -> VerificationResult {
        // Compute the expected digest for this extension commitment
        let commitment_json = serde_json::json!({
            "shared": node_commit.get("shared"),
            "childDigest": node_commit.get("childDigest")
        });

        let computed_digest =
            match JsonBinaryHasher::compute_digest(&commitment_json, EXTENSION_PREFIX) {
                Ok(digest) => digest,
                Err(e) => {
                    return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                        e.to_string(),
                    ))
                }
            };

        // Verify digest matches
        if computed_digest != current_digest {
            return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                "Invalid extension commitment digest".to_string(),
            ));
        }

        // Parse shared path
        let shared_str = match node_commit.get("shared").and_then(|s| s.as_str()) {
            Some(s) => s,
            None => {
                return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                    "Invalid shared path in extension".to_string(),
                ))
            }
        };

        let shared_nibbles = match Nibble::from_hex_string(shared_str) {
            Ok(nibbles) => nibbles,
            Err(e) => {
                return VerificationResult::failure(VerificationError::InvalidPath(e.to_string()))
            }
        };

        // Verify path starts with shared prefix
        for (i, nibble) in shared_nibbles.iter().enumerate() {
            if i >= remaining_path.len() || remaining_path[i] != *nibble {
                return VerificationResult::failure(VerificationError::InvalidPath(
                    "Path does not match extension shared prefix".to_string(),
                ));
            }
        }

        // Update remaining path
        let new_remaining_path = Nibble::sequence_drop(remaining_path, shared_nibbles.len());

        VerificationResult::success().with_remaining_path(new_remaining_path)
    }

    fn verify_branch(
        &self,
        node_commit: &Value,
        current_digest: &str,
        remaining_path: &[u8],
    ) -> VerificationResult {
        if remaining_path.is_empty() {
            return VerificationResult::failure(VerificationError::InvalidPath(
                "No remaining path at branch".to_string(),
            ));
        }

        // Get next nibble in path
        let next_nibble = remaining_path[0];
        let nibble_key = format!("{:x}", next_nibble);

        // Check if branch has path for this nibble
        let paths_digest = match node_commit.get("pathsDigest").and_then(|p| p.as_object()) {
            Some(p) => p,
            None => {
                return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                    "Invalid pathsDigest in branch".to_string(),
                ))
            }
        };

        let child_digest = match paths_digest.get(&nibble_key).and_then(|d| d.as_str()) {
            Some(d) => d.to_string(),
            None => {
                return VerificationResult::failure(VerificationError::InvalidPath(format!(
                    "Path not found in branch: {}",
                    nibble_key
                )))
            }
        };

        // Compute the expected digest for this branch commitment
        let commitment_json = serde_json::json!({
            "pathsDigest": node_commit.get("pathsDigest")
        });

        let computed_digest = match JsonBinaryHasher::compute_digest(&commitment_json, BRANCH_PREFIX)
        {
            Ok(digest) => digest,
            Err(e) => {
                return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                    e.to_string(),
                ))
            }
        };

        // Verify digest matches
        if computed_digest != current_digest {
            return VerificationResult::failure(VerificationError::InvalidNodeCommitment(
                "Invalid branch commitment digest".to_string(),
            ));
        }

        // Update remaining path
        let new_remaining_path = remaining_path[1..].to_vec();

        VerificationResult::success()
            .with_child_digest(child_digest)
            .with_remaining_path(new_remaining_path)
    }
}

/// Load and verify a proof from a JSON file
fn verify_proof_from_file<P: AsRef<Path>>(filepath: P) -> Result<VerificationResult> {
    let data = fs::read_to_string(filepath)?;
    let proof_data: Value = serde_json::from_str(&data)?;

    let root_hash = proof_data
        .get("rootHash")
        .and_then(|r| r.as_str())
        .ok_or_else(|| anyhow!("Missing rootHash in proof file"))?
        .to_string();

    let proof = proof_data
        .get("proof")
        .ok_or_else(|| anyhow!("Missing proof in proof file"))?;

    let verifier = MerklePatriciaVerifier::new(root_hash);
    Ok(verifier.verify(proof))
}

fn main() {
    // Try to load test proof if available
    let test_file = "../test-proof.json";

    if Path::new(test_file).exists() {
        println!("Verifying test proof...");
        match verify_proof_from_file(test_file) {
            Ok(result) => {
                if result.success {
                    println!("✓ Proof is valid!");
                } else {
                    println!("✗ Proof is invalid: {:?}", result.error);
                    std::process::exit(1);
                }
            }
            Err(e) => {
                println!("✗ Failed to load proof: {}", e);
                std::process::exit(1);
            }
        }
    } else {
        println!("Test proof file not found");
    }
}