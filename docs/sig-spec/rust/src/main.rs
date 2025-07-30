use anyhow::Result;
use base64::Engine as _;
use serde::{Deserialize, Serialize};
use serde_json_canonicalizer::to_vec;
use secp256k1::{ecdsa::Signature, Message, PublicKey, Secp256k1, SecretKey};
use secp256k1::rand;
use sha2::{Digest, Sha256, Sha512};
use std::fs;

#[derive(Serialize, Deserialize, Debug, Clone)]
struct TestData {
    id: String,
    value: i32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct TestDataUpdate {
    id: String,
    value: i32,
}

#[derive(Serialize, Deserialize, Debug)]
struct TestVector {
    source: String,
    #[serde(rename = "type")]
    test_type: String,
    data: serde_json::Value,
    canonical_json: String,
    utf8_bytes_hex: String,
    sha256_hash_hex: String,
    signature_hex: String,
    public_key_hex: String,
}

/// Serialize data according to Constellation protocol
fn serialize_data<T: Serialize>(data: &T, is_data_update: bool) -> Result<Vec<u8>> {
    // Use canonical JSON serialization according to RFC 8785
    let canonical_json = to_vec(data)?;

    if is_data_update {
        // Add Constellation prefix for DataUpdate
        let base64_string = base64::engine::general_purpose::STANDARD.encode(&canonical_json);
        let wrapped_string = format!(
            "\x19Constellation Signed Data:\n{}\n{}",
            base64_string.len(),
            base64_string
        );
        Ok(wrapped_string.into_bytes())
    } else {
        Ok(canonical_json)
    }
}

/// Compute SHA-256 hash
fn compute_hash(data: &[u8]) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(data);
    hasher.finalize().to_vec()
}

/// Sign hash using Constellation's protocol
fn sign_hash(hash_bytes: &[u8], secret_key: &SecretKey) -> Result<Vec<u8>> {
    let secp = Secp256k1::new();

    // 1. Convert hash bytes to hex string
    let hash_hex = hex::encode(hash_bytes);

    // 2. Treat hex string as UTF-8 bytes
    let hash_bytes_for_signing = hash_hex.as_bytes();

    // 3. Sign with SHA-512 digest, truncated for secp256k1
    let mut hasher = Sha512::new();
    hasher.update(hash_bytes_for_signing);
    let sha512_hash = hasher.finalize();
    let truncated_hash = &sha512_hash[..32]; // Truncate for secp256k1

    // Create message from truncated hash
    let message = Message::from_digest_slice(truncated_hash)?;

    // Sign with ECDSA
    let signature = secp.sign_ecdsa(&message, secret_key);
    Ok(signature.serialize_der().to_vec())
}

/// Verify signature for self-validation
fn verify_signature(
    hash_bytes: &[u8],
    signature_hex: &str,
    public_key_hex: &str,
) -> Result<bool> {
    let secp = Secp256k1::new();

    // Parse public key
    let public_key_bytes = hex::decode(public_key_hex)?;
    let public_key = PublicKey::from_slice(&public_key_bytes)?;

    // Parse signature
    let signature_bytes = hex::decode(signature_hex)?;
    let signature = Signature::from_der(&signature_bytes)?;

    // Recreate the signing process
    let hash_hex = hex::encode(hash_bytes);
    let hash_bytes_for_signing = hash_hex.as_bytes();
    let mut hasher = Sha512::new();
    hasher.update(hash_bytes_for_signing);
    let sha512_hash = hasher.finalize();
    let truncated_hash = &sha512_hash[..32];

    // Create message from truncated hash
    let message = Message::from_digest_slice(truncated_hash)?;

    // Verify signature
    Ok(secp.verify_ecdsa(&message, &signature, &public_key).is_ok())
}

/// Create signed test vectors for cross-language verification
fn create_test_vectors() -> Result<Vec<TestVector>> {
    let secp = Secp256k1::new();
    let (secret_key, public_key) = secp.generate_keypair(&mut rand::thread_rng());

    // Get public key in uncompressed format
    let public_key_bytes = public_key.serialize_uncompressed();
    let public_key_hex = hex::encode(public_key_bytes);

    let mut test_vectors = Vec::new();

    // Test vector 1: Regular TestData
    let test_data = TestData {
        id: "rust-test-data-001".to_string(),
        value: 42,
    };

    let data_bytes = serialize_data(&test_data, false)?;
    let hash_bytes = compute_hash(&data_bytes);
    let signature = sign_hash(&hash_bytes, &secret_key)?;

    let canonical_json = String::from_utf8(to_vec(&test_data)?)?;

    test_vectors.push(TestVector {
        source: "rust".to_string(),
        test_type: "TestData".to_string(),
        data: serde_json::to_value(&test_data)?,
        canonical_json,
        utf8_bytes_hex: hex::encode(&data_bytes),
        sha256_hash_hex: hex::encode(&hash_bytes),
        signature_hex: hex::encode(&signature),
        public_key_hex: public_key_hex.clone(),
    });

    // Test vector 2: TestDataUpdate with Constellation prefix
    let test_update = TestDataUpdate {
        id: "rust-test-update-001".to_string(),
        value: 123,
    };

    let update_bytes = serialize_data(&test_update, true)?;
    let update_hash_bytes = compute_hash(&update_bytes);
    let update_signature = sign_hash(&update_hash_bytes, &secret_key)?;

    let update_canonical_json = String::from_utf8(to_vec(&test_update)?)?;

    test_vectors.push(TestVector {
        source: "rust".to_string(),
        test_type: "TestDataUpdate".to_string(),
        data: serde_json::to_value(&test_update)?,
        canonical_json: update_canonical_json,
        utf8_bytes_hex: hex::encode(&update_bytes),
        sha256_hash_hex: hex::encode(&update_hash_bytes),
        signature_hex: hex::encode(&update_signature),
        public_key_hex: public_key_hex.clone(),
    });

    // Test vector 3: Additional TestData with different values
    let test_data_2 = TestData {
        id: "rust-test-data-002".to_string(),
        value: 999,
    };

    let data_bytes_2 = serialize_data(&test_data_2, false)?;
    let hash_bytes_2 = compute_hash(&data_bytes_2);
    let signature_2 = sign_hash(&hash_bytes_2, &secret_key)?;

    let canonical_json_2 = String::from_utf8(to_vec(&test_data_2)?)?;

    test_vectors.push(TestVector {
        source: "rust".to_string(),
        test_type: "TestData".to_string(),
        data: serde_json::to_value(&test_data_2)?,
        canonical_json: canonical_json_2,
        utf8_bytes_hex: hex::encode(&data_bytes_2),
        sha256_hash_hex: hex::encode(&hash_bytes_2),
        signature_hex: hex::encode(&signature_2),
        public_key_hex: public_key_hex.clone(),
    });

    // Test vector 4: Additional TestDataUpdate
    let test_update_2 = TestDataUpdate {
        id: "rust-test-update-002".to_string(),
        value: 777,
    };

    let update_bytes_2 = serialize_data(&test_update_2, true)?;
    let update_hash_bytes_2 = compute_hash(&update_bytes_2);
    let update_signature_2 = sign_hash(&update_hash_bytes_2, &secret_key)?;

    let update_canonical_json_2 = String::from_utf8(to_vec(&test_update_2)?)?;

    test_vectors.push(TestVector {
        source: "rust".to_string(),
        test_type: "TestDataUpdate".to_string(),
        data: serde_json::to_value(&test_update_2)?,
        canonical_json: update_canonical_json_2,
        utf8_bytes_hex: hex::encode(&update_bytes_2),
        sha256_hash_hex: hex::encode(&update_hash_bytes_2),
        signature_hex: hex::encode(&update_signature_2),
        public_key_hex: public_key_hex,
    });

    Ok(test_vectors)
}

fn main() -> Result<()> {
    println!("=== Rust: Generating Signed Test Vectors ===\n");

    let test_vectors = create_test_vectors()?;

    // Display test vectors
    for (i, vector) in test_vectors.iter().enumerate() {
        println!("Test Vector {} ({}):", i + 1, vector.test_type);
        println!("  Data: {}", vector.data);
        println!("  Hash: {}", vector.sha256_hash_hex);
        
        let sig = &vector.signature_hex;
        if sig.len() > 64 {
            println!(
                "  Signature: {}...{}",
                &sig[..32],
                &sig[sig.len() - 32..]
            );
        } else {
            println!("  Signature: {}", sig);
        }
        
        let pk = &vector.public_key_hex;
        if pk.len() > 64 {
            println!(
                "  Public Key: {}...{}",
                &pk[..32],
                &pk[pk.len() - 32..]
            );
        } else {
            println!("  Public Key: {}", pk);
        }
        println!();
    }

    // Save test vectors for cross-language verification
    let json = serde_json::to_string_pretty(&test_vectors)?;
    fs::write("test_vectors.json", json)?;

    println!("✓ Test vectors saved to test_vectors.json");

    // Self-verification
    println!("\n=== Verifying our own signatures ===");
    for (i, vector) in test_vectors.iter().enumerate() {
        // Recompute hash
        let data_bytes = if vector.test_type == "TestData" {
            let test_data: TestData = serde_json::from_value(vector.data.clone())?;
            serialize_data(&test_data, false)?
        } else {
            let test_update: TestDataUpdate = serde_json::from_value(vector.data.clone())?;
            serialize_data(&test_update, true)?
        };

        let hash_bytes = compute_hash(&data_bytes);

        // Verify signature
        match verify_signature(&hash_bytes, &vector.signature_hex, &vector.public_key_hex) {
            Ok(true) => println!("Vector {}: ✓ Valid", i + 1),
            Ok(false) => println!("Vector {}: ✗ Invalid", i + 1),
            Err(e) => println!("Vector {}: ✗ Error verifying signature: {}", i + 1, e),
        }
    }

    println!("\n✓ Run cross-language verification to verify Rust signatures");

    Ok(())
}