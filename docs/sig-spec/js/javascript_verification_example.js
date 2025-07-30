#!/usr/bin/env node
/**
 * JavaScript implementation of Constellation Network signature protocol.
 * Generates test vectors with signatures that can be verified in Scala.
 * 
 * This demonstrates cross-language compatibility of the signature protocol
 * as specified in the formal specification.
 */

import elliptic from "elliptic";
import { sha256 } from 'js-sha256';
import { sha512 } from 'js-sha512';
import canonicalize from 'canonicalize';
import { writeFileSync } from 'fs';
import { Buffer } from 'buffer';

// Initialize the secp256k1 curve
const ec = new elliptic.ec('secp256k1');

/**
 * Serialize data according to Constellation protocol
 */
function serializeData(data, isDataUpdate = false) {
    const canonicalJson = canonicalize(data);
    const utf8Bytes = Buffer.from(canonicalJson, 'utf-8');
    
    if (isDataUpdate) {
        // Add Constellation prefix for DataUpdate
        const base64String = utf8Bytes.toString('base64');
        const wrappedString = `\x19Constellation Signed Data:\n${base64String.length}\n${base64String}`;
        return Buffer.from(wrappedString, 'utf-8');
    }
    
    return utf8Bytes;
}

/**
 * Compute SHA-256 hash
 */
function computeHash(dataBytes) {
    return Buffer.from(sha256.array(dataBytes));
}

/**
 * Sign hash using Constellation's protocol:
 * 1. Convert hash bytes to hex string
 * 2. Treat hex string as UTF-8 bytes 
 * 3. Sign with SHA-512 digest, truncated for secp256k1
 */
function signHash(hashBytes, keyPair) {
    const hashHex = hashBytes.toString('hex');
    const hashBytesForSigning = Buffer.from(hashHex, 'utf-8'); // Critical: UTF-8 decede, not hex decode
    const sha512Hash = Buffer.from(sha512.array(hashBytesForSigning));
    const truncatedHash = sha512Hash.subarray(0, 32); // Truncate for secp256k1
    
    // Sign with ECDSA
    const signature = keyPair.sign(truncatedHash);
    return signature.toDER('hex');
}

/**
 * Create signed test vectors for Scala verification
 */
function createTestVectors() {
    // Generate key pair
    const keyPair = ec.genKeyPair();
    const privateKeyHex = keyPair.getPrivate('hex');
    const publicKeyHex = keyPair.getPublic('hex');
    
    const testVectors = [];
    
    // Test vector 1: Regular TestData
    const testData = {
        id: "javascript-test-data-001",
        value: 42
    };
    
    const dataBytes = serializeData(testData);
    const hashBytes = computeHash(dataBytes);
    const signature = signHash(hashBytes, keyPair);
    
    testVectors.push({
        source: "javascript",
        type: "TestData",
        data: testData,
        canonical_json: canonicalize(testData),
        utf8_bytes_hex: dataBytes.toString('hex'),
        sha256_hash_hex: hashBytes.toString('hex'),
        signature_hex: signature,
        public_key_hex: publicKeyHex
    });
    
    // Test vector 2: TestDataUpdate with Constellation prefix
    const testUpdate = {
        id: "javascript-test-update-001",
        value: 123
    };
    
    const updateBytes = serializeData(testUpdate, true);
    const updateHashBytes = computeHash(updateBytes);
    const updateSignature = signHash(updateHashBytes, keyPair);
    
    testVectors.push({
        source: "javascript",
        type: "TestDataUpdate",
        data: testUpdate,
        canonical_json: canonicalize(testUpdate),
        utf8_bytes_hex: updateBytes.toString('hex'),
        sha256_hash_hex: updateHashBytes.toString('hex'),
        signature_hex: updateSignature,
        public_key_hex: publicKeyHex
    });
    
    // Test vector 3: Additional TestData with different values
    const testData2 = {
        id: "javascript-test-data-002",
        value: 888
    };
    
    const dataBytes2 = serializeData(testData2);
    const hashBytes2 = computeHash(dataBytes2);
    const signature2 = signHash(hashBytes2, keyPair);
    
    testVectors.push({
        source: "javascript",
        type: "TestData",
        data: testData2,
        canonical_json: canonicalize(testData2),
        utf8_bytes_hex: dataBytes2.toString('hex'),
        sha256_hash_hex: hashBytes2.toString('hex'),
        signature_hex: signature2,
        public_key_hex: publicKeyHex
    });
    
    // Test vector 4: Additional TestDataUpdate
    const testUpdate2 = {
        id: "javascript-test-update-002",
        value: 555
    };
    
    const updateBytes2 = serializeData(testUpdate2, true);
    const updateHashBytes2 = computeHash(updateBytes2);
    const updateSignature2 = signHash(updateHashBytes2, keyPair);
    
    testVectors.push({
        source: "javascript",
        type: "TestDataUpdate",
        data: testUpdate2,
        canonical_json: canonicalize(testUpdate2),
        utf8_bytes_hex: updateBytes2.toString('hex'),
        sha256_hash_hex: updateHashBytes2.toString('hex'),
        signature_hex: updateSignature2,
        public_key_hex: publicKeyHex
    });
    
    return testVectors;
}

/**
 * Main function to generate test vectors
 */
function main() {
    console.log("=== JavaScript: Generating Signed Test Vectors ===\n");
    
    const testVectors = createTestVectors();
    
    testVectors.forEach((vector, i) => {
        console.log(`Test Vector ${i + 1} (${vector.type}):`);
        console.log(`  Data: ${JSON.stringify(vector.data)}`);
        console.log(`  Hash: ${vector.sha256_hash_hex}`);
        console.log(`  Signature: ${vector.signature_hex.substring(0, 32)}...${vector.signature_hex.substring(vector.signature_hex.length - 32)}`);
        console.log(`  Public Key: ${vector.public_key_hex.substring(0, 32)}...${vector.public_key_hex.substring(vector.public_key_hex.length - 32)}`);
        console.log();
    });
    
    // Save test vectors for Scala verification
    writeFileSync('test_vectors.json', JSON.stringify(testVectors, null, 2));
    
    console.log("✓ Test vectors saved to test_vectors.json");
    console.log("✓ Run Scala verification to verify JavaScript signatures");
    
    // Also display example of how to verify in JavaScript
    console.log("\n=== Verifying our own signatures ===");
    testVectors.forEach((vector, i) => {
        // Recompute hash
        const isDataUpdate = vector.type === "TestDataUpdate";
        const dataBytes = serializeData(vector.data, isDataUpdate);
        const hashBytes = computeHash(dataBytes);
        const hashHex = hashBytes.toString('hex');
        const hashBytesForSigning = Buffer.from(hashHex, 'utf-8');
        const sha512Hash = Buffer.from(sha512.array(hashBytesForSigning));
        const truncatedHash = sha512Hash.subarray(0, 32);
        
        // Verify signature
        const publicKey = ec.keyFromPublic(vector.public_key_hex, 'hex');
        const isValid = publicKey.verify(truncatedHash, vector.signature_hex);
        
        console.log(`Vector ${i + 1}: ${isValid ? '✓ Valid' : '✗ Invalid'}`);
    });
}

// Run the main function
main();