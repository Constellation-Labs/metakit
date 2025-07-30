/*
Go implementation of Constellation Network signature protocol.
Generates test vectors with signatures that can be verified across languages.

USAGE:
  1. Setup environment:
     ./setup_go_env.sh

  2. Run the example:
     go run main.go

  3. Or build and run:
     go build -o constellation-sig-spec main.go
     ./constellation-sig-spec

This generates test_vectors.json with cross-language verification data.
*/
package main

import (
	"crypto/sha256"
	"crypto/sha512"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"log"
	"os"

	"github.com/btcsuite/btcd/btcec/v2"
	"github.com/btcsuite/btcd/btcec/v2/ecdsa"
	"github.com/cyberphone/json-canonicalization/go/src/webpki.org/jsoncanonicalizer"
)

// TestData represents the basic test data structure
type TestData struct {
	ID    string `json:"id"`
	Value int    `json:"value"`
}

// TestDataUpdate represents the test data update structure
type TestDataUpdate struct {
	ID    string `json:"id"`
	Value int    `json:"value"`
}

// TestVector represents a complete test vector with all verification data
type TestVector struct {
	Source        string          `json:"source"`
	Type          string          `json:"type"`
	Data          json.RawMessage `json:"data"`
	CanonicalJSON string          `json:"canonical_json"`
	UTF8BytesHex  string          `json:"utf8_bytes_hex"`
	SHA256HashHex string          `json:"sha256_hash_hex"`
	SignatureHex  string          `json:"signature_hex"`
	PublicKeyHex  string          `json:"public_key_hex"`
}

// serializeData serializes data according to Constellation protocol
func serializeData(data interface{}, isDataUpdate bool) ([]byte, error) {
	// First convert to JSON
	jsonBytes, err := json.Marshal(data)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal JSON: %v", err)
	}

	// Canonicalize JSON according to RFC 8785
	canonicalJSON, err := jsoncanonicalizer.Transform(jsonBytes)
	if err != nil {
		return nil, fmt.Errorf("failed to canonicalize JSON: %v", err)
	}

	if isDataUpdate {
		// Add Constellation prefix for DataUpdate
		base64String := base64.StdEncoding.EncodeToString(canonicalJSON)
		wrappedString := fmt.Sprintf("\x19Constellation Signed Data:\n%d\n%s", len(base64String), base64String)
		return []byte(wrappedString), nil
	}

	return canonicalJSON, nil
}

// computeHash computes SHA-256 hash
func computeHash(data []byte) []byte {
	hash := sha256.Sum256(data)
	return hash[:]
}

// signHash signs hash using Constellation's protocol
func signHash(hashBytes []byte, privateKey *btcec.PrivateKey) ([]byte, error) {
	// 1. Convert hash bytes to hex string
	hashHex := hex.EncodeToString(hashBytes)

	// 2. Treat hex string as UTF-8 bytes
	hashBytesForSigning := []byte(hashHex)

	// 3. Sign with SHA-512 digest, truncated for secp256k1
	sha512Hash := sha512.Sum512(hashBytesForSigning)
	truncatedHash := sha512Hash[:32] // Truncate for secp256k1

	// Sign with ECDSA
	signature := ecdsa.Sign(privateKey, truncatedHash)
	return signature.Serialize(), nil
}

// createTestVectors creates signed test vectors for cross-language verification
func createTestVectors() ([]TestVector, error) {
	// Generate key pair
	privateKey, err := btcec.NewPrivateKey()
	if err != nil {
		return nil, fmt.Errorf("failed to generate private key: %v", err)
	}
	publicKey := privateKey.PubKey()

	// Get public key in uncompressed format
	publicKeyBytes := publicKey.SerializeUncompressed()
	publicKeyHex := hex.EncodeToString(publicKeyBytes)

	var testVectors []TestVector

	// Test vector 1: Regular TestData
	testData := TestData{
		ID:    "go-test-data-001",
		Value: 42,
	}

	dataBytes, err := serializeData(testData, false)
	if err != nil {
		return nil, fmt.Errorf("failed to serialize test data: %v", err)
	}

	hashBytes := computeHash(dataBytes)
	signature, err := signHash(hashBytes, privateKey)
	if err != nil {
		return nil, fmt.Errorf("failed to sign hash: %v", err)
	}

	dataJSON, _ := json.Marshal(testData)
	canonicalJSON, _ := jsoncanonicalizer.Transform(dataJSON)

	testVectors = append(testVectors, TestVector{
		Source:        "go",
		Type:          "TestData",
		Data:          dataJSON,
		CanonicalJSON: string(canonicalJSON),
		UTF8BytesHex:  hex.EncodeToString(dataBytes),
		SHA256HashHex: hex.EncodeToString(hashBytes),
		SignatureHex:  hex.EncodeToString(signature),
		PublicKeyHex:  publicKeyHex,
	})

	// Test vector 2: TestDataUpdate with Constellation prefix
	testUpdate := TestDataUpdate{
		ID:    "go-test-update-001",
		Value: 123,
	}

	updateBytes, err := serializeData(testUpdate, true)
	if err != nil {
		return nil, fmt.Errorf("failed to serialize test update: %v", err)
	}

	updateHashBytes := computeHash(updateBytes)
	updateSignature, err := signHash(updateHashBytes, privateKey)
	if err != nil {
		return nil, fmt.Errorf("failed to sign update hash: %v", err)
	}

	updateDataJSON, _ := json.Marshal(testUpdate)
	updateCanonicalJSON, _ := jsoncanonicalizer.Transform(updateDataJSON)

	testVectors = append(testVectors, TestVector{
		Source:        "go",
		Type:          "TestDataUpdate",
		Data:          updateDataJSON,
		CanonicalJSON: string(updateCanonicalJSON),
		UTF8BytesHex:  hex.EncodeToString(updateBytes),
		SHA256HashHex: hex.EncodeToString(updateHashBytes),
		SignatureHex:  hex.EncodeToString(updateSignature),
		PublicKeyHex:  publicKeyHex,
	})

	// Test vector 3: Additional TestData with different values
	testData2 := TestData{
		ID:    "go-test-data-002",
		Value: 777,
	}

	dataBytes2, err := serializeData(testData2, false)
	if err != nil {
		return nil, fmt.Errorf("failed to serialize test data 2: %v", err)
	}

	hashBytes2 := computeHash(dataBytes2)
	signature2, err := signHash(hashBytes2, privateKey)
	if err != nil {
		return nil, fmt.Errorf("failed to sign hash 2: %v", err)
	}

	dataJSON2, _ := json.Marshal(testData2)
	canonicalJSON2, _ := jsoncanonicalizer.Transform(dataJSON2)

	testVectors = append(testVectors, TestVector{
		Source:        "go",
		Type:          "TestData",
		Data:          dataJSON2,
		CanonicalJSON: string(canonicalJSON2),
		UTF8BytesHex:  hex.EncodeToString(dataBytes2),
		SHA256HashHex: hex.EncodeToString(hashBytes2),
		SignatureHex:  hex.EncodeToString(signature2),
		PublicKeyHex:  publicKeyHex,
	})

	// Test vector 4: Additional TestDataUpdate
	testUpdate2 := TestDataUpdate{
		ID:    "go-test-update-002",
		Value: 555,
	}

	updateBytes2, err := serializeData(testUpdate2, true)
	if err != nil {
		return nil, fmt.Errorf("failed to serialize test update 2: %v", err)
	}

	updateHashBytes2 := computeHash(updateBytes2)
	updateSignature2, err := signHash(updateHashBytes2, privateKey)
	if err != nil {
		return nil, fmt.Errorf("failed to sign update hash 2: %v", err)
	}

	updateDataJSON2, _ := json.Marshal(testUpdate2)
	updateCanonicalJSON2, _ := jsoncanonicalizer.Transform(updateDataJSON2)

	testVectors = append(testVectors, TestVector{
		Source:        "go",
		Type:          "TestDataUpdate",
		Data:          updateDataJSON2,
		CanonicalJSON: string(updateCanonicalJSON2),
		UTF8BytesHex:  hex.EncodeToString(updateBytes2),
		SHA256HashHex: hex.EncodeToString(updateHashBytes2),
		SignatureHex:  hex.EncodeToString(updateSignature2),
		PublicKeyHex:  publicKeyHex,
	})

	return testVectors, nil
}

// verifySignature verifies a signature for self-validation
func verifySignature(hashBytes []byte, signatureHex, publicKeyHex string) (bool, error) {
	// Parse public key
	publicKeyBytes, err := hex.DecodeString(publicKeyHex)
	if err != nil {
		return false, fmt.Errorf("failed to decode public key: %v", err)
	}

	publicKey, err := btcec.ParsePubKey(publicKeyBytes)
	if err != nil {
		return false, fmt.Errorf("failed to parse public key: %v", err)
	}

	// Parse signature  
	signatureBytes, err := hex.DecodeString(signatureHex)
	if err != nil {
		return false, fmt.Errorf("failed to decode signature: %v", err)
	}

	signature, err := ecdsa.ParseDERSignature(signatureBytes)
	if err != nil {
		return false, fmt.Errorf("failed to parse signature: %v", err)
	}

	// Recreate the signing process
	hashHex := hex.EncodeToString(hashBytes)
	hashBytesForSigning := []byte(hashHex)
	sha512Hash := sha512.Sum512(hashBytesForSigning)
	truncatedHash := sha512Hash[:32]

	// Verify signature
	return signature.Verify(truncatedHash, publicKey), nil
}

func main() {
	fmt.Println("=== Go: Generating Signed Test Vectors ===\n")

	testVectors, err := createTestVectors()
	if err != nil {
		log.Fatalf("Failed to create test vectors: %v", err)
	}

	// Display test vectors
	for i, vector := range testVectors {
		fmt.Printf("Test Vector %d (%s):\n", i+1, vector.Type)
		fmt.Printf("  Data: %s\n", string(vector.Data))
		fmt.Printf("  Hash: %s\n", vector.SHA256HashHex)
		if len(vector.SignatureHex) > 64 {
			fmt.Printf("  Signature: %s...%s\n", vector.SignatureHex[:32], vector.SignatureHex[len(vector.SignatureHex)-32:])
		} else {
			fmt.Printf("  Signature: %s\n", vector.SignatureHex)
		}
		if len(vector.PublicKeyHex) > 64 {
			fmt.Printf("  Public Key: %s...%s\n", vector.PublicKeyHex[:32], vector.PublicKeyHex[len(vector.PublicKeyHex)-32:])
		} else {
			fmt.Printf("  Public Key: %s\n", vector.PublicKeyHex)
		}
		fmt.Println()
	}

	// Save test vectors for cross-language verification
	file, err := os.Create("test_vectors.json")
	if err != nil {
		log.Fatalf("Failed to create test vectors file: %v", err)
	}
	defer file.Close()

	encoder := json.NewEncoder(file)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(testVectors); err != nil {
		log.Fatalf("Failed to encode test vectors: %v", err)
	}

	fmt.Println("✓ Test vectors saved to test_vectors.json")

	// Self-verification
	fmt.Println("\n=== Verifying our own signatures ===")
	for i, vector := range testVectors {
		// Recompute hash
		var data interface{}
		if vector.Type == "TestData" {
			var testData TestData
			json.Unmarshal(vector.Data, &testData)
			data = testData
		} else {
			var testUpdate TestDataUpdate
			json.Unmarshal(vector.Data, &testUpdate)
			data = testUpdate
		}

		isDataUpdate := vector.Type == "TestDataUpdate"
		dataBytes, _ := serializeData(data, isDataUpdate)
		hashBytes := computeHash(dataBytes)

		// Verify signature
		isValid, err := verifySignature(hashBytes, vector.SignatureHex, vector.PublicKeyHex)
		if err != nil {
			fmt.Printf("Vector %d: ✗ Error verifying signature: %v\n", i+1, err)
		} else if isValid {
			fmt.Printf("Vector %d: ✓ Valid\n", i+1)
		} else {
			fmt.Printf("Vector %d: ✗ Invalid\n", i+1)
		}
	}

	fmt.Println("\n✓ Run cross-language verification to verify Go signatures")
}