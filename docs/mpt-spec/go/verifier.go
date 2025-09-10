package main

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/cyberphone/json-canonicalization/go/src/webpki.org/jsoncanonicalizer"
)

// VerificationError types
type VerificationError struct {
	Type    string
	Message string
}

func (e VerificationError) Error() string {
	return fmt.Sprintf("%s: %s", e.Type, e.Message)
}

// NodeType constants
const (
	NodeTypeLeaf      = "Leaf"
	NodeTypeBranch    = "Branch"
	NodeTypeExtension = "Extension"
)

// NodePrefix binary prefixes for node types
var (
	LeafPrefix      = []byte{0}
	BranchPrefix    = []byte{1}
	ExtensionPrefix = []byte{2}
)

// Nibble utilities
type Nibble struct{}

func (Nibble) FromHexString(hexStr string) ([]int, error) {
	nibbles := make([]int, len(hexStr))
	for i, char := range hexStr {
		// Convert single hex character to nibble value
		var nibbleValue int
		if char >= '0' && char <= '9' {
			nibbleValue = int(char - '0')
		} else if char >= 'a' && char <= 'f' {
			nibbleValue = int(char - 'a' + 10)
		} else if char >= 'A' && char <= 'F' {
			nibbleValue = int(char - 'A' + 10)
		} else {
			return nil, fmt.Errorf("invalid hex character: %c", char)
		}
		nibbles[i] = nibbleValue
	}
	return nibbles, nil
}

func (Nibble) SequenceEquals(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func (Nibble) SequenceDrop(sequence []int, count int) []int {
	if count >= len(sequence) {
		return []int{}
	}
	return sequence[count:]
}

// JsonBinaryHasher computes hashes with RFC 8785 canonicalization
type JsonBinaryHasher struct{}

func (JsonBinaryHasher) ComputeDigest(jsonObject interface{}, prefixBytes []byte) (string, error) {
	// Marshal to JSON first
	jsonBytes, err := json.Marshal(jsonObject)
	if err != nil {
		return "", fmt.Errorf("failed to marshal JSON: %v", err)
	}

	// Canonicalize JSON according to RFC 8785
	canonicalJSON, err := jsoncanonicalizer.Transform(jsonBytes)
	if err != nil {
		return "", fmt.Errorf("failed to canonicalize JSON: %v", err)
	}

	// Combine prefix and JSON bytes
	combined := append(prefixBytes, canonicalJSON...)

	// Compute SHA-256 hash
	hash := sha256.Sum256(combined)
	return hex.EncodeToString(hash[:]), nil
}

// VerificationResult represents the result of verification
type VerificationResult struct {
	Success       bool
	Error         error
	RemainingPath []int
	ChildDigest   string
}

// MerklePatriciaVerifier verifies inclusion proofs
type MerklePatriciaVerifier struct {
	RootHash string
	nibble   Nibble
	hasher   JsonBinaryHasher
}

// NewVerifier creates a new verifier instance
func NewVerifier(rootHash string) *MerklePatriciaVerifier {
	return &MerklePatriciaVerifier{
		RootHash: rootHash,
		nibble:   Nibble{},
		hasher:   JsonBinaryHasher{},
	}
}

// Verify verifies a Merkle Patricia inclusion proof
func (v *MerklePatriciaVerifier) Verify(proof map[string]interface{}) VerificationResult {
	// Parse the path as nibbles
	pathStr, ok := proof["path"].(string)
	if !ok {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidWitness", Message: "Missing or invalid path"},
		}
	}

	pathNibbles, err := v.nibble.FromHexString(pathStr)
	if err != nil {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidPath", Message: err.Error()},
		}
	}

	// Get witness array
	witnessRaw, ok := proof["witness"].([]interface{})
	if !ok {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidWitness", Message: "Missing or invalid witness"},
		}
	}

	// Process witness in reverse order (from root to leaf)
	witness := make([]map[string]interface{}, len(witnessRaw))
	for i, w := range witnessRaw {
		witness[len(witnessRaw)-1-i] = w.(map[string]interface{})
	}

	// Start verification from root
	currentDigest := v.RootHash
	remainingPath := pathNibbles
	index := 0

	for index < len(witness) {
		commitment := witness[index]

		nodeType, ok := commitment["type"].(string)
		if !ok {
			return VerificationResult{
				Success: false,
				Error:   VerificationError{Type: "InvalidWitness", Message: "Missing commitment type"},
			}
		}

		contents, ok := commitment["contents"].(map[string]interface{})
		if !ok {
			return VerificationResult{
				Success: false,
				Error:   VerificationError{Type: "InvalidWitness", Message: "Missing commitment contents"},
			}
		}

		switch nodeType {
		case NodeTypeLeaf:
			// Leaf must be the final node
			if index != len(witness)-1 {
				return VerificationResult{
					Success: false,
					Error:   VerificationError{Type: "InvalidWitness", Message: "Leaf must be the final commitment"},
				}
			}
			return v.verifyLeaf(contents, currentDigest, remainingPath)

		case NodeTypeExtension:
			result := v.verifyExtension(contents, currentDigest, remainingPath)
			if !result.Success {
				return result
			}
			currentDigest = contents["childDigest"].(string)
			remainingPath = result.RemainingPath
			index++

		case NodeTypeBranch:
			result := v.verifyBranch(contents, currentDigest, remainingPath)
			if !result.Success {
				return result
			}
			currentDigest = result.ChildDigest
			remainingPath = result.RemainingPath
			index++

		default:
			return VerificationResult{
				Success: false,
				Error:   VerificationError{Type: "InvalidWitness", Message: fmt.Sprintf("Unknown commitment type: %s", nodeType)},
			}
		}
	}

	return VerificationResult{
		Success: false,
		Error:   VerificationError{Type: "InvalidWitness", Message: "Proof verification incomplete"},
	}
}

func (v *MerklePatriciaVerifier) verifyLeaf(nodeCommit map[string]interface{}, currentDigest string, remainingPath []int) VerificationResult {
	// Compute the expected digest for this leaf commitment
	commitmentJSON := map[string]interface{}{
		"remaining":  nodeCommit["remaining"],
		"dataDigest": nodeCommit["dataDigest"],
	}

	computedDigest, err := v.hasher.ComputeDigest(commitmentJSON, LeafPrefix)
	if err != nil {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: err.Error()},
		}
	}

	// Verify digest matches
	if computedDigest != currentDigest {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: "Invalid leaf commitment digest"},
		}
	}

	// Parse remaining path from commitment
	remainingStr, ok := nodeCommit["remaining"].(string)
	if !ok {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: "Invalid remaining path in leaf"},
		}
	}

	commitmentRemaining, err := v.nibble.FromHexString(remainingStr)
	if err != nil {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: err.Error()},
		}
	}

	// Verify remaining path matches
	if !v.nibble.SequenceEquals(remainingPath, commitmentRemaining) {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: "Path mismatch at leaf"},
		}
	}

	return VerificationResult{Success: true}
}

func (v *MerklePatriciaVerifier) verifyExtension(nodeCommit map[string]interface{}, currentDigest string, remainingPath []int) VerificationResult {
	// Compute the expected digest for this extension commitment
	commitmentJSON := map[string]interface{}{
		"shared":      nodeCommit["shared"],
		"childDigest": nodeCommit["childDigest"],
	}

	computedDigest, err := v.hasher.ComputeDigest(commitmentJSON, ExtensionPrefix)
	if err != nil {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: err.Error()},
		}
	}

	// Verify digest matches
	if computedDigest != currentDigest {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: "Invalid extension commitment digest"},
		}
	}

	// Parse shared path
	sharedStr, ok := nodeCommit["shared"].(string)
	if !ok {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: "Invalid shared path in extension"},
		}
	}

	sharedNibbles, err := v.nibble.FromHexString(sharedStr)
	if err != nil {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidPath", Message: err.Error()},
		}
	}

	// Verify path starts with shared prefix
	for i, nibble := range sharedNibbles {
		if i >= len(remainingPath) || remainingPath[i] != nibble {
			return VerificationResult{
				Success: false,
				Error:   VerificationError{Type: "InvalidPath", Message: "Path does not match extension shared prefix"},
			}
		}
	}

	// Update remaining path
	newRemainingPath := v.nibble.SequenceDrop(remainingPath, len(sharedNibbles))

	return VerificationResult{
		Success:       true,
		RemainingPath: newRemainingPath,
	}
}

func (v *MerklePatriciaVerifier) verifyBranch(nodeCommit map[string]interface{}, currentDigest string, remainingPath []int) VerificationResult {
	if len(remainingPath) == 0 {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidPath", Message: "No remaining path at branch"},
		}
	}

	// Get next nibble in path
	nextNibble := remainingPath[0]
	nibbleKey := fmt.Sprintf("%x", nextNibble)

	// Check if branch has path for this nibble
	pathsDigest, ok := nodeCommit["pathsDigest"].(map[string]interface{})
	if !ok {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: "Invalid pathsDigest in branch"},
		}
	}

	childDigest, ok := pathsDigest[nibbleKey].(string)
	if !ok {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidPath", Message: fmt.Sprintf("Path not found in branch: %s", nibbleKey)},
		}
	}

	// Compute the expected digest for this branch commitment
	commitmentJSON := map[string]interface{}{
		"pathsDigest": pathsDigest,
	}

	computedDigest, err := v.hasher.ComputeDigest(commitmentJSON, BranchPrefix)
	if err != nil {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: err.Error()},
		}
	}

	// Verify digest matches
	if computedDigest != currentDigest {
		return VerificationResult{
			Success: false,
			Error:   VerificationError{Type: "InvalidNodeCommitment", Message: "Invalid branch commitment digest"},
		}
	}

	// Update remaining path
	newRemainingPath := remainingPath[1:]

	return VerificationResult{
		Success:       true,
		ChildDigest:   childDigest,
		RemainingPath: newRemainingPath,
	}
}

// VerifyProofFromFile loads and verifies a proof from a JSON file
func VerifyProofFromFile(filepath string) (VerificationResult, error) {
	data, err := ioutil.ReadFile(filepath)
	if err != nil {
		return VerificationResult{}, err
	}

	var proofData map[string]interface{}
	err = json.Unmarshal(data, &proofData)
	if err != nil {
		return VerificationResult{}, err
	}

	rootHash, ok := proofData["rootHash"].(string)
	if !ok {
		return VerificationResult{}, fmt.Errorf("missing rootHash in proof file")
	}

	proof, ok := proofData["proof"].(map[string]interface{})
	if !ok {
		return VerificationResult{}, fmt.Errorf("missing proof in proof file")
	}

	verifier := NewVerifier(rootHash)
	return verifier.Verify(proof), nil
}

func main() {
	// Try to load test proof if available
	execPath, _ := os.Executable()
	testFile := filepath.Join(filepath.Dir(execPath), "..", "test-proof.json")
	
	// Also try relative path
	if _, err := os.Stat(testFile); os.IsNotExist(err) {
		testFile = "../test-proof.json"
	}
	
	if _, err := os.Stat(testFile); err == nil {
		fmt.Println("Verifying test proof...")
		result, err := VerifyProofFromFile(testFile)
		if err != nil {
			fmt.Printf("✗ Failed to load proof: %v\n", err)
			os.Exit(1)
		}
		
		if result.Success {
			fmt.Println("✓ Proof is valid!")
		} else {
			fmt.Printf("✗ Proof is invalid: %v\n", result.Error)
			os.Exit(1)
		}
	} else {
		fmt.Println("Test proof file not found")
	}
}