const { 
  MerklePatriciaVerifier, 
  JsonBinaryHasher,
  Nibble,
  InvalidWitness,
  InvalidPath,
  InvalidNodeCommitment 
} = require('./merkle-patricia-verifier');

let testsPassed = 0;
let testsFailed = 0;

function assert(condition, message) {
  if (condition) {
    testsPassed++;
    console.log(`  ✓ ${message}`);
  } else {
    testsFailed++;
    console.log(`  ✗ ${message}`);
  }
}

function testSection(name) {
  console.log(`\n${name}`);
  console.log('='.repeat(name.length));
}

// Test Nibble utilities
testSection('Nibble Utilities');
{
  const hexString = "a1b2c3";
  const nibbles = Nibble.fromHexString(hexString);
  assert(nibbles.length === 6, 'fromHexString creates correct number of nibbles');
  assert(nibbles[0] === 10, 'First nibble is correct (a = 10)');
  assert(nibbles[1] === 1, 'Second nibble is correct');
  
  const hashString = "0123456789abcdef";
  const hashNibbles = Nibble.fromHash(hashString);
  assert(hashNibbles.length === 16, 'fromHash creates correct number of nibbles');
  
  const seq1 = [1, 2, 3];
  const seq2 = [1, 2, 3];
  const seq3 = [1, 2, 4];
  assert(Nibble.sequenceEquals(seq1, seq2), 'sequenceEquals returns true for equal sequences');
  assert(!Nibble.sequenceEquals(seq1, seq3), 'sequenceEquals returns false for different sequences');
  
  const dropped = Nibble.sequenceDrop([1, 2, 3, 4, 5], 2);
  assert(dropped.length === 3 && dropped[0] === 3, 'sequenceDrop removes correct number of elements');
}

// Test JsonBinaryHasher
testSection('JsonBinaryHasher');
{
  const obj1 = { b: 2, a: 1 };
  const obj2 = { a: 1, b: 2 };
  const hash1 = JsonBinaryHasher.computeDigest(obj1);
  const hash2 = JsonBinaryHasher.computeDigest(obj2);
  assert(hash1 === hash2, 'Object key sorting produces consistent hashes');
  
  const objWithPrefix = { test: "data" };
  const hashNoPrefix = JsonBinaryHasher.computeDigest(objWithPrefix);
  const hashWithPrefix = JsonBinaryHasher.computeDigest(objWithPrefix, [1, 2, 3]);
  assert(hashNoPrefix !== hashWithPrefix, 'Different prefixes produce different hashes');
  assert(hashNoPrefix.length === 64, 'Hash is SHA-256 (64 hex chars)');
}

// Test proof verification with synthetic but valid structure
testSection('Proof Verification Structure');
{
  // Create a synthetic proof with proper structure
  const leafCommitment = {
    remaining: "abc567",
    dataDigest: "1234567890123456789012345678901234567890123456789012345678901234"
  };
  
  // Calculate what the leaf digest should be
  const leafDigest = JsonBinaryHasher.computeDigest(leafCommitment, [0]);
  
  const proof = {
    path: "abc567",  // Path must exactly match the leaf's remaining for a leaf-only proof
    witness: [
      {
        type: "Leaf",
        contents: leafCommitment
      }
    ]
  };
  
  const verifier = new MerklePatriciaVerifier(leafDigest);
  const result = verifier.verify(proof);
  assert(result.success === true, 'Valid leaf-only proof verifies successfully');
}

// Test error cases
testSection('Error Handling');
{
  const verifier = new MerklePatriciaVerifier("root123");
  
  // Test invalid witness structure
  const invalidProof1 = {
    path: "abc",
    witness: [{ invalid: "structure" }]
  };
  const result1 = verifier.verify(invalidProof1);
  assert(!result1.success, 'Invalid witness structure is rejected');
  assert(result1.error instanceof InvalidWitness, 'Invalid witness produces correct error type');
  
  // Test path mismatch
  const leafCommitment = {
    remaining: "123",
    dataDigest: "abcd"
  };
  const leafDigest = JsonBinaryHasher.computeDigest(leafCommitment, [0]);
  
  const invalidProof2 = {
    path: "abc456",  // Path doesn't match leaf remaining
    witness: [
      {
        type: "Leaf",
        contents: leafCommitment
      }
    ]
  };
  const verifier2 = new MerklePatriciaVerifier(leafDigest);
  const result2 = verifier2.verify(invalidProof2);
  assert(!result2.success, 'Path mismatch is detected');
  assert(result2.error instanceof InvalidNodeCommitment, 'Path mismatch produces correct error type');
}

// Test extension verification
testSection('Extension Verification');
{
  // Create an extension commitment
  const childDigest = "child1234567890123456789012345678901234567890123456789012345678";
  const extensionCommitment = {
    shared: "ab",
    childDigest: childDigest
  };
  const extensionDigest = JsonBinaryHasher.computeDigest(extensionCommitment, [2]);
  
  // Create a leaf that follows the extension
  const leafCommitment = {
    remaining: "cd",
    dataDigest: "data5678901234567890123456789012345678901234567890123456789012"
  };
  const leafDigest = JsonBinaryHasher.computeDigest(leafCommitment, [0]);
  
  // Update extension to point to the leaf
  extensionCommitment.childDigest = leafDigest;
  const correctedExtensionDigest = JsonBinaryHasher.computeDigest(extensionCommitment, [2]);
  
  const proof = {
    path: "abcd",  // Matches extension shared + leaf remaining
    witness: [
      {
        type: "Leaf",
        contents: leafCommitment
      },
      {
        type: "Extension",
        contents: extensionCommitment
      }
    ]
  };
  
  const verifier = new MerklePatriciaVerifier(correctedExtensionDigest);
  const result = verifier.verify(proof);
  assert(result.success === true, 'Valid extension->leaf proof verifies successfully');
}

// Test branch verification
testSection('Branch Verification');
{
  // Create a branch with multiple paths
  const leafDigest = "leaf1234567890123456789012345678901234567890123456789012345678";
  const branchCommitment = {
    pathsDigest: {
      "0": "path01234567890123456789012345678901234567890123456789012345678",
      "5": leafDigest,  // Path we'll follow
      "a": "patha1234567890123456789012345678901234567890123456789012345678"
    }
  };
  const branchDigest = JsonBinaryHasher.computeDigest(branchCommitment, [1]);
  
  // Create a leaf at path 5
  const leafCommitment = {
    remaining: "678",
    dataDigest: "data9876543210987654321098765432109876543210987654321098765432"
  };
  const actualLeafDigest = JsonBinaryHasher.computeDigest(leafCommitment, [0]);
  
  // Update branch to point to actual leaf
  branchCommitment.pathsDigest["5"] = actualLeafDigest;
  const correctedBranchDigest = JsonBinaryHasher.computeDigest(branchCommitment, [1]);
  
  const proof = {
    path: "5678",  // Branch nibble 5 + leaf remaining
    witness: [
      {
        type: "Leaf",
        contents: leafCommitment
      },
      {
        type: "Branch",
        contents: branchCommitment
      }
    ]
  };
  
  const verifier = new MerklePatriciaVerifier(correctedBranchDigest);
  const result = verifier.verify(proof);
  assert(result.success === true, 'Valid branch->leaf proof verifies successfully');
}

// Summary
console.log('\n' + '='.repeat(40));
console.log(`Tests passed: ${testsPassed}`);
console.log(`Tests failed: ${testsFailed}`);

if (testsFailed === 0) {
  console.log('\n✓ All tests passed!');
  process.exit(0);
} else {
  console.log('\n✗ Some tests failed');
  process.exit(1);
}