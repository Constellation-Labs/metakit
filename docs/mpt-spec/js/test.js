const fs = require('fs');
const path = require('path');

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

// Test sealed-proof dispatch and absence verification (synthetic)
testSection('Absence Verification (sealed format)');
{
  // Branch terminal lacking the queried nibble: absent.
  const branchCommitment = {
    pathsDigest: {
      "a": "86c503bdd9a920e37517bacb40901ae2f62c3f949d9fae6603a22a3e7a7d23f9",
      "b": "9e972d00c5b409546e3ee011f0272259ff443efbb2902db7a7b8d02a2f4e814b"
    }
  };
  const branchDigest = JsonBinaryHasher.computeDigest(branchCommitment, [1]);
  const verifier = new MerklePatriciaVerifier(branchDigest);

  const absent = {
    type: "Absence",
    path: "c3",
    witness: [{ type: "Branch", contents: branchCommitment }]
  };
  assert(verifier.verify(absent).success === true, 'Branch missing-nibble absence proof verifies');

  // The SAME witness cannot prove absence of a PRESENT nibble.
  const present = { ...absent, path: "a1" };
  const presentResult = verifier.verify(present);
  assert(presentResult.success === false, 'Absence claim for a present nibble is rejected');
  assert(presentResult.error instanceof InvalidPath, 'Rejection carries InvalidPath');

  // Path exhausted at a branch: this MPT has no branch value slot, so absent.
  const exhausted = { ...absent, path: "" };
  assert(verifier.verify(exhausted).success === true, 'Path-exhausted-at-branch absence proof verifies');

  // Tampered terminal: digest binding must fail.
  const tampered = {
    type: "Absence",
    path: "c3",
    witness: [{ type: "Branch", contents: { pathsDigest: { "f": branchCommitment.pathsDigest.a } } }]
  };
  const tamperedResult = verifier.verify(tampered);
  assert(tamperedResult.success === false, 'Tampered absence terminal is rejected');
  assert(tamperedResult.error instanceof InvalidNodeCommitment, 'Rejection carries InvalidNodeCommitment');

  // Other-leaf terminal: a different key occupies the position.
  const leafCommitment = { remaining: "abcd", dataDigest: "11".repeat(32) };
  const leafDigest = JsonBinaryHasher.computeDigest(leafCommitment, [0]);
  const leafVerifier = new MerklePatriciaVerifier(leafDigest);
  const otherLeaf = { type: "Absence", path: "abce", witness: [{ type: "Leaf", contents: leafCommitment }] };
  assert(leafVerifier.verify(otherLeaf).success === true, 'Other-leaf absence proof verifies');
  const matchingLeaf = { ...otherLeaf, path: "abcd" };
  assert(leafVerifier.verify(matchingLeaf).success === false, 'A MATCHING leaf proves nothing (rejected)');

  // Extension terminal whose shared run diverges from the remaining path.
  const extCommitment = { shared: "abc", childDigest: "22".repeat(32) };
  const extDigest = JsonBinaryHasher.computeDigest(extCommitment, [2]);
  const extVerifier = new MerklePatriciaVerifier(extDigest);
  const diverged = { type: "Absence", path: "ab12", witness: [{ type: "Extension", contents: extCommitment }] };
  assert(extVerifier.verify(diverged).success === true, 'Extension-divergence absence proof verifies');
  const followable = { ...diverged, path: "abcd" };
  assert(extVerifier.verify(followable).success === false, 'A followable extension proves nothing (rejected)');

  // Unknown proof-level tag.
  const unknownTag = verifier.verify({ type: "Nonsense", path: "c3", witness: absent.witness });
  assert(unknownTag.success === false, 'Unknown proof type tag is rejected');
}

// Legacy fixture: un-tagged {path, witness} still verifies as inclusion
testSection('Fixture: test-proof.json (legacy un-tagged inclusion)');
{
  const fixture = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'test-proof.json'), 'utf8'));
  const verifier = new MerklePatriciaVerifier(fixture.rootHash);
  assert(verifier.verify(fixture.proof).success === true, 'Legacy un-tagged inclusion fixture verifies');
  const tagged = { type: "Inclusion", ...fixture.proof };
  assert(verifier.verify(tagged).success === true, 'Same fixture with the Inclusion tag verifies');
}

// Sealed fixtures: chain-derived, byte-pinned by the Scala MptSpecFixtureSuite
testSection('Fixture: test-sealed-proofs.json (sealed Inclusion/Absence)');
{
  const fixture = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'test-sealed-proofs.json'), 'utf8'));
  for (const testCase of fixture.cases) {
    const verifier = new MerklePatriciaVerifier(testCase.rootHash);
    const result = verifier.verify(testCase.proof);
    assert(result.success === true,
      `${testCase.name} verifies${result.success ? '' : ` (${result.error.message})`}`);
  }

  // Record binding: the Inclusion leaf's dataDigest is sha256(JCS(record)).
  const inclusion = fixture.cases.find(c => c.proof.type === 'Inclusion');
  const leafContents = inclusion.proof.witness[0].contents;
  assert(JsonBinaryHasher.computeDigest(inclusion.record) === leafContents.dataDigest,
    'Inclusion leaf dataDigest equals sha256(JCS(record))');

  // Cross-checks: sealed proofs must not verify against the wrong root or arm.
  const absence = fixture.cases.find(c => c.name === 'absence-branch-missing-nibble');
  const wrongRoot = new MerklePatriciaVerifier('ff'.repeat(32));
  assert(wrongRoot.verify(absence.proof).success === false, 'Absence proof is rejected against a tampered root');
  const relabeled = { ...inclusion.proof, type: "Absence" };
  const relabeledResult = new MerklePatriciaVerifier(inclusion.rootHash).verify(relabeled);
  assert(relabeledResult.success === false, 'Inclusion witness relabeled as Absence is rejected');
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