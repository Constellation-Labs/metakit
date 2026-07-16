const crypto = require('crypto');
const canonicalize = require('canonicalize');

/**
 * Nibble utilities - handles hex character conversions
 */
class Nibble {
  static fromHexString(hexString) {
    const nibbles = [];
    for (const char of hexString) {
      const value = parseInt(char, 16);
      if (isNaN(value)) {
        throw new Error(`Invalid hex character: ${char}`);
      }
      nibbles.push(value);
    }
    return nibbles;
  }

  static fromHash(hash) {
    return Nibble.fromHexString(hash);
  }

  static sequenceEquals(a, b) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
      if (a[i] !== b[i]) return false;
    }
    return true;
  }

  static sequenceDrop(sequence, count) {
    return sequence.slice(count);
  }
}

/**
 * Hash computation for JSON objects with binary prefixes
 */
class JsonBinaryHasher {
  static computeDigest(jsonObject, prefixBytes = []) {
    // Use RFC 8785 compliant canonicalization
    const canonicalJson = canonicalize(jsonObject);
    const jsonBytes = Buffer.from(canonicalJson, 'utf8');
    
    // Combine prefix and JSON bytes
    const combined = Buffer.concat([
      Buffer.from(prefixBytes),
      jsonBytes
    ]);
    
    // Compute SHA-256 hash
    const hash = crypto.createHash('sha256');
    hash.update(combined);
    return hash.digest('hex');
  }
}

/**
 * Node type prefixes for hashing
 */
const NodePrefix = {
  LEAF: [0],
  BRANCH: [1],
  EXTENSION: [2]
};

/**
 * Verification error types
 */
class MerklePatriciaVerificationError extends Error {
  constructor(message, type) {
    super(message);
    this.name = 'MerklePatriciaVerificationError';
    this.type = type;
  }
}

class InvalidWitness extends MerklePatriciaVerificationError {
  constructor(message) {
    super(message, 'InvalidWitness');
  }
}

class InvalidPath extends MerklePatriciaVerificationError {
  constructor(message) {
    super(message, 'InvalidPath');
  }
}

class InvalidNodeCommitment extends MerklePatriciaVerificationError {
  constructor(message) {
    super(message, 'InvalidNodeCommitment');
  }
}

/**
 * Merkle Patricia Verifier - verifies sealed inclusion-or-absence proofs
 *
 * This is THE reference implementation for the sealed
 * `{type: "Inclusion"|"Absence", path, witness}` wire format produced by the
 * Scala `MerklePatriciaProver.provePath` / served by `GET /committed/proof/<key>`.
 * Legacy un-tagged `{path, witness}` proofs are accepted and treated as
 * inclusion (the tagged Inclusion encoding is byte-identical plus the tag).
 */
class MerklePatriciaVerifier {
  constructor(rootHash) {
    this.rootHash = rootHash;
  }

  /**
   * Verify a sealed Merkle Patricia proof, dispatching on the proof-level
   * 'type' tag:
   *   - "Inclusion" (or no tag, the legacy shape) -> inclusion verification
   *   - "Absence" -> absence verification (same fold, different terminal assertion)
   * @param {Object} proof - { type?, path, witness }
   * @returns {Object} - { success: boolean, error?: Error }
   */
  verify(proof) {
    const tag = proof ? proof.type : undefined;
    if (tag === 'Absence') {
      return this.verifyAbsence(proof);
    }
    if (tag === undefined || tag === 'Inclusion') {
      return this.verifyInclusion(proof);
    }
    return { success: false, error: new InvalidWitness(`Unknown proof type: ${tag}`) };
  }

  /**
   * Verify a Merkle Patricia inclusion proof
   * @param {Object} proof - The inclusion proof with 'path' and 'witness' fields
   * @returns {Object} - { success: boolean, error?: Error }
   */
  verifyInclusion(proof) {
    try {
      // Parse the path as nibbles
      const pathNibbles = Nibble.fromHash(proof.path);
      
      // Process witness in reverse order (from root to leaf)
      const witness = [...proof.witness].reverse();
      
      // Start verification from root
      let currentDigest = this.rootHash;
      let remainingPath = pathNibbles;
      let index = 0;

      while (index < witness.length) {
        const commitment = witness[index];
        
        if (!commitment.type || !commitment.contents) {
          throw new InvalidWitness('Invalid commitment structure');
        }

        switch (commitment.type) {
          case 'Leaf':
            return this.verifyLeaf(commitment.contents, currentDigest, remainingPath, index === witness.length - 1);
            
          case 'Extension':
            const extResult = this.verifyExtension(commitment.contents, currentDigest, remainingPath);
            if (!extResult.success) return extResult;
            currentDigest = commitment.contents.childDigest;
            remainingPath = extResult.remainingPath;
            index++;
            break;
            
          case 'Branch':
            const branchResult = this.verifyBranch(commitment.contents, currentDigest, remainingPath);
            if (!branchResult.success) return branchResult;
            currentDigest = branchResult.childDigest;
            remainingPath = branchResult.remainingPath;
            index++;
            break;
            
          default:
            throw new InvalidWitness(`Unknown commitment type: ${commitment.type}`);
        }
      }

      throw new InvalidWitness('Proof verification incomplete');

    } catch (error) {
      if (error instanceof MerklePatriciaVerificationError) {
        return { success: false, error };
      }
      return { success: false, error: new InvalidWitness(`Verification failed: ${error.message}`) };
    }
  }

  /**
   * Verify a Merkle Patricia ABSENCE proof: NO value is committed at proof.path
   * under the root. Mirrors the Scala verifier's confirmAbsence: the witness
   * replays the SAME root-first fold as inclusion (digest recomputation per
   * commitment with the node-type prefix, child-digest threading, nibble
   * consumption); the terminal (deepest) commitment must hash to the digest the
   * fold reached AND structurally refuse the next step:
   *
   *   - Branch:    the remaining path is EMPTY (this MPT's branches carry no
   *                value slot, so a path ending at a branch is necessarily
   *                absent) OR pathsDigest lacks the next nibble;
   *   - Extension: 'shared' is NOT a prefix of the remaining path (divergence
   *                mid-edge, including a remaining path shorter than the edge);
   *   - Leaf:      'remaining' differs from the remaining path (a different key
   *                occupies the position).
   *
   * A terminal that could continue (or a Leaf that MATCHES) proves nothing.
   * @param {Object} proof - The absence proof with 'path' and 'witness' fields
   * @returns {Object} - { success: boolean, error?: Error }
   */
  verifyAbsence(proof) {
    try {
      const pathNibbles = Nibble.fromHash(proof.path);
      const witness = [...proof.witness].reverse();

      if (witness.length === 0) {
        return { success: false, error: new InvalidWitness('Empty witness') };
      }

      let currentDigest = this.rootHash;
      let remainingPath = pathNibbles;

      for (let index = 0; index < witness.length; index++) {
        const commitment = witness[index];

        if (!commitment.type || !commitment.contents) {
          throw new InvalidWitness('Invalid commitment structure');
        }

        // The deepest commitment is the divergence terminal.
        if (index === witness.length - 1) {
          return this.verifyAbsenceTerminal(commitment, currentDigest, remainingPath);
        }

        switch (commitment.type) {
          case 'Extension': {
            const extResult = this.verifyExtension(commitment.contents, currentDigest, remainingPath);
            if (!extResult.success) return extResult;
            currentDigest = commitment.contents.childDigest;
            remainingPath = extResult.remainingPath;
            break;
          }

          case 'Branch': {
            const branchResult = this.verifyBranch(commitment.contents, currentDigest, remainingPath);
            if (!branchResult.success) return branchResult;
            currentDigest = branchResult.childDigest;
            remainingPath = branchResult.remainingPath;
            break;
          }

          case 'Leaf':
            // A leaf has no child to continue into - it can only be terminal.
            return { success: false, error: new InvalidWitness('Leaf before end of witness') };

          default:
            throw new InvalidWitness(`Unknown commitment type: ${commitment.type}`);
        }
      }

      throw new InvalidWitness('Proof verification incomplete');

    } catch (error) {
      if (error instanceof MerklePatriciaVerificationError) {
        return { success: false, error };
      }
      return { success: false, error: new InvalidWitness(`Verification failed: ${error.message}`) };
    }
  }

  /**
   * The absence terminal assertion: bind the terminal commitment to the digest
   * the fold reached (same prefixed hashing as every other step), then require
   * it to structurally refuse the next step of the remaining path.
   */
  verifyAbsenceTerminal(commitment, currentDigest, remainingPath) {
    let commitmentJson;
    let prefix;

    switch (commitment.type) {
      case 'Leaf':
        commitmentJson = {
          remaining: commitment.contents.remaining,
          dataDigest: commitment.contents.dataDigest
        };
        prefix = NodePrefix.LEAF;
        break;

      case 'Branch':
        commitmentJson = {
          pathsDigest: commitment.contents.pathsDigest
        };
        prefix = NodePrefix.BRANCH;
        break;

      case 'Extension':
        commitmentJson = {
          shared: commitment.contents.shared,
          childDigest: commitment.contents.childDigest
        };
        prefix = NodePrefix.EXTENSION;
        break;

      default:
        return { success: false, error: new InvalidWitness(`Unknown commitment type: ${commitment.type}`) };
    }

    const computedDigest = JsonBinaryHasher.computeDigest(commitmentJson, prefix);
    if (computedDigest !== currentDigest) {
      return { success: false, error: new InvalidNodeCommitment('Invalid terminal commitment digest') };
    }

    let witnessesAbsence;
    if (commitment.type === 'Branch') {
      const pathsDigest = commitment.contents.pathsDigest || {};
      witnessesAbsence = remainingPath.length === 0 || !pathsDigest[remainingPath[0].toString(16)];
    } else if (commitment.type === 'Extension') {
      const sharedNibbles = Nibble.fromHexString(commitment.contents.shared);
      let sharedIsPrefix = sharedNibbles.length <= remainingPath.length;
      for (let i = 0; sharedIsPrefix && i < sharedNibbles.length; i++) {
        if (remainingPath[i] !== sharedNibbles[i]) sharedIsPrefix = false;
      }
      witnessesAbsence = !sharedIsPrefix;
    } else {
      const commitmentRemaining = Nibble.fromHexString(commitment.contents.remaining);
      witnessesAbsence = !Nibble.sequenceEquals(remainingPath, commitmentRemaining);
    }

    if (!witnessesAbsence) {
      return { success: false, error: new InvalidPath('Terminal commitment does not witness absence of the path') };
    }

    return { success: true };
  }

  verifyLeaf(nodeCommit, currentDigest, remainingPath, isFinal) {
    // Verify this is the final node in the witness
    if (!isFinal) {
      return { success: false, error: new InvalidWitness('Leaf must be the final commitment') };
    }

    // Compute the expected digest for this leaf commitment
    const commitmentJson = {
      remaining: nodeCommit.remaining,
      dataDigest: nodeCommit.dataDigest
    };
    
    const computedDigest = JsonBinaryHasher.computeDigest(commitmentJson, NodePrefix.LEAF);
    
    // Verify digest matches
    if (computedDigest !== currentDigest) {
      return { success: false, error: new InvalidNodeCommitment('Invalid leaf commitment digest') };
    }
    
    // Parse remaining path from commitment
    const commitmentRemaining = Nibble.fromHexString(nodeCommit.remaining);
    
    // Verify remaining path matches
    if (!Nibble.sequenceEquals(remainingPath, commitmentRemaining)) {
      return { success: false, error: new InvalidNodeCommitment('Path mismatch at leaf') };
    }
    
    return { success: true };
  }

  verifyExtension(nodeCommit, currentDigest, remainingPath) {
    // Compute the expected digest for this extension commitment
    const commitmentJson = {
      shared: nodeCommit.shared,
      childDigest: nodeCommit.childDigest
    };
    
    const computedDigest = JsonBinaryHasher.computeDigest(commitmentJson, NodePrefix.EXTENSION);
    
    // Verify digest matches
    if (computedDigest !== currentDigest) {
      return { success: false, error: new InvalidNodeCommitment('Invalid extension commitment digest') };
    }
    
    // Parse shared path
    const sharedNibbles = Nibble.fromHexString(nodeCommit.shared);
    
    // Verify path starts with shared prefix
    for (let i = 0; i < sharedNibbles.length; i++) {
      if (i >= remainingPath.length || remainingPath[i] !== sharedNibbles[i]) {
        return { success: false, error: new InvalidPath('Path does not match extension shared prefix') };
      }
    }
    
    // Update remaining path
    const newRemainingPath = Nibble.sequenceDrop(remainingPath, sharedNibbles.length);
    
    return { 
      success: true,
      remainingPath: newRemainingPath
    };
  }

  verifyBranch(nodeCommit, currentDigest, remainingPath) {
    if (remainingPath.length === 0) {
      return { success: false, error: new InvalidPath('No remaining path at branch') };
    }
    
    // Get next nibble in path
    const nextNibble = remainingPath[0];
    const nibbleKey = nextNibble.toString(16);
    
    // Check if branch has path for this nibble
    if (!nodeCommit.pathsDigest || !nodeCommit.pathsDigest[nibbleKey]) {
      return { success: false, error: new InvalidPath(`Path not found in branch: ${nibbleKey}`) };
    }
    
    // Compute the expected digest for this branch commitment
    const commitmentJson = {
      pathsDigest: nodeCommit.pathsDigest
    };
    
    const computedDigest = JsonBinaryHasher.computeDigest(commitmentJson, NodePrefix.BRANCH);
    
    // Verify digest matches
    if (computedDigest !== currentDigest) {
      return { success: false, error: new InvalidNodeCommitment('Invalid branch commitment digest') };
    }
    
    // Get child digest for next step
    const childDigest = nodeCommit.pathsDigest[nibbleKey];
    
    // Update remaining path
    const newRemainingPath = remainingPath.slice(1);
    
    return {
      success: true,
      childDigest,
      remainingPath: newRemainingPath
    };
  }
}

module.exports = {
  MerklePatriciaVerifier,
  MerklePatriciaVerificationError,
  InvalidWitness,
  InvalidPath,
  InvalidNodeCommitment,
  JsonBinaryHasher,
  Nibble
};