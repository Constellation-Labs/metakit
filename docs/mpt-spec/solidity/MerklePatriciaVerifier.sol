// SPDX-License-Identifier: MIT
pragma solidity ^0.8.19;

/**
 * @title MerklePatriciaVerifier
 * @notice Verifies Merkle Patricia Trie inclusion proofs compatible with Metakit
 * @dev Uses SHA-256 hashing with binary prefixes to match the Scala implementation
 */
contract MerklePatriciaVerifier {

    // Node type identifiers matching the JSON structure
    enum NodeType {
        Leaf,
        Branch,
        Extension
    }

    // Binary prefixes for node type hashing
    bytes1 constant LEAF_PREFIX = 0x00;
    bytes1 constant BRANCH_PREFIX = 0x01;
    bytes1 constant EXTENSION_PREFIX = 0x02;

    // Custom errors for gas efficiency
    error InvalidWitness(string reason);
    error InvalidPath(string reason);
    error InvalidNodeCommitment(string reason);
    error InvalidProofLength();

    /**
     * @notice Commitment structure for witness nodes
     */
    struct Commitment {
        NodeType nodeType;
        bytes data; // Encoded node-specific data
    }

    /**
     * @notice Inclusion proof structure
     */
    struct InclusionProof {
        bytes path;          // The full path being proven (can be longer than 32 bytes)
        Commitment[] witness; // Array of commitments from leaf to root
    }

    /**
     * @notice Fallback function that handles verify calls
     * @dev Decodes calldata and calls verify function, returns result as bytes
     */
    fallback() external payable {
        // Check if this is a verify call (function selector: 0xfd132ae1)
        if (msg.data.length >= 4) {
            bytes4 selector = bytes4(msg.data[:4]);
            if (selector == bytes4(keccak256("verify(bytes32,(bytes,(uint8,bytes)[]))"))) {
                // Decode and call verify
                (bytes32 rootHash, InclusionProof memory proof) = abi.decode(msg.data[4:], (bytes32, InclusionProof));
                bool result = verify(rootHash, proof);

                // Return result as bytes
                bytes memory returnData = abi.encode(result);
                assembly {
                    return(add(returnData, 0x20), mload(returnData))
                }
            }
        }

        // If not a verify call, revert
        revert("Invalid function call");
    }

    /**
     * @notice Receive function to accept ETH
     */
    receive() external payable {}

    /**
     * @notice Verify an inclusion proof against a root hash
     * @param rootHash The root hash of the Merkle Patricia Trie
     * @param proof The inclusion proof to verify
     * @return bool True if the proof is valid
     */
    function verify(
        bytes32 rootHash,
        InclusionProof memory proof
    ) public pure returns (bool) {
        if (proof.witness.length == 0) {
            revert InvalidProofLength();
        }

        // Convert path to nibbles (each byte becomes 2 nibbles)
        bytes memory pathNibbles = _hashToNibbles(proof.path);

        // Process witness in reverse order (from root to leaf)
        bytes32 currentDigest = rootHash;
        uint256 pathOffset = 0;

        for (uint256 i = proof.witness.length; i > 0; i--) {
            Commitment memory commitment = proof.witness[i - 1];

            if (commitment.nodeType == NodeType.Leaf) {
                // Must be the last commitment (first in original order)
                if (i != 1) {
                    revert InvalidWitness("Leaf must be final commitment");
                }
                return _verifyLeaf(commitment.data, currentDigest, pathNibbles, pathOffset);
            }
            else if (commitment.nodeType == NodeType.Extension) {
                (bytes32 childDigest, uint256 newOffset) = _verifyExtension(
                    commitment.data,
                    currentDigest,
                    pathNibbles,
                    pathOffset
                );
                currentDigest = childDigest;
                pathOffset = newOffset;
            }
            else if (commitment.nodeType == NodeType.Branch) {
                (bytes32 childDigest, uint256 newOffset) = _verifyBranch(
                    commitment.data,
                    currentDigest,
                    pathNibbles,
                    pathOffset
                );
                currentDigest = childDigest;
                pathOffset = newOffset;
            }
        }

        revert InvalidWitness("Proof verification incomplete");
    }

    /**
     * @notice Verify a leaf node commitment
     */
    function _verifyLeaf(
        bytes memory nodeData,
        bytes32 currentDigest,
        bytes memory pathNibbles,
        uint256 pathOffset
    ) private pure returns (bool) {
        // Decode leaf data: [remaining_length][remaining_nibbles][dataDigest]
        // First byte is the length of remaining nibbles
        uint8 remainingLength = uint8(nodeData[0]);

        // Extract remaining nibbles and data digest
        bytes memory remaining = new bytes(remainingLength);
        for (uint256 i = 0; i < remainingLength; i++) {
            remaining[i] = nodeData[1 + i];
        }

        bytes32 dataDigest;
        assembly {
            dataDigest := mload(add(nodeData, add(33, remainingLength)))
        }

        // Compute the expected digest for this leaf
        bytes32 computedDigest = _computeLeafDigest(remaining, dataDigest);

        // Verify digest matches
        if (computedDigest != currentDigest) {
            revert InvalidNodeCommitment("Invalid leaf digest");
        }

        // Verify remaining path matches
        uint256 remainingPathLength = pathNibbles.length - pathOffset;
        if (remainingLength != remainingPathLength) {
            revert InvalidPath("Path length mismatch at leaf");
        }

        for (uint256 i = 0; i < remainingLength; i++) {
            if (pathNibbles[pathOffset + i] != remaining[i]) {
                revert InvalidPath("Path mismatch at leaf");
            }
        }

        return true;
    }

    /**
     * @notice Verify an extension node commitment
     */
    function _verifyExtension(
        bytes memory nodeData,
        bytes32 currentDigest,
        bytes memory pathNibbles,
        uint256 pathOffset
    ) private pure returns (bytes32, uint256) {
        // Decode extension data: [shared_length][shared_nibbles][childDigest]
        uint8 sharedLength = uint8(nodeData[0]);

        bytes memory shared = new bytes(sharedLength);
        for (uint256 i = 0; i < sharedLength; i++) {
            shared[i] = nodeData[1 + i];
        }

        bytes32 childDigest;
        assembly {
            childDigest := mload(add(nodeData, add(33, sharedLength)))
        }

        // Compute expected digest
        bytes32 computedDigest = _computeExtensionDigest(shared, childDigest);

        if (computedDigest != currentDigest) {
            revert InvalidNodeCommitment("Invalid extension digest");
        }

        // Verify path starts with shared prefix
        if (pathOffset + sharedLength > pathNibbles.length) {
            revert InvalidPath("Path too short for extension");
        }

        for (uint256 i = 0; i < sharedLength; i++) {
            if (pathNibbles[pathOffset + i] != shared[i]) {
                revert InvalidPath("Path does not match extension");
            }
        }

        return (childDigest, pathOffset + sharedLength);
    }

    /**
     * @notice Verify a branch node commitment
     */
    function _verifyBranch(
        bytes memory nodeData,
        bytes32 currentDigest,
        bytes memory pathNibbles,
        uint256 pathOffset
    ) private pure returns (bytes32, uint256) {
        if (pathOffset >= pathNibbles.length) {
            revert InvalidPath("No remaining path at branch");
        }

        // Get next nibble in path
        uint8 nextNibble = uint8(pathNibbles[pathOffset]);

        // Decode branch data: [num_paths][[nibble][digest]]...
        uint8 numPaths = uint8(nodeData[0]);

        // Find the digest for our nibble
        bytes32 childDigest;
        bool found = false;

        uint256 offset = 1;
        for (uint256 i = 0; i < numPaths; i++) {
            uint8 nibble = uint8(nodeData[offset]);
            bytes32 digest;
            assembly {
                digest := mload(add(nodeData, add(offset, 33)))
            }

            if (nibble == nextNibble) {
                childDigest = digest;
                found = true;
            }

            offset += 33; // 1 byte nibble + 32 bytes digest
        }

        if (!found) {
            revert InvalidPath("Path not found in branch");
        }

        // Compute expected digest for branch
        bytes32 computedDigest = _computeBranchDigest(nodeData);

        if (computedDigest != currentDigest) {
            revert InvalidNodeCommitment("Invalid branch digest");
        }

        return (childDigest, pathOffset + 1);
    }

    /**
     * @notice Compute digest for a leaf node
     */
    function _computeLeafDigest(
        bytes memory remaining,
        bytes32 dataDigest
    ) private pure returns (bytes32) {
        // Create canonical JSON representation
        // {"remaining":"<hex>","dataDigest":"<hex>"}
        bytes memory json = abi.encodePacked(
            '{"dataDigest":"',
            _bytes32ToHex(dataDigest),
            '","remaining":"',
            _bytesToHex(remaining),
            '"}'
        );

        // Hash with leaf prefix
        return sha256(abi.encodePacked(LEAF_PREFIX, json));
    }

    /**
     * @notice Compute digest for an extension node
     */
    function _computeExtensionDigest(
        bytes memory shared,
        bytes32 childDigest
    ) private pure returns (bytes32) {
        // Create canonical JSON representation
        // {"childDigest":"<hex>","shared":"<hex>"}
        bytes memory json = abi.encodePacked(
            '{"childDigest":"',
            _bytes32ToHex(childDigest),
            '","shared":"',
            _bytesToHex(shared),
            '"}'
        );

        // Hash with extension prefix
        return sha256(abi.encodePacked(EXTENSION_PREFIX, json));
    }

    /**
     * @notice Compute digest for a branch node
     */
    function _computeBranchDigest(bytes memory nodeData) private pure returns (bytes32) {
        uint8 numPaths = uint8(nodeData[0]);

        // Build pathsDigest JSON object
        bytes memory pathsJson = '{"pathsDigest":{';

        uint256 offset = 1;
        for (uint256 i = 0; i < numPaths; i++) {
            uint8 nibble = uint8(nodeData[offset]);
            bytes32 digest;
            assembly {
                digest := mload(add(nodeData, add(offset, 33)))
            }

            if (i > 0) pathsJson = abi.encodePacked(pathsJson, ',');

            pathsJson = abi.encodePacked(
                pathsJson,
                '"',
                _nibbleToHex(nibble),
                '":"',
                _bytes32ToHex(digest),
                '"'
            );

            offset += 33;
        }

        pathsJson = abi.encodePacked(pathsJson, '}}');

        // Hash with branch prefix
        return sha256(abi.encodePacked(BRANCH_PREFIX, pathsJson));
    }

    /**
     * @notice Convert bytes to nibbles (each byte becomes 2 nibbles)
     */
    function _hashToNibbles(bytes memory data) private pure returns (bytes memory) {
        bytes memory nibbles = new bytes(data.length * 2);
        for (uint256 i = 0; i < data.length; i++) {
            uint8 b = uint8(data[i]);
            nibbles[i * 2] = bytes1(b >> 4);      // High nibble
            nibbles[i * 2 + 1] = bytes1(b & 0x0f); // Low nibble
        }
        return nibbles;
    }

    /**
     * @notice Convert bytes32 to hex string
     */
    function _bytes32ToHex(bytes32 data) private pure returns (bytes memory) {
        bytes memory hexChars = "0123456789abcdef";
        bytes memory result = new bytes(64);

        for (uint256 i = 0; i < 32; i++) {
            uint8 b = uint8(data[i]);
            result[i * 2] = hexChars[b >> 4];
            result[i * 2 + 1] = hexChars[b & 0x0f];
        }

        return result;
    }

    /**
     * @notice Convert nibbles to hex string
     */
    function _bytesToHex(bytes memory data) private pure returns (bytes memory) {
        bytes memory hexChars = "0123456789abcdef";
        bytes memory result = new bytes(data.length);

        for (uint256 i = 0; i < data.length; i++) {
            result[i] = hexChars[uint8(data[i])];
        }

        return result;
    }

    /**
     * @notice Convert single nibble to hex character
     */
    function _nibbleToHex(uint8 nibble) private pure returns (bytes1) {
        if (nibble < 10) return bytes1(nibble + 0x30); // '0'-'9'
        return bytes1(nibble - 10 + 0x61); // 'a'-'f'
    }
}