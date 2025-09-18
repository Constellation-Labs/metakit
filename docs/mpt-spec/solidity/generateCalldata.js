#!/usr/bin/env node

const { ethers } = require('ethers');
const fs = require('fs');
const path = require('path');

/**
 * Generates complete calldata for calling MerklePatriciaVerifier.verify()
 * This can be used directly in a raw transaction
 */

function generateCalldata(rootHash, proofData) {
    // Helper to convert hex string to nibbles
    function hexToNibbles(hexStr) {
        const nibbles = [];
        for (let i = 0; i < hexStr.length; i++) {
            nibbles.push(parseInt(hexStr[i], 16));
        }
        return nibbles;
    }

    // Encode leaf node data
    function encodeLeaf(remaining, dataDigest) {
        const nibbles = hexToNibbles(remaining);
        return '0x' +
            nibbles.length.toString(16).padStart(2, '0') +
            nibbles.map(n => n.toString(16).padStart(2, '0')).join('') +
            dataDigest;
    }

    // Encode extension node data
    function encodeExtension(shared, childDigest) {
        const nibbles = hexToNibbles(shared);
        return '0x' +
            nibbles.length.toString(16).padStart(2, '0') +
            nibbles.map(n => n.toString(16).padStart(2, '0')).join('') +
            childDigest;
    }

    // Encode branch node data
    function encodeBranch(pathsDigest) {
        const sorted = Object.entries(pathsDigest).sort((a, b) =>
            parseInt(a[0], 16) - parseInt(b[0], 16)
        );

        let encoded = '0x' + sorted.length.toString(16).padStart(2, '0');
        for (const [nibbleStr, digest] of sorted) {
            encoded += parseInt(nibbleStr, 16).toString(16).padStart(2, '0');
            encoded += digest;
        }
        return encoded;
    }

    // Process witness array
    const witness = proofData.witness.map(commitment => {
        let nodeType, data;

        switch (commitment.type) {
            case 'Leaf':
                nodeType = 0;
                data = encodeLeaf(
                    commitment.contents.remaining,
                    commitment.contents.dataDigest
                );
                break;
            case 'Branch':
                nodeType = 1;
                data = encodeBranch(commitment.contents.pathsDigest);
                break;
            case 'Extension':
                nodeType = 2;
                data = encodeExtension(
                    commitment.contents.shared,
                    commitment.contents.childDigest
                );
                break;
        }

        return { nodeType, data };
    });

    // ABI for the verify function - path is now bytes, not bytes32
    const abi = [
        "function verify(bytes32 rootHash, (bytes path, (uint8 nodeType, bytes data)[] witness) proof) view returns (bool)"
    ];

    // Create interface
    const iface = new ethers.Interface(abi);

    // Convert hex path to bytes
    function hexToBytes(hexString) {
        if (hexString.startsWith('0x')) {
            return hexString;
        }
        // Convert hex string to bytes array
        const bytes = [];
        for (let i = 0; i < hexString.length; i += 2) {
            bytes.push(parseInt(hexString.substr(i, 2), 16));
        }
        return '0x' + bytes.map(b => b.toString(16).padStart(2, '0')).join('');
    }

    const proofStruct = {
        path: hexToBytes(proofData.path),
        witness: witness.map(w => ({
            nodeType: w.nodeType,
            data: w.data
        }))
    };

    // Encode the function call
    const calldata = iface.encodeFunctionData("verify", [
        '0x' + rootHash,
        proofStruct
    ]);

    return calldata;
}

// Main execution
function main() {
    // Get command line arguments
    const args = process.argv.slice(2);

    if (args.length < 1) {
        console.log('Usage: node generateCalldata.js <proof-file.json> [root-hash]');
        console.log('');
        console.log('The proof file should be a JSON file with the following structure:');
        console.log('{');
        console.log('  "rootHash": "optional-root-hash-here",');
        console.log('  "proof": {');
        console.log('    "path": "path-hex-string",');
        console.log('    "witness": [...]');
        console.log('  }');
        console.log('}');
        console.log('');
        console.log('OR just the proof data directly:');
        console.log('{');
        console.log('  "path": "path-hex-string",');
        console.log('  "witness": [...]');
        console.log('}');
        console.log('');
        console.log('If root hash is not in the file, provide it as the second argument.');
        process.exit(1);
    }

    const filename = args[0];
    const rootHashOverride = args[1];

    try {
        // Read and parse the file
        const filePath = path.resolve(filename);
        if (!fs.existsSync(filePath)) {
            console.error(`Error: File not found: ${filePath}`);
            process.exit(1);
        }

        const fileContent = fs.readFileSync(filePath, 'utf8');
        const data = JSON.parse(fileContent);

        // Extract root hash and proof data
        let rootHash;
        let proofData;

        // Check if file has the full structure with rootHash
        if (data.rootHash && data.proof) {
            rootHash = rootHashOverride || data.rootHash;
            proofData = data.proof;
        }
        // Check if file has nested structure with data.path
        else if (data.data && data.data.path) {
            if (!rootHashOverride) {
                console.error('Error: Root hash not found in file. Please provide it as the second argument.');
                process.exit(1);
            }
            rootHash = rootHashOverride;
            proofData = data.data;
        }
        // Otherwise assume the file contains just the proof data
        else if (data.path && data.witness) {
            if (!rootHashOverride) {
                console.error('Error: Root hash not found in file. Please provide it as the second argument.');
                process.exit(1);
            }
            rootHash = rootHashOverride;
            proofData = data;
        }
        else {
            console.error('Error: Invalid file format. Could not find proof data.');
            process.exit(1);
        }

        // Clean up root hash (remove 0x prefix if present)
        rootHash = rootHash.replace(/^0x/, '');

        // Generate calldata
        const calldata = generateCalldata(rootHash, proofData);

        console.log('=== PROOF DETAILS ===\n');
        console.log('File:', filename);
        console.log('Root Hash:', '0x' + rootHash);
        console.log('Path:', proofData.path);
        console.log('Witness Nodes:', proofData.witness.length);

        console.log('\n=== COMPLETE CALLDATA FOR verify() FUNCTION ===\n');
        console.log(calldata);

        console.log('\n\n=== HOW TO USE ===\n');
        console.log('1. Deploy MerklePatriciaVerifier contract');
        console.log('2. Use this calldata in a transaction to the contract address');
        console.log('3. The function selector (first 4 bytes) is for verify(bytes32,(bytes,(uint8,bytes)[]))');

        console.log('\n\n=== FOR REMIX ===');
        console.log('In "Low level interactions" section:');
        console.log('1. Paste the calldata above');
        console.log('2. Click "Transact"');

        console.log('\n\n=== FOR ETHERS.JS ===');
        console.log(`
const tx = {
    to: CONTRACT_ADDRESS,
    data: "${calldata}"
};
const result = await provider.call(tx);
console.log('Verification result:', result === '0x0000000000000000000000000000000000000000000000000000000000000001');
`);

        // Optionally save calldata to file
        const outputFile = filename.replace(/\.json$/, '-calldata.txt');
        fs.writeFileSync(outputFile, calldata);
        console.log(`\n✅ Calldata saved to: ${outputFile}`);

    } catch (error) {
        console.error('Error:', error.message);
        if (error.stack) {
            console.error('\nStack trace:', error.stack);
        }
        process.exit(1);
    }
}

// Run main function if executed directly
if (require.main === module) {
    main();
}

// Export for use in other scripts
if (typeof module !== 'undefined' && module.exports) {
    module.exports = generateCalldata;
}