const { MerklePatriciaVerifier } = require('./merkle-patricia-verifier');

/**
 * Example usage of the Merkle Patricia Verifier
 * 
 * This demonstrates how to verify a Merkle Patricia inclusion proof
 * that has been provided as JSON.
 */

// Example proof structure (this would come from your Scala backend)
const exampleProof = {
  "path" : "185b77b8cc4daf5b7f9975b488bc6b8531b50f61f60910e8df6d33818e7c999f",
  "witness" : [
    {
      "type" : "Leaf",
      "contents" : {
        "remaining" : "5b77b8cc4daf5b7f9975b488bc6b8531b50f61f60910e8df6d33818e7c999f",
        "dataDigest" : "185b77b8cc4daf5b7f9975b488bc6b8531b50f61f60910e8df6d33818e7c999f"
      }
    },
    {
      "type" : "Branch",
      "contents" : {
        "pathsDigest" : {
          "5" : "80a14aca41a93f1dc2f48df7f6f956ecb768ce2232a19c6d94cc7046a6186f8f",
          "8" : "ad00fd85eb14b54aeaa7114ddc2d4ed0e87fcea261995f104898c21c18114dbe",
          "b" : "9ab222ab38ca21b0be402c46747470828dddf6730e1ff53dc48120b015bb8207"
        }
      }
    },
    {
      "type" : "Branch",
      "contents" : {
        "pathsDigest" : {
          "0" : "4c1bb6acb128a758b62e5af576b920db778b66cadcefe07a34f05da08396ff98",
          "a" : "7ca0eb8e46557843dac169fbaaeee33e84c072993850df44ec1344a15868670b",
          "e" : "eca6d1df469adb06b4ea76630281a03e6f69bf2b6329196848387a9c126729b7",
          "1" : "b69f4ea8e5359b488df6cfee64d33586694a547214f921c7ff8a229282376df6",
          "9" : "e0cc8d6a00dde6ca94f1fed0571481b610ed8ed2fe98d5dfcde8fedfba587708",
          "d" : "aa6c3f4262010452c43b3ba86ce5a27ea562c9caa52dbedaa084113428c419fd",
          "2" : "26d3db32fe726e342672829484f82229aec9971bad4802a0a28969847dc60146",
          "c" : "353dff68b69bec055b30fa351028e0b02ea16f011e7e34daef8418d571738d4d",
          "7" : "e4a3f39450062f20f4c179e689722ef941f9d0a05e7cada26bc7c4ea004cd8e8",
          "b" : "e163f322b391ae9ebae6847a1663ef2f577dda055bbe3781d6b32649b1cd8384",
          "8" : "58ba82d21591da582165f1c4e3f0b061e4aeaa6c3bb63f2c2a1df6ad655e44f0",
          "4" : "c2a5885576202fa65f5ec6215d865d580c334780984949833588ab8d4d669612"
        }
      }
    }
  ]
};

// The root hash of the Merkle Patricia Trie
const rootHash = "4bde493b43d66c45a381a3e18a6e4c840cfa550319e24f397fb1aa1ab35a7a71";

// Create verifier instance
const verifier = new MerklePatriciaVerifier(rootHash);

// Verify the proof
function verifyProof(proof) {
  console.log('Verifying Merkle Patricia inclusion proof...');
  console.log('Path:', proof.path);
  console.log('Witness length:', proof.witness.length);
  console.log('Root hash:', rootHash);
  
  const result = verifier.verify(proof);
  
  if (result.success) {
    console.log('✓ Proof is valid!');
  } else {
    console.log('✗ Proof is invalid:', result.error.message);
    console.log('  Error type:', result.error.type);
  }
  
  return result;
}

// Example: Loading proof from JSON file
function verifyProofFromFile(filePath) {
  const fs = require('fs');
  
  try {
    const jsonContent = fs.readFileSync(filePath, 'utf8');
    const proof = JSON.parse(jsonContent);
    
    return verifyProof(proof);
  } catch (error) {
    console.error('Failed to load proof from file:', error.message);
    return { success: false, error };
  }
}

// Example: Verifying proof from API response
async function verifyProofFromAPI(apiUrl) {
  try {
    const response = await fetch(apiUrl);
    const data = await response.json();
    
    // Assume the API returns { proof, rootHash }
    const verifier = new MerklePatriciaVerifier(data.rootHash);
    return verifier.verify(data.proof);
  } catch (error) {
    console.error('Failed to fetch proof from API:', error.message);
    return { success: false, error };
  }
}

// Export for use in other modules
module.exports = {
  verifyProof,
  verifyProofFromFile,
  verifyProofFromAPI
};

// Run example if this file is executed directly
if (require.main === module) {
  console.log('Running example verification...\n');
  
  // Note: This example proof won't actually verify correctly
  // because the hashes are made up. In a real scenario,
  // you would get a valid proof from your Scala backend.
  verifyProof(exampleProof);
}