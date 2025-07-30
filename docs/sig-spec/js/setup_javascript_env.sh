#!/bin/bash
# Setup script for JavaScript/Node.js environment and dependencies

set -e

echo "Setting up JavaScript/Node.js environment for Constellation signature protocol verification..."

# Check if Node.js is available
if ! command -v node &> /dev/null; then
    echo "Error: Node.js is not installed. Please install Node.js 14 or later."
    exit 1
fi

# Check if npm is available
if ! command -v npm &> /dev/null; then
    echo "Error: npm is not installed. Please install npm."
    exit 1
fi

echo "Node.js version: $(node --version)"
echo "npm version: $(npm --version)"

# Create package.json if it doesn't exist
if [ ! -f "package.json" ]; then
    echo "Creating package.json..."
    cat > package.json << 'EOL'
{
  "name": "constellation-signature-verification",
  "version": "1.0.0",
  "description": "JavaScript implementation of Constellation Network signature protocol",
  "main": "javascript_verification_example.js",
  "type": "module",
  "scripts": {
    "test": "node javascript_verification_example.js"
  },
  "keywords": ["constellation", "signature", "ecdsa", "secp256k1"],
  "author": "",
  "license": "ISC"
}
EOL
else
    echo "package.json already exists."
fi

# Install dependencies
echo "Installing dependencies..."
npm install elliptic@6.5.4
npm install js-sha256@0.9.0
npm install js-sha512@0.8.0
npm install canonicalize@2.0.0

echo ""
echo "Setup complete!"
echo ""
echo "To run the verification example:"
echo "  node javascript_verification_example.js"
echo ""
echo "Dependencies installed:"
echo "  - elliptic: Elliptic curve cryptography (secp256k1)"
echo "  - js-sha256: SHA-256 hashing"
echo "  - js-sha512: SHA-512 hashing"
echo "  - canonicalize: RFC 8785 JSON canonicalization"