#!/bin/bash

# Go signature verification environment setup script
# This script sets up the Go environment for the Constellation signature verification example
# Assumes Go is already installed on the system

set -e  # Exit on any error

echo "=== Setting up Go Signature Verification Environment ==="

# Check if Go is installed
if ! command -v go &> /dev/null; then
    echo "❌ Go is not installed. Please install Go 1.21 or later from https://golang.org/dl/"
    echo "   On Ubuntu/Debian: sudo apt install golang-go"
    echo "   On macOS: brew install go"
    echo "   Or download from: https://golang.org/dl/"
    exit 1
fi

# Check Go version
GO_VERSION=$(go version | cut -d' ' -f3 | sed 's/go//')
echo "✓ Found Go version: $GO_VERSION"

# Verify Go version is 1.18 or later
if ! go version | grep -E 'go1\.(1[8-9]|[2-9][0-9])' &> /dev/null; then
    echo "⚠️  Warning: Go 1.18+ required. Current version: $GO_VERSION"
    echo "   Please upgrade Go for compatibility."
fi

# Initialize Go module if go.mod doesn't exist
if [ ! -f "go.mod" ]; then
    echo "❌ go.mod not found. Make sure you're in the correct directory."
    exit 1
fi

echo "✓ Found go.mod file"

# Clean up any existing go.sum to start fresh
if [ -f "go.sum" ]; then
    echo "🧹 Cleaning up existing go.sum..."
    rm go.sum
fi

# Initialize dependencies using go get
echo "📦 Getting Go dependencies..."
go get github.com/btcsuite/btcd/btcec/v2@v2.3.2
go get github.com/gibson042/canonicaljson-go@v1.0.3

# Tidy up dependencies
echo "🧹 Tidying dependencies..."
go mod tidy

# Download all dependencies
echo "📦 Downloading all dependencies..."
go mod download

# Verify dependencies
echo "🔍 Verifying dependencies..."
go mod verify

# Test build
echo "🔧 Testing build..."
go build -o constellation-sig-spec main.go

# Clean up test binary
rm -f constellation-sig-spec

echo ""
echo "✅ Go environment setup complete!"
echo ""
echo "To run the signature verification example:"
echo "  go run main.go"
echo ""
echo "To build a binary:"
echo "  go build -o constellation-sig-spec main.go"
echo "  ./constellation-sig-spec"
echo ""
echo "Dependencies installed:"
echo "  - github.com/btcsuite/btcd/btcec/v2 (ECDSA secp256k1)"
echo "  - github.com/gibson042/canonicaljson-go (RFC 8785 canonical JSON)"
echo ""
echo "The generated test_vectors.json can be used for cross-language verification."