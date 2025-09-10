#!/bin/bash

# Setup Go environment for Merkle Patricia Verifier

echo "Setting up Go environment..."

# Check if Go is installed
if ! command -v go &> /dev/null; then
    echo "✗ Go is not installed. Please install Go first."
    echo "  Visit: https://golang.org/doc/install"
    exit 1
fi

echo "✓ Go is installed: $(go version)"

# Download dependencies
echo "Downloading dependencies..."
go mod download
if [ $? -eq 0 ]; then
    echo "✓ Dependencies downloaded"
else
    echo "✗ Failed to download dependencies"
    exit 1
fi

# Build the verifier
echo "Building verifier..."
go build -o mpt-verifier verifier.go
if [ $? -eq 0 ]; then
    echo "✓ Build successful"
else
    echo "✗ Build failed"
    exit 1
fi

echo ""
echo "Setup complete! To run the verifier:"
echo "  ./mpt-verifier"
echo ""
echo "Or use the run script:"
echo "  ./run.sh"