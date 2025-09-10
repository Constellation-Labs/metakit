#!/bin/bash

# Setup Rust environment for Merkle Patricia Verifier

echo "Setting up Rust environment..."

# Check if Rust is installed
if ! command -v cargo &> /dev/null; then
    echo "✗ Rust is not installed. Please install Rust first."
    echo "  Visit: https://www.rust-lang.org/tools/install"
    echo "  Or run: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    exit 1
fi

echo "✓ Rust is installed: $(rustc --version)"

# Download dependencies
echo "Downloading dependencies..."
cargo fetch
if [ $? -eq 0 ]; then
    echo "✓ Dependencies downloaded"
else
    echo "✗ Failed to download dependencies"
    exit 1
fi

# Build the verifier in release mode
echo "Building verifier (release mode)..."
cargo build --release
if [ $? -eq 0 ]; then
    echo "✓ Build successful"
else
    echo "✗ Build failed"
    exit 1
fi

echo ""
echo "Setup complete! To run the verifier:"
echo "  cargo run --release"
echo "  or"
echo "  ./target/release/mpt-verifier"
echo ""
echo "Or use the run script:"
echo "  ./run.sh"