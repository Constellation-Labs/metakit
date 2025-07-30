#!/bin/bash

# Rust signature verification environment setup script
# This script sets up the Rust environment for the Constellation signature verification example
# Assumes Rust is already installed on the system

set -e  # Exit on any error

echo "=== Setting up Rust Signature Verification Environment ==="

# Check if Rust is installed
if ! command -v rustc &> /dev/null; then
    echo "❌ Rust is not installed. Please install Rust from https://rustup.rs/"
    echo "   Run: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    echo "   Then restart your terminal or run: source ~/.cargo/env"
    exit 1
fi

# Check if Cargo is installed
if ! command -v cargo &> /dev/null; then
    echo "❌ Cargo is not installed. Please install Rust with Cargo from https://rustup.rs/"
    exit 1
fi

# Check Rust version
RUST_VERSION=$(rustc --version | cut -d' ' -f2)
echo "✓ Found Rust version: $RUST_VERSION"

# Check Cargo version
CARGO_VERSION=$(cargo --version | cut -d' ' -f2)
echo "✓ Found Cargo version: $CARGO_VERSION"

# Verify Cargo.toml exists
if [ ! -f "Cargo.toml" ]; then
    echo "❌ Cargo.toml not found. Make sure you're in the correct directory."
    exit 1
fi

echo "✓ Found Cargo.toml file"

# Create src directory if it doesn't exist
if [ ! -d "src" ]; then
    echo "📁 Creating src directory..."
    mkdir -p src
fi

# Check if main.rs exists
if [ ! -f "src/main.rs" ]; then
    echo "❌ src/main.rs not found. Make sure the main.rs file is in the src directory."
    exit 1
fi

echo "✓ Found src/main.rs file"

# Update Rust toolchain (optional but recommended)
echo "🔄 Updating Rust toolchain..."
rustup update

# Download and compile dependencies
echo "📦 Downloading and compiling Rust dependencies..."
cargo build --release

# Run tests to ensure everything works
echo "🧪 Running tests..."
cargo test

echo ""
echo "✅ Rust environment setup complete!"
echo ""
echo "To run the signature verification example:"
echo "  cargo run"
echo ""
echo "To run in release mode (faster):"
echo "  cargo run --release"
echo ""
echo "To build a binary:"
echo "  cargo build --release"
echo "  ./target/release/constellation-sig-spec"
echo ""
echo "Dependencies installed:"
echo "  - serde & serde_json (JSON serialization)"
echo "  - serde_json_canonicalizer (RFC 8785 canonical JSON)"
echo "  - secp256k1 (ECDSA secp256k1 cryptography)"
echo "  - sha2 (SHA-256/SHA-512 hashing)"
echo "  - hex (hexadecimal encoding/decoding)"
echo "  - base64 (base64 encoding)"
echo "  - anyhow (error handling)"
echo ""
echo "The generated test_vectors.json can be used for cross-language verification."