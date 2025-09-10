#!/bin/bash

# Run the Rust Merkle Patricia Verifier

# Check if binary exists
if [ ! -f "target/release/mpt-verifier" ]; then
    echo "Verifier not built. Running setup..."
    ./setup.sh
    if [ $? -ne 0 ]; then
        echo "Setup failed. Exiting."
        exit 1
    fi
fi

# Run the verifier
./target/release/mpt-verifier