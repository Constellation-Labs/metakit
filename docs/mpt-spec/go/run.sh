#!/bin/bash

# Run the Go Merkle Patricia Verifier

# Check if binary exists
if [ ! -f "mpt-verifier" ]; then
    echo "Verifier not built. Running setup..."
    ./setup.sh
    if [ $? -ne 0 ]; then
        echo "Setup failed. Exiting."
        exit 1
    fi
fi

# Run the verifier
./mpt-verifier