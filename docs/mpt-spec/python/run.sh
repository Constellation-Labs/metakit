#!/bin/bash

# Run the Merkle Patricia Verifier

# Check if virtual environment exists
if [ ! -d "venv" ]; then
    echo "Virtual environment not found. Running setup..."
    ./setup.sh
fi

# Activate virtual environment and run
source venv/bin/activate
python merkle_patricia_verifier.py