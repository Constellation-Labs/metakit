#!/bin/bash

# Setup Python virtual environment for Merkle Patricia Verifier

echo "Setting up Python virtual environment..."

# Create virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    python3 -m venv venv
    echo "✓ Virtual environment created"
else
    echo "✓ Virtual environment already exists"
fi

# Activate virtual environment
source venv/bin/activate

# Install dependencies
echo "Installing dependencies..."
pip install -q -r requirements.txt
echo "✓ Dependencies installed"

echo ""
echo "Setup complete! To use the verifier:"
echo "  source venv/bin/activate"
echo "  python merkle_patricia_verifier.py"
echo ""
echo "Or run directly with:"
echo "  ./run.sh"