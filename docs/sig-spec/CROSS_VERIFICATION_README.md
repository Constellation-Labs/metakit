# Cross-Language Signature Protocol Verification

This directory contains tools for verifying the Constellation Metagraph Standard Signature Protocol implementation across different programming languages.

## Files

- `python_verification_example.py` - Python implementation that generates test vectors
- `scala_verification_worksheet.sc` - Scala worksheet for cross-language verification
- `setup_python_env.sh` - Script to set up Python virtual environment
- `requirements.txt` - Python dependencies
- `signature-protocol-developer-guide.md` - Complete developer documentation

## Prerequisites

### Python Setup
1. **Python 3.8 or later** (check with `python3 --version`)
2. **Virtual environment setup:**
   ```bash
   # Run the setup script (recommended)
   ./setup_python_env.sh
   
   # Or set up manually:
   python3 -m venv venv
   source venv/bin/activate
   pip install -r requirements.txt
   ```

### Scala Dependencies
The Scala worksheet uses the existing metakit dependencies (already in build.sbt).

## Running the Cross-Verification

1. **Setup Python environment (first time only):**
   ```bash
   cd /home/scas/git/metakit
   ./setup_python_env.sh
   ```

2. **Generate Python test vectors:**
   ```bash
   # Activate virtual environment
   source venv/bin/activate
   
   # Run the verification script
   python python_verification_example.py
   ```
   
   This creates `test_vectors.json` with canonical JSON, hashes, and signatures.

3. **Verify in Scala (choose your preferred method):**

   **A. Using Ammonite (Command Line):**
   ```bash
   # Option 1: Run from the sig-spec directory
   cd /home/scas/git/metakit/docs/sig-spec
   amm scala_verification_worksheet.sc
   
   # Option 2: Use environment variable
   export TEST_VECTORS_PATH=/home/scas/git/metakit/docs/sig-spec/test_vectors.json
   amm scala_verification_worksheet.sc
   
   # Option 3: Use system property
   amm -Dtest.vectors.path=/home/scas/git/metakit/docs/sig-spec/test_vectors.json scala_verification_worksheet.sc
   ```

   **B. Using IntelliJ IDEA Worksheet:**
   - Open `scala_verification_worksheet.sc` in IntelliJ
   - Ensure "Use compile server" is checked in worksheet settings
   - Check the "Current working directory" output when you run the worksheet
   - The worksheet will try to find test_vectors.json in several locations:
     - Current directory
     - `docs/sig-spec/test_vectors.json` 
     - `git/metakit/docs/sig-spec/test_vectors.json` (if running from home directory)
   - If the file is not found, adjust the paths list in the worksheet to match your setup
   - Run the worksheet (Ctrl+Shift+F10 or click green arrow)

   **C. Using Scastie (Online):**
   - Visit the [live Scastie worksheet](https://scastie.scala-lang.org/JdqYq7wuTiScHrCyG4mj6w)
   - Click "Run" to execute the cross-language verification
   - The worksheet includes embedded test vectors and demonstrates signature verification
   - No setup required - runs entirely in the browser

## What Gets Verified

The cross-verification tests:

1. **JSON Canonicalization (RFC 8785)**
   - Key sorting by UTF-16BE binary order
   - Number formatting (no trailing zeros)
   - String escaping rules
   - Whitespace removal

2. **Binary Serialization**
   - UTF-8 encoding of canonical JSON
   - DataUpdate prefix handling (`\x19Constellation Signed Data:\n`)
   - Base64 encoding for DataUpdate types

3. **Hash Computation**
   - SHA-256 algorithm
   - Byte-level compatibility
   - Hex string representation

4. **Signature Protocol Flow**
   - The critical hex conversion step: `hash_bytes → hex_string → bytes_for_signing`
   - ECDSA with SHA-512 signature algorithm
   - secp256k1 curve for the Python example

## Expected Output

When run successfully, you should see output like:

```
=== Constellation Network Signature Protocol Test Vectors ===

Test Vector 1 (TestData):
  Data: {"id": "test-001", "value": 42}
  Canonical JSON: {"id":"test-001","value":42}
  UTF-8 Bytes (hex): 7b226964223a22746573742d303031222c2276616c7565223a34327d
  SHA-256 Hash (hex): a1b2c3d4...
  ...

=== Scala Verification of Python Test Vectors ===

Verifying Test Vector 1 (TestData):
  ✓ Canonical JSON: true
  ✓ UTF-8 bytes: true
  ✓ SHA-256 hash: true
```

## Key Implementation Details

The verification confirms these critical aspects:

1. **Hex Conversion**: The implementation stores hashes as hex strings, then converts them back to bytes for signing
2. **DataUpdate Handling**: Special serialization with Constellation prefix for DataUpdate types
3. **RFC 8785 Compliance**: Exact matching of canonical JSON output
4. **Cross-Platform Compatibility**: Python and Scala produce identical results

## Troubleshooting

- Ensure `test_vectors.json` is generated before running the Scala worksheet
- Check that all dependencies are installed