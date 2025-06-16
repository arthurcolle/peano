#!/bin/bash

# Simple test runner for Peano-LFE

echo "==================================="
echo "Peano-LFE Simple Test Runner"
echo "==================================="

# Compile the modules
echo "Compiling modules..."
for file in src/*.lfe; do
    if [ -f "$file" ]; then
        echo "Compiling $(basename $file)..."
        lfec -o ebin "$file" 2>&1
    fi
done

# Run the test script
echo ""
echo "Running tests..."
echo ""

# Make test runner executable and run it
chmod +x test/test-runner.lfe
test/test-runner.lfe

echo ""
echo "==================================="
echo "Test run completed"
echo "==================================="