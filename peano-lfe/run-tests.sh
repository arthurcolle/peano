#!/bin/bash

# Peano-LFE Test Runner Script

echo "==================================="
echo "Peano-LFE Test Suite"
echo "==================================="

# Check if rebar3 is installed
if ! command -v rebar3 &> /dev/null; then
    echo "Error: rebar3 is not installed"
    echo "Please install rebar3 first"
    exit 1
fi

# Compile the project
echo "Compiling project..."
rebar3 compile

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo ""
echo "Running tests..."
echo ""

# Make test runner executable
chmod +x test/test-runner.lfe

# Run tests based on argument
if [ $# -eq 0 ]; then
    # No arguments - run all tests
    lfescript test/test-runner.lfe
elif [ "$1" == "help" ]; then
    echo "Usage: ./run-tests.sh [module]"
    echo ""
    echo "Available modules:"
    echo "  peano      - Test Peano axioms"
    echo "  arithmetic - Test arithmetic operations"
    echo "  pairs      - Test pair encoding"
    echo "  lists      - Test list operations"
    echo "  tuples     - Test tuple operations"
    echo "  records    - Test record operations"
    echo "  adt        - Test algebraic data types"
    echo "  vectors    - Test vector operations"
    echo "  maps       - Test map operations"
    echo "  sets       - Test set operations"
    echo "  all        - Run all tests (default)"
else
    # Run specific test module
    lfescript test/test-runner.lfe "$1"
fi

echo ""
echo "==================================="
echo "Test run completed"
echo "===================================" 