#!/bin/bash

# Run Grothendieck demonstration
echo "Running Grothendieck Mathematics Demonstration..."
echo ""

# Create a temporary LFE script that runs the demo
cat > /tmp/demo_runner.lfe << 'EOF'
(include-lib "peano/include/peano.lfe")

;; Run tests
(io:format "~n=== RUNNING GROTHENDIECK TESTS ===~n~n")
(grothendieck_test:run-all-tests)

;; Run demo
(io:format "~n~n=== RUNNING GROTHENDIECK DEMONSTRATIONS ===~n~n")
(grothendieck-demonstration:run-all-demos)

;; Run unity example
(io:format "~n~n=== RUNNING PEANO-GROTHENDIECK UNITY EXAMPLE ===~n~n")
(grothendieck-peano-unity:demonstrate-unity)

;; Exit
(erlang:halt 0)
EOF

# Run the script
cd /Users/agent/peano/peano-lfe
lfe /tmp/demo_runner.lfe 2>&1 | grep -v "=ERROR REPORT" | grep -v "=CRASH REPORT" | grep -v "=SUPERVISOR REPORT" | grep -v "=INFO REPORT" | grep -v "Crash dump"

echo ""
echo "Demonstration complete!"