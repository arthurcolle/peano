# Peano-LFE Running Guide

## Quick Start

### 1. Using Make (Recommended)
```bash
# Compile all modules
make compile

# Run tests
make test

# Start interactive REPL
make repl

# Clean build files
make clean
```

### 2. Using Simple Scripts
```bash
# Run simple test suite (doesn't require rebar3)
./test-simple.sh

# Run interactive demo
./demo.lfe
```

### 3. Using Rebar3
```bash
# Setup project dependencies
rebar3 get-deps

# Compile
rebar3 compile

# Run tests
./run-tests.sh

# Start REPL
rebar3 lfe repl
```

## Available Scripts

1. **run-tests.sh** - Full test runner using rebar3
2. **test-simple.sh** - Simple test runner (no rebar3 needed)
3. **demo.lfe** - Interactive demonstration of all modules
4. **test/test-runner.lfe** - Main test suite

## Interactive REPL Usage

Once in the REPL (via `make repl` or `rebar3 lfe repl`):

```lisp
;; Basic Peano numbers
> (set zero (peano:zero))
> (set five (peano:nat->peano 5))
> (peano:peano->nat five)
5

;; Arithmetic
> (peano:peano->nat (arithmetic:add five (peano:nat->peano 3)))
8

;; Lists
> (set lst (lists-peano:cons five (lists-peano:nil)))
> (peano:peano->nat (lists-peano:head lst))
5

;; Maps
> (set m (maps-peano:insert five (peano:nat->peano 100) (maps-peano:empty)))
> (maps-peano:get five m)
#(some <peano-number>)
```

## Troubleshooting

If you encounter compilation errors with rebar3:
1. Use `test-simple.sh` instead of `run-tests.sh`
2. Compile manually: `lfec -o ebin src/*.lfe`
3. Run REPL with: `lfe -pa ebin`

## Module Overview

- **peano** - Core Peano number type and basic operations
- **arithmetic** - Addition, multiplication, factorial, primes, etc.
- **pairs** - Binary pair encoding
- **booleans** - Boolean logic
- **lists-peano** - Linked lists
- **tuples** - Fixed-size tuples
- **vectors** - Balanced binary trees for O(log n) access
- **sets** - Mathematical sets
- **maps-peano** - Key-value maps (AVL trees)
- **records** - Named fields with type IDs
- **adt** - Algebraic data types (Option, Either, etc.)
- **hypergraph** - Graph with hyperedges
- **state-machine** - Finite state machines
- **expression** - Expression trees