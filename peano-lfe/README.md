# Peano Arithmetic in Lisp-Flavored Erlang

This implementation demonstrates how to build up from Peano arithmetic axioms to arbitrary computation, including complex data structures like hypergraphs, following the concepts from the Stack Overflow answer about PA's computational power.

## Structure

### Core Arithmetic
- `src/peano.lfe` - Basic Peano axioms: zero, successor, predecessor
- `src/arithmetic.lfe` - Arithmetic operations: +, *, /, %, pow, factorial, etc.

### Basic Data Structures
- `src/pairs.lfe` - Binary pair encoding/decoding (encode 2 numbers as 1)
- `src/lists.lfe` - List operations built from pairs

### Advanced Type System
- `src/types.lfe` - General type system for encoding any data structure
- `src/tuples.lfe` - Tuples of arbitrary arity with O(1) access
- `src/records.lfe` - Records with named fields
- `src/adt.lfe` - Algebraic data types (Option, Either, Trees, etc.)
- `src/vectors.lfe` - Vectors/arrays with O(log n) access using balanced trees
- `src/maps.lfe` - Maps/dictionaries using AVL trees
- `src/sets.lfe` - Sets with union, intersection, difference operations

### Mathematical Structures
- `src/ordinals.lfe` - Ordinal arithmetic and Cantor normal form
- `src/hypergraphs.lfe` - Hypergraph data structure implementation

### Demonstrations
- `src/compute.lfe` - Demonstration of PA's full computational power
- `src/datatypes.lfe` - Comprehensive examples using all data types

## Key Concepts

1. **From Numbers to Pairs**: Using bit interleaving to encode two numbers as one
2. **From Pairs to Lists**: Building cons cells from pairs
3. **From Lists to Everything**: With lists, we can build any data structure
4. **Type System**: Every value is tagged with its type
5. **Ordinals**: Representing ordinals up to ε₀ in Cantor normal form
6. **Hypergraphs**: Complex mathematical structures as the culmination

## Running

```bash
# Setup
make setup

# Compile
make compile

# Run examples
make run-examples

# Interactive shell
make shell
```

## Example Usage

```lisp
;; In the LFE shell:
> (peano:nat->peano 5)
#(s #(s #(s #(s #(s 0)))))

> (arithmetic:add (peano:nat->peano 3) (peano:nat->peano 2))
#(s #(s #(s #(s #(s 0)))))

> (pairs:pair (peano:nat->peano 7) (peano:nat->peano 11))
;; Returns encoded number representing the pair (7, 11)

> (ordinals:omega)
;; Returns ω as ((1, (1, 1)))
```

## Complete Type System

The implementation provides a complete type system built mechanistically from Peano axioms:

### Primitive Types
- **Numbers** - Peano numbers with arithmetic operations
- **Booleans** - Encoded as 0 (false) and s(0) (true)
- **Pairs** - Binary encoding to store two numbers as one

### Composite Types
- **Tuples** - Arbitrary arity with indexed access
- **Records** - Named fields with get/set operations
- **Lists** - Linked lists with map, filter, fold
- **Vectors** - Balanced trees for O(log n) access
- **Maps** - AVL trees for key-value storage
- **Sets** - Mathematical sets with standard operations

### Algebraic Data Types
- **Option/Maybe** - Some(value) or None
- **Either/Result** - Left(error) or Right(value)
- **Trees** - Binary trees with leaf and node constructors
- **Custom ADTs** - Framework for defining sum types

### Advanced Structures
- **Ordinals** - Up to ε₀ in Cantor normal form
- **Hypergraphs** - Nodes and hyperedges
- **State Machines** - States and transitions
- **Expression Trees** - Type-safe ASTs

## The Big Picture

This implementation shows that despite PA's simple axioms (0, successor, induction), we can:

1. Define all arithmetic operations
2. Encode multiple values as single numbers
3. Build lists and recursive data structures
4. Create a complete type system with:
   - Product types (tuples, records)
   - Sum types (ADTs, Option, Either)
   - Collection types (lists, vectors, maps, sets)
5. Represent ordinals and perform transfinite induction (up to ε₀)
6. Encode arbitrary computation (Turing-complete)
7. Build complex mathematical objects like hypergraphs

This demonstrates that PA can prove "PA can prove each Goodstein sequence terminates" even though it cannot prove "all Goodstein sequences terminate".