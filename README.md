# Peano

An implementation of Peano Arithmetic in Lisp Flavored Erlang (LFE), demonstrating that Peano Arithmetic is computationally complete.

## Overview

This project builds up from the basic axioms of Peano arithmetic (zero, successor, and induction) to achieve full computational power. Starting from just numbers, it constructs pairs, lists, and eventually complex data structures like vectors, maps, sets, and even hypergraphs - all using only the fundamental operations of Peano arithmetic.

## Key Concept

The project proves that Peano Arithmetic (PA) can encode arbitrary computation by showing how to:
1. Encode two numbers as one (via bit interleaving)
2. Build pairs from numbers
3. Construct lists from pairs
4. Create any data structure from lists
5. Achieve Turing-completeness using only Peano axioms

## Project Structure

### Core Arithmetic
- `src/peano.lfe` - Basic Peano axioms: zero, successor, predecessor
- `src/arithmetic.lfe` - Arithmetic operations: addition, multiplication, division, etc.

### Basic Data Structures
- `src/pairs.lfe` - Binary pair encoding using bit interleaving
- `src/lists.lfe` - Linked lists built from pairs

### Advanced Type System
- `src/types.lfe` - General type system framework
- `src/tuples.lfe` - Arbitrary arity tuples with O(1) access
- `src/records.lfe` - Named field structures
- `src/adt.lfe` - Algebraic data types (Option, Either, Trees)
- `src/vectors.lfe` - Arrays with O(log n) access using balanced trees
- `src/maps.lfe` - Dictionaries using AVL trees
- `src/sets.lfe` - Mathematical sets with standard operations

### Mathematical Structures
- `src/ordinals.lfe` - Ordinal arithmetic up to ε₀ in Cantor normal form
- `src/hypergraphs.lfe` - Complex graph structures

### Demonstrations
- `src/compute.lfe` - Showcases PA's full computational power
- `src/datatypes.lfe` - Comprehensive examples

## Getting Started

### Prerequisites
- Erlang/OTP
- Rebar3
- LFE (Lisp Flavored Erlang)

### Building
```bash
rebar3 compile
```

### Running Tests
```bash
rebar3 ltest
```

### Interactive Development
```bash
rebar3 lfe repl
```

## Comprehensive Examples

All examples below use: `(include-lib "peano/include/peano.lfe")`

### Basic Peano Arithmetic

```lfe
;; 1. Zero and successor
> (peano:zero)
0
> (peano:s (peano:zero))
1
> (peano:s (peano:s (peano:zero)))
2

;; 2. Predecessor
> (peano:p 5)
4
> (peano:p 1)
0
> (peano:p 0)
0

;; 3. Zero check
> (peano:zero? 0)
true
> (peano:zero? 5)
false

;; 4. Equality
> (peano:eq 3 3)
true
> (peano:eq 3 5)
false

;; 5. Natural to Peano conversion
> (peano:nat->peano 7)
7
> (peano:peano->nat (peano:s (peano:s (peano:zero))))
2

;; 6. Comparisons
> (peano:lt 3 5)
true
> (peano:gt 7 4)
true
> (peano:lte 5 5)
true
> (peano:gte 8 10)
false

;; 7. Min and max
> (peano:min 3 7)
3
> (peano:max 3 7)
7
```

### Arithmetic Operations

```lfe
;; 8. Addition
> (arithmetic:add 5 3)
8
> (arithmetic:add 0 7)
7

;; 9. Multiplication
> (arithmetic:mult 4 6)
24
> (arithmetic:mult 0 5)
0

;; 10. Power
> (arithmetic:pow 2 8)
256
> (arithmetic:pow 3 4)
81

;; 11. Subtraction (partial)
> (arithmetic:sub 10 3)
7
> (arithmetic:sub 3 5)
0

;; 12. Division
> (arithmetic:div 20 4)
5
> (arithmetic:div 17 5)
3

;; 13. Modulo
> (arithmetic:mod 17 5)
2
> (arithmetic:mod 20 4)
0

;; 14. Factorial
> (arithmetic:fact 5)
120
> (arithmetic:fact 0)
1

;; 15. Fibonacci
> (arithmetic:fib 0)
0
> (arithmetic:fib 1)
1
> (arithmetic:fib 10)
55

;; 16. Greatest common divisor
> (arithmetic:gcd 48 18)
6
> (arithmetic:gcd 17 19)
1

;; 17. Prime testing
> (arithmetic:prime? 17)
true
> (arithmetic:prime? 18)
false
> (arithmetic:prime? 2)
true
```

### Pairs and Triples

```lfe
;; 18. Creating pairs
> (pairs:pair 3 7)
<encoded-pair>

;; 19. Accessing pair elements
> (pairs:first (pairs:pair 3 7))
3
> (pairs:second (pairs:pair 3 7))
7

;; 20. Creating triples
> (pairs:triple 1 2 3)
<encoded-triple>

;; 21. Accessing triple elements
> (pairs:triple-first (pairs:triple 5 10 15))
5
> (pairs:triple-second (pairs:triple 5 10 15))
10
> (pairs:triple-third (pairs:triple 5 10 15))
15
```

### Lists

```lfe
;; 22. Empty list
> (lists:nil)
<empty-list>

;; 23. Cons (adding elements)
> (lists:cons 5 (lists:nil))
<list-with-5>
> (lists:cons 3 (lists:cons 5 (lists:nil)))
<list-3-5>

;; 24. Head and tail
> (lists:head (lists:cons 3 (lists:cons 5 (lists:nil))))
3
> (lists:tail (lists:cons 3 (lists:cons 5 (lists:nil))))
<list-5>

;; 25. List length
> (lists:length (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
3

;; 26. Nth element
> (lists:nth 0 (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil)))))
10
> (lists:nth 2 (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil)))))
30

;; 27. Append lists
> (lists:append (lists:cons 1 (lists:cons 2 (lists:nil))) 
                (lists:cons 3 (lists:cons 4 (lists:nil))))
<list-1-2-3-4>

;; 28. Reverse list
> (lists:reverse (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
<list-3-2-1>

;; 29. Map function over list
> (lists:map (lambda (x) (arithmetic:mult x 2)) 
             (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
<list-2-4-6>

;; 30. Filter list
> (lists:filter (lambda (x) (arithmetic:prime? x)) 
                (lists:cons 2 (lists:cons 3 (lists:cons 4 (lists:cons 5 (lists:nil))))))
<list-2-3-5>

;; 31. Fold left
> (lists:foldl arithmetic:add 0 
               (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
6

;; 32. Fold right
> (lists:foldr arithmetic:mult 1 
               (lists:cons 2 (lists:cons 3 (lists:cons 4 (lists:nil)))))
24

;; 33. Member check
> (lists:member? 3 (lists:cons 1 (lists:cons 3 (lists:cons 5 (lists:nil)))))
true
> (lists:member? 4 (lists:cons 1 (lists:cons 3 (lists:cons 5 (lists:nil)))))
false

;; 34. Find element
> (lists:find (lambda (x) (peano:gt x 5)) 
              (lists:cons 3 (lists:cons 7 (lists:cons 2 (lists:nil)))))
7
```

### Type System

```lfe
;; 35. Creating typed values
> (types:make-typed 'number 42)
<typed-value>

;; 36. Type checking
> (types:type-of (types:make-number 42))
number
> (types:value-of (types:make-number 42))
42

;; 37. Boolean types
> (types:make-boolean (peano:s (peano:zero)))
<typed-boolean>
> (types:boolean? (types:make-boolean (peano:s (peano:zero))))
true

;; 38. Pair types
> (types:make-pair 3 7)
<typed-pair>
> (types:pair? (types:make-pair 3 7))
true

;; 39. List types
> (types:make-list (lists:cons 1 (lists:cons 2 (lists:nil))))
<typed-list>
> (types:list? (types:make-list (lists:nil)))
true

;; 40. Symbol types
> (types:make-symbol 123)
<typed-symbol>
> (types:symbol? (types:make-symbol 456))
true

;; 41. String types
> (types:make-string (lists:cons 72 (lists:cons 105 (lists:nil))))
<typed-string>

;; 42. Tree node types
> (types:make-tree-node 5 (pairs:pair (types:make-tree-node 3 (pairs:pair 0 0)) 
                                      (types:make-tree-node 7 (pairs:pair 0 0))))
<typed-tree-node>
```

### Algebraic Data Types (ADT)

```lfe
;; 43. Option/Maybe type - None
> (adt:none)
<none>
> (adt:is-none? (adt:none))
true

;; 44. Option/Maybe type - Some
> (adt:some 42)
<some-42>
> (adt:is-some? (adt:some 42))
true
> (adt:some-value (adt:some 42))
42

;; 45. Option map
> (adt:option-map (lambda (x) (arithmetic:mult x 2)) (adt:some 21))
<some-42>
> (adt:option-map (lambda (x) (arithmetic:mult x 2)) (adt:none))
<none>

;; 46. Option bind
> (adt:option-bind (adt:some 10) 
                   (lambda (x) (if (peano:gt x 5) (adt:some x) (adt:none))))
<some-10>

;; 47. Either/Result type - Left
> (adt:left 'error)
<left-error>
> (adt:is-left? (adt:left 'error))
true
> (adt:left-value (adt:left 'error))
error

;; 48. Either/Result type - Right
> (adt:right 42)
<right-42>
> (adt:is-right? (adt:right 42))
true
> (adt:right-value (adt:right 42))
42

;; 49. Either map
> (adt:either-map (lambda (x) (arithmetic:add x 1)) (adt:right 41))
<right-42>
> (adt:either-map (lambda (x) (arithmetic:add x 1)) (adt:left 'error))
<left-error>

;; 50. Tree ADT - Leaf
> (adt:leaf 5)
<leaf-5>
> (adt:is-leaf? (adt:leaf 5))
true

;; 51. Tree ADT - Node
> (adt:node 10 (adt:leaf 5) (adt:leaf 15))
<node-10>
> (adt:is-node? (adt:node 10 (adt:leaf 5) (adt:leaf 15)))
true

;; 52. Tree map
> (adt:tree-map (lambda (x) (arithmetic:mult x 2)) 
                (adt:node 10 (adt:leaf 5) (adt:leaf 15)))
<node-20-leaf-10-leaf-30>

;; 53. Tree fold
> (adt:tree-fold arithmetic:add 0 
                 (adt:node 10 (adt:leaf 5) (adt:leaf 15)))
30

;; 54. Boolean ADT
> (adt:bool-true)
<true>
> (adt:bool-false)
<false>
> (adt:is-true? (adt:bool-true))
true
> (adt:is-false? (adt:bool-false))
true
```

### Tuples

```lfe
;; 55. Creating tuples
> (tuples:tuple)
<empty-tuple>
> (tuples:tuple-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
<tuple-1-2-3>

;; 56. Tuple arity
> (tuples:tuple-arity (tuples:tuple-from-list (lists:cons 1 (lists:cons 2 (lists:nil)))))
2

;; 57. Tuple reference
> (tuples:tuple-ref 0 (tuples:tuple-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
10
> (tuples:tuple-ref 2 (tuples:tuple-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
30

;; 58. Tuple set
> (tuples:tuple-set 1 99 (tuples:tuple-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
<tuple-10-99-30>

;; 59. Tuple to list
> (tuples:tuple->list (tuples:tuple-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
<list-1-2-3>

;; 60. Tuple map
> (tuples:tuple-map (lambda (x) (arithmetic:mult x 2)) 
                    (tuples:tuple-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
<tuple-2-4-6>

;; 61. Tuple fold
> (tuples:tuple-fold arithmetic:add 0 
                     (tuples:tuple-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
60

;; 62. Tuple zip
> (tuples:tuple-zip (tuples:tuple-from-list (lists:cons 1 (lists:cons 2 (lists:nil))))
                    (tuples:tuple-from-list (lists:cons 10 (lists:cons 20 (lists:nil)))))
<tuple-of-pairs>

;; 63. Tuple equality
> (tuples:tuple-eq? (tuples:tuple-from-list (lists:cons 1 (lists:cons 2 (lists:nil))))
                    (tuples:tuple-from-list (lists:cons 1 (lists:cons 2 (lists:nil)))))
true
```

### Records

```lfe
;; 64. Creating records
> (records:make-record 'person (lists:cons (pairs:pair 'name 'alice) 
                                          (lists:cons (pairs:pair 'age 30) (lists:nil))))
<record-person>

;; 65. Record get
> (records:record-get 'name (records:make-person 'bob 25))
bob
> (records:record-get 'age (records:make-person 'bob 25))
25

;; 66. Record set
> (records:record-set 'age 26 (records:make-person 'bob 25))
<record-person-bob-26>

;; 67. Record update
> (records:record-update (lists:cons (pairs:pair 'age 31) (lists:nil)) 
                        (records:make-person 'alice 30))
<record-person-alice-31>

;; 68. Record equality
> (records:record-eq? (records:make-person 'alice 30) 
                      (records:make-person 'alice 30))
true

;; 69. Record field names
> (records:record-field-names (records:make-person 'alice 30))
<list-name-age>

;; 70. Record field values
> (records:record-field-values (records:make-person 'alice 30))
<list-alice-30>

;; 71. Point records
> (records:make-point 10 20)
<record-point-10-20>
> (records:record-get 'x (records:make-point 10 20))
10
> (records:record-get 'y (records:make-point 10 20))
20
```

### Vectors

```lfe
;; 72. Empty vector
> (vectors:vector-empty)
<empty-vector>

;; 73. Vector from list
> (vectors:vector-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil)))))
<vector-10-20-30>

;; 74. Vector reference
> (vectors:vector-ref 1 (vectors:vector-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
20

;; 75. Vector set
> (vectors:vector-set 1 99 (vectors:vector-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
<vector-10-99-30>

;; 76. Vector append
> (vectors:vector-append 40 (vectors:vector-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
<vector-10-20-30-40>

;; 77. Vector to list
> (vectors:vector->list (vectors:vector-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
<list-1-2-3>

;; 78. Vector map
> (vectors:vector-map (lambda (x) (arithmetic:mult x 2)) 
                      (vectors:vector-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
<vector-2-4-6>

;; 79. Vector fold
> (vectors:vector-fold arithmetic:add 0 
                       (vectors:vector-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
60

;; 80. Vector size
> (vectors:vector-size (vectors:vector-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:cons 4 (lists:nil)))))))
4

;; 81. Make vector with initial value
> (vectors:make-vector 5 99)
<vector-99-99-99-99-99>

;; 82. Vector slice
> (vectors:vector-slice 1 3 (vectors:vector-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:cons 40 (lists:cons 50 (lists:nil))))))))
<vector-20-30-40>
```

### Maps (Dictionaries)

```lfe
;; 83. Empty map
> (maps:map-empty)
<empty-map>

;; 84. Map insert
> (maps:map-insert 'key 'value (maps:map-empty))
<map-with-key-value>

;; 85. Map get
> (maps:map-get 'key (maps:map-insert 'key 42 (maps:map-empty)))
42

;; 86. Map has key
> (maps:map-has-key? 'key (maps:map-insert 'key 42 (maps:map-empty)))
true
> (maps:map-has-key? 'other (maps:map-insert 'key 42 (maps:map-empty)))
false

;; 87. Map remove
> (maps:map-remove 'key (maps:map-insert 'key 42 (maps:map-empty)))
<empty-map>

;; 88. Map from list
> (maps:map-from-list (lists:cons (pairs:pair 'a 1) 
                                 (lists:cons (pairs:pair 'b 2) 
                                           (lists:cons (pairs:pair 'c 3) (lists:nil)))))
<map-a-1-b-2-c-3>

;; 89. Map to list
> (maps:map->list (maps:map-from-list (lists:cons (pairs:pair 'x 10) 
                                                 (lists:cons (pairs:pair 'y 20) (lists:nil)))))
<list-of-pairs>

;; 90. Map size
> (maps:map-size (maps:map-from-list (lists:cons (pairs:pair 'a 1) 
                                                (lists:cons (pairs:pair 'b 2) 
                                                          (lists:cons (pairs:pair 'c 3) (lists:nil))))))
3

;; 91. Map over values
> (maps:map-map (lambda (v) (arithmetic:mult v 2)) 
                (maps:map-from-list (lists:cons (pairs:pair 'a 1) 
                                              (lists:cons (pairs:pair 'b 2) (lists:nil)))))
<map-a-2-b-4>

;; 92. Map keys
> (maps:map-keys (maps:map-from-list (lists:cons (pairs:pair 'x 10) 
                                                (lists:cons (pairs:pair 'y 20) (lists:nil)))))
<list-x-y>

;; 93. Map values
> (maps:map-values (maps:map-from-list (lists:cons (pairs:pair 'x 10) 
                                                  (lists:cons (pairs:pair 'y 20) (lists:nil)))))
<list-10-20>

;; 94. Map merge
> (maps:map-merge (maps:map-from-list (lists:cons (pairs:pair 'a 1) (lists:nil)))
                  (maps:map-from-list (lists:cons (pairs:pair 'b 2) (lists:nil))))
<map-a-1-b-2>
```

### Sets

```lfe
;; 95. Empty set
> (sets:set-empty)
<empty-set>

;; 96. Set from list
> (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
<set-1-2-3>

;; 97. Set add
> (sets:set-add 4 (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
<set-1-2-3-4>

;; 98. Set remove
> (sets:set-remove 2 (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
<set-1-3>

;; 99. Set member
> (sets:set-member? 2 (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
true
> (sets:set-member? 5 (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
false

;; 100. Set size
> (sets:set-size (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 2 (lists:cons 3 (lists:nil)))))))
3

;; 101. Set to list
> (sets:set->list (sets:set-from-list (lists:cons 3 (lists:cons 1 (lists:cons 2 (lists:nil))))))
<sorted-list-1-2-3>

;; 102. Set union
> (sets:set-union (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:nil))))
                  (sets:set-from-list (lists:cons 2 (lists:cons 3 (lists:nil)))))
<set-1-2-3>

;; 103. Set intersection
> (sets:set-intersection (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
                         (sets:set-from-list (lists:cons 2 (lists:cons 3 (lists:cons 4 (lists:nil))))))
<set-2-3>

;; 104. Set difference
> (sets:set-difference (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
                       (sets:set-from-list (lists:cons 2 (lists:cons 4 (lists:nil)))))
<set-1-3>

;; 105. Set subset
> (sets:set-subset? (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:nil))))
                    (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
true

;; 106. Set equality
> (sets:set-eq? (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
                (sets:set-from-list (lists:cons 3 (lists:cons 1 (lists:cons 2 (lists:nil))))))
true

;; 107. Set map
> (sets:set-map (lambda (x) (arithmetic:mult x 2)) 
                (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))))
<set-2-4-6>

;; 108. Set filter
> (sets:set-filter arithmetic:prime? 
                   (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:cons 4 (lists:cons 5 (lists:nil))))))))
<set-2-3-5>

;; 109. Set fold
> (sets:set-fold arithmetic:add 0 
                 (sets:set-from-list (lists:cons 10 (lists:cons 20 (lists:cons 30 (lists:nil))))))
60

;; 110. Power set
> (sets:set-powerset (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:nil)))))
<set-of-sets>

;; 111. Cartesian product
> (sets:set-product (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:nil))))
                    (sets:set-from-list (lists:cons 'a (lists:cons 'b (lists:nil)))))
<set-of-pairs>
```

### Ordinals

```lfe
;; 112. Zero ordinal
> (ordinals:ord-zero)
<ordinal-0>

;; 113. Finite ordinals
> (ordinals:ord-finite 5)
<ordinal-5>

;; 114. Omega (first infinite ordinal)
> (ordinals:omega)
<ordinal-ω>

;; 115. Ordinal comparison
> (ordinals:ord-lt (ordinals:ord-finite 5) (ordinals:omega))
true
> (ordinals:ord-eq (ordinals:ord-finite 5) (ordinals:ord-finite 5))
true

;; 116. Ordinal addition
> (ordinals:ord-add (ordinals:ord-finite 3) (ordinals:ord-finite 5))
<ordinal-8>
> (ordinals:ord-add (ordinals:ord-finite 5) (ordinals:omega))
<ordinal-ω>

;; 117. Ordinal multiplication
> (ordinals:ord-mult (ordinals:ord-finite 3) (ordinals:ord-finite 4))
<ordinal-12>
> (ordinals:ord-mult (ordinals:omega) (ordinals:ord-finite 2))
<ordinal-ω·2>

;; 118. Ordinal multiplication by natural
> (ordinals:ord-mult-nat (ordinals:omega) 3)
<ordinal-ω·3>

;; 119. Omega exponentiation
> (ordinals:omega-to (ordinals:ord-finite 2))
<ordinal-ω²>
> (ordinals:omega-to (ordinals:omega))
<ordinal-ω^ω>

;; 120. Goodstein sequence step
> (ordinals:goodstein-step 2 266)
<next-goodstein-value>
```

### Hypergraphs

```lfe
;; 121. Empty hypergraph
> (hypergraphs:empty-hypergraph)
<empty-hypergraph>

;; 122. Add node
> (hypergraphs:add-node 1 (hypergraphs:empty-hypergraph))
<hypergraph-with-node-1>

;; 123. Add hyperedge
> (hypergraphs:add-hyperedge (sets:set-from-list (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil)))))
                             (hypergraphs:add-node 3 
                               (hypergraphs:add-node 2 
                                 (hypergraphs:add-node 1 (hypergraphs:empty-hypergraph)))))
<hypergraph-with-edge>

;; 124. Incident edges
> (hypergraphs:incident-edges 2 <some-hypergraph>)
<set-of-edges>

;; 125. Node degree
> (hypergraphs:node-degree 2 <some-hypergraph>)
3

;; 126. Neighbors
> (hypergraphs:neighbors 1 <some-hypergraph>)
<set-of-neighbors>

;; 127. K-uniform check
> (hypergraphs:k-uniform? 3 <some-hypergraph>)
true

;; 128. Dual hypergraph
> (hypergraphs:dual <some-hypergraph>)
<dual-hypergraph>

;; 129. Line graph
> (hypergraphs:line-graph <some-hypergraph>)
<line-graph>

;; 130. Connected components
> (hypergraphs:connected-components <some-hypergraph>)
<list-of-components>
```

### Fundamental Types (New)

```lfe
;; 131. Integers (signed numbers)
> (fundamentals:make-integer 0 42)  ; +42
<integer-+42>
> (fundamentals:make-integer 1 42)  ; -42
<integer--42>
> (fundamentals:integer-add (fundamentals:make-integer 0 10) 
                            (fundamentals:make-integer 1 5))
<integer-+5>
> (fundamentals:integer-mult (fundamentals:make-integer 1 3) 
                             (fundamentals:make-integer 1 4))
<integer-+12>
> (fundamentals:integer-negate (fundamentals:make-integer 0 7))
<integer--7>

;; 132. Rational numbers (fractions)
> (fundamentals:make-rational (fundamentals:nat->integer 1) 
                              (fundamentals:nat->integer 2))
<rational-1/2>
> (fundamentals:rational-add (fundamentals:make-rational (fundamentals:nat->integer 1) 
                                                         (fundamentals:nat->integer 3))
                             (fundamentals:make-rational (fundamentals:nat->integer 1) 
                                                         (fundamentals:nat->integer 6)))
<rational-1/2>
> (fundamentals:rational-mult (fundamentals:make-rational (fundamentals:nat->integer 2) 
                                                          (fundamentals:nat->integer 3))
                              (fundamentals:make-rational (fundamentals:nat->integer 3) 
                                                          (fundamentals:nat->integer 4)))
<rational-1/2>
> (fundamentals:rational-simplify (fundamentals:make-rational (fundamentals:nat->integer 6) 
                                                              (fundamentals:nat->integer 8)))
<rational-3/4>

;; 133. Characters
> (fundamentals:make-char 65)  ; 'A'
<char-A>
> (fundamentals:char->code (fundamentals:make-char 97))  ; 'a'
97
> (fundamentals:char-upcase (fundamentals:make-char 97))
<char-A>
> (fundamentals:char-downcase (fundamentals:make-char 65))
<char-a>
> (fundamentals:char-alpha? (fundamentals:make-char 65))
true
> (fundamentals:char-digit? (fundamentals:make-char 53))  ; '5'
true
> (fundamentals:char-whitespace? (fundamentals:make-char 32))  ; space
true

;; 134. Bytes
> (fundamentals:make-byte 255)
<byte-255>
> (fundamentals:byte-and (fundamentals:make-byte 170) 
                         (fundamentals:make-byte 204))  ; 10101010 & 11001100
<byte-136>
> (fundamentals:byte-or (fundamentals:make-byte 170) 
                        (fundamentals:make-byte 85))   ; 10101010 | 01010101
<byte-255>
> (fundamentals:byte-xor (fundamentals:make-byte 255) 
                         (fundamentals:make-byte 170))
<byte-85>
> (fundamentals:byte-shift-left (fundamentals:make-byte 1) 3)
<byte-8>
> (fundamentals:byte-shift-right (fundamentals:make-byte 128) 2)
<byte-32>

;; 135. Fixed-point numbers
> (fundamentals:make-fixed 3 14 100)  ; 3.14 with scale 100
<fixed-3.14>
> (fundamentals:fixed-add (fundamentals:make-fixed 2 50 100)  ; 2.50
                          (fundamentals:make-fixed 1 25 100)) ; 1.25
<fixed-3.75>
> (fundamentals:fixed-mult (fundamentals:make-fixed 2 5 10)   ; 2.5
                           (fundamentals:make-fixed 3 0 10))   ; 3.0
<fixed-7.5>
> (fundamentals:fixed-round (fundamentals:make-fixed 3 14159 100000) 2)
<fixed-3.14>

;; 136. Intervals
> (fundamentals:make-interval 0 10 1 1)  ; [0, 10]
<interval-[0,10]>
> (fundamentals:make-interval 0 10 0 0)  ; (0, 10)
<interval-(0,10)>
> (fundamentals:interval-contains? (fundamentals:make-interval 0 10 1 1) 5)
true
> (fundamentals:interval-contains? (fundamentals:make-interval 0 10 0 0) 0)
false
> (fundamentals:interval-intersect (fundamentals:make-interval 0 10 1 1)
                                   (fundamentals:make-interval 5 15 1 1))
<interval-[5,10]>
> (fundamentals:interval-width (fundamentals:make-interval 3 8 1 1))
5

;; 137. Ranges
> (fundamentals:make-range 0 10 2)  ; 0, 2, 4, 6, 8
<range-0-10-2>
> (fundamentals:range->list (fundamentals:make-range 1 10 2))
<list-1-3-5-7-9>
> (fundamentals:range-contains? (fundamentals:make-range 0 20 3) 12)
true
> (fundamentals:range-length (fundamentals:make-range 0 100 5))
20
> (fundamentals:range-nth (fundamentals:make-range 10 50 10) 2)
30

;; 138. Dates
> (fundamentals:make-date 2024 12 25)
<date-2024-12-25>
> (fundamentals:date-leap-year? (fundamentals:make-date 2024 1 1))
true
> (fundamentals:date-add-days (fundamentals:make-date 2024 12 25) 7)
<date-2025-01-01>
> (fundamentals:date-diff (fundamentals:make-date 2024 12 31) 
                          (fundamentals:make-date 2024 12 1))
30
> (fundamentals:date-day-of-week (fundamentals:make-date 2024 12 25))
3  ; Wednesday

;; 139. Times
> (fundamentals:make-time 14 30 45 0)  ; 14:30:45
<time-14:30:45>
> (fundamentals:time-add (fundamentals:make-time 23 45 30 0)
                         (fundamentals:make-time 0 30 40 0))
(<time-00:16:10> . 1)  ; Next day
> (fundamentals:time-to-seconds (fundamentals:make-time 1 30 45 0))
5445
> (fundamentals:seconds-to-time 3661)
<time-01:01:01>

;; 140. DateTimes
> (fundamentals:make-datetime (fundamentals:make-date 2024 12 25)
                              (fundamentals:make-time 15 30 0 0)
                              -300)  ; EST (UTC-5)
<datetime-2024-12-25T15:30:00-05:00>
> (fundamentals:datetime-to-utc 
    (fundamentals:make-datetime (fundamentals:make-date 2024 12 25)
                                (fundamentals:make-time 15 30 0 0)
                                -300))
<datetime-2024-12-25T20:30:00Z>

;; 141. Durations
> (fundamentals:make-duration 1 2 30 45 0)  ; 1 day, 2:30:45
<duration-1d-2h-30m-45s>
> (fundamentals:duration-add (fundamentals:make-duration 0 1 30 0 0)
                             (fundamentals:make-duration 0 2 45 30 0))
<duration-0d-4h-15m-30s>
> (fundamentals:duration-to-seconds (fundamentals:make-duration 0 1 30 45 0))
5445
> (fundamentals:duration-mult (fundamentals:make-duration 0 0 30 0 0) 3)
<duration-0d-1h-30m-0s>

;; 142. Complex numbers
> (fundamentals:make-complex (fundamentals:integer->rational (fundamentals:nat->integer 3))
                             (fundamentals:integer->rational (fundamentals:nat->integer 4)))
<complex-3+4i>
> (fundamentals:complex-add (fundamentals:make-complex 
                              (fundamentals:make-rational (fundamentals:nat->integer 1) 
                                                          (fundamentals:nat->integer 1))
                              (fundamentals:make-rational (fundamentals:nat->integer 2) 
                                                          (fundamentals:nat->integer 1)))
                            (fundamentals:make-complex 
                              (fundamentals:make-rational (fundamentals:nat->integer 3) 
                                                          (fundamentals:nat->integer 1))
                              (fundamentals:make-rational (fundamentals:nat->integer 1) 
                                                          (fundamentals:nat->integer 1))))
<complex-4+3i>
> (fundamentals:complex-mult (fundamentals:make-complex 
                               (fundamentals:integer->rational (fundamentals:nat->integer 2))
                               (fundamentals:integer->rational (fundamentals:nat->integer 3)))
                             (fundamentals:make-complex 
                               (fundamentals:integer->rational (fundamentals:nat->integer 1))
                               (fundamentals:integer->rational (fundamentals:make-integer 1 2))))
<complex-8-1i>
> (fundamentals:complex-conjugate (fundamentals:make-complex 
                                    (fundamentals:integer->rational (fundamentals:nat->integer 3))
                                    (fundamentals:integer->rational (fundamentals:nat->integer 4))))
<complex-3-4i>
> (fundamentals:complex-magnitude (fundamentals:make-complex 
                                    (fundamentals:integer->rational (fundamentals:nat->integer 3))
                                    (fundamentals:integer->rational (fundamentals:nat->integer 4))))
<rational-25/1>  ; 3² + 4² = 25

;; 143. Matrices
> (fundamentals:make-matrix 2 2 (vectors:vector-from-list 
                                  (lists:cons 1 (lists:cons 2 
                                    (lists:cons 3 (lists:cons 4 (lists:nil)))))))
<matrix-2x2>
> (fundamentals:matrix-ref <matrix> 1 0)
3
> (fundamentals:matrix-add (fundamentals:make-matrix 2 2 
                              (vectors:vector-from-list 
                                (lists:cons 1 (lists:cons 2 
                                  (lists:cons 3 (lists:cons 4 (lists:nil)))))))
                           (fundamentals:make-matrix 2 2 
                             (vectors:vector-from-list 
                               (lists:cons 5 (lists:cons 6 
                                 (lists:cons 7 (lists:cons 8 (lists:nil))))))))
<matrix-2x2-sum>
> (fundamentals:matrix-transpose (fundamentals:make-matrix 2 3 
                                   (vectors:vector-from-list 
                                     (lists:cons 1 (lists:cons 2 (lists:cons 3 
                                       (lists:cons 4 (lists:cons 5 (lists:cons 6 
                                         (lists:nil)))))))))
<matrix-3x2>
> (fundamentals:matrix-identity 3)
<matrix-3x3-identity>

;; 144. Lazy sequences
> (fundamentals:lazy-repeat 42)
<lazy-seq-infinite-42s>
> (fundamentals:lazy-take 5 (fundamentals:lazy-repeat 7))
<list-7-7-7-7-7>
> (fundamentals:lazy-take 10 (fundamentals:lazy-iterate (lambda (x) (arithmetic:add x 2)) 0))
<list-0-2-4-6-8-10-12-14-16-18>
> (fundamentals:lazy-take 5 (fundamentals:lazy-map (lambda (x) (arithmetic:mult x x))
                                                   (fundamentals:lazy-range 1 100 1)))
<list-1-4-9-16-25>
> (fundamentals:lazy-take 5 (fundamentals:lazy-filter arithmetic:prime?
                                                      (fundamentals:lazy-range 2 100 1)))
<list-2-3-5-7-11>

;; 145. Bit vectors
> (fundamentals:make-bitvector 8 170)  ; 10101010
<bitvector-8-170>
> (fundamentals:bitvector-ref (fundamentals:make-bitvector 8 170) 1)
1
> (fundamentals:bitvector-ref (fundamentals:make-bitvector 8 170) 0)
0
> (fundamentals:bitvector-set (fundamentals:make-bitvector 8 0) 3 1)
<bitvector-8-8>
> (fundamentals:bitvector-and (fundamentals:make-bitvector 8 170)  ; 10101010
                              (fundamentals:make-bitvector 8 204)) ; 11001100
<bitvector-8-136>  ; 10001000
> (fundamentals:bitvector-or (fundamentals:make-bitvector 8 170)   ; 10101010
                             (fundamentals:make-bitvector 8 85))   ; 01010101
<bitvector-8-255>  ; 11111111
> (fundamentals:bitvector-xor (fundamentals:make-bitvector 8 170)  ; 10101010
                              (fundamentals:make-bitvector 8 204)) ; 11001100
<bitvector-8-102>  ; 01100110
> (fundamentals:bitvector-not (fundamentals:make-bitvector 8 170))
<bitvector-8-85>   ; 01010101
> (fundamentals:bitvector-count (fundamentals:make-bitvector 8 170))
4  ; Four 1-bits
```

### Advanced Computational Examples

```lfe
;; 146. Expression evaluation
> (datatypes:eval-expr (datatypes:make-expr-add 
                         (datatypes:make-expr-literal 5)
                         (datatypes:make-expr-mult 
                           (datatypes:make-expr-literal 3)
                           (datatypes:make-expr-literal 4))))
17

;; 147. JSON-like data structures
> (datatypes:json-object (lists:cons (pairs:pair 'name (datatypes:json-string 'alice))
                                   (lists:cons (pairs:pair 'age (datatypes:json-number 30)) 
                                             (lists:nil))))
<json-object>

;; 148. Database tables
> (datatypes:make-table 'users 
                       (lists:cons 'id (lists:cons 'name (lists:cons 'age (lists:nil))))
                       (lists:cons (lists:cons 1 (lists:cons 'alice (lists:cons 30 (lists:nil))))
                                 (lists:nil)))
<table-users>

;; 149. Table select
> (datatypes:table-select (lambda (row) (peano:gt (lists:nth 2 row) 25)) <users-table>)
<filtered-table>

;; 150. State machines
> (datatypes:make-state-machine 
    'start
    (sets:set-from-list (lists:cons 'start (lists:cons 'end (lists:nil))))
    (lists:cons (pairs:pair (pairs:pair 'start 'a) 'end) (lists:nil)))
<state-machine>

;; 151. Computation state (VM-like)
> (compute:make-computation-state 
    (lists:cons 1 (lists:cons 2 (lists:nil)))  ; stack
    (maps:map-from-list (lists:cons (pairs:pair 'x 10) (lists:nil)))  ; memory
    0)  ; program counter
<computation-state>

;; 152. Execute instruction
> (compute:execute-instruction 
    (pairs:pair 'push 5)
    (compute:make-computation-state (lists:nil) (maps:map-empty) 0))
<new-computation-state>

;; 153. Proof steps
> (compute:make-proof-step 
    'modus-ponens
    (lists:cons 'p (lists:cons '(implies p q) (lists:nil)))
    'q)
<proof-step>

;; 154. Formal grammars
> (compute:make-grammar 
    (sets:set-from-list (lists:cons 'S (lists:cons 'A (lists:nil))))  ; non-terminals
    (sets:set-from-list (lists:cons 'a (lists:cons 'b (lists:nil))))  ; terminals
    'S  ; start symbol
    (lists:cons (pairs:pair 'S (lists:cons 'a (lists:cons 'A (lists:nil)))) (lists:nil)))  ; productions
<formal-grammar>

;; 155. Transfinite recursion
> (compute:transfinite-rec 
    (lambda (ord) (if (ordinals:ord-eq ord (ordinals:ord-zero)) 
                      0 
                      (arithmetic:add 1 (compute:transfinite-rec (ordinals:ord-pred ord)))))
    (ordinals:ord-finite 5))
5

;; 156. Queue implementation using two lists
> (let ((queue (pairs:pair (lists:cons 1 (lists:cons 2 (lists:nil)))  ; front
                          (lists:cons 4 (lists:cons 3 (lists:nil))))))  ; back (reversed)
    queue)
<queue-1-2-3-4>

;; 157. Graph representation as adjacency list
> (let ((graph (maps:map-from-list 
                 (lists:cons (pairs:pair 1 (lists:cons 2 (lists:cons 3 (lists:nil))))
                           (lists:cons (pairs:pair 2 (lists:cons 1 (lists:cons 3 (lists:nil))))
                                     (lists:cons (pairs:pair 3 (lists:cons 1 (lists:cons 2 (lists:nil))))
                                               (lists:nil)))))))
    (maps:map-get 1 graph))
<list-2-3>

;; 158. Infinite stream simulation (first n primes)
> (let ((primes-up-to (lambda (n)
                        (lists:filter arithmetic:prime? 
                                     (lists:map (lambda (i) (arithmetic:add i 2))
                                               (lists:range 0 (arithmetic:sub n 2)))))))
    (primes-up-to 20))
<list-2-3-5-7-11-13-17-19>

;; 159. Church encoding simulation
> (let ((church-zero (lambda (f x) x))
        (church-succ (lambda (n) (lambda (f x) (f ((n f) x)))))
        (church-add (lambda (m n) (lambda (f x) ((m f) ((n f) x))))))
    (let ((two (church-succ (church-succ church-zero)))
          (three (church-succ (church-succ (church-succ church-zero)))))
      (((church-add two three) (lambda (x) (arithmetic:add x 1))) 0)))
5

;; 160. Y combinator (fixed-point combinator)
> (let ((Y (lambda (f)
             ((lambda (x) (f (lambda (y) ((x x) y))))
              (lambda (x) (f (lambda (y) ((x x) y))))))))
    (let ((fact-gen (lambda (f)
                      (lambda (n)
                        (if (peano:zero? n)
                            1
                            (arithmetic:mult n (f (peano:p n))))))))
      ((Y fact-gen) 5)))
120

;; 161. Memoization with lazy sequences
> (let ((fib-seq (fundamentals:lazy-iterate 
                   (lambda (pair)
                     (pairs:pair (pairs:second pair)
                                (arithmetic:add (pairs:first pair) 
                                               (pairs:second pair))))
                   (pairs:pair 0 1))))
    (lists:map pairs:first (fundamentals:lazy-take 10 fib-seq)))
<list-0-1-1-2-3-5-8-13-21-34>

;; 162. Binary search tree operations
> (let ((bst-insert (lambda (tree key value)
                      (if (peano:zero? tree)
                          (adt:node (pairs:pair key value) (adt:leaf 0) (adt:leaf 0))
                          (let ((node-kv (adt:node-value tree))
                                (left (adt:node-left tree))
                                (right (adt:node-right tree)))
                            (if (peano:lt key (pairs:first node-kv))
                                (adt:node node-kv (bst-insert left key value) right)
                                (adt:node node-kv left (bst-insert right key value))))))))
    (bst-insert (bst-insert (bst-insert (adt:leaf 0) 5 'five) 3 'three) 7 'seven))
<balanced-bst>

;; 163. Polynomial arithmetic
> (let ((poly-add (lambda (p1 p2)
                    (lists:map (lambda (pair)
                                 (arithmetic:add (pairs:first pair) 
                                                (pairs:second pair)))
                               (lists:zip p1 p2)))))
    (poly-add (lists:cons 1 (lists:cons 2 (lists:cons 3 (lists:nil))))  ; 3x² + 2x + 1
              (lists:cons 5 (lists:cons 0 (lists:cons 1 (lists:nil))))))  ; x² + 0x + 5
<list-6-2-4>  ; 4x² + 2x + 6

;; 164. Prime factorization
> (let ((factor (lambda (n)
                  (let loop ((n n) (d 2) (factors (lists:nil)))
                    (cond
                      ((peano:eq n 1) (lists:reverse factors))
                      ((peano:zero? (arithmetic:mod n d))
                       (loop (arithmetic:div n d) d (lists:cons d factors)))
                      (else (loop n (peano:s d) factors)))))))
    (factor 60))
<list-2-2-3-5>

;; 165. Run-length encoding
> (let ((rle-encode (lambda (lst)
                      (if (lists:nil? lst)
                          (lists:nil)
                          (let loop ((current (lists:head lst))
                                     (count 1)
                                     (rest (lists:tail lst))
                                     (result (lists:nil)))
                            (cond
                              ((lists:nil? rest)
                               (lists:reverse (lists:cons (pairs:pair current count) result)))
                              ((peano:eq current (lists:head rest))
                               (loop current (peano:s count) (lists:tail rest) result))
                              (else
                               (loop (lists:head rest) 1 (lists:tail rest)
                                     (lists:cons (pairs:pair current count) result)))))))))
    (rle-encode (lists:cons 1 (lists:cons 1 (lists:cons 2 (lists:cons 2 
                  (lists:cons 2 (lists:cons 3 (lists:cons 3 (lists:nil))))))))))
<list-(1.2)-(2.3)-(3.2)>
```

## Academic Significance

This project addresses theoretical questions about what Peano Arithmetic can prove about itself. For example, while PA can prove each individual Goodstein sequence terminates, it cannot prove "all Goodstein sequences terminate". This implementation demonstrates PA's computational completeness through practical construction.

## Future Work

See `ROADMAP.md` and `TODO-50.md` for extensive plans including:
- Mathematical structures (rationals, matrices, polynomials)
- Advanced algorithms (FFT, graph algorithms, optimization)
- Computational models (Turing machines, lambda calculus)
- Type theory concepts (dependent types, linear types)
- Concurrent models (actors, CSP, STM)

## License

[License information to be added]

## Acknowledgments

Inspired by discussions on Stack Overflow about the computational power of Peano Arithmetic and the theoretical foundations of mathematics and computation.