#!/usr/bin/env lfescript
;;; Examples demonstrating the computational power of Peano arithmetic in LFE

(defun main (args)
  ;; Basic arithmetic
  (io:format "Basic Peano arithmetic:~n")
  (let ((five (peano:nat->peano 5))
        (three (peano:nat->peano 3)))
    (io:format "  5 + 3 = ~p~n" 
               (list (peano:peano->nat (arithmetic:add five three))))
    (io:format "  5 * 3 = ~p~n" 
               (list (peano:peano->nat (arithmetic:mult five three))))
    (io:format "  5! = ~p~n" 
               (list (peano:peano->nat (arithmetic:fact five)))))
  
  ;; Pair encoding
  (io:format "~nPair encoding:~n")
  (let* ((p (pairs:pair (peano:nat->peano 7) (peano:nat->peano 11)))
         (h (pairs:first p))
         (t (pairs:second p)))
    (io:format "  pair(7, 11) encoded as: ~p~n" (list (peano:peano->nat p)))
    (io:format "  first: ~p, second: ~p~n" 
               (list (peano:peano->nat h) (peano:peano->nat t))))
  
  ;; List operations
  (io:format "~nList operations:~n")
  (let* ((lst (lists:list (peano:nat->peano 1) 
                          (peano:nat->peano 2) 
                          (peano:nat->peano 3)))
         (len (lists:length lst)))
    (io:format "  list length: ~p~n" (list (peano:peano->nat len)))
    (io:format "  reversed: ~p~n" 
               (list (lists:map #'peano:peano->nat/1 
                               (lists:reverse lst)))))
  
  ;; Ordinals
  (io:format "~nOrdinal arithmetic:~n")
  (let* ((w (ordinals:omega))
         (five (ordinals:ord-finite (peano:nat->peano 5)))
         (w+5 (ordinals:ord-add w five)))
    (io:format "  ω + 5 constructed~n")
    (io:format "  ω < ω+5? ~p~n" (list (ordinals:ord-lt w w+5))))
  
  ;; Hypergraphs
  (io:format "~nHypergraph example:~n")
  (let* ((hg (hypergraphs:empty-hypergraph))
         (hg1 (hypergraphs:add-hyperedge hg 
                (lists:list (peano:nat->peano 1)
                           (peano:nat->peano 2)
                           (peano:nat->peano 3))))
         (hg2 (hypergraphs:add-hyperedge hg1
                (lists:list (peano:nat->peano 2)
                           (peano:nat->peano 3)
                           (peano:nat->peano 4)))))
    (io:format "  Created hypergraph with 2 hyperedges~n")
    (io:format "  Nodes: 1, 2, 3, 4~n")
    (io:format "  Edges: {1,2,3}, {2,3,4}~n"))
  
  (io:format "~nThis demonstrates how PA can encode:~n")
  (io:format "  - Natural numbers and arithmetic~n")
  (io:format "  - Pairs and tuples~n")
  (io:format "  - Lists and recursive structures~n")
  (io:format "  - Ordinals up to ε₀~n")
  (io:format "  - Complex structures like hypergraphs~n")
  (io:format "  - And therefore, all of computation!~n"))