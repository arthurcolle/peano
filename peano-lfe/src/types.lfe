(defmodule types
  (export all)
  (import (from peano (zero 0) (s 1) (eq 2) (zero? 1))
          (from pairs (pair 2) (first 1) (second 1))
          (from lists (list 0) (nil 0))))

;;; General type encoding system
;;; Every value is encoded as (type-tag, value)

;; Type tags
(defun type-number () (zero))
(defun type-boolean () (s (zero)))
(defun type-pair () (s (s (zero))))
(defun type-list () (s (s (s (zero)))))
(defun type-function () (s (s (s (s (zero))))))
(defun type-symbol () (s (s (s (s (s (zero)))))))
(defun type-string () (s (s (s (s (s (s (zero))))))))
(defun type-tree () (s (s (s (s (s (s (s (zero)))))))))
(defun type-graph () (s (s (s (s (s (s (s (s (zero))))))))))
(defun type-hypergraph () (s (s (s (s (s (s (s (s (s (zero)))))))))))

;; Create typed value
(defun make-typed (type-tag value)
  (pair type-tag value))

;; Get type of value
(defun type-of (typed-val)
  (first typed-val))

;; Get raw value
(defun value-of (typed-val)
  (second typed-val))

;; Type predicates
(defun number? (val)
  (eq (type-of val) (type-number)))

(defun boolean? (val)
  (eq (type-of val) (type-boolean)))

(defun pair? (val)
  (eq (type-of val) (type-pair)))

(defun list? (val)
  (eq (type-of val) (type-list)))

;; Constructors for typed values
(defun make-number (n)
  (make-typed (type-number) n))

(defun make-boolean (b)
  (make-typed (type-boolean) (if b (s (zero)) (zero))))

(defun make-pair (a b)
  (make-typed (type-pair) (pair a b)))

(defun make-list (lst)
  (make-typed (type-list) lst))

;; Boolean values
(defun true () (make-boolean 'true))
(defun false () (make-boolean 'false))

;; Extract boolean value
(defun boolean-value (typed-bool)
  (if (zero? (value-of typed-bool)) 'false 'true))

;; Symbol encoding (as list of numbers)
(defun make-symbol (name-as-list)
  (make-typed (type-symbol) name-as-list))

;; String encoding (as list of character codes)
(defun make-string (char-list)
  (make-typed (type-string) char-list))

;; Tree structure
(defun make-tree-node (value children)
  (make-typed (type-tree) (pair value children)))

(defun tree-value (tree)
  (first (value-of tree)))

(defun tree-children (tree)
  (second (value-of tree)))

;; Graph structure (adjacency list representation)
(defun make-graph (nodes edges)
  (make-typed (type-graph) (pair nodes edges)))

(defun graph-nodes (graph)
  (first (value-of graph)))

(defun graph-edges (graph)
  (second (value-of graph)))

;; Hypergraph structure
(defun make-hypergraph (nodes hyperedges)
  (make-typed (type-hypergraph) (pair nodes hyperedges)))

(defun hypergraph-nodes (hg)
  (first (value-of hg)))

(defun hypergraph-edges (hg)
  (second (value-of hg)))