(defmodule datatypes
  (export all)
  (import (from peano (zero 0) (s 1) (nat->peano 1) (peano->nat 1))
          (from arithmetic (add 2) (mult 2))
          (from types (make-number 1) (make-string 1) (make-symbol 1))
          (from tuples (tuple 0) (tuple-ref 2) (tuple-arity 1))
          (from records (make-person 2) (make-point 2) (record-get 2))
          (from adt (none 0) (some 1) (left 1) (right 1) 
                    (leaf 1) (node 3) (option-map 2))
          (from vectors (vector-from-list 1) (vector-ref 2) (vector-map 2))
          (from maps (map-from-list 1) (map-get 2) (map-insert 3))
          (from sets (set-from-list 1) (set-union 2) (set-member? 2))
          (from lists (list 0) (map 2))))

;;; Comprehensive examples demonstrating all data types

;; Example: Complex nested data structure
(defun make-company (name employees departments)
  (records:record (company-type-id)
                  (name-field) name
                  (employees-field) employees
                  (departments-field) departments))

;; Type IDs
(defun company-type-id () (s (s (zero))))
(defun employee-type-id () (s (s (s (zero)))))
(defun department-type-id () (s (s (s (s (zero))))))

;; Field IDs
(defun name-field () (zero))
(defun employees-field () (s (s (s (s (s (s (s (zero)))))))))
(defun departments-field () (s (s (s (s (s (s (s (s (zero))))))))))
(defun manager-field () (s (s (s (s (s (s (s (s (s (zero)))))))))))
(defun members-field () (s (s (s (s (s (s (s (s (s (s (zero))))))))))))

;; Example: Building a type-safe expression tree
(defun make-expr-literal (n)
  (adt:make-adt (expr-literal-tag) n))

(defun make-expr-add (left right)
  (adt:make-adt (expr-add-tag) (tuple left right)))

(defun make-expr-mult (left right)
  (adt:make-adt (expr-mult-tag) (tuple left right)))

;; Expression tags
(defun expr-literal-tag () (zero))
(defun expr-add-tag () (s (zero)))
(defun expr-mult-tag () (s (s (zero))))

;; Evaluate expression
(defun eval-expr (expr)
  (let ((tag (adt:adt-constructor expr)))
    (cond
      ((peano:eq tag (zero))  ; literal
       (adt:adt-data expr))
      ((peano:eq tag (s (zero)))  ; add
       (let ((operands (adt:adt-data expr)))
         (add (eval-expr (tuple-ref operands (zero)))
              (eval-expr (tuple-ref operands (s (zero)))))))
      ((peano:eq tag (s (s (zero))))  ; mult
       (let ((operands (adt:adt-data expr)))
         (mult (eval-expr (tuple-ref operands (zero)))
               (eval-expr (tuple-ref operands (s (zero))))))))))

;; Example: Graph algorithms using our data structures
(defun make-graph-adt (nodes edges)
  (records:record (graph-type-id)
                  (nodes-field) nodes
                  (edges-field) edges))

(defun graph-type-id () (s (s (s (s (s (zero)))))))
(defun nodes-field () (s (s (s (s (s (s (s (s (s (s (s (zero)))))))))))))
(defun edges-field () (s (s (s (s (s (s (s (s (s (s (s (s (zero))))))))))))))

;; Dijkstra's algorithm skeleton
(defun dijkstra (graph start)
  (let* ((distances (map-from-list (list (pairs:pair start (zero)))))
         (visited (set-from-list (lists:nil)))
         (queue (vectors:vector-from-list (list start))))
    ;; Implementation would go here
    distances))

;; Example: Type-safe JSON-like structure
(defun json-null () (adt:make-adt (json-null-tag) (lists:nil)))
(defun json-bool (b) (adt:make-adt (json-bool-tag) b))
(defun json-number (n) (adt:make-adt (json-number-tag) n))
(defun json-string (s) (adt:make-adt (json-string-tag) s))
(defun json-array (arr) (adt:make-adt (json-array-tag) arr))
(defun json-object (obj) (adt:make-adt (json-object-tag) obj))

;; JSON tags
(defun json-null-tag () (zero))
(defun json-bool-tag () (s (zero)))
(defun json-number-tag () (s (s (zero))))
(defun json-string-tag () (s (s (s (zero)))))
(defun json-array-tag () (s (s (s (s (zero))))))
(defun json-object-tag () (s (s (s (s (s (zero)))))))

;; Example: Building a simple database schema
(defun make-table (name columns rows)
  (records:record (table-type-id)
                  (table-name-field) name
                  (columns-field) columns
                  (rows-field) rows))

(defun table-type-id () (s (s (s (s (s (s (zero))))))))
(defun table-name-field () (s (s (s (s (s (s (s (s (s (s (s (s (s (zero)))))))))))))))
(defun columns-field () (s (s (s (s (s (s (s (s (s (s (s (s (s (s (zero))))))))))))))))
(defun rows-field () (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (zero)))))))))))))))))

;; Query operations
(defun table-select (table predicate)
  (let ((rows (record-get table (rows-field))))
    (vectors:vector-from-list
     (lists:filter predicate (vectors:vector->list rows)))))

;; Example: State machine
(defun make-state-machine (states transitions initial)
  (records:record (fsm-type-id)
                  (states-field) states
                  (transitions-field) transitions
                  (current-field) initial))

(defun fsm-type-id () (s (s (s (s (s (s (s (zero)))))))))
(defun states-field () (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (zero))))))))))))))))))
(defun transitions-field () (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (zero)))))))))))))))))))
(defun current-field () (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (zero))))))))))))))))))))

;; Comprehensive test demonstrating all types
(defun demo-all-types ()
  (let* (;; Basic types
         (num (make-number (nat->peano 42)))
         (str (make-string (list (nat->peano 72) (nat->peano 105))))
         
         ;; Tuples
         (pair-tup (tuple (nat->peano 1) (nat->peano 2)))
         (triple-tup (tuple (nat->peano 1) (nat->peano 2) (nat->peano 3)))
         
         ;; Records
         (person (make-person (nat->peano 25) (nat->peano 30)))
         (point (make-point (nat->peano 10) (nat->peano 20)))
         
         ;; ADTs
         (opt-none (none))
         (opt-some (some (nat->peano 42)))
         (either-left (left (nat->peano 1)))
         (either-right (right (nat->peano 2)))
         
         ;; Trees
         (tree (node (leaf (nat->peano 1))
                    (nat->peano 2)
                    (leaf (nat->peano 3))))
         
         ;; Vectors
         (vec (vector-from-list (list (nat->peano 1) 
                                     (nat->peano 2) 
                                     (nat->peano 3))))
         
         ;; Maps
         (dict (map-from-list (list (pairs:pair (nat->peano 1) (nat->peano 10))
                                   (pairs:pair (nat->peano 2) (nat->peano 20)))))
         
         ;; Sets
         (set1 (set-from-list (list (nat->peano 1) (nat->peano 2) (nat->peano 3))))
         (set2 (set-from-list (list (nat->peano 2) (nat->peano 3) (nat->peano 4))))
         
         ;; Complex expression
         (expr (make-expr-add 
                (make-expr-mult (make-expr-literal (nat->peano 2))
                               (make-expr-literal (nat->peano 3)))
                (make-expr-literal (nat->peano 4)))))
    
    ;; Return all created structures
    (list num str pair-tup triple-tup person point
          opt-none opt-some either-left either-right
          tree vec dict set1 set2 expr)))