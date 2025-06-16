(defmodule adt
  (export all)
  (import (from peano (zero 0) (s 1) (eq 2))
          (from pairs (pair 2) (first 1) (second 1))
          (from lists (nil 0) (cons 2))
          (from types (make-typed 2) (type-of 1) (value-of 1))))

;;; Algebraic Data Types (Sum and Product Types)
;;; Sum types: encoded as (constructor-tag, data)
;;; Product types: encoded as tuples

;; Type tag for ADTs
(defun type-adt ()
  (s (s (s (s (s (s (s (s (s (s (s (s (zero))))))))))))))

;; Create ADT value
(defun make-adt (constructor-tag data)
  (make-typed (type-adt) (pair constructor-tag data)))

;; Get constructor tag
(defun adt-constructor (adt)
  (first (value-of adt)))

;; Get constructor data
(defun adt-data (adt)
  (second (value-of adt)))

;; Check if value is ADT
(defun is-adt? (val)
  (eq (type-of val) (type-adt)))

;; Pattern matching helper
(defun adt-match? (adt constructor-tag)
  (eq (adt-constructor adt) constructor-tag))

;;; Option/Maybe type
;; Constructors
(defun none-tag () (zero))
(defun some-tag () (s (zero)))

(defun none ()
  (make-adt (none-tag) (nil)))

(defun some (value)
  (make-adt (some-tag) value))

;; Pattern matching
(defun is-none? (opt)
  (adt-match? opt (none-tag)))

(defun is-some? (opt)
  (adt-match? opt (some-tag)))

;; Extract value from Some
(defun some-value (opt)
  (if (is-some? opt)
      (adt-data opt)
      (error "cannot extract value from none")))

;; Option operations
(defun option-map (f opt)
  (if (is-none? opt)
      (none)
      (some (funcall f (some-value opt)))))

(defun option-bind (opt f)
  (if (is-none? opt)
      (none)
      (funcall f (some-value opt))))

(defun option-or-else (opt default)
  (if (is-none? opt)
      default
      (some-value opt)))

;;; Either/Result type
;; Constructors
(defun left-tag () (zero))
(defun right-tag () (s (zero)))

(defun left (value)
  (make-adt (left-tag) value))

(defun right (value)
  (make-adt (right-tag) value))

;; Pattern matching
(defun is-left? (either)
  (adt-match? either (left-tag)))

(defun is-right? (either)
  (adt-match? either (right-tag)))

;; Extract values
(defun left-value (either)
  (if (is-left? either)
      (adt-data either)
      (error "not a left value")))

(defun right-value (either)
  (if (is-right? either)
      (adt-data either)
      (error "not a right value")))

;; Either operations
(defun either-map (f either)
  (if (is-left? either)
      either
      (right (funcall f (right-value either)))))

(defun either-map-left (f either)
  (if (is-right? either)
      either
      (left (funcall f (left-value either)))))

(defun either-bind (either f)
  (if (is-left? either)
      either
      (funcall f (right-value either))))

;;; List ADT (alternative implementation)
;; Constructors
(defun nil-tag () (zero))
(defun cons-tag () (s (zero)))

(defun list-nil ()
  (make-adt (nil-tag) (nil)))

(defun list-cons (head tail)
  (make-adt (cons-tag) (pair head tail)))

(defun is-list-nil? (lst)
  (adt-match? lst (nil-tag)))

(defun is-list-cons? (lst)
  (adt-match? lst (cons-tag)))

(defun list-head (lst)
  (first (adt-data lst)))

(defun list-tail (lst)
  (second (adt-data lst)))

;;; Tree ADT
;; Constructors
(defun leaf-tag () (zero))
(defun node-tag () (s (zero)))

(defun leaf (value)
  (make-adt (leaf-tag) value))

(defun node (left value right)
  (make-adt (node-tag) 
            (lists:list left value right)))

(defun is-leaf? (tree)
  (adt-match? tree (leaf-tag)))

(defun is-node? (tree)
  (adt-match? tree (node-tag)))

(defun leaf-value (tree)
  (adt-data tree))

(defun node-left (tree)
  (lists:nth (adt-data tree) (zero)))

(defun node-value (tree)
  (lists:nth (adt-data tree) (s (zero))))

(defun node-right (tree)
  (lists:nth (adt-data tree) (s (s (zero)))))

;; Tree operations
(defun tree-map (f tree)
  (if (is-leaf? tree)
      (leaf (funcall f (leaf-value tree)))
      (node (tree-map f (node-left tree))
            (funcall f (node-value tree))
            (tree-map f (node-right tree)))))

(defun tree-fold (f acc tree)
  (if (is-leaf? tree)
      (funcall f acc (leaf-value tree))
      (let* ((acc1 (tree-fold f acc (node-left tree)))
             (acc2 (funcall f acc1 (node-value tree))))
        (tree-fold f acc2 (node-right tree)))))

;;; Custom ADT builder
(defun define-adt (constructors)
  ;; Returns a list of constructor functions
  (define-constructors constructors (zero)))

(defun define-constructors
  ((() _) (nil))
  (((cons (cons name arity) rest) tag)
   (cons (pair name (pair tag arity))
         (define-constructors rest (s tag)))))

;;; Boolean ADT
(defun false-tag () (zero))
(defun true-tag () (s (zero)))

(defun bool-false ()
  (make-adt (false-tag) (nil)))

(defun bool-true ()
  (make-adt (true-tag) (nil)))

(defun is-false? (b)
  (adt-match? b (false-tag)))

(defun is-true? (b)
  (adt-match? b (true-tag)))

;;; Comparison Result ADT
(defun lt-tag () (zero))
(defun eq-tag () (s (zero)))
(defun gt-tag () (s (s (zero))))

(defun cmp-lt ()
  (make-adt (lt-tag) (nil)))

(defun cmp-eq ()
  (make-adt (eq-tag) (nil)))

(defun cmp-gt ()
  (make-adt (gt-tag) (nil)))