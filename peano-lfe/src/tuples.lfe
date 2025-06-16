(defmodule tuples
  (export all)
  (import (from peano (zero 0) (s 1) (p 1) (zero? 1) (eq 2))
          (from pairs (pair 2) (first 1) (second 1))
          (from lists (nil 0) (cons 2) (head 1) (tail 1) (nth 2) (length 1))
          (from types (make-typed 2) (type-of 1) (value-of 1))))

;;; Tuples of arbitrary arity
;;; Encoded as (arity, list-of-elements)

;; Helper function
(defun nil? (x)
  (eq x (nil)))

;; Type tag for tuples
(defun type-tuple () 
  (s (s (s (s (s (s (s (s (s (s (zero))))))))))))

;; Create tuple from list of elements
(defun tuple-from-list (elements)
  (make-typed (type-tuple) 
              (pair (length elements) elements)))

;; Create tuple from arguments
(defun tuple
  (() (tuple-from-list (nil)))
  (args (tuple-from-list args)))

;; Get tuple arity
(defun tuple-arity (tup)
  (first (value-of tup)))

;; Get tuple element at position (0-indexed)
(defun tuple-ref (tup index)
  (nth (second (value-of tup)) index))

;; Set tuple element (returns new tuple)
(defun tuple-set (tup index value)
  (let ((elements (second (value-of tup))))
    (tuple-from-list (set-nth elements index value))))

;; Helper: set nth element of list
(defun set-nth (lst n val)
  (if (zero? n)
      (cons val (tail lst))
      (cons (head lst) (set-nth (tail lst) (p n) val))))

;; Convert tuple to list
(defun tuple->list (tup)
  (second (value-of tup)))

;; Map function over tuple
(defun tuple-map (f tup)
  (tuple-from-list (lists:map f (tuple->list tup))))

;; Fold over tuple
(defun tuple-fold (f acc tup)
  (lists:foldl f acc (tuple->list tup)))

;; Zip two tuples
(defun tuple-zip (tup1 tup2)
  (tuple-from-list (zip-lists (tuple->list tup1) (tuple->list tup2))))

;; Helper: zip two lists
(defun zip-lists (lst1 lst2)
  (if (or (nil? lst1) (nil? lst2))
      (nil)
      (cons (pair (head lst1) (head lst2))
            (zip-lists (tail lst1) (tail lst2)))))

;; Specific tuple constructors for common arities
(defun pair-tuple (a b)
  (tuple a b))

(defun triple-tuple (a b c)
  (tuple a b c))

(defun quadruple-tuple (a b c d)
  (tuple a b c d))

;; Pattern matching helpers
(defun is-tuple? (val)
  (eq (type-of val) (type-tuple)))

(defun tuple-match-arity? (tup arity)
  (eq (tuple-arity tup) arity))

;; Destructuring helpers
(defun tuple-first (tup)
  (tuple-ref tup (zero)))

(defun tuple-second (tup)
  (tuple-ref tup (s (zero))))

(defun tuple-third (tup)
  (tuple-ref tup (s (s (zero)))))

;; Tuple equality
(defun tuple-eq? (tup1 tup2)
  (and (eq (tuple-arity tup1) (tuple-arity tup2))
       (lists-eq? (tuple->list tup1) (tuple->list tup2))))

;; Helper: list equality
(defun lists-eq? (lst1 lst2)
  (cond
    ((and (nil? lst1) (nil? lst2)) 'true)
    ((or (nil? lst1) (nil? lst2)) 'false)
    ('true (and (eq (head lst1) (head lst2))
                (lists-eq? (tail lst1) (tail lst2))))))