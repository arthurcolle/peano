(defmodule lists
  (export all)
  (import (from peano (zero 0) (s 1) (p 1) (zero? 1) (eq 2))
          (from pairs (pair 2) (first 1) (second 1))))

;; Aliases for compatibility
(defun cons (head tail) (list-cons head tail))
(defun list (args)
  (make-list args))
(defun map (f lst) (list-map f lst))

;;; List implementation using pairs
;;; nil = 0, cons cell = pair of (s head) tail

;; Empty list
(defun nil () (zero))

;; Check if list is empty
(defun nil? (lst)
  (zero? lst))

;; List-cons - add element to front of list
(defun list-cons (head tail)
  (pair (s head) tail))

;; Head of list
(defun head (lst)
  (if (nil? lst)
      (error "head of empty list")
      (p (first lst))))

;; Tail of list  
(defun tail (lst)
  (if (nil? lst)
      (error "tail of empty list")
      (second lst)))

;; List length
(defun length
  ((lst) (if (nil? lst) (zero) (s (length (tail lst))))))

;; Get nth element (0-indexed)
(defun nth
  ((lst n) (if (zero? n) (head lst) (nth (tail lst) (p n)))))

;; Append two lists
(defun append
  ((lst1 lst2) (if (nil? lst1) lst2 (list-cons (head lst1) (append (tail lst1) lst2)))))

;; Reverse a list
(defun reverse (lst)
  (reverse-helper lst (nil)))

(defun reverse-helper
  ((lst acc) (if (nil? lst) acc (reverse-helper (tail lst) (list-cons (head lst) acc)))))

;; Map function over list
(defun list-map
  ((f lst) (if (nil? lst) (nil) (list-cons (funcall f (head lst)) (list-map f (tail lst))))))

;; Filter list by predicate
(defun filter
  ((pred lst) 
   (cond ((nil? lst) (nil))
         ((funcall pred (head lst)) (list-cons (head lst) (filter pred (tail lst))))
         ('true (filter pred (tail lst))))))

;; Fold left
(defun foldl
  ((f acc lst) (if (nil? lst) acc (foldl f (funcall f acc (head lst)) (tail lst)))))

;; Fold right
(defun foldr
  ((f acc lst) (if (nil? lst) acc (funcall f (head lst) (foldr f acc (tail lst))))))

;; Create list from elements
(defun make-list (args)
  (if (== args '())
      (nil)
      (list-cons (car args) (make-list (cdr args)))))

;; Insert at position
(defun insert-at
  ((lst n val) (if (zero? n) (list-cons val lst) (list-cons (head lst) (insert-at (tail lst) (p n) val)))))

;; Remove at position
(defun remove-at
  ((lst n) (if (zero? n) (tail lst) (list-cons (head lst) (remove-at (tail lst) (p n))))))

;; Find first element satisfying predicate
(defun find
  ((pred lst)
   (cond ((nil? lst) 'not-found)
         ((funcall pred (head lst)) (head lst))
         ('true (find pred (tail lst))))))

;; Check if element is in list
(defun member? (x lst)
  (case (find (lambda (y) (eq x y)) lst)
    ('not-found 'false)
    (_ 'true)))