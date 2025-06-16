(defmodule arithmetic
  (export all)
  (import (from peano (s 1) (p 1) (zero 0) (zero? 1) (eq 2) (lt 2))))

;;; Basic arithmetic operations in Peano arithmetic

;; Addition
(defun add
  ((n m) (if (zero? n) m (s (add (p n) m)))))

;; Multiplication
(defun mult
  ((n m) (if (zero? n) (zero) (add (mult (p n) m) m))))

;; Exponentiation
(defun pow
  ((n m) (if (zero? m) (s (zero)) (mult n (pow n (p m))))))

;; Subtraction (partial function - only defined when n >= m)
(defun sub
  ((n m) 
   (cond ((zero? m) n)
         ((zero? n) (error "negative result"))
         ('true (sub (p n) (p m))))))

;; Integer division and modulo
;; Helper for modulo
(defun mod-helper
  ((n m acc) 
   (cond ((zero? n) acc)
         ((eq acc (p m)) (mod-helper (p n) m (zero)))
         ('true (mod-helper (p n) m (s acc))))))

;; Modulo operation
(defun mod (n m)
  (mod-helper n m (zero)))

;; Integer division
(defun div
  ((n m) (if (lt n m) (zero) (s (div (sub n m) m)))))

;; Factorial
(defun fact
  ((n) (if (zero? n) (s (zero)) (mult n (fact (p n))))))

;; Fibonacci
(defun fib
  ((n) 
   (cond ((zero? n) (zero))
         ((eq n (s (zero))) (s (zero)))
         ('true (add (fib (p n)) (fib (p (p n))))))))

;; Greatest common divisor (Euclidean algorithm)
(defun gcd
  ((n m) (if (zero? m) n (gcd m (mod n m)))))

;; Test if prime (naive algorithm)
(defun prime?-helper
  ((n d) 
   (cond ((eq d (s (zero))) 'true)
         ((zero? (mod n d)) 'false)
         ('true (prime?-helper n (p d))))))

(defun prime?
  ((n) (if (lt n (s (s (zero)))) 'false (prime?-helper n (p n)))))