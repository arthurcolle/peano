(defmodule peano
  (export all))

;;; Peano Arithmetic in LFE
;;; Based on the Stack Overflow answer about encoding computation in PA

;; Zero representation
(defun zero () 0)

;; Successor function
(defun s (n) `#(s ,n))

;; Predecessor function - undefined for zero
(defun p
  ((0) (error "predecessor of zero is undefined"))
  ((`#(s ,n)) n))

;; Check if zero
(defun zero? (n)
  (=:= n 0))

;; Equality check
(defun eq
  ((0 0) 'true)
  ((0 _) 'false)
  ((_ 0) 'false)
  ((`#(s ,n) `#(s ,m)) (eq n m)))

;; Convert natural number to peano
(defun nat->peano (n)
  (if (=:= n 0)
      0
      (s (nat->peano (- n 1)))))

;; Convert peano to natural number (for debugging)
(defun peano->nat
  ((0) 0)
  ((`#(s ,n)) (+ 1 (peano->nat n))))

;; Less than comparison
(defun lt (n m)
  (cond
    ((eq m 0) 'false)
    ((eq n 0) 'true)
    ('true (lt (p n) (p m)))))

;; Greater than
(defun gt (n m)
  (lt m n))

;; Less than or equal
(defun lte (n m)
  (or (lt n m) (eq n m)))

;; Greater than or equal  
(defun gte (n m)
  (or (gt n m) (eq n m)))

;; Min and max
(defun min (n m)
  (if (lt n m) n m))

(defun max (n m)
  (if (lt n m) m n))