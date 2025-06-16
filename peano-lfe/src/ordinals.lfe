(defmodule ordinals
  (export all)
  (import (from peano (zero 0) (s 1) (p 1) (zero? 1) (eq 2) (lt 2))
          (from pairs (pair 2) (first 1) (second 1))
          (from lists (nil 0) (nil? 1) (cons 2) (head 1) (tail 1))
          (from arithmetic (add 2) (mult 2))))

;;; Ordinals in Cantor Normal Form
;;; Represented as list of pairs (coefficient, exponent)
;;; where exponent is also an ordinal

;; Zero ordinal
(defun ord-zero () (nil))

;; Check if ordinal is zero
(defun ord-zero? (ord)
  (nil? ord))

;; Finite ordinal from natural number
(defun ord-finite (n)
  (if (zero? n)
      (ord-zero)
      (cons (pair n (ord-zero)) (nil))))

;; Omega (first infinite ordinal)
(defun omega ()
  (cons (pair (s (zero)) (ord-finite (s (zero)))) (nil)))

;; Get leading term
(defun ord-leading-term (ord)
  (if (ord-zero? ord)
      (error "no leading term of zero ordinal")
      (head ord)))

;; Get coefficient of leading term
(defun ord-leading-coeff (ord)
  (first (ord-leading-term ord)))

;; Get exponent of leading term
(defun ord-leading-exp (ord)
  (second (ord-leading-term ord)))

;; Compare ordinals lexicographically
(defun ord-lt (ord1 ord2)
  (cond
    ((ord-zero? ord1) (not (ord-zero? ord2)))
    ((ord-zero? ord2) 'false)
    ('true
     (let ((exp1 (ord-leading-exp ord1))
           (exp2 (ord-leading-exp ord2)))
       (cond
         ((ord-lt exp1 exp2) 'true)
         ((ord-lt exp2 exp1) 'false)
         ('true 
          (let ((coeff1 (ord-leading-coeff ord1))
                (coeff2 (ord-leading-coeff ord2)))
            (cond
              ((lt coeff1 coeff2) 'true)
              ((lt coeff2 coeff1) 'false)
              ('true (ord-lt (tail ord1) (tail ord2))))))))))

;; Ordinal equality
(defun ord-eq (ord1 ord2)
  (and (not (ord-lt ord1 ord2))
       (not (ord-lt ord2 ord1))))

;; Add ordinals
(defun ord-add (ord1 ord2)
  (cond
    ((ord-zero? ord1) ord2)
    ((ord-zero? ord2) ord1)
    ('true
     (let ((exp1 (ord-leading-exp ord1))
           (exp2 (ord-leading-exp ord2)))
       (cond
         ((ord-lt exp1 exp2) ord2)
         ((ord-lt exp2 exp1) (cons (head ord1) (ord-add (tail ord1) ord2)))
         ('true
          (cons (pair (add (ord-leading-coeff ord1)
                           (ord-leading-coeff ord2))
                      exp1)
                (tail ord2))))))))

;; Multiply ordinal by natural number
(defun ord-mult-nat (ord n)
  (cond
    ((zero? n) (ord-zero))
    ((ord-zero? ord) (ord-zero))
    ('true
     (cons (pair (mult (ord-leading-coeff ord) n)
                 (ord-leading-exp ord))
           (tail ord)))))

;; Multiply ordinals
(defun ord-mult (ord1 ord2)
  (cond
    ((ord-zero? ord1) (ord-zero))
    ((ord-zero? ord2) (ord-zero))
    ('true
     (ord-add-terms 
      (ord-mult-term (head ord1) ord2)
      (ord-mult (tail ord1) ord2)))))

;; Helper: multiply term by ordinal
(defun ord-mult-term (term ord)
  (if (ord-zero? ord)
      (ord-zero)
      (let ((coeff1 (first term))
            (exp1 (second term)))
        (cons (pair (mult coeff1 (ord-leading-coeff ord))
                    (ord-add exp1 (ord-leading-exp ord)))
              (ord-mult-term term (tail ord))))))

;; Helper: add terms maintaining order
(defun ord-add-terms (terms ord)
  (cond
    ((nil? terms) ord)
    ('true (ord-add (cons (head terms) (nil))
                    (ord-add-terms (tail terms) ord)))))

;; Ordinal exponentiation (omega^ord)
(defun omega-to (ord)
  (if (ord-zero? ord)
      (ord-finite (s (zero)))
      (cons (pair (s (zero)) ord) (nil))))

;; Convert ordinal to hereditary base notation
(defun ord-to-hereditary (ord base)
  (if (ord-zero? ord)
      (zero)
      (add (mult (ord-leading-coeff ord)
                 (pow-hereditary base (ord-to-hereditary 
                                      (ord-leading-exp ord) base)))
           (ord-to-hereditary (tail ord) base))))

;; Helper for hereditary base
(defun pow-hereditary (base exp)
  (if (zero? exp)
      (s (zero))
      (mult base (pow-hereditary base (p exp)))))

;; Goodstein sequence step
(defun goodstein-step (n base)
  (let ((hereditary (to-hereditary-base n base)))
    (p (from-hereditary-base hereditary (s base)))))

;; Convert to hereditary base (simplified)
(defun to-hereditary-base (n base)
  (if (lt n base)
      n
      (let ((q (div n base))
            (r (mod n base)))
        (add (mult (to-hereditary-base q base) base) r))))

;; Convert from hereditary base
(defun from-hereditary-base (n base)
  n)