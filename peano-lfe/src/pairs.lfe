(defmodule pairs
  (export all)
  (import (from peano (zero 0) (s 1) (p 1) (zero? 1))
          (from arithmetic (add 2) (mult 2) (div 2) (mod 2))))

;;; Binary pair encoding/decoding
;;; Encode two numbers into one by interleaving their bits

;; Convert to/from base 2 representation (for clarity)
(defun to-binary (n)
  (if (zero? n)
      '()
      (cons (mod n (s (s (zero))))
            (to-binary (div n (s (s (zero))))))))

(defun from-binary
  (('()) (zero))
  ((bits) (from-binary-helper bits (zero) (s (zero)))))

(defun from-binary-helper
  (('() acc _) acc)
  (((cons bit rest) acc pow)
   (from-binary-helper rest 
                       (add acc (mult bit pow))
                       (mult pow (s (s (zero)))))))

;; Extract odd-positioned bits (head of pair)
(defun get-binary-head (n)
  (if (zero? n)
      (zero)
      (add (mod n (s (s (zero))))
           (mult (get-binary-head (div n (mult (s (s (zero))) 
                                              (s (s (zero))))))
                 (s (s (zero)))))))

;; Extract even-positioned bits (tail of pair)
(defun get-binary-tail (n)
  (get-binary-head (div n (s (s (zero))))))

;; Create a number with bits only in odd positions
(defun make-binary-head (n)
  (if (zero? n)
      (zero)
      (add (mod n (s (s (zero))))
           (mult (make-binary-head (div n (s (s (zero)))))
                 (mult (s (s (zero))) (s (s (zero)))))))

;; Create a number with bits only in even positions  
(defun make-binary-tail (n)
  (mult (make-binary-head n) (s (s (zero)))))

;; Combine two numbers into a pair
(defun make-binary-pair (head tail)
  (add (make-binary-head head) (make-binary-tail tail)))

;; Update head of a pair
(defun set-binary-head (pair new-head)
  (add (make-binary-head new-head) 
       (make-binary-tail (get-binary-tail pair))))

;; Update tail of a pair
(defun set-binary-tail (pair new-tail)
  (add (make-binary-head (get-binary-head pair))
       (make-binary-tail new-tail)))

;; Convenience functions
(defun pair (a b) (make-binary-pair a b))
(defun first (p) (get-binary-head p))
(defun second (p) (get-binary-tail p))

;; Triple encoding (pair of pair and single)
(defun triple (a b c)
  (pair (pair a b) c))

(defun triple-first (t)
  (first (first t)))

(defun triple-second (t)
  (second (first t)))

(defun triple-third (t)
  (second t))