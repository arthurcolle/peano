(defmodule vectors
  (export all)
  (import (from peano (zero 0) (s 1) (p 1) (zero? 1) (eq 2) (lt 2))
          (from arithmetic (add 2) (sub 2) (mult 2) (div 2) (pow 2))
          (from pairs (pair 2) (first 1) (second 1))
          (from lists (nil 0) (cons 2) (head 1) (tail 1) (length 1))
          (from types (make-typed 2) (type-of 1) (value-of 1))))

;;; Vectors/Arrays with O(log n) access using balanced trees
;;; Implemented as complete binary trees for efficiency

;; Helper function
(defun nil? (x)
  (eq x (nil)))

;; Type tag for vectors
(defun type-vector ()
  (s (s (s (s (s (s (s (s (s (s (s (s (s (zero)))))))))))))))

;; Vector representation: (size, tree)
;; Tree is either:
;; - nil (empty)
;; - (leaf value)
;; - (branch left right size-left)

;; Tree node constructors
(defun make-leaf (value)
  (cons 'leaf (cons value (nil))))

(defun make-branch (left right size-left)
  (cons 'branch (cons left (cons right (cons size-left (nil))))))

(defun is-leaf? (node)
  (eq (head node) 'leaf))

(defun is-branch? (node)
  (eq (head node) 'branch))

(defun leaf-value (node)
  (head (tail node)))

(defun branch-left (node)
  (head (tail node)))

(defun branch-right (node)
  (head (tail (tail node))))

(defun branch-size-left (node)
  (head (tail (tail (tail node)))))

;; Create empty vector
(defun vector-empty ()
  (make-typed (type-vector) (pair (zero) (nil))))

;; Get vector size
(defun vector-size (vec)
  (first (value-of vec)))

;; Get vector tree
(defun vector-tree (vec)
  (second (value-of vec)))

;; Create vector from list
(defun vector-from-list (lst)
  (make-typed (type-vector) 
              (pair (length lst) 
                    (build-tree lst (length lst)))))

;; Build balanced tree from list
(defun build-tree (lst size)
  (cond
    ((zero? size) (nil))
    ((eq size (s (zero))) (make-leaf (head lst)))
    ('true
     (let* ((size-left (div size (s (s (zero)))))
            (size-right (sub size size-left))
            (left-tree (build-tree lst size-left))
            (right-list (drop lst size-left))
            (right-tree (build-tree right-list size-right)))
       (make-branch left-tree right-tree size-left)))))

;; Helper: drop n elements from list
(defun drop (lst n)
  (if (zero? n)
      lst
      (drop (tail lst) (p n))))

;; Get element at index (O(log n))
(defun vector-ref (vec index)
  (if (lt index (vector-size vec))
      (tree-ref (vector-tree vec) index)
      (error "index out of bounds")))

;; Get element from tree
(defun tree-ref (tree idx)
  (if (is-leaf? tree)
      (leaf-value tree)
      (let ((size-left (branch-size-left tree)))
        (if (lt idx size-left)
            (tree-ref (branch-left tree) idx)
            (tree-ref (branch-right tree) (sub idx size-left))))))

;; Set element at index (returns new vector)
(defun vector-set (vec index value)
  (if (lt index (vector-size vec))
      (make-typed (type-vector)
                  (pair (vector-size vec)
                        (tree-set (vector-tree vec) index value)))
      (error "index out of bounds")))

;; Set element in tree
(defun tree-set (tree idx val)
  (if (is-leaf? tree)
      (make-leaf val)
      (let ((size-left (branch-size-left tree)))
        (if (lt idx size-left)
            (make-branch (tree-set (branch-left tree) idx val)
                         (branch-right tree)
                         size-left)
            (make-branch (branch-left tree)
                         (tree-set (branch-right tree) (sub idx size-left) val)
                         size-left)))))

;; Append element to vector
(defun vector-append (vec value)
  (let ((new-size (s (vector-size vec))))
    (make-typed (type-vector)
                (pair new-size
                      (insert-at-end (vector-tree vec) 
                                    (vector-size vec) 
                                    value)))))

;; Insert at end of tree
(defun insert-at-end (tree size val)
  (cond
    ((zero? size) (make-leaf val))
    ((is-leaf? tree) (make-branch tree (make-leaf val) (s (zero))))
    ('true
     (let ((size-left (branch-size-left tree))
           (size-right (sub size size-left)))
       (if (eq size-left size-right)
           ;; Balanced, grow left
           (make-branch (insert-at-end (branch-left tree) size-left val)
                        (branch-right tree)
                        (s size-left))
           ;; Unbalanced, grow right
           (make-branch (branch-left tree)
                        (insert-at-end (branch-right tree) size-right val)
                        size-left)))))

;; Convert vector to list
(defun vector->list (vec)
  (tree->list (vector-tree vec)))

(defun tree->list (tree)
  (cond
    ((nil? tree) (nil))
    ((is-leaf? tree) (cons (leaf-value tree) (nil)))
    ('true (lists:append (tree->list (branch-left tree))
                         (tree->list (branch-right tree))))))

;; Map function over vector
(defun vector-map (f vec)
  (make-typed (type-vector)
              (pair (vector-size vec)
                    (tree-map f (vector-tree vec)))))

(defun tree-map (f tree)
  (cond
    ((nil? tree) (nil))
    ((is-leaf? tree) (make-leaf (funcall f (leaf-value tree))))
    ('true (make-branch (tree-map f (branch-left tree))
                        (tree-map f (branch-right tree))
                        (branch-size-left tree)))))

;; Fold over vector
(defun vector-fold (f acc vec)
  (tree-fold f acc (vector-tree vec)))

(defun tree-fold (f acc tree)
  (cond
    ((nil? tree) acc)
    ((is-leaf? tree) (funcall f acc (leaf-value tree)))
    ('true
     (let ((acc1 (tree-fold f acc (branch-left tree))))
       (tree-fold f acc1 (branch-right tree))))))

;; Create vector of size n with initial value
(defun make-vector (size init-val)
  (vector-from-list (replicate size init-val)))

;; Helper: create list with n copies of value
(defun replicate (n val)
  (if (zero? n)
      (nil)
      (cons val (replicate (p n) val))))

;; Check if value is vector
(defun is-vector? (val)
  (eq (type-of val) (type-vector)))

;; Vector equality
(defun vector-eq? (vec1 vec2)
  (and (eq (vector-size vec1) (vector-size vec2))
       (tree-eq? (vector-tree vec1) (vector-tree vec2))))

(defun tree-eq? (t1 t2)
  (cond
    ((and (nil? t1) (nil? t2)) 'true)
    ((and (is-leaf? t1) (is-leaf? t2))
     (eq (leaf-value t1) (leaf-value t2)))
    ((and (is-branch? t1) (is-branch? t2))
     (and (tree-eq? (branch-left t1) (branch-left t2))
          (tree-eq? (branch-right t1) (branch-right t2))))
    ('true 'false)))

;; Slice vector (get subvector from start to end)
(defun vector-slice (vec start end)
  (vector-from-list (slice-list (vector->list vec) start end)))

(defun slice-list (lst start end)
  (cond
    ((zero? end) (nil))
    ((zero? start)
     (cons (head lst) (slice-list (tail lst) (zero) (p end))))
    ('true (slice-list (tail lst) (p start) (p end)))))