(defmodule maps
  (export all)
  (import (from peano (zero 0) (s 1) (zero? 1) (eq 2) (lt 2))
          (from pairs (pair 2) (first 1) (second 1))
          (from lists (nil 0) (cons 2) (head 1) (tail 1))
          (from types (make-typed 2) (type-of 1) (value-of 1))
          (from adt (make-adt 2) (adt-constructor 1) (adt-data 1) 
                    (none 0) (some 1) (is-none? 1) (some-value 1))))

;;; Maps/Dictionaries implemented as balanced binary search trees
;;; Using AVL tree for O(log n) operations

;; Helper functions
(defun nil? (x)
  (eq x (nil)))

(defun p (n)
  (if (zero? n) (zero) (arithmetic:sub n (s (zero)))))

;; Type tag for maps
(defun type-map ()
  (s (s (s (s (s (s (s (s (s (s (s (s (s (s (zero))))))))))))))))

;; Map representation: tree
;; Tree nodes: (empty) or (node key value height left right)

;; Node constructors
(defun empty-node () (nil))

(defun make-node (key value height left right)
  (cons 'node (cons key (cons value (cons height (cons left (cons right (nil))))))))

(defun is-empty? (node)
  (nil? node))

(defun node-key (node)
  (head (tail node)))

(defun node-value (node)
  (head (tail (tail node))))

(defun node-height (node)
  (head (tail (tail (tail node)))))

(defun node-left (node)
  (head (tail (tail (tail (tail node))))))

(defun node-right (node)
  (head (tail (tail (tail (tail (tail node)))))))

;; Get height of node
(defun height (node)
  (if (is-empty? node)
      (zero)
      (node-height node)))

;; Calculate balance factor
(defun balance-factor (node)
  (if (is-empty? node)
      (zero)
      (sub-safe (height (node-left node)) 
                (height (node-right node)))))

;; Safe subtraction (returns 0 if would be negative)
(defun sub-safe (a b)
  (if (lt a b) (zero) (arithmetic:sub a b)))

;; Create empty map
(defun map-empty ()
  (make-typed (type-map) (empty-node)))

;; Insert key-value pair
(defun map-insert (m key value)
  (make-typed (type-map) 
              (insert-node (value-of m) key value)))

(defun insert-node (node key value)
  (if (is-empty? node)
      (make-node key value (s (zero)) (empty-node) (empty-node))
      (cond
        ((lt key (node-key node))
         (balance (make-node (node-key node)
                            (node-value node)
                            (node-height node)
                            (insert-node (node-left node) key value)
                            (node-right node))))
        ((lt (node-key node) key)
         (balance (make-node (node-key node)
                            (node-value node)
                            (node-height node)
                            (node-left node)
                            (insert-node (node-right node) key value))))
        ('true  ; key equals node-key
         (make-node key value (node-height node) 
                   (node-left node) (node-right node))))))

;; Update height of node
(defun update-height (node)
  (if (is-empty? node)
      node
      (make-node (node-key node)
                 (node-value node)
                 (s (max (height (node-left node))
                        (height (node-right node))))
                 (node-left node)
                 (node-right node))))

;; Balance node (AVL rotations)
(defun balance (node)
  (if (is-empty? node)
      node
      (let ((bf (balance-factor node)))
        (cond
          ;; Left heavy
          ((gt bf (s (zero)))
           (if (lt (balance-factor (node-left node)) (zero))
               (rotate-right (make-node (node-key node)
                                       (node-value node)
                                       (node-height node)
                                       (rotate-left (node-left node))
                                       (node-right node)))
               (rotate-right node)))
          ;; Right heavy
          ((lt bf (p (zero)))
           (if (gt (balance-factor (node-right node)) (zero))
               (rotate-left (make-node (node-key node)
                                      (node-value node)
                                      (node-height node)
                                      (node-left node)
                                      (rotate-right (node-right node))))
               (rotate-left node)))
          ;; Balanced
          ('true (update-height node))))))

;; Helper: max
(defun max (a b)
  (if (lt a b) b a))

;; Helper: gt (greater than)
(defun gt (a b)
  (lt b a))

;; Left rotation
(defun rotate-left (node)
  (let ((right (node-right node)))
    (update-height
     (make-node (node-key right)
                (node-value right)
                (zero)  ; will be updated
                (update-height
                 (make-node (node-key node)
                           (node-value node)
                           (zero)
                           (node-left node)
                           (node-left right)))
                (node-right right)))))

;; Right rotation
(defun rotate-right (node)
  (let ((left (node-left node)))
    (update-height
     (make-node (node-key left)
                (node-value left)
                (zero)  ; will be updated
                (node-left left)
                (update-height
                 (make-node (node-key node)
                           (node-value node)
                           (zero)
                           (node-right left)
                           (node-right node)))))))

;; Lookup key in map
(defun map-get (m key)
  (lookup-node (value-of m) key))

(defun lookup-node (node key)
  (if (is-empty? node)
      (none)
      (cond
        ((lt key (node-key node))
         (lookup-node (node-left node) key))
        ((lt (node-key node) key)
         (lookup-node (node-right node) key))
        ('true  ; found
         (some (node-value node))))))

;; Check if key exists
(defun map-has-key? (m key)
  (not (is-none? (map-get m key))))

;; Remove key from map
(defun map-remove (m key)
  (make-typed (type-map)
              (remove-node (value-of m) key)))

(defun remove-node (node key)
  (if (is-empty? node)
      node
      (cond
        ((lt key (node-key node))
         (balance (make-node (node-key node)
                            (node-value node)
                            (node-height node)
                            (remove-node (node-left node) key)
                            (node-right node))))
        ((lt (node-key node) key)
         (balance (make-node (node-key node)
                            (node-value node)
                            (node-height node)
                            (node-left node)
                            (remove-node (node-right node) key))))
        ('true  ; found - remove it
         (cond
           ((is-empty? (node-left node)) (node-right node))
           ((is-empty? (node-right node)) (node-left node))
           ('true  ; has both children
            (let ((min-right (find-min (node-right node))))
              (balance (make-node (node-key min-right)
                                 (node-value min-right)
                                 (node-height node)
                                 (node-left node)
                                 (remove-min (node-right node))))))))))))

;; Find minimum node
(defun find-min (node)
  (if (is-empty? (node-left node))
      node
      (find-min (node-left node))))

;; Remove minimum node
(defun remove-min (node)
  (if (is-empty? (node-left node))
      (node-right node)
      (balance (make-node (node-key node)
                         (node-value node)
                         (node-height node)
                         (remove-min (node-left node))
                         (node-right node)))))

;; Convert map to list of key-value pairs
(defun map->list (m)
  (tree->list (value-of m)))

(defun tree->list (node)
  (if (is-empty? node)
      (nil)
      (lists:append (tree->list (node-left node))
                    (cons (pair (node-key node) (node-value node))
                          (tree->list (node-right node))))))

;; Create map from list of key-value pairs
(defun map-from-list (lst)
  (lists:foldl (lambda (kv m)
                 (map-insert m (first kv) (second kv)))
               (map-empty)
               lst))

;; Map size
(defun map-size (m)
  (tree-size (value-of m)))

(defun tree-size (node)
  (if (is-empty? node)
      (zero)
      (s (arithmetic:add (tree-size (node-left node))
                        (tree-size (node-right node))))))

;; Map over values
(defun map-map (f m)
  (make-typed (type-map)
              (tree-map-values f (value-of m))))

(defun tree-map-values (f node)
  (if (is-empty? node)
      node
      (make-node (node-key node)
                 (funcall f (node-value node))
                 (node-height node)
                 (tree-map-values f (node-left node))
                 (tree-map-values f (node-right node)))))

;; Get all keys
(defun map-keys (m)
  (lists:map #'first/1 (map->list m)))

;; Get all values  
(defun map-values (m)
  (lists:map #'second/1 (map->list m)))

;; Check if value is a map
(defun is-map? (val)
  (eq (type-of val) (type-map)))

;; Merge two maps (values from m2 override m1)
(defun map-merge (m1 m2)
  (lists:foldl (lambda (kv m)
                 (map-insert m (first kv) (second kv)))
               m1
               (map->list m2)))