(defmodule sets
  (export all)
  (import (from peano (zero 0) (s 1) (eq 2) (lt 2))
          (from lists (nil 0) (cons 2) (map 2) (filter 2) (foldl 3))
          (from types (make-typed 2) (type-of 1) (value-of 1))
          (from maps (map-empty 0) (map-insert 3) (map-has-key? 2)
                     (map-remove 2) (map-keys 1) (map->list 1))))

;;; Sets implemented using maps (keys only, values are unit)

;; Helper function
(defun nil? (x)
  (eq x (nil)))

;; Type tag for sets
(defun type-set ()
  (s (s (s (s (s (s (s (s (s (s (s (s (s (s (s (zero)))))))))))))))))

;; Unit type for map values
(defun unit () (zero))

;; Create empty set
(defun set-empty ()
  (make-typed (type-set) (map-empty)))

;; Create set from list
(defun set-from-list (lst)
  (make-typed (type-set)
              (foldl (lambda (elem m)
                      (map-insert m elem (unit)))
                     (map-empty)
                     lst)))

;; Add element to set
(defun set-add (set elem)
  (make-typed (type-set)
              (map-insert (value-of set) elem (unit))))

;; Remove element from set
(defun set-remove (set elem)
  (make-typed (type-set)
              (map-remove (value-of set) elem)))

;; Check if element is in set
(defun set-member? (set elem)
  (map-has-key? (value-of set) elem))

;; Get set size
(defun set-size (set)
  (maps:map-size (value-of set)))

;; Convert set to list
(defun set->list (set)
  (map-keys (value-of set)))

;; Set union
(defun set-union (set1 set2)
  (foldl (lambda (elem s) (set-add s elem))
         set1
         (set->list set2)))

;; Set intersection
(defun set-intersection (set1 set2)
  (set-from-list
   (filter (lambda (elem) (set-member? set2 elem))
           (set->list set1))))

;; Set difference (elements in set1 but not in set2)
(defun set-difference (set1 set2)
  (set-from-list
   (filter (lambda (elem) (not (set-member? set2 elem)))
           (set->list set1))))

;; Symmetric difference
(defun set-symmetric-difference (set1 set2)
  (set-union (set-difference set1 set2)
             (set-difference set2 set1)))

;; Check if set1 is subset of set2
(defun set-subset? (set1 set2)
  (all? (lambda (elem) (set-member? set2 elem))
        (set->list set1)))

;; Helper: check if predicate holds for all elements
(defun all? (pred lst)
  (if (nil? lst)
      'true
      (and (funcall pred (lists:head lst))
           (all? pred (lists:tail lst)))))

;; Set equality
(defun set-eq? (set1 set2)
  (and (set-subset? set1 set2)
       (set-subset? set2 set1)))

;; Check if sets are disjoint
(defun set-disjoint? (set1 set2)
  (set-empty? (set-intersection set1 set2)))

;; Check if set is empty
(defun set-empty? (set)
  (eq (set-size set) (zero)))

;; Map function over set
(defun set-map (f set)
  (set-from-list (map f (set->list set))))

;; Filter set by predicate
(defun set-filter (pred set)
  (set-from-list (filter pred (set->list set))))

;; Fold over set
(defun set-fold (f acc set)
  (foldl f acc (set->list set)))

;; Power set (set of all subsets)
(defun set-powerset (set)
  (let ((lst (set->list set)))
    (set-from-list (map #'set-from-list/1 (powerset-list lst)))))

;; Helper: generate powerset as list of lists
(defun powerset-list (lst)
  (if (nil? lst)
      (cons (nil) (nil))
      (let ((elem (lists:head lst))
            (rest-powerset (powerset-list (lists:tail lst))))
        (lists:append rest-powerset
                      (map (lambda (subset) (cons elem subset))
                           rest-powerset)))))

;; Cartesian product of two sets
(defun set-product (set1 set2)
  (set-from-list
   (flatten
    (map (lambda (x)
           (map (lambda (y) (pairs:pair x y))
                (set->list set2)))
         (set->list set1)))))

;; Helper: flatten list of lists
(defun flatten (lst-of-lsts)
  (if (nil? lst-of-lsts)
      (nil)
      (lists:append (lists:head lst-of-lsts) 
                    (flatten (lists:tail lst-of-lsts)))))

;; Check if value is a set
(defun is-set? (val)
  (eq (type-of val) (type-set)))

;; Set comprehension helper
(defun set-comprehension (generator filter-pred transform)
  (set-from-list
   (map transform
        (filter filter-pred generator))))

;; Singleton set
(defun set-singleton (elem)
  (set-add (set-empty) elem))

;; Union of list of sets
(defun set-unions (sets)
  (foldl #'set-union/2 (set-empty) sets))

;; Intersection of list of sets
(defun set-intersections (sets)
  (if (nil? sets)
      (set-empty)
      (foldl #'set-intersection/2 
             (lists:head sets) 
             (lists:tail sets))))