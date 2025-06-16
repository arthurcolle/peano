(defmodule records
  (export all)
  (import (from peano (zero 0) (s 1) (eq 2))
          (from pairs (pair 2) (first 1) (second 1))
          (from lists (nil 0) (cons 2) (head 1) (tail 1) (find 2) (map 2))
          (from types (make-typed 2) (type-of 1) (value-of 1))
          (from arithmetic (add 2))))

;;; Records with named fields
;;; Encoded as (record-type-id, list-of-field-value-pairs)

;; Helper function
(defun nil? (x)
  (eq x (nil)))

;; Type tag for records
(defun type-record ()
  (s (s (s (s (s (s (s (s (s (s (s (zero)))))))))))))

;; Global record type registry (simplified - in real system would be mutable)
;; For now, we use predefined IDs for record types

;; Record type definitions
(defun make-record-type (name fields)
  (pair name fields))

;; Create a record
(defun make-record (type-id field-values)
  (make-typed (type-record)
              (pair type-id field-values)))

;; Get record type ID
(defun record-type-id (rec)
  (first (value-of rec)))

;; Get record fields
(defun record-fields (rec)
  (second (value-of rec)))

;; Get field value by name
(defun record-get (rec field-name)
  (let ((field-pair (find-field (record-fields rec) field-name)))
    (if (eq field-pair 'not-found)
        (error "field not found")
        (second field-pair))))

;; Helper: find field in field list
(defun find-field (fields name)
  (lists:find (lambda (field-pair) 
                (eq (first field-pair) name))
              fields))

;; Set field value (returns new record)
(defun record-set (rec field-name value)
  (make-record (record-type-id rec)
               (update-fields (record-fields rec) field-name value)))

;; Helper: update field in field list
(defun update-fields (fields name val)
  (if (nil? fields)
      (cons (pair name val) (nil))
      (let ((field-pair (head fields)))
        (if (eq (first field-pair) name)
            (cons (pair name val) (tail fields))
            (cons field-pair (update-fields (tail fields) name val))))))

;; Update multiple fields
(defun record-update (rec updates)
  (lists:foldl (lambda (update acc-rec)
                 (record-set acc-rec (first update) (second update)))
               rec
               updates))

;; Record equality
(defun record-eq? (rec1 rec2)
  (and (eq (record-type-id rec1) (record-type-id rec2))
       (fields-eq? (record-fields rec1) (record-fields rec2))))

;; Helper: field list equality
(defun fields-eq? (fields1 fields2)
  (and (eq (lists:length fields1) (lists:length fields2))
       (all-fields-match? fields1 fields2)))

(defun all-fields-match? (fields1 fields2)
  (if (nil? fields1)
      'true
      (let ((field1 (head fields1)))
        (case (find-field fields2 (first field1))
          ('not-found 'false)
          (field2 (and (eq (second field1) (second field2))
                       (all-fields-match? (tail fields1) fields2)))))))

;; Create specific record types with constructors

;; Person record type
(defun person-type-id () (zero))

(defun make-person (name age)
  (make-record (person-type-id)
               (lists:list (pair (name-field) name)
                          (pair (age-field) age))))

(defun person-name (person)
  (record-get person (name-field)))

(defun person-age (person)
  (record-get person (age-field)))

;; Field name encodings
(defun name-field () (zero))
(defun age-field () (s (zero)))
(defun x-field () (s (s (zero))))
(defun y-field () (s (s (s (zero)))))
(defun value-field () (s (s (s (s (zero))))))
(defun left-field () (s (s (s (s (s (zero)))))))
(defun right-field () (s (s (s (s (s (s (zero))))))))

;; Point record type
(defun point-type-id () (s (zero)))

(defun make-point (x y)
  (make-record (point-type-id)
               (lists:list (pair (x-field) x)
                          (pair (y-field) y))))

(defun point-x (point)
  (record-get point (x-field)))

(defun point-y (point)
  (record-get point (y-field)))

;; Generic record builder
(defun record
  ((type-id) (make-record type-id (nil)))
  ((type-id field val . rest)
   (let ((fields (build-fields (cons field (cons val rest)))))
     (make-record type-id fields))))

;; Helper: build field list from alternating field/value list
(defun build-fields
  (() (nil))
  ((field val . rest) (cons (pair field val) (build-fields rest)))
  ((_) (error "odd number of field/value arguments")))

;; Record inspection
(defun record-field-names (rec)
  (map (lambda (field-pair) (first field-pair))
       (record-fields rec)))

(defun record-field-values (rec)
  (map (lambda (field-pair) (second field-pair))
       (record-fields rec)))

;; Check if value is a record
(defun is-record? (val)
  (eq (type-of val) (type-record)))

;; Check if record has field
(defun record-has-field? (rec field-name)
  (case (find-field (record-fields rec) field-name)
    ('not-found 'false)
    (_ 'true)))