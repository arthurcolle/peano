;;;; Additional fundamental types for Peano arithmetic
;;;; Extends the type system with commonly needed data types

(defmodule fundamentals
  (export
    ;; Integers (signed numbers)
    (make-integer 2) (integer? 1)
    (integer-sign 1) (integer-magnitude 1)
    (integer-negate 1) (integer-abs 1)
    (integer-add 2) (integer-sub 2)
    (integer-mult 2) (integer-div 2)
    (integer-compare 2) (integer-eq? 2)
    (integer->nat 1) (nat->integer 1)
    
    ;; Rational numbers
    (make-rational 2) (rational? 1)
    (rational-numerator 1) (rational-denominator 1)
    (rational-simplify 1) (rational-normalize 1)
    (rational-add 2) (rational-sub 2)
    (rational-mult 2) (rational-div 2)
    (rational-eq? 2) (rational-lt? 2)
    (rational->integer 1) (integer->rational 1)
    
    ;; Characters
    (make-char 1) (char? 1)
    (char->code 1) (code->char 1)
    (char-upcase 1) (char-downcase 1)
    (char-alpha? 1) (char-digit? 1)
    (char-whitespace? 1) (char-eq? 2)
    (char-lt? 2)
    
    ;; Bytes
    (make-byte 1) (byte? 1)
    (byte-and 2) (byte-or 2)
    (byte-xor 2) (byte-not 1)
    (byte-shift-left 2) (byte-shift-right 2)
    (bytes->nat 1) (nat->bytes 2)
    
    ;; Fixed-point numbers
    (make-fixed 3) (fixed? 1)
    (fixed-integer-part 1) (fixed-fractional-part 1)
    (fixed-scale 1) (fixed-add 2)
    (fixed-sub 2) (fixed-mult 2)
    (fixed-div 2) (fixed-round 2)
    
    ;; Intervals
    (make-interval 4) (interval? 1)
    (interval-lower 1) (interval-upper 1)
    (interval-closed-lower? 1) (interval-closed-upper? 1)
    (interval-contains? 2) (interval-empty? 1)
    (interval-intersect 2) (interval-union 2)
    (interval-width 1)
    
    ;; Ranges
    (make-range 3) (range? 1)
    (range-start 1) (range-end 1)
    (range-step 1) (range->list 1)
    (range-contains? 2) (range-length 1)
    (range-nth 2) (range-map 2)
    
    ;; Dates
    (make-date 3) (date? 1)
    (date-year 1) (date-month 1) (date-day 1)
    (date-valid? 1) (date-leap-year? 1)
    (date-add-days 2) (date-diff 2)
    (date-day-of-week 1) (date-compare 2)
    
    ;; Times
    (make-time 4) (time? 1)
    (time-hours 1) (time-minutes 1)
    (time-seconds 1) (time-nanos 1)
    (time-valid? 1) (time-add 2)
    (time-diff 2) (time-to-seconds 1)
    (seconds-to-time 1)
    
    ;; DateTimes
    (make-datetime 3) (datetime? 1)
    (datetime-date 1) (datetime-time 1)
    (datetime-timezone 1) (datetime-to-utc 1)
    (datetime-add-duration 2) (datetime-compare 2)
    
    ;; Durations
    (make-duration 5) (duration? 1)
    (duration-days 1) (duration-hours 1)
    (duration-minutes 1) (duration-seconds 1)
    (duration-nanos 1) (duration-add 2)
    (duration-sub 2) (duration-mult 2)
    (duration-to-seconds 1)
    
    ;; Complex numbers
    (make-complex 2) (complex? 1)
    (complex-real 1) (complex-imag 1)
    (complex-add 2) (complex-sub 2)
    (complex-mult 2) (complex-div 2)
    (complex-conjugate 1) (complex-magnitude 1)
    (complex-argument 1)
    
    ;; Matrices
    (make-matrix 3) (matrix? 1)
    (matrix-rows 1) (matrix-cols 1)
    (matrix-ref 3) (matrix-set 4)
    (matrix-add 2) (matrix-mult 2)
    (matrix-transpose 1) (matrix-identity 1)
    (matrix-from-lists 1)
    
    ;; Lazy sequences
    (make-lazy-seq 3) (lazy-seq? 1)
    (lazy-head 1) (lazy-tail 1)
    (lazy-take 2) (lazy-drop 2)
    (lazy-map 2) (lazy-filter 2)
    (lazy-concat 2) (lazy-iterate 2)
    (lazy-repeat 1) (lazy-range 3)
    
    ;; Bit vectors
    (make-bitvector 2) (bitvector? 1)
    (bitvector-length 1) (bitvector-ref 2)
    (bitvector-set 3) (bitvector-clear 2)
    (bitvector-and 2) (bitvector-or 2)
    (bitvector-xor 2) (bitvector-not 1)
    (bitvector-count 1)))

;; Import required modules
(include-lib "peano/include/peano.lfe")

;;; Integers (Signed Numbers) ;;;

(defun make-integer (sign magnitude)
  "Create an integer with sign (0 for positive, s(0) for negative) and magnitude"
  (types:make-typed 'integer (pairs:pair sign magnitude)))

(defun integer? (x)
  "Check if value is an integer"
  (peano:eq (types:type-of x) 'integer))

(defun integer-sign (int)
  "Get the sign of an integer"
  (pairs:first (types:value-of int)))

(defun integer-magnitude (int)
  "Get the magnitude of an integer"
  (pairs:second (types:value-of int)))

(defun integer-negate (int)
  "Negate an integer"
  (let ((sign (integer-sign int))
        (mag (integer-magnitude int)))
    (if (peano:zero? mag)
        int  ; -0 = 0
        (make-integer (if (peano:zero? sign) (peano:s (peano:zero)) (peano:zero)) mag))))

(defun integer-abs (int)
  "Absolute value of an integer"
  (make-integer (peano:zero) (integer-magnitude int)))

(defun integer-add (a b)
  "Add two integers"
  (let ((sign-a (integer-sign a))
        (mag-a (integer-magnitude a))
        (sign-b (integer-sign b))
        (mag-b (integer-magnitude b)))
    (if (peano:eq sign-a sign-b)
        ;; Same sign: add magnitudes
        (make-integer sign-a (arithmetic:add mag-a mag-b))
        ;; Different signs: subtract magnitudes
        (if (peano:gte mag-a mag-b)
            (make-integer sign-a (arithmetic:sub mag-a mag-b))
            (make-integer sign-b (arithmetic:sub mag-b mag-a))))))

(defun integer-sub (a b)
  "Subtract two integers"
  (integer-add a (integer-negate b)))

(defun integer-mult (a b)
  "Multiply two integers"
  (let ((sign-a (integer-sign a))
        (mag-a (integer-magnitude a))
        (sign-b (integer-sign b))
        (mag-b (integer-magnitude b))
        (result-mag (arithmetic:mult mag-a mag-b)))
    (if (peano:zero? result-mag)
        (make-integer (peano:zero) (peano:zero))
        (make-integer (if (peano:eq sign-a sign-b) (peano:zero) (peano:s (peano:zero))) result-mag))))

(defun integer-div (a b)
  "Integer division"
  (let ((sign-a (integer-sign a))
        (mag-a (integer-magnitude a))
        (sign-b (integer-sign b))
        (mag-b (integer-magnitude b)))
    (if (peano:zero? mag-b)
        (error "Division by zero")
        (let ((result-mag (arithmetic:div mag-a mag-b)))
          (if (peano:zero? result-mag)
              (make-integer (peano:zero) (peano:zero))
              (make-integer (if (peano:eq sign-a sign-b) (peano:zero) (peano:s (peano:zero))) result-mag))))))

(defun integer-compare (a b)
  "Compare two integers: returns -1, 0, or 1"
  (let ((sign-a (integer-sign a))
        (mag-a (integer-magnitude a))
        (sign-b (integer-sign b))
        (mag-b (integer-magnitude b)))
    (cond
      ((and (peano:zero? sign-a) (peano:not (peano:zero? sign-b))) (peano:s (peano:zero)))  ; positive > negative
      ((and (peano:not (peano:zero? sign-a)) (peano:zero? sign-b)) (peano:zero))  ; negative < positive
      ((peano:zero? sign-a)  ; both positive
       (if (peano:lt mag-a mag-b) (peano:zero)
           (if (peano:gt mag-a mag-b) (peano:s (peano:zero)) (peano:zero))))
      (else  ; both negative
       (if (peano:lt mag-a mag-b) (peano:s (peano:zero))
           (if (peano:gt mag-a mag-b) (peano:zero) (peano:zero)))))))

(defun integer-eq? (a b)
  "Check if two integers are equal"
  (and (peano:eq (integer-sign a) (integer-sign b))
       (peano:eq (integer-magnitude a) (integer-magnitude b))))

(defun integer->nat (int)
  "Convert positive integer to natural number"
  (if (peano:zero? (integer-sign int))
      (integer-magnitude int)
      (error "Cannot convert negative integer to natural")))

(defun nat->integer (n)
  "Convert natural number to positive integer"
  (make-integer (peano:zero) n))

;;; Rational Numbers ;;;

(defun make-rational (num denom)
  "Create a rational number (fraction)"
  (if (peano:zero? (integer-magnitude denom))
      (error "Denominator cannot be zero")
      (let ((normalized (rational-normalize num denom)))
        (types:make-typed 'rational normalized))))

(defun rational? (x)
  "Check if value is a rational"
  (peano:eq (types:type-of x) 'rational))

(defun rational-numerator (rat)
  "Get numerator of rational"
  (pairs:first (types:value-of rat)))

(defun rational-denominator (rat)
  "Get denominator of rational"
  (pairs:second (types:value-of rat)))

(defun rational-normalize (num denom)
  "Normalize a rational by ensuring positive denominator"
  (if (peano:zero? (integer-sign denom))
      (pairs:pair num denom)
      (pairs:pair (integer-negate num) (integer-negate denom))))

(defun rational-simplify (rat)
  "Simplify rational to lowest terms"
  (let* ((num (rational-numerator rat))
         (denom (rational-denominator rat))
         (g (arithmetic:gcd (integer-magnitude num) (integer-magnitude denom))))
    (make-rational (make-integer (integer-sign num) (arithmetic:div (integer-magnitude num) g))
                   (make-integer (integer-sign denom) (arithmetic:div (integer-magnitude denom) g)))))

(defun rational-add (a b)
  "Add two rationals"
  (let ((num-a (rational-numerator a))
        (denom-a (rational-denominator a))
        (num-b (rational-numerator b))
        (denom-b (rational-denominator b)))
    (rational-simplify
      (make-rational (integer-add (integer-mult num-a denom-b)
                                  (integer-mult num-b denom-a))
                     (integer-mult denom-a denom-b)))))

(defun rational-sub (a b)
  "Subtract rationals"
  (rational-add a (make-rational (integer-negate (rational-numerator b))
                                 (rational-denominator b))))

(defun rational-mult (a b)
  "Multiply rationals"
  (rational-simplify
    (make-rational (integer-mult (rational-numerator a) (rational-numerator b))
                   (integer-mult (rational-denominator a) (rational-denominator b)))))

(defun rational-div (a b)
  "Divide rationals"
  (rational-mult a (make-rational (rational-denominator b) (rational-numerator b))))

(defun rational-eq? (a b)
  "Check if two rationals are equal"
  (let ((simp-a (rational-simplify a))
        (simp-b (rational-simplify b)))
    (and (integer-eq? (rational-numerator simp-a) (rational-numerator simp-b))
         (integer-eq? (rational-denominator simp-a) (rational-denominator simp-b)))))

(defun rational-lt? (a b)
  "Check if a < b"
  (let ((diff (rational-sub a b)))
    (peano:not (peano:zero? (integer-sign (rational-numerator diff))))))

(defun rational->integer (rat)
  "Convert rational to integer (truncate)"
  (integer-div (rational-numerator rat) (rational-denominator rat)))

(defun integer->rational (int)
  "Convert integer to rational"
  (make-rational int (nat->integer (peano:s (peano:zero)))))

;;; Characters ;;;

(defun make-char (code)
  "Create a character from Unicode code point"
  (types:make-typed 'char code))

(defun char? (x)
  "Check if value is a character"
  (peano:eq (types:type-of x) 'char))

(defun char->code (ch)
  "Get Unicode code point of character"
  (types:value-of ch))

(defun code->char (code)
  "Create character from code point"
  (make-char code))

(defun char-upcase (ch)
  "Convert character to uppercase"
  (let ((code (char->code ch)))
    ;; ASCII lowercase a-z: 97-122
    ;; ASCII uppercase A-Z: 65-90
    (if (and (peano:gte code 97) (peano:lte code 122))
        (make-char (arithmetic:sub code 32))
        ch)))

(defun char-downcase (ch)
  "Convert character to lowercase"
  (let ((code (char->code ch)))
    ;; ASCII uppercase A-Z: 65-90
    ;; ASCII lowercase a-z: 97-122
    (if (and (peano:gte code 65) (peano:lte code 90))
        (make-char (arithmetic:add code 32))
        ch)))

(defun char-alpha? (ch)
  "Check if character is alphabetic"
  (let ((code (char->code ch)))
    (or (and (peano:gte code 65) (peano:lte code 90))   ; A-Z
        (and (peano:gte code 97) (peano:lte code 122))))) ; a-z

(defun char-digit? (ch)
  "Check if character is a digit"
  (let ((code (char->code ch)))
    (and (peano:gte code 48) (peano:lte code 57))))  ; 0-9

(defun char-whitespace? (ch)
  "Check if character is whitespace"
  (let ((code (char->code ch)))
    (or (peano:eq code 32)   ; space
        (peano:eq code 9)    ; tab
        (peano:eq code 10)   ; newline
        (peano:eq code 13)))) ; carriage return

(defun char-eq? (a b)
  "Check if two characters are equal"
  (peano:eq (char->code a) (char->code b)))

(defun char-lt? (a b)
  "Check if character a < b"
  (peano:lt (char->code a) (char->code b)))

;;; Bytes ;;;

(defun make-byte (n)
  "Create a byte (0-255)"
  (if (peano:gt n 255)
      (error "Byte value must be 0-255")
      (types:make-typed 'byte n)))

(defun byte? (x)
  "Check if value is a byte"
  (peano:eq (types:type-of x) 'byte))

(defun byte-and (a b)
  "Bitwise AND of two bytes"
  (make-byte (bitwise-and (types:value-of a) (types:value-of b))))

(defun byte-or (a b)
  "Bitwise OR of two bytes"
  (make-byte (bitwise-or (types:value-of a) (types:value-of b))))

(defun byte-xor (a b)
  "Bitwise XOR of two bytes"
  (make-byte (bitwise-xor (types:value-of a) (types:value-of b))))

(defun byte-not (a)
  "Bitwise NOT of a byte"
  (make-byte (bitwise-and 255 (bitwise-not (types:value-of a)))))

(defun byte-shift-left (byte n)
  "Shift byte left by n bits"
  (make-byte (bitwise-and 255 (arithmetic:mult (types:value-of byte) (arithmetic:pow 2 n)))))

(defun byte-shift-right (byte n)
  "Shift byte right by n bits"
  (make-byte (arithmetic:div (types:value-of byte) (arithmetic:pow 2 n))))

(defun bytes->nat (bytes)
  "Convert list of bytes to natural number (big-endian)"
  (lists:foldl (lambda (byte acc)
                 (arithmetic:add (arithmetic:mult acc 256) (types:value-of byte)))
               0
               bytes))

(defun nat->bytes (n len)
  "Convert natural number to list of bytes with given length"
  (if (peano:zero? len)
      (lists:nil)
      (lists:append (nat->bytes (arithmetic:div n 256) (peano:p len))
                    (lists:cons (make-byte (arithmetic:mod n 256)) (lists:nil)))))

;; Helper bitwise functions
(defun bitwise-and (a b)
  "Bitwise AND of two natural numbers"
  (if (or (peano:zero? a) (peano:zero? b))
      0
      (arithmetic:add (if (and (peano:eq (arithmetic:mod a 2) 1)
                              (peano:eq (arithmetic:mod b 2) 1))
                          1 0)
                      (arithmetic:mult 2 (bitwise-and (arithmetic:div a 2)
                                                     (arithmetic:div b 2))))))

(defun bitwise-or (a b)
  "Bitwise OR of two natural numbers"
  (if (and (peano:zero? a) (peano:zero? b))
      0
      (arithmetic:add (if (or (peano:eq (arithmetic:mod a 2) 1)
                             (peano:eq (arithmetic:mod b 2) 1))
                          1 0)
                      (arithmetic:mult 2 (bitwise-or (arithmetic:div a 2)
                                                    (arithmetic:div b 2))))))

(defun bitwise-xor (a b)
  "Bitwise XOR of two natural numbers"
  (if (and (peano:zero? a) (peano:zero? b))
      0
      (arithmetic:add (if (peano:eq (arithmetic:mod a 2) (arithmetic:mod b 2))
                          0 1)
                      (arithmetic:mult 2 (bitwise-xor (arithmetic:div a 2)
                                                     (arithmetic:div b 2))))))

(defun bitwise-not (a)
  "Bitwise NOT of a natural number"
  (arithmetic:sub 255 a))  ; For byte context

;;; Fixed-Point Numbers ;;;

(defun make-fixed (integer-part fractional-part scale)
  "Create a fixed-point number with given scale (denominator)"
  (types:make-typed 'fixed (pairs:triple integer-part fractional-part scale)))

(defun fixed? (x)
  "Check if value is a fixed-point number"
  (peano:eq (types:type-of x) 'fixed))

(defun fixed-integer-part (fix)
  "Get integer part of fixed-point number"
  (pairs:triple-first (types:value-of fix)))

(defun fixed-fractional-part (fix)
  "Get fractional part of fixed-point number"
  (pairs:triple-second (types:value-of fix)))

(defun fixed-scale (fix)
  "Get scale (denominator) of fixed-point number"
  (pairs:triple-third (types:value-of fix)))

(defun fixed-add (a b)
  "Add two fixed-point numbers"
  (if (peano:eq (fixed-scale a) (fixed-scale b))
      (let* ((scale (fixed-scale a))
             (total-a (arithmetic:add (arithmetic:mult (fixed-integer-part a) scale)
                                     (fixed-fractional-part a)))
             (total-b (arithmetic:add (arithmetic:mult (fixed-integer-part b) scale)
                                     (fixed-fractional-part b)))
             (total (arithmetic:add total-a total-b)))
        (make-fixed (arithmetic:div total scale)
                    (arithmetic:mod total scale)
                    scale))
      (error "Fixed-point numbers must have same scale")))

(defun fixed-sub (a b)
  "Subtract fixed-point numbers"
  (if (peano:eq (fixed-scale a) (fixed-scale b))
      (let* ((scale (fixed-scale a))
             (total-a (arithmetic:add (arithmetic:mult (fixed-integer-part a) scale)
                                     (fixed-fractional-part a)))
             (total-b (arithmetic:add (arithmetic:mult (fixed-integer-part b) scale)
                                     (fixed-fractional-part b)))
             (total (arithmetic:sub total-a total-b)))
        (make-fixed (arithmetic:div total scale)
                    (arithmetic:mod total scale)
                    scale))
      (error "Fixed-point numbers must have same scale")))

(defun fixed-mult (a b)
  "Multiply fixed-point numbers"
  (if (peano:eq (fixed-scale a) (fixed-scale b))
      (let* ((scale (fixed-scale a))
             (total-a (arithmetic:add (arithmetic:mult (fixed-integer-part a) scale)
                                     (fixed-fractional-part a)))
             (total-b (arithmetic:add (arithmetic:mult (fixed-integer-part b) scale)
                                     (fixed-fractional-part b)))
             (product (arithmetic:mult total-a total-b))
             (scaled (arithmetic:div product scale)))
        (make-fixed (arithmetic:div scaled scale)
                    (arithmetic:mod scaled scale)
                    scale))
      (error "Fixed-point numbers must have same scale")))

(defun fixed-div (a b)
  "Divide fixed-point numbers"
  (if (peano:eq (fixed-scale a) (fixed-scale b))
      (let* ((scale (fixed-scale a))
             (total-a (arithmetic:add (arithmetic:mult (fixed-integer-part a) scale)
                                     (fixed-fractional-part a)))
             (total-b (arithmetic:add (arithmetic:mult (fixed-integer-part b) scale)
                                     (fixed-fractional-part b)))
             (quotient (arithmetic:div (arithmetic:mult total-a scale) total-b)))
        (make-fixed (arithmetic:div quotient scale)
                    (arithmetic:mod quotient scale)
                    scale))
      (error "Fixed-point numbers must have same scale")))

(defun fixed-round (fix places)
  "Round fixed-point number to given decimal places"
  (let* ((scale (fixed-scale fix))
         (round-scale (arithmetic:pow 10 places))
         (total (arithmetic:add (arithmetic:mult (fixed-integer-part fix) scale)
                               (fixed-fractional-part fix)))
         (scaled-total (arithmetic:mult total round-scale))
         (rounded (arithmetic:div (arithmetic:add scaled-total (arithmetic:div scale 2)) scale)))
    (make-fixed (arithmetic:div rounded round-scale)
                (arithmetic:mod (arithmetic:mult (arithmetic:mod rounded round-scale) scale) 
                               (arithmetic:mult round-scale scale))
                scale)))

;;; Intervals ;;;

(defun make-interval (lower upper closed-lower? closed-upper?)
  "Create an interval [lower, upper] or (lower, upper)"
  (types:make-typed 'interval (tuples:tuple-from-list 
                                (lists:cons lower 
                                  (lists:cons upper 
                                    (lists:cons closed-lower? 
                                      (lists:cons closed-upper? (lists:nil))))))))

(defun interval? (x)
  "Check if value is an interval"
  (peano:eq (types:type-of x) 'interval))

(defun interval-lower (interval)
  "Get lower bound of interval"
  (tuples:tuple-ref 0 (types:value-of interval)))

(defun interval-upper (interval)
  "Get upper bound of interval"
  (tuples:tuple-ref 1 (types:value-of interval)))

(defun interval-closed-lower? (interval)
  "Check if lower bound is closed"
  (tuples:tuple-ref 2 (types:value-of interval)))

(defun interval-closed-upper? (interval)
  "Check if upper bound is closed"
  (tuples:tuple-ref 3 (types:value-of interval)))

(defun interval-contains? (interval x)
  "Check if x is in the interval"
  (let ((lower (interval-lower interval))
        (upper (interval-upper interval))
        (closed-lower? (interval-closed-lower? interval))
        (closed-upper? (interval-closed-upper? interval)))
    (and (if closed-lower? (peano:gte x lower) (peano:gt x lower))
         (if closed-upper? (peano:lte x upper) (peano:lt x upper)))))

(defun interval-empty? (interval)
  "Check if interval is empty"
  (let ((lower (interval-lower interval))
        (upper (interval-upper interval)))
    (or (peano:gt lower upper)
        (and (peano:eq lower upper)
             (not (and (interval-closed-lower? interval)
                      (interval-closed-upper? interval)))))))

(defun interval-intersect (a b)
  "Intersection of two intervals"
  (let ((lower-a (interval-lower a))
        (upper-a (interval-upper a))
        (lower-b (interval-lower b))
        (upper-b (interval-upper b))
        (new-lower (peano:max lower-a lower-b))
        (new-upper (peano:min upper-a upper-b)))
    (make-interval new-lower new-upper
                   (if (peano:eq new-lower lower-a)
                       (interval-closed-lower? a)
                       (interval-closed-lower? b))
                   (if (peano:eq new-upper upper-a)
                       (interval-closed-upper? a)
                       (interval-closed-upper? b)))))

(defun interval-union (a b)
  "Union of two intervals (if they overlap)"
  (let ((lower-a (interval-lower a))
        (upper-a (interval-upper a))
        (lower-b (interval-lower b))
        (upper-b (interval-upper b)))
    ;; Check if intervals overlap or touch
    (if (or (interval-contains? a lower-b)
            (interval-contains? a upper-b)
            (interval-contains? b lower-a)
            (interval-contains? b upper-a)
            (and (peano:eq upper-a lower-b)
                 (or (interval-closed-upper? a)
                     (interval-closed-lower? b)))
            (and (peano:eq upper-b lower-a)
                 (or (interval-closed-upper? b)
                     (interval-closed-lower? a))))
        (make-interval (peano:min lower-a lower-b)
                       (peano:max upper-a upper-b)
                       (if (peano:eq (peano:min lower-a lower-b) lower-a)
                           (interval-closed-lower? a)
                           (interval-closed-lower? b))
                       (if (peano:eq (peano:max upper-a upper-b) upper-a)
                           (interval-closed-upper? a)
                           (interval-closed-upper? b)))
        (error "Intervals do not overlap"))))

(defun interval-width (interval)
  "Width of the interval"
  (arithmetic:sub (interval-upper interval) (interval-lower interval)))

;;; Ranges ;;;

(defun make-range (start end step)
  "Create an arithmetic range"
  (types:make-typed 'range (pairs:triple start end step)))

(defun range? (x)
  "Check if value is a range"
  (peano:eq (types:type-of x) 'range))

(defun range-start (range)
  "Get start of range"
  (pairs:triple-first (types:value-of range)))

(defun range-end (range)
  "Get end of range (exclusive)"
  (pairs:triple-second (types:value-of range)))

(defun range-step (range)
  "Get step of range"
  (pairs:triple-third (types:value-of range)))

(defun range->list (range)
  "Convert range to list"
  (let ((start (range-start range))
        (end (range-end range))
        (step (range-step range)))
    (let loop ((current start)
               (acc (lists:nil)))
      (if (peano:gte current end)
          (lists:reverse acc)
          (loop (arithmetic:add current step)
                (lists:cons current acc))))))

(defun range-contains? (range x)
  "Check if x is in range"
  (let ((start (range-start range))
        (end (range-end range))
        (step (range-step range)))
    (and (peano:gte x start)
         (peano:lt x end)
         (peano:zero? (arithmetic:mod (arithmetic:sub x start) step)))))

(defun range-length (range)
  "Get number of elements in range"
  (let ((start (range-start range))
        (end (range-end range))
        (step (range-step range)))
    (if (peano:gte start end)
        0
        (arithmetic:add 1 (arithmetic:div (arithmetic:sub (arithmetic:sub end 1) start) step)))))

(defun range-nth (range n)
  "Get nth element of range"
  (let ((start (range-start range))
        (step (range-step range)))
    (arithmetic:add start (arithmetic:mult n step))))

(defun range-map (f range)
  "Map function over range"
  (lists:map f (range->list range)))

;;; Dates ;;;

(defun make-date (year month day)
  "Create a date"
  (if (date-valid? (types:make-typed 'date (pairs:triple year month day)))
      (types:make-typed 'date (pairs:triple year month day))
      (error "Invalid date")))

(defun date? (x)
  "Check if value is a date"
  (peano:eq (types:type-of x) 'date))

(defun date-year (date)
  "Get year from date"
  (pairs:triple-first (types:value-of date)))

(defun date-month (date)
  "Get month from date"
  (pairs:triple-second (types:value-of date)))

(defun date-day (date)
  "Get day from date"
  (pairs:triple-third (types:value-of date)))

(defun date-valid? (date)
  "Check if date is valid"
  (let ((year (date-year date))
        (month (date-month date))
        (day (date-day date)))
    (and (peano:gte year 1)
         (peano:gte month 1)
         (peano:lte month 12)
         (peano:gte day 1)
         (peano:lte day (days-in-month year month)))))

(defun date-leap-year? (date)
  "Check if date is in a leap year"
  (let ((year (date-year date)))
    (or (peano:zero? (arithmetic:mod year 400))
        (and (peano:zero? (arithmetic:mod year 4))
             (peano:not (peano:zero? (arithmetic:mod year 100)))))))

(defun days-in-month (year month)
  "Get number of days in month"
  (cond
    ((or (peano:eq month 1) (peano:eq month 3) (peano:eq month 5)
         (peano:eq month 7) (peano:eq month 8) (peano:eq month 10)
         (peano:eq month 12)) 31)
    ((or (peano:eq month 4) (peano:eq month 6) (peano:eq month 9)
         (peano:eq month 11)) 30)
    ((peano:eq month 2)
     (if (or (peano:zero? (arithmetic:mod year 400))
             (and (peano:zero? (arithmetic:mod year 4))
                  (peano:not (peano:zero? (arithmetic:mod year 100)))))
         29 28))
    (else (error "Invalid month"))))

(defun date-add-days (date n)
  "Add n days to date"
  (let loop ((year (date-year date))
             (month (date-month date))
             (day (date-day date))
             (remaining n))
    (if (peano:zero? remaining)
        (make-date year month day)
        (let ((days-left (arithmetic:sub (days-in-month year month) day)))
          (if (peano:gte remaining (arithmetic:add days-left 1))
              ;; Move to next month
              (if (peano:eq month 12)
                  (loop (peano:s year) 1 1 (arithmetic:sub remaining (arithmetic:add days-left 1)))
                  (loop year (peano:s month) 1 (arithmetic:sub remaining (arithmetic:add days-left 1))))
              ;; Stay in current month
              (make-date year month (arithmetic:add day remaining)))))))

(defun date-diff (a b)
  "Difference in days between two dates"
  ;; Simplified version - convert to day number since epoch
  (arithmetic:sub (date-to-days a) (date-to-days b)))

(defun date-to-days (date)
  "Convert date to days since year 1"
  (let ((year (date-year date))
        (month (date-month date))
        (day (date-day date)))
    ;; Simplified calculation
    (arithmetic:add (arithmetic:mult year 365)
                    (arithmetic:add (days-before-month year month) day))))

(defun days-before-month (year month)
  "Days before given month in year"
  (let loop ((m 1) (days 0))
    (if (peano:eq m month)
        days
        (loop (peano:s m) (arithmetic:add days (days-in-month year m))))))

(defun date-day-of-week (date)
  "Get day of week (0=Sunday, 6=Saturday)"
  ;; Simplified version using Zeller's congruence
  (let ((year (date-year date))
        (month (date-month date))
        (day (date-day date)))
    (arithmetic:mod (date-to-days date) 7)))

(defun date-compare (a b)
  "Compare two dates"
  (let ((year-a (date-year a))
        (month-a (date-month a))
        (day-a (date-day a))
        (year-b (date-year b))
        (month-b (date-month b))
        (day-b (date-day b)))
    (cond
      ((peano:lt year-a year-b) (peano:zero))
      ((peano:gt year-a year-b) (peano:s (peano:zero)))
      ((peano:lt month-a month-b) (peano:zero))
      ((peano:gt month-a month-b) (peano:s (peano:zero)))
      ((peano:lt day-a day-b) (peano:zero))
      ((peano:gt day-a day-b) (peano:s (peano:zero)))
      (else (peano:zero)))))

;;; Times ;;;

(defun make-time (hours minutes seconds nanos)
  "Create a time"
  (if (time-valid? (types:make-typed 'time (tuples:tuple-from-list 
                                              (lists:cons hours 
                                                (lists:cons minutes 
                                                  (lists:cons seconds 
                                                    (lists:cons nanos (lists:nil))))))))
      (types:make-typed 'time (tuples:tuple-from-list 
                                (lists:cons hours 
                                  (lists:cons minutes 
                                    (lists:cons seconds 
                                      (lists:cons nanos (lists:nil)))))))
      (error "Invalid time")))

(defun time? (x)
  "Check if value is a time"
  (peano:eq (types:type-of x) 'time))

(defun time-hours (time)
  "Get hours from time"
  (tuples:tuple-ref 0 (types:value-of time)))

(defun time-minutes (time)
  "Get minutes from time"
  (tuples:tuple-ref 1 (types:value-of time)))

(defun time-seconds (time)
  "Get seconds from time"
  (tuples:tuple-ref 2 (types:value-of time)))

(defun time-nanos (time)
  "Get nanoseconds from time"
  (tuples:tuple-ref 3 (types:value-of time)))

(defun time-valid? (time)
  "Check if time is valid"
  (and (peano:lt (time-hours time) 24)
       (peano:lt (time-minutes time) 60)
       (peano:lt (time-seconds time) 60)
       (peano:lt (time-nanos time) 1000000000)))

(defun time-add (a b)
  "Add two times (returns time and overflow days)"
  (let* ((total-nanos (arithmetic:add (time-nanos a) (time-nanos b)))
         (nano-carry (arithmetic:div total-nanos 1000000000))
         (nanos (arithmetic:mod total-nanos 1000000000))
         (total-seconds (arithmetic:add (arithmetic:add (time-seconds a) (time-seconds b)) nano-carry))
         (second-carry (arithmetic:div total-seconds 60))
         (seconds (arithmetic:mod total-seconds 60))
         (total-minutes (arithmetic:add (arithmetic:add (time-minutes a) (time-minutes b)) second-carry))
         (minute-carry (arithmetic:div total-minutes 60))
         (minutes (arithmetic:mod total-minutes 60))
         (total-hours (arithmetic:add (arithmetic:add (time-hours a) (time-hours b)) minute-carry))
         (days (arithmetic:div total-hours 24))
         (hours (arithmetic:mod total-hours 24)))
    (pairs:pair (make-time hours minutes seconds nanos) days)))

(defun time-diff (a b)
  "Difference between two times in seconds"
  (arithmetic:sub (time-to-seconds a) (time-to-seconds b)))

(defun time-to-seconds (time)
  "Convert time to total seconds"
  (arithmetic:add (arithmetic:mult (time-hours time) 3600)
                  (arithmetic:add (arithmetic:mult (time-minutes time) 60)
                                  (time-seconds time))))

(defun seconds-to-time (seconds)
  "Convert seconds to time"
  (let* ((hours (arithmetic:div seconds 3600))
         (remaining (arithmetic:mod seconds 3600))
         (minutes (arithmetic:div remaining 60))
         (secs (arithmetic:mod remaining 60)))
    (make-time hours minutes secs 0)))

;;; DateTimes ;;;

(defun make-datetime (date time timezone-offset)
  "Create a datetime with timezone offset in minutes"
  (types:make-typed 'datetime (pairs:triple date time timezone-offset)))

(defun datetime? (x)
  "Check if value is a datetime"
  (peano:eq (types:type-of x) 'datetime))

(defun datetime-date (dt)
  "Get date from datetime"
  (pairs:triple-first (types:value-of dt)))

(defun datetime-time (dt)
  "Get time from datetime"
  (pairs:triple-second (types:value-of dt)))

(defun datetime-timezone (dt)
  "Get timezone offset from datetime"
  (pairs:triple-third (types:value-of dt)))

(defun datetime-to-utc (dt)
  "Convert datetime to UTC"
  (let* ((offset-minutes (datetime-timezone dt))
         (offset-hours (arithmetic:div offset-minutes 60))
         (offset-mins (arithmetic:mod offset-minutes 60))
         (time-result (time-add (datetime-time dt) 
                               (make-time (arithmetic:sub 0 offset-hours)
                                         (arithmetic:sub 0 offset-mins) 0 0)))
         (new-time (pairs:first time-result))
         (day-diff (pairs:second time-result)))
    (make-datetime (date-add-days (datetime-date dt) day-diff) new-time 0)))

(defun datetime-add-duration (dt dur)
  "Add duration to datetime"
  (let* ((date (datetime-date dt))
         (time (datetime-time dt))
         (new-date (date-add-days date (duration-days dur)))
         (time-to-add (make-time (duration-hours dur)
                                (duration-minutes dur)
                                (duration-seconds dur)
                                (duration-nanos dur)))
         (time-result (time-add time time-to-add))
         (new-time (pairs:first time-result))
         (extra-days (pairs:second time-result)))
    (make-datetime (date-add-days new-date extra-days) new-time (datetime-timezone dt))))

(defun datetime-compare (a b)
  "Compare two datetimes"
  (let ((date-cmp (date-compare (datetime-date a) (datetime-date b))))
    (if (peano:zero? date-cmp)
        (let ((time-a-sec (time-to-seconds (datetime-time a)))
              (time-b-sec (time-to-seconds (datetime-time b))))
          (cond
            ((peano:lt time-a-sec time-b-sec) (peano:zero))
            ((peano:gt time-a-sec time-b-sec) (peano:s (peano:zero)))
            (else (peano:zero))))
        date-cmp)))

;;; Durations ;;;

(defun make-duration (days hours minutes seconds nanos)
  "Create a duration"
  (types:make-typed 'duration 
                    (tuples:tuple-from-list 
                      (lists:cons days 
                        (lists:cons hours 
                          (lists:cons minutes 
                            (lists:cons seconds 
                              (lists:cons nanos (lists:nil)))))))))

(defun duration? (x)
  "Check if value is a duration"
  (peano:eq (types:type-of x) 'duration))

(defun duration-days (dur)
  "Get days from duration"
  (tuples:tuple-ref 0 (types:value-of dur)))

(defun duration-hours (dur)
  "Get hours from duration"
  (tuples:tuple-ref 1 (types:value-of dur)))

(defun duration-minutes (dur)
  "Get minutes from duration"
  (tuples:tuple-ref 2 (types:value-of dur)))

(defun duration-seconds (dur)
  "Get seconds from duration"
  (tuples:tuple-ref 3 (types:value-of dur)))

(defun duration-nanos (dur)
  "Get nanoseconds from duration"
  (tuples:tuple-ref 4 (types:value-of dur)))

(defun duration-add (a b)
  "Add two durations"
  (let* ((total-nanos (arithmetic:add (duration-nanos a) (duration-nanos b)))
         (nano-carry (arithmetic:div total-nanos 1000000000))
         (nanos (arithmetic:mod total-nanos 1000000000))
         (total-seconds (arithmetic:add (arithmetic:add (duration-seconds a) (duration-seconds b)) nano-carry))
         (second-carry (arithmetic:div total-seconds 60))
         (seconds (arithmetic:mod total-seconds 60))
         (total-minutes (arithmetic:add (arithmetic:add (duration-minutes a) (duration-minutes b)) second-carry))
         (minute-carry (arithmetic:div total-minutes 60))
         (minutes (arithmetic:mod total-minutes 60))
         (total-hours (arithmetic:add (arithmetic:add (duration-hours a) (duration-hours b)) minute-carry))
         (hour-carry (arithmetic:div total-hours 24))
         (hours (arithmetic:mod total-hours 24))
         (days (arithmetic:add (arithmetic:add (duration-days a) (duration-days b)) hour-carry)))
    (make-duration days hours minutes seconds nanos)))

(defun duration-sub (a b)
  "Subtract two durations"
  ;; Simplified - convert to seconds and back
  (let ((sec-a (duration-to-seconds a))
        (sec-b (duration-to-seconds b))
        (diff (arithmetic:sub sec-a sec-b)))
    (seconds-to-duration diff)))

(defun duration-mult (dur n)
  "Multiply duration by scalar"
  (let ((total-seconds (arithmetic:mult (duration-to-seconds dur) n)))
    (seconds-to-duration total-seconds)))

(defun duration-to-seconds (dur)
  "Convert duration to total seconds"
  (arithmetic:add (arithmetic:mult (duration-days dur) 86400)
                  (arithmetic:add (arithmetic:mult (duration-hours dur) 3600)
                                  (arithmetic:add (arithmetic:mult (duration-minutes dur) 60)
                                                  (duration-seconds dur)))))

(defun seconds-to-duration (seconds)
  "Convert seconds to duration"
  (let* ((days (arithmetic:div seconds 86400))
         (remaining (arithmetic:mod seconds 86400))
         (hours (arithmetic:div remaining 3600))
         (remaining2 (arithmetic:mod remaining 3600))
         (minutes (arithmetic:div remaining2 60))
         (secs (arithmetic:mod remaining2 60)))
    (make-duration days hours minutes secs 0)))

;;; Complex Numbers ;;;

(defun make-complex (real imag)
  "Create a complex number"
  (types:make-typed 'complex (pairs:pair real imag)))

(defun complex? (x)
  "Check if value is a complex number"
  (peano:eq (types:type-of x) 'complex))

(defun complex-real (z)
  "Get real part of complex number"
  (pairs:first (types:value-of z)))

(defun complex-imag (z)
  "Get imaginary part of complex number"
  (pairs:second (types:value-of z)))

(defun complex-add (a b)
  "Add two complex numbers"
  (make-complex (rational-add (complex-real a) (complex-real b))
                (rational-add (complex-imag a) (complex-imag b))))

(defun complex-sub (a b)
  "Subtract two complex numbers"
  (make-complex (rational-sub (complex-real a) (complex-real b))
                (rational-sub (complex-imag a) (complex-imag b))))

(defun complex-mult (a b)
  "Multiply two complex numbers"
  (let ((real-a (complex-real a))
        (imag-a (complex-imag a))
        (real-b (complex-real b))
        (imag-b (complex-imag b)))
    (make-complex (rational-sub (rational-mult real-a real-b)
                               (rational-mult imag-a imag-b))
                  (rational-add (rational-mult real-a imag-b)
                               (rational-mult imag-a real-b)))))

(defun complex-div (a b)
  "Divide two complex numbers"
  (let* ((conj-b (complex-conjugate b))
         (denom (complex-mult b conj-b))
         (denom-real (complex-real denom))
         (num (complex-mult a conj-b)))
    (make-complex (rational-div (complex-real num) denom-real)
                  (rational-div (complex-imag num) denom-real))))

(defun complex-conjugate (z)
  "Complex conjugate"
  (make-complex (complex-real z)
                (rational-sub (integer->rational (nat->integer 0))
                             (complex-imag z))))

(defun complex-magnitude (z)
  "Magnitude of complex number"
  ;; |z| = sqrt(a² + b²)
  ;; Returns squared magnitude as rational
  (let ((real (complex-real z))
        (imag (complex-imag z)))
    (rational-add (rational-mult real real)
                  (rational-mult imag imag))))

(defun complex-argument (z)
  "Argument (angle) of complex number"
  ;; Simplified - returns quadrant (0-3) * 90 degrees
  (let ((real (complex-real z))
        (imag (complex-imag z)))
    (cond
      ((and (rational-lt? (integer->rational (nat->integer 0)) real)
            (rational-lt? (integer->rational (nat->integer 0)) imag)) 0)  ; First quadrant
      ((and (rational-lt? real (integer->rational (nat->integer 0)))
            (rational-lt? (integer->rational (nat->integer 0)) imag)) 1)  ; Second quadrant
      ((and (rational-lt? real (integer->rational (nat->integer 0)))
            (rational-lt? imag (integer->rational (nat->integer 0)))) 2)  ; Third quadrant
      (else 3))))  ; Fourth quadrant

;;; Matrices ;;;

(defun make-matrix (rows cols elements)
  "Create a matrix with given dimensions and elements (as vector)"
  (if (peano:eq (vectors:vector-size elements) (arithmetic:mult rows cols))
      (types:make-typed 'matrix (pairs:triple rows cols elements))
      (error "Element count does not match matrix dimensions")))

(defun matrix? (x)
  "Check if value is a matrix"
  (peano:eq (types:type-of x) 'matrix))

(defun matrix-rows (mat)
  "Get number of rows"
  (pairs:triple-first (types:value-of mat)))

(defun matrix-cols (mat)
  "Get number of columns"
  (pairs:triple-second (types:value-of mat)))

(defun matrix-ref (mat row col)
  "Get element at (row, col)"
  (let ((cols (matrix-cols mat))
        (elements (pairs:triple-third (types:value-of mat))))
    (vectors:vector-ref (arithmetic:add (arithmetic:mult row cols) col) elements)))

(defun matrix-set (mat row col val)
  "Set element at (row, col)"
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat))
        (elements (pairs:triple-third (types:value-of mat)))
        (index (arithmetic:add (arithmetic:mult row cols) col)))
    (make-matrix rows cols (vectors:vector-set index val elements))))

(defun matrix-add (a b)
  "Add two matrices"
  (if (and (peano:eq (matrix-rows a) (matrix-rows b))
           (peano:eq (matrix-cols a) (matrix-cols b)))
      (let ((rows (matrix-rows a))
            (cols (matrix-cols a))
            (elements-a (pairs:triple-third (types:value-of a)))
            (elements-b (pairs:triple-third (types:value-of b))))
        (make-matrix rows cols (vectors:vector-map arithmetic:add elements-a elements-b)))
      (error "Matrix dimensions must match")))

(defun matrix-mult (a b)
  "Multiply two matrices"
  (if (peano:eq (matrix-cols a) (matrix-rows b))
      (let ((rows-a (matrix-rows a))
            (cols-a (matrix-cols a))
            (cols-b (matrix-cols b)))
        (let ((result-elements 
                (vectors:vector-from-list
                  (let loop-i ((i 0) (acc (lists:nil)))
                    (if (peano:eq i rows-a)
                        (lists:reverse acc)
                        (loop-i (peano:s i)
                               (let loop-j ((j 0) (row-acc acc))
                                 (if (peano:eq j cols-b)
                                     row-acc
                                     (let ((sum (let loop-k ((k 0) (s 0))
                                                  (if (peano:eq k cols-a)
                                                      s
                                                      (loop-k (peano:s k)
                                                             (arithmetic:add s 
                                                                           (arithmetic:mult 
                                                                             (matrix-ref a i k)
                                                                             (matrix-ref b k j))))))))
                                       (loop-j (peano:s j) (lists:cons sum row-acc)))))))))))
          (make-matrix rows-a cols-b result-elements)))
      (error "Invalid matrix dimensions for multiplication")))

(defun matrix-transpose (mat)
  "Transpose a matrix"
  (let ((rows (matrix-rows mat))
        (cols (matrix-cols mat)))
    (make-matrix cols rows
                 (vectors:vector-from-list
                   (let loop-i ((i 0) (acc (lists:nil)))
                     (if (peano:eq i cols)
                         (lists:reverse acc)
                         (loop-i (peano:s i)
                                (let loop-j ((j 0) (col-acc acc))
                                  (if (peano:eq j rows)
                                      col-acc
                                      (loop-j (peano:s j)
                                             (lists:cons (matrix-ref mat j i) col-acc)))))))))))

(defun matrix-identity (n)
  "Create n×n identity matrix"
  (make-matrix n n
               (vectors:vector-from-list
                 (let loop-i ((i 0) (acc (lists:nil)))
                   (if (peano:eq i n)
                       (lists:reverse acc)
                       (loop-i (peano:s i)
                              (let loop-j ((j 0) (row-acc acc))
                                (if (peano:eq j n)
                                    row-acc
                                    (loop-j (peano:s j)
                                           (lists:cons (if (peano:eq i j) 1 0) row-acc))))))))))

(defun matrix-from-lists (lists-of-lists)
  "Create matrix from list of lists"
  (let* ((rows (lists:length lists-of-lists))
         (cols (if (peano:zero? rows) 0 (lists:length (lists:head lists-of-lists))))
         (elements (lists:foldl lists:append (lists:nil) lists-of-lists)))
    (make-matrix rows cols (vectors:vector-from-list elements))))

;;; Lazy Sequences ;;;

(defun make-lazy-seq (computed generator state)
  "Create a lazy sequence"
  (types:make-typed 'lazy-seq (pairs:triple computed generator state)))

(defun lazy-seq? (x)
  "Check if value is a lazy sequence"
  (peano:eq (types:type-of x) 'lazy-seq))

(defun lazy-head (seq)
  "Get first element of lazy sequence"
  (let ((computed (pairs:triple-first (types:value-of seq))))
    (if (lists:nil? computed)
        (let* ((generator (pairs:triple-second (types:value-of seq)))
               (state (pairs:triple-third (types:value-of seq)))
               (result (generator state)))
          (if result
              (pairs:first result)
              (error "Empty lazy sequence")))
        (lists:head computed))))

(defun lazy-tail (seq)
  "Get tail of lazy sequence"
  (let ((computed (pairs:triple-first (types:value-of seq)))
        (generator (pairs:triple-second (types:value-of seq)))
        (state (pairs:triple-third (types:value-of seq))))
    (if (lists:nil? computed)
        (let ((result (generator state)))
          (if result
              (make-lazy-seq (lists:nil) generator (pairs:second result))
              (error "Empty lazy sequence")))
        (make-lazy-seq (lists:tail computed) generator state))))

(defun lazy-take (n seq)
  "Take n elements from lazy sequence"
  (if (peano:zero? n)
      (lists:nil)
      (lists:cons (lazy-head seq) (lazy-take (peano:p n) (lazy-tail seq)))))

(defun lazy-drop (n seq)
  "Drop n elements from lazy sequence"
  (if (peano:zero? n)
      seq
      (lazy-drop (peano:p n) (lazy-tail seq))))

(defun lazy-map (f seq)
  "Map function over lazy sequence"
  (make-lazy-seq (lists:nil)
                 (lambda (inner-seq)
                   (if (lazy-seq? inner-seq)
                       (pairs:pair (f (lazy-head inner-seq)) (lazy-tail inner-seq))
                       #f))
                 seq))

(defun lazy-filter (pred seq)
  "Filter lazy sequence by predicate"
  (make-lazy-seq (lists:nil)
                 (lambda (inner-seq)
                   (let loop ((s inner-seq))
                     (if (lazy-seq? s)
                         (let ((h (lazy-head s)))
                           (if (pred h)
                               (pairs:pair h (lazy-tail s))
                               (loop (lazy-tail s))))
                         #f)))
                 seq))

(defun lazy-concat (seq1 seq2)
  "Concatenate two lazy sequences"
  (make-lazy-seq (lists:nil)
                 (lambda (state)
                   (let ((s1 (pairs:first state))
                         (s2 (pairs:second state)))
                     (if (lazy-seq? s1)
                         (pairs:pair (lazy-head s1) (pairs:pair (lazy-tail s1) s2))
                         (if (lazy-seq? s2)
                             (pairs:pair (lazy-head s2) (pairs:pair #f (lazy-tail s2)))
                             #f))))
                 (pairs:pair seq1 seq2)))

(defun lazy-iterate (f init)
  "Create infinite sequence by iterating f"
  (make-lazy-seq (lists:nil)
                 (lambda (x) (pairs:pair x (f x)))
                 init))

(defun lazy-repeat (x)
  "Create infinite sequence of x"
  (make-lazy-seq (lists:nil)
                 (lambda (val) (pairs:pair val val))
                 x))

(defun lazy-range (start end step)
  "Create lazy range"
  (make-lazy-seq (lists:nil)
                 (lambda (n)
                   (if (peano:lt n end)
                       (pairs:pair n (arithmetic:add n step))
                       #f))
                 start))

;;; Bit Vectors ;;;

(defun make-bitvector (length bits)
  "Create a bit vector"
  (types:make-typed 'bitvector (pairs:pair length bits)))

(defun bitvector? (x)
  "Check if value is a bit vector"
  (peano:eq (types:type-of x) 'bitvector))

(defun bitvector-length (bv)
  "Get length of bit vector"
  (pairs:first (types:value-of bv)))

(defun bitvector-ref (bv index)
  "Get bit at index"
  (let ((bits (pairs:second (types:value-of bv))))
    (arithmetic:mod (arithmetic:div bits (arithmetic:pow 2 index)) 2)))

(defun bitvector-set (bv index val)
  "Set bit at index"
  (let ((length (bitvector-length bv))
        (bits (pairs:second (types:value-of bv)))
        (mask (arithmetic:pow 2 index)))
    (if (peano:eq val 1)
        (make-bitvector length (bitwise-or bits mask))
        (make-bitvector length (bitwise-and bits (bitwise-not mask))))))

(defun bitvector-clear (bv index)
  "Clear bit at index"
  (bitvector-set bv index 0))

(defun bitvector-and (a b)
  "Bitwise AND of bit vectors"
  (if (peano:eq (bitvector-length a) (bitvector-length b))
      (make-bitvector (bitvector-length a)
                      (bitwise-and (pairs:second (types:value-of a))
                                  (pairs:second (types:value-of b))))
      (error "Bit vectors must have same length")))

(defun bitvector-or (a b)
  "Bitwise OR of bit vectors"
  (if (peano:eq (bitvector-length a) (bitvector-length b))
      (make-bitvector (bitvector-length a)
                      (bitwise-or (pairs:second (types:value-of a))
                                 (pairs:second (types:value-of b))))
      (error "Bit vectors must have same length")))

(defun bitvector-xor (a b)
  "Bitwise XOR of bit vectors"
  (if (peano:eq (bitvector-length a) (bitvector-length b))
      (make-bitvector (bitvector-length a)
                      (bitwise-xor (pairs:second (types:value-of a))
                                  (pairs:second (types:value-of b))))
      (error "Bit vectors must have same length")))

(defun bitvector-not (bv)
  "Bitwise NOT of bit vector"
  (let ((length (bitvector-length bv))
        (bits (pairs:second (types:value-of bv)))
        (mask (arithmetic:sub (arithmetic:pow 2 length) 1)))
    (make-bitvector length (bitwise-xor bits mask))))

(defun bitvector-count (bv)
  "Count number of set bits"
  (let ((bits (pairs:second (types:value-of bv))))
    (let loop ((b bits) (count 0))
      (if (peano:zero? b)
          count
          (loop (arithmetic:div b 2)
                (arithmetic:add count (arithmetic:mod b 2)))))))