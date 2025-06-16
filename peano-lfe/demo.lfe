#!/usr/bin/env lfescript
;;; Interactive demo of all Peano-LFE modules

(defun main (args)
  (io:format "~n=== Peano-LFE Demo ===~n~n")
  
  ;; Basic Peano arithmetic
  (demo-peano)
  
  ;; Arithmetic operations
  (demo-arithmetic)
  
  ;; Data structures
  (demo-pairs)
  (demo-booleans)
  (demo-lists)
  (demo-tuples)
  (demo-vectors)
  (demo-sets)
  (demo-maps)
  
  ;; Advanced types
  (demo-records)
  (demo-adt)
  (demo-hypergraph)
  
  (io:format "~n=== Demo Complete ===~n"))

(defun demo-peano ()
  (io:format "~n--- Peano Numbers ---~n")
  (let* ((zero (peano:zero))
         (one (peano:s zero))
         (two (peano:s one))
         (three (peano:s two)))
    (io:format "Zero: ~p~n" (list zero))
    (io:format "One: ~p~n" (list one))
    (io:format "Three as natural: ~p~n" (list (peano:peano->nat three)))
    (io:format "Is 2 < 3? ~p~n" (list (peano:lt two three)))))

(defun demo-arithmetic ()
  (io:format "~n--- Arithmetic Operations ---~n")
  (let* ((five (peano:nat->peano 5))
         (three (peano:nat->peano 3)))
    (io:format "5 + 3 = ~p~n" (list (peano:peano->nat (arithmetic:add five three))))
    (io:format "5 * 3 = ~p~n" (list (peano:peano->nat (arithmetic:mult five three))))
    (io:format "5^3 = ~p~n" (list (peano:peano->nat (arithmetic:pow five three))))
    (io:format "5! = ~p~n" (list (peano:peano->nat (arithmetic:fact five))))
    (io:format "gcd(15,10) = ~p~n" (list (peano:peano->nat (arithmetic:gcd (peano:nat->peano 15) (peano:nat->peano 10)))))
    (io:format "Is 7 prime? ~p~n" (list (arithmetic:prime? (peano:nat->peano 7))))))

(defun demo-pairs ()
  (io:format "~n--- Pairs ---~n")
  (let* ((p (pairs:pair (peano:nat->peano 42) (peano:nat->peano 17)))
         (fst (pairs:first p))
         (snd (pairs:second p)))
    (io:format "Pair(42,17) first: ~p~n" (list (peano:peano->nat fst)))
    (io:format "Pair(42,17) second: ~p~n" (list (peano:peano->nat snd)))))

(defun demo-booleans ()
  (io:format "~n--- Booleans ---~n")
  (let* ((t (types:true))
         (f (types:false)))
    (io:format "True boolean: ~p~n" (list (types:boolean-value t)))
    (io:format "False boolean: ~p~n" (list (types:boolean-value f)))))

(defun demo-lists ()
  (io:format "~n--- Lists ---~n")
  (let* ((l1 (lists:cons (peano:nat->peano 1)
               (lists:cons (peano:nat->peano 2)
                (lists:cons (peano:nat->peano 3)
                 (lists:nil)))))
         (len (lists:length l1)))
    (io:format "List [1,2,3] length: ~p~n" (list (peano:peano->nat len)))
    (io:format "Head of list: ~p~n" (list (peano:peano->nat (lists:head l1))))
    (let ((sum (lists:foldl #'arithmetic:add/2 (peano:zero) l1)))
      (io:format "Sum of list: ~p~n" (list (peano:peano->nat sum))))))

(defun demo-tuples ()
  (io:format "~n--- Tuples ---~n")
  (let* ((t3 (tuples:triple-tuple (peano:nat->peano 10)
                                  (peano:nat->peano 20)
                                  (peano:nat->peano 30))))
    (io:format "Tuple(10,20,30) element 1: ~p~n" 
               (list (peano:peano->nat (tuples:tuple-ref t3 (peano:s (peano:zero))))))
    (io:format "Tuple(10,20,30) element 2: ~p~n" 
               (list (peano:peano->nat (tuples:tuple-ref t3 (peano:s (peano:s (peano:zero)))))))))

(defun demo-vectors ()
  (io:format "~n--- Vectors ---~n")
  (let* ((v (vectors:vector-from-list 
             (lists:cons (peano:nat->peano 5)
              (lists:cons (peano:nat->peano 10)
               (lists:cons (peano:nat->peano 15)
                (lists:nil))))))
         (elem (vectors:vector-ref v (peano:s (peano:zero)))))
    (io:format "Vector [5,10,15] at index 1: ~p~n" (list (peano:peano->nat elem)))
    (io:format "Vector size: ~p~n" (list (peano:peano->nat (vectors:vector-size v))))))

(defun demo-sets ()
  (io:format "~n--- Sets ---~n")
  (let* ((s1 (sets:set-add (sets:set-add (sets:set-add (sets:set-empty) (peano:nat->peano 1)) (peano:nat->peano 2)) (peano:nat->peano 3)))
         (s2 (sets:set-add (sets:set-add (sets:set-add (sets:set-empty) (peano:nat->peano 2)) (peano:nat->peano 3)) (peano:nat->peano 4)))
         (union (sets:set-union s1 s2))
         (inter (sets:set-intersection s1 s2)))
    (io:format "Set {1,2,3} size: ~p~n" (list (peano:peano->nat (sets:set-size s1))))
    (io:format "Union size: ~p~n" (list (peano:peano->nat (sets:set-size union))))
    (io:format "Intersection size: ~p~n" (list (peano:peano->nat (sets:set-size inter))))))

(defun demo-maps ()
  (io:format "~n--- Maps ---~n")
  (let* ((m (maps:map-insert (maps:map-insert (maps:map-empty) (peano:nat->peano 1) (peano:nat->peano 100)) (peano:nat->peano 2) (peano:nat->peano 200)))
         (val (maps:map-get m (peano:nat->peano 1))))
    (io:format "Map get(1): ~p~n" 
               (list (if (adt:is-some? val)
                         (peano:peano->nat (adt:some-value val))
                         'not-found)))))

(defun demo-records ()
  (io:format "~n--- Records ---~n")
  (let* ((person (records:make-person (peano:nat->peano 42) (peano:nat->peano 25))))
    (io:format "Person record type: ~p~n" (list (peano:peano->nat (records:record-type-id person))))
    (io:format "Person age field: ~p~n" (list (peano:peano->nat (records:person-age person))))))

(defun demo-adt ()
  (io:format "~n--- Algebraic Data Types ---~n")
  (let* ((some-val (adt:some (peano:nat->peano 42)))
         (none-val (adt:none)))
    (io:format "Option Some(42): ~p~n" 
               (list (if (adt:is-some? some-val)
                         (peano:peano->nat (adt:some-value some-val))
                         'nothing)))
    (io:format "Option None: ~p~n"
               (list (if (adt:is-some? none-val)
                         (peano:peano->nat (adt:some-value none-val))
                         'nothing)))))

(defun demo-hypergraph ()
  (io:format "~n--- Hypergraph ---~n")
  (let* ((g (hypergraphs:hypergraph-empty))
         (g1 (hypergraphs:add-vertex g (peano:nat->peano 1)))
         (g2 (hypergraphs:add-vertex g1 (peano:nat->peano 2)))
         (g3 (hypergraphs:add-vertex g2 (peano:nat->peano 3)))
         (edge (lists:cons (peano:nat->peano 1)
                (lists:cons (peano:nat->peano 2)
                 (lists:cons (peano:nat->peano 3)
                  (lists:nil)))))
         (g4 (hypergraphs:add-hyperedge g3 edge)))
    (io:format "Hypergraph vertices: ~p~n" 
               (list (peano:peano->nat (sets:set-size (hypergraphs:vertices g4)))))
    (io:format "Hypergraph edges: ~p~n"
               (list (peano:peano->nat (lists:length (hypergraphs:hyperedges g4)))))))