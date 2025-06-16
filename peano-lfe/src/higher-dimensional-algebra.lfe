;;;; Higher Dimensional Algebra and ∞-Cosmoi
;;;; The ultimate categorical structures

(defmodule higher-dimensional-algebra
  (export
    ;; ∞-Cosmoi
    (make-infinity-cosmos 3) (infinity-cosmos? 1)
    (biequivalence 2) (cofunctor 2)
    (simplicially-coherent 1) (homotopically-coherent 1)
    (cosmos-universe 1) (cosmos-bimodule 3)
    
    ;; Higher Dimensional Categories
    (omega-category 1) (strict-omega-cat 1)
    (weak-omega-cat 1) (globular-omega-cat 1)
    (opetopic-omega-cat 1) (multitopic-cat 1)
    (computad 2) (polygraph 2)
    
    ;; Double and Multiple Categories
    (double-category 4) (triple-category 6)
    (n-tuple-category 2) (intercategory 2)
    (quintet-construction 1) (companion-pair 2)
    (h-double-functor 2) (v-double-functor 2)
    
    ;; Higher Dimensional Limits
    (weighted-bilimit 3) (indexed-bilimit 3)
    (flexible-limit 2) (pie-limit 3)
    (comma-object 3) (iso-comma 3)
    (lax-limit 2) (pseudo-limit 2)
    
    ;; Enriched Higher Categories
    (enriched-bicategory 2) (enriched-double 2)
    (fc-multicategory 2) (virtual-double 2)
    (augmented-virtual 2) (monoidal-fibration 2)
    
    ;; Higher Spans
    (span-bicategory 1) (bispan 3)
    (polynomial-functor 2) (polynomial-monad 1)
    (dependent-polynomial 2) (parametric-right-adjoint 2)
    
    ;; Higher Dimensional Monadicity
    (pseudomonad 3) (bimonad 3)
    (distributive-law 2) (wreaths 2)
    (formal-composite 2) (two-monad 3)
    (indexed-monad 2) (graded-monad 2)
    
    ;; String Diagrams
    (string-diagram 2) (surface-diagram 2)
    (movie-move 2) (isotopy-invariant 1)
    (progressive-plane 1) (sheet-diagram 2)
    
    ;; Categorified Linear Algebra
    (two-vector-space 1) (bimodule-bicategory 2)
    (morita-bicategory 1) (two-hilbert-space 1)
    (categorified-trace 1) (dimension-bicategory 1)
    
    ;; Higher Grothendieck Construction
    (two-grothendieck 1) (lax-grothendieck 1)
    (oplax-grothendieck 1) (two-dimensional-sheaf 2)
    (indexed-bicategory 2) (fibered-bicategory 2)
    
    ;; Tricategories
    (tricategory 1) (gray-tricategory 1)
    (triequivalence 2) (tritransformation 2)
    (trimodification 2) (perturbation 3)
    (syllepsis-invertor 2) (normalized-tricategory 1)
    
    ;; Higher Operads
    (multicategory 1) (cyclic-multicategory 1)
    (modular-multicategory 1) (wheeled-prop 1)
    (halfprop 1) (twisted-modular 1)
    (genus-graded 2) (feynman-category 1)
    
    ;; ∞-Dimensional Structures
    (stratified-space 2) (exit-path-category 1)
    (fundamental-category 1) (singular-manifold 2)
    (derived-manifold 1) (dg-manifold 2)
    
    ;; Higher Gauge Theory
    (two-gauge-theory 2) (three-gauge-theory 2)
    (higher-cs-theory 2) (bfv-bv-quantization 2)
    (factorization-algebra-qft 2)
    
    ;; L∞-Algebras
    (l-infinity-algebra 1) (l-infinity-morphism 2)
    (maurer-cartan 1) (dgla 1)
    (sh-lie-algebra 1) (kontsevich-formality 0)
    
    ;; Higher Symplectic
    (multisymplectic 2) (polysymplectic 2)
    (plectic 2) (n-plectic 2)
    (higher-courant 2) (exceptional-generalized 2)
    
    ;; Derived Deformation Theory
    (formal-moduli 1) (derived-deformation 2)
    (dgla-deformation 2) (l-infinity-deformation 2)
    (koszul-duality 2) (bar-cobar 2)
    
    ;; Higher Topos Cohomology
    (schreiber-cohomology 2) (nonabelian-cohomology 3)
    (differential-cohomology 2) (twisted-cohomology 3)
    (equivariant-cohomology 3) (orbifold-cohomology 2)
    
    ;; Higher Linear Logic
    (differential-linear-logic 1)
    (graded-linear-logic 1) (indexed-linear-logic 1)
    (quantum-linear-logic 1) (bunched-implications 1)
    
    ;; Higher Computational Structures
    (higher-inductive-recursive 2)
    (quotient-inductive-inductive 2)
    (higher-dimensional-rewriting 2)
    (polygraphic-resolution 2)))

(include-lib "peano/include/peano.lfe")

;;; ∞-Cosmoi ;;;

(defun make-infinity-cosmos (base simplicially-enriched bicompleteness)
  "Create an ∞-cosmos"
  (types:make-typed 'infinity-cosmos 
                    (pairs:triple base simplicially-enriched bicompleteness)))

(defun infinity-cosmos? (x)
  "Check if value is ∞-cosmos"
  (peano:eq (types:type-of x) 'infinity-cosmos))

(defun biequivalence (cosmos1 cosmos2)
  "Biequivalence between ∞-cosmoi"
  (types:make-typed 'biequivalence (pairs:pair cosmos1 cosmos2)))

(defun cofunctor (source target)
  "∞-cofunctor between cosmoi"
  (types:make-typed 'cofunctor (pairs:pair source target)))

(defun simplicially-coherent (cosmos)
  "Simplicially coherent ∞-cosmos"
  (types:make-typed 'simp-coherent cosmos))

(defun homotopically-coherent (cosmos)
  "Homotopically coherent cosmos"
  (types:make-typed 'homotopy-coherent cosmos))

(defun cosmos-universe (cosmos)
  "Universe in ∞-cosmos"
  (types:make-typed 'cosmos-universe cosmos))

(defun cosmos-bimodule (left right bimod)
  "Bimodule between ∞-cosmoi"
  (types:make-typed 'cosmos-bimodule 
                    (pairs:triple left right bimod)))

;;; Higher Dimensional Categories ;;;

(defun omega-category (data)
  "ω-category"
  (types:make-typed 'omega-category data))

(defun strict-omega-cat (data)
  "Strict ω-category"
  (types:make-typed 'strict-omega data))

(defun weak-omega-cat (data)
  "Weak ω-category"
  (types:make-typed 'weak-omega data))

(defun globular-omega-cat (data)
  "Globular ω-category"
  (types:make-typed 'globular-omega data))

(defun opetopic-omega-cat (data)
  "Opetopic ω-category"
  (types:make-typed 'opetopic-omega data))

(defun multitopic-cat (data)
  "Multitopic ω-category"
  (types:make-typed 'multitopic data))

(defun computad (dimension data)
  "n-computad"
  (types:make-typed 'computad (pairs:pair dimension data)))

(defun polygraph (dimension data)
  "n-polygraph"
  (types:make-typed 'polygraph (pairs:pair dimension data)))

;;; Double and Multiple Categories ;;;

(defun double-category (objects h-morphisms v-morphisms squares)
  "Double category"
  (types:make-typed 'double-category 
                    (tuples:tuple-from-list 
                      (lists:cons objects 
                        (lists:cons h-morphisms 
                          (lists:cons v-morphisms 
                            (lists:cons squares (lists:nil))))))))

(defun triple-category (objs h-mor v-mor t-mor squares cubes)
  "Triple category"
  (types:make-typed 'triple-category 
                    (tuples:tuple-from-list 
                      (lists:cons objs (lists:cons h-mor 
                        (lists:cons v-mor (lists:cons t-mor 
                          (lists:cons squares 
                            (lists:cons cubes (lists:nil))))))))))

(defun n-tuple-category (n data)
  "n-tuple category"
  (types:make-typed 'n-tuple-category (pairs:pair n data)))

(defun intercategory (left right)
  "Intercategory"
  (types:make-typed 'intercategory (pairs:pair left right)))

(defun quintet-construction (double-cat)
  "Quintet construction"
  (types:make-typed 'quintet double-cat))

(defun companion-pair (f g)
  "Companion pair in double category"
  (types:make-typed 'companion-pair (pairs:pair f g)))

(defun h-double-functor (source target)
  "Horizontal double functor"
  (types:make-typed 'h-double-functor (pairs:pair source target)))

(defun v-double-functor (source target)
  "Vertical double functor"
  (types:make-typed 'v-double-functor (pairs:pair source target)))

;;; Higher Dimensional Limits ;;;

(defun weighted-bilimit (diagram weight bicategory)
  "Weighted bilimit"
  (types:make-typed 'weighted-bilimit 
                    (pairs:triple diagram weight bicategory)))

(defun indexed-bilimit (indexing diagram bicategory)
  "Indexed bilimit"
  (types:make-typed 'indexed-bilimit 
                    (pairs:triple indexing diagram bicategory)))

(defun flexible-limit (diagram bicategory)
  "Flexible limit"
  (types:make-typed 'flexible-limit (pairs:pair diagram bicategory)))

(defun pie-limit (diagram weight bicategory)
  "PIE limit"
  (types:make-typed 'pie-limit 
                    (pairs:triple diagram weight bicategory)))

(defun comma-object (f g bicategory)
  "Comma object in bicategory"
  (types:make-typed 'comma-object 
                    (pairs:triple f g bicategory)))

(defun iso-comma (f g bicategory)
  "Iso-comma object"
  (types:make-typed 'iso-comma 
                    (pairs:triple f g bicategory)))

(defun lax-limit (diagram bicategory)
  "Lax limit"
  (types:make-typed 'lax-limit (pairs:pair diagram bicategory)))

(defun pseudo-limit (diagram bicategory)
  "Pseudo limit"
  (types:make-typed 'pseudo-limit (pairs:pair diagram bicategory)))

;;; Enriched Higher Categories ;;;

(defun enriched-bicategory (base enrichment)
  "Enriched bicategory"
  (types:make-typed 'enriched-bicategory (pairs:pair base enrichment)))

(defun enriched-double (base enrichment)
  "Enriched double category"
  (types:make-typed 'enriched-double (pairs:pair base enrichment)))

(defun fc-multicategory (base fibration)
  "FC-multicategory"
  (types:make-typed 'fc-multicategory (pairs:pair base fibration)))

(defun virtual-double (base equipment)
  "Virtual double category"
  (types:make-typed 'virtual-double (pairs:pair base equipment)))

(defun augmented-virtual (virtual augmentation)
  "Augmented virtual double category"
  (types:make-typed 'augmented-virtual (pairs:pair virtual augmentation)))

(defun monoidal-fibration (total base)
  "Monoidal fibration"
  (types:make-typed 'monoidal-fibration (pairs:pair total base)))

;;; Higher Spans ;;;

(defun span-bicategory (category)
  "Bicategory of spans"
  (types:make-typed 'span-bicategory category))

(defun bispan (apex left right)
  "Bispan"
  (types:make-typed 'bispan (pairs:triple apex left right)))

(defun polynomial-functor (base direction)
  "Polynomial functor"
  (types:make-typed 'polynomial-functor (pairs:pair base direction)))

(defun polynomial-monad (polynomial)
  "Polynomial monad"
  (types:make-typed 'polynomial-monad polynomial))

(defun dependent-polynomial (base family)
  "Dependent polynomial functor"
  (types:make-typed 'dependent-polynomial (pairs:pair base family)))

(defun parametric-right-adjoint (left parameter)
  "Parametric right adjoint"
  (types:make-typed 'parametric-right-adjoint (pairs:pair left parameter)))

;;; Higher Dimensional Monadicity ;;;

(defun pseudomonad (bicategory endo mult unit)
  "Pseudomonad"
  (types:make-typed 'pseudomonad 
                    (tuples:tuple-from-list 
                      (lists:cons bicategory 
                        (lists:cons endo 
                          (lists:cons mult 
                            (lists:cons unit (lists:nil))))))))

(defun bimonad (bicategory monad comonad)
  "Bimonad"
  (types:make-typed 'bimonad 
                    (pairs:triple bicategory monad comonad)))

(defun distributive-law (monad1 monad2)
  "Distributive law"
  (types:make-typed 'distributive-law (pairs:pair monad1 monad2)))

(defun wreaths (monad1 monad2)
  "Wreath product"
  (types:make-typed 'wreaths (pairs:pair monad1 monad2)))

(defun formal-composite (monad1 monad2)
  "Formal composite of monads"
  (types:make-typed 'formal-composite (pairs:pair monad1 monad2)))

(defun two-monad (bicategory data coherence)
  "2-monad"
  (types:make-typed 'two-monad 
                    (pairs:triple bicategory data coherence)))

(defun indexed-monad (indexing monad)
  "Indexed monad"
  (types:make-typed 'indexed-monad (pairs:pair indexing monad)))

(defun graded-monad (grading monad)
  "Graded monad"
  (types:make-typed 'graded-monad (pairs:pair grading monad)))

;;; String Diagrams ;;;

(defun string-diagram (dimension data)
  "String diagram"
  (types:make-typed 'string-diagram (pairs:pair dimension data)))

(defun surface-diagram (surface data)
  "Surface diagram"
  (types:make-typed 'surface-diagram (pairs:pair surface data)))

(defun movie-move (before after)
  "Movie move"
  (types:make-typed 'movie-move (pairs:pair before after)))

(defun isotopy-invariant (diagram)
  "Isotopy invariant"
  (types:make-typed 'isotopy-invariant diagram))

(defun progressive-plane (diagram)
  "Progressive plane"
  (types:make-typed 'progressive-plane diagram))

(defun sheet-diagram (dimension data)
  "Sheet diagram"
  (types:make-typed 'sheet-diagram (pairs:pair dimension data)))

;;; Categorified Linear Algebra ;;;

(defun two-vector-space (field)
  "2-vector space"
  (types:make-typed 'two-vector-space field))

(defun bimodule-bicategory (ring1 ring2)
  "Bicategory of bimodules"
  (types:make-typed 'bimodule-bicategory (pairs:pair ring1 ring2)))

(defun morita-bicategory (theory)
  "Morita bicategory"
  (types:make-typed 'morita-bicategory theory))

(defun two-hilbert-space (field)
  "2-Hilbert space"
  (types:make-typed 'two-hilbert-space field))

(defun categorified-trace (endomorphism)
  "Categorified trace"
  (types:make-typed 'categorified-trace endomorphism))

(defun dimension-bicategory (bicategory)
  "Dimension of object in bicategory"
  (types:make-typed 'dimension-bicategory bicategory))

;;; Higher Grothendieck Construction ;;;

(defun two-grothendieck (pseudofunctor)
  "2-Grothendieck construction"
  (types:make-typed 'two-grothendieck pseudofunctor))

(defun lax-grothendieck (lax-functor)
  "Lax Grothendieck construction"
  (types:make-typed 'lax-grothendieck lax-functor))

(defun oplax-grothendieck (oplax-functor)
  "Oplax Grothendieck construction"
  (types:make-typed 'oplax-grothendieck oplax-functor))

(defun two-dimensional-sheaf (site bicategory)
  "2-dimensional sheaf"
  (types:make-typed 'two-dimensional-sheaf (pairs:pair site bicategory)))

(defun indexed-bicategory (base family)
  "Indexed bicategory"
  (types:make-typed 'indexed-bicategory (pairs:pair base family)))

(defun fibered-bicategory (total base)
  "Fibered bicategory"
  (types:make-typed 'fibered-bicategory (pairs:pair total base)))

;;; Tricategories ;;;

(defun tricategory (data)
  "Tricategory"
  (types:make-typed 'tricategory data))

(defun gray-tricategory (data)
  "Gray tricategory"
  (types:make-typed 'gray-tricategory data))

(defun triequivalence (source target)
  "Triequivalence"
  (types:make-typed 'triequivalence (pairs:pair source target)))

(defun tritransformation (f g)
  "Tritransformation"
  (types:make-typed 'tritransformation (pairs:pair f g)))

(defun trimodification (alpha beta)
  "Trimodification"
  (types:make-typed 'trimodification (pairs:pair alpha beta)))

(defun perturbation (tricategory direction amount)
  "Perturbation"
  (types:make-typed 'perturbation 
                    (pairs:triple tricategory direction amount)))

(defun syllepsis-invertor (cell data)
  "Syllepsis and invertor"
  (types:make-typed 'syllepsis-invertor (pairs:pair cell data)))

(defun normalized-tricategory (tricategory)
  "Normalized tricategory"
  (types:make-typed 'normalized-tricategory tricategory))

;;; Higher Operads ;;;

(defun multicategory (data)
  "Multicategory"
  (types:make-typed 'multicategory data))

(defun cyclic-multicategory (data)
  "Cyclic multicategory"
  (types:make-typed 'cyclic-multicategory data))

(defun modular-multicategory (data)
  "Modular multicategory"
  (types:make-typed 'modular-multicategory data))

(defun wheeled-prop (data)
  "Wheeled PROP"
  (types:make-typed 'wheeled-prop data))

(defun halfprop (data)
  "HalfPROP"
  (types:make-typed 'halfprop data))

(defun twisted-modular (data)
  "Twisted modular operad"
  (types:make-typed 'twisted-modular data))

(defun genus-graded (genus data)
  "Genus graded"
  (types:make-typed 'genus-graded (pairs:pair genus data)))

(defun feynman-category (data)
  "Feynman category"
  (types:make-typed 'feynman-category data))

;;; ∞-Dimensional Structures ;;;

(defun stratified-space (space stratification)
  "Stratified space"
  (types:make-typed 'stratified-space (pairs:pair space stratification)))

(defun exit-path-category (stratified)
  "Exit path ∞-category"
  (types:make-typed 'exit-path-category stratified))

(defun fundamental-category (space)
  "Fundamental ∞-category"
  (types:make-typed 'fundamental-category space))

(defun singular-manifold (manifold singularities)
  "Singular manifold"
  (types:make-typed 'singular-manifold (pairs:pair manifold singularities)))

(defun derived-manifold (manifold)
  "Derived manifold"
  (types:make-typed 'derived-manifold manifold))

(defun dg-manifold (manifold algebra)
  "Differential graded manifold"
  (types:make-typed 'dg-manifold (pairs:pair manifold algebra)))

;;; Higher Gauge Theory ;;;

(defun two-gauge-theory (group manifold)
  "2-gauge theory"
  (types:make-typed 'two-gauge-theory (pairs:pair group manifold)))

(defun three-gauge-theory (group manifold)
  "3-gauge theory"
  (types:make-typed 'three-gauge-theory (pairs:pair group manifold)))

(defun higher-cs-theory (level manifold)
  "Higher Chern-Simons theory"
  (types:make-typed 'higher-cs-theory (pairs:pair level manifold)))

(defun bfv-bv-quantization (classical quantum)
  "BFV-BV quantization"
  (types:make-typed 'bfv-bv-quantization (pairs:pair classical quantum)))

(defun factorization-algebra-qft (spacetime observable)
  "Factorization algebra approach to QFT"
  (types:make-typed 'factorization-qft (pairs:pair spacetime observable)))

;;; L∞-Algebras ;;;

(defun l-infinity-algebra (data)
  "L∞-algebra"
  (types:make-typed 'l-infinity-algebra data))

(defun l-infinity-morphism (source target)
  "L∞-morphism"
  (types:make-typed 'l-infinity-morphism (pairs:pair source target)))

(defun maurer-cartan (algebra)
  "Maurer-Cartan elements"
  (types:make-typed 'maurer-cartan algebra))

(defun dgla (algebra)
  "Differential graded Lie algebra"
  (types:make-typed 'dgla algebra))

(defun sh-lie-algebra (algebra)
  "Strongly homotopy Lie algebra"
  (types:make-typed 'sh-lie-algebra algebra))

(defun kontsevich-formality ()
  "Kontsevich formality theorem"
  (types:make-typed 'kontsevich-formality 'theorem))

;;; Higher Symplectic ;;;

(defun multisymplectic (n manifold)
  "n-symplectic manifold"
  (types:make-typed 'multisymplectic (pairs:pair n manifold)))

(defun polysymplectic (k manifold)
  "k-polysymplectic"
  (types:make-typed 'polysymplectic (pairs:pair k manifold)))

(defun plectic (n manifold)
  "n-plectic"
  (types:make-typed 'plectic (pairs:pair n manifold)))

(defun n-plectic (n manifold)
  "n-plectic structure"
  (types:make-typed 'n-plectic (pairs:pair n manifold)))

(defun higher-courant (level algebroid)
  "Higher Courant algebroid"
  (types:make-typed 'higher-courant (pairs:pair level algebroid)))

(defun exceptional-generalized (type geometry)
  "Exceptional generalized geometry"
  (types:make-typed 'exceptional-generalized (pairs:pair type geometry)))

;;; Derived Deformation Theory ;;;

(defun formal-moduli (problem)
  "Formal moduli problem"
  (types:make-typed 'formal-moduli problem))

(defun derived-deformation (space theory)
  "Derived deformation theory"
  (types:make-typed 'derived-deformation (pairs:pair space theory)))

(defun dgla-deformation (dgla element)
  "DGLA deformation"
  (types:make-typed 'dgla-deformation (pairs:pair dgla element)))

(defun l-infinity-deformation (l-inf element)
  "L∞ deformation"
  (types:make-typed 'l-infinity-deformation (pairs:pair l-inf element)))

(defun koszul-duality (algebra coalgebra)
  "Koszul duality"
  (types:make-typed 'koszul-duality (pairs:pair algebra coalgebra)))

(defun bar-cobar (algebra coalgebra)
  "Bar-cobar duality"
  (types:make-typed 'bar-cobar (pairs:pair algebra coalgebra)))

;;; Higher Topos Cohomology ;;;

(defun schreiber-cohomology (space coefficients)
  "Schreiber's cohomology"
  (types:make-typed 'schreiber-cohomology (pairs:pair space coefficients)))

(defun nonabelian-cohomology (space group degree)
  "Nonabelian cohomology"
  (types:make-typed 'nonabelian-cohomology 
                    (pairs:triple space group degree)))

(defun differential-cohomology (space structure)
  "Differential cohomology"
  (types:make-typed 'differential-cohomology (pairs:pair space structure)))

(defun twisted-cohomology (space twist coefficients)
  "Twisted cohomology"
  (types:make-typed 'twisted-cohomology 
                    (pairs:triple space twist coefficients)))

(defun equivariant-cohomology (space group coefficients)
  "Equivariant cohomology"
  (types:make-typed 'equivariant-cohomology 
                    (pairs:triple space group coefficients)))

(defun orbifold-cohomology (orbifold coefficients)
  "Orbifold cohomology"
  (types:make-typed 'orbifold-cohomology (pairs:pair orbifold coefficients)))

;;; Higher Linear Logic ;;;

(defun differential-linear-logic (syntax)
  "Differential linear logic"
  (types:make-typed 'differential-linear-logic syntax))

(defun graded-linear-logic (grading)
  "Graded linear logic"
  (types:make-typed 'graded-linear-logic grading))

(defun indexed-linear-logic (indexing)
  "Indexed linear logic"
  (types:make-typed 'indexed-linear-logic indexing))

(defun quantum-linear-logic (basis)
  "Quantum linear logic"
  (types:make-typed 'quantum-linear-logic basis))

(defun bunched-implications (logic)
  "Bunched implications logic"
  (types:make-typed 'bunched-implications logic))

;;; Higher Computational Structures ;;;

(defun higher-inductive-recursive (mutual definition)
  "Higher inductive-recursive definition"
  (types:make-typed 'higher-inductive-recursive (pairs:pair mutual definition)))

(defun quotient-inductive-inductive (left right)
  "Quotient inductive-inductive type"
  (types:make-typed 'quotient-inductive-inductive (pairs:pair left right)))

(defun higher-dimensional-rewriting (dimension system)
  "Higher dimensional rewriting"
  (types:make-typed 'higher-dimensional-rewriting (pairs:pair dimension system)))

(defun polygraphic-resolution (complex resolution)
  "Polygraphic resolution"
  (types:make-typed 'polygraphic-resolution (pairs:pair complex resolution)))