;;;; Homotopy Type Theory and Univalent Foundations
;;;; The most advanced foundation of mathematics

(defmodule homotopy-type-theory
  (export
    ;; Type Universe Hierarchy
    (universe 1) (universe-level 1)
    (lift-universe 2) (universe-polymorphism 1)
    (cumulative-hierarchy 2) (type-in-type 0)
    
    ;; Identity Types
    (identity-type 3) (refl 1)
    (path-type 3) (path-space 2)
    (j-eliminator 5) (k-axiom 3)
    (transport 3) (apd 3)
    (path-induction 4)
    
    ;; Higher Inductive Types
    (hit-circle 0) (hit-sphere 1)
    (hit-torus 0) (hit-suspension 1)
    (hit-pushout 3) (hit-truncation 2)
    (hit-quotient 2) (hit-coequalizer 3)
    (hit-interval 0) (hit-join 2)
    
    ;; Homotopy Levels
    (is-contr 1) (is-prop 1) (is-set 1)
    (is-groupoid 1) (is-2-groupoid 1)
    (is-n-type 2) (truncation-level 1)
    (h-level 2) (connected 2)
    
    ;; Equivalences
    (is-equiv 1) (equiv 2)
    (quasi-inverse 1) (bi-invertible 1)
    (half-adjoint-equiv 1) (contractible-fibers 1)
    (voevodsky-equiv 1) (equiv-to-path 1)
    
    ;; Univalence
    (univalence-axiom 1) (ua 1)
    (function-extensionality 0)
    (propositional-extensionality 0)
    (axiom-of-choice 1) (lem-status 1)
    
    ;; Fibrations
    (fibration 2) (fiber 3)
    (total-space 1) (projection 1)
    (pullback-fibration 3) (hofmann-streicher 1)
    (descent-data 2) (flattening-lemma 1)
    
    ;; Higher Paths
    (path-composition 2) (path-inverse 1)
    (horizontal-composition 2) (vertical-composition 2)
    (whiskering 3) (eckmann-hilton 2)
    (syllepsis 4) (coherence-laws 1)
    
    ;; Homotopy Groups
    (loop-space 2) (omega-spectrum 1)
    (homotopy-group 2) (fundamental-group 2)
    (higher-homotopy-group 3) (hopf-fibration 0)
    (freudenthal-suspension 0) (blakers-massey 0)
    
    ;; Modalities
    (modality 1) (modal-operator 1)
    (lex-modality 1) (accessible-modality 1)
    (truncation-modality 1) (shape-modality 1)
    (cohesive-modality 1) (differential-modality 1)
    
    ;; Synthetic Homotopy Theory
    (covering-space 2) (universal-cover 1)
    (deck-transformation 2) (galois-correspondence-hott 2)
    (seifert-van-kampen 0) (mayer-vietoris 0)
    (spectral-sequence 2) (serre-fibration 1)
    
    ;; Cubical Type Theory
    (interval-type 0) (path-type-cubical 3)
    (kan-filling 3) (composition 4)
    (glue-type 3) (unglue 1)
    (cubical-transport 3) (cubical-j 5)
    
    ;; Directed Type Theory
    (directed-path 3) (2-path 4)
    (kan-complex 1) (quasi-category 1)
    (simplicial-type 2) (segal-type 1)
    (complete-segal 1) (rezk-type 1)
    
    ;; Higher Topos Theory
    (infinity-topos 1) (elementary-topos 1)
    (grothendieck-topos 1) (lawvere-tierney 2)
    (subobject-classifier 1) (internal-logic 1)
    (stack 2) (higher-stack 3)
    
    ;; Cohomology
    (cohomology-theory 2) (eilenberg-maclane 2)
    (k-theory 1) (cobordism 2)
    (stable-homotopy 1) (chromatic-homotopy 1)
    (motivic-cohomology 2) (crystalline-cohomology 2)
    
    ;; Model Categories
    (model-structure 3) (quillen-equivalence 2)
    (left-proper 1) (right-proper 1)
    (cofibration 1) (fibration-model 1)
    (weak-equivalence 1) (homotopy-limit 2)
    
    ;; Higher Algebra
    (infinity-operad 1) (e-infinity-algebra 1)
    (a-infinity-algebra 1) (l-infinity-algebra 1)
    (factorization-homology 2) (topological-chiral-homology 2)
    (derived-algebraic-geometry 1)
    
    ;; Synthetic Differential Geometry
    (infinitesimal 1) (tangent-bundle 1)
    (differential-cohesion 1) (microlinear 1)
    (synthetic-differential 2) (lie-differentiation 2)
    (cartan-geometry 2) (klein-geometry 2)
    
    ;; Higher Category Theory
    (infinity-category 1) (n-category 1)
    (globular-set 1) (opetopic-set 1)
    (batanin-category 1) (trimble-category 1)
    (gray-category 1) (bicategory 1)
    
    ;; Formal Category Theory
    (yoneda-lemma-hott 2) (adjoint-functor-theorem 2)
    (monadicity-theorem 2) (beck-chevalley 4)
    (grothendieck-construction 2) (profunctor-hott 2)
    (ends-coends 3) (kan-extension-hott 3)))

(include-lib "peano/include/peano.lfe")

;;; Type Universe Hierarchy ;;;

(defun universe (level)
  "Type universe at level"
  (types:make-typed 'universe level))

(defun universe-level (type)
  "Get universe level of type"
  (cond
    ((types:type-of type) 0)
    ((universe? type) (types:value-of type))
    (else 0)))

(defun lift-universe (type n)
  "Lift type to higher universe"
  (types:make-typed 'lifted (pairs:pair type n)))

(defun universe-polymorphism (term)
  "Universe polymorphic term"
  (types:make-typed 'poly-universe term))

(defun cumulative-hierarchy (level1 level2)
  "Cumulative universe hierarchy"
  (peano:lte level1 level2))

(defun type-in-type ()
  "Type : Type (inconsistent but useful)"
  (universe 0))

(defun universe? (x)
  "Check if value is universe"
  (peano:eq (types:type-of x) 'universe))

;;; Identity Types ;;;

(defun identity-type (A a b)
  "Identity type a =_A b"
  (types:make-typed 'identity (pairs:triple A a b)))

(defun refl (a)
  "Reflexivity proof refl : a = a"
  (types:make-typed 'refl a))

(defun path-type (A a b)
  "Path type Path_A(a,b)"
  (identity-type A a b))

(defun path-space (A a)
  "Path space from a"
  (types:make-typed 'path-space (pairs:pair A a)))

(defun j-eliminator (C c-refl A a b p)
  "J eliminator (path induction)"
  (if (refl? p)
      (c-refl a)
      (j-compute C c-refl A a b p)))

(defun k-axiom (A a p)
  "K axiom (uniqueness of identity proofs)"
  (if (universe-level A)
      (refl (refl a))
      (error "K axiom only for sets")))

(defun transport (P p x)
  "Transport x along path p"
  (if (refl? p)
      x
      (transport-compute P p x)))

(defun apd (f p)
  "Dependent application of f to path p"
  (types:make-typed 'apd (pairs:pair f p)))

(defun path-induction (C c-refl p x)
  "Path induction principle"
  (j-eliminator C c-refl 
                (path-type-of p) 
                (path-start p) 
                (path-end p) 
                p))

;; Helper functions
(defun refl? (p)
  "Check if path is reflexivity"
  (peano:eq (types:type-of p) 'refl))

(defun j-compute (C c-refl A a b p)
  "Compute J elimination"
  (c-refl a)) ; Simplified

(defun transport-compute (P p x)
  "Compute transport"
  x) ; Simplified

(defun path-type-of (p) 'type) ; Simplified
(defun path-start (p) 'start) ; Simplified
(defun path-end (p) 'end) ; Simplified

;;; Higher Inductive Types ;;;

(defun hit-circle ()
  "Circle S¹"
  (types:make-typed 'hit-circle 
                    (pairs:pair 'base 'loop)))

(defun hit-sphere (n)
  "n-sphere Sⁿ"
  (types:make-typed 'hit-sphere n))

(defun hit-torus ()
  "Torus T²"
  (types:make-typed 'hit-torus 
                    (pairs:triple 'base 'meridian 'longitude)))

(defun hit-suspension (A)
  "Suspension ΣA"
  (types:make-typed 'hit-suspension 
                    (pairs:triple 'north 'south A)))

(defun hit-pushout (A B C f g)
  "Pushout of f : A → B and g : A → C"
  (types:make-typed 'hit-pushout 
                    (tuples:tuple-from-list 
                      (lists:cons A (lists:cons B (lists:cons C 
                        (lists:cons f (lists:cons g (lists:nil)))))))))

(defun hit-truncation (A n)
  "n-truncation ||A||_n"
  (types:make-typed 'hit-truncation (pairs:pair A n)))

(defun hit-quotient (A R)
  "Quotient A/R"
  (types:make-typed 'hit-quotient (pairs:pair A R)))

(defun hit-coequalizer (A B f g)
  "Coequalizer of f,g : A → B"
  (types:make-typed 'hit-coequalizer 
                    (tuples:tuple-from-list 
                      (lists:cons A (lists:cons B 
                        (lists:cons f (lists:cons g (lists:nil))))))))

(defun hit-interval ()
  "Interval I"
  (types:make-typed 'hit-interval (pairs:pair 'zero 'one)))

(defun hit-join (A B)
  "Join A * B"
  (types:make-typed 'hit-join (pairs:pair A B)))

;;; Homotopy Levels ;;;

(defun is-contr (A)
  "Is contractible"
  (types:make-typed 'is-contr A))

(defun is-prop (A)
  "Is proposition (h-level 1)"
  (types:make-typed 'is-prop A))

(defun is-set (A)
  "Is set (h-level 2)"
  (types:make-typed 'is-set A))

(defun is-groupoid (A)
  "Is groupoid (h-level 3)"
  (types:make-typed 'is-groupoid A))

(defun is-2-groupoid (A)
  "Is 2-groupoid (h-level 4)"
  (types:make-typed 'is-2-groupoid A))

(defun is-n-type (A n)
  "Is n-type (h-level n+2)"
  (types:make-typed 'is-n-type (pairs:pair A n)))

(defun truncation-level (A)
  "Get truncation level of A"
  (compute-h-level A))

(defun h-level (A n)
  "Has h-level n"
  (peano:lte (truncation-level A) n))

(defun connected (A n)
  "n-connected type"
  (types:make-typed 'connected (pairs:pair A n)))

(defun compute-h-level (A)
  "Compute h-level"
  0) ; Simplified

;;; Equivalences ;;;

(defun is-equiv (f)
  "Is equivalence"
  (types:make-typed 'is-equiv f))

(defun equiv (A B)
  "Equivalence A ≃ B"
  (types:make-typed 'equiv (pairs:pair A B)))

(defun quasi-inverse (f)
  "Quasi-inverse data"
  (types:make-typed 'quasi-inv f))

(defun bi-invertible (f)
  "Bi-invertible map"
  (types:make-typed 'bi-inv f))

(defun half-adjoint-equiv (f)
  "Half adjoint equivalence"
  (types:make-typed 'hae f))

(defun contractible-fibers (f)
  "Has contractible fibers"
  (types:make-typed 'contr-fibers f))

(defun voevodsky-equiv (f)
  "Voevodsky's definition"
  (contractible-fibers f))

(defun equiv-to-path (e)
  "Convert equivalence to path (using UA)"
  (ua e))

;;; Univalence ;;;

(defun univalence-axiom (level)
  "Univalence axiom at level"
  (types:make-typed 'univalence level))

(defun ua (e)
  "Univalence map equiv→path"
  (types:make-typed 'ua e))

(defun function-extensionality ()
  "Function extensionality"
  (types:make-typed 'funext 'axiom))

(defun propositional-extensionality ()
  "Propositional extensionality"
  (types:make-typed 'propext 'axiom))

(defun axiom-of-choice (level)
  "Axiom of choice at level"
  (types:make-typed 'ac level))

(defun lem-status (level)
  "Law of excluded middle status"
  (types:make-typed 'lem level))

;;; Fibrations ;;;

(defun fibration (E B)
  "Fibration E → B"
  (types:make-typed 'fibration (pairs:pair E B)))

(defun fiber (f b)
  "Fiber f⁻¹(b)"
  (types:make-typed 'fiber (pairs:pair f b)))

(defun total-space (P)
  "Total space Σ(x:A).P(x)"
  (types:make-typed 'total P))

(defun projection (E)
  "Projection π₁"
  (types:make-typed 'proj E))

(defun pullback-fibration (f P)
  "Pullback f*P"
  (types:make-typed 'pullback (pairs:pair f P)))

(defun hofmann-streicher (A)
  "Hofmann-Streicher universe"
  (types:make-typed 'hs-universe A))

(defun descent-data (P f)
  "Descent data for P along f"
  (types:make-typed 'descent (pairs:pair P f)))

(defun flattening-lemma (P)
  "Flattening lemma"
  (types:make-typed 'flattening P))

;;; Higher Paths ;;;

(defun path-composition (p q)
  "Path composition p · q"
  (types:make-typed 'path-comp (pairs:pair p q)))

(defun path-inverse (p)
  "Path inverse p⁻¹"
  (types:make-typed 'path-inv p))

(defun horizontal-composition (α β)
  "Horizontal composition α ★ β"
  (types:make-typed 'h-comp (pairs:pair α β)))

(defun vertical-composition (α β)
  "Vertical composition α • β"
  (types:make-typed 'v-comp (pairs:pair α β)))

(defun whiskering (p α q)
  "Whiskering p ◃ α ▹ q"
  (types:make-typed 'whisker (pairs:triple p α q)))

(defun eckmann-hilton (α β)
  "Eckmann-Hilton argument"
  (types:make-typed 'eckmann-hilton (pairs:pair α β)))

(defun syllepsis (p q r s)
  "Syllepsis coherence"
  (types:make-typed 'syllepsis 
                    (tuples:tuple-from-list 
                      (lists:cons p (lists:cons q 
                        (lists:cons r (lists:cons s (lists:nil))))))))

(defun coherence-laws (dimension)
  "Coherence laws at dimension"
  (types:make-typed 'coherence dimension))

;;; Homotopy Groups ;;;

(defun loop-space (A a)
  "Loop space Ω(A,a)"
  (path-type A a a))

(defun omega-spectrum (spaces)
  "Omega spectrum"
  (types:make-typed 'omega-spectrum spaces))

(defun homotopy-group (n A a)
  "n-th homotopy group πₙ(A,a)"
  (if (peano:zero? n)
      (path-components A)
      (fundamental-group (iterate-loop-space (peano:p n) A a))))

(defun fundamental-group (A a)
  "Fundamental group π₁(A,a)"
  (loop-space A a))

(defun higher-homotopy-group (n A a)
  "Higher homotopy group πₙ(A,a)"
  (homotopy-group n A a))

(defun hopf-fibration ()
  "Hopf fibration S³ → S²"
  (fibration (hit-sphere 3) (hit-sphere 2)))

(defun freudenthal-suspension ()
  "Freudenthal suspension theorem"
  (types:make-typed 'freudenthal 'theorem))

(defun blakers-massey ()
  "Blakers-Massey theorem"
  (types:make-typed 'blakers-massey 'theorem))

;; Helper functions
(defun path-components (A)
  "Path components π₀(A)"
  (hit-quotient A (path-relation A)))

(defun iterate-loop-space (n A a)
  "Iterate loop space n times"
  (if (peano:zero? n)
      A
      (loop-space (iterate-loop-space (peano:p n) A a) a)))

(defun path-relation (A)
  "Path equivalence relation"
  'path-rel) ; Simplified

;;; Modalities ;;;

(defun modality (L η)
  "Modality with modal operator L and unit η"
  (types:make-typed 'modality (pairs:pair L η)))

(defun modal-operator (M)
  "Modal operator L"
  (pairs:first (types:value-of M)))

(defun lex-modality (M)
  "Left exact modality"
  (types:make-typed 'lex M))

(defun accessible-modality (M)
  "Accessible modality"
  (types:make-typed 'accessible M))

(defun truncation-modality (n)
  "n-truncation modality"
  (modality (lambda (A) (hit-truncation A n))
            (lambda (A) (truncation-unit A n))))

(defun shape-modality (M)
  "Shape modality"
  (types:make-typed 'shape M))

(defun cohesive-modality (M)
  "Cohesive modality"
  (types:make-typed 'cohesive M))

(defun differential-modality (M)
  "Differential modality"
  (types:make-typed 'differential M))

(defun truncation-unit (A n)
  "Unit for truncation"
  'unit) ; Simplified

;;; Synthetic Homotopy Theory ;;;

(defun covering-space (E B)
  "Covering space E → B"
  (types:make-typed 'covering (pairs:pair E B)))

(defun universal-cover (B)
  "Universal covering space"
  (types:make-typed 'universal-cover B))

(defun deck-transformation (E B)
  "Deck transformation group"
  (types:make-typed 'deck (pairs:pair E B)))

(defun galois-correspondence-hott (covers subgroups)
  "Galois correspondence for covers"
  (types:make-typed 'galois-hott (pairs:pair covers subgroups)))

(defun seifert-van-kampen ()
  "Seifert-van Kampen theorem"
  (types:make-typed 'svk 'theorem))

(defun mayer-vietoris ()
  "Mayer-Vietoris sequence"
  (types:make-typed 'mayer-vietoris 'sequence))

(defun spectral-sequence (E r)
  "Spectral sequence E_r"
  (types:make-typed 'spectral (pairs:pair E r)))

(defun serre-fibration (E B)
  "Serre fibration"
  (types:make-typed 'serre (pairs:pair E B)))

;;; Cubical Type Theory ;;;

(defun interval-type ()
  "Cubical interval type"
  (types:make-typed 'interval 'cubical))

(defun path-type-cubical (A i a b)
  "Cubical path type"
  (types:make-typed 'cubical-path 
                    (tuples:tuple-from-list 
                      (lists:cons A (lists:cons i 
                        (lists:cons a (lists:cons b (lists:nil))))))))

(defun kan-filling (n box)
  "Kan filling operation"
  (types:make-typed 'kan-fill (pairs:pair n box)))

(defun composition (A φ u a₀)
  "Composition operation"
  (types:make-typed 'comp 
                    (tuples:tuple-from-list 
                      (lists:cons A (lists:cons φ 
                        (lists:cons u (lists:cons a₀ (lists:nil))))))))

(defun glue-type (A φ T f)
  "Glue type"
  (types:make-typed 'glue 
                    (tuples:tuple-from-list 
                      (lists:cons A (lists:cons φ 
                        (lists:cons T (lists:cons f (lists:nil))))))))

(defun unglue (φ e)
  "Unglue operation"
  (types:make-typed 'unglue (pairs:pair φ e)))

(defun cubical-transport (A φ a)
  "Cubical transport"
  (types:make-typed 'cubical-transport (pairs:triple A φ a)))

(defun cubical-j (A C d a b p)
  "Cubical J eliminator"
  (types:make-typed 'cubical-j 
                    (tuples:tuple-from-list 
                      (lists:cons A (lists:cons C 
                        (lists:cons d (lists:cons a 
                          (lists:cons b (lists:cons p (lists:nil))))))))))

;;; Directed Type Theory ;;;

(defun directed-path (A a b)
  "Directed path a → b"
  (types:make-typed 'directed-path (pairs:triple A a b)))

(defun 2-path (A f g α β)
  "2-path between paths"
  (types:make-typed '2-path 
                    (tuples:tuple-from-list 
                      (lists:cons A (lists:cons f 
                        (lists:cons g (lists:cons α 
                          (lists:cons β (lists:nil)))))))))

(defun kan-complex (K)
  "Kan complex"
  (types:make-typed 'kan-complex K))

(defun quasi-category (C)
  "Quasi-category (∞,1)-category"
  (types:make-typed 'quasi-cat C))

(defun simplicial-type (n ssets)
  "Simplicial type"
  (types:make-typed 'simplicial (pairs:pair n ssets)))

(defun segal-type (S)
  "Segal type"
  (types:make-typed 'segal S))

(defun complete-segal (S)
  "Complete Segal type"
  (types:make-typed 'complete-segal S))

(defun rezk-type (R)
  "Rezk type"
  (types:make-typed 'rezk R))

;;; Higher Topos Theory ;;;

(defun infinity-topos (T)
  "(∞,1)-topos"
  (types:make-typed 'infinity-topos T))

(defun elementary-topos (E)
  "Elementary topos"
  (types:make-typed 'elementary-topos E))

(defun grothendieck-topos (C)
  "Grothendieck topos"
  (types:make-typed 'grothendieck-topos C))

(defun lawvere-tierney (j Ω)
  "Lawvere-Tierney topology"
  (types:make-typed 'lt-topology (pairs:pair j Ω)))

(defun subobject-classifier (Ω)
  "Subobject classifier"
  (types:make-typed 'subobj-classifier Ω))

(defun internal-logic (topos)
  "Internal logic of topos"
  (types:make-typed 'internal-logic topos))

(defun stack (site conditions)
  "Stack on site"
  (types:make-typed 'stack (pairs:pair site conditions)))

(defun higher-stack (n site conditions)
  "n-stack"
  (types:make-typed 'n-stack 
                    (pairs:triple n site conditions)))

;;; Cohomology ;;;

(defun cohomology-theory (E n)
  "Cohomology theory Eⁿ"
  (types:make-typed 'cohomology (pairs:pair E n)))

(defun eilenberg-maclane (G n)
  "Eilenberg-MacLane space K(G,n)"
  (types:make-typed 'em-space (pairs:pair G n)))

(defun k-theory (R)
  "K-theory spectrum"
  (types:make-typed 'k-theory R))

(defun cobordism (n structure)
  "Cobordism theory"
  (types:make-typed 'cobordism (pairs:pair n structure)))

(defun stable-homotopy (spectrum)
  "Stable homotopy theory"
  (types:make-typed 'stable spectrum))

(defun chromatic-homotopy (height)
  "Chromatic homotopy theory"
  (types:make-typed 'chromatic height))

(defun motivic-cohomology (field spectrum)
  "Motivic cohomology"
  (types:make-typed 'motivic (pairs:pair field spectrum)))

(defun crystalline-cohomology (X p)
  "Crystalline cohomology"
  (types:make-typed 'crystalline (pairs:pair X p)))

;;; Model Categories ;;;

(defun model-structure (C W F)
  "Model structure with cofibrations C, fibrations F, weak equivalences W"
  (types:make-typed 'model-struct (pairs:triple C W F)))

(defun quillen-equivalence (F G)
  "Quillen equivalence"
  (types:make-typed 'quillen-equiv (pairs:pair F G)))

(defun left-proper (M)
  "Left proper model category"
  (types:make-typed 'left-proper M))

(defun right-proper (M)
  "Right proper model category"
  (types:make-typed 'right-proper M))

(defun cofibration (f)
  "Cofibration"
  (types:make-typed 'cofib f))

(defun fibration-model (f)
  "Fibration in model structure"
  (types:make-typed 'fib-model f))

(defun weak-equivalence (f)
  "Weak equivalence"
  (types:make-typed 'weak-equiv f))

(defun homotopy-limit (D F)
  "Homotopy limit"
  (types:make-typed 'holim (pairs:pair D F)))

;;; Higher Algebra ;;;

(defun infinity-operad (O)
  "∞-operad"
  (types:make-typed 'infinity-operad O))

(defun e-infinity-algebra (A)
  "E∞-algebra"
  (types:make-typed 'e-infinity A))

(defun a-infinity-algebra (A)
  "A∞-algebra"
  (types:make-typed 'a-infinity A))

(defun l-infinity-algebra (L)
  "L∞-algebra"
  (types:make-typed 'l-infinity L))

(defun factorization-homology (M A)
  "Factorization homology"
  (types:make-typed 'fact-homology (pairs:pair M A)))

(defun topological-chiral-homology (M A)
  "Topological chiral homology"
  (types:make-typed 'tch (pairs:pair M A)))

(defun derived-algebraic-geometry (scheme)
  "Derived algebraic geometry"
  (types:make-typed 'dag scheme))

;;; Synthetic Differential Geometry ;;;

(defun infinitesimal (ε)
  "Infinitesimal element"
  (types:make-typed 'infinitesimal ε))

(defun tangent-bundle (M)
  "Tangent bundle TM"
  (types:make-typed 'tangent M))

(defun differential-cohesion (C)
  "Differential cohesion"
  (types:make-typed 'diff-cohesion C))

(defun microlinear (space)
  "Microlinear space"
  (types:make-typed 'microlinear space))

(defun synthetic-differential (f M)
  "Synthetic differential"
  (types:make-typed 'synth-diff (pairs:pair f M)))

(defun lie-differentiation (X f)
  "Lie differentiation"
  (types:make-typed 'lie-diff (pairs:pair X f)))

(defun cartan-geometry (G H)
  "Cartan geometry"
  (types:make-typed 'cartan (pairs:pair G H)))

(defun klein-geometry (G H)
  "Klein geometry G/H"
  (types:make-typed 'klein (pairs:pair G H)))

;;; Higher Category Theory ;;;

(defun infinity-category (C)
  "(∞,1)-category"
  (types:make-typed 'infinity-cat C))

(defun n-category (n)
  "n-category"
  (types:make-typed 'n-cat n))

(defun globular-set (G)
  "Globular set"
  (types:make-typed 'globular G))

(defun opetopic-set (O)
  "Opetopic set"
  (types:make-typed 'opetopic O))

(defun batanin-category (B)
  "Batanin ω-category"
  (types:make-typed 'batanin B))

(defun trimble-category (T)
  "Trimble n-category"
  (types:make-typed 'trimble T))

(defun gray-category (G)
  "Gray category"
  (types:make-typed 'gray G))

(defun bicategory (B)
  "Bicategory"
  (types:make-typed 'bicat B))

;;; Formal Category Theory ;;;

(defun yoneda-lemma-hott (C F)
  "Yoneda lemma in HoTT"
  (types:make-typed 'yoneda-hott (pairs:pair C F)))

(defun adjoint-functor-theorem (conditions)
  "Adjoint functor theorem"
  (types:make-typed 'aft conditions))

(defun monadicity-theorem (U)
  "Beck monadicity theorem"
  (types:make-typed 'monadicity U))

(defun beck-chevalley (f g p q)
  "Beck-Chevalley condition"
  (types:make-typed 'beck-chevalley 
                    (tuples:tuple-from-list 
                      (lists:cons f (lists:cons g 
                        (lists:cons p (lists:cons q (lists:nil))))))))

(defun grothendieck-construction (F)
  "Grothendieck construction"
  (types:make-typed 'grothendieck F))

(defun profunctor-hott (C D)
  "Profunctor in HoTT"
  (types:make-typed 'prof-hott (pairs:pair C D)))

(defun ends-coends (F G H)
  "Ends and coends"
  (types:make-typed 'ends-coends (pairs:triple F G H)))

(defun kan-extension-hott (F G p)
  "Kan extension in HoTT"
  (types:make-typed 'kan-hott (pairs:triple F G p)))