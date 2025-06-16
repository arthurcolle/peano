;;;; Grothendieck Mathematics
;;;; The revolutionary vision of Alexander Grothendieck

(defmodule grothendieck-mathematics
  (export
    ;; Schemes and Algebraic Geometry
    (make-scheme 2) (scheme? 1)
    (affine-scheme 1) (projective-scheme 1)
    (spec 1) (proj 1)
    (structure-sheaf 1) (scheme-morphism 2)
    (base-change 2) (fiber-product 2)
    (separated-scheme 1) (proper-morphism 1)
    
    ;; Topos Theory
    (make-topos 2) (topos? 1)
    (grothendieck-topos 1) (elementary-topos 1)
    (sheaf-topos 2) (presheaf-topos 1)
    (geometric-morphism 2) (logical-functor 2)
    (subobject-classifier 1) (power-object 2)
    (lawvere-tierney-topology 2) (internal-logic 1)
    
    ;; Categories and Functors
    (make-category 3) (category? 1)
    (fibered-category 2) (grothendieck-construction 1)
    (stack 2) (algebraic-stack 2)
    (gerbe 2) (2-gerbe 2)
    (descent-data 2) (effective-descent 1)
    (flat-descent 1) (faithfully-flat-descent 1)
    
    ;; Cohomology Theories
    (etale-cohomology 2) (l-adic-cohomology 3)
    (crystalline-cohomology 2) (de-rham-cohomology 1)
    (flat-cohomology 2) (fppf-cohomology 2)
    (syntomic-cohomology 2) (motivic-cohomology 2)
    (weil-cohomology 1) (universal-cohomology 0)
    
    ;; Grothendieck Topologies
    (make-site 2) (site? 1)
    (zariski-topology 1) (etale-topology 1)
    (fppf-topology 1) (fpqc-topology 1)
    (nisnevich-topology 1) (cdh-topology 1)
    (h-topology 1) (qfh-topology 1)
    (covering-family 2) (sheaf-condition 2)
    
    ;; Derived Categories
    (derived-category 1) (bounded-derived 1)
    (perfect-complex 1) (pseudo-coherent 1)
    (dualizing-complex 1) (exceptional-inverse-image 2)
    (derived-tensor-product 2) (derived-hom 2)
    (six-operations 0) (grothendieck-duality 2)
    
    ;; Motives
    (pure-motive 2) (mixed-motive 2)
    (chow-motive 1) (numerical-motive 1)
    (motivic-galois-group 1) (tannakian-category 1)
    (standard-conjecture 1) (motivic-decomposition 1)
    (motivic-realization 2) (period-matrix 1)
    
    ;; Fundamental Groups
    (etale-fundamental-group 2) (algebraic-fundamental-group 1)
    (tame-fundamental-group 2) (wild-fundamental-group 2)
    (galois-category 2) (fiber-functor 2)
    (grothendieck-galois-theory 0) (anabelian-geometry 1)
    
    ;; Moduli Spaces
    (moduli-problem 1) (fine-moduli 1)
    (coarse-moduli 1) (moduli-stack 1)
    (hilbert-scheme 2) (quot-scheme 3)
    (picard-scheme 1) (albanese-variety 1)
    (deformation-theory 2) (cotangent-complex 1)
    
    ;; Grothendieck-Riemann-Roch
    (todd-class 1) (chern-character 1)
    (grothendieck-group 1) (k-theory 1)
    (adams-operations 2) (lambda-operations 2)
    (grr-formula 2) (hirzebruch-rr 2)
    
    ;; EGA/SGA Concepts
    (ega-style-morphism 1) (universally-closed 1)
    (universally-open 1) (formally-smooth 1)
    (formally-unramified 1) (formally-etale 1)
    (excellent-ring 1) (japanese-ring 1)
    (nagata-ring 1) (henselian-local 1)
    
    ;; Algebraic Spaces
    (algebraic-space 1) (etale-equivalence-relation 2)
    (algebraic-space-stack 1) (deligne-mumford-stack 1)
    (artin-stack 1) (geometric-point 2)
    (atlas 1) (smooth-cover 1)
    
    ;; Intersection Theory
    (chow-ring 1) (operational-chow 1)
    (bivariant-theory 1) (fulton-macpherson 2)
    (excess-intersection 2) (virtual-fundamental-class 1)
    (cone-construction 1) (segre-class 1)
    
    ;; Perverse Sheaves
    (perverse-sheaf 2) (intersection-cohomology 2)
    (decomposition-theorem 1) (hard-lefschetz 1)
    (weight-filtration 1) (purity-theorem 1)
    (nearby-cycles 2) (vanishing-cycles 2)
    
    ;; Grothendieck's Vision
    (pursuing-stacks 0) (homotopy-hypothesis 0)
    (test-categories 1) (modelizers 1)
    (derivateurs 1) (anabelian-section-conjecture 0)
    (dessins-denfants 1) (esquisse-dun-programme 0)
    
    ;; Categorical Patterns
    (kan-extension 3) (yoneda-embedding 1)
    (day-convolution 2) (ends-coends 2)
    (weighted-limit 3) (indexed-limit 3)
    (monadicity-theorem 1) (beck-chevalley 4)))

(include-lib "peano/include/peano.lfe")

;;; Schemes and Algebraic Geometry ;;;

(defun make-scheme (topology structure-sheaf)
  "Create a scheme from topology and structure sheaf"
  (types:make-typed 'scheme (pairs:pair topology structure-sheaf)))

(defun scheme? (x)
  "Check if value is a scheme"
  (peano:eq (types:type-of x) 'scheme))

(defun affine-scheme (ring)
  "Affine scheme Spec(R)"
  (types:make-typed 'affine-scheme ring))

(defun projective-scheme (ring)
  "Projective scheme Proj(R)"
  (types:make-typed 'projective-scheme ring))

(defun spec (ring)
  "Spectrum of a ring"
  (affine-scheme ring))

(defun proj (graded-ring)
  "Proj construction"
  (types:make-typed 'proj graded-ring))

(defun structure-sheaf (scheme)
  "Structure sheaf of scheme"
  (pairs:second (types:value-of scheme)))

(defun scheme-morphism (source target)
  "Morphism of schemes"
  (types:make-typed 'scheme-morphism (pairs:pair source target)))

(defun base-change (morphism new-base)
  "Base change of morphism"
  (types:make-typed 'base-change (pairs:pair morphism new-base)))

(defun fiber-product (x-to-s y-to-s)
  "Fiber product X ×_S Y"
  (types:make-typed 'fiber-product (pairs:pair x-to-s y-to-s)))

(defun separated-scheme (scheme)
  "Check if scheme is separated"
  (types:make-typed 'separated scheme))

(defun proper-morphism (morphism)
  "Check if morphism is proper"
  (types:make-typed 'proper morphism))

;;; Topos Theory ;;;

(defun make-topos (category topology)
  "Create a topos"
  (types:make-typed 'topos (pairs:pair category topology)))

(defun topos? (x)
  "Check if value is a topos"
  (peano:eq (types:type-of x) 'topos))

(defun grothendieck-topos (site)
  "Grothendieck topos of sheaves"
  (types:make-typed 'grothendieck-topos site))

(defun elementary-topos (axioms)
  "Elementary topos"
  (types:make-typed 'elementary-topos axioms))

(defun sheaf-topos (site coverage)
  "Topos of sheaves on site"
  (grothendieck-topos (make-site site coverage)))

(defun presheaf-topos (category)
  "Presheaf topos"
  (types:make-typed 'presheaf-topos category))

(defun geometric-morphism (source target)
  "Geometric morphism of topoi"
  (types:make-typed 'geometric-morphism (pairs:pair source target)))

(defun logical-functor (source target)
  "Logical functor between topoi"
  (types:make-typed 'logical-functor (pairs:pair source target)))

(defun subobject-classifier (topos)
  "Subobject classifier Ω"
  (types:make-typed 'subobject-classifier topos))

(defun power-object (topos object)
  "Power object P(A)"
  (types:make-typed 'power-object (pairs:pair topos object)))

(defun lawvere-tierney-topology (topos j)
  "Lawvere-Tierney topology"
  (types:make-typed 'lawvere-tierney (pairs:pair topos j)))

(defun internal-logic (topos)
  "Internal logic of topos"
  (types:make-typed 'internal-logic topos))

;;; Categories and Functors ;;;

(defun make-category (objects morphisms composition)
  "Create a category"
  (types:make-typed 'category 
                    (pairs:triple objects morphisms composition)))

(defun category? (x)
  "Check if value is a category"
  (peano:eq (types:type-of x) 'category))

(defun fibered-category (total base)
  "Fibered category"
  (types:make-typed 'fibered-category (pairs:pair total base)))

(defun grothendieck-construction (pseudofunctor)
  "Grothendieck construction"
  (types:make-typed 'grothendieck-construction pseudofunctor))

(defun stack (site conditions)
  "Stack on site"
  (types:make-typed 'stack (pairs:pair site conditions)))

(defun algebraic-stack (site representability)
  "Algebraic stack"
  (types:make-typed 'algebraic-stack (pairs:pair site representability)))

(defun gerbe (site band)
  "Gerbe with band"
  (types:make-typed 'gerbe (pairs:pair site band)))

(defun 2-gerbe (site band)
  "2-gerbe"
  (types:make-typed '2-gerbe (pairs:pair site band)))

(defun descent-data (covering data)
  "Descent data"
  (types:make-typed 'descent-data (pairs:pair covering data)))

(defun effective-descent (morphism)
  "Effective descent morphism"
  (types:make-typed 'effective-descent morphism))

(defun flat-descent (morphism)
  "Flat descent"
  (types:make-typed 'flat-descent morphism))

(defun faithfully-flat-descent (morphism)
  "Faithfully flat descent"
  (types:make-typed 'ff-descent morphism))

;;; Cohomology Theories ;;;

(defun etale-cohomology (scheme coefficients)
  "Étale cohomology"
  (types:make-typed 'etale-cohomology (pairs:pair scheme coefficients)))

(defun l-adic-cohomology (scheme l prime?)
  "ℓ-adic cohomology"
  (types:make-typed 'l-adic-cohomology 
                    (pairs:triple scheme l prime?)))

(defun crystalline-cohomology (scheme prime)
  "Crystalline cohomology"
  (types:make-typed 'crystalline-cohomology (pairs:pair scheme prime)))

(defun de-rham-cohomology (scheme)
  "de Rham cohomology"
  (types:make-typed 'de-rham-cohomology scheme))

(defun flat-cohomology (scheme coefficients)
  "Flat cohomology"
  (types:make-typed 'flat-cohomology (pairs:pair scheme coefficients)))

(defun fppf-cohomology (scheme coefficients)
  "fppf cohomology"
  (types:make-typed 'fppf-cohomology (pairs:pair scheme coefficients)))

(defun syntomic-cohomology (scheme n)
  "Syntomic cohomology"
  (types:make-typed 'syntomic-cohomology (pairs:pair scheme n)))

(defun motivic-cohomology (scheme coefficients)
  "Motivic cohomology"
  (types:make-typed 'motivic-cohomology (pairs:pair scheme coefficients)))

(defun weil-cohomology (theory)
  "Weil cohomology theory"
  (types:make-typed 'weil-cohomology theory))

(defun universal-cohomology ()
  "Universal cohomology theory"
  (types:make-typed 'universal-cohomology 'grothendieck-dream))

;;; Grothendieck Topologies ;;;

(defun make-site (category coverage)
  "Create a site"
  (types:make-typed 'site (pairs:pair category coverage)))

(defun site? (x)
  "Check if value is a site"
  (peano:eq (types:type-of x) 'site))

(defun zariski-topology (scheme)
  "Zariski topology"
  (types:make-typed 'zariski-topology scheme))

(defun etale-topology (scheme)
  "Étale topology"
  (types:make-typed 'etale-topology scheme))

(defun fppf-topology (scheme)
  "fppf topology"
  (types:make-typed 'fppf-topology scheme))

(defun fpqc-topology (scheme)
  "fpqc topology"
  (types:make-typed 'fpqc-topology scheme))

(defun nisnevich-topology (scheme)
  "Nisnevich topology"
  (types:make-typed 'nisnevich-topology scheme))

(defun cdh-topology (scheme)
  "cdh topology"
  (types:make-typed 'cdh-topology scheme))

(defun h-topology (scheme)
  "h-topology"
  (types:make-typed 'h-topology scheme))

(defun qfh-topology (scheme)
  "qfh topology"
  (types:make-typed 'qfh-topology scheme))

(defun covering-family (object covers)
  "Covering family"
  (types:make-typed 'covering-family (pairs:pair object covers)))

(defun sheaf-condition (presheaf covering)
  "Check sheaf condition"
  (types:make-typed 'sheaf-condition (pairs:pair presheaf covering)))

;;; Derived Categories ;;;

(defun derived-category (abelian)
  "Derived category D(A)"
  (types:make-typed 'derived-category abelian))

(defun bounded-derived (abelian)
  "Bounded derived category D^b(A)"
  (types:make-typed 'bounded-derived abelian))

(defun perfect-complex (scheme)
  "Perfect complex"
  (types:make-typed 'perfect-complex scheme))

(defun pseudo-coherent (complex)
  "Pseudo-coherent complex"
  (types:make-typed 'pseudo-coherent complex))

(defun dualizing-complex (scheme)
  "Dualizing complex"
  (types:make-typed 'dualizing-complex scheme))

(defun exceptional-inverse-image (morphism complex)
  "Exceptional inverse image f^!"
  (types:make-typed 'exceptional-inverse (pairs:pair morphism complex)))

(defun derived-tensor-product (complex1 complex2)
  "Derived tensor product"
  (types:make-typed 'derived-tensor (pairs:pair complex1 complex2)))

(defun derived-hom (complex1 complex2)
  "Derived Hom"
  (types:make-typed 'derived-hom (pairs:pair complex1 complex2)))

(defun six-operations ()
  "Six operations formalism"
  (types:make-typed 'six-operations 'grothendieck))

(defun grothendieck-duality (morphism dualizing)
  "Grothendieck duality"
  (types:make-typed 'grothendieck-duality (pairs:pair morphism dualizing)))

;;; Motives ;;;

(defun pure-motive (variety equivalence)
  "Pure motive"
  (types:make-typed 'pure-motive (pairs:pair variety equivalence)))

(defun mixed-motive (variety weight)
  "Mixed motive"
  (types:make-typed 'mixed-motive (pairs:pair variety weight)))

(defun chow-motive (variety)
  "Chow motive"
  (types:make-typed 'chow-motive variety))

(defun numerical-motive (variety)
  "Numerical motive"
  (types:make-typed 'numerical-motive variety))

(defun motivic-galois-group (category)
  "Motivic Galois group"
  (types:make-typed 'motivic-galois-group category))

(defun tannakian-category (category)
  "Tannakian category"
  (types:make-typed 'tannakian-category category))

(defun standard-conjecture (type)
  "Standard conjecture"
  (types:make-typed 'standard-conjecture type))

(defun motivic-decomposition (variety)
  "Motivic decomposition"
  (types:make-typed 'motivic-decomposition variety))

(defun motivic-realization (motive cohomology)
  "Motivic realization"
  (types:make-typed 'motivic-realization (pairs:pair motive cohomology)))

(defun period-matrix (motive)
  "Period matrix"
  (types:make-typed 'period-matrix motive))

;;; Fundamental Groups ;;;

(defun etale-fundamental-group (scheme basepoint)
  "Étale fundamental group"
  (types:make-typed 'etale-fundamental (pairs:pair scheme basepoint)))

(defun algebraic-fundamental-group (scheme)
  "Algebraic fundamental group"
  (types:make-typed 'algebraic-fundamental scheme))

(defun tame-fundamental-group (scheme primes)
  "Tame fundamental group"
  (types:make-typed 'tame-fundamental (pairs:pair scheme primes)))

(defun wild-fundamental-group (scheme prime)
  "Wild fundamental group"
  (types:make-typed 'wild-fundamental (pairs:pair scheme prime)))

(defun galois-category (scheme basepoint)
  "Galois category"
  (types:make-typed 'galois-category (pairs:pair scheme basepoint)))

(defun fiber-functor (category point)
  "Fiber functor"
  (types:make-typed 'fiber-functor (pairs:pair category point)))

(defun grothendieck-galois-theory ()
  "Grothendieck's Galois theory"
  (types:make-typed 'grothendieck-galois 'categorical))

(defun anabelian-geometry (curve)
  "Anabelian geometry"
  (types:make-typed 'anabelian-geometry curve))

;;; Moduli Spaces ;;;

(defun moduli-problem (functor)
  "Moduli problem"
  (types:make-typed 'moduli-problem functor))

(defun fine-moduli (space)
  "Fine moduli space"
  (types:make-typed 'fine-moduli space))

(defun coarse-moduli (space)
  "Coarse moduli space"
  (types:make-typed 'coarse-moduli space))

(defun moduli-stack (problem)
  "Moduli stack"
  (types:make-typed 'moduli-stack problem))

(defun hilbert-scheme (scheme polynomial)
  "Hilbert scheme"
  (types:make-typed 'hilbert-scheme (pairs:pair scheme polynomial)))

(defun quot-scheme (scheme module polynomial)
  "Quot scheme"
  (types:make-typed 'quot-scheme 
                    (pairs:triple scheme module polynomial)))

(defun picard-scheme (scheme)
  "Picard scheme"
  (types:make-typed 'picard-scheme scheme))

(defun albanese-variety (variety)
  "Albanese variety"
  (types:make-typed 'albanese-variety variety))

(defun deformation-theory (scheme infinitesimal)
  "Deformation theory"
  (types:make-typed 'deformation-theory (pairs:pair scheme infinitesimal)))

(defun cotangent-complex (morphism)
  "Cotangent complex"
  (types:make-typed 'cotangent-complex morphism))

;;; Grothendieck-Riemann-Roch ;;;

(defun todd-class (bundle)
  "Todd class"
  (types:make-typed 'todd-class bundle))

(defun chern-character (bundle)
  "Chern character"
  (types:make-typed 'chern-character bundle))

(defun grothendieck-group (category)
  "Grothendieck group K_0"
  (types:make-typed 'grothendieck-group category))

(defun k-theory (scheme)
  "K-theory"
  (types:make-typed 'k-theory scheme))

(defun adams-operations (element k)
  "Adams operations ψ^k"
  (types:make-typed 'adams-operations (pairs:pair element k)))

(defun lambda-operations (element k)
  "Lambda operations λ^k"
  (types:make-typed 'lambda-operations (pairs:pair element k)))

(defun grr-formula (morphism sheaf)
  "Grothendieck-Riemann-Roch formula"
  (types:make-typed 'grr-formula (pairs:pair morphism sheaf)))

(defun hirzebruch-rr (variety sheaf)
  "Hirzebruch-Riemann-Roch"
  (types:make-typed 'hirzebruch-rr (pairs:pair variety sheaf)))

;;; EGA/SGA Concepts ;;;

(defun ega-style-morphism (morphism)
  "EGA-style morphism properties"
  (types:make-typed 'ega-morphism morphism))

(defun universally-closed (morphism)
  "Universally closed"
  (types:make-typed 'universally-closed morphism))

(defun universally-open (morphism)
  "Universally open"
  (types:make-typed 'universally-open morphism))

(defun formally-smooth (morphism)
  "Formally smooth"
  (types:make-typed 'formally-smooth morphism))

(defun formally-unramified (morphism)
  "Formally unramified"
  (types:make-typed 'formally-unramified morphism))

(defun formally-etale (morphism)
  "Formally étale"
  (types:make-typed 'formally-etale morphism))

(defun excellent-ring (ring)
  "Excellent ring"
  (types:make-typed 'excellent-ring ring))

(defun japanese-ring (ring)
  "Japanese ring (universally catenary)"
  (types:make-typed 'japanese-ring ring))

(defun nagata-ring (ring)
  "Nagata ring"
  (types:make-typed 'nagata-ring ring))

(defun henselian-local (ring)
  "Henselian local ring"
  (types:make-typed 'henselian-local ring))

;;; Algebraic Spaces ;;;

(defun algebraic-space (quotient)
  "Algebraic space"
  (types:make-typed 'algebraic-space quotient))

(defun etale-equivalence-relation (scheme relation)
  "Étale equivalence relation"
  (types:make-typed 'etale-equivalence (pairs:pair scheme relation)))

(defun algebraic-space-stack (space)
  "Algebraic space as stack"
  (types:make-typed 'algebraic-space-stack space))

(defun deligne-mumford-stack (stack)
  "Deligne-Mumford stack"
  (types:make-typed 'deligne-mumford stack))

(defun artin-stack (stack)
  "Artin stack"
  (types:make-typed 'artin-stack stack))

(defun geometric-point (space field)
  "Geometric point"
  (types:make-typed 'geometric-point (pairs:pair space field)))

(defun atlas (stack)
  "Atlas for stack"
  (types:make-typed 'atlas stack))

(defun smooth-cover (space)
  "Smooth cover"
  (types:make-typed 'smooth-cover space))

;;; Intersection Theory ;;;

(defun chow-ring (variety)
  "Chow ring"
  (types:make-typed 'chow-ring variety))

(defun operational-chow (variety)
  "Operational Chow ring"
  (types:make-typed 'operational-chow variety))

(defun bivariant-theory (category)
  "Bivariant theory"
  (types:make-typed 'bivariant-theory category))

(defun fulton-macpherson (normal deformation)
  "Fulton-MacPherson deformation"
  (types:make-typed 'fulton-macpherson (pairs:pair normal deformation)))

(defun excess-intersection (expected actual)
  "Excess intersection"
  (types:make-typed 'excess-intersection (pairs:pair expected actual)))

(defun virtual-fundamental-class (stack)
  "Virtual fundamental class"
  (types:make-typed 'virtual-fundamental stack))

(defun cone-construction (morphism)
  "Normal cone construction"
  (types:make-typed 'cone-construction morphism))

(defun segre-class (cone)
  "Segre class"
  (types:make-typed 'segre-class cone))

;;; Perverse Sheaves ;;;

(defun perverse-sheaf (space perversity)
  "Perverse sheaf"
  (types:make-typed 'perverse-sheaf (pairs:pair space perversity)))

(defun intersection-cohomology (space perversity)
  "Intersection cohomology"
  (types:make-typed 'intersection-cohomology (pairs:pair space perversity)))

(defun decomposition-theorem (morphism)
  "Decomposition theorem"
  (types:make-typed 'decomposition-theorem morphism))

(defun hard-lefschetz (variety)
  "Hard Lefschetz theorem"
  (types:make-typed 'hard-lefschetz variety))

(defun weight-filtration (mixed-sheaf)
  "Weight filtration"
  (types:make-typed 'weight-filtration mixed-sheaf))

(defun purity-theorem (sheaf)
  "Purity theorem"
  (types:make-typed 'purity-theorem sheaf))

(defun nearby-cycles (morphism sheaf)
  "Nearby cycles"
  (types:make-typed 'nearby-cycles (pairs:pair morphism sheaf)))

(defun vanishing-cycles (morphism sheaf)
  "Vanishing cycles"
  (types:make-typed 'vanishing-cycles (pairs:pair morphism sheaf)))

;;; Grothendieck's Vision ;;;

(defun pursuing-stacks ()
  "Pursuing Stacks"
  (types:make-typed 'pursuing-stacks 'manuscript))

(defun homotopy-hypothesis ()
  "Homotopy hypothesis"
  (types:make-typed 'homotopy-hypothesis 'grothendieck))

(defun test-categories (category)
  "Test categories"
  (types:make-typed 'test-categories category))

(defun modelizers (category)
  "Modelizers"
  (types:make-typed 'modelizers category))

(defun derivateurs (category)
  "Derivateurs"
  (types:make-typed 'derivateurs category))

(defun anabelian-section-conjecture ()
  "Section conjecture"
  (types:make-typed 'section-conjecture 'anabelian))

(defun dessins-denfants (graph)
  "Dessins d'enfants"
  (types:make-typed 'dessins-denfants graph))

(defun esquisse-dun-programme ()
  "Esquisse d'un Programme"
  (types:make-typed 'esquisse 'grothendieck-vision))

;;; Categorical Patterns ;;;

(defun kan-extension (functor along direction)
  "Kan extension"
  (types:make-typed 'kan-extension 
                    (pairs:triple functor along direction)))

(defun yoneda-embedding (category)
  "Yoneda embedding"
  (types:make-typed 'yoneda-embedding category))

(defun day-convolution (monoidal bifunctor)
  "Day convolution"
  (types:make-typed 'day-convolution (pairs:pair monoidal bifunctor)))

(defun ends-coends (functor bifunctor)
  "Ends and coends"
  (types:make-typed 'ends-coends (pairs:pair functor bifunctor)))

(defun weighted-limit (diagram weight category)
  "Weighted limit"
  (types:make-typed 'weighted-limit 
                    (pairs:triple diagram weight category)))

(defun indexed-limit (indexing diagram category)
  "Indexed limit"
  (types:make-typed 'indexed-limit 
                    (pairs:triple indexing diagram category)))

(defun monadicity-theorem (functor)
  "Beck's monadicity theorem"
  (types:make-typed 'monadicity functor))

(defun beck-chevalley (square p q f g)
  "Beck-Chevalley condition"
  (types:make-typed 'beck-chevalley 
                    (tuples:tuple-from-list 
                      (lists:cons square (lists:cons p 
                        (lists:cons q (lists:cons f 
                          (lists:cons g (lists:nil)))))))))