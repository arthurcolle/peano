;;;; ∞-Topos Theory and Higher Mathematics
;;;; The absolute pinnacle of mathematical abstraction

(defmodule infinity-topos-theory
  (export
    ;; ∞-Topoi
    (make-infinity-topos 3) (infinity-topos? 1)
    (geometric-morphism 2) (logical-morphism 2)
    (hyperconnected 1) (localic 1)
    (cohesive-topos 1) (differentially-cohesive 1)
    (super-topos 1) (higher-topos 2)
    
    ;; ∞-Sites and Sheaves
    (infinity-site 2) (grothendieck-topology 2)
    (infinity-sheaf 2) (sheafification 1)
    (descent-data 2) (effective-descent 2)
    (hypercover 2) (cech-nerve 2)
    (simplicial-presheaf 2) (model-topos 1)
    
    ;; Higher Stacks
    (infinity-stack 3) (derived-stack 2)
    (geometric-stack 2) (moduli-stack 2)
    (quotient-stack 2) (classifying-stack 1)
    (stack-cohomology 2) (derived-geometry 1)
    
    ;; ∞-Categories
    (infinity-groupoid 1) (kan-complex 1)
    (quasi-category 1) (complete-segal-space 1)
    (segal-category 1) (theta-space 1)
    (complicial-set 1) (marked-simplicial-set 2)
    
    ;; Higher Algebra
    (e-infinity-ring 1) (a-infinity-category 1)
    (dg-category 1) (spectral-category 1)
    (derived-category 1) (triangulated-category 1)
    (stable-infinity-category 1) (symmetric-monoidal 1)
    
    ;; Motivic Homotopy Theory
    (motivic-space 2) (motivic-spectrum 2)
    (a1-homotopy 1) (nisnevich-topology 1)
    (cdh-topology 1) (h-topology 1)
    (algebraic-cobordism 1) (algebraic-k-theory 1)
    
    ;; Chromatic Homotopy Theory
    (chromatic-tower 1) (morava-k-theory 2)
    (morava-e-theory 2) (tmf-spectrum 0)
    (hopkins-miller 0) (lubin-tate 2)
    (gross-hopkins 1) (chromatic-splitting 1)
    
    ;; Derived Algebraic Geometry
    (derived-scheme 1) (derived-stack-dag 1)
    (perfect-complex 1) (tor-amplitude 2)
    (cotangent-complex 1) (derived-intersection 2)
    (virtual-fundamental-class 1)
    
    ;; Higher Topos Logic
    (homotopy-type-theory-topos 1)
    (univalent-fibration 1) (object-classifier 1)
    (higher-modality 1) (lex-modality 1)
    (cohesive-homotopy-type-theory 1)
    
    ;; Spectral Algebraic Geometry
    (spectral-scheme 1) (e-infinity-scheme 1)
    (spectral-deligne-mumford 1)
    (tannaka-duality 1) (galois-theory-spectra 1)
    
    ;; ∞-Operads
    (infinity-operad 1) (dendroidal-set 1)
    (symmetric-sequence 1) (cyclic-operad 1)
    (modular-operad 1) (properadic 1)
    (dioperadic 1) (higher-lawvere-theory 1)
    
    ;; Factorization Algebras
    (factorization-algebra 2) (prefactorization 2)
    (locally-constant 1) (translation-invariant 1)
    (observables 2) (quantum-field-theory 1)
    
    ;; Higher Representation Theory
    (higher-representation 2) (two-representation 2)
    (categorification 1) (decategorification 1)
    (higher-character 2) (higher-trace 2)
    
    ;; Geometric Langlands
    (geometric-langlands 2) (hecke-eigensheaf 2)
    (langlands-dual 1) (geometric-satake 1)
    (fundamental-lemma 0) (arthur-selberg 0)
    
    ;; Perfectoid Spaces
    (perfectoid-space 1) (perfectoid-algebra 1)
    (tilting-equivalence 1) (untilt 1)
    (diamond 1) (v-topology 1)
    (pro-etale 1) (arc-topology 1)
    
    ;; Prismatic Cohomology
    (prism 2) (prismatic-site 1)
    (hodge-tate-stack 1) (breuil-kisin 1)
    (q-de-rham 1) (syntomification 1)
    
    ;; Condensed Mathematics
    (condensed-set 1) (condensed-abelian 1)
    (liquid-module 1) (solid-module 1)
    (analytic-ring 1) (nuclear-space 1)
    
    ;; Higher Symplectic Geometry
    (n-symplectic 2) (lagrangian-correspondence 2)
    (shifted-symplectic 2) (derived-critical-locus 1)
    (bfv-complex 1) (aksz-construction 2)
    
    ;; Homological Mirror Symmetry
    (fukaya-category 1) (mirror-pair 2)
    (syz-correspondence 2) (coherent-sheaf-category 1)
    (stability-condition 1) (bridgeland-space 1)
    
    ;; Higher Gauge Theory
    (higher-gauge-field 2) (higher-connection 2)
    (higher-parallel-transport 2) (higher-holonomy 2)
    (gerbe 2) (n-gerbe 2) (higher-bundle 2)
    
    ;; Noncommutative Geometry
    (spectral-triple 3) (cyclic-cohomology 1)
    (connes-character 1) (noncommutative-space 1)
    (quantum-group-c-star 1) (hopf-c-star 1)
    
    ;; Higher Category Number Theory
    (arithmetic-topos 1) (etale-topos 1)
    (galois-topos 1) (anabelian-geometry 1)
    (section-conjecture 0) (iut-theory 0)))

(include-lib "peano/include/peano.lfe")

;;; ∞-Topoi ;;;

(defun make-infinity-topos (site topology presentability)
  "Create an ∞-topos from site with topology"
  (types:make-typed 'infinity-topos 
                    (pairs:triple site topology presentability)))

(defun infinity-topos? (x)
  "Check if value is ∞-topos"
  (peano:eq (types:type-of x) 'infinity-topos))

(defun geometric-morphism (source target)
  "Geometric morphism between ∞-topoi"
  (types:make-typed 'geometric-morphism (pairs:pair source target)))

(defun logical-morphism (source target)
  "Logical morphism preserving internal logic"
  (types:make-typed 'logical-morphism (pairs:pair source target)))

(defun hyperconnected (topos)
  "Hyperconnected ∞-topos"
  (types:make-typed 'hyperconnected topos))

(defun localic (topos)
  "Localic ∞-topos"
  (types:make-typed 'localic topos))

(defun cohesive-topos (topos)
  "Cohesive ∞-topos with shape/flat/sharp"
  (types:make-typed 'cohesive topos))

(defun differentially-cohesive (topos)
  "Differentially cohesive ∞-topos"
  (types:make-typed 'diff-cohesive topos))

(defun super-topos (topos)
  "Super ∞-topos for supergeometry"
  (types:make-typed 'super topos))

(defun higher-topos (n topos)
  "(∞,n)-topos"
  (types:make-typed 'n-topos (pairs:pair n topos)))

;;; ∞-Sites and Sheaves ;;;

(defun infinity-site (category coverage)
  "∞-site with coverage"
  (types:make-typed 'infinity-site (pairs:pair category coverage)))

(defun grothendieck-topology (site sieves)
  "Grothendieck topology on ∞-site"
  (types:make-typed 'grothendieck (pairs:pair site sieves)))

(defun infinity-sheaf (site presheaf)
  "∞-sheaf on site"
  (types:make-typed 'infinity-sheaf (pairs:pair site presheaf)))

(defun sheafification (presheaf)
  "Sheafification of presheaf"
  (types:make-typed 'sheafification presheaf))

(defun descent-data (morphism data)
  "Descent data for morphism"
  (types:make-typed 'descent (pairs:pair morphism data)))

(defun effective-descent (morphism category)
  "Effective descent morphism"
  (types:make-typed 'effective-descent (pairs:pair morphism category)))

(defun hypercover (object covering)
  "Hypercover of object"
  (types:make-typed 'hypercover (pairs:pair object covering)))

(defun cech-nerve (cover object)
  "Čech nerve of cover"
  (types:make-typed 'cech-nerve (pairs:pair cover object)))

(defun simplicial-presheaf (site functor)
  "Simplicial presheaf"
  (types:make-typed 'simplicial-presheaf (pairs:pair site functor)))

(defun model-topos (model-category)
  "Model topos from model category"
  (types:make-typed 'model-topos model-category))

;;; Higher Stacks ;;;

(defun infinity-stack (site conditions objects)
  "∞-stack on site"
  (types:make-typed 'infinity-stack 
                    (pairs:triple site conditions objects)))

(defun derived-stack (classical derivation)
  "Derived ∞-stack"
  (types:make-typed 'derived-stack (pairs:pair classical derivation)))

(defun geometric-stack (atlas presentation)
  "Geometric ∞-stack"
  (types:make-typed 'geometric-stack (pairs:pair atlas presentation)))

(defun moduli-stack (functor representability)
  "Moduli ∞-stack"
  (types:make-typed 'moduli-stack (pairs:pair functor representability)))

(defun quotient-stack (space action)
  "Quotient ∞-stack [X/G]"
  (types:make-typed 'quotient-stack (pairs:pair space action)))

(defun classifying-stack (group)
  "Classifying ∞-stack BG"
  (types:make-typed 'classifying-stack group))

(defun stack-cohomology (stack coefficients)
  "Cohomology of ∞-stack"
  (types:make-typed 'stack-cohomology (pairs:pair stack coefficients)))

(defun derived-geometry (space)
  "Derived geometry of space"
  (types:make-typed 'derived-geometry space))

;;; ∞-Categories ;;;

(defun infinity-groupoid (space)
  "∞-groupoid (Kan complex)"
  (types:make-typed 'infinity-groupoid space))

(defun kan-complex (simplicial-set)
  "Kan complex structure"
  (types:make-typed 'kan-complex simplicial-set))

(defun quasi-category (simplicial-set)
  "Quasi-category (weak Kan complex)"
  (types:make-typed 'quasi-category simplicial-set))

(defun complete-segal-space (space)
  "Complete Segal space"
  (types:make-typed 'complete-segal space))

(defun segal-category (category)
  "Segal category"
  (types:make-typed 'segal-category category))

(defun theta-space (space)
  "Θ-space (Rezk)"
  (types:make-typed 'theta-space space))

(defun complicial-set (set)
  "Complicial set (Street)"
  (types:make-typed 'complicial-set set))

(defun marked-simplicial-set (set marking)
  "Marked simplicial set"
  (types:make-typed 'marked-simplicial (pairs:pair set marking)))

;;; Higher Algebra ;;;

(defun e-infinity-ring (spectrum)
  "E∞-ring spectrum"
  (types:make-typed 'e-infinity-ring spectrum))

(defun a-infinity-category (category)
  "A∞-category"
  (types:make-typed 'a-infinity-category category))

(defun dg-category (category)
  "Differential graded category"
  (types:make-typed 'dg-category category))

(defun spectral-category (category)
  "Spectral category"
  (types:make-typed 'spectral-category category))

(defun derived-category (abelian)
  "Derived ∞-category"
  (types:make-typed 'derived-category abelian))

(defun triangulated-category (category)
  "Triangulated ∞-category"
  (types:make-typed 'triangulated-category category))

(defun stable-infinity-category (category)
  "Stable ∞-category"
  (types:make-typed 'stable-infinity-category category))

(defun symmetric-monoidal (category)
  "Symmetric monoidal ∞-category"
  (types:make-typed 'symmetric-monoidal category))

;;; Motivic Homotopy Theory ;;;

(defun motivic-space (scheme space)
  "Motivic space over scheme"
  (types:make-typed 'motivic-space (pairs:pair scheme space)))

(defun motivic-spectrum (scheme spectrum)
  "Motivic spectrum over scheme"
  (types:make-typed 'motivic-spectrum (pairs:pair scheme spectrum)))

(defun a1-homotopy (category)
  "A¹-homotopy theory"
  (types:make-typed 'a1-homotopy category))

(defun nisnevich-topology (scheme)
  "Nisnevich topology"
  (types:make-typed 'nisnevich scheme))

(defun cdh-topology (scheme)
  "cdh-topology"
  (types:make-typed 'cdh scheme))

(defun h-topology (scheme)
  "h-topology"
  (types:make-typed 'h-topology scheme))

(defun algebraic-cobordism (scheme)
  "Algebraic cobordism"
  (types:make-typed 'algebraic-cobordism scheme))

(defun algebraic-k-theory (scheme)
  "Algebraic K-theory spectrum"
  (types:make-typed 'algebraic-k-theory scheme))

;;; Chromatic Homotopy Theory ;;;

(defun chromatic-tower (prime)
  "Chromatic tower at prime p"
  (types:make-typed 'chromatic-tower prime))

(defun morava-k-theory (n prime)
  "Morava K-theory K(n)"
  (types:make-typed 'morava-k (pairs:pair n prime)))

(defun morava-e-theory (n prime)
  "Morava E-theory E_n"
  (types:make-typed 'morava-e (pairs:pair n prime)))

(defun tmf-spectrum ()
  "Topological modular forms"
  (types:make-typed 'tmf 'spectrum))

(defun hopkins-miller ()
  "Hopkins-Miller theorem"
  (types:make-typed 'hopkins-miller 'theorem))

(defun lubin-tate (n prime)
  "Lubin-Tate theory"
  (types:make-typed 'lubin-tate (pairs:pair n prime)))

(defun gross-hopkins (prime)
  "Gross-Hopkins duality"
  (types:make-typed 'gross-hopkins prime))

(defun chromatic-splitting (spectrum)
  "Chromatic splitting"
  (types:make-typed 'chromatic-splitting spectrum))

;;; Derived Algebraic Geometry ;;;

(defun derived-scheme (classical)
  "Derived scheme"
  (types:make-typed 'derived-scheme classical))

(defun derived-stack-dag (stack)
  "Derived stack in DAG"
  (types:make-typed 'derived-stack-dag stack))

(defun perfect-complex (scheme)
  "Perfect complex on scheme"
  (types:make-typed 'perfect-complex scheme))

(defun tor-amplitude (complex bounds)
  "Tor-amplitude"
  (types:make-typed 'tor-amplitude (pairs:pair complex bounds)))

(defun cotangent-complex (morphism)
  "Cotangent complex"
  (types:make-typed 'cotangent-complex morphism))

(defun derived-intersection (x y)
  "Derived intersection"
  (types:make-typed 'derived-intersection (pairs:pair x y)))

(defun virtual-fundamental-class (stack)
  "Virtual fundamental class"
  (types:make-typed 'virtual-fundamental stack))

;;; Higher Topos Logic ;;;

(defun homotopy-type-theory-topos (topos)
  "HoTT internal to ∞-topos"
  (types:make-typed 'hott-topos topos))

(defun univalent-fibration (fibration)
  "Univalent fibration"
  (types:make-typed 'univalent-fibration fibration))

(defun object-classifier (topos)
  "Object classifier in ∞-topos"
  (types:make-typed 'object-classifier topos))

(defun higher-modality (modality)
  "Higher modality"
  (types:make-typed 'higher-modality modality))

(defun lex-modality (modality)
  "Left exact modality"
  (types:make-typed 'lex-modality modality))

(defun cohesive-homotopy-type-theory (theory)
  "Cohesive HoTT"
  (types:make-typed 'cohesive-hott theory))

;;; Spectral Algebraic Geometry ;;;

(defun spectral-scheme (scheme)
  "Spectral scheme"
  (types:make-typed 'spectral-scheme scheme))

(defun e-infinity-scheme (scheme)
  "E∞-scheme"
  (types:make-typed 'e-infinity-scheme scheme))

(defun spectral-deligne-mumford (stack)
  "Spectral Deligne-Mumford stack"
  (types:make-typed 'spectral-dm stack))

(defun tannaka-duality (category)
  "Tannaka duality for spectra"
  (types:make-typed 'tannaka-duality category))

(defun galois-theory-spectra (extension)
  "Galois theory for spectra"
  (types:make-typed 'galois-spectra extension))

;;; ∞-Operads ;;;

(defun infinity-operad (operad)
  "∞-operad"
  (types:make-typed 'infinity-operad operad))

(defun dendroidal-set (set)
  "Dendroidal set"
  (types:make-typed 'dendroidal-set set))

(defun symmetric-sequence (sequence)
  "Symmetric sequence"
  (types:make-typed 'symmetric-sequence sequence))

(defun cyclic-operad (operad)
  "Cyclic ∞-operad"
  (types:make-typed 'cyclic-operad operad))

(defun modular-operad (operad)
  "Modular ∞-operad"
  (types:make-typed 'modular-operad operad))

(defun properadic (properadic)
  "∞-properadic"
  (types:make-typed 'properadic properadic))

(defun dioperadic (dioperadic)
  "∞-dioperadic"
  (types:make-typed 'dioperadic dioperadic))

(defun higher-lawvere-theory (theory)
  "Higher Lawvere theory"
  (types:make-typed 'higher-lawvere theory))

;;; Factorization Algebras ;;;

(defun factorization-algebra (manifold values)
  "Factorization algebra"
  (types:make-typed 'factorization-algebra (pairs:pair manifold values)))

(defun prefactorization (manifold values)
  "Prefactorization algebra"
  (types:make-typed 'prefactorization (pairs:pair manifold values)))

(defun locally-constant (algebra)
  "Locally constant factorization algebra"
  (types:make-typed 'locally-constant algebra))

(defun translation-invariant (algebra)
  "Translation invariant"
  (types:make-typed 'translation-invariant algebra))

(defun observables (theory space)
  "Observables in QFT"
  (types:make-typed 'observables (pairs:pair theory space)))

(defun quantum-field-theory (data)
  "Quantum field theory"
  (types:make-typed 'qft data))

;;; Higher Representation Theory ;;;

(defun higher-representation (group category)
  "Higher representation"
  (types:make-typed 'higher-rep (pairs:pair group category)))

(defun two-representation (two-group category)
  "2-representation"
  (types:make-typed 'two-rep (pairs:pair two-group category)))

(defun categorification (structure)
  "Categorification"
  (types:make-typed 'categorification structure))

(defun decategorification (structure)
  "Decategorification"
  (types:make-typed 'decategorification structure))

(defun higher-character (representation trace)
  "Higher character"
  (types:make-typed 'higher-character (pairs:pair representation trace)))

(defun higher-trace (endomorphism category)
  "Higher trace"
  (types:make-typed 'higher-trace (pairs:pair endomorphism category)))

;;; Geometric Langlands ;;;

(defun geometric-langlands (curve group)
  "Geometric Langlands correspondence"
  (types:make-typed 'geometric-langlands (pairs:pair curve group)))

(defun hecke-eigensheaf (sheaf eigenvalue)
  "Hecke eigensheaf"
  (types:make-typed 'hecke-eigensheaf (pairs:pair sheaf eigenvalue)))

(defun langlands-dual (group)
  "Langlands dual group"
  (types:make-typed 'langlands-dual group))

(defun geometric-satake (group)
  "Geometric Satake"
  (types:make-typed 'geometric-satake group))

(defun fundamental-lemma ()
  "Fundamental lemma"
  (types:make-typed 'fundamental-lemma 'proven))

(defun arthur-selberg ()
  "Arthur-Selberg trace formula"
  (types:make-typed 'arthur-selberg 'formula))

;;; Perfectoid Spaces ;;;

(defun perfectoid-space (space)
  "Perfectoid space"
  (types:make-typed 'perfectoid-space space))

(defun perfectoid-algebra (algebra)
  "Perfectoid algebra"
  (types:make-typed 'perfectoid-algebra algebra))

(defun tilting-equivalence (characteristic)
  "Tilting equivalence"
  (types:make-typed 'tilting characteristic))

(defun untilt (space)
  "Untilt perfectoid space"
  (types:make-typed 'untilt space))

(defun diamond (space)
  "Diamond (perfectoid space modulo Frobenius)"
  (types:make-typed 'diamond space))

(defun v-topology (space)
  "v-topology"
  (types:make-typed 'v-topology space))

(defun pro-etale (scheme)
  "Pro-étale site"
  (types:make-typed 'pro-etale scheme))

(defun arc-topology (scheme)
  "Arc-topology"
  (types:make-typed 'arc-topology scheme))

;;; Prismatic Cohomology ;;;

(defun prism (ring ideal)
  "Prism (A,I)"
  (types:make-typed 'prism (pairs:pair ring ideal)))

(defun prismatic-site (scheme)
  "Prismatic site"
  (types:make-typed 'prismatic-site scheme))

(defun hodge-tate-stack (prime)
  "Hodge-Tate stack"
  (types:make-typed 'hodge-tate-stack prime))

(defun breuil-kisin (module)
  "Breuil-Kisin module"
  (types:make-typed 'breuil-kisin module))

(defun q-de-rham (complex)
  "q-de Rham complex"
  (types:make-typed 'q-de-rham complex))

(defun syntomification (complex)
  "Syntomification"
  (types:make-typed 'syntomification complex))

;;; Condensed Mathematics ;;;

(defun condensed-set (set)
  "Condensed set"
  (types:make-typed 'condensed-set set))

(defun condensed-abelian (group)
  "Condensed abelian group"
  (types:make-typed 'condensed-abelian group))

(defun liquid-module (module)
  "Liquid module"
  (types:make-typed 'liquid-module module))

(defun solid-module (module)
  "Solid module"
  (types:make-typed 'solid-module module))

(defun analytic-ring (ring)
  "Analytic ring"
  (types:make-typed 'analytic-ring ring))

(defun nuclear-space (space)
  "Nuclear space"
  (types:make-typed 'nuclear-space space))

;;; Higher Symplectic Geometry ;;;

(defun n-symplectic (n manifold)
  "n-symplectic structure"
  (types:make-typed 'n-symplectic (pairs:pair n manifold)))

(defun lagrangian-correspondence (l1 l2)
  "Lagrangian correspondence"
  (types:make-typed 'lagrangian-corr (pairs:pair l1 l2)))

(defun shifted-symplectic (shift structure)
  "Shifted symplectic structure"
  (types:make-typed 'shifted-symplectic (pairs:pair shift structure)))

(defun derived-critical-locus (function)
  "Derived critical locus"
  (types:make-typed 'derived-critical function))

(defun bfv-complex (manifold)
  "BFV complex"
  (types:make-typed 'bfv-complex manifold))

(defun aksz-construction (source target)
  "AKSZ construction"
  (types:make-typed 'aksz (pairs:pair source target)))

;;; Homological Mirror Symmetry ;;;

(defun fukaya-category (symplectic)
  "Fukaya ∞-category"
  (types:make-typed 'fukaya symplectic))

(defun mirror-pair (a-side b-side)
  "Mirror pair"
  (types:make-typed 'mirror-pair (pairs:pair a-side b-side)))

(defun syz-correspondence (a-side b-side)
  "Strominger-Yau-Zaslow correspondence"
  (types:make-typed 'syz (pairs:pair a-side b-side)))

(defun coherent-sheaf-category (variety)
  "Derived category of coherent sheaves"
  (types:make-typed 'coh-sheaf-cat variety))

(defun stability-condition (category)
  "Bridgeland stability condition"
  (types:make-typed 'stability category))

(defun bridgeland-space (category)
  "Space of stability conditions"
  (types:make-typed 'bridgeland-space category))

;;; Higher Gauge Theory ;;;

(defun higher-gauge-field (n bundle)
  "n-gauge field"
  (types:make-typed 'n-gauge-field (pairs:pair n bundle)))

(defun higher-connection (n connection)
  "n-connection"
  (types:make-typed 'n-connection (pairs:pair n connection)))

(defun higher-parallel-transport (n path)
  "n-parallel transport"
  (types:make-typed 'n-parallel-transport (pairs:pair n path)))

(defun higher-holonomy (n loop)
  "n-holonomy"
  (types:make-typed 'n-holonomy (pairs:pair n loop)))

(defun gerbe (space band)
  "Gerbe with band"
  (types:make-typed 'gerbe (pairs:pair space band)))

(defun n-gerbe (n space)
  "n-gerbe"
  (types:make-typed 'n-gerbe (pairs:pair n space)))

(defun higher-bundle (n structure)
  "Higher bundle"
  (types:make-typed 'higher-bundle (pairs:pair n structure)))

;;; Noncommutative Geometry ;;;

(defun spectral-triple (algebra hilbert dirac)
  "Spectral triple (A,H,D)"
  (types:make-typed 'spectral-triple 
                    (pairs:triple algebra hilbert dirac)))

(defun cyclic-cohomology (algebra)
  "Cyclic cohomology"
  (types:make-typed 'cyclic-cohomology algebra))

(defun connes-character (k-theory)
  "Connes-Chern character"
  (types:make-typed 'connes-character k-theory))

(defun noncommutative-space (algebra)
  "Noncommutative space"
  (types:make-typed 'nc-space algebra))

(defun quantum-group-c-star (group)
  "Quantum group C*-algebra"
  (types:make-typed 'quantum-group-c-star group))

(defun hopf-c-star (algebra)
  "Hopf C*-algebra"
  (types:make-typed 'hopf-c-star algebra))

;;; Higher Category Number Theory ;;;

(defun arithmetic-topos (prime)
  "Arithmetic ∞-topos"
  (types:make-typed 'arithmetic-topos prime))

(defun etale-topos (scheme)
  "Étale ∞-topos"
  (types:make-typed 'etale-topos scheme))

(defun galois-topos (field)
  "Galois ∞-topos"
  (types:make-typed 'galois-topos field))

(defun anabelian-geometry (curve)
  "Anabelian geometry"
  (types:make-typed 'anabelian curve))

(defun section-conjecture ()
  "Section conjecture"
  (types:make-typed 'section-conjecture 'open))

(defun iut-theory ()
  "Inter-universal Teichmüller theory"
  (types:make-typed 'iut 'mochizuki))