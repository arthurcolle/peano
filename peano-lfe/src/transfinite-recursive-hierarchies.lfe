;;;; Transfinite Recursive Hierarchies and Ultimate Ordinals
;;;; The absolute limits of mathematical construction

(defmodule transfinite-recursive-hierarchies
  (export
    ;; Ordinal Hierarchies
    (church-kleene-ordinal 0) (recursive-ordinals 0)
    (admissible-ordinals 1) (recursively-inaccessible 1)
    (recursively-mahlo 1) (pi11-reflecting 1)
    (stable-ordinals 1) (nonprojectible 1)
    
    ;; Large Countable Ordinals
    (first-uncountable 0) (first-inaccessible 0)
    (first-mahlo 0) (first-weakly-compact 0)
    (first-indescribable 0) (first-measurable 0)
    (first-supercompact 0) (first-huge 0)
    
    ;; Ordinal Notation Systems
    (cantor-normal-form 1) (veblen-hierarchy 2)
    (bachmann-howard-ordinal-system 0) (buchholz-psi 1)
    (rathjen-psi 1) (stegert-psi 1)
    (madore-psi 1) (bridge-notation 2)
    
    ;; Proof Theoretic Ordinals
    (peano-ordinal 0) (predicative-ordinal 0)
    (feferman-schutte 0) (ackermann-ordinal 0)
    (small-veblen 0) (large-veblen 0)
    (bachmann-howard 0) (takeuti-feferman-buchholz 0)
    
    ;; Recursive Analogues
    (recursive-well-ordering 1) (kleene-brouwer 1)
    (recursive-pseudo-well-ordering 1)
    (hyperarithmetic-hierarchy-ordinal 1)
    (delta12-ordinal 0) (sigma11-bounding 0)
    
    ;; Transfinite Induction Hierarchies
    (epsilon-induction 1) (gamma-induction 1)
    (veblen-induction 2) (psi-induction 1)
    (ordinal-collapsing-induction 2)
    (mahlo-induction 1) (weakly-compact-induction 1)
    
    ;; Hierarchies of Functions
    (fast-growing-hierarchy 2) (hardy-hierarchy 2)
    (slow-growing-hierarchy 2) (middle-growing-hierarchy 2)
    (buchholz-hierarchy 2) (lob-wainer-hierarchy 2)
    (girard-hierarchy 2) (veblen-hierarchy-functions 2)
    
    ;; Ordinal Analysis
    (ordinal-analysis-system 1) (proof-theoretic-strength 1)
    (ordinal-representation 2) (fundamental-sequences 2)
    (bachmann-property 1) (derivatives-ordinals 1)
    (hardy-derivatives 1) (veblen-derivatives 1)
    
    ;; Beyond Gamma_0
    (feferman-schutte-ordinal 0) (ackermann-ordinal-proper 0)
    (small-veblen-ordinal 0) (large-veblen-ordinal 0)
    (bachmann-howard-ordinal-proper 0) (takeuti-ordinal-analysis 0)
    (proof-theoretic-ordinal-id 0) (stability-strength 1)
    
    ;; Extended Systems
    (kripke-platek-ordinal 0) (power-kripke-platek 0)
    (iterated-kripke-platek 1) (admissible-recursion 1)
    (beta-recursion 2) (sigma1-recursion 1)
    (constructive-ordinal 1) (bishop-ordinal 1)
    
    ;; Reflection Principles
    (mahlo-universe-ordinal 0) (pi-n-reflection 2)
    (total-reflection 1) (subtle-cardinal-ordinal 1)
    (ineffable-ordinal 1) (indescribable-ordinals 2)
    (completely-ineffable-ordinal 1)
    
    ;; Computational Ordinals
    (busy-beaver-ordinal 1) (kolmogorov-ordinal 1)
    (solomonoff-ordinal 1) (chaitin-ordinal 1)
    (oracle-ordinal 2) (hypercomputation-ordinal 1)
    (supertask-ordinal 1) (accelerating-turing-ordinal 1)
    
    ;; Set Theoretic Ordinals
    (hartogs-number 1) (aleph-hierarchy 1)
    (beth-hierarchy 1) (gimel-hierarchy 1)
    (regular-cardinals 1) (singular-cardinals 1)
    (fixed-points-aleph 0) (fixed-points-beth 0)
    
    ;; Descriptive Set Theory
    (borel-hierarchy-ordinal 1) (projective-ordinals 1)
    (wadge-ordinal 1) (steel-ordinal 1)
    (martin-ordinal 1) (moschovakis-ordinal 1)
    (effective-ordinals 1) (lightface-ordinals 1)
    
    ;; Higher Recursion
    (e-recursion-ordinal 1) (alpha-recursion-ordinal 2)
    (beta-recursion-ordinal 2) (moschovakis-witness 1)
    (spector-classes 1) (gandy-ordinal 0)
    (jensen-ordinal 1) (aanderaa-ordinal 1)
    
    ;; Ultimate Constructions
    (absolute-ordinal 0) (inconstructible-ordinal 0)
    (proper-class-ordinal 0) (ur-ordinal 0)
    (transcendent-ordinal 0) (ineffable-limit 0)
    (beyond-ordinals 0) (non-ordinal-well-order 0)
    
    ;; Ordinal Arithmetic
    (natural-sum 2) (natural-product 2)
    (natural-exponentiation 2) (epsilon-numbers 1)
    (gamma-numbers 1) (veblen-fixed-points 2)
    (admissible-collapse 1) (mahlo-collapse 1)
    
    ;; Transfinite Types
    (ordinal-indexed-type 2) (cumulative-hierarchy-type 1)
    (hereditarily-finite-ordinal 1) (hereditarily-countable-ordinal 1)
    (constructibility-degrees-ordinal 1) (minimal-model-ordinal 1)
    
    ;; Special Ordinals
    (critical-ordinals 1) (additively-indecomposable 1)
    (multiplicatively-indecomposable 1) (epsilon-critical 1)
    (strongly-critical 1) (reflecting-ordinals 1)
    (stationary-ordinals 1) (club-ordinals 1)))

(include-lib "peano/include/peano.lfe")

;;; Ordinal Hierarchies ;;;

(defun church-kleene-ordinal ()
  "Church-Kleene ordinal ω_1^CK"
  (types:make-typed 'church-kleene 'omega-1-ck))

(defun recursive-ordinals ()
  "Recursive ordinals"
  (types:make-typed 'recursive-ordinals 'computable))

(defun admissible-ordinals (alpha)
  "Admissible ordinals"
  (types:make-typed 'admissible-ordinals alpha))

(defun recursively-inaccessible (alpha)
  "Recursively inaccessible"
  (types:make-typed 'recursively-inaccessible alpha))

(defun recursively-mahlo (alpha)
  "Recursively Mahlo"
  (types:make-typed 'recursively-mahlo alpha))

(defun pi11-reflecting (alpha)
  "Π¹₁-reflecting ordinal"
  (types:make-typed 'pi11-reflecting alpha))

(defun stable-ordinals (alpha)
  "Stable ordinals"
  (types:make-typed 'stable-ordinals alpha))

(defun nonprojectible (alpha)
  "Nonprojectible ordinal"
  (types:make-typed 'nonprojectible alpha))

;;; Large Countable Ordinals ;;;

(defun first-uncountable ()
  "First uncountable ordinal ω₁"
  (types:make-typed 'first-uncountable 'omega-1))

(defun first-inaccessible ()
  "First inaccessible cardinal"
  (types:make-typed 'first-inaccessible 'kappa))

(defun first-mahlo ()
  "First Mahlo cardinal"
  (types:make-typed 'first-mahlo 'M))

(defun first-weakly-compact ()
  "First weakly compact"
  (types:make-typed 'first-weakly-compact 'K))

(defun first-indescribable ()
  "First indescribable"
  (types:make-typed 'first-indescribable 'Pi-1-1))

(defun first-measurable ()
  "First measurable cardinal"
  (types:make-typed 'first-measurable 'mu))

(defun first-supercompact ()
  "First supercompact"
  (types:make-typed 'first-supercompact 'kappa))

(defun first-huge ()
  "First huge cardinal"
  (types:make-typed 'first-huge 'huge))

;;; Ordinal Notation Systems ;;;

(defun cantor-normal-form (ordinal)
  "Cantor normal form"
  (types:make-typed 'cantor-normal-form ordinal))

(defun veblen-hierarchy (base level)
  "Veblen hierarchy φ_β(α)"
  (types:make-typed 'veblen-hierarchy (pairs:pair base level)))

(defun bachmann-howard-ordinal-system ()
  "Bachmann-Howard ordinal system"
  (types:make-typed 'bachmann-howard-system 'psi))

(defun buchholz-psi (ordinal)
  "Buchholz's ψ function"
  (types:make-typed 'buchholz-psi ordinal))

(defun rathjen-psi (ordinal)
  "Rathjen's ψ function"
  (types:make-typed 'rathjen-psi ordinal))

(defun stegert-psi (ordinal)
  "Stegert's ψ function"
  (types:make-typed 'stegert-psi ordinal))

(defun madore-psi (ordinal)
  "Madore's ψ function"
  (types:make-typed 'madore-psi ordinal))

(defun bridge-notation (system1 system2)
  "Bridge between notation systems"
  (types:make-typed 'bridge-notation (pairs:pair system1 system2)))

;;; Proof Theoretic Ordinals ;;;

(defun peano-ordinal ()
  "Proof theoretic ordinal of PA"
  (types:make-typed 'peano-ordinal 'epsilon-0))

(defun predicative-ordinal ()
  "Predicative ordinal Γ₀"
  (types:make-typed 'predicative-ordinal 'gamma-0))

(defun feferman-schutte ()
  "Feferman-Schütte ordinal Γ₀"
  (types:make-typed 'feferman-schutte 'gamma-0))

(defun ackermann-ordinal ()
  "Ackermann ordinal"
  (types:make-typed 'ackermann-ordinal 'phi-omega))

(defun small-veblen ()
  "Small Veblen ordinal"
  (types:make-typed 'small-veblen 'sv))

(defun large-veblen ()
  "Large Veblen ordinal"
  (types:make-typed 'large-veblen 'lv))

(defun bachmann-howard ()
  "Bachmann-Howard ordinal"
  (types:make-typed 'bachmann-howard 'psi-omega-omega))

(defun takeuti-feferman-buchholz ()
  "Takeuti-Feferman-Buchholz ordinal"
  (types:make-typed 'tfb-ordinal 'psi-omega-omega-omega))

;;; Recursive Analogues ;;;

(defun recursive-well-ordering (notation)
  "Recursive well-ordering"
  (types:make-typed 'recursive-well-ordering notation))

(defun kleene-brouwer (ordering)
  "Kleene-Brouwer ordering"
  (types:make-typed 'kleene-brouwer ordering))

(defun recursive-pseudo-well-ordering (ordering)
  "Recursive pseudo-well-ordering"
  (types:make-typed 'recursive-pseudo-well-ordering ordering))

(defun hyperarithmetic-hierarchy-ordinal (level)
  "Hyperarithmetic hierarchy ordinal"
  (types:make-typed 'hyperarithmetic-ordinal level))

(defun delta12-ordinal ()
  "Δ¹₂ ordinal"
  (types:make-typed 'delta12-ordinal 'beta-0))

(defun sigma11-bounding ()
  "Σ¹₁ bounding ordinal"
  (types:make-typed 'sigma11-bounding 'beta-0))

;;; Transfinite Induction Hierarchies ;;;

(defun epsilon-induction (ordinal)
  "ε-induction"
  (types:make-typed 'epsilon-induction ordinal))

(defun gamma-induction (ordinal)
  "Γ-induction"
  (types:make-typed 'gamma-induction ordinal))

(defun veblen-induction (level ordinal)
  "Veblen hierarchy induction"
  (types:make-typed 'veblen-induction (pairs:pair level ordinal)))

(defun psi-induction (ordinal)
  "ψ-induction"
  (types:make-typed 'psi-induction ordinal))

(defun ordinal-collapsing-induction (function ordinal)
  "Ordinal collapsing function induction"
  (types:make-typed 'collapsing-induction (pairs:pair function ordinal)))

(defun mahlo-induction (ordinal)
  "Mahlo ordinal induction"
  (types:make-typed 'mahlo-induction ordinal))

(defun weakly-compact-induction (ordinal)
  "Weakly compact induction"
  (types:make-typed 'weakly-compact-induction ordinal))

;;; Hierarchies of Functions ;;;

(defun fast-growing-hierarchy (level n)
  "Fast-growing hierarchy f_α(n)"
  (types:make-typed 'fast-growing (pairs:pair level n)))

(defun hardy-hierarchy (level n)
  "Hardy hierarchy H_α(n)"
  (types:make-typed 'hardy (pairs:pair level n)))

(defun slow-growing-hierarchy (level n)
  "Slow-growing hierarchy g_α(n)"
  (types:make-typed 'slow-growing (pairs:pair level n)))

(defun middle-growing-hierarchy (level n)
  "Middle-growing hierarchy m_α(n)"
  (types:make-typed 'middle-growing (pairs:pair level n)))

(defun buchholz-hierarchy (level n)
  "Buchholz hierarchy B_α(n)"
  (types:make-typed 'buchholz-hierarchy (pairs:pair level n)))

(defun lob-wainer-hierarchy (level n)
  "Löb-Wainer hierarchy"
  (types:make-typed 'lob-wainer (pairs:pair level n)))

(defun girard-hierarchy (level n)
  "Girard hierarchy"
  (types:make-typed 'girard (pairs:pair level n)))

(defun veblen-hierarchy-functions (level n)
  "Veblen hierarchy functions"
  (types:make-typed 'veblen-functions (pairs:pair level n)))

;;; Ordinal Analysis ;;;

(defun ordinal-analysis-system (theory)
  "Ordinal analysis of theory"
  (types:make-typed 'ordinal-analysis theory))

(defun proof-theoretic-strength (theory)
  "Proof theoretic strength"
  (types:make-typed 'proof-theoretic-strength theory))

(defun ordinal-representation (ordinal system)
  "Ordinal representation in system"
  (types:make-typed 'ordinal-representation (pairs:pair ordinal system)))

(defun fundamental-sequences (ordinal limit)
  "Fundamental sequences"
  (types:make-typed 'fundamental-sequences (pairs:pair ordinal limit)))

(defun bachmann-property (system)
  "Bachmann property"
  (types:make-typed 'bachmann-property system))

(defun derivatives-ordinals (ordinal)
  "Derivatives of ordinals"
  (types:make-typed 'derivatives-ordinals ordinal))

(defun hardy-derivatives (ordinal)
  "Hardy derivatives"
  (types:make-typed 'hardy-derivatives ordinal))

(defun veblen-derivatives (ordinal)
  "Veblen derivatives"
  (types:make-typed 'veblen-derivatives ordinal))

;;; Beyond Gamma_0 ;;;

(defun feferman-schutte-ordinal ()
  "Feferman-Schütte ordinal Γ₀"
  (types:make-typed 'fs-ordinal 'predicativity))

(defun ackermann-ordinal-proper ()
  "Ackermann ordinal proper"
  (types:make-typed 'ackermann-proper 'phi-1-omega))

(defun small-veblen-ordinal ()
  "Small Veblen ordinal"
  (types:make-typed 'small-veblen-ordinal 'theta-omega))

(defun large-veblen-ordinal ()
  "Large Veblen ordinal"
  (types:make-typed 'large-veblen-ordinal 'theta-omega-omega))

(defun bachmann-howard-ordinal-proper ()
  "Bachmann-Howard ordinal"
  (types:make-typed 'bh-ordinal 'psi-epsilon-omega+1))

(defun takeuti-ordinal-analysis ()
  "Takeuti's ordinal analysis"
  (types:make-typed 'takeuti-ordinal 'pi11-ca))

(defun proof-theoretic-ordinal-id ()
  "Proof theoretic ordinal of ID"
  (types:make-typed 'id-ordinal 'psi-omega))

(defun stability-strength (theory)
  "Stability strength"
  (types:make-typed 'stability-strength theory))

;;; Extended Systems ;;;

(defun kripke-platek-ordinal ()
  "Kripke-Platek ordinal"
  (types:make-typed 'kp-ordinal 'omega-ck))

(defun power-kripke-platek ()
  "Power Kripke-Platek"
  (types:make-typed 'power-kp 'admissible))

(defun iterated-kripke-platek (n)
  "Iterated Kripke-Platek"
  (types:make-typed 'iterated-kp n))

(defun admissible-recursion (ordinal)
  "Admissible recursion"
  (types:make-typed 'admissible-recursion ordinal))

(defun beta-recursion (beta set)
  "β-recursion"
  (types:make-typed 'beta-recursion (pairs:pair beta set)))

(defun sigma1-recursion (ordinal)
  "Σ₁-recursion"
  (types:make-typed 'sigma1-recursion ordinal))

(defun constructive-ordinal (system)
  "Constructive ordinal"
  (types:make-typed 'constructive-ordinal system))

(defun bishop-ordinal (constructive)
  "Bishop-style ordinal"
  (types:make-typed 'bishop-ordinal constructive))

;;; Reflection Principles ;;;

(defun mahlo-universe-ordinal ()
  "Mahlo universe ordinal"
  (types:make-typed 'mahlo-universe 'M))

(defun pi-n-reflection (n ordinal)
  "Πⁿ reflection"
  (types:make-typed 'pi-n-reflection (pairs:pair n ordinal)))

(defun total-reflection (ordinal)
  "Total reflection"
  (types:make-typed 'total-reflection ordinal))

(defun subtle-cardinal-ordinal (ordinal)
  "Subtle cardinal ordinal"
  (types:make-typed 'subtle-ordinal ordinal))

(defun ineffable-ordinal (ordinal)
  "Ineffable ordinal"
  (types:make-typed 'ineffable-ordinal ordinal))

(defun indescribable-ordinals (level ordinal)
  "Indescribable ordinals"
  (types:make-typed 'indescribable-ordinals (pairs:pair level ordinal)))

(defun completely-ineffable-ordinal (ordinal)
  "Completely ineffable"
  (types:make-typed 'completely-ineffable ordinal))

;;; Computational Ordinals ;;;

(defun busy-beaver-ordinal (n)
  "Busy beaver ordinal"
  (types:make-typed 'busy-beaver-ordinal n))

(defun kolmogorov-ordinal (string)
  "Kolmogorov complexity ordinal"
  (types:make-typed 'kolmogorov-ordinal string))

(defun solomonoff-ordinal (data)
  "Solomonoff induction ordinal"
  (types:make-typed 'solomonoff-ordinal data))

(defun chaitin-ordinal (omega)
  "Chaitin's Ω ordinal"
  (types:make-typed 'chaitin-ordinal omega))

(defun oracle-ordinal (oracle level)
  "Oracle computation ordinal"
  (types:make-typed 'oracle-ordinal (pairs:pair oracle level)))

(defun hypercomputation-ordinal (model)
  "Hypercomputation ordinal"
  (types:make-typed 'hypercomputation-ordinal model))

(defun supertask-ordinal (task)
  "Supertask ordinal"
  (types:make-typed 'supertask-ordinal task))

(defun accelerating-turing-ordinal (machine)
  "Accelerating Turing machine ordinal"
  (types:make-typed 'accelerating-turing machine))

;;; Set Theoretic Ordinals ;;;

(defun hartogs-number (set)
  "Hartogs number"
  (types:make-typed 'hartogs set))

(defun aleph-hierarchy (index)
  "Aleph hierarchy ℵ_α"
  (types:make-typed 'aleph index))

(defun beth-hierarchy (index)
  "Beth hierarchy ℶ_α"
  (types:make-typed 'beth index))

(defun gimel-hierarchy (kappa)
  "Gimel function ℷ(κ)"
  (types:make-typed 'gimel kappa))

(defun regular-cardinals (kappa)
  "Regular cardinals"
  (types:make-typed 'regular kappa))

(defun singular-cardinals (kappa)
  "Singular cardinals"
  (types:make-typed 'singular kappa))

(defun fixed-points-aleph ()
  "Fixed points of aleph"
  (types:make-typed 'fixed-aleph 'kappa))

(defun fixed-points-beth ()
  "Fixed points of beth"
  (types:make-typed 'fixed-beth 'kappa))

;;; Descriptive Set Theory ;;;

(defun borel-hierarchy-ordinal (level)
  "Borel hierarchy ordinal"
  (types:make-typed 'borel-hierarchy level))

(defun projective-ordinals (level)
  "Projective hierarchy ordinals"
  (types:make-typed 'projective-ordinals level))

(defun wadge-ordinal (set)
  "Wadge ordinal"
  (types:make-typed 'wadge-ordinal set))

(defun steel-ordinal (determinacy)
  "Steel's ordinal analysis"
  (types:make-typed 'steel-ordinal determinacy))

(defun martin-ordinal (measure)
  "Martin's ordinal"
  (types:make-typed 'martin-ordinal measure))

(defun moschovakis-ordinal (pointclass)
  "Moschovakis ordinal"
  (types:make-typed 'moschovakis-ordinal pointclass))

(defun effective-ordinals (hierarchy)
  "Effective ordinals"
  (types:make-typed 'effective-ordinals hierarchy))

(defun lightface-ordinals (hierarchy)
  "Lightface ordinals"
  (types:make-typed 'lightface-ordinals hierarchy))

;;; Higher Recursion ;;;

(defun e-recursion-ordinal (e)
  "E-recursion ordinal"
  (types:make-typed 'e-recursion-ordinal e))

(defun alpha-recursion-ordinal (alpha inadmissible)
  "α-recursion ordinal"
  (types:make-typed 'alpha-recursion-ordinal (pairs:pair alpha inadmissible)))

(defun beta-recursion-ordinal (beta model)
  "β-recursion ordinal"
  (types:make-typed 'beta-recursion-ordinal (pairs:pair beta model)))

(defun moschovakis-witness (pointclass)
  "Moschovakis witness"
  (types:make-typed 'moschovakis-witness pointclass))

(defun spector-classes (level)
  "Spector classes"
  (types:make-typed 'spector-classes level))

(defun gandy-ordinal ()
  "Gandy's ordinal"
  (types:make-typed 'gandy-ordinal 'sigma-1-1))

(defun jensen-ordinal (model)
  "Jensen's ordinal"
  (types:make-typed 'jensen-ordinal model))

(defun aanderaa-ordinal (priority)
  "Aanderaa's ordinal"
  (types:make-typed 'aanderaa-ordinal priority))

;;; Ultimate Constructions ;;;

(defun absolute-ordinal ()
  "Absolute ordinal"
  (types:make-typed 'absolute-ordinal 'ORD))

(defun inconstructible-ordinal ()
  "Inconstructible ordinal"
  (types:make-typed 'inconstructible-ordinal '0-sharp))

(defun proper-class-ordinal ()
  "Proper class ordinal"
  (types:make-typed 'proper-class-ordinal 'ON))

(defun ur-ordinal ()
  "Ur-ordinal"
  (types:make-typed 'ur-ordinal 'primordial))

(defun transcendent-ordinal ()
  "Transcendent ordinal"
  (types:make-typed 'transcendent-ordinal 'beyond))

(defun ineffable-limit ()
  "Ineffable limit"
  (types:make-typed 'ineffable-limit 'indescribable))

(defun beyond-ordinals ()
  "Beyond ordinals"
  (types:make-typed 'beyond-ordinals 'surreal))

(defun non-ordinal-well-order ()
  "Non-ordinal well-order"
  (types:make-typed 'non-ordinal-well-order 'exotic))

;;; Ordinal Arithmetic ;;;

(defun natural-sum (alpha beta)
  "Natural sum of ordinals"
  (types:make-typed 'natural-sum (pairs:pair alpha beta)))

(defun natural-product (alpha beta)
  "Natural product of ordinals"
  (types:make-typed 'natural-product (pairs:pair alpha beta)))

(defun natural-exponentiation (alpha beta)
  "Natural exponentiation"
  (types:make-typed 'natural-exponentiation (pairs:pair alpha beta)))

(defun epsilon-numbers (index)
  "Epsilon numbers ε_α"
  (types:make-typed 'epsilon-numbers index))

(defun gamma-numbers (index)
  "Gamma numbers Γ_α"
  (types:make-typed 'gamma-numbers index))

(defun veblen-fixed-points (function level)
  "Veblen fixed points"
  (types:make-typed 'veblen-fixed-points (pairs:pair function level)))

(defun admissible-collapse (ordinal)
  "Admissible collapse"
  (types:make-typed 'admissible-collapse ordinal))

(defun mahlo-collapse (ordinal)
  "Mahlo collapse"
  (types:make-typed 'mahlo-collapse ordinal))

;;; Transfinite Types ;;;

(defun ordinal-indexed-type (ordinal type)
  "Ordinal indexed type"
  (types:make-typed 'ordinal-indexed-type (pairs:pair ordinal type)))

(defun cumulative-hierarchy-type (level)
  "Cumulative hierarchy type V_α"
  (types:make-typed 'cumulative-hierarchy level))

(defun hereditarily-finite-ordinal (ordinal)
  "Hereditarily finite"
  (types:make-typed 'hereditarily-finite ordinal))

(defun hereditarily-countable-ordinal (ordinal)
  "Hereditarily countable"
  (types:make-typed 'hereditarily-countable ordinal))

(defun constructibility-degrees-ordinal (degree)
  "Constructibility degrees"
  (types:make-typed 'constructibility-degrees degree))

(defun minimal-model-ordinal (theory)
  "Minimal model ordinal"
  (types:make-typed 'minimal-model-ordinal theory))

;;; Special Ordinals ;;;

(defun critical-ordinals (property)
  "Critical ordinals"
  (types:make-typed 'critical-ordinals property))

(defun additively-indecomposable (ordinal)
  "Additively indecomposable"
  (types:make-typed 'additively-indecomposable ordinal))

(defun multiplicatively-indecomposable (ordinal)
  "Multiplicatively indecomposable"
  (types:make-typed 'multiplicatively-indecomposable ordinal))

(defun epsilon-critical (ordinal)
  "ε-critical ordinal"
  (types:make-typed 'epsilon-critical ordinal))

(defun strongly-critical (ordinal)
  "Strongly critical"
  (types:make-typed 'strongly-critical ordinal))

(defun reflecting-ordinals (formula)
  "Reflecting ordinals"
  (types:make-typed 'reflecting-ordinals formula))

(defun stationary-ordinals (kappa)
  "Stationary ordinals"
  (types:make-typed 'stationary-ordinals kappa))

(defun club-ordinals (kappa)
  "Club ordinals"
  (types:make-typed 'club-ordinals kappa))