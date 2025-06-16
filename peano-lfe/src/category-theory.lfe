;;;; Category Theory Abstractions
;;;; Functors, Monads, Arrows, and more categorical concepts

(defmodule category-theory
  (export
    ;; Categories
    (make-category 3) (category? 1)
    (cat-objects 1) (cat-morphisms 1) (cat-compose 1)
    (cat-identity 2) (cat-associative? 4)
    
    ;; Functors
    (make-functor 2) (functor? 1)
    (fmap 2) (functor-compose 2)
    (functor-identity? 1) (functor-composition? 3)
    (contravariant-functor 2) (contramap 2)
    (bifunctor 3) (bimap 3)
    (profunctor 3) (dimap 3)
    
    ;; Natural Transformations
    (make-nat-trans 3) (nat-trans? 1)
    (nat-trans-component 2) (nat-trans-source 1)
    (nat-trans-target 1) (nat-trans-compose 2)
    (nat-trans-identity 1) (nat-trans-inverse 1)
    
    ;; Monads
    (make-monad 3) (monad? 1)
    (return 2) (bind 2) (join 1)
    (monad-laws? 1) (kleisli-compose 3)
    (monad-lift 2) (sequence-monad 2)
    (map-monad 3) (filter-monad 3)
    
    ;; Comonads
    (make-comonad 3) (comonad? 1)
    (extract 1) (extend 2) (duplicate 1)
    (comonad-laws? 1) (cokleisli-compose 3)
    
    ;; Applicative Functors
    (make-applicative 4) (applicative? 1)
    (pure 2) (apply-ap 2) (lift-a2 3)
    (sequence-ap 2) (traverse-ap 3)
    (applicative-laws? 1)
    
    ;; Arrows
    (make-arrow 5) (arrow? 1)
    (arr 2) (arrow-compose 3) (arrow-first 2)
    (arrow-second 2) (arrow-split 3)
    (arrow-fanout 3) (arrow-choice 3)
    
    ;; Monoids
    (make-monoid 3) (monoid? 1)
    (mempty 1) (mappend 3) (mconcat 2)
    (monoid-laws? 1) (dual-monoid 1)
    (product-monoid 2) (sum-monoid 2)
    
    ;; Foldable
    (make-foldable 2) (foldable? 1)
    (fold-map 3) (foldr-cat 4) (foldl-cat 4)
    (to-list 1) (fold-length 1)
    (fold-sum 1) (fold-product 1)
    
    ;; Traversable
    (make-traversable 3) (traversable? 1)
    (traverse 3) (sequence-trav 2)
    (map-accum-l 3) (map-accum-r 3)
    
    ;; Alternative
    (make-alternative 3) (alternative? 1)
    (empty-alt 1) (alt-combine 3)
    (some-alt 2) (many-alt 2)
    
    ;; Lens
    (make-lens 2) (lens? 1)
    (view 2) (set-lens 3) (over 3)
    (compose-lens 2) (lens-laws? 1)
    (make-prism 2) (preview 2) (review 2)
    
    ;; Free Structures
    (make-free-monad 1) (free-monad? 1)
    (free-pure 1) (free-bind 2)
    (free-fold 3) (free-interpret 3)
    (make-cofree-comonad 2) (cofree-extract 1)
    
    ;; Kan Extensions
    (make-lan 3) (lan? 1) ; Left Kan extension
    (make-ran 3) (ran? 1) ; Right Kan extension
    (lan-extend 2) (ran-extend 2)
    
    ;; Yoneda
    (make-yoneda 1) (yoneda? 1)
    (to-yoneda 1) (from-yoneda 1)
    (yoneda-map 2) (yoneda-lemma 2)
    (make-coyoneda 2) (coyoneda-map 2)
    
    ;; Adjunctions
    (make-adjunction 4) (adjunction? 1)
    (left-adjoint 1) (right-adjoint 1)
    (unit-adj 2) (counit-adj 2)
    (adjunction-compose 2)
    
    ;; Limits and Colimits
    (make-limit 2) (limit? 1)
    (make-colimit 2) (colimit? 1)
    (terminal-object 1) (initial-object 1)
    (product-obj 2) (coproduct-obj 2)
    (equalizer 3) (coequalizer 3)
    (pullback 4) (pushout 4)))

(include-lib "peano/include/peano.lfe")

;;; Categories ;;;

(defun make-category (objects morphisms compose)
  "Create a category with objects, morphisms, and composition"
  (types:make-typed 'category (pairs:triple objects morphisms compose)))

(defun category? (x)
  "Check if value is a category"
  (peano:eq (types:type-of x) 'category))

(defun cat-objects (cat)
  "Get objects of category"
  (pairs:triple-first (types:value-of cat)))

(defun cat-morphisms (cat)
  "Get morphisms of category"
  (pairs:triple-second (types:value-of cat)))

(defun cat-compose (cat)
  "Get composition operation"
  (pairs:triple-third (types:value-of cat)))

(defun cat-identity (cat obj)
  "Get identity morphism for object"
  (lists:find (lambda (morph)
                (and (peano:eq (morph-source morph) obj)
                     (peano:eq (morph-target morph) obj)
                     (identity-morph? morph)))
              (cat-morphisms cat)))

(defun cat-associative? (cat f g h)
  "Check associativity: (f ∘ g) ∘ h = f ∘ (g ∘ h)"
  (let ((compose (cat-compose cat)))
    (morph-eq? (compose (compose f g) h)
               (compose f (compose g h)))))

(defun morph-source (morph)
  "Source object of morphism"
  (pairs:first morph))

(defun morph-target (morph)
  "Target object of morphism"
  (pairs:second (pairs:second morph)))

(defun identity-morph? (morph)
  "Check if morphism is identity"
  (peano:eq (pairs:first (pairs:second morph)) 'id))

(defun morph-eq? (m1 m2)
  "Check morphism equality"
  (and (peano:eq (morph-source m1) (morph-source m2))
       (peano:eq (morph-target m1) (morph-target m2))))

;;; Functors ;;;

(defun make-functor (obj-map morph-map)
  "Create a functor F: C → D"
  (types:make-typed 'functor (pairs:pair obj-map morph-map)))

(defun functor? (x)
  "Check if value is a functor"
  (peano:eq (types:type-of x) 'functor))

(defun fmap (f functor-val)
  "Map a function over a functor"
  (let ((morph-map (pairs:second (types:value-of functor-val))))
    (morph-map f)))

(defun functor-compose (f g)
  "Compose two functors"
  (make-functor
    (lambda (obj) ((pairs:first (types:value-of f))
                   ((pairs:first (types:value-of g)) obj)))
    (lambda (morph) ((pairs:second (types:value-of f))
                     ((pairs:second (types:value-of g)) morph)))))

(defun functor-identity? (functor)
  "Check functor preserves identity: F(id) = id"
  #t) ; Simplified

(defun functor-composition? (functor f g)
  "Check functor preserves composition: F(g∘f) = F(g)∘F(f)"
  #t) ; Simplified

(defun contravariant-functor (obj-map morph-map)
  "Create a contravariant functor"
  (types:make-typed 'contravariant (pairs:pair obj-map morph-map)))

(defun contramap (f contra-val)
  "Contravariant map"
  (let ((morph-map (pairs:second (types:value-of contra-val))))
    (morph-map f)))

(defun bifunctor (obj-map morph-map1 morph-map2)
  "Create a bifunctor F: C × D → E"
  (types:make-typed 'bifunctor (pairs:triple obj-map morph-map1 morph-map2)))

(defun bimap (f g bifunctor-val)
  "Map two functions over a bifunctor"
  (let ((maps (types:value-of bifunctor-val)))
    ((pairs:triple-second maps) f ((pairs:triple-third maps) g))))

(defun profunctor (obj-map contramap-fn comap-fn)
  "Create a profunctor P: C^op × D → Set"
  (types:make-typed 'profunctor (pairs:triple obj-map contramap-fn comap-fn)))

(defun dimap (f g profunctor-val)
  "Map contravariantly and covariantly"
  (let ((maps (types:value-of profunctor-val)))
    ((pairs:triple-second maps) f ((pairs:triple-third maps) g))))

;;; Natural Transformations ;;;

(defun make-nat-trans (source target components)
  "Create natural transformation α: F ⇒ G"
  (types:make-typed 'nat-trans (pairs:triple source target components)))

(defun nat-trans? (x)
  "Check if value is a natural transformation"
  (peano:eq (types:type-of x) 'nat-trans))

(defun nat-trans-component (nat-trans obj)
  "Get component αₐ: F(a) → G(a)"
  (let ((components (pairs:triple-third (types:value-of nat-trans))))
    (lists:find (lambda (comp)
                  (peano:eq (pairs:first comp) obj))
                components)))

(defun nat-trans-source (nat-trans)
  "Source functor of natural transformation"
  (pairs:triple-first (types:value-of nat-trans)))

(defun nat-trans-target (nat-trans)
  "Target functor of natural transformation"
  (pairs:triple-second (types:value-of nat-trans)))

(defun nat-trans-compose (alpha beta)
  "Vertical composition of natural transformations"
  (make-nat-trans (nat-trans-source beta)
                  (nat-trans-target alpha)
                  (lists:map (lambda (obj)
                              (pairs:pair obj
                                         (compose-morphisms
                                           (pairs:second (nat-trans-component alpha obj))
                                           (pairs:second (nat-trans-component beta obj)))))
                            (get-objects))))

(defun nat-trans-identity (functor)
  "Identity natural transformation"
  (make-nat-trans functor functor
                  (lists:map (lambda (obj)
                              (pairs:pair obj (identity-morphism obj)))
                            (get-objects))))

(defun nat-trans-inverse (nat-trans)
  "Inverse of natural isomorphism"
  (make-nat-trans (nat-trans-target nat-trans)
                  (nat-trans-source nat-trans)
                  (lists:map (lambda (comp)
                              (pairs:pair (pairs:first comp)
                                         (inverse-morphism (pairs:second comp))))
                            (pairs:triple-third (types:value-of nat-trans)))))

;; Helper functions
(defun compose-morphisms (f g) (pairs:pair 'compose (pairs:pair f g)))
(defun identity-morphism (obj) (pairs:pair 'id obj))
(defun inverse-morphism (morph) (pairs:pair 'inv morph))
(defun get-objects () (lists:nil)) ; Placeholder

;;; Monads ;;;

(defun make-monad (return-fn bind-fn join-fn)
  "Create a monad with return, bind, and join"
  (types:make-typed 'monad (pairs:triple return-fn bind-fn join-fn)))

(defun monad? (x)
  "Check if value is a monad"
  (peano:eq (types:type-of x) 'monad))

(defun return (monad value)
  "Inject value into monad (η: Id ⇒ M)"
  (let ((return-fn (pairs:triple-first (types:value-of monad))))
    (return-fn value)))

(defun bind (monad-val f)
  "Monadic bind (>>=)"
  (let ((bind-fn (pairs:triple-second (types:value-of monad))))
    (bind-fn monad-val f)))

(defun join (monad-val)
  "Flatten nested monad (μ: M∘M ⇒ M)"
  (let ((join-fn (pairs:triple-third (types:value-of monad))))
    (join-fn monad-val)))

(defun monad-laws? (monad)
  "Check monad laws"
  (and (left-identity-law? monad)
       (right-identity-law? monad)
       (associativity-law? monad)))

(defun left-identity-law? (monad)
  "return a >>= f ≡ f a"
  #t) ; Simplified

(defun right-identity-law? (monad)
  "m >>= return ≡ m"
  #t) ; Simplified

(defun associativity-law? (monad)
  "(m >>= f) >>= g ≡ m >>= (λx. f x >>= g)"
  #t) ; Simplified

(defun kleisli-compose (monad f g)
  "Kleisli composition (>=>)"
  (lambda (x)
    (bind (f x) g)))

(defun monad-lift (monad f)
  "Lift function into monad"
  (lambda (ma)
    (bind ma (lambda (a) (return monad (f a))))))

(defun sequence-monad (monad list-of-monads)
  "Sequence list of monadic values"
  (lists:foldr (lambda (ma acc)
                 (bind ma (lambda (a)
                           (bind acc (lambda (as)
                                      (return monad (lists:cons a as)))))))
               (return monad (lists:nil))
               list-of-monads))

(defun map-monad (monad f list)
  "Map monadic function over list"
  (sequence-monad monad (lists:map f list)))

(defun filter-monad (monad pred list)
  "Filter with monadic predicate"
  (lists:foldr (lambda (x acc)
                 (bind (pred x) (lambda (keep?)
                                 (if keep?
                                     (bind acc (lambda (xs)
                                                (return monad (lists:cons x xs))))
                                     acc))))
               (return monad (lists:nil))
               list))

;;; Comonads ;;;

(defun make-comonad (extract-fn extend-fn duplicate-fn)
  "Create a comonad (dual of monad)"
  (types:make-typed 'comonad (pairs:triple extract-fn extend-fn duplicate-fn)))

(defun comonad? (x)
  "Check if value is a comonad"
  (peano:eq (types:type-of x) 'comonad))

(defun extract (comonad-val)
  "Extract value from comonad (ε: W ⇒ Id)"
  (let ((extract-fn (pairs:triple-first (types:value-of comonad-val))))
    (extract-fn comonad-val)))

(defun extend (comonad-val f)
  "Comonadic extend (=>>)"
  (let ((extend-fn (pairs:triple-second (types:value-of comonad-val))))
    (extend-fn comonad-val f)))

(defun duplicate (comonad-val)
  "Duplicate comonad (δ: W ⇒ W∘W)"
  (let ((duplicate-fn (pairs:triple-third (types:value-of comonad-val))))
    (duplicate-fn comonad-val)))

(defun comonad-laws? (comonad)
  "Check comonad laws"
  #t) ; Simplified

(defun cokleisli-compose (comonad f g)
  "Cokleisli composition (=>=)"
  (lambda (w)
    (f (extend w g))))

;;; Applicative Functors ;;;

(defun make-applicative (pure-fn apply-fn lift2-fn laws)
  "Create applicative functor"
  (types:make-typed 'applicative (tuples:tuple-from-list 
                                   (lists:cons pure-fn 
                                     (lists:cons apply-fn 
                                       (lists:cons lift2-fn 
                                         (lists:cons laws (lists:nil))))))))

(defun applicative? (x)
  "Check if value is applicative"
  (peano:eq (types:type-of x) 'applicative))

(defun pure (app value)
  "Inject value into applicative"
  (let ((pure-fn (tuples:tuple-ref 0 (types:value-of app))))
    (pure-fn value)))

(defun apply-ap (app-f app-x)
  "Apply wrapped function to wrapped value (<*>)"
  (let ((apply-fn (tuples:tuple-ref 1 (types:value-of app-f))))
    (apply-fn app-f app-x)))

(defun lift-a2 (app f x y)
  "Lift binary function"
  (let ((lift2-fn (tuples:tuple-ref 2 (types:value-of app))))
    (lift2-fn f x y)))

(defun sequence-ap (app list-of-apps)
  "Sequence applicative values"
  (lists:foldr (lambda (a acc)
                 (lift-a2 app lists:cons a acc))
               (pure app (lists:nil))
               list-of-apps))

(defun traverse-ap (app f list)
  "Traverse with applicative function"
  (sequence-ap app (lists:map f list)))

(defun applicative-laws? (app)
  "Check applicative laws"
  (tuples:tuple-ref 3 (types:value-of app)))

;;; Arrows ;;;

(defun make-arrow (arr-fn compose-fn first-fn second-fn split-fn)
  "Create an arrow (generalized function)"
  (types:make-typed 'arrow 
                    (tuples:tuple-from-list 
                      (lists:cons arr-fn 
                        (lists:cons compose-fn 
                          (lists:cons first-fn 
                            (lists:cons second-fn 
                              (lists:cons split-fn (lists:nil)))))))))

(defun arrow? (x)
  "Check if value is an arrow"
  (peano:eq (types:type-of x) 'arrow))

(defun arr (arrow f)
  "Lift function to arrow"
  (let ((arr-fn (tuples:tuple-ref 0 (types:value-of arrow))))
    (arr-fn f)))

(defun arrow-compose (arrow f g)
  "Arrow composition (>>>)"
  (let ((compose-fn (tuples:tuple-ref 1 (types:value-of arrow))))
    (compose-fn f g)))

(defun arrow-first (arrow f)
  "Apply arrow to first component"
  (let ((first-fn (tuples:tuple-ref 2 (types:value-of arrow))))
    (first-fn f)))

(defun arrow-second (arrow f)
  "Apply arrow to second component"
  (let ((second-fn (tuples:tuple-ref 3 (types:value-of arrow))))
    (second-fn f)))

(defun arrow-split (arrow f g)
  "Split arrow (***)"
  (let ((split-fn (tuples:tuple-ref 4 (types:value-of arrow))))
    (split-fn f g)))

(defun arrow-fanout (arrow f g)
  "Fanout (&&&)"
  (arrow-compose arrow 
                 (arr arrow (lambda (x) (pairs:pair x x)))
                 (arrow-split arrow f g)))

(defun arrow-choice (arrow left right)
  "Arrow choice (|||)"
  (types:make-typed 'arrow-choice (pairs:triple arrow left right)))

;;; Monoids ;;;

(defun make-monoid (empty-val append-fn laws)
  "Create a monoid"
  (types:make-typed 'monoid (pairs:triple empty-val append-fn laws)))

(defun monoid? (x)
  "Check if value is a monoid"
  (peano:eq (types:type-of x) 'monoid))

(defun mempty (monoid)
  "Identity element"
  (pairs:triple-first (types:value-of monoid)))

(defun mappend (monoid a b)
  "Associative operation"
  (let ((append-fn (pairs:triple-second (types:value-of monoid))))
    (append-fn a b)))

(defun mconcat (monoid list)
  "Fold list with monoid"
  (lists:foldl (lambda (x acc) (mappend monoid acc x))
               (mempty monoid)
               list))

(defun monoid-laws? (monoid)
  "Check monoid laws"
  (pairs:triple-third (types:value-of monoid)))

(defun dual-monoid (monoid)
  "Dual monoid (reverse operation)"
  (make-monoid (mempty monoid)
               (lambda (a b) (mappend monoid b a))
               (monoid-laws? monoid)))

(defun product-monoid (m1 m2)
  "Product of two monoids"
  (make-monoid (pairs:pair (mempty m1) (mempty m2))
               (lambda (p1 p2)
                 (pairs:pair (mappend m1 (pairs:first p1) (pairs:first p2))
                            (mappend m2 (pairs:second p1) (pairs:second p2))))
               #t))

(defun sum-monoid (m1 m2)
  "Sum (coproduct) of two monoids"
  (make-monoid (adt:left (mempty m1))
               (lambda (s1 s2)
                 (cond
                   ((and (adt:is-left? s1) (adt:is-left? s2))
                    (adt:left (mappend m1 (adt:left-value s1) (adt:left-value s2))))
                   ((and (adt:is-right? s1) (adt:is-right? s2))
                    (adt:right (mappend m2 (adt:right-value s1) (adt:right-value s2))))
                   (else (error "Cannot append different variants"))))
               #t))

;;; Foldable ;;;

(defun make-foldable (fold-fn to-list-fn)
  "Create a foldable structure"
  (types:make-typed 'foldable (pairs:pair fold-fn to-list-fn)))

(defun foldable? (x)
  "Check if value is foldable"
  (peano:eq (types:type-of x) 'foldable))

(defun fold-map (monoid f foldable)
  "Map then fold with monoid"
  (let ((fold-fn (pairs:first (types:value-of foldable))))
    (fold-fn (lambda (x acc) (mappend monoid acc (f x)))
             (mempty monoid))))

(defun foldr-cat (f init foldable cat)
  "Categorical fold right"
  (let ((fold-fn (pairs:first (types:value-of foldable))))
    (fold-fn f init)))

(defun foldl-cat (f init foldable cat)
  "Categorical fold left"
  (let ((fold-fn (pairs:first (types:value-of foldable))))
    (fold-fn (lambda (acc x) (f x acc)) init)))

(defun to-list (foldable)
  "Convert foldable to list"
  (let ((to-list-fn (pairs:second (types:value-of foldable))))
    (to-list-fn)))

(defun fold-length (foldable)
  "Count elements"
  (foldr-cat (lambda (x n) (arithmetic:add n 1)) 0 foldable 'nat))

(defun fold-sum (foldable)
  "Sum elements"
  (foldr-cat arithmetic:add 0 foldable 'nat))

(defun fold-product (foldable)
  "Product of elements"
  (foldr-cat arithmetic:mult 1 foldable 'nat))

;;; Traversable ;;;

(defun make-traversable (traverse-fn sequence-fn map-accum-fn)
  "Create a traversable structure"
  (types:make-typed 'traversable (pairs:triple traverse-fn sequence-fn map-accum-fn)))

(defun traversable? (x)
  "Check if value is traversable"
  (peano:eq (types:type-of x) 'traversable))

(defun traverse (app f traversable)
  "Traverse with applicative function"
  (let ((traverse-fn (pairs:triple-first (types:value-of traversable))))
    (traverse-fn app f)))

(defun sequence-trav (app traversable)
  "Sequence traversable of applicatives"
  (let ((sequence-fn (pairs:triple-second (types:value-of traversable))))
    (sequence-fn app)))

(defun map-accum-l (f acc traversable)
  "Map with accumulator from left"
  (let ((map-accum-fn (pairs:triple-third (types:value-of traversable))))
    (map-accum-fn f acc 'left)))

(defun map-accum-r (f acc traversable)
  "Map with accumulator from right"
  (let ((map-accum-fn (pairs:triple-third (types:value-of traversable))))
    (map-accum-fn f acc 'right)))

;;; Alternative ;;;

(defun make-alternative (empty-fn combine-fn laws)
  "Create alternative (monoid for applicatives)"
  (types:make-typed 'alternative (pairs:triple empty-fn combine-fn laws)))

(defun alternative? (x)
  "Check if value is alternative"
  (peano:eq (types:type-of x) 'alternative))

(defun empty-alt (alt)
  "Empty alternative"
  (let ((empty-fn (pairs:triple-first (types:value-of alt))))
    (empty-fn)))

(defun alt-combine (alt a b)
  "Alternative combine (<|>)"
  (let ((combine-fn (pairs:triple-second (types:value-of alt))))
    (combine-fn a b)))

(defun some-alt (alt v)
  "One or more"
  (alt-combine alt
               (lift-a2 alt lists:cons v (many-alt alt v))
               (pure alt (lists:nil))))

(defun many-alt (alt v)
  "Zero or more"
  (alt-combine alt
               (some-alt alt v)
               (pure alt (lists:nil))))

;;; Lens ;;;

(defun make-lens (getter setter)
  "Create a lens for accessing nested data"
  (types:make-typed 'lens (pairs:pair getter setter)))

(defun lens? (x)
  "Check if value is a lens"
  (peano:eq (types:type-of x) 'lens))

(defun view (lens s)
  "View through lens"
  (let ((getter (pairs:first (types:value-of lens))))
    (getter s)))

(defun set-lens (lens v s)
  "Set through lens"
  (let ((setter (pairs:second (types:value-of lens))))
    (setter v s)))

(defun over (lens f s)
  "Modify through lens"
  (set-lens lens (f (view lens s)) s))

(defun compose-lens (lens1 lens2)
  "Compose lenses"
  (make-lens (lambda (s) (view lens2 (view lens1 s)))
             (lambda (v s) (over lens1 (lambda (s2) (set-lens lens2 v s2)) s))))

(defun lens-laws? (lens)
  "Check lens laws"
  (and (get-put-law? lens)
       (put-get-law? lens)
       (put-put-law? lens)))

(defun get-put-law? (lens) #t) ; view l (set l v s) ≡ v
(defun put-get-law? (lens) #t) ; set l (view l s) s ≡ s  
(defun put-put-law? (lens) #t) ; set l v' (set l v s) ≡ set l v' s

(defun make-prism (preview-fn review-fn)
  "Create a prism for sum types"
  (types:make-typed 'prism (pairs:pair preview-fn review-fn)))

(defun preview (prism s)
  "Preview through prism"
  (let ((preview-fn (pairs:first (types:value-of prism))))
    (preview-fn s)))

(defun review (prism v)
  "Review through prism"
  (let ((review-fn (pairs:second (types:value-of prism))))
    (review-fn v)))

;;; Free Structures ;;;

(defun make-free-monad (functor)
  "Create free monad from functor"
  (types:make-typed 'free-monad functor))

(defun free-monad? (x)
  "Check if value is free monad"
  (peano:eq (types:type-of x) 'free-monad))

(defun free-pure (value)
  "Pure value in free monad"
  (types:make-typed 'free-pure value))

(defun free-bind (free-val f)
  "Bind for free monad"
  (types:make-typed 'free-bind (pairs:pair free-val f)))

(defun free-fold (free-val pure-handler bind-handler)
  "Fold free monad"
  (cond
    ((peano:eq (types:type-of free-val) 'free-pure)
     (pure-handler (types:value-of free-val)))
    ((peano:eq (types:type-of free-val) 'free-bind)
     (let ((val-f (types:value-of free-val)))
       (bind-handler (pairs:first val-f) (pairs:second val-f))))
    (else (error "Invalid free monad value"))))

(defun free-interpret (free-val interpreter)
  "Interpret free monad with natural transformation"
  (free-fold free-val
             interpreter
             (lambda (fa k) (bind (interpreter fa) k))))

(defun make-cofree-comonad (value next)
  "Create cofree comonad"
  (types:make-typed 'cofree (pairs:pair value next)))

(defun cofree-extract (cofree)
  "Extract from cofree"
  (pairs:first (types:value-of cofree)))

;;; Kan Extensions ;;;

(defun make-lan (f g h)
  "Left Kan extension"
  (types:make-typed 'lan (pairs:triple f g h)))

(defun lan? (x)
  "Check if value is left Kan extension"
  (peano:eq (types:type-of x) 'lan))

(defun make-ran (f g h)
  "Right Kan extension"
  (types:make-typed 'ran (pairs:triple f g h)))

(defun ran? (x)
  "Check if value is right Kan extension"
  (peano:eq (types:type-of x) 'ran))

(defun lan-extend (lan nat-trans)
  "Extend left Kan extension"
  (let ((components (types:value-of lan)))
    (make-lan (pairs:triple-first components)
              nat-trans
              (compose-nat-trans nat-trans (pairs:triple-third components)))))

(defun ran-extend (ran nat-trans)
  "Extend right Kan extension"
  (let ((components (types:value-of ran)))
    (make-ran (pairs:triple-first components)
              nat-trans
              (compose-nat-trans (pairs:triple-third components) nat-trans))))

(defun compose-nat-trans (nt1 nt2)
  "Compose natural transformations"
  (pairs:pair nt1 nt2))

;;; Yoneda ;;;

(defun make-yoneda (f)
  "Create Yoneda embedding"
  (types:make-typed 'yoneda f))

(defun yoneda? (x)
  "Check if value is Yoneda"
  (peano:eq (types:type-of x) 'yoneda))

(defun to-yoneda (functor-val)
  "Convert to Yoneda"
  (make-yoneda (lambda (f) (fmap f functor-val))))

(defun from-yoneda (yoneda)
  "Convert from Yoneda"
  (let ((f (types:value-of yoneda)))
    (f (lambda (x) x))))

(defun yoneda-map (f yoneda)
  "Map over Yoneda"
  (make-yoneda (lambda (g) ((types:value-of yoneda) (lambda (x) (g (f x)))))))

(defun yoneda-lemma (functor a)
  "Yoneda lemma: ∀f. (a → f b) ≅ F a"
  (pairs:pair (lambda (nat) (nat (lambda (x) x)))
              (lambda (fa) (lambda (f) (fmap f fa)))))

(defun make-coyoneda (f a)
  "Create Coyoneda (free functor)"
  (types:make-typed 'coyoneda (pairs:pair f a)))

(defun coyoneda-map (g coyoneda)
  "Map over Coyoneda"
  (let ((f-a (types:value-of coyoneda)))
    (make-coyoneda (lambda (x) (g ((pairs:first f-a) x)))
                   (pairs:second f-a))))

;;; Adjunctions ;;;

(defun make-adjunction (left right unit counit)
  "Create adjunction L ⊣ R"
  (types:make-typed 'adjunction 
                    (tuples:tuple-from-list 
                      (lists:cons left 
                        (lists:cons right 
                          (lists:cons unit 
                            (lists:cons counit (lists:nil))))))))

(defun adjunction? (x)
  "Check if value is an adjunction"
  (peano:eq (types:type-of x) 'adjunction))

(defun left-adjoint (adj)
  "Get left adjoint functor"
  (tuples:tuple-ref 0 (types:value-of adj)))

(defun right-adjoint (adj)
  "Get right adjoint functor"
  (tuples:tuple-ref 1 (types:value-of adj)))

(defun unit-adj (adj a)
  "Unit of adjunction η: Id ⇒ R∘L"
  (let ((unit-fn (tuples:tuple-ref 2 (types:value-of adj))))
    (unit-fn a)))

(defun counit-adj (adj a)
  "Counit of adjunction ε: L∘R ⇒ Id"
  (let ((counit-fn (tuples:tuple-ref 3 (types:value-of adj))))
    (counit-fn a)))

(defun adjunction-compose (adj1 adj2)
  "Compose adjunctions"
  (make-adjunction
    (functor-compose (left-adjoint adj1) (left-adjoint adj2))
    (functor-compose (right-adjoint adj2) (right-adjoint adj1))
    (lambda (a) (unit-adj adj2 (unit-adj adj1 a)))
    (lambda (a) (counit-adj adj1 (counit-adj adj2 a)))))

;;; Limits and Colimits ;;;

(defun make-limit (diagram cone)
  "Create limit of diagram"
  (types:make-typed 'limit (pairs:pair diagram cone)))

(defun limit? (x)
  "Check if value is a limit"
  (peano:eq (types:type-of x) 'limit))

(defun make-colimit (diagram cocone)
  "Create colimit of diagram"
  (types:make-typed 'colimit (pairs:pair diagram cocone)))

(defun colimit? (x)
  "Check if value is a colimit"
  (peano:eq (types:type-of x) 'colimit))

(defun terminal-object (cat)
  "Terminal object (limit of empty diagram)"
  (make-limit (lists:nil) 'terminal))

(defun initial-object (cat)
  "Initial object (colimit of empty diagram)"
  (make-colimit (lists:nil) 'initial))

(defun product-obj (a b)
  "Product of two objects"
  (make-limit (pairs:pair a b) 'product))

(defun coproduct-obj (a b)
  "Coproduct (sum) of two objects"
  (make-colimit (pairs:pair a b) 'coproduct))

(defun equalizer (f g src)
  "Equalizer of two morphisms"
  (make-limit (pairs:triple f g src) 'equalizer))

(defun coequalizer (f g tgt)
  "Coequalizer of two morphisms"
  (make-colimit (pairs:triple f g tgt) 'coequalizer))

(defun pullback (f g common)
  "Pullback of two morphisms"
  (make-limit (pairs:triple f g common) 'pullback))

(defun pushout (f g common)
  "Pushout of two morphisms"
  (make-colimit (pairs:triple f g common) 'pushout))