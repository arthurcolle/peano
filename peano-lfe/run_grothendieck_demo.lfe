;;;; Run Grothendieck demonstration

(include-lib "peano/include/peano.lfe")

;; Run the demonstration
(grothendieck-demonstration:run-all-demos)

;; Exit cleanly
(erlang:halt 0)