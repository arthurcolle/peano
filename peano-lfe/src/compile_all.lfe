(defmodule compile-all
  (export (all 0)))

(defun all ()
  (lists:foreach
    (lambda (file)
      (io:format "Compiling ~s...~n" (list file))
      (case (lfe_comp:file file)
        (#(ok _) (io:format "  ✓ Success~n"))
        (err (io:format "  ✗ Error: ~p~n" (list err)))))
    '("src/peano.lfe"
      "src/arithmetic.lfe"
      "src/pairs.lfe"
      "src/lists.lfe"
      "src/ordinals.lfe"
      "src/hypergraphs.lfe"
      "src/compute.lfe"))
  'done)