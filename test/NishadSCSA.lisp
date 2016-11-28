(load (merge-pathnames "../lib/NishadSCSA.lisp" *load-truename*))

(subtest "Testing SCSA matcher"
  (is t
    (2-color-checker-p '(A B A B A B))
    "2-color-checker-p returns T for (A B A B A B)")

  (is nil
    (2-color-checker-p '(A C B A C B))
    "2-color-checker-p returns Nil for (A C B A C B)")

  (is 1
    (matches-scsa 'two-color '(A B A B A)))
  
  (is 0
    (matches-scsa 'two-color '(A B C A B))))