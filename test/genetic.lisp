(load "lib/genetic.lisp")

(plan 1)

(is '(A B C A B) (first-guess 5 '(a b c)))

(finalize)