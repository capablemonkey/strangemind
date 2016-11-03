(load "lib/genetic.lisp")

(plan 2)

(is '(A B C A B) (first-guess 5 '(a b c)))

; ensure initial population is of size *population-size*
(is *population-size* (length (initial-population 4 '(A B C D E F))))

; TODO: ensure each individual in initial population contains only valid colors
; TODO: ensure each individual in initial population is of the correct length

(finalize)