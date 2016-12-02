(load (merge-pathnames "benchmark.lisp" *load-truename*))

(defparameter *pegs* 4)
(defparameter *colors* 6)
(Mastermind *pegs* *colors* nil)

(defparameter *population-size* 200)
(defparameter *generations-per-guess* 100)
(defparameter *mutation-rate* 0.01)
(defparameter *permutation-rate* 0.01)
(defparameter *inversion-rate* 0.01)

(defparameter *scsa-consistency-multiplier* 0.25)

(defmacro test-scsa (scsa-name)
  `(progn
    (set-parameter *scsa-name* ,scsa-name)
    (Mastermind *pegs* *colors* ,scsa-name)
    (benchmark-tournament 'Genetic 5)))

(test-scsa 'insert-colors)
(test-scsa 'two-color)
(test-scsa 'ab-color)
(test-scsa 'two-color-alternating)
(test-scsa 'first-and-last)
(test-scsa 'usually-fewer)
(test-scsa 'prefer-fewer)
(quit)