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

(set-parameter *scsa-name* 'insert-colors)
(benchmark-tournament 'Genetic 5)

(set-parameter *scsa-name* 'two-color)
(benchmark-tournament 'Genetic 5)

(set-parameter *scsa-name* 'ab-color)
(benchmark-tournament 'Genetic 5)

(set-parameter *scsa-name* 'two-color-alternating)
(benchmark-tournament 'Genetic 5)

(set-parameter *scsa-name* 'first-and-last)
(benchmark-tournament 'Genetic 5)

(set-parameter *scsa-name* 'usually-fewer)
(benchmark-tournament 'Genetic 5)

(set-parameter *scsa-name* 'prefer-fewer)
(benchmark-tournament 'Genetic 5)

(quit)