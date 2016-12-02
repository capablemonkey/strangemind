(load (merge-pathnames "benchmark.lisp" *load-truename*))

(defparameter *peg-count-range* '(4 5 6 7 8 9 10))
(defparameter *color-count-range* '(6 8 10 12 16 20))

(defparameter *population-size* 200)
(defparameter *generations-per-guess* 100)
(defparameter *mutation-rate* 0.01)
(defparameter *permutation-rate* 0.01)
(defparameter *inversion-rate* 0.01)

(defparameter *scsa-name* 'insert-colors)

(loop
  for peg-count in *peg-count-range*
  do
    (loop
      for color-count in *color-count-range*
      do
        (Mastermind peg-count color-count nil)
        (benchmark-tournament 'Genetic 5)))