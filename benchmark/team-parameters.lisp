(load (merge-pathnames "benchmark.lisp" *load-truename*))

;
; Benchmarks stage 1
;

(defparameter *pegs* 4)
(defparameter *colors* 6)
(Mastermind *pegs* *colors* nil)

; Base case:
(defparameter *tournaments-per-trial* 5)
(defparameter *default-population* 150)
(defparameter *default-generations* 50)
(defparameter *default-mutation* 0.03)
(defparameter *default-inversion* 0.03)
(defparameter *default-permutation* 0.03)

(print "Base case")
(defparameter *population-size* *default-population*)
(defparameter *generations-per-guess* *default-generations*)
(defparameter *mutation-rate* *default-mutation*)
(defparameter *permutation-rate* *default-permutation*)
(defparameter *inversion-rate* *default-inversion*)
(benchmark-tournament 'Genetic *tournaments-per-trial*)

(print "Tweaking population")
(set-parameter *population-size* 50)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *population-size* 100)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *population-size* 200)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *population-size* 500)
(benchmark-tournament 'Genetic *tournaments-per-trial*)

; important: reset population to baseline:
(set-parameter *population-size* *default-population*)

(print "Tweaking generations")
(set-parameter *generations-per-guess* 1)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *generations-per-guess* 10)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *generations-per-guess* 100)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *generations-per-guess* 200)
(benchmark-tournament 'Genetic *tournaments-per-trial*)

(set-parameter *generations-per-guess* *default-generations*)

(print "Tweaking mutation-rate")
(set-parameter *mutation-rate* 0.01)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *mutation-rate* 0.10)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *mutation-rate* 0.20)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *mutation-rate* 0.50)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *mutation-rate* *default-mutation*)

(print "Tweaking inversion-rate")
(set-parameter *inversion-rate* 0.01)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *inversion-rate* 0.10)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *inversion-rate* 0.20)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *inversion-rate* 0.50)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *inversion-rate* *default-inversion*)

(print "Tweaking permutation-rate")
(set-parameter *permutation-rate* 0.01)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *permutation-rate* 0.10)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *permutation-rate* 0.20)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *permutation-rate* 0.50)
(benchmark-tournament 'Genetic *tournaments-per-trial*)
(set-parameter *permutation-rate* *default-permutation*)

(quit)