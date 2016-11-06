(load (merge-pathnames "../lib/utility.lisp" *load-truename*))
(load (merge-pathnames "../lib/genetic.lisp" *load-truename*))

(defparameter *test-colors* '(A B C D E F G))

(subtest "Testing Genetic Algorithm team"
  ; TODO: ensure each individual in initial population contains only valid colors
  ; TODO: ensure each individual in initial population is of the correct length

  (is '(A B C A B) (first-guess 5 '(a b c))
    "First guess should generate the expected first guess")

  (is
    *population-size*
    (length (initial-population 4 '(A B C D E F)))
    "Ensure initial population is of size *population-size*")

  (subtest "Test fitness function"
    (is
      3
      (fitness
        '(A B C D)
        *test-colors*
        '((A A A A) (B B B B) (G G G G))
        '((1 0) (1 0) (0 0))))

    (is
      0
      (fitness
        '(F F F F)
        *test-colors*
        '((A A A A) (B B B B) (G G G G))
        '((1 1) (1 1) (1 1)))))

  (let
    ; set some values to re-use in our tests:
    (
      (population '((A B C D) (F F F F) (G D F B)))
      (guesses '((A A F D) (B B B B) (F C C C) (A G F B)))
      (responses '((2 0) (1 0) (1 0) (2 1))))

    (is
      '(
        ((A B C D) 0.5)
        ((F F F F) 0.16666667)
        ((G D F B) 0.33333334))
      (population-by-relative-fitness
        population
        *test-colors*
        guesses
        responses)
      "population-by-relative-fitness maps individuals to their fitness relative to the rest of population")

    (ok
      (let*
        (
          (selections
            (loop
              for i from 1 to 1000
              collect (random-selection population *test-colors* guesses responses)))
          (count-abcd
            (count '(A B C D) selections :test #'equal))
          (count-ffff
            (count '(F F F F) selections :test #'equal)))
        (and
          (and
            (< count-abcd 550)
            (> count-abcd 450))
          (and
            (< count-ffff 200)
            (> count-ffff 100))))
      "random-selection selects individuals with probability based on their fitness")))
