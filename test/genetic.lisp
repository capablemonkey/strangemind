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

  (is
    ; 4 + 4 + 4
    12
    (response-similarity-score
      '(A B C D)
      *test-colors*
      '((A A A A) (B B B B) (G G G G))
      '((1 0) (1 0) (0 0)))
    "response-similarity-score should return a correct score")

  (is
    ; 4 + 3 + 0
    7
    (response-similarity-score
      '(A B C D)
      *test-colors*
      '((A A A A) (B B B B) (G G G G))
      '((1 0) (0 0) (4 0)))
    "response-similarity-score should return a correct score")

  (is
    4
    (length (1-point-crossover '(A A A A) '(F F F F)))
    "1-point-crossover returns offspring of the correct length")

  (ok
    (let ((child (1-point-crossover '(A B C D) '(E F G H))))
      (or
        (equal child '(A F G H))
        (equal child '(A B G H))
        (equal child '(A B C H))))
    "1-point-crossover returns a valid child")

  (is
    4
    (length (mutate *test-colors* '(A B C D)))
    "mutate returns a mutated individual of the same length")

  (is
    4
    (length (mutate-with-chance *test-colors* '(A B C D) 0.50))
    "mutate-with-chance returns an individual of the correct length")

  (let
    ; set some values to re-use in our tests:
    (
      (population '((A B C D) (F F F F) (G D F B)))
      (guesses '((A A F D) (B B B B) (F C C C) (A G F B)))
      (responses '((2 0) (1 0) (1 0) (2 1)))
      (scsa-name 'insert-colors)
      (pop-by-relative-fitness
        '(
          ((A B C D) 0.5)
          ((F F F F) 0.16666667)
          ((G D F B) 0.33333334))))

    (is
      '(
        ((A B C D) 15)
        ((F F F F) 12)
        ((G D F B) 12))
      (population-by-fitness
        population
        *test-colors*
        guesses
        responses
        scsa-name)
      "population-fitness maps individuals to their fitness")

    (is
      '(
        ((A B C D) 0.3846154)
        ((F F F F) 0.30769232)
        ((G D F B) 0.30769232))
      (population-by-relative-fitness
        population
        *test-colors*
        guesses
        responses
        scsa-name)
      "population-by-relative-fitness maps individuals to their fitness relative to the rest of population")

    (is
      '((A B C D) 15)
      (fittest-individual
        population
        *test-colors*
        guesses
        responses
        scsa-name)
      "fittest-individual returns the tuple with the most fit individual and its score e.g. ((A B C D) 39)")

    (is
      (length population)
      (length (genetic-algorithm population *test-colors* guesses responses scsa-name))
      "genetic-algorithm returns a new population of the same size as the input population")

    (ok
      (let*
        (
          (selections
            (loop
              for i from 1 to 1000
              collect (random-selection pop-by-relative-fitness)))
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
