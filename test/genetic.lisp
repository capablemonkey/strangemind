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

  (is
    '(((a) (1)) ((b) (2)))
    (guess-response-pairs '((a) (b)) '((1) (2)))
    "guess-response-pairs should zip 2 lists into one")

  (is
    '(
      (((A B C D) (2 1)) 4.0)
      (((E F G H) (0 5)) 5.0))
    (score-pairs
      '(
        ((A B C D) (2 1))
        ((E F G H) (0 5))))
    "score-pairs returns the 'helpfulness' score of each guess-response tuple")

  (is
    '(
      (((D D D D) (3 1)) 5.5)
      (((B B B B) (2 2)) 5.0))
    (top-n-guess-response-pairs
      '(
        (A A A A)
        (B B B B)
        (C C C C)
        (D D D D))
      '(
        (1 0)
        (2 2)
        (0 1)
        (3 1))
      2)
    "top-n-guess-response-pairs returns the n highest scoring guess-response pairs")

  (is
    '(A B C D)
    (Genetic 4 '(A B C D E F) nil nil)
    "Genetic on first turn returns a guess which uses as many colors as possible")

  (is
    4
    (progn
      (setf *guesses* '((A B C D)))
      (length
        (Genetic 4 '(A B C D E F) nil '(1 0 1))))
    "Genetic returns a guess of the correct length when it's not the first turn")

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
        ((A B C D) 15.0)
        ((F F F F) 12.0)
        ((G D F B) 12.0))
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
      '((A B C D) 15.0)
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
