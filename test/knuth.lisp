(load (merge-pathnames "../lib/utility.lisp" *load-truename*))
(load (merge-pathnames "../lib/knuth.lisp" *load-truename*))

(defparameter *test-colors* '(A B C D E F))

(subtest "Knuth's algorithm"
  (is
    '(A A B B)
    (Knuth 4 '(A B C D E F) nil nil)
    "Knuth's first guess should always be AABB for 4,6")

  (is
    1
    (score-guess
      *test-colors*
      '(A A A A)
      '((B B B B) (C C C C) (D D D D) (A B C D)))
    "score-guess returns the minimum number of codes the guess would eliminate")

  (is
    3
    (score-guess
      *test-colors*
      '(A B C D)
      '((A B B B) (A B C C) (D D D D) (A B C D)))
    "score-guess returns the minimum number of codes the guess would eliminate")

  (is
    '(
      ((A A A A) 0)
      ((A B F F) 1))
    (score-guesses
      *test-colors*
      '((A A A A) (A B F F))
      '((B B B B) (F F F F)))
    "score-guesses returns a list of tuples with guesses and their scores")

  (is
    '(A B C D)
    (maximize-minimum-codes-eliminated
      *test-colors*
      '((A A A A) (B B B B) (A B C D))
      '((C C C C) (D C B A) (C D C D)))
    "maximize-minimum-codes-eliminated should return the guess which maximizes the minimum number of eliminated codes"))