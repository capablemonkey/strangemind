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
    "score-guess returns the minimum number of codes the guess would eliminate"))