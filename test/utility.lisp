(load (merge-pathnames "../lib/utility.lisp" *load-truename*))

(subtest "Testing utility functions"
  (is 'a (pick-with-probability '((a 1)))
    "pick-with-probability returns an item from a given tuple list")

  (ok
    (let*
      (
        (picks
          (loop
            for i from 1 to 1000
            collect (pick-with-probability '((a 0.45) (b 0.10) (c 0.15) (d 0.30)))))
        (c-count (count 'c picks))
        (d-count (count 'd picks)))
      (and
        (and
          (< c-count 200)
          (> c-count 100))
        (and
          (< d-count 350)
          (> d-count 250))))
    "pick-with-probability picks items with the correct probability")

  (is
    (k-permutations-with-repetition '(a b c) 2)
    '((A A) (A B) (A C) (B A) (B B) (B C) (C A) (C B) (C C))
    "k-permutations-with-repetition works as expected")

  (subtest "test my-process-guess"
    (is
      (my-process-guess '(a b c d e f) '(a b c d) '(f f f f))
      '(0 0))

    (is
      (my-process-guess '(a b c d e f) '(a b c d) '(a f f f))
      '(1 0))

    (is
      (my-process-guess '(a b c d e f) '(a b c d) '(f f f a))
      '(0 1))

    (is
      (my-process-guess '(a b c d e f) '(a b c d) '(f f a a))
      '(0 1))

    (is
      (my-process-guess '(a b c d e f) '(a b c d) '(a b d c))
      '(2 2))))