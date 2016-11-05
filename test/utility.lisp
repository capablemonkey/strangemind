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
    "pick-with-probability picks items with the correct probability"))