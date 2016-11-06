(load (merge-pathnames "../lib/scsa.lisp" *load-truename*))

(subtest "Testing SCSAs"
  (ok
    (listp (insert-colors 4 '(A B C D E F)))
    "insert-colors should return a list")

  (is 4 (length (insert-colors 4 '(A B C D E F)))
    "insert-colors should return a the correct number of colors"))