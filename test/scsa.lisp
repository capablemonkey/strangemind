(load "lib/scsa.lisp")

(plan 2)

(ok (listp (insert-colors 4 '(A B C D E F))))
(is 4 (length (insert-colors 4 '(A B C D E F))))

(finalize)