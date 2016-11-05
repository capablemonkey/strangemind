(load (merge-pathnames "../lib/game.lisp" *load-truename*))

(subtest "Testing provided game logic"
  (is '(1 2 3) (firstn 3 '(1 2 3 4 5 6))))