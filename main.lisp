(load "mastermind.lisp")

(Mastermind 3 3 'only-once)
(play-tournament *Mastermind* 'RandomFolks 'two-color-alternating 1)