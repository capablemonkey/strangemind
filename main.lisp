(load "lib/game.lisp")
(load "lib/scsa.lisp")
(load "lib/teams.lisp")

(Mastermind 3 3 'only-once)
(play-tournament *Mastermind* 'RandomFolks 'two-color-alternating 1)