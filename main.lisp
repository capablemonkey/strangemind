(load "lib/game.lisp")
(load "lib/scsa.lisp")
(load "lib/teams.lisp")

; Make sure we're setting a new random state for the PRNG:
(setf *random-state* (make-random-state t))

(Mastermind 3 3 'only-once)
(play-tournament *Mastermind* 'RandomFolks 'two-color-alternating 100)


