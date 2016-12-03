(load "lib/game.lisp")
(load "lib/scsa.lisp")
(load "lib/teams.lisp")

; Make sure we're setting a new random state for the PRNG:
(setf *random-state* (make-random-state t))

(Mastermind 8 10 'two-color-alternating)

(setf (answer *Mastermind*) (insert-colors (board *Mastermind*) (colors *Mastermind*)))

; (play-round *Mastermind* 'Knuth)
; (play-round *Mastermind* 'Genetic)

; (play-tournament *Mastermind* 'RandomFolks 'two-color-alternating 100)
(play-tournament *Mastermind* 'Genetic 'two-color-alternating 10)