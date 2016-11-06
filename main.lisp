(load "lib/game.lisp")
(load "lib/scsa.lisp")
(load "lib/teams.lisp")

; Make sure we're setting a new random state for the PRNG:
(setf *random-state* (make-random-state t))

(Mastermind 4 6 nil)

(setf (answer *Mastermind*) (insert-colors (board *Mastermind*) (colors *Mastermind*)))

; (play-round *Mastermind* 'Knuth)
(play-round *Mastermind* 'Genetic)

; (play-tournament *Mastermind* 'RandomFolks 'two-color-alternating 100)
; (play-tournament *Mastermind* 'Knuth 'two-color-alternating 100)