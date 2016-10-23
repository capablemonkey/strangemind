(load "lib/game.lisp")
(load "lib/scsa.lisp")
(load "lib/teams.lisp")

; Make sure we're setting a new random state for the PRNG:
(setf *random-state* (make-random-state t))

(Mastermind 4 6 nil)

; (play-tournament *Mastermind* 'RandomFolks 'two-color-alternating 100)
; (play-tournament *Mastermind* 'Knuth 'two-color-alternating 100)

(setf (answer *Mastermind*) (two-color-alternating (board *Mastermind*) (colors *Mastermind*)))

(play-round *Mastermind* 'Knuth)