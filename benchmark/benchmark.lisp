(load (merge-pathnames "../lib/game.lisp" *load-truename*))
(load (merge-pathnames "../lib/utility.lisp" *load-truename*))
(load (merge-pathnames "../lib/scsa.lisp" *load-truename*))
(load (merge-pathnames "../lib/teams.lisp" *load-truename*))

; Make sure we're setting a new random state for the PRNG:
(setf *random-state* (make-random-state t))

; Support functions
(defun average (list)
  (/ (reduce #'+ list) (float (list-length list))))

(defparameter *SCSA-list*
  '(insert-colors
    two-color
    ab-color
    two-color-alternating
    first-and-last
    usually-fewer
    prefer-fewer))
    ; only-once ; WARNING: this SCSA requires the board size to equal the number of colors

(defparameter *scsa-name* 'insert-colors)

(defun random-scsa ()
  (nth (random (length *SCSA-list*)) *SCSA-list*))

; tournament return value is (<accumulated score>, <rounds lost>, <rounds failed>)

; benchmark team against a tournament 100 times against random SCSAs:
(defun benchmark-tournament (team trials)
  (let*
    (
      (results ; return values from running tournaments
        (mapcar ; TODO: can we parallelize each tournament?
          (lambda (_) (measure-time (lambda () (play-tournament *Mastermind* team *scsa-name* 100))))
          (make-list trials)))
      (scores (mapcar (lambda (result) (scoring-function result)) results))
      (games-lost (mapcar #'second results))
      (games-failed (mapcar #'third results))
      (tournament-times (mapcar #'fourth results))
      (average-score (average scores))
      (total-played (* trials 100))
      (total-lost (reduce #'+ games-lost))
      (total-failed (reduce #'+ games-failed))
      (avg-time-per-tournament (average tournament-times))
      (avg-time-per-game (/ avg-time-per-tournament 100 1000)))
    (format t "~%*** Benchmark ~a against board with ~a colors and ~a pegs:"
      team
      (number-of-colors *Mastermind*)
      (board *Mastermind*))
    (format t "~%Total games: ~a" total-played)
    (format t "~%Total games won: ~a" (- total-played total-lost))
    (format t "~%Total games lost: ~a" total-lost)
    (format t "~%Total games with invalid guesses (!): ~a" total-failed)
    (format t "~%Average score: ~a" average-score)
    (format t "~%Average time per game: ~a ms" avg-time-per-game)
    (format t "~%")
    ; (print results)
    results))

; appends total tournament time to tournament return value
(defun measure-time (fn)
  (let ((start (get-internal-real-time)))
    (append
      (funcall fn)
      (list
        (- (get-internal-real-time) start)))))

; Run the tournament 10 times and calculate statistics on performance of team:
; (benchmark-tournament 'RandomFolks 10)

; keeping colors fixed at 6, benchmark against board size:
(defun benchmark-pegs (team min-size max-size)
  (let ((colors 6))
    (loop for pegs from min-size to max-size do
      (Mastermind pegs colors NIL)
      (benchmark-tournament team 2))))

; keeping pegs fixed at 4, benchmark against colors:

(defun benchmark-colors (team min-colors max-colors)
  (let ((pegs 4))
    (loop for colors from min-colors to max-colors do
      (Mastermind pegs colors NIL)
      (benchmark-tournament team 2))))

(defmacro set-parameter (param value)
  (format t "~%[!] Setting ~a to ~a~%" param value)
  `(defparameter ,param ,value))

; (benchmark-pegs 'RandomFolks 3 4)
; (benchmark-pegs 'Knuth 3 5)

; (benchmark-colors 'RandomFolks 6 8)
; (benchmark-colors 'Knuth 5 8)

; (benchmark-pegs 'Genetic 4 4)
; (benchmark-colors 'Genetic 5 8)