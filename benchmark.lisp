(load "lib/game.lisp")
(load "lib/scsa.lisp")
(load "lib/teams.lisp")

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
    prefer-fewer
    ; only-once ; WARNING: this SCSA requires the board size to equal the number of colors
    ))

(defun random-scsa ()
  (nth (random (length *SCSA-list*)) *SCSA-list*))

; tournament return value is (<accumulated score>, <rounds lost>, <rounds failed>)

; benchmark team against a tournament 100 times against random SCSAs:
(defun benchmark-tournament (team trials)
  (let*
    ((results ; return values from running tournaments
      (mapcar
        (lambda (_) (play-tournament *Mastermind* team (random-scsa) 100))
        (make-list trials)))
      (scores (mapcar #'first results))
      (games-lost (mapcar #'second results))
      (games-failed (mapcar #'third results))
      (average-score (average scores))
      (total-played (* trials 100))
      (total-lost (reduce #'+ games-lost))
      (total-failed (reduce #'+ games-failed)))
    (format t "~%Total games: ~a" total-played)
    (format t "~%Total games won: ~a" (- total-played total-lost))
    (format t "~%Total games lost: ~a" total-lost)
    (format t "~%Total games with invalid guesses (!): ~a" total-failed)
    (format t "~%Average score: ~a" average-score)
    ; (print results)
    results
    ))

; Run the tournament 10 times and calculate statistics on performance of team:
; (benchmark-tournament 'RandomFolks 10)

; TODO: Benchmark against colors
; TODO: Benchmark against SCSAs

; keeping colors fixed at 6, benchmark against board size:
(defun benchmark-boards (team min-size max-size)
  (let ((colors 6))
    (loop for pegs from min-size to max-size do
      (Mastermind pegs colors NIL)
      (format t "~%*** Benchmark against board with ~a colors and ~a pegs:" colors pegs)
      (benchmark-tournament team 100))))

(benchmark-boards 'RandomFolks 3 10)
