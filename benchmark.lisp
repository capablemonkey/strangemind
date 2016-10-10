(load "lib/game.lisp")
(load "lib/scsa.lisp")
(load "lib/teams.lisp")

; Make sure we're setting a new random state for the PRNG:
(setf *random-state* (make-random-state t))

(defun average (list)
  (/ (reduce #'+ list) (float (list-length list))))

; tournament return value is (<accumulated score>, <rounds lost>, <rounds failed>)

(defun benchmark-tournament (team trials)
  ; TODO: add list of SCSAs and apply one at random for each trial...

  (Mastermind 3 3 'only-once)

  (let*
    ((results ; return values from running tournaments
      (mapcar
        (lambda (_) (play-tournament *Mastermind* team 'two-color-alternating 100))
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
    ))

; Run the tournament 10 times and calculate statistics on performance of team:
(benchmark-tournament 'RandomFolks 10)

; TODO: Benchmark against board size
; TODO: Benchmark against colors
; TODO: Benchmark against SCSAs
