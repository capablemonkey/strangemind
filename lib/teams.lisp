
;;;*******************************************************************************************
;;Sample teams
;;;*******************************************************************************************
;this is a really dumb team... it makes random guesses
(defun RandomFolks (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
  (insert-colors board colors))

;this isn't much better.... it guesses all the same color, and chooses that color at random
(defun Boring (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
    (make-list board :initial-element (random-chooser colors)))

;
; Knuth's algorithm
;

(defparameter *knuth-remaining-codes* nil)
(defparameter *knuth-past-guesses* nil)

(defun k-permutations-with-repetition (n-list k)
  (cond ((equal 0 k) (list nil))
        ((null n-list) nil)
        ((null (cdr n-list)) (list n-list))
        (t (loop for element in n-list
             append (mapcar (lambda (l) (cons element l))
                            (k-permutations-with-repetition n-list (- k 1)))))))

(defun possible-codes (board colors)
  (k-permutations-with-repetition colors board))

(defun first-guess (board colors)
  (loop
    for i from 1 to board
    when (<= i (/ board 2))
    collect (first colors)
    else
    collect (second colors)))

(defun my-color-counter (number-of-colors list)
  (loop with tally = (make-array number-of-colors :initial-element 0)
     for peg in list
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))

(defun my-process-guess (number-of-colors answer guess)
  (loop
     with guess-color-count = (my-color-counter number-of-colors guess)
     with true-color-count = (my-color-counter number-of-colors answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- number-of-colors)
              for guessed = (aref true-color-count i)
              for true = (aref guess-color-count i)
              when (<= true guessed)
              sum true
              else sum guessed)))))

(defun my-map-hash (func hash)
  (loop for k being the hash-keys in hash using (hash-value v)
    collect (funcall func k v)))

(defun get-values (hash)
  (my-map-hash (lambda (k v) v) hash))

(defun maximum (list)
  (loop for element in list maximizing element))

(defun score-guess (number-of-colors guess remaining-codes)
  (let
    ((tally (make-hash-table))
    (max-hit 0))
    (loop
      for code in remaining-codes
      for response = (my-process-guess number-of-colors code guess)
      do
      (setf (gethash response tally) (+ 1 (gethash response tally 0))))

    (setf max-hit (maximum (get-values tally)))
    (- (length remaining-codes) max-hit)))

(defun Knuth (board colors SCSA last-response)
  (declare (ignore SCSA))

  ; if this is a new game: reset state and always guess 1122
  (when (null last-response)
    (setf *knuth-remaining-codes* (possible-codes board colors))
    (setf *knuth-past-guesses* nil)
    (push (first-guess board colors) *knuth-past-guesses*)
    (return-from Knuth (first-guess board colors)))

  ; remove from possible codes the codes which would not have generated this response for the last guess
  (setf *knuth-remaining-codes*
    (remove-if-not
      (lambda (code)
        ; (FORMAT T "~%~a with guess ~a gives ~a vs ~a"
        ;   code
        ;   (first *knuth-past-guesses*)
        ;   (my-process-guess (length colors) code (first *knuth-past-guesses*))
        ;   (firstn 2 last-response))
        (equal
          (firstn 2 last-response)
          (my-process-guess (length colors) code (first *knuth-past-guesses*))))
      *knuth-remaining-codes*))

  ; (print (length *knuth-remaining-codes*))

  (let*
    ; list of tuples (<guess> <score of guess>)
    ((poss-guesses-with-scores
      (mapcar
        (lambda (guess)
            (list guess (score-guess (length colors) guess *knuth-remaining-codes*)))
        ; TODO: use set of all possible codes instead of remaining codes because
        ; the ideal guess may not be one of the remaining codes.
        *knuth-remaining-codes*))
    ; filter out guesses we've made before
    (past-guesses-filtered
      (remove-if
        (lambda (guess-with-score)
          (member (first guess-with-score) *knuth-past-guesses* :test 'equal))
        poss-guesses-with-scores))
    ; find the tuple with the best score
    (guess-with-score-max
      (first
        (sort past-guesses-filtered #'> :key #'second)))
    (guess (first guess-with-score-max)))

    (push guess *knuth-past-guesses*)
    guess))
