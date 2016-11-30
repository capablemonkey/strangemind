;
; Knuth's algorithm
;

(defparameter *knuth-remaining-codes* nil)
(defparameter *knuth-past-guesses* nil)

(defun possible-codes (board colors)
  (k-permutations-with-repetition colors board))

(defun first-guess (board colors)
  (loop
    for i from 1 to board
    when (<= i (/ board 2))
    collect (first colors)
    else
    collect (second colors)))

(defun my-map-hash (func hash)
  (loop for k being the hash-keys in hash using (hash-value v)
    collect (funcall func k v)))

(defun get-values (hash)
  (my-map-hash (lambda (k v) v) hash))

(defun maximum (list)
  (loop for element in list maximizing element))

(defun score-guess (colors guess remaining-codes)
  (let
    ((tally (make-hash-table :test #'equal)))
    (loop
      for code in remaining-codes
      for response = (my-process-guess colors code guess)
      do
      (setf (gethash response tally) (+ 1 (gethash response tally 0))))
    (- (length remaining-codes) (maximum (get-values tally)))))

(defun filter-past-guesses (codes past-guesses)
  (remove-if
    (lambda (code) (member code past-guesses :test 'equal))
    codes))

; list of tuples (<guess> <score of guess>)
(defun score-guesses (colors guesses remaining-codes)
  (mapcar
    (lambda (guess)
      (list guess (score-guess colors guess remaining-codes)))
    guesses))

(defun guess-with-score-max (tuples)
  (first
    (first
      (sort tuples #'> :key #'second))))

(defun maximize-minimum-codes-eliminated (colors potential-guesses remaining-codes)
  (guess-with-score-max
    (score-guesses
      colors
      potential-guesses
      remaining-codes)))

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
        (equal
          (firstn 2 last-response)
          (my-process-guess colors code (first *knuth-past-guesses*))))
      *knuth-remaining-codes*))

  (let
    ((guess
      (maximize-minimum-codes-eliminated
        colors
        ; TODO: use set of all possible codes instead of remaining codes because
        ; the ideal guess may not be one of the remaining codes.
        (filter-past-guesses *knuth-remaining-codes* *knuth-past-guesses*)
        *knuth-remaining-codes*)))

    (push guess *knuth-past-guesses*)
    guess))
