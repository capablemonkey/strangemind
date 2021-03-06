; Helper functions which teams require:

(defun k-permutations-with-repetition (n-list k)
  (cond ((equal 0 k) (list nil))
        ((null n-list) nil)
        ((null (cdr n-list)) (list n-list))
        (t (loop for element in n-list
             append (mapcar (lambda (l) (cons element l))
                            (k-permutations-with-repetition n-list (- k 1)))))))

(defun my-spot (color)
  (case color
    (A 0)
    (B 1)
    (C 2)
    (D 3)
    (E 4)
    (F 5)
    (G 6)
    (H 7)
    (I 8)
    (J 9)
    (K 10)
    (L 11)
    (M 12)
    (N 13)
    (O 14)
    (P 15)
    (Q 16)
    (R 17)
    (S 18)
    (TT 19)
    (U 20)
    (V 21)
    (W 22)
    (X 23)
    (Y 24)
    (Z 25)))

(defun safe-division (x y)
  "Does not raise an exception if dividing by zero, returns 0 instead"
  (if (= y 0)
    0.0
    (/ x y)))

(defun my-color-counter (colors list)
  (loop with tally = (make-array (length colors) :initial-element 0)
     for peg in list
     for index = (my-spot peg)
     do (incf (aref tally index))
     finally (return tally)))

(defun my-process-guess (colors answer guess)
  (loop
     with guess-color-count = (my-color-counter colors guess)
     with true-color-count = (my-color-counter colors answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact
     do (incf exact-counter)
     and do (decf (aref guess-color-count (my-spot entry)))
     and do (decf (aref true-color-count (my-spot entry)))
     finally
      (return
        (list
          exact-counter
          (loop
            for i from 0 to (1- (length colors))
            for guessed = (aref true-color-count i)
            for true = (aref guess-color-count i)
            when (<= true guessed)
            sum true
            else sum guessed)))))

; Usage:
; (setf *random-state* (make-random-state t))
; (print (pick-with-probability '((a 0.45) (b 0.10) (c 0.15) (d 0.30))))
(defun pick-with-probability (items-with-probabilties)
  "Weighted random choice from a list of tuples (item probability)"
  (let ((random-number (random 1.0)))
    (loop
      for tuple in items-with-probabilties
      for probability = (second tuple)
      summing probability into running-sum
      when (>= running-sum random-number)
      do (return-from pick-with-probability (first tuple))

      ; if probability threshold never met, always pick something:
      finally (return-from pick-with-probability (first tuple)))))

(defun set-nth (list n val)
  "Return a copy of list where the nth element is val"
  (loop for i from 0 for j in list collect (if (= i n) val j)))

(defun generate-n-unique (n generator)
  (let ((collection (remove-duplicates (loop for i from 1 to n collect (funcall generator)))))
    (loop
      until (= n (length collection))
      for appendage = (loop for i from 1 to (- n (length collection)) collect (funcall generator))
      do (setf collection (remove-duplicates (append collection appendage))))
    collection))

;makes a list of length length containing colors selected at random from colors
(defun rand-colors (length colors)
  (loop for i from 1 to length
     collect (random-chooser colors)))

(defun random-chooser (list)
  (nth (random (length list)) list))

(defun my-firstn (number list)
  (loop for i from 1 to number
     for item in list
     collect item))

(defun random-from-1-to (n)
  "Returns a random number between 1 and n, inclusive"
  (1+ (random (1- n))))

(defun coin-flip ()
  "Returns T or NIL at random"
  (= 0 (random 2)))
