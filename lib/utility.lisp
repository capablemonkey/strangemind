; Helper functions which teams require:

(defun k-permutations-with-repetition (n-list k)
  (cond ((equal 0 k) (list nil))
        ((null n-list) nil)
        ((null (cdr n-list)) (list n-list))
        (t (loop for element in n-list
             append (mapcar (lambda (l) (cons element l))
                            (k-permutations-with-repetition n-list (- k 1)))))))

(defun my-color-counter (colors list)
  (loop with tally = (make-array (length colors) :initial-element 0)
     for peg in list
     for index = (spot peg)
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
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- (length colors))
              for guessed = (aref true-color-count i)
              for true = (aref guess-color-count i)
              when (<= true guessed)
              sum true
              else sum guessed)))))