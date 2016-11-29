(load (merge-pathnames "utility.lisp" *load-truename*))
(load (merge-pathnames "knuth.lisp" *load-truename*))

(defvar *guesses* nil)
(defvar *responses* nil)

; TODO: scale population size with # of pegs and # of colors
(defparameter *population-size* 150)
(defparameter *mutation-rate* 0.03)
(defparameter *permutation-rate* 0.03)
(defparameter *inversion-rate* 0.03)
(defparameter *fitness-slick-weight* 1)

(defparameter *max-generations-per-guess* 100)
(defparameter *max-eligible-codes-per-guess* 100)

(defparameter *1-point-crossover-rate* 0.5) ; p
(defparameter *2-point-crossover-rate* 0.5) ; 1-p

(defun first-guess (board colors)
  "Make a guess with as many colors as possible"
  (loop for i from 0 to (- board 1)
    for color_index = (mod i (length colors))
    collect (nth color_index colors)))

(defun initial-population (board colors)
  "Generate the initial population of size *population-size* using individuals with randomly picked colors."
  ; TODO: ensure there are no duplicate individuals
  (generate-n-unique *population-size* (lambda () (insert-colors board colors))))

; Count the number of guess-response pairs that the individual is consistent with
(defun consistentcy-score (individual colors guesses responses)
  (loop for guess in guesses
    for response in responses
    when (equal response (my-process-guess colors individual guess))
    sum 1))

(defun response-similarity-score (individual colors guesses responses)
  (loop
    for guess in guesses
    for response in responses
    for response-for-individual = (my-process-guess colors individual guess)
    for diff-bulls = (abs (- (first response) (first response-for-individual)))
    for diff-cows = (abs (- (second response) (second response-for-individual)))
    for slick = (* (length individual) *fitness-slick-weight*)
    for score = (- slick diff-cows diff-bulls)
    sum score))

(defun fitness (individual colors guesses responses)
  "Determines fitness of individual based on how consistent it is with past guesses and responses"
  (+
    ; (consistentcy-score individual colors guesses responses)
    (response-similarity-score individual colors guesses responses)
    0))

(defun population-by-fitness (population colors guesses responses)
  (mapcar
    (lambda (individual) (list individual (fitness individual colors guesses responses)))
    population))

(defun population-by-relative-fitness (population colors guesses responses)
  (let*
    (
      (individuals-to-fitness (population-by-fitness population colors guesses responses))
      (sum-of-fitnesses (reduce #'+ (mapcar #'second individuals-to-fitness))))
    (mapcar
      (lambda (individual-to-fitness)
        ; for each individual, divide their fitness by the total fitness of the population
        (list (first individual-to-fitness) (safe-division (second individual-to-fitness) (float sum-of-fitnesses))))
      individuals-to-fitness)))

(defun prune-already-guessed (population guesses)
  (set-difference population guesses :test 'equal))

(defun fittest-individual (population colors guesses responses)
  (first
    (first
      (sort
        (population-by-fitness (prune-already-guessed population guesses) colors guesses responses)
        #'>
        :key #'second))))

(defun random-selection (population-by-relative-fitness)
  "Chooses a individual from the population with a bias for fitness"
  (pick-with-probability population-by-relative-fitness))

;Used by reproduce 2. and inversion.
(defun get-two-indexes (individual)
  "Generates list with 2 random indexes in individual. First is smaller than second."
  (let
    (
      (index1 (1+ (random (1- (length individual)))))
      (index2 (1+ (random (1- (length individual))))))
    (loop while (= index1 index2) do
      (setf index1 (1+ (random (1- (length individual))))))

    (if (< index1 index2)
        (list index1 index2)
        (list index2 index1))))

; TODO: We may want to tweak the crossover point as time goes on
; ADDED: Either parent can be on left or right
; 1 Point Crossover
(defun 1-point-crossover (parent-a parent-b)
  "Returns a single individual offspring from two individuals using 1-point crossover"
  (let
    (
      (crossover-index (1+ (random (1- (length parent-a)))))
      (coin-flip (random 2)))
    (if (= coin-flip 0)
      (append
        (subseq parent-a 0 crossover-index)
        (subseq parent-b crossover-index (length parent-b)))
      (append
        (subseq parent-b 0 crossover-index)
        (subseq parent-a crossover-index (length parent-b))))))

; TODO: We may want to tweak the crossover point as time goes on
; ADDED: Either parent can be on left or right
; 2 Point Crossover
(defun 2-point-crossover (parent-a parent-b)
  "Returns a single individual offspring from two individuals using 2-point crossover"
  (let
    (
      (two-index (get-two-indexes parent-a))
      (coin-flip (random 2)))
    (if (= coin-flip 0)
       (append
         (subseq parent-a 0 (first two-index))
         (subseq parent-b (first two-index) (second two-index))
         (subseq parent-a (second two-index) (length parent-a)))
       (append
         (subseq parent-b 0 (first two-index))
         (subseq parent-a (first two-index) (second two-index))
         (subseq parent-b (second two-index) (length parent-a))))))

; TODO: paper suggests .5 each. Maybe adjust rate of reproduce1, reproduce 2 based on SCSA?
(defun reproduce-with-chance (par1 par2)
  "Returns a single individual offspring from two individuals"
    (if (<= (random 1.0) *1-point-crossover-rate*)
      (1-point-crossover par1 par2)
      (2-point-crossover par1 par2)))

; Change a random peg to a random color
(defun mutate (colors individual)
  "Mutate the individual"
  (set-nth
    individual
    (random (1- (length individual)))
    (nth (random (length colors)) colors)))

(defun permutate (individual)
  "Permutate the individual"
  (let ((two-index (get-two-indexes individual)))
    (rotatef (nth (first two-index) individual) (nth (second two-index) individual))
    individual))

(defun inversion (individual)
  "Invert the individual"
  (let ((two-index (get-two-indexes individual)))
    (append
      (subseq individual 0 (first two-index))
      (reverse (subseq individual (first two-index) (second two-index)))
      (subseq individual (second two-index) (length individual)))))

; TODO: we may want the mutation rate to decrease with time, similar to simulated annealing
; Currently always mutates.
(defun mutate-with-chance (colors individual chance)
  "Choose to mutate the individual based on some probability"
  (if (<= (random 1.0) chance)
    (mutate colors individual)
    individual))

(defun permutate-with-chance (individual chance)
  "Choose to permutate the individual based on some probability"
  (if (<= (random 1.0) chance)
    (permutate individual)
    individual))

(defun inversion-with-chance (individual chance)
  "Choose to use inversion on the individual based on some probability"
  (if (<= (random 1.0) chance)
    (inversion individual)
    individual))

(defun genetic-algorithm (population colors guesses responses)
  "Given a population, return a new generation by breeding."
  (let
    ((current-population-by-relative-fitness (population-by-relative-fitness population colors guesses responses)))
    (loop for _ in population
      ; TODO: should we allow parents to be identical/self?
      for parent-a = (random-selection current-population-by-relative-fitness)
      for parent-b = (random-selection current-population-by-relative-fitness)
      for child = (reproduce-with-chance parent-a parent-b)
      for possibly-mutated-child = (mutate-with-chance colors child *mutation-rate*)
      for possibly-permutated-child = (permutate-with-chance possibly-mutated-child *permutation-rate*)
      for possibly-inverted-child = (inversion-with-chance possibly-permutated-child *inversion-rate*)
      collect
        (if (not (equal (find possibly-inverted-child population :test #'equal) nil))
          (rand-colors (length possibly-inverted-child) colors)
          possibly-inverted-child))))

(defun eligible-p (individual colors guesses responses)
  ; TODO: can use `every` function which may be more efficient than map
  (equal (length guesses) (consistentcy-score individual colors guesses responses)))

(defun eligible-individuals (population colors guesses responses)
  (remove-if-not
    (lambda (individual) (eligible-p individual colors guesses responses))
    population))

(defun generate-eligible-codes (board colors guesses responses)
  (let
    (
      (eligible-codes nil)
      (population (initial-population board colors)))
    (loop
      with generation-counter = 0
      until
        (or
          (>= (length eligible-codes) *max-eligible-codes-per-guess*)
          (>= generation-counter *max-generations-per-guess*))
      do
        (progn
          (setf population (genetic-algorithm population colors guesses responses))
          (setf eligible-codes
            (remove-duplicates
              (append
                eligible-codes
                (eligible-individuals population colors guesses responses))
              :test 'equal)))
      do (incf generation-counter 1))
    (format t "~%Eligible codes: ~a" (length eligible-codes))
    (if (> (length eligible-codes) *max-eligible-codes-per-guess*)
      (subseq eligible-codes 0 *max-eligible-codes-per-guess*)
      eligible-codes)))

; Genetic team.  Interfaces with the game
(defun Genetic (board colors SCSA last-response)
  (declare (ignore SCSA))

  ; if first move, reset state and give a guess that includes all the colors
  (when (null last-response)
    (setf *guesses* nil)
    (setf *responses* nil)
    ; TODO: ensure population does not include first guess
    (push (first-guess board colors) *guesses*)
    (return-from Genetic (first-guess board colors)))

  ; record last response
  (push (firstn 2 last-response) *responses*)

  (let*
    (
      (eligible-codes (generate-eligible-codes board colors *guesses* *responses*))
      (guess (maximize-minimum-codes-eliminated colors eligible-codes eligible-codes)))
      ; (guess (fittest-individual (genetic-algorithm (initial-population board colors) colors *guesses* *responses*) colors *guesses* *responses*)))
    (push guess *guesses*)
    guess))
