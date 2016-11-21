; (load (merge-pathnames "utility.lisp" *load-truename*))

(defvar *guesses* nil)
(defvar *responses* nil)

; TODO: scale population size with # of pegs and # of colors
;(defparameter *population-size* 150)
;(defparameter *mutation-rate* 0.03)
;(defparameter *permutation-rate* 0.03)
;(defparameter *inversion-rate* 0.03)


(defparameter *population-size* 200)

 (defparameter *mutation-rate* 0.3)
 (defparameter *permutation-rate* 0.3)
 (defparameter *inversion-rate* 0.35)

; ******** THE FOLLOWING MUST SUM TO 1. USED FOR REPRODUCE *********
(defparameter *1-point-crossover-rate* 0.4) ; p
(defparameter *2-point-crossover-rate* 0.6) ; 1-p


(defvar *population* nil)

(defun first-guess (board colors)
  "Make a guess with as many colors as possible"
  (loop for i from 0 to (- board 1)
    for color_index = (mod i (length colors))
    collect (nth color_index colors)))

(defun initial-population (board colors)
  "Generate the initial population of size *population-size* using individuals with randomly picked colors."
  ; TODO: ensure there are no duplicate individuals
  (loop for i from 1 to *population-size*
    collect (insert-colors board colors)))

(defun consistentcy-score (individual colors guesses responses)
  (loop for guess in guesses
    for response in responses
    when (equal response (my-process-guess colors individual guess))
    sum 1))

(defun previous-guess-penalty (individual guesses)
  (loop for guess in guesses
    when (equal individual guess)
    sum -0.5))

(defun fitness (individual colors guesses responses)
  "Determines fitness of individual based on how consistent it is with past guesses and responses"
  ; For now, just a count of how many guess-response pairs it matches
  (+
    (consistentcy-score individual colors guesses responses)
    ; (previous-guess-penalty individual guesses)))
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

;BUG: CAN IF POPULATION ONLY HAS PREVIOUS GUESSES, THEN POPULATION BECOMES NIL. INVALID GUESS
(defun prune-already-guessed (population guesses)
  (set-difference population guesses :test 'equal))

(defun fittest-individual (population colors guesses responses)
  (first
    (first
      (sort
        ;(prune-already-guessed population guesses) ;WITHOUT PRUNE THERES NO MORE INVALID GUESSES
        (population-by-fitness population colors guesses responses)
        #'>
        :key #'second))))

(defun random-selection (population-by-relative-fitness)
  "Chooses a individual from the population with a bias for fitness"
  (pick-with-probability population-by-relative-fitness))

;Used by reproduce 2. and inversion.
(defun get-two-indexes (individual)
  "Generates list with 2 random indexes in individual. First is smaller than second."
  (let ((index1 (1+ (random (1- (length individual)))))
        (index2 (1+ (random (1- (length individual))))))
        (loop while (= index1 index2) do
          (setf index1 (1+ (random (1- (length individual)))))
           (setf index2 (1+ (random (1- (length individual))))))

    (if (< index1 index2)
        (list index1 index2)
        (list index2 index1))))

; TODO: We may want to tweak the crossover point as time goes on
; ADDED: Either parent can be on left or right
; 1 Point Crossover
(defun reproduce-1 (parent-a parent-b)
  "Returns a single individual offspring from two individuals"
  (let ((crossover-index (1+ (random (1- (length parent-a)))))
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
(defun reproduce-2 (parent-a parent-b)
  "Returns a single individual offspring from two individuals"
  (let ((two-index (get-two-indexes parent-a))
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
(defun reproduce-with-chance ( par1 par2)
  "Returns a single individual offspring from two individuals"
    (if (<= (random 1.0) *1-point-crossover-rate*)
        (reproduce-1 par1 par2)
        (reproduce-2 par1 par2)))

; Change a random peg to a random color
(defun mutate (colors individual)
  "Mutate the individual"
  (set-nth
    individual
    (random (1- (length individual)))
    (nth (random (length colors)) colors)))

(defun permutate (individual)
  "Permutate the individual"
  (let ((index1 (random (length individual)))
        (index2 (random (length individual))))
        (if (not (equal index1 index2))
          (rotatef (nth index1 individual) (nth index2 individual)))
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


(defun genetic-algorithm (colors guesses responses)
  "Breeds a new generation and returns its most fit individual"
  ; (print *population*)
  (let*
    (
      (current-population-by-relative-fitness (population-by-relative-fitness *population* colors guesses responses))
      (new-population
        (loop for _ in *population*
          ; TODO: should we allow parents to be identical/self?
          for parent-a = (random-selection current-population-by-relative-fitness)
          for parent-b = (random-selection current-population-by-relative-fitness)
          for child = (reproduce-with-chance parent-a parent-b)
          for possibly-mutated-child = (mutate-with-chance colors child *mutation-rate*)
          for possibly-permutated-child = (permutate-with-chance possibly-mutated-child *permutation-rate*)
          for possibly-inverted-child = (inversion-with-chance possibly-permutated-child  *inversion-rate*)
          for possibly-random-child = (rand-colors (length possibly-inverted-child) colors)
          collect (if (EQUAL (FIND possibly-inverted-child *population* :TEST #'EQUAL) NIL)
                     possibly-inverted-child
                    possibly-random-child) )))

    (setf *population* (remove-duplicates new-population :test #'equal))

    (fittest-individual new-population colors guesses responses)))

; Genetic team.  Interfaces with the game
(defun Genetic (board colors SCSA last-response)
  (declare (ignore SCSA))

  ; if first move, reset state and give a guess that includes all the colors
  (when (null last-response)
    (setf *guesses* nil)
    (setf *responses* nil)
    ; TODO: ensure population does not include first guess
    ; Population isnt exactly *population-size*
    (setf *population* (remove-duplicates (initial-population board colors) :test #'equal))
    (push (first-guess board colors) *guesses*)
    (return-from Genetic (first-guess board colors)))

  ; record last response
  (push (firstn 2 last-response) *responses*)
  ;(if (>= (length *responses*) 10)
  ;  (loop for x from 1 to 4 do (genetic-algorithm colors *guesses* *responses*)))

  ;(loop for x from 1 to 10 do (genetic-algorithm colors *guesses* *responses*))

  (let ((guess (genetic-algorithm colors *guesses* *responses*)))
    (push guess *guesses*)
    guess))
