(load (merge-pathnames "utility.lisp" *load-truename*))

(defvar *guesses* nil)
(defvar *responses* nil)

(defparameter *population-size* 10)
(defparameter *mutation-rate* 0.05)
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

(defun fitness (individual colors guesses responses)
  "Determines fitness of individual based on how consistent it is with past guesses and responses"
  ; For now, just a count of how many guess-response pairs it matches
  (loop for guess in guesses
    for response in responses
    when (equal response (my-process-guess colors individual guess))
    sum 1))

(defun population-by-relative-fitness (population colors guesses responses)
  (let*
    (
      (individuals-to-fitness
        (mapcar
          (lambda (individual) (list individual (fitness individual colors guesses responses)))
          population))
      (sum-of-fitnesses (reduce #'+ (mapcar #'second individuals-to-fitness))))
    (mapcar
      (lambda (individual-to-fitness)
        ; for each individual, divide their fitness by the total fitness of the population
        (list (first individual-to-fitness) (safe-division (second individual-to-fitness) (float sum-of-fitnesses))))
      individuals-to-fitness)))

(defun random-selection (population colors guesses responses)
  "Chooses a individual from the population with a bias for fitness"
  (pick-with-probability (population-by-relative-fitness population colors guesses responses)))

(defun reproduce (parent-a parent-b)
  "Returns a single individual offspring from two individuals")

(defun mutate (individual)
  "Mutate the individual")

(defun mutate-with-chance (individual chance)
  "Choose to mutate the individual based on some probability")

(defun genetic-algorithm (colors guesses responses)
  "Breeds a new generation and returns its most fit individual"
  (let
    ((new-population
      (loop for _ in *population*
        for parent-a = (random-selection *population* colors guesses responses)
        for parent-b = (random-selection *population* colors guesses responses)
        for child = (reproduce parent-a parent-b)
        for possibly-mutated-child = (mutate-with-chance child *mutation-rate*)
        collect possibly-mutated-child)))
    (setf *population* new-population)

    ; TODO: sort new population by fitness and pick most fit
    (first new-population)))

; Genetic team.  Interfaces with the game
(defun Genetic (board colors SCSA last-response)
  (declare (ignore SCSA))

  ; if first move, reset state and give a guess that includes all the colors
  (when (null last-response)
    (setf *guesses* nil)
    (setf *responses* nil)
    ; TODO: ensure population does not include first guess
    (setf *population* (initial-population board colors))
    (push (first-guess board colors) *guesses*)
    (return-from Genetic (first-guess board colors)))

  ; record last response
  (push last-response *responses*)

  (let ((guess (genetic-algorithm colors *guesses* *responses*)))
    (push guess *guesses*)
    guess))
