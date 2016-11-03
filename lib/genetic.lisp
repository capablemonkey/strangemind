(defun fitness (individual guesses responses))

(defvar *guesses* nil)
(defvar *responses* nil)

(defun first-guess (board colors)
  "Make a guess with as many colors as possible"
  (loop for i from 0 to (- board 1)
    for color_index = (mod i (length colors))
    collect (nth color_index colors)))

; Genetic team.  Interfaces with the game
(defun Genetic (board colors SCSA last-response)
  (declare (ignore SCSA))

  ; if first move, reset state and give a guess that includes all the colors
  (when (null last-response)
    (setf *guesses* nil)
    (setf *responses* nil)
    (push (first-guess board colors) *guesses*)
    (return-from Genetic (first-guess board colors)))

  ; record last response
  (push last-response *responses*))
