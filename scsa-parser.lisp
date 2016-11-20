; Mystery-5?  Output from sampler for insert-colors seems to match
; (defparameter *scsa-random* 0)
(defparameter *scsa-two-color* 0)
(defparameter *scsa-ab-color* 0)

; This is probably mystery-4 right?
(defparameter *scsa-two-color-alternating* 0)
(defparameter *scsa-only-once* 0)
(defparameter *scsa-first-and-last* 0)
(defparameter *scsa-usually-fewer* 0)
(defparameter *scsa-prefer-fewer* 0)

; I think mystery-1 is just 3 color alternating,
; rather than the more complicated board/2 color alternating,
; if only because she already has a 2 color alternating.
(defparameter *scsa-mystery-1* 0)

; The output pattern from the sampler for prefer-fewer seems to match
; the sample of mystery-2, but I'm still not sure how to detect it
; except for when there's an answer of all one color
(defparameter *scsa-mystery-2* 0)

; I think this is 3 color because she already has 2 color
; and 2 color alternating, so if I'm right about mystery-1
; being 3 color alternating, then this should be just 3 color.
(defparameter *scsa-mystery-3* 0)

; spot is here for debugging, but also I
; don't know if we'll have access to it when
; she runs the tournament.
(defun spot (color)
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

(defun reset-stats()
	; (setf *scsa-random* 0)
	(setf *scsa-two-color* 0)
	(setf *scsa-ab-color* 0)
	(setf *scsa-two-color-alternating* 0)
	(setf *scsa-only-once* 0)
	(setf *scsa-first-and-last* 0)
	(setf *scsa-usually-fewer* 0)
	(setf *scsa-prefer-fewer* 0)
	(setf *scsa-mystery-1* 0)
	(setf *scsa-mystery-2* 0)
	(setf *scsa-mystery-3* 0))

(defun first-last-check (peg-one peg-two)
	(if (equal peg-one peg-two)
		(incf *scsa-first-and-last*)))

(defun alternating-check (even odd)
	(if (and (equal even t) (equal odd t))
		(incf *scsa-two-color-alternating*)))

(defun ab-check (peg-a peg-b board)
	(let ((total (+ peg-a peg-b)))
		(if (= total board)
			(incf *scsa-ab-color*))))

(defun only-once-check (number)
	(if (= number 1)
		(incf *scsa-only-once*)))

(defun usually-fewer-check (num-of-colors)
	(if (or (= num-of-colors 2)
			(= num-of-colors 3))
		(incf *scsa-usually-fewer*)))

(defun two-color-check (num-of-colors)
	(if (= num-of-colors 2)
		(incf *scsa-two-color*)))

(defun mystery-1-check (is-equal)
	(if (equal is-equal t)
		(incf *scsa-mystery-1*)))

(defun mystery-2-check (num-of-colors)
	; The most obvious way of checking this
	; is checking to see if it's all 1 number,
	; but it's not a 100% guarantee
	(if (= num-of-colors 1)
		(incf *scsa-mystery-2*)))

(defun mystery-3-check (num-of-colors)
	(if (= num-of-colors 3)
		(incf *scsa-mystery-3*)))

(defun print-results()
	; (format t "~%SCSA RANDOM: ~a" *scsa-random*)
	(format t "~%SCSA TWO-COLOR: ~a" *scsa-two-color*)
	(format t "~%SCSA AB-COLOR: ~a" *scsa-ab-color*)
	(format t "~%SCSA TWO-COLOR-ALTERNATING: ~a" *scsa-two-color-alternating*)
	(format t "~%SCSA ONLY-ONCE: ~a" *scsa-only-once*)
	(format t "~%SCSA FIRST-AND-LAST: ~a" *scsa-first-and-last*)
	(format t "~%SCSA USUALLY-FEWER: ~a" *scsa-usually-fewer*)
	(format t "~%SCSA PREFER-FEWER: ~a" *scsa-prefer-fewer*)
	(format t "~%SCSA MYSTERY-1: ~a" *scsa-mystery-1*)
	(format t "~%SCSA MYSTERY-2: ~a" *scsa-mystery-2*)
	(format t "~%SCSA MYSTERY-3: ~a~%" *scsa-mystery-3*))

(defun scsa-parser (guess board colors)
	(let (
		  (peg-array (make-array colors :initial-element 0))
		  (first-peg nil)
		  (second-peg nil)
		  (third-peg nil)
		  (last-peg nil)
		  (is-equal T)
		  (mark-1 3)
		  (mark-2 4)
		  (mark-3 5)
		  (even-peg T)
		  (odd-peg T)
		  (max-num 0)
		  (num-of-zeros 0)
		  (num-of-colors 0))

		(loop for element in guess
			for index from 0 to (1- board)

			; Set 0th element of guess to first-peg
			; Accessing a char from a string like an array (aref string-guess index)
			; gives back a weird format of #\char
			; (intern (string (aref string-guess index))) turns that back into
			; a symbol
			when (= index 0)
				do (setf first-peg element)

			when (= index 1)
				do (setf second-peg element)

			when (= index 2)
				do (setf third-peg element)

			; Set last element of guess to last-peg
			when (= index (1- board))
				do (setf last-peg element)

			; Check if odd elements of guess are equal to element 1
			; If odd-peg ends up as true, all odd positions are one color
			when (and (oddp index) (equal odd-peg T))
				if (not (equal (second guess) 
								element))
					do (setf odd-peg nil)

			; Check if even elements of guess are equal to element 0
			; If even-peg ends up as true, all even positions are one color
			when (and (evenp index) (equal even-peg T))
				if (not (equal (first guess) 
								element))
					do (setf even-peg nil)

			; Pattern check for Mystery-1
			; If the three colors alternating pattern breaks,
			; we shouldn't ever enter this big block
			when (equal is-equal T)
				do (when (= mark-1 index)
					(if (not (equal element first-peg))
						(setf is-equal nil))
					(setf mark-1 (+ mark-1 3)))

				do (when (= mark-2 index)
					(if (not (equal element second-peg))
						(setf is-equal nil))
					(setf mark-2 (+ mark-2 3)))

				do (when (= mark-3 index)
					(if (not (equal element third-peg))
						(setf is-equal nil))
					(setf mark-3 (+ mark-3 3)))

			do (incf (aref peg-array (spot element))))

		; num-of-zeros represents number of colors not present in guess
		(loop for index from 0 to (1- colors)
			when (> (aref peg-array index) max-num)
				do (setf max-num (aref peg-array index))
			when (= (aref peg-array index) 0)
				do (incf num-of-zeros))

		(setf num-of-colors (- colors num-of-zeros))

		(first-last-check first-peg last-peg)

		(alternating-check even-peg odd-peg)

		(ab-check (aref peg-array 0) (aref peg-array 1) board)

		(only-once-check max-num)

		(usually-fewer-check num-of-colors)

		(two-color-check num-of-colors)

		(mystery-1-check is-equal)

		(mystery-2-check num-of-colors)

		(mystery-3-check num-of-colors)))

		; Not sure if random should always be incremented
		;(incf *scsa-random*)))
		; (format t "~%NUM-OF-COLORS: ~a" num-of-colors)
		; (format t "~%NUM-OF-ZEROS: ~a" num-of-zeros))