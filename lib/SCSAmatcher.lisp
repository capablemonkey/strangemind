;;;
;;; SCSA-Matcher
;;;
		

; TODO: ADD random?
(defun matches-scsa (scsa-name code)
  (cond
    (
      (equal scsa-name 'two-color)
      (if (2-color-checker-p code) 1 0))
    (
      (equal scsa-name 'prefer-fewer)
     (score-prefer-fewer code))
    (
     (equal scsa-name 'ab-color)
     (if (AB-checker-p code) 1 0))
    (
     (equal scsa-name 'two-color-alternating)
     (if (2-color-alt-checker-p code) 1 0))
    (
     (equal scsa-name 'only-once)
     (if (at-most-once-checker-p code) 1 0))
    (
     (equal scsa-name 'first-and-last)
     (if (first-last-checker-p code) 1 0))
    (
     (equal scsa-name 'usually-fewer)
     (if (less-than-three-checker-p code) 1 0))
    (
     (equal scsa-name 'mystery-1)
     (if (mystery-1-checker-p code) 1 0))
    (
     (equal scsa-name 'mystery-2)
     (score-mystery-2 code))
    (
     (equal scsa-name 'mystery-3)
     (if (mystery-3-checker-p code) 1 0))
    (
     (equal scsa-name 'mystery-4)
     (if (myster-4-checker-p code) 1 0))
    (
     (equal scsa-name 'mystery-5)
     (score-mystery-5 code))   
    (t 0)))

; TODO: finish me
(defun score-prefer-fewer (code)
  (let ((number-of-colors (length (remove-duplicates code))))
    (cond
      ((= number-of-colors 1) 0.5)
      ((= number-of-colors 2) 0.25)
      ((= number-of-colors 3) 0.13)
      ((= number-of-colors 4) 0.08)
      ((= number-of-colors 5) 0.03)
      ((= number-of-colors 6) 0.01)
      (t 0))))

(defun score-mystery-2 (code)
  (let ((number-of-colors (length (remove-duplicates code))))
    (cond
      ((= number-of-colors 1) 0.45)
      ((= number-of-colors 2) 0.39)
      ((= number-of-colors 3) 0.04)
      ((= number-of-colors 4) 0.08)
      ((= number-of-colors 5) 0.04)
      ((= number-of-colors 6) 0.00)
      (t 0))))

(defun score-mystery-5 (code)
  (let ((number-of-colors (length (remove-duplicates code))))
    (cond
      ((= number-of-colors 1) 0.0)
      ((= number-of-colors 2) 0.06)
      ((= number-of-colors 3) 0.45)
      ((= number-of-colors 4) 0.49)
      ((= number-of-colors 5) 0.00)
      ((= number-of-colors 6) 0.00)
      (t 0))))


;;
;;Converts our guess into something we can compare to a SCSA
;;ie: (B B A C E D A E ) converts to (0 0 1 2 3 4 1 3)
(defun convert-guess (guess)
  (loop
     with counter = 0
     with my-hash = (make-hash-table)
     with local-converted-guess = nil
     for letter in guess
     do (cond
	  ((equal local-converted-guess nil)(progn (setf (gethash letter my-hash) counter)
						   (setf local-converted-guess (make-list 1 :initial-element counter))
						   (setf counter (+ 1 counter))))
	  ((gethash letter my-hash)(setf local-converted-guess (append local-converted-guess (list(gethash letter my-hash)))))
	  (T (progn (setf (gethash letter my-hash) counter)
		    (setf counter (+ 1 counter))
		    (setf local-converted-guess (append local-converted-guess (list (gethash letter my-hash)))))))
       finally (return local-converted-guess))) 
	

;;Creates the pattern for Mystery-1
(defun mystery-1-create-pattern (board)
 (loop for i below board collect (mod i 3)))

(defun mystery-1-checker-p (guess)
  (let ((mystery-1-pattern nil) (converted-guess nil))
    (setf mystery-1-pattern (mystery-1-create-pattern (length guess)))
    (setf converted-guess (convert-guess guess))
    (if (equal mystery-1-pattern converted-guess) T)))
	

;;;
;;;Mystery code 2
;;;looks like prefer fewer?

;;;
;;;Mystery code 3 has the patterns 2 of 2 elements and 3  of a third
;;; Might also be 3 elements with even distribution
;;;SCSA could be (0 1 0 2 1) on a board of size 5


;;;count the number of each element in our guess
;;; (mod board 3) == 0 then each number should be equal
;;; (mod board 3) == 1 only 1 element should have 1 greater
;;; (mod board 3) == 2 only 2 elements should have 1 greater

   ;; check if (length (remove-duplicate guess)) == 3
   ;; if true check guess length mod 3
   ;;if = 0 then count 0 == count 1 == count 2
   ;;if = 1 then one of them is larger
   ;;if = 2 then one of them is smaller

   ;;make a list of the count of each element, sort list in order,
   ;;when you sort it, it will be either 1 greater and or 1 lesser
   ;;mod board 3 == 1 then  2 2 3, first == second && third == first - 1
   ;;mod board 3 == 2 then 2 3 3, second == third, && third == first + 1



(defun mystery-3-checker-p (guess)
  (let ((colors-in-guess nil) (sorted-count nil))
    (if (equal (length (remove-duplicates guess)) 3)
      (progn (setq colors-in-guess (remove-duplicates guess))
	     (setq sorted-count (sort (list
				       (count (first colors-in-guess) guess)
				       (count (second colors-in-guess) guess)
				       (count (third colors-in-guess) guess)) #'<))
	     (cond
	       ((and (equal (mod (length guess) 3) 0)
		     (and (equal (first sorted-count)
				 (second sorted-count))
			  (equal (first sorted-count)
				 (third sorted-count))))
		T)
	       ((and (equal (mod (length guess) 3) 1)
		     (equal (first sorted-count) (second sorted-count))
		     (equal (third sorted-count) (+ (first sorted-count) 1)))
		T)
	       ((and (equal (mod (length guess) 3) 2)
		     (equal (second sorted-count) (third sorted-count))
		     (equal (third sorted-count) (+ (first sorted-count) 1)))
		T))))))


;;Build Mystery-4-pattern
(defun mystery-4-create-pattern (board)
  (loop for i below board collect (mod i 2)))

(defun mystery-4-checker-p (guess)
  (let ((mystery-4-pattern nil) (converted-guess nil))
    (setf mystery-4-pattern (mystery-4-create-pattern (length guess)))
    (setf converted-guess (convert-guess guess))
    (if (equal mystery-4-pattern converted-guess) T)))

;;2-color list
;; remove duplicates from guess, if is length 2 then this is true
(defun 2-color-checker-p (guess)
  (if (equal (length (remove-duplicates guess)) 2) T))

;;list of only AB
;; remove duplicates from guess if the list is length 2 and only has A and B this is true
(defparameter result nil)

(defun AB-checker-p (guess)
  (progn(setq result (remove-duplicates guess))
	(if (and (equal (length result) 2) (member 'A result) (member 'B result)) T)))

;;list of alternate 2 colors
;;Use mystery-4 code

(defun 2-color-alt-checker-p (guess)
  (if (equal *mystery-4-pattern* guess) T))


;;a list in which colors appear at most once
;;remove duplicates in a guess and see if length changes, if not then true
(defun at-most-once-checker-p (guess)
  (if (equal (length guess) (length (remove-duplicates guess))) T))


;;first and last are same
;;just check first and last of guess to be equal
(defun first-last-checker-p (guess)
  (if (equal (first guess) (last guess)) T))


;;Fewer colors (2 or 3)
;;remove duplicates from guess and check length if lenght is <= 3 then true
(defun less-than-three-checker-p (guess)
  (if (<= (length (remove-duplicates guess)) 3) T))

;;makes a list with preference for fewer colors
;; 50% chance to have 1 color
;; 25% chance to have 2 colors
;; 13% chance to have 3 colors
;; 8% chance to have 4 colors
;; 3% chance to have 5 colors
;; 1% chance to have 6 colors
