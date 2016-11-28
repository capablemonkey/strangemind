;;;
;;; SCSA test Equals
;;;
		
		
;;Generate list of all possible SCSA for the current board
;;compare answers agaisnt that SCSA


	 

;;TODO Convert the checkers into predicates so we can use them to determine our guess follows the SCSA



;;These are the buckets for our SCSAs

(defparameter *mystery-1* 0)

(defparameter *mystery-2* 0)

(defparameter *mystery-3* 0)

(defparameter *mystery-4* 0)

(defparameter *mystery-5* 0)

(defparameter *2-color-SCSA* 0)

(defparameter *AB-SCSA* 0)

(defparameter *2-color-alt-SCSA* 0)

(defparameter *at-most-once*  0)

(defparameter *first-last* 0)

(defparameter *less-than-three* 0)

(defparameter *preference-fewer* 0)

(defparameter *random-SCSA* 0)

;;Patterns
;;;

;;Global Variable that stores our converted guess to be checked with other SCSA
;;We store it so we don't have to keep converting the same thing over and over again
;;
(defparameter *converted-to-generic* nil)


;;;
;;;Mystery code 1 is just the same 3 symbols repeated over and over again 
;;;SCSA would be (0 1 2 0 1 2 0) for a 7 size board
;;
(defparameter *mystery-1-pattern* nil)

;;; Mystery code 4 is 2 elements repeated one after another
;;; SCSA could be (0 1 0 1 0 1 0)
;;;This stores the pattern
(defparameter *mystery-4-pattern* nil)



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
       finally (setf *converted-to-generic* local-converted-guess))) 
	
		




;;Creates the pattern for Mystery-1
(defun mystery-1 (board)
  (setf *mystery-1-pattern* (loop for i below board collect (mod i 3))))

(defun mystery-1-checker-p (guess)
  (if (equal *mystery-1-pattern* guess) T ))
  
  



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

;;TODO need to maybe do a defparameter for colors-in-guess and sorted-count
(defparameter colors-in-guess nil)
(defparameter sorted-count nil)



(defun mystery-3-checker-p (guess)
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
		T)))))



   ;; check if (length (remove-duplicate guess)) == 3
   ;; if true check guess length mod 3
   ;;if = 0 then count 0 == count 1 == count 2
   ;;if = 1 then one of them is larger
   ;;if = 2 then one of them is smaller

   ;;make a list of the count of each element, sort list in order,
   ;;when you sort it, it will be either 1 greater and or 1 lesser
   ;;mod board 3 == 1 then  2 2 3, first == second && third == first - 1
   ;;mod board 3 == 2 then 2 3 3, second == third, && third == first + 1









;(defun mystery-4 (board)
;  (loop
;     with SCSA-code = (make-list board :initial-element 1)   
;     for i from 0 below board by 2
;     do (setf (nth i SCSA-code) 0)
;     finally (setf *mystery-4* SCSA-code)))



;;Build Mystery-4-pattern
(defun mystery-4 (board)
  (setf *mystery-4-pattern* (loop for i below board collect (mod i 2))))

(defun mystery-4-checker-p (guess)
  (if (equal *mystery-4-pattern* guess) T))


       
       
  

;;;
;;;Mystery code 5 
;;;


;;;SCSA from the SCSA LISP file



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
;;(defun preference-fewer-checker-p (guess)
;;  ( if (<= (length (remove-duplicates guess)) 6) T))
  


;;Resets all our stats related to scsa
(defun reset-SCSA-stats ()
  (progn (setf *mystery-1* 0)
	 (setf *mystery-2* 0)
	 (setf *mystery-3* 0)
	 (setf *mystery-4* 0)
	 (setf *mystery-5* 0)
	 (setf *2-color-SCSA* 0)
	 (setf *AB-SCSA* 0)
	 (setf *2-color-alt-SCSA* 0)
	 (setf *at-most-once* 0)
	 (setf *first-last* 0)
	 (setf *less-than-three* 0)
	 (setf *preference-fewer* 0)
	 (setf *converted-to-generic* nil)
	 (setf *mystery-1-pattern* nil)
	 (setf *mystery-4-pattern* nil)))




;;To see if the functions work properly
(defun SCSA-Tester (guess board)
  (progn (mystery-1 board)
	 (mystery-4 board)
	 (convert-guess guess)
	 (format t "~%Mystery-1-checker-p: ~a" (mystery-1-checker-p *converted-to-generic*))
	 (format t "~%Mystery-3-checker-p: ~a" (mystery-3-checker-p guess))
	 (format t "~%Mystery-4-checker-p: ~a" (mystery-4-checker-p *converted-to-generic*))
	 (format t "~%2-color-checker-p: ~a" (2-color-checker-p guess))
	 (format t "~%AB-checker-p: ~a" (AB-checker-p guess))
	 (format t "~%2-color-alt-checker-p: ~a" (2-color-alt-checker-p guess))
	 (format t "~%at-most-once-checker-p: ~a" (at-most-once-checker-p guess))
	 (format t "~%first-last-checker-p: ~a" (first-last-checker-p guess))
	 (format t "~%less-then-three-checker-p: ~a" (less-than-three-checker-p guess))))


;;SCSA-Matcher assumes you have mystery-1 and mystery-4 patterns set
;;TODO add paramater, guessNumber which will be used for the 3 ratio code as the increment number.
(defun SCSA-Matcher (guess)
  (progn (convert-guess guess)
	 (if (mystery-1-checker-p *converted-to-generic*) (setf *mystery-1* (+ *mystery-1* 1)))
	 (if (mystery-3-checker-p guess) (setf *mystery-3* (+ *mystery-3* 1)))
	 (if (mystery-4-checker-p *converted-to-generic*) (setf *mystery-4* (+ *mystery-4* 1)))
	 (if (2-color-checker-p guess) (setf *2-color-SCSA* (+ *2-color-SCSA* 1)))
	 (if (AB-checker-p guess) (setf *AB-SCSA* (+ *AB-SCSA* 1)))
	 (if (2-color-alt-checker-p guess) (setf *2-color-alt-SCSA* (+ *2-color-alt-SCSA* 1)))
	 (if (at-most-once-checker-p guess) (setf *at-most-once* (+ *at-most-once* 1)))
	 (if (first-last-checker-p guess) (setf *first-last* (+ *first-last* 1)))
	 (if (less-than-three-checker-p guess) (setf *less-than-three* (+ *less-than-three* 1)))
	 (setf *random-SCSA* (+ *random-SCSA* .75))))


(defun display-buckets ()
  (progn (format t "~% *mystery-1*: ~a" *mystery-1*)
	 (format t "~% *mystery-2*: ~a" *mystery-2*)
	 (format t "~% *mystery-3*: ~a" *mystery-3*)
	 (format t "~% *mystery-4*: ~a" *mystery-4*)
	 (format t "~% *mystery-5*: ~a" *mystery-5*)
	 (format t "~% *2-color-SCSA*: ~a" *2-color-SCSA*)
	 (format t "~% *AB-SCSA*: ~a" *AB-SCSA*)
	 (format t "~% *2-color-alt-SCSA*: ~a" *2-color-alt-SCSA*)
	 (format t "~% *at-most-once*: ~a" *at-most-once*)
	 (format t "~% *first-last*: ~a" *first-last*)
	 (format t "~% *less-than-three*: ~a" *less-than-three*)
	 (format t "~% *preference-fewer*: ~a" *preference-fewer*)
	 (format t "~% *random-SCSA*: ~a" *random-SCSA*)))
	 








;;Make a list of n colors with 0 in each position
;;call color-feq it increments the list with the number of colors in it.

(defparameter *solution-color* nil)

(defun set-solution-color (colors)
  (setf *solution-color* (make-list colors :initial-element 0)))


(defun color-feq (guess)
  (setf (nth (- (length (remove-duplicates guess)) 1) *solution-color*)
	(+ (nth (- (length (remove-duplicates guess)) 1) *solution-color*) 1)))

(defparameter *freq-divider* 1)

(defun set-freq-divider ()
  (setf *freq-divider* (apply #'+ *solution-color*)))

(defparameter *solution-color-freq* nil)

(defun make-color-freq-list ()
  (setf *solution-color-freq*
	(loop
	   for x in *solution-color*
	   collect (float (/ x freq-divider)))))


(defparameter *pref-fewer-ratio* '(0.5 0.25 0.13 0.08 0.03 0.01))

(defparameter *mystery-2-ratio* '(0.45 0.39 0.04 0.08 0.04))

(defparameter *mystery-5-ratio* '(0.0 0.06 0.45 0.49 0.0))


(defparameter *pref-fewer-diff* 0.0)

(defparameter *mystery-2-diff* 0.0)

(defparameter *mystery-5-diff* 0.0)


(defun find-diff-pref-fewer ()
  (setf *pref-fewer-diff*
	(loop
	   for x in *solution-color-freq*
	   for y in *pref-fewer-ratio*
	   sum (abs (- x y)))))

(defun find-diff-mystery-2 ()
  (setf *mystery-2-diff*
	(loop
	   for x in *solution-color-freq*
	   for y in *mystery-2-ratio*
	   sum (abs (- x y)))))

(defun find-diff-mystery-5 ()
  (setf *mystery-5-diff*
	(loop
	   for x in *solution-color-freq*
	   for y in *mystery-5-ratio*
	   sum (abs (- x y)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Game Number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *game-number* 0)



;;This should be called whenever we check previous guess = nil so we know we're in a new game
(defun game-number-incrementer ()
  (progn (when (>= *game-number* 100)
	   (setf *game-number* 0))
	 (setf *game-number* (+ *game-number* 1))))

;;call reset function when games = 1


;;make tuples of name of scsa and confidence


 ;*mystery-1* *mystery-2*  *mystery-3*  *mystery-4*  *mystery-5*  *2-color-SCSA* *AB-SCSA*  *2-color-alt-SCSA* 
 ;*at-most-once*  

; *first-last*
 ;*less-than-three* 

 ;*preference-fewer*
 ;*random-SCSA* 

;order for subsets
;AB-list, mystery-4, alternate-2colors, 2-color-list  

;mystery-1, mystery3, , prefer fewer,

;mystery-2, mystery-5, at-most-once, first-last, less-than-three, random



(defparameter *SCSA-confidence* nil)

(defun get-confidence ()
  (let ((sum (+  *mystery-1*
		 *mystery-2*
		 *mystery-3*
		 *mystery-4*
		 *mystery-5*
		 *2-color-SCSA*
		 *AB-SCSA*
		 *2-color-alt-SCSA*
		 *at-most-once*
		 *first-last*
		 *less-than-three*
		 *preference-fewer*
		 *random-SCSA*))
	(confidence nil))
    (setf confidence (list (cons 'AB-checker-p (list (float (/ *AB-SCSA* sum))))
			   (cons 'mystery-4-checker-p (list (float (/ *mystery-4* sum))))
			   (cons '2-color-alt-checker-p (list (float (/ *2-color-alt-SCSA* sum))))
			   (cons '2-color-checker-p (list (float (/ *2-color-SCSA* sum))))
			   (cons 'mystery-1-checker-p (list (float (/ *mystery-1* sum))))
			   (cons 'mystery-3-checker-p (list (float (/ *mystery-3* sum))))
			   (cons '*preference-fewer* (list (float (/ *preference-fewer* sum))))
			   (cons '*mystery-2* (list (float (/ *mystery-2* sum))))
			   (cons '*mystery-5* (list (float (/ *mystery-5* sum))))
			   (cons '*at-most-once-checker-p (list (float (/ *at-most-once* sum))))
			   (cons 'first-last-checker-p (list (float (/ *first-last* sum))))
			   (cons 'less-than-three-checker-p (list (float (/ *less-than-three* sum))))
			   (cons '*random-SCSA* (list (float (/ *random-SCSA* sum))))))
    (setf *SCSA-confidence* confidence)))
			   







