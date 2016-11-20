;;;
;;; SCSA test Equals
;;;
		
		
;;Generate list of all possible SCSA for the current board
;;compare answers agaisnt that SCSA


	 




;;These are the buckets for our SCSAs

(defparameter *mystery-1* 0)

(defparameter *mystery-2* 0)

(defparameter *mystery-3* 0)

(defparameter *mystery-4* 0)

(defparameter *mystery-5* 0)

(defparameter *2-color-SCSA* 0)

(defparameter *AB-SCSA* 0)

(defparameter *at-most-once*  0)

(defparameter *first-last* 0)

(defparameter *less-than-three* 0)

(defparameter *preference-fewer* 0)


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

(defun mystery-1-checker (guess)
  (if (equal *mystery-1-pattern* guess) (setf *mysytery-1* (+ 1 *mystery-1*))))
  
  



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


;;ERROR here can't use Equal to check all 3, Equal can only check 2 at a time.
(defun mystery-3-checker (guess)
  (if (equal (length (remove-duplicates guess)) 3)
      (progn (setq colors-in-guess (remove-duplicates guess))
	     (setq sorted-count (sort (list
				       (count (first colors-in-guess) guess)
				       (count (second colors-in-guess) guess)
				       (count (third colors-in-guess) guess)) #'<))
	     (cond
	       ((and (equal (mod (length guess) 3) 0)
		     (equal (first sorted-count)
			    (second sorted-count)
			    (third sorted-count)))
		(setf *mystery-3* (+ 1 *mystery-3*)))
	       ((and (equal (mod (length guess) 3) 1)
		     (equal (first sorted-count) (second sorted-count))
		     (equal (third sorted-count) (- (first sorted-count) 1)))
		(setf *mystery-3* (+ 1 *mystery-3*)))
	       ((and (equal (mod (length guess) 3) 2)
		     (equal (second sorted-count) (third sorted-count))
		     (equal (third sorted-count) (+ (first sorted-count) 1)))
		(setf *mystery-3* (+ 1 *mystery-3*)))))))



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

(defun mystery-4-checker (guess)
  (if (equal *mystery-4-pattern* guess) (setf *mystery-4* (+ 1 *mystery-4*))))


       
       
  

;;;
;;;Mystery code 5 
;;;


;;;SCSA from the SCSA LISP file



;;2-color list
;; remove duplicates from guess, if is length 2 then this is true
(defun 2-color-checker (guess)
  (if (equal (length (remove-duplicates guess)) 2)(setf *2-color-SCSA* (+ 1 *2-color-SCSA*))))




;;list of only AB
;; remove duplicates from guess if the list is length 2 and only has A and B this is true
(defun AB-checker (guess)
  ((setq result (remove-duplicates guess))
   (if (and (equal (length result) 2) (member A result) (member B result)) (setf *AB-SCSA* (+ 1 *AB-SCSA*)))))
     


;;list of alternate 2 colors
;;Use mystery-4 code



;;a list in which colors appear at most once
;;remove duplicates in a guess and see if length changes, if not then true
(defun at-most-once-checker (guess)
  (if (equal (length guess) (length (remove-duplicates guess))) (setf *at-most-once* (+ 1 *at-most-once*))))




;;first and last are same
;;just check first and last of guess to be equal
(defun first-last-checker (guess)
  (if (equal (first guess) (last guess))(setf *first-last* (+ 1 *first-last*))))



;;Fewer colors (2 or 3)
;;remove duplicates from guess and check length if lenght is <= 3 then true
(defun less-than-three-checker (guess)
  (if (<= (length (remove-duplicates guess)) 3) (setf *less-than-three* (+ 1 *less-than-three* 0))))



;;makes a list with preference for fewer colors
;; 50% chance to have 1 color
;; 25% chance to have 2 colors
;; 13% chance to have 3 colors
;; 8% chance to have 4 colors
;; 3% chance to have 5 colors
;; 1% chance to have 6 colors
(defun preference-fewer-checker (guess)
  ( if (<= (length (remove-duplicates guess)) 6) (setf *preference-fewer* (+ 1 *preference-fewer*))))



;;Flawed right now
;;Need to determine whether this is a new tournament or old.
;;last-response is a guess or a 3 tuple?
(defun SCSA-guesser (board colors SCSA last-response)
  (progn (when (null last-response)
	   (reset-SCSA-stats)
	   (convert-guess last-response)
	   (mystery-1 board)
	   (mystery-4 board))
	 (mystery-1-checker *converted-to-generic*)
	 (mystery-3-checker last-response)
	 (mystery-4-checker last-response)
	 (2-color-checker last-response)
	 (AB-Checker last-response)
	 (at-most-once-checker last-response)
	 (first-last-checker last-response)
	 (less-than-three-checker last-response)
	 (preference-fewer-checker last-response)))


;;Resets all our stats related to scsa
(defun reset-SCSA-stats ()
  (progn (setf *mystery-1* 0)
	 (setf *mystery-2* 0)
	 (setf *mystery-3* 0)
	 (setf *mystery-4* 0)
	 (setf *mystery-5* 0)
	 (setf *2-color-SCSA* 0)
	 (setf *AB-SCSA* 0)
	 (setf *at-most-once* 0)
	 (setf *first-last* 0)
	 (setf *less-than-three* 0)
	 (setf *preference-fewer* 0)
	 (setf *converted-to-generic* nil)
	 (setf *mystery-1-pattern* nil)
	 (setf *mystery-4-pattern* nil)))
