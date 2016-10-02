;;;*******************************************************************************************
;;Sample SCSAs
;;;*******************************************************************************************

;to see what codes from a specific SCSA look like, use this function
;for example (scsa-sampler 100 'first-and-last 6 8) will give 100 samples for 6 pegs with 8 colors using the SCSA first-and-last
;collects and returns number of secret codes generated by SCSA for pegs pegs and colors colors
(defun SCSA-sampler (number SCSA pegs colors)
  (loop for i from 1 to number
       collect (funcall SCSA pegs (firstn colors '(A B C D E F G H I J K L M N O P Q R S TT U V W X Y Z)))))

;makes a list of length length containing colors selected at random from colors
(defun insert-colors (length colors)
  (loop for i from 1 to length
     collect (random-chooser colors)))

;makes a 2-color list 
(defun two-color (length colors)
  (loop with choices = (choose-n-random 2 colors)
     for i from 1 to length 
     collect (random-chooser choices)))

;makes a list of only As and Bs
(defun ab-color (length colors)
  (declare (ignore colors))
  (loop with choices = '(A B)
     for i from 1 to length
     collect (random-chooser choices)))

;makes a list that alternates 2 colors
(defun two-color-alternating (length colors)
  (loop with choices = (choose-n-random 2 colors) 
     with first-color = (first choices)
     with second-color = (second choices)
     for i from 1 to length 
     when (oddp i) 
     collect first-color
     else collect second-color))  

;makes a list in which a color appears at most once
(defun only-once (length colors)
  (if (< (length colors) length) 
      (break)
      (loop for i from 1 to length
   for color-list = (copy-list colors) then (set-difference color-list (list choice))
   for choice = (random-chooser color-list)
   collect choice)))

;makes a list in which the first and last colors are the same
(defun first-and-last (length colors)
  (loop with first = (random-chooser colors)
       for i from 2 to (1- length)
       collect (random-chooser colors) into ans
       finally (return (append (cons first ans) (list first)))))

;makes a list that usually has fewer (2 or 3) colors
(defun usually-fewer (length colors)
  (let* ((probability (random 100))
   (coin-flip (when (< probability 90) (random-chooser '(2 3))))
   (choices (cond (coin-flip (if (= 3 coin-flip) (choose-n-random 3 colors) (choose-n-random 2 colors)))
      (t colors))))
    (loop for i from 1 to length
       collect (random-chooser choices))))

;makes a list with preferences for fewer colors
(defun prefer-fewer (length colors)
  (let* ((probability (random 100))
   (color-count (length colors))
   (coin-flip (cond ((<= probability 49) 1)
          ((<= probability 74) 2)
          ((<= probability 87) (if (>= color-count 3) 3 color-count))
          ((<= probability 95) (if (>= color-count 4) 4 color-count))
          ((<= probability 98) (if (>= color-count 5) 5 color-count))
          (t (if (>= color-count 6) (random-chooser (loop for i from 6 to color-count 
                   collect i)) color-count))))
   (choices (choose-n-random coin-flip colors))) 
    (loop for i from 1 to length
       collect (random-chooser choices))))

;;Support functions
;selects a random element from a list
(defun random-chooser (list)
  (nth (random (length list)) list))

;selects n distinct random elements from list)
(defun choose-n-random (n list)
  (if (> n (length list))
      (print 'error)
      (loop for i from 1 to n
   for choices = (copy-list list) then (set-difference choices (list chosen))
   for chosen = (random-chooser choices)
   collect chosen)))
