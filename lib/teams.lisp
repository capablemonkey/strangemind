(load "lib/knuth.lisp")

;;;*******************************************************************************************
;;Sample teams
;;;*******************************************************************************************
;this is a really dumb team... it makes random guesses
(defun RandomFolks (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
  (insert-colors board colors))

;this isn't much better.... it guesses all the same color, and chooses that color at random
(defun Boring (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
    (make-list board :initial-element (random-chooser colors)))

