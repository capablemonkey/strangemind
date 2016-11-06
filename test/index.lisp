(ql:quickload :prove)
(in-package :cl-user)
(defpackage main
  (:use :cl
        :prove))
(in-package :main)

(declaim (sb-ext:muffle-conditions cl:style-warning))

(load (merge-pathnames "game.lisp" *load-truename*))
(load (merge-pathnames "scsa.lisp" *load-truename*))
(load (merge-pathnames "genetic.lisp" *load-truename*))
(load (merge-pathnames "utility.lisp" *load-truename*))
