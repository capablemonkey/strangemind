(ql:quickload :prove)
(in-package :cl-user)
(defpackage main
  (:use :cl
        :prove))
(in-package :main)

(declaim (sb-ext:muffle-conditions cl:style-warning))

(load "test/game.lisp")
(load "test/scsa.lisp")
(load "test/genetic.lisp")