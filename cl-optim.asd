;;; -*- mode: lisp -*-
(defpackage :cl-optim-system
  (:use :cl :asdf :cl-linalg))
(in-package :cl-optim-system)

(defsystem cl-optim
  :author "Arnold N'GORAN"
  :licence "LLGPL"
  :components ((:file "utils")
	       (:file "simplex"
		      :depends-on ("utils")))
  :depends-on ())
