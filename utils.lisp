(defpackage :cl-optim.utils
  (:use :cl)
  (:export :true :square))

(in-package :cl-optim.utils)

(defun true (arg)
  (if arg
      t
      nil))

(defun square (x)
  (* x x))

(defun nonpositive? (x)
  (<= x 0))

(defun nonegative? (x)
  (>= x 0))
