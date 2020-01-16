(defpackage :cl-optim.simplex
  (:use :cl :cl-linalg)
  (:export (defpackage :cl-linalg.matrix
  (:nicknames :matrix)
  (:use :cl :cl-linalg.utils)
  (:export :defmatrix :matrix-p
	   :matrix+ :matrix* :matrix-map :matrix-loop
	   :matrix-aref :matrix-setf :matrix-flatten
	   :matrix-inverse :gauss-elimination))

(in-package :cl-optim.simplex)

(defun solve-std-form (A c m n)
  "Given the augmented matrix of constraints and right-hand side `A', the vector of the coefficients of the objective of variables `n', return the optimal solution and value. 
It is assumed that the problem as already been put in STANDARD FORM, that is, all the constraints are in EQUALITY form and that the `m' first columns are reserved for basic variables and form an identity matrix. These variables are actually `slack' variables."
  (let* ((tbl-nc (+ m n 2))
	 (tbl-nr (+ m 1))
	 (An (defmatrix (make-array (list tbl-nr tbl-nc))))
	 (x (defmatrix (make-array (list (+ n m) 1))))
	 (basic-var (defmatrix (make-array (list (1+ m) 1))))
	 (obj-lst nil)
	 (x-lst nil))
    (labels ((stop? ()
	       (let ((c (matrix-aref An :row 0 :col '(1))))
		 (format t "c: ~A" c)
		 (matrix-reduce #'(lambda (x y) (and x y))
				(matrix-map #'nonpositive? c)))))
      (matrix-setf An A :row 1 :col 1)
      (matrix-setf An -1 :row 0 :col 0)
      (print "0")
      (matrix-setf An c :row 0 :col tbl-nr)
      (print "1")
      (matrix-setf x (matrix-aref An :row '(1) :col (1- tbl-nc))
		   :row 0 :col 0)
      (print "2")
      (dotimes (i m)
	(matrix-setf basic-var (+ 1 i) :row (1+ i) :col 0))
      (print "3")
      ;;(matrix-setf An basic-var :row 1 :col 0)
      (push 0 obj-lst)
      (push x x-lst)
      (format t "~&Tableau:~%~A~%" (matrix-augment basic-var An))
      (format t "~&Solution:~%~A~%" x)
      (format t "~&Obj value: 0~%")
      (do ((i 0 (incf i)))
	  ((stop?) (values x-lst obj-lst))
	;; Determination of the new pivot
	(multiple-value-bind (max-val max-idx)
	    (matrix-max (matrix-aref An :row 0 :col (list 0 tbl-nc)))
	  (declare (ignore max-val))
	  (let* ((pivot-col-idx (cdr max-idx))
		 (pivot-col (matrix-aref An :row '(1) :col
					 pivot-col-idx))
		 (rhs (matrix-aref An :row '(1) :col (1- tbl-nc)))
		 (rhs-vs-pivot (matrix/ rhs pivot-col)))
	    (print "4")
	    (multiple-value-bind (min-val min-idx)
		(matrix-min rhs-vs-pivot :test #'plusp)
	      (declare (ignore min-val))
	      (let* ((pivot-row-idx (car min-idx))
		     (x (defmatrix (make-array (list (+ n m) 1)))))
		(print "5")
		;; Update the basic vars entries
		(matrix-setf basic-var pivot-col-idx
			     :row (1+ pivot-row-idx) :col 0)
		(print "6")
		(print An)
		(format t "pivot row: ~A | col: ~A" pivot-row-idx
			pivot-col-idx)
		;; Perform the pivoting
		(nullify-coeff An (1+ pivot-row-idx) pivot-col-idx)
		(print "7")
		(print basic-var)
		(print An)
		;; Save the solution
		(dotimes (i m)
		  (matrix-setf x (matrix-aref An :row (1+ i)
					      :col (1- tbl-nc))
			       :row (1- (matrix-aref basic-var
						     :row (1+ i)
						     :col 0))
			       :col 0))
		(format t "~&Tableau:~%~A~%"
			(matrix-augment basic-var An))
		(format t "~&Solution:~%~A~%" x)
		(format t "~&Obj value: ~A~%"
			(matrix-aref An :row 0 :col (1- tbl-nc)))
		(push x x-lst)
		(push (matrix-aref An :row 0 :col (1- tbl-nc))
		      obj-lst)))))))))
	      
	    
