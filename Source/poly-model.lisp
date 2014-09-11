;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; algebraic-reasoning.lisp
;;;
;;; Defines a package for handling SparQ commands for reasoning about
;;; multivirate polynomial algebra

;; Change history (most recent first):
;; 2007-10-29 DW  documentation
;; 2007-10-26 DW  started this file

(defpackage :poly-model
  (:use :common-lisp :sparq)
  (:export :equation-variables :derivative :compute-model))

(in-package :poly-model)

;; returns the list of all variables occuring in a set of polynomial equations
(defun equation-variables (poly)
  (remove-duplicates (mapcar #'first (apply #'append (mapcar #'second poly)))))

;; utility function for partial derivative: a term's partial derivative
(defun term-deriv (term v)
  (let* ((f (first term))
	 (monom (second term))
	 (v (find v monom :key #'first)))
    (if v
	(let ((rest-monom (remove v monom))
	      (e (second v)))
	  (if (= e 1)
	      (list (list f rest-monom))
	      (list (list (* f e) (substitute (list (first v) (- e 1)) v monom :test #'equal)))))
	())))

;; computes partial derivative d p / d v
;;
;; ARGUMENTS:
;; poly polynomial
;; v    variable (symbol)
;;
;; RETURN VALUES:
;; partial derivative (polynomial)
(defun partial-derivative (poly v)
  (reduce #'(lambda (poly term)
	      (poly:add poly (term-deriv term v)))
	  poly
	  :initial-value ()))

;; computes derivative of polynomial
;;
;; AGRUMENTS:
;; poly      polynomial
;; all-vars  list of all symbols occuring in the polynomial
;;
;; RETURN VALUES:
;; list of partial derivatives (polynomials), position of variable in argument 'all-vars'
;; gives position of its partial derivative in returned list
(defun derivative (poly all-vars)
  (let ((d (make-list (length all-vars) :initial-element nil))
	(count 0))
    (dolist (v all-vars)
      (let ((pd (partial-derivative poly v)))
	(when pd
	  (setf (nth count d) (poly::add (nth count d) pd))))
      (incf count))
    d))

;; computes the value of a polynomial
;;
;; ARGUMENTS:
;; poly       polynomial (as defined in polynomials package)
;; var-assoc  assoc list of variables (symbols) occuring in poly and their values
;;
;; RETURN VALUES:
;; value (type number)
(defun poly-value (poly var-assoc)
  (reduce #'(lambda (sum term)
	      (+ sum (* (first term)
			(reduce #'(lambda (p m)
				    (* p (expt (cdr (assoc (first m) var-assoc)) (second m))))
				(second term)
				:initial-value 1))))
	  poly
	  :initial-value 0))

;; computes a model (i.e. a variable assignment) that suits the model equations that are compiled
;; (e.g. by adding them) into a single polynomial model-eqn
;;
;; ARGUMENTS:
;; stream     where to print result
;; model-eqn  polynomial equation to minimize
;; deriv      derivative of model-eqn
;; vars       list of all variables (symbols) occuring in model-eqn
;; state      assoc list of variables and their values
;;
;; RETURN VALUES:
;; 1st value: updated state
;; 2nd value: residual error
(defun compute-model (stream model-eqn deriv vars state &key (max-steps 100))
  (let ((state-vector (make-array (length vars)))
	(n (length vars))
	(i 0))
    ;;(print vars)
    ;;(print state)
    (labels ((unfold-vec (state)
	       (loop for x across state for v in vars collecting (cons v x)))
	     (val-fun (v)
	       (poly-value model-eqn (unfold-vec v)))
	     (deriv-fun (v)
	       (let ((dvec (make-array n))
		     (state (unfold-vec v))
		     (i 0))
		 (dolist (d deriv)
		   (setf (aref dvec i)  (poly-value d state)
			 i (+ i 1)))
		 dvec)))	 
      (dolist (v vars)
	(setf (aref state-vector i) (cdr (assoc v state))
	      i (+ i 1)))
      (let* ((result-state (optimizer:gradient-descent #'val-fun #'deriv-fun state-vector :steps max-steps :info nil))
	     (unfolded-result-state (unfold-vec result-state))
	     (v (poly-value model-eqn unfolded-result-state)))
	;;(print unfolded-result-state)
	(values unfolded-result-state v)))))

