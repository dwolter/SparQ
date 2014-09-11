;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;; QUANTIFIER
;;; construct models for qualititative scenarios

(defpackage quantifier
  (:use :common-lisp :sparq)
  (:export quantify-scenario))

(in-package :quantifier)

(defun QUANTIFY-SCENARIO (stream calculus command scenario)
  (declare (ignore command))
  (if (calculi:calculus-quantifier calculus)
      (cond 
	;; Dedicated quantifier function
	((functionp (calculi:calculus-quantifier calculus))
	 (signal-error "Quantification not implemented yet!"))

	 ;; Algebraic
	 ((or (eq (calculi:calculus-quantifier calculus) :algebraic)
	      (calculi:calculus-algebraic-spec calculus))
	  (a-reasoning:a-model stream calculus scenario))

	 ;; Error handler
	 (t (signal-error "Cannot handle quantifier specification of type ~a." (type-of (calculi:calculus-quantifier calculus)))))
      ;; Error handler 
      (signal-error "No quantfier specified for calculus ~a." (calculi:calculus-name calculus))))



#|
(calculi:load-calculus-registry)
(calcli:load-calculus "pc")
(quantifier:quantify-scenario *standard-output* calculi:*calculus* nil '(((a < b))))

|#