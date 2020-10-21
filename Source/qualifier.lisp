;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006-2010 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; SparQ relation qualification
;;;

;; Change history (most recent first):
;; 2011-03-14 DW  added support for polygons
;; 2010-04-29 DW  started migration to new command architecture
;; 2006-10-26 DW  initial version for SparQ V0.7

(defpackage :qualifier
  (:use :common-lisp :sparq :calculi :constraint-reasoning)
  (:export :qualify :parse-entity :scene :scene-specification :scene-objects))

(in-package :qualifier)


;;; 
;;; THE FOLLOWING SHOULD BECOME PROPER SPARQ PRIMITIVES !!!
;;; 

(defgeneric parse-entity (entity it)
  (declare (ignore entity it)))

(defmethod parse-entity (entity it) ; catchall
  (signal-error "cannot parse entity '~a'" entity))

(defmethod parse-entity ((what (eql :polygon)) it)
  (if (and (consp it)
	   (symbolp (first it))
	   (every #'(lambda (p)
		      (and (listp p)
			   (= (length p) 2)
			   (realp (first p))
			   (realp (second p))))
		  (cdr it)))
      it
      (signal-error "Specification '~a' is not of the required type polygon (identifier (x1 y1) (x2 y2) ... (xn yn))" it)))

(defmethod parse-entity ((what (eql :1d-point)) it)
  (if (or (not (consp it))
	  (nthcdr 2 it)
	  (not (symbolp (car it)))
	  (not (realp (second it))))
      (signal-error "Specification '~a' is not of the required type 1d point, i.e. (identifier x)~%" it)
      it))

(defmethod parse-entity ((what (eql :2d-point)) it)
  (if (or (not (consp it))
	  (nthcdr 3 it)
	  (not (symbolp (car it)))
	  (not (realp (second it)))
	  (not (realp (third it))))
      (signal-error "Specification '~a' is not of the required type 2d point, i.e. (identifier x y)~%" it)
      it))

(defmethod parse-entity ((what (eql :3d-point)) it)
  (if (or (not (consp it))
	  (nthcdr 4 it)
	  (not (symbolp (car it)))
	  (not (realp (second it)))
	  (not (realp (third it))))
	  (not (realp (fourth it))))
      (signal-error "Specification '~a' is not of the required type 3d point, i.e. (identifier x y z)~%" it)
      it)

(defmethod parse-entity ((what (eql :interval)) it)
  (if (or (not (consp it))
	  (nthcdr 3 it)
	  (not (symbolp (car it)))
	  (not (realp (second it)))
	  (not (realp (third it))))
      (signal-error "Specification '~a' is not of the required type point, i.e. (identifier a b)~%" it)
      it))

(defmethod parse-entity ((what (eql :dipole)) it)
  (if (or (not (consp it))
	  (nthcdr 5 it)
	  (not (symbolp (car it)))
	  (not (realp (second it)))
	  (not (realp (third it)))
	  (not (realp (fourth it)))
	  (not (realp (fifth it))))
      (signal-error "Specification '~a' is not of the required type dipole, i.e. (identifier start-x start-y end-x end-y)~%" it)
      it))

(defmethod parse-entity ((what (eql :2d-box)) it)
  (if (or (not (consp it))
	  (nthcdr 5 it)
	  (not (symbolp (car it)))
	  (not (realp (second it)))
	  (not (realp (third it)))
	  (not (realp (fourth it)))
	  (not (realp (fifth it))))
      (signal-error "Specification '~a' is not of the required type 2d-box, i.e. (identifier x-min y-min x-max y-max)~%" it)
      it))

(defmethod parse-entity ((what (eql :disc)) it)
  (if (or (not (consp it))
	  (nthcdr 4 it)
	  (not (symbolp (car it)))
	  (not (realp (second it)))
	  (not (realp (third it)))
	  (not (realp (fourth it))))
      (signal-error "Specification '~a' is not of the required type disc, i.e. (identifier center-x center-y radius)~%" it)
      it))

(defmethod parse-entity ((what (eql :2d-oriented-point)) it)
  (if (or (not (consp it))
	  (nthcdr 5 it)
	  (not (symbolp (car it)))
	  (not (realp (second it)))
	  (not (realp (third it)))
	  (not (realp (fourth it)))
	  (not (realp (fifth it))))
      (signal-error "Specification '~a' is not of the required type dipole, i.e. (identifier start-x start-y dir-x dir-y)~%" it)
      it))

(defclass scene (sparq:primitive)
  ((specification :reader scene-specification
		  :initarg :specification)
   (objects :reader scene-objects
	    :initarg :objects)))

(defmethod parse-primitive ((s (eql 'scene)) expr &key calculus)
  (let ((b       (calculi:calculus-basis-entity calculus))  ; entity type of calculus, e.g., 2d-point, interval
	(objs    ()) ; list of objects in scene
	(doubles ()) ; list of objects defined twice: signal error if any
	(expr    (read-from-string (format nil "~a" expr)))) ; scene description
    (if (null b)
	(cons :FAIL (format nil "no basis entities defined for calculus ~a" calculus))
	(if (listp expr)
	    (let ((err (catch 'error (progn (mapc #'(lambda (x) 
						      (parse-entity b x)   ; check that entity "x" is formatted as expected for type "b"
						      (let ((o (first x))) ; name of entity is always first in list
							(if (find o objs :key #'first)
							    (pushnew x doubles)
							    (push x objs))))
						  expr)
					    (when doubles
					      (signal-error "Scene specification contains multiple definitions of object~P '~a'~{, '~a'~}" (length doubles) (first doubles) (rest doubles)))
					    ()))))
	      (if err
		  (cons :FAIL err)
		  (make-instance 'scene 
				 :specification expr
				 :objects objs)))
	    (cons :FAIL (format nil "No scene given (list expected, but ~a found)" (type-of expr)))))))

(defmethod print-object ((s scene) stream)
  (format stream "~a" (scene-specification s)))


;; Dummy declaration to make SBCL happy
(defgeneric qualify-scene (arity option calculus scene)
  (declare (ignore arity option calculus scene)))


(defmethod qualify-scene :before (arity option calculus scene)
  (declare (ignore arity option scene))
  (unless (calculi::calculus-qualifier calculus)
    (signal-error "No qualifier function supplied")))

(defmethod qualify-scene ((arity (eql :binary)) (option (eql :first2all)) calculus s)
  (let* ((scenario (scene-specification s))
	 (o1 (first scenario))
	 (b (calculi:calculus-basis-entity calculus))
	 (rel-rep (calculi:calculus-relation-representation calculus))
	 (qfun (calculi::calculus-qualifier calculus)))
    (mapc #'(lambda (x)
	      (parse-entity b x))
	  scenario) ; Error checking
    (make-instance 'constraint-network 
		   :constraints (mapcar #'(lambda (o2)
					    (make-constraint (car o1) 
							     (ofunc:ofuncall (relations:relation-representation-encoder rel-rep) rel-rep (funcall qfun (cdr o1) (cdr o2)) )
							     (car o2)))
					(rest scenario))
		   :objects (scene-objects s)
		   :calculus calculus)))

(defmethod qualify-scene ((arity (eql :binary)) (option (eql :all)) calculus s)
  (let ((qfun (calculi::calculus-qualifier calculus))
	(rel-rep (calculi:calculus-relation-representation calculus))
	scene)
    (do ((o1s (scene-specification s) (cdr o1s)))
	((null o1s))
      (do ((o2s (cdr o1s) (cdr o2s)))
	  ((null o2s))
	(push (make-constraint (caar o1s) 
			       (ofunc:ofuncall (relations:relation-representation-encoder rel-rep) rel-rep (funcall qfun (cdar o1s) (cdar o2s)))
			       (caar o2s))
	      scene)))
    (make-instance 'constraint-network
		   :constraints (nreverse scene)
		   :objects (scene-objects s)
		   :calculus calculus)))

(defmethod qualify-scene ((arity (eql :ternary)) (option (eql :first2all)) calculus s)
  (let* ((scenario (scene-specification s))
	 (o1 (first scenario))
	 (rel-rep (calculi:calculus-relation-representation calculus))
	 (o2 (second scenario))
	 (qfun (calculi::calculus-qualifier calculus)))
    (make-instance 'constraint-network
		   :constraints (mapcar #'(lambda (o3)
					    (make-constraint (list (car o1) (car o2))
							     (ofunc:ofuncall (relations:relation-representation-encoder rel-rep) rel-rep (funcall qfun (cdr o1) (cdr o2) (cdr o3)) )
							     (car o3)))
					(cddr scenario))
		   :objects (scene-objects s)
		   :calculus calculus)))

(defmethod qualify-scene ((arity (eql :ternary)) (option (eql :all)) calculus s)
  (let ((qfun (calculi::calculus-qualifier calculus))
	(rel-rep (calculi:calculus-relation-representation calculus))
	scene)
    (do ((o1s (scene-specification s) (cdr o1s)))
	((null o1s))
      (do ((o2s (cdr o1s) (cdr o2s)))
	  ((null o2s))
	(do ((o3s (cdr o2s) (cdr o3s)))
	    ((null o3s))
	  (push (make-constraint (list (caar o1s) (caar o2s))
				 (ofunc:ofuncall (relations:relation-representation-encoder rel-rep) rel-rep (funcall qfun (cdar o1s) (cdar o2s) (cdar o3s)))
				 (caar o3s))
		scene))))
    (make-instance 'constraint-network
		   :objects (scene-objects s)
		   :constraints (nreverse scene)
		   :calculus calculus)))

(defun qualify (scene calculus option)
  "qualifies a scene in a given calculus"
  (qualify-scene (calculus-arity calculus) option calculus scene))

;; schoen waere:
;; (defcommand ("qualify" (c calculi:calculus) (option ...) (list-of (satisifies (fits-to-calculus TYPE C)))) ... )
;;
;; -> hierzu muss das Argument 'c' irgendwie in den Test zu satisfy übertragen werden
;; -> alternativ könnte man auch in Richtung Unifikation gehen, also (... (c calculus (entity ?X)) ... (scene (list-of ?X)))
;;    dazu müsste der Parser Variablen in Typdeklarationen erkennen (z.B. ?-Notation) und Bindungen zurückliefern
;;    letztere würden dann ins Parsieren von 'scene' wieder hineingegeben. 
;;  Beides aufwendig - lohnt sich das?

(defcommand ("qualify" (c calculi:calculus) (option (member cl-user::first2all cl-user::all)) (s scene c))
    "determines qualitative representation from a quantitative configuration"
    (qualify s
	     c
	     (cdr (assoc option '((cl-user::first2all . :first2all)
				  (cl-user::all       . :all))))))

;;;
;;; Parsing functions for the qualifier
;;;

(in-package :cl-user)

(defun point-distance2 (p1 p2)
  (+ (expt (- (first p1) (first p2)) 2)
     (expt (- (second p1) (second p2)) 2)))

(defun dipole-point-relation (d p)
  (destructuring-bind (sax say eax eay) d
    (destructuring-bind (px py) p
      (cond ((and (= sax eax px)
		  (= say eay py)) 'tri)
	    ((and (= sax eax)
		  (= say eay)) 'dou)
	    ((and (= sax px)
		  (= say py)) 's)
	    ((and (= eax px)
		  (= eay py)) 'e)
	    (t (let* ((dax (- eax sax))
		      (day (- eay say))
		      (sbrx (- px sax))
		      (sbry (- py say))
		      (x (+ (* sbrx day)
			    (* sbry (- dax)))))
		 (cond ((> 0 x) 'l)
		       ((< 0 x) 'r)
		       ((= x 0) (if (> 0 (+ (* (- px eax) dax) (* (- py eay) day)))
				    (if (< 0 (+ (* (- px sax) dax) (* (- py say) day)))
					'i
					'b)
				    'f)))))))))
