;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006-2011 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; SparQ constraint reasoning
;;;

;; Change history (most recent first):
;; 2010-05-03 DW  migrating to new command architecture
;; 2007-03-14 DW  improved scenario-consistency; splitted files
;; 2007-03-13 DW  ofunc-optimized path consistency
;; 2006-10-26 DW  initial version for SparQ V0.7

(defpackage :constraint-reasoning
  (:use :common-lisp :sparq :calculi :ofunc)
  (:nicknames :csp)
  (:export :propagate :check-consistency :refine-configurations :extend-configurations :n-ary-closure :test-ternary-closure :test-ternary-consistency :n-ary-consistency
	   :parse-constraint-network :print-network :constraint :constraint-object-1 :constraint-relation :constraint-object-2 :make-constraint :test-pathconsistency/binary 
	   :constraint-network :constraint-network-objects :constraint-network-calculus :constraints :make-constraint))

(in-package :constraint-reasoning)

;;;
;;; Constraint structure
;;;

;; used in the user interface
(defstruct (constraint (:constructor make-constraint (object-1 relation object-2)))
  object-1   ; contains an object descriptor (symbol)
  relation   ; contains a relation (-> relation-representation)
  object-2)  ; contains an object descriptor (symbol)

(defun read-file-name (stream)
  (let ((quote? nil)
        (char #\a)
        (name (make-array 31 :adjustable t :fill-pointer 0 :element-type 'base-char)))
    (loop until (or (eq char +nothing+) (eq char #\Space)) do
          (setq char (read-char stream nil +nothing+))
          (if (eq char #\\)
            (setq quote? t)
            (progn
              (unless (or (eq char +nothing+) (and (eq char #\Space) (not quote?)))
                (vector-push-extend char name)
                (setq char #\a))
              (setq quote? nil))))
    name))

(defun parse-constraint-network (calculus constraints-spec &key allow-multiple-definitions?)
  ;; Check, if a file is to be read
  (if (string= (format nil "~:@(~a~:@)" (caar constraints-spec)) "FROM-FILE")
    (let ((file (symbol-name (second (car constraints-spec)))))
      (if (null file)
        (signal-error "Missing file name in directive :from-file~%")
        (if (and (stringp file)
                 (probe-file file))
          (with-open-file (constraints-spec-file file :direction :input)
            (setq constraints-spec (read constraints-spec-file nil +nothing+)))
          (signal-error "Error reading file ~w~%" file))))
    ;; Otherwise, if read from standard-input, if there has been nothing on the cmd-line
    (if constraints-spec
	(setq constraints-spec (car constraints-spec))
	(setq constraints-spec (read *standard-input* nil +nothing+))))
  ;; Now, check the constraints read, construct the CSP data structure, and invoke the reasoner
  (when (eq constraints-spec +nothing+)
    (signal-error "Could not read constraints~%"))
  (unless (listp constraints-spec)
    (signal-error "Wrong constraint format; list of types ( (obj-1 rel-1 obj-2) ...) are required, supplied was: ~a (a ~a)~%" constraints-spec (type-of constraints-spec)))
  (let (objects constraints)
    (if (eq (calculus-arity calculus) :binary)
	;; Parse binary constraint network
	(let* ((rel-rep (calculi:calculus-relation-representation calculus))	     
	       (encoder (relations:relation-representation-encoder rel-rep))
	       (double-defs ()) ; constraints defined multiple times
	       (objects-hash (make-hash-table :size 1024))) ;; hash-table of all objects and all constraints parsed
	  (declare (dynamic-extent double-defs objects-hash))
	  (dolist (c constraints-spec)
	    (when (or (not (consp c))
		      (not (= 3 (length c))))
	      (signal-error "Syntax error in constraint specification (obj-1 rel obj-2): ~a~%" c))	    
	    
	    (let ((o1 (first c)) ; object #1
		  (o2 (third c))) ; object #2
	      (when (eq o1 o2)
		(signal-error "Syntax error in constraint specification - object is related to itself; the erroneous constraint is ~a." c))

	      ;; mark that we encountered objects o1 and o2, update list of all objects if necessary
              
	      (unless (gethash o1 objects-hash)
		(setf (gethash o1 objects-hash) (make-hash-table))
		(push o1 objects))
	      (unless (gethash o2 objects-hash)
		(setf (gethash o2 objects-hash) (make-hash-table))
		(push o2 objects))
	      #|
	      (unless (gethash o1 objects-hash)
		(setf (gethash o1 objects-hash) t)
		(push o1 objects))
	      (unless (gethash o2 objects-hash)
		(setf (gethash o2 objects-hash) t)
		(push o2 objects))
	      |# 
	      (let ((constraint (make-constraint (first c) 
						 (ofuncall encoder rel-rep (second c))
						 (third c))))
		;; check whether constraint between o1 and o2 hash already been declared
                
		(if (< (sxhash o1) (sxhash o2)) ; trick to store constraint marker only once
		    (if (gethash o2 (gethash o1 objects-hash))
			(push constraint double-defs)
			(setf (gethash o2 (gethash o1 objects-hash)) t))
		    (if (gethash o1 (gethash o2 objects-hash))
			(pushnew constraint double-defs)
			(setf (gethash o1 (gethash o2 objects-hash)) t)))
                
		(push constraint constraints))
	      (when (and double-defs (not allow-multiple-definitions?))
		(signal-error "Error in constraint specification, multiple constraint definitions: ~(~a~)~%"
			      (mapcar #'(lambda (c) (list (constraint-object-1 c) 
							  (ofunc:ofuncall (relations:relation-representation-decoder rel-rep) rel-rep (constraint-relation c))
							  (constraint-object-2 c)))
				      double-defs)))))
	  (values objects constraints))
	;; Parse ternary constraint network
	(let* ((rel-rep (calculi:calculus-relation-representation calculus))
	       (encoder (relations:relation-representation-encoder rel-rep)))
					;(format t "~%constraint-spec = ~w (~a)" constraints-spec (type-of constraints-spec))
	  (dolist (c constraints-spec)
	    (when (or (not (listp c))
		      (not (= 4 (length c))))
	      (signal-error "Syntax error in constraint specification (obj-1 obj-2 rel obj-3): ~a~%" c))
	    (when (or (eq (first c) (second c))
		      (eq (first c) (fourth c))
		      (eq (second c) (fourth c)))
	      (signal-error "Syntax error in constraint specification - object is related to itself; the erroneous constraint is ~a." c)) ; ###
	    (pushnew (first c) objects)
	    (pushnew (second c) objects)
	    (pushnew (fourth c) objects)
	    (push (make-constraint (list (first c) (second c))
				   (ofuncall encoder rel-rep (third c))
				   (fourth c))
		  constraints))
	  (let ((double-defs (remove-if-not #'(lambda (c1)
						(some #'(lambda (c2)
							  (and (not (eq c1 c2))
							       (null (set-difference (cons (constraint-object-2 c1) (constraint-object-1 c1))
										     (cons (constraint-object-2 c2) (constraint-object-1 c2))))))
						      constraints))
					    constraints)))
	    (when (and double-defs (not allow-multiple-definitions?))
	      (signal-error "Error in constraint specification, multiple constraint definitions: ~(~a~)~%"
			    (mapcar #'(lambda (c) (append (constraint-object-1 c) (list (constraint-relation c) (constraint-object-2 c))))
				    double-defs))))
	  (values objects constraints)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                          ;;;
;;; Computing with constraint networks (extending, refining) ;;;
;;;                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Utility function for constraint network merging
(defun conflicting-constraints? (arity c1 c2)
  "Retrieves overlapping constraints from two contraint networks"
  (let ((objects (if (eq arity :ternary)
		     (union (cons (constraint-object-2 c1) (constraint-object-1 c1))
			    (cons (constraint-object-2 c2) (constraint-object-1 c2)))
		     (union (list (constraint-object-1 c1) (constraint-object-2 c1))
			    (list (constraint-object-1 c2) (constraint-object-2 c2))))))
    (eq (length objects)
	(if (eq arity :ternary)
	    3
	    2))))

;; Merging of constraint networks, conflicting constraint definitions
;; are resolved according to a merge function that generates a constraint-specific
;; decision (choose left/right, unite, ...)
(defun merge-constraints (merge-function calculus c1 c2)
  (if (eq :binary (calculi:calculus-arity calculus))
      ;; binary constraint
      (if (eq (constraint-object-1 c1) (constraint-object-1 c2))
	  ;; (A r1 B) /\ (A r2 B)
	  (make-constraint (constraint-object-1 c1)
			   (funcall merge-function (constraint-relation c1) (constraint-relation c2))
			   (constraint-object-2 c1))
	  ;; (A r1 B) /\ (B r2 A)
	  (make-constraint (constraint-object-1 c1)
			   (funcall merge-function (constraint-relation c1) (calculi:converse calculus (constraint-relation c2)))
			   (constraint-object-2 c1)))
      ;; ternary constraint
      (cond ((equal (constraint-object-1 c1) (constraint-object-1 c2)) ; A,B r1 C  & A,B r2 C
	     (make-constraint (constraint-object-1 c1)
			      (funcall merge-function (constraint-relation c1) (constraint-relation c2))
			      (constraint-object-2 c1)))

	    ((and (eq (first (constraint-object-1 c1)) (second (constraint-object-1 c2))) ; A,B r1 C & B,A r2 C
		  (eq (second (constraint-object-1 c1)) (first (constraint-object-1 c2))))
	     (make-constraint (constraint-object-1 c1)
			      (funcall merge-function (constraint-relation c1) (inverse calculus (constraint-relation c2)))
			      (constraint-object-2 c1)))

	    ((and (eq (first (constraint-object-1 c1)) (constraint-object-2 c2)) ; A,B r1 C & C,B r2 A
		  (eq (second (constraint-object-1 c1)) (second (constraint-object-1 c2))))
	     (make-constraint (constraint-object-1 c1)
			      (funcall merge-function (constraint-relation c1) (inverse calculus (shortcut calculus (inverse calculus (constraint-relation c2)))))
			      (constraint-object-2 c1)))

	    ((and (eq (constraint-object-2 c1) (first (constraint-object-1 c2))) ; A,B r1 C & C,A r2 B
		  (eq (constraint-object-2 c2) (first (constraint-object-1 c1))))
	     (make-constraint (constraint-object-1 c1)
			      (funcall merge-function (constraint-relation c1) (homing calculus (homing calculus (constraint-relation c2))))
			      (constraint-object-2 c1)))

	    ((and (eq (constraint-object-2 c1) (first (constraint-object-1 c2))) ; A,B r1 C & A,C r2 B
		  (eq (constraint-object-2 c2) (second (constraint-object-1 c1))))
	     (make-constraint (constraint-object-1 c1)
			      (funcall merge-function (constraint-relation c1) (shortcut calculus (constraint-relation c2)))
			      (constraint-object-2 c1)))

	    ((and (eq (second (constraint-object-1 c1)) (first (constraint-object-1 c2))) ; A,B r1 C & B,C r2 A
		  (eq (constraint-object-2 c1) (second (constraint-object-1 c2))))
	     (make-constraint (constraint-object-1 c1)
			      (funcall merge-function (constraint-relation c1) (homing calculus (constraint-relation c2)))
			      (constraint-object-2 c1)))

	    (t (signal-error "Internal error: dropping through cond statement in merge-constraints; this shouldn't have occured! c-1 = ~a, c-2 = ~a~%Please report this bug!" c1 c2)))))
  

;; Prints a constraint network represented as a list of constraints if the calculus is ternary
(defun dump-ternary-constraint-network (stream calculus constraints)
  (let* ((rel-rep (calculus-relation-representation calculus))
	 (decoder (relations:relation-representation-decoder rel-rep)))
    (format stream "(")
    (mapc #'(lambda (c) 
	      (format stream "(~a ~a ~a ~a)" 
		      (first (constraint-object-1 c)) (second (constraint-object-1 c))
		      (ofuncall decoder rel-rep (constraint-relation c))
		      (constraint-object-2 c)))
	  (sort constraints #'string< :key #'(lambda (c) (format nil "~a" (constraint-object-1 c)))))
    (format stream ")~%")))

;; Prints a constraint network represented as list of constraints
(defun print-network (calculus stream network)
  (if (eq (calculi:calculus-arity calculus) :ternary)
      (dump-ternary-constraint-network stream calculus network)
      (let* ((rel-rep (calculus-relation-representation calculus))
	     (decoder (relations:relation-representation-decoder rel-rep)))
	(format stream "(")
	(mapc #'(lambda (c)
		  (format stream "(~a ~(~a~) ~a) "
			  (constraint-object-1 c) 
			  (ofuncall decoder rel-rep (constraint-relation c))
			  (constraint-object-2 c)))
	      (sort (copy-list network) #'string< :key (lambda (c) (format nil "~a-~a" (constraint-object-1 c) (constraint-object-2 c)))))
	(format stream ")~%"))))

;; Main function for merging constraints 
(defun merge-configurations (merge-function calculus cn1 cn2)
  (let* ((constraints-1 (constraints cn1))
	 (constraints-2 (constraints cn2))
	 (network (reduce #'(lambda (constraint configuration)
			      (let ((conflicting-constraint (find-if #'(lambda (c)
									 (conflicting-constraints? (calculi:calculus-arity calculus) c constraint))
								     configuration)))
				(if conflicting-constraint
				    ;; refine conflicting constraints
				    (cons (merge-constraints merge-function calculus conflicting-constraint constraint)
					  (remove conflicting-constraint configuration))
				    ;; add non-conflicting constraint
				    (cons constraint configuration))))
			  constraints-2
			  :from-end t
			  :initial-value constraints-1))
	 (rel-rep (calculi:calculus-relation-representation calculus)))
    (prog1 
	(mapcar #'(lambda (c)
		    (append (if (listp (constraint-reasoning::constraint-object-1 c) )
				(constraint-reasoning::constraint-object-1 c)
				(list (constraint-reasoning::constraint-object-1 c)))
			    (list (read-from-string (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep (constraint-reasoning::constraint-relation c)))
				  
				  (constraint-reasoning::constraint-object-2 c))))
		network)
      (make-instance 'constraint-network
		     :constraints network))))

;; Combining two constraint networks by intersecting corresponding constraints
(defun refine-configurations (calculus cn1 cn2)
  (merge-configurations #'(lambda (r1 r2) (calculi:intersect calculus r1 r2))
			calculus cn1 cn2))

;; Combining two constraint networks by uniting corresponding constraints
(defun extend-configurations  (calculus cn1 cn2)
  (merge-configurations #'(lambda (r1 r2) (calculi:unite calculus r1 r2))
			calculus cn1 cn2))



#|
(defstruct (constraint-network (:constructor make-constraint-network% (objects calculus n obj-hash matrix)))
  objects
  calculus
  (n 0 :type fixnum)
  objects-idx-hash
  matrix)

(defun make-constraint-network (calculus objetcs n constraints)
  (let ((rel-rep (calculi:calculus-relation-representation calculus))
	(idx-hash (make-hash-table :size n)))
    (do ((i 0 (1+ i))
	 (os objects (cdr os)))
	((null os))
      (let ((obj (car os)))
	(setf (gethash os idx-hash) i)))
    (make-constraint-network% objects
			      calculus
			      n
			      idx-hash
			      (make-array (/ (* n (1+ n)) 2) :initial-element (relations:relation-representation-universal-relation rel-rep)))))


|#

(defclass constraint-network (sparq:primitive)
  ((constraints :initarg :constraints)
   (objects :reader constraint-network-objects
	    :initarg :objects)
   (matrix :reader constraint-network-matrix
	   :initform nil
	   :initarg :matrix)
   (calculus :reader constraint-network-calculus
	     :initarg :calculus)))

(defun matrix->constraints (csp)
  (let ((cs ()))
    (do ((o1s (constraint-network-objects csp) (cdr o1s))
	 (i1 0 (+ i1 1)))
	((null o1s))
      (let ((o1 (car o1s))
	    (i2 i1))
	(dolist (o2 (cdr o1s))
	  (incf i2)
	  (push (make-constraint o1 (aref (constraint-network-matrix csp) i1 i2) o2) cs))))
    cs))

(defmethod constraints ((csp constraint-network))
  (if (slot-boundp csp 'constraints)
      (slot-value csp 'constraints)
      (setf (slot-value csp 'constraints) (matrix->constraints csp))))

;; Prints out a constraint matrix as used in binary constraint reasoning
(defun dump-constraint-matrix (stream calculus objects constraint-matrix)
  "Prints a constraint matrix to standard-out using the object names supplied" 
  (let* ((rel-rep (calculi:calculus-relation-representation calculus))
	 (decoder (relations:relation-representation-decoder rel-rep)))
    (format stream "(")
    (dotimes (a (length objects))
      (do ((b 0 (1+ b)))
	  ((>= b a))
	(format stream "(~a ~a ~a)" (nth a objects)  (ofuncall decoder rel-rep (aref constraint-matrix a b)) (nth b objects))))
    (format stream ")~%")
    (finish-output stream)))

(defmethod print-object ((cn constraint-network) stream)
  (if (constraint-network-matrix cn)
      (dump-constraint-matrix stream (constraint-network-calculus cn) (constraint-network-objects cn) (constraint-network-matrix cn))
      (print-network (constraint-network-calculus cn) stream (constraints cn))))

(defmethod parse-primitive ((x (eql 'constraint-network)) expression &key calculus allow-multiple-definitions?)
  (cond ((listp expression)
	 (let ((lisp-expr (if (null expression) 
			      (read *sparq-io* nil nil) ; STDIN Parsing?
			      (read-from-string (format nil "~w" expression)))))
	   (cons :FAIL (catch 'error (multiple-value-bind (objs cns) 
					 (parse-constraint-network calculus (list lisp-expr) :allow-multiple-definitions? allow-multiple-definitions?)
				       (return-from parse-primitive (make-instance 'constraint-network
										   :objects objs
										   :constraints cns
										   :calculus calculus)))))))
	((eq 'constraint-network (type-of expression))
	 expression)
	(t
	 (cons :FAIL "Constraint networks need to be lists"))))

;;;
;;; SparQ commands
;;;

(defcommand ("constraint-reasoning" (c calculus) "refine" (cn constraint-network c) (cn2 constraint-network c) &rest csps)
  "merges two constraint-networks by intersecting corresponding constraints"
  (let ((cns (mapcar #'(lambda (net)
			 (parse-primitive 'constraint-network net :calculus c))
		     csps)))
    (if (every #'(lambda (cn) (eq 'constraint-network (type-of cn))) cns)
	(reduce #'(lambda (accu csp)
		    (refine-configurations c accu csp))
		cns
		:initial-value (parse-primitive 'constraint-network (refine-configurations c cn cn2) :calculus c))
	(signal-error "parse error in constraint-network #~d:~a" (+ 3 (position-if #'listp cns)) (cdr (nth (position-if #'listp cns) cns))))))

(defcommand ("constraint-reasoning" (c calculus) "extend" (cn constraint-network c) (cn2 constraint-network c))
  "merges two constraint-networks by relaxing corresponding constraints"
  (extend-configurations c cn cn2))

(defcommand ("constraint-reasoning" (c calculus) "update" (cn constraint-network c) (cn2 constraint-network c))
  "merges two constraint-networks by updating corresponding constraints"
  (merge-configurations #'(lambda (r1 r2) r2)
			c cn cn2))

