;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; SparQ calculi representation
;;;
;;; Provides:
;;; - Representation & accessors for operations on calcluli (e.g. composition)
;;; - Compiler that constructs an appropriate representation from a calculus definition file
;;;

;; Change history (most recent first):
;; 2012-08-21 DW  introduced function for library access
;; 2011-10-25 DW  added functions for creating and comparing relations
;; 2011-03-14 DW  added polygons to base entities
;; 2010-04-30 DW  started migration to command architecture
;; 2010-03-01 JOW neighborhood structures and now maintained in real lookup-table
;; 2010-02-08 JOW added rudimentary support for conceptual neighborhood structures (cnhs) as required for neighborhood-based reasoning
;; 2008-05-30 DW  added tractable subsets
;; 2008-05-29 DW  improved time estimation for precomputation
;; 2008-05-19 DW  fix: algebraic spec not mandatory
;; 2008-05-16 DW  added parser for algebraic specs
;; 2007-10-09 DW  added n-ary/ ternary composition to the calculi definition
;; 2007-03-16 DW  fixed prefetching in ofuncs for LOFNs
;; 2007-01-26 DW  adapted to latest ofunc functionality; bug fixes
;; 2006-11-02 DW  bug fixes (attempts to dump functions rather than ofuncs into fasls); exported set-theoretic stuff and restrictiveness
;; 2006-10-26 DW  changed relation-idx lookup to storing the relation representation instead of teh index
;; 2006-10-25 DW  bug fixes---should work now ;-9
;; 2006-10-17 DW  added the remaining compilers
;; 2006-10-13 DW  introduces use of ofunc code struct
;; 2006-10-12 DW  added compiler for directly denoted 2-elem lists (like used for e.g. converse)
;; 2006-10-09 DW  started file from SparQ's old calculi.lisp, retaining what could be used for new representation

(defpackage :calculi
  (:use :common-lisp :sparq :relations :ofunc)
  (:export :load-calculus-registry :*calculus* :*calculus-parameter* :load-calculus :expand-relation :calculus
           :converse :reflection :inverse :converse :homing :composition :shortcut :restrictiveness :relation-value
	   :sc :conv :compl :ho :inv :unite :intersect :compl :minus :calculus-algebraic-spec
	   :calculus-n-ary-composition :calculus-quantifier :calculus-tractable-subsets
           :calculus-base-relations :calculus-arity :calculus-name :calculus-identity-relation 
	   :calculus-relation-representation :calculus-basis-entity :ternary-composition :calculus-consistency-method
	   :calculus-cnhs :relation :relation= :relation->string))

(in-package :calculi)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ;;;
;;; Global variables ;;;
;;;                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *registry* nil
  "Holds the calculus registry loaded from Calculi/calculus-registry.lisp")

(defparameter *calculus* nil
  "Holds the calculus most recently read")

(defparameter *calculus-nicks* nil
  "list of nick names or abbreviations of currenty active calculus")

(defparameter *calculus-parameter* ""
  "Holds parameters of parametric calculi as string (e.g. '4' when requesting 'OPRA-4')")

;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;; Basis entities ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defparameter *basis-entities* '(:2d-point :3d-point :interval :1d-point :dipole :2d-oriented-point :disc :polygon :2d-box)
  "The list of types that can be used as basic objects")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         ;;;
;;; Calculus representation ;;;
;;;                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   
;; Calculi are represented by comprehensive structures - OOP techniques appear quite
;; difficult to handle the sophisticated on-demand compiling utilized in SparQ.
;; The basic idea of this organization is that all operations are stored as ofuncs, i.e.
;; a special kind of function that provides alongside a function definition a special 
;; compiler form is provided such that all calls can be inlined and compiled into a
;; specialized constraint solver using the ofunc structure.
;; Let's go kick some ass!
 
(defclass calculus (sparq:primitive)
  ((name :type string
	 :accessor calculus-name
	 :initarg :name
	 :initform "")
   (arity :initform :binary
	  :type keyword
	  :initarg :arity
	  :accessor calculus-arity)
   (relation-representation :initarg :relation-representation
			    :accessor calculus-relation-representation)
   (algebraic-spec :initarg :algebraic-spec
		   :accessor calculus-algebraic-spec)
   (qualifier :initarg :qualifier
	      :accessor calculus-qualifier)  
   (converse :initarg :converse
	     :accessor calculus-converse)
   (inverse :initarg :inverse
	    :accessor calculus-inverse)
   (homing  :initarg :homing
	    :accessor calculus-homing)
   (shortcut :initarg :shortcut
	     :accessor calculus-shortcut)
   (composition :initarg :composition
		:accessor calculus-composition)
   (n-ary-composition :initarg :n-ary-composition
		      :accessor calculus-n-ary-composition)
   (restrictiveness :initarg :restrictiveness
		    :accessor calculus-restrictiveness)
   (basis-entity :initarg :basis-entity
		 :accessor calculus-basis-entity)
   (quantifier :initarg :quantifier
	       :accessor calculus-quantifier)
   (tractable-subsets :initarg :tractable-subsets
		      :accessor calculus-tractable-subsets)
   (identity-relation :initarg :identity-relation
		      :accessor calculus-identity-relation)
   (consistency-method :initarg :consistency-method
		       :accessor calculus-consistency-method)
   (cnhs :initarg :cnhs
	 :accessor calculus-cnhs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                ;;;
;;; Simple functions for computing ;;;
;;;                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun open-calculus-library (libname)
  "opens a C library requested by a calculus definition"
  (let ((lib (make-local-pathname "Lib/bin" libname)))
    (do ()
	((probe-file lib)  
	 #+SBCL (sb-alien:load-shared-object lib)
	 #+OPENMCL (ccl:open-shared-library lib))
      (cerror "You will be prompted for a new file name."
	      "Failed to localize required library '~S'." lib)
      (format *sparq-io* "~&New file name: ")
      (setq lib (read-line *sparq-io*)))))


;;
;; All functions for computng with calculi are realized by calling
;; the ofunc-callable represented in the appropriate slot of the 
;; specific calculus
;;

(declaim (inline relation))
(defun relation (calculus relation-specifier)
  "interprets a relation specification (constructor)"
  (check-type calculus calculus)
  (when (stringp relation-specifier)
    (setq relation-specifier (read-from-string relation-specifier)))
  (let ((rel-rep (calculi:calculus-relation-representation calculus)))
    (ofuncall (relation-representation-encoder rel-rep) rel-rep relation-specifier)))

(declaim (inline relation->string))
(defun relation->string (calculus relation)
  "returns print name of a relation as string"
  (check-type calculus calculus)
  (let ((rel-rep (calculi:calculus-relation-representation calculus)))
    (ofuncall (relation-representation-decoder rel-rep) rel-rep relation)))

(declaim (inline relation=))
(defun relation= (calculus relation-1 relation-2)
  (check-type calculus calculus)
  (let ((rel-rep (calculi:calculus-relation-representation calculus)))
    (ofuncall (relation-representation-same-relation? rel-rep) relation-1 relation-2)))

(declaim (inline converse))
(defun converse (calculus relation)
  "A r B => B r' A"
  (ofuncall (calculus-converse calculus) calculus relation))

(declaim (inline inverse))	      
(defun inverse (calculus relation) 
  "A B r1 C => B A r1' C" 
  (ofuncall (calculus-inverse calculus) calculus relation))

(declaim (inline shortcut))
(defun shortcut (calculus relation)
  (ofuncall (calculus-shortcut calculus) calculus relation))

(declaim (inline homing))
(defun homing (calculus relation)
  (ofuncall (calculus-homing calculus) calculus relation))

(declaim (inline composition))
(defun composition (calculus r1 r2)
  (ofuncall (calculus-composition calculus) calculus r1 r2))

(declaim (inline ternary-composition))
(defun ternary-composition (calculus r1 r2 r3)
  (ofuncall (calculus-n-ary-composition calculus) calculus r1 r2 r3))

(declaim (inline unite))
(defun unite (calculus r1 r2)
  (ofuncall (relation-representation-unite (calculus-relation-representation calculus))
	    r1 r2))

(declaim (inline intersect))
(defun intersect (calculus r1 r2)
  (ofuncall (relation-representation-intersect (calculus-relation-representation calculus))
	    r1 r2))

(declaim (inline minus))
(defun minus (calculus r1 r2)
  (ofuncall (relation-representation-minus (calculus-relation-representation calculus))
	    r1 r2))

(declaim (inline compl))
(defun compl (calculus r)
  (let ((ur (relations:relation-representation-universal-relation (calculus-relation-representation calculus))))
    (minus calculus ur r)))

(declaim (inline restrictiveness))
(defun restrictiveness (calculus r)
  (ofuncall (calculus-restrictiveness calculus) calculus r))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               ;;;
;;; Compiling base relations      ;;;
;;;                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compiling base relations basically retrieves all base-relations (maybe opening an external 
;; library to get them) and returns an appropriate relation-representation
;;
;; Input:  br-spec, specification from the calculus definition:
;; Output: relation-representation and source code to generate it

(defun base-rel-spec->relation-represention (br-spec)
  (sparq:debug-out 1 "setting up relation representation...")
  (unless (consp br-spec)
    (signal-error "While compiling base-relations: base relation is not a list of type (rel-1 rel-2 ...) ~
                   or (:external-lib \"LIBNAME\" \"C_FUNCTIONNAME\")"))
  (let ((brs (cond ((eq :external-lib (first br-spec))
		    ;; Compiling from external library - we need to query the library and juice out the base-rels
		    (let ((lib-name (second br-spec)) ; library to open
			 (c-func   (third br-spec))) ; c-function to call
		      (unless (and (stringp lib-name)
				   (stringp c-func))
			(sparq:signal-error  "While compiling base relations: external library reference '~a' if ~
                                              not of the type (:external-lib \"LIBNAME\" \"C_FUNCTIONNAME\").~%" br-spec))
		      ;; Load the external library
		      (open-calculus-library lib-name)
		      ;; Since sb-alien:extern-alien is a special form that rightaway performs name conversion (i.e. lisp-style -> c_style), we need to wrap the call into an eval form
		      (sort (read-from-string #+SBCL (let ((tmp (eval `(sb-alien:alien-funcall (sb-alien:extern-alien ,c-func (sb-alien:function sb-alien:c-string   ; c-string return value
																		 sb-alien:c-string)) ; c-string calculus parameter
											       ,(the string *calculus-parameter*)))))
						    (if (stringp tmp)
							tmp
							""))
					      #+MCL "") ; SO FAR, NO EXTERNAL CALLS IN OPENMCL!!
			    #'string<
			    :key #'(lambda (x) (format nil "~a" x)))))

		   ;; Base relations defined by a Lisp function?
		   ((eq 'function (first br-spec))
		    (multiple-value-bind (rels error) (ignore-errors (funcall (eval br-spec))) ; then, we just call the function and hope to retrieve a list of base relations

		      (when error
			(sparq:signal-error "While compiling base-relations: supplied Lisp function does not evaluate; error: ~a" error))
		      (unless (and (listp rels)
				   (not (null rels))
				   (every #'symbolp rels))
			(sparq:signal-error "While compiling base-relations: supplied Lisp function does not return a non-empty list of symbols (got ~a which is a ~a)"
					    rels (type-of rels)))
		      rels))
		   
		   ;; Standard case: base-relations directly specified as list (just gets sorted)
		   (t
		    (sort br-spec #'string< :key #'(lambda (x) (format nil "~a" x)))))))
    ;; Once we have the base-relations as a list of strings just call the constructor
    (values (relations:make-representation brs)
	    `(relations:make-representation ',brs))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               ;;;
;;; Vector representation for RxR ;;;
;;;                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vectors are the most simple form of storing lookup information that maps one
;; relation into another. They provide the most compact representation (storing only
;; a vector of size n (where n denotes the number of relations). When computing the
;; value for mapping a general relation, several lookups need to be done and the
;; result needs to be combined using the relation-representation's unite function
;; (or the corresponding macro code). Relations are indexed using the relations
;; index rather than ther bit field representation. That's where the mapper 
;; function/macro of the calculus' relation-representation comes into play.

;;
;; Baisc compiler for lisp tables of the type R x R, i.e. ((rel-1 rel-2) ... )
;; as e.g. used when specifying converse et al.
;;
;; Input: rel-rep, relation-representation for the calculus
;;        op-spec, specification of the operation from the calculus specification (-> def-calculus), e.g. ((llrr rrll) (ll rr**) ...)
;;                 or a reference to an external c-function (:external-lib "my-Lib" "myCFunc")
;;        name,    the name of the operation
;;
;; Returns: Source code for an ofunc representation

(defun compile-basic-lookup-from-table (rel-rep op-spec name)
  (sparq:debug-out 1 "compiling operation '~a'..." name)
  (unless (and (listp op-spec)
	       (or (eq (first op-spec) :external-lib)
		   (every #'listp op-spec)))
    (signal-error "While compiling operation ~a: operation must be specified as list of lists, i.e. ((rel1a rel1b) (rel2a rel2b) ...) or as external call (:external-lib \"LIBNAME\" \"C_FUNCTION\")" name))
  (let* ((empty-rel (relation-representation-empty-relation rel-rep))
	 (n         (relation-representation-num-base-relations rel-rep))
	 (br-vec    (relation-representation-base-relations rel-rep))
	 (table     (apply #'vector  ; setup emtpy vector 
			   (make-list n :initial-element empty-rel)))
	 (rels-read (apply #'vector (make-list n))) ; vector of flags that registers when we read a specific relation (-> error checking)
	 (idx-table (relation-representation-relation-idx-table rel-rep)))  ; Lookup of a relation's index
    ;; Iterate over the list, parse the relation, and store them
    (if (eq (first op-spec) :external-lib)
	;; Read relation operation from external library
	(let ((libname (second op-spec))
	      (cfun    (third op-spec)))
	  (unless (and (stringp libname) (stringp cfun))
	    (signal-error "While compiling operation ~a: external library call not correctly specfied in the form ~
                          (:external-lib \"LIBNAME\" \"C_FUNCTION\")" name))
	  (open-calculus-library libname)

	  (let ((alien-fn (eval `(sb-alien:extern-alien ,cfun (sb-alien:function sb-alien:c-string   ; c-string return value
										 sb-alien:c-string   ; c-string calculus parameter (dummy)
										 sb-alien:c-string)))))  ; c-string relation name
	    (dotimes (i n)
	      (let* ((res (read-from-string 
			   #+SBCL (let ((tmp (sb-alien:alien-funcall (the (sb-alien:alien (function sb-alien:c-string sb-alien:c-string sb-alien:c-string )) alien-fn)
								     (the string *calculus-parameter*) (format nil "~a" (svref br-vec i)))))
				    (if (stringp tmp)
					tmp
					""))
			   #+MCL ""))) ; SO FAR, NO EXTERNAL CALLS IN OPENMCL!!
		(setf (svref table i) (ofuncall (relation-representation-encoder rel-rep) rel-rep res))))))
	;; Parse relation specification as list ((rel1a rel1b) ...)
	(progn
	  (dolist (entry op-spec)
	    ;; entry is of the type (rel-1 rel-2)      
	    (let ((r1-idx (gethash (first entry) idx-table))
		  (r2 (ofuncall (relation-representation-encoder rel-rep) rel-rep (rest entry))))
	      ;; Error checking
	      (unless r1-idx
	  (signal-error "While compiling ~a: relation '~a' is not a base relation.~%" name (first entry)))
	      (when (svref rels-read r1-idx)
		(sparq:signal-error "While compiling ~a: relation '~a' is specified twice (previously defined as '~a').~%"
				    name (first entry) (ofuncall (relation-representation-decoder rel-rep) rel-rep (svref table r1-idx))))
	      ;; store lookup value
	      (setf (svref table r1-idx) r2
		    (svref rels-read r1-idx) t)))
	  ;; Check whether all base-relations have been defined
	  (dotimes (i n)
	    (unless (svref rels-read i)
	      (warn "Whilecompiling ~a: no entry for base relation '~a'.~%" name (svref (relation-representation-base-relations rel-rep) i))))))
    ;; Return the ofunc
    (let ((lookupsym (gensym))
	  (ressym (gensym)))
      `(def-ofunc ,name (calculus r)
	 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	 (let* ((rel-rep (calculus-relation-representation calculus))
		(,lookupsym ',table)
		(,ressym (relation-representation-empty-relation rel-rep)))
	   (with-inlined-ofuncs ((unite  ,(relation-representation-unite rel-rep))
				 (mapper ,(relation-representation-mapper rel-rep)))
	     (let ((result ,ressym))
	       (mapper #'(lambda (idx)
			   (setq result (unite result (svref ,lookupsym idx))))
		       r)
	       result)))))))

;;
;; Compiler for tables of the type R x R, i.e. ((rel-1 rel-2) ... )
;; as e.g. used when specifying converse et al.
;; This compiler precomputes all lookups for general relations, thus storing 2^n relations where n is the number of base-relations
;;
;; 
;;
;; Input: rel-rep, relation-representation for the calculus
;;        op-spec, specification of the operation from the calculus specification (-> def-calculus), e.g. ((llrr rrll) (ll rr**) ...)
;;        name,    the name of the operation
;;
;; Returns: Source code for an ofunc representation
(defun compile-full-lookup-from-table (rel-rep op-spec name)
  (sparq:debug-out 1 "optimizing operation '~a'..." name)
  (let* ((ofunc  (compile-basic-lookup-from-table rel-rep op-spec name)) ; First, compile into an ordinary lookup
	 (lookup (make-array (expt 2 (relation-representation-num-base-relations rel-rep))
			     :initial-element (relation-representation-empty-relation rel-rep))))
    ;; evaluate simple lookup such that we can call 'name', unless the ofunc has been evaluated already
    (unless (fboundp name)
      (eval ofunc))
     ;; Precompute the lookup for all 2^n general relations
    (let ((dc (make-instance 'calculus :relation-representation rel-rep)))
      (with-all-general-relations (r rel-rep)
	(setf (svref lookup r) (funcall name dc r))) ; relation 'r' is used directly as index since it must be a fixnum (actually a quite small one ;-)
      (let ((lookupsym (gensym)))
	`(def-ofunc ,name (dummy r)
	   (let ((,lookupsym ',lookup))
	     (svref ,lookupsym r)))))))
								    
;;
;; Baisc compiler for lisp tables of the type R x R, i.e. ((rel-1 rel-2) ... )
;; as e.g. used when specifying converse et al.
;;
;; Parameter:
;; rel-rep    - the relation-representation used for representing relations in the calculus
;; table-spec - the list specified in the calculus specification (-> def-calculus) , e.g. ((llrr rrll) (llll rr**) ...)
;; name       - name (symbol) to be used as function name in the macro code (typically composition)
;; 
;; Returns: 
;; Source code for an ofunc structure
;;

(defun compile-basic-lookup-from-composition-table (rel-rep table-spec name)
  (sparq:debug-out 1 "compiling operation 'composition'...")
  (unless (and (listp table-spec)
	       (or (eq (first table-spec) :external-lib)
		   (every #'listp table-spec)))
    (signal-error "While compiling operation ~a: operation must be specified as list of lists, i.e. ~
                   ((rel1a rel1b rel1c) (rel2a rel2b rel2c) ...) or as external call (:external-lib ~
                   \"LIBNAME\" \"C_FUNCTION\")" name))
  (let* ((empty-rel (relation-representation-empty-relation rel-rep))
	 (n         (relation-representation-num-base-relations rel-rep))
	 (br-vec    (relation-representation-base-relations rel-rep))
	 (table     (make-array (list n n) :initial-element empty-rel))    ; setup emtpy composition table 
	 (rels-read (make-array (list n n) :initial-element nil))          ; array of flags that registers when we read a specific relation (-> error checking)
	 (idx-table (relation-representation-relation-idx-table rel-rep))) ; Lookup of a relation's index
    (if (eq (first table-spec) :external-lib)
	;; Read relation operation from external library
	(let ((libname (second table-spec))
	      (cfun    (third table-spec)))
	  (unless (and (stringp libname) (stringp cfun))
	    (signal-error "While compiling operation ~a: external library call not correctly specfied in the form (:external-lib \"LIBNAME\" \"C_FUNCTION\")" name))
	  (open-calculus-library libname)
	  ;; I think we need to use eval for constructing an extern-alien since this appears to be a macro which uses the symbol's print name (here: cfun)
	  ;; as the name of the external c function. Grrr!
	  (let ((alien-fn #+SBCL (eval `(sb-alien:extern-alien ,cfun (sb-alien:function sb-alien:c-string      ; c-string return value
											sb-alien:c-string      ; c-string calculus parameter (dummy)
											sb-alien:c-string      ; c-string relation name 1
											sb-alien:c-string)))   ; c-string relation name 2
			  #+MCL nil))
	    (dotimes (i n)
	      (dotimes (j n)
		(let ((res (read-from-string #+SBCL (let ((tmp (sb-alien:alien-funcall (the (sb-alien:alien (function sb-alien:c-string sb-alien:c-string sb-alien:c-string sb-alien:c-string)) alien-fn)
								(the string *calculus-parameter*) 
								(format nil "~a" (svref br-vec i)) 
								(format nil "~a" (svref br-vec j)))))
						      (if (stringp tmp)
							  tmp
							  ""))
					     #+MCL ""))) ; SO FAR, NO EXTERNAL CALLS IN OPENMCL!!
		  (setf (aref table i j) (ofuncall (relation-representation-encoder rel-rep) rel-rep res)))))))

	;; Parse relation specification as list ((rel1a rel1b) ...)
	(progn
	  ;; Iterate over the list, parse the relation, and store them
	  (dolist (entry table-spec)
	    ;; entry is of the type (rel-1 rel-2 rel-3)      
	    (let ((r1-idx (gethash (first entry) idx-table))
		  (r2-idx (gethash (second entry) idx-table))
		  (r3 (ofuncall (relation-representation-encoder rel-rep) rel-rep (if (listp (third entry)) ; handle specs of type (rel1 rel2 rel3), of type (rel1 rel2 (rel3a rel3b)), and of type (rel1 rel2 rel3a rel3b...)
										      (third entry)
										      (cddr entry)))))
	      ;; Error checking
	      (unless r1-idx
		(error "While compiling ~a: relation #1 '~a' is not a base relation.~%" name (first entry)))
	      (unless r2-idx
		(error "While compiling ~a: relation #2 '~a' is not a base relation.~%" name (second entry)))
	      (when (aref rels-read r1-idx r2-idx)
		(error "While compiling ~a: relation '~a o ~a' is specified twice (previously defined as '~a').~%"
				    name (first entry) (second entry) (ofuncall (relation-representation-decoder rel-rep) rel-rep (aref table r1-idx r2-idx))))
	      ;; store lookup value
	      (setf (aref table r1-idx r2-idx) r3
		    (aref rels-read r1-idx r2-idx) t)))
	  ;; Check whether all base-relations have been defined
	  (dotimes (i n)
	    (dotimes (j n)
	      (unless (aref rels-read i j)
		(warn "Which compiling ~a: no entry for base relation '~a o ~a'.~%" name (svref (relation-representation-base-relations rel-rep) i) (svref (relation-representation-base-relations rel-rep) j)))))))
    ;; Return the ofunc
    (let ((lookupsym (gensym)))
      `(def-ofunc ,name (calculus r1 r2)
	 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	 (let* ((rel-rep (calculus-relation-representation calculus))
		(,lookupsym ',table)
		(emptyrel (relation-representation-empty-relation rel-rep)))
	   (with-inlined-ofuncs ((unite ,(relation-representation-unite rel-rep))
				 (mapper ,(relation-representation-mapper rel-rep)))
	     (let ((result emptyrel))
	       (mapper #'(lambda (idx)
			   (let ((tmp emptyrel))
			     (mapper #'(lambda (idx2)
					 (setq tmp (unite tmp (aref ,lookupsym idx idx2))))
				     r2)
			     (setq result (unite result tmp))))
		       r1)
	       result)))))))

;;
;; Compiler for lisp composition tables that get inlined completey
;;
;; Parameter:
;; rel-rep    - the relation-representation used for representing relations in the calculus
;; the-table  - the list specified in the calculus specification (-> def-calculus) , e.g. ((llrr rrll) (llll rr**) ...)
;; name       - name (symbol) to be used as function name in the macro code (typically composition)
;; 
;; Returns:
;; Source code for an ofunc representation

(defun compile-full-lookup-from-composition-table (rel-rep op-spec name)
  (sparq:debug-out 1 "optimizing operation 'composition'...")
  (let* ((ofunc  (compile-basic-lookup-from-composition-table rel-rep op-spec name)) ; First, compile into an ordinary lookup
	 (dc (make-instance 'calculus :relation-representation rel-rep))
	 (lookup (make-array (expt 2 (* 2 (relation-representation-num-base-relations rel-rep)))
			     :initial-element (relation-representation-empty-relation rel-rep))))
    ;; evaluate simple lookup such that we can call 'name'
    (unless (fboundp name)
      (eval ofunc))
    ;; Precompute the lookup for all 2^n * 2^n general relations
    (when (< 6 (relation-representation-num-base-relations rel-rep))
      (multiple-value-bind (min sec) (floor (/ (expt 2 (* 2 (relation-representation-num-base-relations rel-rep))) 40000.0 (/ *processor-speed* 2000000000.0)) 60) ; guess time based on time of my computer and is scaled based on available processor clock
	(setq sec (if (< 20 sec) (* 10 (ceiling sec 10)) (ceiling sec))) ; round to full seconds or to full 10s
	(cond ((= 0 min) (format *error-output* "~&;; NOTE: Optimizing may take a while, I'm guessing a time of around ~a seconds.~%" sec))
	      ((< 3 min) (format *error-output* "~&;; NOTE: Optimizing may take a while, I'm guessing a time of around ~a minutes.~%" min))
	      (t (format *error-output* "~&;; NOTE: Optimizing may take a while, I'm guessing a time of around ~a minutes and ~a seconds.~%" min sec)))
	(force-output)))
    (with-all-general-relations (r1 rel-rep)
      (with-all-general-relations (r2 rel-rep)
	(setf (aref lookup (ofuncall (relation-representation-combine rel-rep) rel-rep r1 r2)) (funcall name dc r1 r2))))
    (let ((lookupsym (gensym)))
      `(def-ofunc ,name (calculus r1 r2)
	 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	 (let ((,lookupsym ',lookup)
	       (rel-rep (calculus-relation-representation calculus)))
	   (with-inlined-ofuncs (((combine rel-rep) ,(relation-representation-combine rel-rep)))
	     (aref ,lookupsym (the fixnum (combine rel-rep r1 r2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   ;;;
;;; n-ary composition ;;;
;;;                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-basic-lookup-from-n-composition-table (rel-rep table-spec name)
  (sparq:debug-out 1 "compiling operation 'n-ary-composition'...")
  (unless (and (listp table-spec)
	       (or (eq (first table-spec) :external-lib)
		   (every #'listp table-spec)))
    (signal-error "While compiling operation ~a: operation must be specified as list of lists, i.e. ((rel1a rel1b rel1c) (rel2a rel2b rel2c) ...) or as external call (:external-lib \"LIBNAME\" \"C_FUNCTION\")" name))
  (let* ((empty-rel (relation-representation-empty-relation rel-rep))
	 (n         (relation-representation-num-base-relations rel-rep))
	 (br-vec    (relation-representation-base-relations rel-rep))
	 (table     (make-array (list n n n) :initial-element empty-rel))    ; setup emtpy composition table 
	 (rels-read (make-array (list n n n) :initial-element nil))          ; array of flags that registers when we read a specific relation (-> error checking)
	 (idx-table (relation-representation-relation-idx-table rel-rep))) ; Lookup of a relation's index
    (if (eq (first table-spec) :external-lib)
	;; Read relation operation from external library
	(let ((libname (second table-spec))
	      (cfun    (third table-spec)))
	  (unless (and (stringp libname) (stringp cfun))
	    (signal-error "While compiling operation ~a: external library call not correctly specfied in the form (:external-lib \"LIBNAME\" \"C_FUNCTION\")" name))
	  (open-calculus-library libname)
	  ;; I think we need to use eval for constructing an extern-alien since this appears to be a macro which uses the symbol's print name (here: cfun)
	  ;; as the name of the external c function. Grrr!
	  (let ((alien-fn #+SBCL (eval `(sb-alien:extern-alien ,cfun (sb-alien:function sb-alien:c-string      ; c-string return value
											sb-alien:c-string      ; c-string calculus parameter (dummy)
											sb-alien:c-string      ; c-string relation name 1
											sb-alien:c-string      ; c-string relation name 2
											sb-alien:c-string)))   ; c-string relation name 3
			  #+MCL nil))
	    (dotimes (i n)
	      (dotimes (j n)
		(dotimes (k n)
		  (let ((res (read-from-string #+SBCL (let ((tmp (sb-alien:alien-funcall (the (sb-alien:alien (function sb-alien:c-string sb-alien:c-string sb-alien:c-string sb-alien:c-string sb-alien:c-string)) alien-fn)
											 (the string *calculus-parameter*) 
											 (format nil "~a" (svref br-vec i)) 
											 (format nil "~a" (svref br-vec j))
											 (format nil "~a" (svref br-vec k)))))
							(if (stringp tmp)
							    tmp
							    ""))
					       #+MCL ""))) ; SO FAR, NO EXTERNAL CALLS IN OPENMCL!!
		    (setf (aref table i j k) (ofuncall (relation-representation-encoder rel-rep) rel-rep res))))))))

	;; Parse relation specification as list ((rel1a rel1b) ...)
	(progn
	  ;; Iterate over the list, parse the relation, and store them
	  (dolist (entry table-spec)
	    ;; entry is of the type (rel-1 rel-2 rel-3)      
	    (let ((r1-idx (gethash (first entry) idx-table))
		  (r2-idx (gethash (second entry) idx-table))
		  (r3-idx (gethash (third entry) idx-table))
		  (r (ofuncall (relation-representation-encoder rel-rep) rel-rep (if (listp (fourth entry)) ; handle specs of type (rel1 rel2 rel3), of type (rel1 rel2 (rel3a rel3b)), and of type (rel1 rel2 rel3a rel3b...)
										      (fourth entry)
										      (cdddr entry)))))
	      ;; Error checking
	      (unless r1-idx
		(error "While compiling ~a: relation #1 '~a' is not a base relation.~%" name (first entry)))
	      (unless r2-idx
		(error "While compiling ~a: relation #2 '~a' is not a base relation.~%" name (second entry)))
	      (unless r3-idx
		(error "While compiling ~a: relation #3 '~a' is not a base relation.~%" name (third entry)))
	      (when (aref rels-read r1-idx r2-idx r3-idx)
		(error "While compiling ~a: relation '~a * ~a * ~a' is specified twice (previously defined as '~a').~%"
				    name (first entry) (second entry) (third entry) (ofuncall (relation-representation-decoder rel-rep) rel-rep (aref table r1-idx r2-idx r3-idx))))
	      ;; store lookup value
	      (setf (aref table r1-idx r2-idx r3-idx) r
		    (aref rels-read r1-idx r2-idx r3-idx) t)))
	  ;; Check whether all base-relations have been defined
	  (dotimes (i n)
	    (dotimes (j n)
	      (dotimes (k n)
		(unless (aref rels-read i j k)
		  (warn "Which compiling ~a: no entry for base relation '~a * ~a * ~a'.~%" name (svref (relation-representation-base-relations rel-rep) i) (svref (relation-representation-base-relations rel-rep) i) (svref (relation-representation-base-relations rel-rep) k))))))))
    ;; Return the ofunc
    (let ((lookupsym (gensym)))
      `(def-ofunc ,name (calculus r1 r2 r3)
	 (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	 (let* ((rel-rep (calculus-relation-representation calculus))
		(,lookupsym ',table)
		(emptyrel (relation-representation-empty-relation rel-rep)))
	   (with-inlined-ofuncs ((unite ,(relation-representation-unite rel-rep))
				 (mapper ,(relation-representation-mapper rel-rep)))
	     (let ((result emptyrel))
	       (mapper #'(lambda (idx)
			   (mapper #'(lambda (idx2)
				       (mapper #'(lambda (idx3)
						   (setq result (unite result (aref ,lookupsym idx idx2 idx3))))
					       r3))
				   r2))
		       r1)
	       result)))))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;                 ;;;
;;; Restrictiveness ;;;
;;;                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Restrictiveness provides information about how little information gets
;; lost in constraint propagation, thus, starting with the most restrictive
;; relation, gives a good heuristic in constraint reasoning. In calculi of
;; few base relations only the restrictiveness can be computed exactly for
;; all 2^n general relations. For more complex calculi, restrictiveness is
;; approximated using a fast heuristic based on look values for base relations

;; 
;; compile-basic-restrictiveness constructs the ofunc for restrictiveness using
;; a compact representation that only precomputes restrictiveness for all base
;; relations.
;;
;; Parameters;
;; rel-rep            - relation-representation of the calculus
;; name               - name of the resulting ofunc (typically 'restrictiveness)
;; composition-source - the ofunc definition source code for the compilation
;; composition-name   - name of the composition function (generated by gensym in order not to get redefinition warnings)
;;
(defun compile-basic-restrictiveness (rel-rep name composition-source composition-name)
  (sparq:debug-out 1 "computing heuristics...")
  ;; First, construct a callable composition function
  (unless (fboundp composition-name)
    (eval composition-source))
  (let ((dummy-calculus (make-instance 'calculus :relation-representation rel-rep))
	(br-enc (relation-representation-br-encodings rel-rep))
	(n (relation-representation-num-base-relations rel-rep)))
    (flet ((n-compose (r1 r2) ; compute the number of base-relations contained in a composition operation
	     (let ((count 0))
	       (ofuncall (relation-representation-mapper rel-rep)
			 #'(lambda (idx)
			     (declare (ignore idx))
			     (incf count))
			 (funcall composition-name dummy-calculus r1 r2))
	       count)))
      (let ((lookup (apply 'vector (make-list n :initial-element 0))))
	(dotimes (i n)
	  (let ((r1 (svref br-enc i))
		(count 0))
	    (dotimes (j n)
	      (force-output)
	      (incf count (n-compose r1 (svref br-enc j))))
	    (setf (svref lookup i) (round count n))))

	(let ((lookupsym (gensym)))
	  `(def-ofunc ,name (calculus r)
	     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	     (let ((,lookupsym ,lookup)
		   (rel-rep (calculus-relation-representation calculus)))
	       (with-inlined-ofuncs (((mapper rel-rep) ,(relation-representation-mapper rel-rep)))
		 (let ((count 0))
		   (mapper #'(lambda (idx)
			       (incf count (svref ,lookupsym idx)))
			   r)
		   count)))))))))

;;				 
;; Variant of restrictiveness that correctly precomputes all values and maintains s
;; lookup of size 2^n where n is the number of base relations.
;;
;; Parameters;
;; rel-rep            - relation-representation of the calculus
;; name               - name of the resulting ofunc (typically 'restrictiveness)
;; composition-source - the ofunc definition source code for the compilation
;; composition-name   - usually 'composition - but maybe I'd change that to calculi-specific names
;;

(defun compile-full-restrictiveness (rel-rep name composition-source composition-name)
  (sparq:debug-out 1 "optimizing heuristics...")
  ;; First, construct a callable composition function
  (unless (fboundp composition-name)
    (eval composition-source))
  (let ((dummy-calculus (make-instance 'calculus :relation-representation rel-rep))
	(br-enc (relation-representation-br-encodings rel-rep))
	(n (relation-representation-num-base-relations rel-rep)))
    (flet ((n-compose (r1 r2) ; compute the number of base-relations contained in a composition operation
	     (let ((count 0))
	       (ofuncall (relation-representation-mapper rel-rep)
			 #'(lambda (idx)
			     (declare (ignore idx))
			     (incf count))
			 (funcall composition-name dummy-calculus r1 r2))
	       count)))
      ;; Setup an array that contains lookup values for all 2^n general relations
      ;; Here we assume that relations are encoded using a fixnum!
      (let ((lookup (apply 'vector (make-list (expt 2 n) :initial-element 0)))
	    (max 0))
	(declare (type fixnum max))
	(with-all-general-relations (r1 rel-rep)
	  (dotimes (j n)
	    (let ((val (n-compose r1 (svref br-enc j))))
	      (when (< max val)
		(setq max val))
	      (setf (svref lookup r1) val))))
	;; scale value to 0...31 (to fit 32bit archs, -> bin.-cons.-reas. 2do-item)
	(when (< 31 max)
	  (let ((f (/ max 31)))
	    (dotimes (i (expt 2 n))
	      (setf (svref lookup i) (floor (* f (svref lookup i)))))))
	;(format *error-output* "~%LOOKUP:~%~a" lookup)
	
	;; Return the ofunc
	(let ((lookupsym (gensym)))
	  `(def-ofunc ,name (dummy r)
	     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	     (let ((,lookupsym ',lookup))
	       (svref ,lookupsym r))))))))

(defun ext-qualifier-params (basis-entity arity) ;; FIXME: doesn't handle polygons as parameters!
  "Constructs the parameter of an external qualifier function in a C Library"
  (let ((obj-param (make-list (cadr (assoc basis-entity '((:2d-point 2) (:dipole 4) (:2d-oriented-point 4) (:1d-point 1) (:3d-point 3) (:disc 3) (:2d-box 4))))
                              :initial-element 'sb-alien:double-float)))
    (cons 'sb-alien:function
	  (append '(sb-alien:c-string sb-alien:c-string)
		  (if (eq arity :binary)
		      (append obj-param obj-param)
		      (append obj-param obj-param obj-param))))))

;;;
;;; algebraic specification parser
;;;

(defun parse-algebraic-spec (spec rel-rep)
  (let* ((n (relation-representation-num-base-relations rel-rep))
	 (polys (make-array n))
	 (error-count 0)
	 (errmsg (make-string-output-stream)))
    (flet ((handle-constants (poly) ; encodes simple numbers as monoms
	     (remove nil (mapcar #'(lambda (term)
				     (if (numberp term)
					 (if (zerop term)
					     nil
					     (list term ()))
					 term))
				 poly))))
      (dotimes (i n)
	(let* ((r (svref (relation-representation-br-encodings rel-rep) i))
	       (relevant-specs (apply #'append (mapcar #'cdr (remove-if-not #'(lambda (es) 
										(ofuncall (relation-representation-empty-relation? rel-rep)
											  (ofuncall (relation-representation-minus rel-rep)
												    r (ofuncall (relation-representation-encoder rel-rep) rel-rep (first es)))))
									    spec)))))
	  ;(format t "~%i = ~a, relevant specs = ~a" i relevant-specs)
	  (if relevant-specs
	      (setf (aref polys i) (loop for spec in relevant-specs collecting (let ((pos-op (position-if #'(lambda (x)
													      (member x '(< > = <= >=)))
													  spec)))
										 (if (null pos-op)
										     (progn
										       (incf error-count)
										       (format errmsg "no order relation (<, >, =, <=, >=) found in specification of relation ~a~%" (svref (relation-representation-base-relations rel-rep) i)))
										     ;; construct the algebraic-spec: op, left side, right side
										     (list (nth pos-op spec) 
											   (handle-constants (subseq spec 0 pos-op)) 
											   (handle-constants (subseq spec (1+ pos-op))))))))
	      (progn
		(incf error-count)
		(format errmsg "specification for relation ~a is missing~%" (svref (relation-representation-base-relations rel-rep) i)))))))
    (if (< 0 error-count)
	(signal-error "Error in algebraic specification, there ~[is~:;are~] ~D error~:P:~%~A" (1- error-count) error-count (get-output-stream-string errmsg))
	polys)))

(defun best-split (r es)
  (let ((candidates (remove-if #'(lambda (r2)
				   (/= 0 (logandc1 r r2)))
			       es)))
    (labels ((smallest-split (r2 rels)
	       (if (null rels)
		   (values nil MOST-POSITIVE-FIXNUM)
		   (let ((r3 (logior r2 (car rels))))
		     (if (eq r r3)
			 (values (list (car rels)) 1)
			 (multiple-value-bind (sp1 l1) (smallest-split r3 (cdr rels)) ; in l1 ist die relation (car rels) noch nicht mitgezaehlt!
			   (multiple-value-bind (sp2 l2) (smallest-split r2 (cdr rels))
			     (if (< l1 l2)
				 (values (cons (car rels) sp1) (1+ l1))
				 (values sp2 l2)))))))))		           
					;(format t "~%;; r = ~a~%candidates = ~a~%split = ~a" r candidates (smallest-split 0 candidates))
					;(finish-output)
      (smallest-split 0 candidates))))

;; computes the best splits for all 2^n relations given a tractable set ts
(defun flood-split (n ts)
  (let* ((nrels (expt 2 n))
	 (best-splits (make-array nrels :initial-element nil))
	 (front ts)
	 (new-front ()))
    (dolist (r ts)
      (setf (aref best-splits r) (cons 1 (list r))))
    (loop while front do
	 (dolist (r1 front)
	   (dolist (r2 ts)
	     (let ((r3 (logior r1 r2)))
	       (unless (aref best-splits r3)
		 (let ((s1 (aref best-splits r1)))
		   (setf (aref best-splits r3) (cons (1+ (car s1)) (cons r2 (cdr s1)))))
		 (push r3 new-front)))))
	 (setq front new-front
	       new-front nil))
    best-splits))

(defun 4-cluster (scores)
  (let* ((n (length scores))
	 (s (copy-list (sort scores #'<))))
    (list (nth (round n 3) s)
	  (nth (round n 2) s)
	  (nth (round (* 3 n) 4) s))))

;; encodes tractable subsets
(defun compile-full-tsets (rel-rep name spec restr-code restr-name)
  (sparq:debug-out 1 "optimizing tractable sets...")
  (let ((n (relation-representation-num-base-relations rel-rep))
	(enc-spec (mapcar #'(lambda (ts)
			      (mapcar #'(lambda (r)
					  (ofuncall (relation-representation-encoder rel-rep) rel-rep r))
				      ts))
			  spec))
	(u (relation-representation-universal-relation rel-rep)))
    ;; Verify tractable sets
    (let ((faulty-sets (remove-if #'(lambda (ts)
				      (eq u (apply #'logior ts)))
				  enc-spec)))
      (when faulty-sets
	(let ((poss (mapcar #'1+ (mapcar #'(lambda (fs)
					     (position fs enc-spec))
					 faulty-sets)))
	      (multiple? (cdr faulty-sets)))
	  (signal-error "The tractable set~:[~;s~] #~{~a, ~}~a ~:[is~;are~] erroneous: not all relations can be generated" multiple? (butlast poss) (car (last poss)) multiple?))))

    ;;;; MISSING ;;;;

    ;; Setup a list that contains lookup values for all 2^n general relations
    (eval restr-code)
    (let* ((scores ())
	   (count 0)
	   (lookup (coerce (mapcar #'(lambda (ts)
				       (let ((ts-split (flood-split n ts)))
					 (incf count)
					 (do ((i 1 (1+ i)))
					     ((eq i (length ts-split)))
					   (setf (aref ts-split i) (let ((split (aref ts-split i)))
								     (unless split
								       (signal-error "Relation ~a cannot be refined by the ~d. tractable set ~a"
										     (ofuncall (relation-representation-decoder rel-rep) rel-rep i)
										     count
										     (mapcar #'(lambda (r)
												 (ofuncall (relation-representation-decoder rel-rep) rel-rep r))
											     ts)))
								     (let ((score (- (round (apply #'+ (mapcar #'(lambda (r)
														   (ofuncall restr-name nil r))
													       (cdr split)))
											    (first split))
										     (ofuncall restr-name nil i))))
								       (push score scores)  ;; <- eigentlich muessten dies Scores getrennt nach tractable subset sein!
								       (list (first split)
									     score
									     (sort (copy-list (cdr split)) #'< :key #'(lambda (r)
															(ofuncall restr-name nil r))))))))
					 
					 ts-split))
				   enc-spec)
			   'vector)))
      ;;(format t "~%CLUSTER = ~a" (4-cluster scores))
      ;;(finish-output)
      ;; Return the ofunc
      (let ((lookupsym (gensym)))
	`(def-ofunc ,name (ts r)
	   (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
	   (let ((,lookupsym ',lookup))
	     (aref (svref ,lookupsym ts) r)))))))

;; compile conceptual neighborhood tables
(defun compile-neighborhood-tables (cnhs rel-rep)
  (mapcar (lambda (n) 
	    (let ((nrels (second n))
		  (neighbors/of (intern (string-upcase (format nil "~a" (gensym "neighbors"))))))
	      (let ((neighbors-ofunc (compile-basic-lookup-from-table rel-rep nrels neighbors/of)))
		(list (car n) neighbors/of neighbors-ofunc))))
	  cnhs))
 

;;;;;;;;;;;;;;;;;;;;;
;;;               ;;;
;;; def-calculus  ;;;
;;;               ;;;
;;;;;;;;;;;;;;;;;;;;;

;; def-calculus is the macro the user employs to specify a calculus
;; macroexpansion checks the specification for possible errors, derives
;; a suitable way for compiling the calculus (e.g. precompute some lookup
;; tables), and finally compiles the calculus

(defmacro cl-user::def-calculus (name &rest key-args &key 
				 (arity                   nil arity-spec?)
				 (parametric?             nil parametric-spec?)
				 (basis-entity            nil entity-spec?)
				 (identity-relation       nil identity-spec?)
				 (converse-operation      nil converse-spec?)
				 (inverse-operation       nil inverse-spec?)
				 (qualifier               nil qualifier-spec?)
				 (homing-operation        nil homing-spec?)
				 (shortcut-operation      nil shortcut-spec?)
				 (base-relations          nil base-relations-spec?)
				 (composition-operation   nil composition-spec?)
				 (algebraic-specification nil algebraic-spec?)
				 (n-ary-composition-operation nil n-ary-composition-spec?)
				 (quantifier              nil)
				 (tractable-subsets       nil tsets-spec?)
				 (cn                      nil cns-spec?)
				 (consistency             nil consistency-spec?)
				 (cnhs 			  nil cnhs-spec?)
				 &allow-other-keys
				 )
  ;; check if there are extra keys in calculus definition and warn user
  (let ((extra-keys ()))
    (do ((rest-key-args key-args (cddr rest-key-args)))
	((null rest-key-args))
      (let ((key (car rest-key-args)))
	(unless (find key '(:arity :parametric? :basis-entity :identity-relation :converse-operation :inverse-operation :qualifier :homing-operation :shortcut-operation
			    :consistency :base-relations :composition-operation :algebraic-specification :n-ary-composition-operation :quantifier :tractable-subsets :cnhs)) ; arg not in keyword list?
	  (push key extra-keys))))
    (when extra-keys
      (format *error-output* "Warning: Possible error in calculus definition of ~a, ~D key value pair~:P will be ignored: ~{~w ~}~%" name (length extra-keys) extra-keys)))

  (multiple-value-bind (rel-rep rel-rep-source) (base-rel-spec->relation-represention base-relations)
    (let (external-libs) ; Holds a list of libraries we're supposed to load
      ;; Some utility functions for signaling the user if there's something fishy in the specification    
      (labels ((missing-error (what)
		 (sparq:signal-error "Error in ~a calculus definition of ~a: ~a not specified~%" arity name what))
	       (missing-warn (what)
		 (format *error-output* "Warning: Possible error in ~a calculus definition of ~a: ~a not specified~%" arity name what)
		 (force-output *error-output*))
	       (extra-warn (what)
		 (format *error-output* "Warning: Possible error in ~a calculus definition of ~a: ~a is given, but not specified for ~a calculi~%" arity name what arity)
		 (force-output *error-output*))
	       (gen-name (what)
		 (intern (string-upcase (gensym (format nil "~a-~a" name what)))))
	       (calculus-code ()
		 (let* ((n (relation-representation-num-base-relations rel-rep))
			(converse/of (gen-name "converse"))
			(inverse/of  (gen-name "inverse"))
			(shortcut/of (gen-name "shortcut"))
			(homing/of   (gen-name "homing"))
			(restrictiveness/of (gen-name "restrictiveness"))
			(composition/of (gen-name "composition"))
			(n-ary-composition/of (gen-name "n-composition"))
			(tsets/of (gen-name "tsets"))
			(conv-ofunc (if (eq arity :binary)
					(if (< n 24)
					    (compile-full-lookup-from-table rel-rep converse-operation converse/of)
					    (compile-basic-lookup-from-table rel-rep converse-operation converse/of))))
			(inv-ofunc (if (eq arity :ternary)
				       (if (< n 24)
					   (compile-full-lookup-from-table rel-rep inverse-operation inverse/of)
					   (compile-basic-lookup-from-table rel-rep inverse-operation inverse/of))))
			(sc-ofunc (if (eq arity :ternary)
				      (if (< n 24)
					  (compile-full-lookup-from-table rel-rep shortcut-operation shortcut/of)
					  (compile-basic-lookup-from-table rel-rep shortcut-operation shortcut/of))))
			(ho-ofunc (if (eq arity :ternary)
				      (if (< n 24)
					  (compile-full-lookup-from-table rel-rep homing-operation homing/of)
					  (compile-basic-lookup-from-table rel-rep homing-operation homing/of))))
			(comp-ofunc (if (< n 12)
					(compile-full-lookup-from-composition-table rel-rep composition-operation composition/of)
					(compile-basic-lookup-from-composition-table rel-rep composition-operation composition/of)))
			(restr-ofunc (if (< n 12)
					 (compile-full-restrictiveness rel-rep restrictiveness/of comp-ofunc composition/of)
					 (compile-basic-restrictiveness rel-rep restrictiveness/of comp-ofunc composition/of)))
			(n-comp-ofunc (if n-ary-composition-spec?
					  (compile-basic-lookup-from-n-composition-table rel-rep n-ary-composition-operation n-ary-composition/of)))
			(tset-ofunc (if (and tsets-spec? (< n 12))
					(compile-full-tsets rel-rep tsets/of tractable-subsets restr-ofunc restrictiveness/of)))
			(neighborhoods (compile-neighborhood-tables cnhs rel-rep))
			(code `(eval-when (:load-toplevel) (progn
							     ,@(mapcar #'open-calculus-library
								       (remove-duplicates external-libs :test #'string=))
							     ,conv-ofunc
							     ,inv-ofunc
							     ,sc-ofunc
							     ,ho-ofunc
							     ,comp-ofunc
							     ,restr-ofunc
							     ,n-comp-ofunc
							     ,tset-ofunc
							     ,@(mapcar #'third neighborhoods)
							     (defparameter calculi::*calculus* 
							       (make-instance 'calculi::calculus 
									      :name ,name
									      :arity ,arity
									      :algebraic-spec ',(if algebraic-spec? (parse-algebraic-spec algebraic-specification rel-rep))
									      :qualifier ,qualifier
									      :basis-entity ,basis-entity
									      :identity-relation ',identity-relation
									      :relation-representation ,rel-rep-source
									      :restrictiveness ',restrictiveness/of
									      :converse ',converse/of
									      :inverse ',inverse/of
									      :homing  ',homing/of
									      :shortcut ',shortcut/of
									      :composition ',composition/of
									      :tractable-subsets ,(if tset-ofunc
												      `',tsets/of
												      '())
									      :quantifier 'quantifier
									      :consistency-method ',consistency
									      :n-ary-composition ',(if n-comp-ofunc n-ary-composition/of)
									      :cnhs ',(mapcar #'butlast neighborhoods)))))))
		   ;; Now, this is a little nasty: during compilation we might have defined composition and converse, if tables got inlined
		   ;; To avoid warnings whining that we shouldn't redefine the function, let's just dispose of it. It would be a bad idea,
		   ;; to wrap some conditionals around the generated code, since our gensyms aren't real gensyms considering multiple, independent
		   ;; invocations. nuff said
		   (when (fboundp converse/of)
		     (fmakunbound converse/of))
		   (when (fboundp composition/of)
		     (fmakunbound composition/of))
		   ;;(when sparq:*debug* (pprint code *STANDARD-OUTPUT*))
		   code)))
	;; Error checking---make sure, all required slots are actually specified     
	(unless parametric-spec?
	  (missing-warn ":parametric?"))
	(when (null base-relations-spec?)
	  (missing-error "Base relations"))
	(when (null arity-spec?)
	  (missing-error "Arity"))
	(unless (member arity '(binary ternary :binary :ternary 2 3))
	  (error "Error in calculus definition of ~a: Error in arity specification (~a) " name arity))
	(case arity ; arity will be represented internally as either :binary or :ternary
	  (2 (setq arity :binary))
	  (binary (setq arity :binary))
	  (ternary (setq arity :ternary))
	  (3 (setq arity :ternary)))
	
	(if consistency-spec?
	    (if (not (member consistency '(:unknown :a-closure :algebraic-closure :n-ary-closure :scenario-consistency :n-ary-scenario-consistency)))
		(sparq:signal-error "Error in ~a calculus definition of ~a: consistency method '~a' not support (must be member of :unknown, :a-closure. :algebraic-closure, :scenario-consistency)~%" arity name consistency))
	    (missing-warn "Decision method for deciding consistency of constraint network"))
	
	(unless identity-spec?
	  (missing-error "Identity relation"))
	(unless composition-spec? 
	  (missing-error "Composition relation"))
	(unless (stringp name)
	  (setq name (format nil "~(~a~)" name)))
	;; Shortcut only for ternary calculi
	(when (and (eq arity :ternary) (not shortcut-spec?))
	  (missing-warn "shortcut"))
	(when (and (eq arity :binary) shortcut-spec?)
	  (extra-warn "Shortcut"))
	
	;; Homing only for ternary calculi
	(when (and (eq arity :ternary) (not homing-spec?))
	  (missing-warn "homing"))
	(when (and (eq arity :binary) homing-spec?)
	  (extra-warn "homing"))
	
	;; Inverse only for ternary calculi
	(when (and (eq arity :ternary) (not inverse-spec?))
	  (missing-warn "inverse"))
	(when (and (eq arity :binary) inverse-spec?)
	  (extra-warn "inverse"))
	
	;; Converse only for binary calculi
	(when (and (eq arity :binary) (not converse-spec?))
	  (missing-warn "converse"))
	(when (and (eq arity :ternary) converse-spec?)
	  (extra-warn "converse"))
	
	;; Check that identity-relation is a base-relation
	(let ((idrel (string-downcase (format nil "~a" identity-relation)))) ; ###
	  (unless (find-if #'(lambda (r)
			       (string= (string-downcase (format nil "~a" r)) idrel))
			   (coerce (relations:relation-representation-base-relations rel-rep) 'list))
	  (signal-error "Error in calculus definition of ~a: Identity relation '~a' is not a base-relation" name identity-relation)))
	
	
	(when entity-spec?
	  (unless (find basis-entity *basis-entities*)
	    (signal-error "Error in calculus definition of ~a: Basis entity of type '~a' not supported" name basis-entity)))	  
	
	;; Qualifier is tricky---this should be 'outsourced' 
	(if qualifier-spec?
	    (cond ((and (not (consp qualifier)) 
			(not (functionp qualifier)))
		   (sparq:signal-error "Error in ~a calculus definition of ~a: qualifier specification ~a not understood!~%" arity name qualifier))
		  
		  ((eq (car qualifier) 'function))     ; OK, we already have a lisp function and directly use it
		  ((eq (car qualifier) 'lambda))
		  
		  ((eq (car qualifier) 'cl-user::external-lib)  ; Qualification by external library, so we compile that into a lisp function
		   (let ((lib-name (second qualifier))
			 (function-name (third qualifier)))
		     #+OPENMCL (declare (ignore function-name))
		     (push lib-name external-libs)
		     (unless entity-spec?
		       (sparq:signal-error "Error in calculus definition of ~a: external qualifier requires basis-entity to be specified!~%" name))
		     (setq qualifier
			   (if (eq arity :binary)
			       `(lambda (o1 o2)				
				  (read-from-string #+SBCL (apply #'sb-alien:alien-funcall 
								  (cons (sb-alien:extern-alien ,function-name ,(ext-qualifier-params basis-entity arity))
									(cons *calculus-parameter*
									      (mapcar #'(lambda (x)
											  (coerce x 'double-float))
										      (append o1 o2)))))
						    #+OPENMCL ""  ; SO FAR NO EXTERNAL CALLS IN OPENMCL!
						    nil nil))
			       `(lambda (o1 o2 o3)
				  (read-from-string #+SBCL (apply #'sb-alien:alien-funcall 
								  (cons (sb-alien:extern-alien ,function-name ,(ext-qualifier-params basis-entity arity))
									(cons *calculus-parameter*
									      (mapcar #'(lambda (x)
											  (coerce x 'double-float))
										      (append o1 o2 o3)))))
						    #+OPENMCL ""  ; SO FAR NO EXTERNAL CALLS IN OPENMCL!
						    nil nil))))))
		  (t
		   (sparq:signal-error "Error in ~a calculus definition of ~a: qualifier specification ~a not understood!~%" arity name qualifier)))
	    (if (not algebraic-spec?)
		(missing-warn "qualification")
		(format t "~%Algebraic semantics specified---SparQ doesn't yet support automatic qualification. Please wait for the next version, we're working on that!~%")))
	
	(if parametric?
	    ;; Compilation is different for parametric calculi - in this case we can't pre-compile anything.
	    ;; Instead, we must wait until a specific calculus is initialized the first time. Then, a temporary
	    ;; calculus definition is written into a file name <calculus-name>-cache-<parameter>.lisp wherebey
	    ;; :paramteric? is set to nil. This file is then compiled the usual way and deleted afterwards.
	    ;; But first of all, it is checked whether the compiled <calculus-name>-cache-<parameter>.fasl
	    ;; is already on disc.
	    `(let* ((filename  (format nil "~a-cache-~a" ,name *calculus-parameter*))
		    (cachefile (sparq:make-local-pathname "Calculi" filename "fasl")))
	       (declaim (sb-ext:muffle-conditions sb-ext:compiler-note)
			(sb-ext:muffle-conditions sb-ext:compiler-note))
	       ,@(mapcar #'open-calculus-library
			 (remove-duplicates external-libs :test #'string=))							   
	       (unless (probe-file cachefile) ; if not already compiled for current *calculus-parameter*...
		 (let ((source-file (sparq:make-local-pathname "Calculi" filename "lisp")))
		   ;; write temporary calculus definition...
					;(when (probe-file source-file) ;; Weired, but :supersede won't do...
					;  (delete-file source-file))
		   (with-open-file (out source-file :direction :output :if-exists :supersede)
		     (format out "(eval-when (:load-toplevel) (progn ~{(calculi::open-calculus-library ~w) ~}))"
			     ',(remove-duplicates external-libs :test #'string=))
		     (format out "(cl-user::def-calculus ~w~% :arity ~w~%:parametric? nil~%:basis-entity ~w~%:identity-relation ~w~%:base-relations ~w~%" ',name ',arity ',basis-entity ',identity-relation ',base-relations)
		     ,@(let (code)
			    (when converse-spec? (push `(format out ":converse-operation ~w~%" ',converse-operation) code))
			    (when inverse-spec? (push `(format out ":inverse-operation ~w~%" ',inverse-operation) code))
			    (when qualifier-spec? (push `(format out ":qualifier ~w~%" ',qualifier) code))
			    (when homing-spec? (push `(format out ":homing-operation ~w~%" ',homing-operation) code))
			    (when shortcut-spec? (push `(format out ":converse-operation ~w~%" ',shortcut-operation) code))
			    (when base-relations-spec? `(push (format out ":base-relations ~w~%" ',base-relations) code))
			    (when composition-spec? (push `(format out ":composition-operation ~w~%" ',composition-operation) code))
			    (when algebraic-spec? (push `(format out ":algebraic-spec ~w~%" ',algebraic-specification) code))
			    code)		 
		     (format out ")~%"))
		   (unwind-protect (compile-file source-file :output-file (make-pathname :directory (list :relative) :name filename :type "fasl")) ; ...and compile it
		     (delete-file source-file)))) ; delete temporary file afterwards
	       (format t "~%Loading '~a'~%" cachefile)
	       (load cachefile))
	    ;; If the calculus isn't parametric we can just return the appropriate code to the lisp compiler
	    (calculus-code))))))
  
  ;;
  ;; load the calculus registry into *registry*
  ;;
  (defun load-calculus-registry ()  
    (with-open-file (registry (make-local-pathname "Calculi" "calculus-registry" "lisp") :direction :input)
      ;; Read registry
      (let ((registry (read registry)))
	;; Perform some error checking
	(if (and (listp registry)
		 (every #'listp registry)
		 (every #'(lambda (def)
			    (every #'stringp def))
			registry))
	    (setf *registry* registry)
	    (sparq:signal-error "Error reading calculus registry - see comments in registry file for details.")))))
  
(defun locate-calculus (calculus)
  (let ((calc-info (find-if #'(lambda (def)
				(find calculus def :test #'(lambda (str1 str2)
							     (let ((l1 (length str1))
								   (l2 (length str2)))                                                                                    
							       (if (and (<= l2 l1)
									(if (char= (char str2 (1- l2)) #\-) ; calculus expecting parameter...
									    (string= (string-downcase str1) (string-downcase str2) :end1 l2)
									    (string= (string-downcase str1) (string-downcase str2))))
								   (progn
								     (when (< l2 l1)
								       (setf *calculus-parameter* (format nil "~a" (read-from-string str1 nil nil :start l2))))
								     t))))))
			    *registry*)))
    (values (if calc-info 
		(first (last calc-info))
		(let ((cfile (parse-namestring calculus)))
		  (if (probe-file cfile)
		      cfile)))
	    calc-info)))

(defun load-calculus (calculus)
  "Loads a calculus by its identifier. Looks up file to load in registry and then loads the calculus into variable *calculus*"
  (multiple-value-bind (calculus-file calc-info) (locate-calculus calculus)
    (if (null calculus-file)
	(let ((calculi (mapcar #'(lambda (cdef)
				   (let ((names (butlast cdef)))
				     (format nil "~a~:[~; ~(~a~)~]" (car names) (cdr names) (cdr names))))
			       *registry*)))
	  (sparq:signal-error "Calculus '~a' unknown; known calculi are: ~{~a, ~}~a~%" 
			      calculus 
			      (butlast calculi)
                            (car (last calculi))))
	(multiple-value-bind (dummy error) (progn ;ignore-errors
					     (setf *calculus-nicks* calc-info) ; memorize nicknames 
					     (if sparq:*debug*
						 (if calc-info ; calc registered in registry?
						     (sparq:load/compile "Calculi" calculus-file :verbose sparq:*debug*)
						     (sparq:load/compile nil nil :verbose t :pathname calculus-file))
						 ;; If we're not in debug mode, kill all compiler notices
						 (let ((bu *standard-output*)
						       (err (make-string-output-stream)))
						   (unwind-protect  (progn
								      (setq *standard-output* err)
								      (if calc-info ; calc registered in registry?
									  (sparq:load/compile "Calculi" calculus-file :verbose nil)
									  (sparq:load/compile nil nil :verbose nil :pathname calculus-file)))
						     (setq *standard-output* bu)))))
	  (declare (ignore dummy))
	  (when error
	    (if (not (probe-file calculus-file))
		(sparq:signal-error "Loading calculus '~a' from file '~a' failed, file not found.~%" calculus calculus-file)
		(sparq:signal-error "Loading calculus '~a' from file '~a' failed, no calculus definition found." calculus calculus-file)))
	  (unless (eq (class-name (class-of *calculus*)) 'calculus)
	    (sparq:signal-error "Loading calculus '~a' from file '~a' failed, error in calculus definition or missing calculus definition." calculus calculus-file)))))
  *calculus*)

(let ((cache (make-hash-table :test 'equal))) ; cache parsing attempts so we do not need to reload a calculus
  (defmethod parse-primitive ((c (eql 'calculus)) specifier &rest extra-spec)
    (if (eq specifier 'cl-user::*)
	(if *calculus*
	    (if (car extra-spec)
		(if (find (format nil "~(~a~)" (caar extra-spec)) (cons (calculus-name *calculus*) *calculus-nicks*) 
			  :test #'(lambda (s1 s2) (string-equal s1 (string-downcase s2))))
		    *calculus*
		    (cons :FAIL (format nil "Error: Calculus ~a does not match with specialization ~a" (calculus-name *calculus*) (caar extra-spec))))
		*calculus*)
	    (cons :FAIL "No calculus loaded"))
	;; calculus specifier either corresponds to calculus nickname or path/filename
	(let ((cfile (locate-calculus (format nil "~a" specifier))))
	  (multiple-value-bind (dummy error) (ignore-errors (let ((err (catch 'error (prog1 nil
									       (let ((cvalue (gethash cfile cache)))
										 (if cvalue
										     (setq *calculus* cvalue)
										     (progn 
										       (load-calculus (format nil "~a" specifier))
										       (check-type *calculus* calculus)
										       (setf (gethash cfile cache) *calculus*)
										       *calculus*))))))) 
						      (values nil err)))
	    (declare (ignore dummy))
	    ;; check extra calculus specifier, if given
	    (when (and (not error)
		       (car extra-spec))
	      (let ((rname (format nil "~(~a~)" (caar extra-spec))))
		;;(format t "~%vgl.: cfile ~a <=> loc ~a,  name ~a <=> ~a ~%" cfile (locate-calculus rname) (calculus-name *calculus*) rname)
		(unless (or (equal cfile (locate-calculus rname))
			    (and (find rname (cons (calculus-name *calculus*) *calculus-nicks*)
				       :test #'(lambda (s1 s2) (string-equal s1 (string-downcase s2))))))
		  (setq error (format nil "Calculus '~a' does not satisfy restriction ~a" *calculus* rname)))))
	  (if error
	      (cons :FAIL (format nil "~a" error))
	      *calculus*)))))
)

(defmethod print-object ((c calculus) stream)
  (format stream "<calculus ~a>" (calculus-name c)))

(defcommand ("load-calculus" (c calculus))
  "loads a calculus"
  c)

(defclass relation (primitive)
  ((value :accessor relation-value
	  :initarg :value)
   (calculus :accessor relation-calculus
	     :initarg :calculus)))

(defmethod print-object ((r relation) stream)
  (let ((rel-rep (calculus-relation-representation (relation-calculus r))))
    (format stream "~a" (ofuncall (relation-representation-decoder rel-rep) rel-rep (relation-value r)))))

(defmethod parse-primitive ((r (eql 'relation)) expr &key (calculus nil calculus-supplied?))
  (unless calculus-supplied? ;; this shouldn't be possible...
    (signal-error "Error: no calculus supplied to relation parser (likely to be caused by faulty command specification)"))
  (let ((rel-rep (calculus-relation-representation calculus)))
    (catch 'error (let ((value (ofuncall (relation-representation-encoder rel-rep)
					 rel-rep
					 (let ((*readtable* *lisp-readtable*))
					   (read-from-string (format nil "~a" expr))))))
		    (return-from parse-primitive (make-instance 'relation 
								:value value
								:calculus calculus))))))

