;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; ofuncs - functions for instance-specific optimization
;;;
;;; 


(defpackage :ofunc
  (:use :common-lisp)
  (:export :def-ofunc :def-ofunc/macro :optimize-for-instance :return-from-function :with-inlined-ofuncs :with-ofuncs :ofuncall))

(in-package :ofunc)

;;
;; Ofuncs are data structures for optimized function calls
;; that can be inlined in a sophisticated way. The basic 
;; concept is the ofunc object that bundles function code, executable
;; (i.e. compiled function) and inline macro code. Ofuncs define a 
;; make-load-form specializer, thus they can be dumped to fasl files 
;; during compilation. So, ofuncs can be used like ordinary functions.
;; However, if they are called from with a defun/optimized function
;; then a call to optimize-for-instance constructs a specialized variant 
;; that inlines/avoids most function calls and prefetches all data that
;; would otherwise be retrieved during run-time. Bottom line: its way faster

;; Change history (most recent first):
;; 2009-05-29 DW  changed declaration of compiler policy
;; 2007-10-09 DW  introduced fetch-initial-let-binding so that declare statements now work in ofuncs
;; 2007-03-15 DW  intrdoduced return-from-function
;; 2007-03-12 DW  tweaks in optimize-for-instance / with-inlined-ofuncs
;; 2007-03-09 DW  improved optimize-for-instance
;; 2007-01-18 DW  bug fix in def-ofunc/macro; kicked ofunc aliasing in favor of more simple with-ofuncs-macro
;; 2006-11-16 DW  added def-ofunc/macro
;; 2006-11-14 DW  added aliases for ofuncs
;; 2006-11-09 DW  redesigned ofuncs and introduced macros for convenient definition
;; 2006-11-03 DW  started this file from ofunc source in sparq.lis & added the defun/optimizable macro


;;; HOW TO USE OFUNCS IN YOUR CODE:
;;;
;;; 1) Declare functions using the def-ofun macro. You need to specify data which
;;;    is constant in normal use of this function, e.g. multiple operations on the
;;;    same data structure. Example:
;;;
;;;    (def-ofunc composition (calculus rel-1 rel-2)
;;;      (let ((lookup (some-calculus-composition-look calculus)))
;;;        (aref lookup rel-1 rel-2)))
;;;
;;;    Here, the lookup associated with 'calculus' needs to be retrieved only once for multiple calls to
;;;    composition.
;;;
;;; 2) Use optimize-for-instance to obtain a compiled function optimized for a non-changing context. Example:
;;;
;;;    (def-ofunc compute-alot (calculus all-relations)
;;;        (dolist (r1 all-relations)
;;;          (dolist (r2 all-relations)
;;;            (do-some-stuff-with (composition calculus rel-1 rel-2))))))
;;;
;;; 3) Obtaining an optimized version of compute-alot: ********OBSOLETE  - DOCUMENTATION REQUIRES UPDATE !!!*********
;;;
;;;    (optimize-for-instance (compute-alot *my-calculus*) (composition *my-calculus*))
;;;
;;;    That's it. Note that 'optimize-for-instance' will recursively descend into the function body of the 
;;;    provided ofunc, optimizing additional ofuncs encountered as well. If there are only few calls to an
;;;    ofunc then don't call optimize-for-instance in advance, but simply call the regular function defined
;;;    alongside by def-ofun as the overhead of compiling will eat up any gain in execution speed.

(defstruct ofunc
  name        ; <- name of the corresponding function
  prefetch    ; <- data to be prefetched
  lambda-list ; <- lambda list of the function/macro
  code)       ; <- code to define the function/macro

(defun ofunc-obj-name (symbol)
  "retrieves the ofunc object designated by a symbol"
  (intern (format nil "*OFUNC-~a*" symbol) (symbol-package symbol)))

(defun get-ofunc (name)
  (symbol-value (ofunc-obj-name name)))


;; backquotes all arguments in a function body such that the body
;; can serve for macro expansion
;; Example:
;; lambda-list = (r1 r2)
;; body = (logior r1 r2)
;; => `(logior ,r1 ,r2)
(defun backquote-lambda-vars (lambda-list code)
  ;; replaces vars in all but the first position in a list - needed since function calls must not be affected (e.g. lambda=(foo) code=(foo foo) -> `(foo ,foo)
  (cond ((null code) nil)
	((listp code) (if (atom (car code))
			  `(list ',(car code) ,@(mapcar #'(lambda (code) (backquote-lambda-vars lambda-list code)) (cdr code)))
			  `(list ,@(mapcar #'(lambda (code) (backquote-lambda-vars lambda-list code)) code))))
	((atom code) (if (find code lambda-list)
			 code
			 (list 'quote code)))
	(t (error "Wow - it's not a list nor a null nor an atom... It's a ~a! (It's actually a ~a)~%Sorry, I didn't expect this to happen." (type-of code) code))))

;; returns the leading let binding in a code body 
(defun fetch-initial-let-binding (body)
  (if (consp (car body))
      (cond ((or (eq (caar body) 'let)
		 (eq (caar body) 'let*)) (values (cadar body) (cddar body)))
	    ((and (eq (caar body) 'declare)
		  (or (eq (caadr body) 'let)
		      (eq (caadr body) 'let*))) (values (cadadr body) (cddadr body)))
	    (t (values nil body)))
      (values nil body)))

;; Macro for convenient definition of ofuncs
(defmacro def-ofunc (name lambda-list &body body)
  (let* ((dummy-vars (remove-if-not #'(lambda (n)
					(let ((str (symbol-name n)))
					  (and (< 4 (length str))
					       (string= "dummy" (string-downcase str :end 5) :end2 5))))
				    lambda-list))
	 (dummy-ignore `((declare (ignore ,@dummy-vars)))))
    (multiple-value-bind (prefetch-binding of-body) (fetch-initial-let-binding body)
      (if (not prefetch-binding)
	  (progn
					;(warn "def-ofun prefetching ineffective in '~a'---function body doesn't start with LET or LET*." name)
	    `(progn
	       (defun ,name ,lambda-list
		 ,@body)
	       (defparameter ,(ofunc-obj-name name)
		 (make-ofunc :prefetch nil
			     :name ',name
			     :lambda-list ',lambda-list
			     :code '((cons 'progn ,(backquote-lambda-vars lambda-list body)))))))
	  `(progn
	     (defun ,name ,lambda-list
	       ,@dummy-ignore
	       ,@body)
	     (defparameter ,(ofunc-obj-name name)
	       (make-ofunc :prefetch ',prefetch-binding
			   :name ',name
			   :lambda-list ',lambda-list
			   :code '((cons 'progn ,(backquote-lambda-vars lambda-list of-body))))))))))

;; Macro for defining ofuncs that allows to specify alternative
;; macro code
(defmacro def-ofunc/macro (name lambda-list &body body)
  (let ((fn-start (position :function body))
	(mc-start (position :macro body)))
    (unless (and (numberp fn-start)
		 (numberp mc-start))
      (error "Can't find ':function' section or ':macro' section in def-ofunc/macro of ~a." name))
    (let ((fn-body (if (< fn-start mc-start)
		       (subseq body (1+ fn-start) mc-start)
		       (subseq body (1+ fn-start))))
	  (mc-body (if (< fn-start mc-start)
		       (subseq body (1+ mc-start))
		       (subseq body (1+ mc-start) fn-start))))
      `(progn
	 (defun ,name ,lambda-list
	   ,@fn-body)
	 (defparameter ,(ofunc-obj-name name)
	   (make-ofunc :prefetch ,(if (and (consp (car mc-body)) ; use a starting let-statement's bindings (if that exists) as prefetch-data
					   (or (eq (caar mc-body) 'let)
					       (eq (caar mc-body) 'let*)))
				      (cadar body))
		       :name ',name
		       :lambda-list ',lambda-list
		       :code ',mc-body))))))


;; Macro to be used in user code for requesting a instance-specific
;; compilation. The macro just invokes compile-instance-specific (see below)
;; passing the actual function parameters on as bindings.
(defmacro optimize-for-instance (vars ofunc-calls &body body)
  (let ((fn-name (gensym))
	(ofs (gensym)))

#|
    (pprint `(let ((,ofs (list ,@(mapcar #'second ofunc-calls))))
	       (compile ',fn-name `(lambda ,',vars
				     
				     (with-inlined-ofuncs ,(mapcar #'list ',the-ofunc-names ,ofs)
				       ,@',body)))
	       ',fn-name))
|#

    `(let ((,ofs (list ,@(mapcar #'second ofunc-calls)))
           (,fn-name (gensym)))
       (compile ,fn-name `(lambda ,',vars
                            (declare (optimize (speed 3) (debug 0) (safety 0))
                                     (sb-ext:muffle-conditions sb-ext:compiler-note style-warning)  ;; <- uncomment for lots of debugging output
				     )
                            (block ,',fn-name
                              (macrolet ((return-from-function (&rest args)
                                           (append (list 'return-from ',',fn-name) args)))
                                (with-inlined-ofuncs ,(mapcar #'list ',(mapcar #'car ofunc-calls) ,ofs)
                                  ,@',body)))))
       ,fn-name)))


;; Takes a form like (with-inlined-ofuncs
  
(defmacro with-inlined-ofuncs (ofunc-list &body body)
  (let* ((the-ofuncs (mapcar #'get-ofunc (mapcar #'second ofunc-list)))
	 (the-local-names (mapcar #'(lambda (spec) (if (listp (car spec)) (caar spec) (car spec))) ofunc-list))
#|	 (dummy (progn
		  (format t "~2% the-ofuncs = ~a~%the-local-names = ~a~%" the-ofuncs the-local-names)
		  (force-output))) |#
	 (lambda-conversion (apply #'append (mapcar #'(lambda (of-call of)
							;;(format t "~3%of-call = ~a~%of=~a" of-call of)(force-output)(break)
							(if (listp of-call) ; just a symbol or paired with variables?
							    (remove-if #'null (mapcar #'(lambda (var2call var-prefetch)
											  (if var2call
											      (list var-prefetch var2call)))
										      (cdr of-call) (ofunc-lambda-list of)))))
						    (mapcar #'car ofunc-list) the-ofuncs)))
	 (macro-code (mapcar #'(lambda (of name)
				 `(,name ,(ofunc-lambda-list of) ,@(ofunc-code of)))
			     the-ofuncs the-local-names)) 
	 (prefetch-bindings (apply #'append (mapcar #'ofunc-prefetch the-ofuncs))))
#|
    (format t "~%macro-code = ~a~%prefetch = ~a~%" macro-code prefetch-bindings)
    (format t "~%FABRIZIERE:~%")
    (pprint `(let (,@lambda-conversion ,@prefetch-bindings)
	       (macrolet (,@macro-code)
		 ,@body)))
    (force-output)
|#    
    `(let* (,@lambda-conversion
	    ,@prefetch-bindings)
       (macrolet (,@macro-code)
	 ,@body))))



;; Idee:        Den alias-Kram sich sparen und auf das Anlegen der Funktionen unter dem alias-Namen verzichten.
;; Stattdessen: Ein Macro, dass die ofuncs aus einem Objekt als lokale Funktionen zur Verfuegung stellt.

(defmacro with-ofuncs (ofunc-list &body body)
  (let ((the-ofuncs (mapcar #'second ofunc-list))
	(the-local-names (mapcar #'first ofunc-list)))
    `(labels ,(mapcar #'(lambda (ofunc fn-name)
			  `(,fn-name (&rest args)
			      (let ((of (get-ofunc ,ofunc)))
				(apply (ofunc-name of) args))))
		      the-ofuncs the-local-names)
       ,@body)))

(defun ofuncall (ofunc &rest args)
  "equivalent to funcall for ofuncs"
  (apply (ofunc-name (get-ofunc ofunc)) args))

