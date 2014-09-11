;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006 - 2010 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; COMPUTE-RELATION  --- package implementing SparQ user
;;; commands for computing with relations
;;;

;;; There is only one important function in this package: compute-relation
;;; This function parses the user's command and invokes the corresponding
;;; functions for computing. Evaluating complex e pressions such as e.g. 
;;; (converse (composition r1 r2) r2) is performed by compute-complex-expression
;;; Most of the code in this package is involved with error checking and error
;;; messages. For internal needs of computing with relation, the functions in
;;; calculi.lisp are invoked directly.

;; Change history (most recent first):
;; 2012-11-05 DW  added test-properties
;; 2012-09-21 DW  updated test of calculus properties
;; 2011-11-27 DW  change/fix: operator selection with string comparison (when did the old code break??)
;; 2010-05-05 DW  migration to new command architecture
;; 2007-01-26 DW  bug fix in read-relation macro wrt. to latest ofunc interface
;; 2007-01-25 DW  adapted to the new architecture of ofuncs and relation-representation as of V0.7
;; 2006-11-02 DW  started history aadapted SparQ's V0.6 compute-relation to new functions in calculi

(defpackage :compute-relation
  (:use :common-lisp :sparq :calculi :ofunc :relations)
  (:export :compute-relation))

(in-package :compute-relation)


(defvar +num-counterexamples+ 5
  "number of counter-examples to report in detail")

;; Macro that iterates over (unordered) pairs
;; (do-pairs (x '(1 2 3)) (y '(a b c)) (print (cons x y))) => (1 . 1) (1 . 2) (1 . 3) (2 . 2) (2 . 3) (3 . 3)
(defmacro do-pairs (vars list &body body)
  (let ((ls1 (gensym))
	(ls2 (gensym))
	(var1 (first vars))	
	(var2 (second vars)))
    `(do ((,ls1 ,list (cdr ,ls1)))
	 ((null ,ls1))
       (let ((,var1 (car ,ls1)))
	 (do ((,ls2 ,ls1 (cdr ,ls2)))
	     ((null ,ls2))
	   (let ((,var2 (car ,ls2)))
	     ,@body))))))

;; Computes the closure of a set of relations
;; If calculus is binary, closure is computed over the operations composition and converse,
;; otherwise (ternary) over the operations composition, inverse, homing, and shortcut

(defun closure (calculus relations)
  (declare (type list relations))
  (let* ((rel-rep (calculus-relation-representation calculus))
	 (func (optimize-for-instance (calculus rel-rep relations)
		   (((composition calculus) (calculi::calculus-composition calculus)) ; Consider these two calls in optimization...
		    (intersect   (relations::relation-representation-intersect rel-rep))
		    ((converse calculus) (calculi::calculus-converse calculus)))
		 
		 (let* ((rel< (symbol-function (relations:relation-representation-< rel-rep)))
			(iters 0)
			(count (length (the list relations)))
			(closure-set (reduce #'(lambda (set relation)
						 (data:r/b-tree-insert set relation rel<))
					     relations
					     :initial-value nil))
			(new-in-closure nil))
		 (declare (type fixnum iters count))

		 (labels ((register (r)
			    (unless (data:r/b-tree-find closure-set r rel<)
			      (push r new-in-closure)
			      (incf count)
			      (setq closure-set (data:r/b-tree-insert closure-set r rel<)))))
		 (when *debug*
		   (format *error-output* "~&;; Computing closure of ~a (a set with ~a relations)~%;; starting at ~a~%" 
			   (mapcar #'(lambda (r)
				       (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r))
				   relations)
			   count
			   (time-string)))
		 ;; Kickoff: relate the set of relations itself
		 ;; individually expand new relations
		 (dolist (r new-in-closure)
		   (register (converse calculus r)))
		 (do-pairs (r1 r2) relations
		   (register (composition calculus r1 r2))
		   (register (composition calculus r2 r1))
		   (register (intersect r1 r2)))
		   
		   ;; Iterate while new relations emerge
		   (loop while new-in-closure do
			(let ((this-round new-in-closure))
			  (when *debug*
			    (finish-output)
			    (format *error-output* ";; Starting iteration #~a with a total of ~a relations (~a new in last iteration) at ~a~%;; -> ~a steps~%" iters count (length new-in-closure) (time-string) (+ (length new-in-closure) (* (length new-in-closure) count) (expt (length new-in-closure) 2)))
			    #|
			    (multiple-value-bind (min sec) (floor (/ (+ (* (length this-round) count) (expt (length this-round) 2)) 
			    149889.0) 60)
			    (format *error-output* ";; I'm guessing this iteration takes about ~a minutes and ~a seconds~%" min sec))			    |#
			    (finish-output *error-output*)
			    (incf iters))
			
			  (setq new-in-closure nil)

			  ;; individually expand new relations
			  (dolist (r this-round)
			    (register (converse calculus r)))
			  ;; relate newly added relations to themselves
			  ;(print "done single expand")
			  
			  (do-pairs (r1 r2) this-round
			    (register (composition calculus r1 r2))
			    (register (composition calculus r2 r1))
			    (register (intersect r1 r2)))

			  ;(print "done new x new")
			  ;; relate 'old' relations to the new ones
			  
			  
			  (dolist (r1 (data:r/b-tree->list closure-set))
			    (dolist (r2 this-round)
			      (register (composition calculus r1 r2))
			      (register (composition calculus r2 r1))
			      (register (intersect r1 r2))))
			  
					;(print "done new x old")
			  ;; iteration done
			  ))
		   (when *debug* 
		     (format *error-output* ";; done at ~a: ~a relations after ~a iterations~%" (time-string) count iters))
		   (data:r/b-tree->list closure-set))))))    
    (funcall func calculus rel-rep relations)))

(defun closure/alt (calculus relations)
  (declare (type list relations))
  (let* ((rel-rep (calculus-relation-representation calculus))
	 (rel< (symbol-function (relations:relation-representation-< rel-rep)))
	 (iters 0)
	 (count (length (the list relations)))
	 (closure-set (reduce #'(lambda (set relation)
				  (data:r/b-tree-insert set relation rel<))
			      relations
			      :initial-value nil))
	 (new-in-closure nil))
    (declare (type fixnum iters count))
    (labels ((combine (r1 r2)
	       (list (calculi:composition  calculus r1 r2)
		     (calculi:composition calculus r2 r1)
		     (calculi:intersect calculus r1 r2)))
	     (expand (r)
	       (list (calculi:converse calculus r)))
			(register (rs)
			  (dolist (r rs)
			    (unless (data:r/b-tree-find closure-set r rel<)
			      (push r new-in-closure)
			      (incf count)
				(setq closure-set (data:r/b-tree-insert closure-set r rel<))))))
		 (when *debug*
		   (format *error-output* "~&;; Computing closure of ~a (a set with ~a relations)~%;; starting at ~a~%" 
			   (mapcar #'(lambda (r)
				       (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r))
				   relations)
			   count
			   (time-string)))
		 ;; Kickoff: relate the set of relations itself
		 ;; individually expand new relations
		 (dolist (r new-in-closure)
		   (register (expand r)))
		   (do-pairs (r1 r2) relations
		     (register (combine r1 r2)))
		   
		   ;; Iterate while new relations emerge
		   (loop while new-in-closure do
			(let (next-round)
			  (when *debug*
			    (format *error-output* ";; Starting iteration #~a with ~a relations at ~a~%" iters count (time-string))
			    (finish-output *error-output*)
			    (incf iters))
			  ;; individually expand new relations
			  (dolist (r new-in-closure)
			    (register (expand r)))
			  ;; relate newly added relations to themselves
			  (do-pairs (r1 r2) new-in-closure
			    (register (combine r1 r2)))
			  ;; relate 'old' relations to the new ones
			  (do ((r1-node (data:r/b-tree-min-node closure-set) (data:r/b-node-successor r1-node)))
			      ((null r1-node))
			    (let ((r1 (data:r/b-node-data r1-node)))
			    (dolist (r2 new-in-closure)
			      (register (combine r1 r2)))))
			;; iteration done
			  (setq new-in-closure next-round)))
		   (when *debug* 
		     (format *error-output* ";; done at ~a: ~a relations after ~a iterations~%" (time-string) count iters))
		   (data:r/b-tree->list closure-set))))

;; Two macros that signal an error to the user if the arity of a calculus isn't
;; OK, e.g. when asking to compute homing in a binary calculus
(defmacro ensure-binary-calculus (calculus &body body)
  `(if (eq (calculus-arity ,calculus) :binary)
     (progn
       ,@body)
     (signal-error "Calculus '~a' is no binary calculus!~%" (calculi:calculus-name ,calculus))))

(defmacro ensure-ternary-calculus (calculus &body body)
  `(if (eq (calculus-arity ,calculus) :ternary)
     (progn
       ,@body)
     (signal-error "Calculus '~a' is no ternary calculus!~%" (calculi:calculus-name ,calculus))))

(defun compute-complex-expression (calculus expression)
  (let* ((rel-rep (calculi:calculus-relation-representation calculus))	 
	 (encoder  (relations:relation-representation-encoder rel-rep)))
    (labels ((eval-expression (exp)
	       (if (atom exp)
		   (ofuncall encoder rel-rep exp)
		   (let ((fn (first exp))
			 (args (rest exp)))
		     (let ((func (assoc (format nil "~a" fn) (list (cons "SHORTCUT" 'shortcut)
								   (cons "SC" 'shortcut)
								   (cons "SHORTCUTI" #'(lambda (c r) (inverse c (shortcut c r))))
								   (cons "SCI" #'(lambda (c r) (inverse c (shortcut c r))))
								   (cons "HOMING" 'homing)
								   (cons "HM" 'homing)
								   (cons "HOMINGI" #'(lambda (c r) (inverse c (homing c r))))
								   (cons "HMI" #'(lambda (c r) (inverse c (homing c r))))
								   (cons "CONVERSE" 'converse)
								   (cons "CNV" 'converse)
								   (cons "INV" 'inverse)
								   (cons "INVERSE" 'inverse)
								   (cons "CMPL" #'(lambda (c r) (compl c r)))
								   (cons "COMPLEMENT" #'(lambda (c r) (compl c r))))
					:test #'string=)))
		       ;;(format t "~%fn=~w ~a... func=~w~%" fn (type-of fn) func)
		       (if func ; handle unary function
			   (if (= 1 (length args))
			       (funcall (cdr func) calculus (eval-expression (car args)))
			       (signal-error "~%Syntax error: ~d argument~:P to unary operation ~a." (length args) (car func)))
			   (let ((func (assoc fn (let ((tmp (list (cons "COMPOSITION" 'composition)
								  (cons "UNION" #'(lambda (c r1 r2) (calculi:unite c r1 r2)))
								  (cons "MINUS" #'(lambda (c r1 r2) (calculi:minus c r1 r2)))
								  (cons "ISEC" #'(lambda (c r1 r2) (calculi:intersect c r1 r2)))
								  (cons "INTERSECTION" #'(lambda (c r1 r2) (calculi:intersect c r1 r2)))
								  (cons "EQUALS" #'(lambda (c r1 r2) (declare (ignore c)) (equal r1 r2)))
								  (cons "COVERS" #'(lambda (c r1 r2)
										     (and (equal (calculi:intersect c r1 r2)
												 r2)
											  (not (equal r1 r2)))))
								  (cons "COVERSEQ" #'(lambda (c r1 r2)
										       (equal (calculi:intersect c r1 r2)
											      r2)))
								  (cons "ISCOVERED" #'(lambda (c r1 r2)
											(and (equal (calculi:intersect c r1 r2)
												    r1)
											     (not (equal r1 r2)))))
								  (cons "ISCOVEREDEQ" #'(lambda (c r1 r2)
											  (equal (calculi:intersect c r1 r2)
												 r1))))))
						   (when (eq :binary (calculus-arity calculus))
						     (push (cons "NCOMP" 'composition) tmp)
						     (push (cons "N-ARY-COMPOSITION" 'composition) tmp))
						   tmp)
					      :test #'string=)))
			     (if func ; handle binary function
				 (if (and (cdr args) (null (cddr args)))
				     (funcall (cdr func) calculus (eval-expression (car args)) (eval-expression (cadr args)))
				     (signal-error "~%Syntax error: ~a arguments to binary operation ~a." (length args) (car func)))
				 (let ((func (assoc fn (let ((tmp (list (cons 'cl-user::ternary-composition 'ternary-composition)
									(cons 'cl-user::tcomp 'ternary-composition))))
							 (when (eq :ternary (calculus-arity calculus))
							   (push (cons 'cl-user::n-ary-composition 'ternary-composition) tmp)
							   (push (cons 'cl-user::ncomp 'ternary-composition) tmp))))))
				   (if func ; handle ternary function
				       (if (and (cddr args) (null (cdddr args)))
					   (funcall (cdr func) calculus (eval-expression (car args)) (eval-expression (cadr args)) (eval-expression (caddr args)))
					   (signal-error "~%Syntax error: ~a arguments to ternary operation ~a." (length args) (car func)))
				       (multiple-value-bind (relation error) (ignore-errors (ofuncall encoder rel-rep exp))
					 (if error                             
					     (signal-error "~%Syntax error: Operation ~a unknwown and expression ~a cannot be interpreted as list of relations" fn exp)
					     relation))))))))))))
      (eval-expression expression))))


(defmacro read-relation (list calculus)
  (let ((r (gensym))
	(rel-rep (gensym)))
    `(let ((,r (if ,list (pop ,list)
                   (sparq:signal-error "Error: At least one more relation required!")))
	   (,rel-rep (calculi:calculus-relation-representation ,calculus)))
       (ofuncall (relations:relation-representation-encoder ,rel-rep)
		 ,rel-rep
		 ,r))))


(defun classify-relations/ternary (calculus)
  (declare (ignore calculus))
  (format *sparq-io* "~%not implemented yet!")
)

(defun classify-relations/binary (calculus)
  (let* ((ids ())
	 (rel-rep (calculi:calculus-relation-representation calculus))
	 (decoder (relations:relation-representation-decoder rel-rep))
	 (br-enc  (relations:relation-representation-br-encodings rel-rep))
	 (n       (relations:relation-representation-num-base-relations rel-rep))
	 (same-rel? (relations:relation-representation-same-relation? rel-rep)))
    
    (dotimes (i n)
      (let ((r1 (svref br-enc i))
	    (id? t))
	(do ((j 1 (+ j 1)))
	    ((eq j n))
	  (let ((r2 (svref br-enc j)))
	    (unless (and (ofuncall same-rel? (calculi:composition calculus r1 r2)
				   r2)
			 (ofuncall same-rel? (calculi:composition calculus (svref br-enc j) r1)
				   r2))
	      (setq id? nil)
	      (return))))
	(when id?
	  (push r1 ids))))
    (format *sparq-io* "~%;;  There ~:[are~;is~] ~d relation~:P exhibiting properties of an identiy relation (~{~a ~})." 
	    (eq 1 (length ids))
	    (length ids)
	    (mapcar #'(lambda (r) (ofuncall decoder rel-rep r)) ids))))

(defcommand ("analyze-calculus" (c calculus) "test-relations")
    "checks base relations for relation properties"
  (if (eq :binary (calculi:calculus-arity c))
      (classify-relations/binary c)
      (classify-relations/ternary c)))

;; A special operator that performs a test on a calculus. If the test fails for some relation,
;; this is signaled to the user as in this case there might be an error in the calculus definition.
;; Of course we can't tell whether failure of a test really points to an error since the calculus
;; may have some odd properties, e.g. converse is not idempotent.
;; Test for ternary calculi:
;;   (inverse (shortcut r)) ^ 3 = a
;; Test for binary calculi:
;;   (composition r1 r2) == (converse (composition (converse r2) (converse r1)))
(defun do-calculus-test (calculus)
  (let* ((ok?        t) ; test passed?
	 (rel-rep    (calculi:calculus-relation-representation calculus))
	 (decoder    (relations:relation-representation-decoder rel-rep))
	 (br-enc     (relations:relation-representation-br-encodings rel-rep))
	 (rel-mapper (relations:relation-representation-mapper rel-rep)))
    (if (eq :ternary (calculi:calculus-arity calculus))
	;; Test for ternary calculi
	(progn 
	  ;; test ternary composition
	  (when (calculi:calculus-n-ary-composition calculus)
	    (let ((test-ok? t))
	      (flet ((rotation (r)
		       (calculi:shortcut calculus (calculi:inverse calculus r)))
		     (decode (r)
		       (ofuncall decoder rel-rep r)))
		(ofuncall rel-mapper
			  #'(lambda (r1-idx)
			      (let ((r1 (svref br-enc r1-idx)))
				(ofuncall rel-mapper 
					  #'(lambda (r2-idx)
					      (let ((r2 (svref br-enc r2-idx)))
						(ofuncall rel-mapper
							  #'(lambda (r3-idx)
							      (let* ((r3 (svref br-enc r3-idx))
								     (comp0 (calculi:ternary-composition calculus r1 r2 r3))
								     (comp1 (rotation comp0))
								     (comp2 (calculi:ternary-composition calculus (rotation r3) (rotation r1) (rotation r2)))
								     (comp3 (calculi:shortcut calculus comp0))
								     (comp4 (calculi:ternary-composition calculus (calculi:shortcut calculus r2) (calculi:shortcut calculus r1) (calculi:shortcut calculus r3))))
								(unless (ofuncall (relations:relation-representation-same-relation? rel-rep) comp1 comp2)
								  (setq test-ok? nil)
								  (print-out "FAILED§b0§ test *(~a, ~a, ~a)^ROT == ~a != ~a == *(~a^ROT ~a^ROT ~a^ROT)~%"
									     (decode r1) (decode r2) (decode r3) (decode comp1)
									     (decode comp2) (decode r3) (decode r1) (decode r2)))
								(unless (ofuncall (relations:relation-representation-same-relation? rel-rep) comp3 comp4)
								  (setq test-ok? nil)
								  (print-out "§b1§FAILED§b0§ test (sc *(~a ~a ~a)) == ~a != ~a == *( sc ~a, sc ~a, sc ~a)~%"
									     (decode r1)  (decode r2) (decode r3) (decode comp3)
									     (decode comp4) (decode r2)  (decode r1) (decode r3)))))
							  (relations:relation-representation-universal-relation rel-rep))))
					  (relations:relation-representation-universal-relation rel-rep))))
			  (relations:relation-representation-universal-relation rel-rep)))
	      (when test-ok?
		(print-out "~&Tests passed *(r1,r2,r3)^ROT == *(r3^ROT, r1^ROT, r2^ROT) and sc(*(r1,r2,r3)) == *(sc r2, sc r1, sc r3)~%"))))
	  
	  ;; test  r == (inverse (shortcut r))^3
	  (ofuncall rel-mapper
		    #'(lambda (r1-idx)
			(let* ((r1 (svref br-enc r1-idx))
			       (r2 r1))
			  (dotimes (i 3)
			    (setq r2 (calculi:inverse calculus (calculi:shortcut calculus r2))))
			  (unless (equal r2 r1)
			    (setq ok? nil)
			    (print-out "Failed test r == (inverse (shortcut r))^3 for relation ~a, computed ~a~%" 
				       (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r1) 
				       (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r2)))))
		    (relations:relation-representation-universal-relation rel-rep)))
	;; Test for binary calculi
	;; we check properties of relation algebras
	;;
	(let ((test-func
	       (optimize-for-instance ; Compile a function to be called 
		;; The variables passed to the function
		(calculus rel-rep u id br-enc +num-counterexamples+)
		;; Optimize these calls:
		(((composition calculus)     (calculi::calculus-composition calculus))
		 (intersect                  (relations:relation-representation-intersect rel-rep))
		 ((empty-rel? rel-rep)       (relations::relation-representation-empty-relation? rel-rep))
		 ((same-rel? rel-rep)        (relations::relation-representation-same-relation? rel-rep))
		 ((rel-mapper rel-rep)       (relations::relation-representation-mapper rel-rep))
		 ((converse calculus)        (calculi::calculus-converse calculus)))
		(let ((violations1 ())
		      (violations2 ())
		      (violations3 ())
		      (violations4 ())
		      (violations5 ())
		      (v1  0)
		      (v2  0)
		      (v3  0)
		      (v4  0)
		      (v5  0))
		  (declare (type integer v1 v2 v3 v4 v5)
			   (type list violations1 violations2 violations3 violations4 violations5))
		  (rel-mapper
		   #'(lambda (r1-idx)
		       (let ((r1 (svref br-enc r1-idx)))
			   ;; Test (1)
			 (when (or (not (same-rel? r1 (composition calculus r1 id)))
				   (not (same-rel? r1 (composition calculus id r1))))
			   (incf v1)
			   (when (<= v1 +num-counterexamples+)
			     (push r1 violations1)))
			 ;; Test (2)
			 (unless (same-rel? r1 (converse calculus (converse calculus r1)))
			   (incf v2)
			   (when (<= v2 +num-counterexamples+)
			     (push r1 violations2)))
			   (rel-mapper
			    #'(lambda (r2-idx)
				(let* ((r2 (svref br-enc r2-idx))
				       (r3-1 (converse calculus (composition calculus r1 r2)))
				       (r3-2 (composition calculus (converse calculus r2) (converse calculus r1))))
				  ;; Test (3)
				  (unless (equal r3-1 r3-2)
				    (incf v3)
				    (when (< v3 +num-counterexamples+)
				      (push (list r1 r2 r3-1 r3-2) violations3)))
				  
				  ;; Test (4), (5)
				  (rel-mapper
				   #'(lambda (r3-idx)
				       (let* ((r3 (svref br-enc r3-idx))
					      (lhs (intersect (composition calculus r1 r2) (converse calculus r3)))
					      (rhs (intersect (composition calculus r2 r3) (converse calculus r1)))
					      (lhs5 (composition calculus r1 (composition calculus r2 r3)))
					      (rhs5 (composition calculus (composition calculus r1 r2) r3)))
					 (unless (eq (empty-rel? lhs)
						     (empty-rel? rhs))
					   (incf v4)
					   (when (<= v4 +num-counterexamples+)
					     (push (list r1 r2 r3 lhs rhs) violations4)))
					 
					 (unless (same-rel? lhs5 rhs5)
					   (incf v5)
					   (when (<= v5 +num-counterexamples+)
					     (push (list r1 r2 r3 lhs5 rhs5) violations5)))))
				   u)))
			    u)))
		   u)
		  (values v1 violations1 v2 violations2 v3 violations3 v4 violations4 v5 violations5)))))
	  (report-time "test-algebra setup/compile")
	  (print-out "~%;; testing non-trivial relation algebra axioms")
	  (multiple-value-bind (v1 violations1 v2 violations2 v3 violations3 v4 violations4 v5 violations5) 
	      (funcall test-func calculus 
		       rel-rep 
		       (relations:relation-representation-universal-relation rel-rep) 
		       (ofuncall (relations:relation-representation-encoder rel-rep) rel-rep (calculi:calculus-identity-relation calculus)) 
		       (relations:relation-representation-br-encodings rel-rep) 
		       +num-counterexamples+)
	    (report-time "test-algebra")
	    (flet ((test-report (str1 str2 v violations)
		     (print-out str1)
		     (if violations
			 (progn
			     (setq ok? nil)
			     (print-out "~%;; test §b1§FAILED§b0§ for ~d base relation~:P ~a, ~d counter-example~:P: ~{~a ~}" 
					v  str2 (length violations) violations))
			 (print-out "~%;; test passed")))
		   (decode (r)
		     (ofuncall decoder rel-rep r)))

	      (test-report "~%;; (1) testing identity relation: for all base relations r: r o id = id o r = r"
			   ""
			   v1 
			   (mapcar #'decode violations1))

	      (test-report "~%;; (2) testing converse: for all base relations r: (converse (converse r)) = r"
			   ""
			   v2
			   (mapcar #'decode violations2))

	      (test-report "~%;; (3) testing converse/composition: for all relations r1, r2:~%;;     (converse (composition r1 r2)) == (composition (converse r2) (converse r1))"
			   "pairs"
			   v3
			   (mapcar #'(lambda (v)
					 (let ((rels (mapcar #'decode v)))
					   (format nil "\~%;;    (composition ~a ~a) = ~a /= ~a = (converse (composition (converse ~a) (converse ~a)))"
						   (first rels) (second rels) (third rels) (fourth rels) (second rels) (first rels))))
				   violations3))
	      
	      (test-report "~%;; (4) testing for all relations r,s,t:~%;;     (intersection (composition r s) (converse t)) = () if and only if (intersection (composition s t) (converse r)) = ()"
			   "triples"
			   v4
			   (mapcar #'(lambda (v)
				       (let ((rels (mapcar #'decode v)))
					 (format nil "\~%;;    (intersection (composition ~a ~a) (converse ~a)) = ~a /= ~a = (intersection (composition ~a ~a) (converse ~a))"
						 (first rels) (second rels) (third rels) (fourth rels) (fifth rels) (second rels) (third rels) (first rels))))
				   violations4))
	      
	      (test-report "~%;; (5) testing for all relations r,s,t:~%;; (compose r (compose s t)) = (compose (compose r s) t)"
			     "triples"
			     v5
			     (mapcar #'(lambda (v)
					 (let ((rels (mapcar #'decode v)))
					   (format nil "\~%;;    (compose ~a (compose ~a ~a)) = ~a /= ~a = (compose (compose ~a ~a) ~a)"
						   (first rels) (second rels) (third rels) (fourth rels) (fifth rels) (first rels) (second rels) (third rels))))
				     violations5)))
	    
	    (if ok?
		(print-out "~%§b1§Tests passed§b0§, calculus is a relation algebra.")
		(print-out "~%§b1§Tests failed§b0§, calculus is no relation algebra.")))))))

#|
(defun calculus-analysis (stream calculus)
  (let* ((basis-entity (calculus-basis-entity calculus))
	 (rel-rep (calculus-relation-representation calculus))
	 (base-rels (relations:relation-representation-base-relations rel-rep))
	 (num-rels (relations:relation-representation-num-base-relations rel-rep))
	 (arity (calculus-arity calculus))
	 (obj-params (cadr (assoc basis-entity '((:2d-point 2) (:dipole 4) (:2d-oriented-point 4) (:1d-point 1) (:3d-point 3) (:2d-box 4))))) ; FIXME: doesn't handle polygons
	 (can-determine-probs? (and obj-params (calculi::calculus-qualifier calculus)))
	 (probs (if can-determine-probs?
		    ;; if qualify is available we can determine the distribution of probalities that some
		    ;; base-relation occurs by qualifying randomly generated instantiations
		    (let* ((count (make-array num-rels :initial-element 0))
			   (o1 (make-list obj-params)))
		      ;; preset variables that we don't need to change
		      (flet ((rlist ()
			       (loop for i from 1 to obj-params collecting (- (random 20) 10))))
			(dotimes (i obj-params)
			  (setf (nth i o1) i))
			(dotimes (i 100000)
			  (let ((rel (if (eq arity :binary)
					 (funcall (calculi::calculus-qualifier calculus) o1 (rlist))
					 (funcall (calculi::calculus-qualifier calculus) o1 (rlist) (rlist)))))
			    (incf (aref count (position rel base-rels))))))
		      (dotimes (i num-rels)
			(setf (aref count i) (/ (max 1 (aref count i)) 100000.0)))
		      count)
		    ;; assume equal distribution otherwise
		    (make-array num-rels :initial-element (/ 1.0 num-rels)))))

    (let ((icont (coerce (loop for pr across probs collecting (- (log pr num-rels))) 'vector)) ; information content
	  (rel-encs (relations:relation-representation-br-encodings rel-rep)))
      (format stream "~&Estimate of probabilities of relation occurence: ")
      (if can-determine-probs?
	  (loop for br across base-rels
	        for pr across probs 
	        for ic across icont do
	       (format stream "~%~8A : ~4f  (~4f)" br pr ic))
	  (format stream "cannot be determined, assuming equal probability of ~f" (aref probs 0)))
      (format stream "~2%composition table analysis:")
      (loop for r1 across rel-encs do
	   (loop for r2 across rel-encs do
		(let ((comp (ofuncall (calculi::calculus-composition calculus) calculus r1 r2))
		      (ic (log num-rels 2)))
		  (ofuncall (relations:relation-representation-mapper rel-rep) #'(lambda (idx)
										   (setf ic (min ic (aref icont idx))))
			    comp)
		  (format stream "~%~8A ~8A -> ~F" 
			  (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r1) 
			  (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r2)
			  ic))))

      (format stream "~%das war's fuers erste...~%"))))
|#

(defcommand ("analyze-calculus" (c calculus) "test-algebra")
  "checks which axioms are met by the algebraic structure of the calculus"
  (do-calculus-test c)
  (make-instance 'sparq:silent-value :value ()))

(defun compute-relation (calculus args)
  "Main function for handling compute-relation commands"
  (labels ((make-rel (r)
	     (make-instance 'calculi:relation
			    :value r
			    :calculus calculus)))
    (let ((operator (and args
			 (pop args)))
	  (rel-rep (calculi:calculus-relation-representation calculus)))
      (cond ((or (eq operator 'cl-user::self-test)
		 (eq operator 'cl-user::test-properties)) (with-output-to-string (str)
							    (do-calculus-test calculus)))

	    ;((eq operator 'cl-user::operation-analyis) (with-output-to-string (str)
	    ;(calculus-analysis str calculus)))
	    
	    ((listp operator)  (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep (compute-complex-expression calculus operator)))
	    
	    ((eq operator 'cl-user::base-closure)
	     (mapcar #'make-rel (closure calculus (coerce (relations:relation-representation-br-encodings (calculus-relation-representation calculus)) 'list))))

	    ((eq operator 'cl-user::closure) 
	     (unless (listp (car args))
	       (signal-error "Argument to closure is not a list.~%Usage: compute-relation <calculus> closure <list-of-relations>"))
	     (mapcar #'make-rel (closure calculus (mapcar #'(lambda (r) (ofuncall (relations:relation-representation-encoder rel-rep) rel-rep r))
							  (car args)))))

	    (t 
	     (let ((relation (read-relation args calculus)))
	       (if (null operator)
		   (signal-error "~%Usage: sparq compute-relation <CALCULUS> <OPERATOR> <RELATION>~%")
		   (let ((op (symbol-name operator)))
		     (cond ;((eq operator 'cl-user::reflection )
					; (print-relation stream (reflection calculus relation)))
		       ((find op '("converse" "cnv") :test #'string-equal)
			(ensure-binary-calculus calculus
						(make-rel (converse calculus relation))))
		       
		       ((find op '("inverse" "inv") :test #'string-equal)
			(ensure-ternary-calculus calculus
						 (make-rel (inverse calculus relation))))
		       
		       ((find op '("shortcut" "sc") :test #'string-equal)
			(ensure-ternary-calculus calculus
						 (make-rel (shortcut calculus relation))))
		       
		       ((find op '("shortcuti" "sci") :test #'string-equal)
			(ensure-ternary-calculus calculus
						 (make-rel (inverse calculus (shortcut calculus relation)))))
		       
		       ((find op '("homing" "hm") :test #'string-equal)
			(ensure-ternary-calculus calculus
						 (make-rel (homing calculus relation))))
		       
		       ((find op '("homingi" "hmi") :test #'string-equal)
			(ensure-ternary-calculus calculus
						 (make-rel (inverse calculus (homing calculus relation)))))
		       
		       ((find op '("composition" "comp" "binary-composition") :test #'string-equal)
			(let ((relation2 (read-relation args calculus)))
			  (make-rel (composition calculus relation relation2))))
		       
		       ((string-equal op "union")
			(let ((relation2 (read-relation args calculus)))
			  (make-rel (unite calculus relation relation2))))
		     
		       ((find op '("intersection" "isec") :test #'string-equal)
			(let ((relation2 (read-relation args calculus)))
			  (make-rel (calculi:intersect calculus relation relation2))))
		       
		       ((string-equal op "minus")
		      (let ((relation2 (read-relation args calculus)))
                        (make-rel (calculi:minus calculus relation relation2))))
		       
		       ((find op '("complement" "cmpl") :test #'string-equal)
			(make-rel (compl calculus relation)))
		       
		       ((find op '("ternary-composition" "tcomp") :test #'string-equal)
			(ensure-ternary-calculus calculus
						 (make-rel (ternary-composition calculus relation (read-relation args calculus) (read-relation args calculus)))))		     
		       
		       ((find op '("n-ary-composition" "ncomp") :test #'string-equal)
			(if (eq :binary (calculus-arity calculus))
			    (make-rel (composition calculus relation (read-relation args calculus)))
			    (make-rel (ternary-composition calculus relation (read-relation args calculus) (read-relation args calculus)))))
		       (t               
			(signal-error "Error: Operator ~a unknown!~%" operator)))))))))))


(defcommand ("compute-relation" (c calculus) &rest expression)
  "perform relation-algebraic operations"
  (compute-relation c (read-from-string (format nil "~a" expression))))

(defun sample-k-composition-density (calculus k time)
  "randomly selects chains of relation compositions, counting the size of disjunctions obtained"
  (let* ((relrep (calculi:calculus-relation-representation calculus))
	 (rel-mapper (relations:relation-representation-mapper relrep))
	 (u (relations:relation-representation-universal-relation relrep))
	 (n (relations:relation-representation-num-base-relations relrep))
	 (br-enc (relations:relation-representation-br-encodings relrep))
	 (sampleflags (let ((a (make-array k)))                       ;; flags for chains of atomic relations already considered to avoid re-sampling
			(dotimes (i k)
			  (setf (aref a i) (make-array (expt n (+ i 2)) :element-type 'bit :initial-element 0)))
			a))
	 (relcounts (make-array k :initial-element 0))                ;; total # of atomic relations resulting from composition per depth of composition chain
	 (relhists (make-array (list k (+ n 1)) :initial-element 0))  ;; distribution of size of disjunctions (0...n) per depth of composition chaing
	 (samplecount (make-array k :initial-element 0))              ;; samples taken per depth of composition chain
	 (overlaps (make-array (list k) :initial-element 0))          ;; overlap of relations
	 (last-rels (make-array (list k) :initial-element u))         ;; tmp storage for last relation to compute overlaps
	 (stoptime (+ (get-internal-real-time) (* time 1000))))       ;; time to finish sampling
    (loop while (< (get-internal-real-time) stoptime) do
	 (dotimes (i 10000) ; don't check time after every iteration
	   (let* ((ridx (random n))
		  (r (svref br-enc ridx))
		  (sampleid ridx))
	     (dotimes (s k) ; go along chain up to k steps
	       (let* ((ridx (random n))
		      (r2 (svref br-enc ridx)))
		 (setq r (calculi:composition calculus r r2)
		       sampleid (+ ridx (* n sampleid)))
		 (unless (= 1 (aref (aref sampleflags s) sampleid)) ; sampled before?
		   (setf (aref (aref sampleflags s) sampleid) 1)
		   (incf (aref samplecount s))
		   (let ((cnt 0))
		     (ofuncall rel-mapper
			       #'(lambda (dummy)
				   (declare (ignore dummy))
				   (incf cnt))
			       (calculi:intersect calculus r (aref last-rels s)))
		     (incf (aref overlaps s) cnt))
		   (let ((cnt 0))
		     (ofuncall rel-mapper
			       #'(lambda (dummy)
				   (declare (ignore dummy))
				   (incf cnt)
				   (incf (aref relcounts s)))
			       r)
		     (incf (aref relhists s cnt)))
		   (setf (aref last-rels s) r)))))))
    (let ((stddevs (make-array k :initial-element 0.0)))
      (dotimes (s k)
	(if (< 1 (aref samplecount s))
	    (setf (aref relcounts s) (/ (aref relcounts s) (aref samplecount s) n)
		  (aref overlaps s) (/ (aref overlaps s) (- (aref samplecount s) 1) n))
	    (setf (aref relcounts s) 0
		  (aref overlaps s) 0))
	(let ((avg (aref relcounts s))
	      (cnt 0))
	  (dotimes (i (+ n 1))
	    (incf cnt (* (aref relhists s i) (expt (- i avg) 2))))
	  (unless (= 0 (aref samplecount s))
	    (setf (aref stddevs s) (/ (sqrt (/ cnt (aref samplecount s))) n)
		  (aref samplecount s) (/ (aref samplecount s) (expt n (+ s 2)))))))
      (values relcounts    ;; array of k elements: avg. information content after k step composition (= avg. disjunction size / # base-relations)
	      samplecount  ;; array of k elements: sample coverage in %
	      stddevs      ;; array of k elements: std. deviation of the avg. information content
	      overlaps)))) ;; array of k elements: avg. degree of overlap between relations that occur after k steps

(defcommand ("analyze-calculus" (c calculus) "algebra-stats" (ti (or null (integer 0 3600))))
    "determines some statistical parameters for a given calculus by sampling for some time (default 10 sec.)"
  (if (eq :binary (calculi:calculus-arity c))
      (let* ((relrep (calculi:calculus-relation-representation c))
	     (n (relations:relation-representation-num-base-relations relrep)))
	(print-out "~&;; computing statistics, please be patient...")
	(print-out "~%base relations~20T: ~d" n)
	(print-out "~%information content of k-step composition (max. value k will be determined by memory requirements)")	
	;(print-out "~%1-step~20T: ~4,2f [exact, +/- 0]" (- 1 (composition-density c)))
	(let ((k (floor (log (* 2 1024 1024 8) n)))) ; limit depth k by est. memory requirements
	;(let ((k (ceiling (log (* 2 1024 1024 8) n)))) ; limit depth k by est. memory requirements
	  (multiple-value-bind (k-step k-samples k-std k-overlap) (sample-k-composition-density c k (or ti 10))
	    (dotimes (i k)
	      (print-out "~%~2d-step~20T: ~4,2f [~6,2f% sample coverage; std dev. ~4,2f; relation overlap ~4,2f]"
			 (+ i 1) 
			 (- 1 (aref k-step i)) 
			 (* 100 (aref k-samples i))
			 (aref k-std i)
			 (aref k-overlap i)))))
	(make-instance 'sparq:silent-value :value ())) ; don't return a value to be printed
      (sparq:signal-error "command only available for binary calculi")))

(defun substitute-var (var val expression)
  "replaces symbol 'var' by 'val' in nested list structure 'expression'"
  (if expression
      (if (listp expression)
	  (mapcar #'(lambda (x) (substitute-var var val x)) expression)
	  (if (eq var expression) val expression))))

(defun bitfield-to-relation (relrep num)
  "constructs a general relation from a number, interpreting bits as switches for base-relations"
  (let ((r (relation-representation-empty-relation relrep))
	(i 0))
    (loop while (< 0 num) do
	 (if (= 1 (mod num 2))
	     (setq r (ofuncall (relation-representation-unite relrep) r (svref (relation-representation-br-encodings relrep) i))))
	 (setq num (floor num 2)
	       i   (+ i 1)))
    (read-from-string (ofuncall (relation-representation-decoder relrep) relrep r))))

(defun test-prop (calculus ranges expression)
  "tests an expression containing qualified variables 'ranges'"
  (if (null ranges)
      (compute-complex-expression calculus expression)
      (destructuring-bind ((qualif var range) . rest) ranges
	(let* ((rel-rep (calculus-relation-representation calculus))
	       (n (relations:relation-representation-num-base-relations rel-rep))
	       (baserels (relations:relation-representation-base-relations rel-rep)))
	  
	  ;; stupid case switch for different combinations of exists / forall and ranges baserel / rel
	  (cond ((and (eq range 'cl-user::baserel)  ; exists baserel...
		      (eq qualif 'cl-user::exists))
		 (let ((default-info ()))
		   (dotimes (i n (values nil default-info))
		     (let ((r (svref baserels i)))
		       (multiple-value-bind (ok info) (test-prop calculus rest (substitute-var var r expression))
			 (unless default-info (setq default-info info))
			 (when ok
			   (return (values ok
					   (cons (list var '<- r) info)))))))))

		((and (eq range 'cl-user::rel) ; exists rel...
		      (eq qualif 'cl-user::exists))
		 (let ((default-info ()))
		   (dotimes (i (expt 2 n) (values nil default-info))
		     (let ((r (bitfield-to-relation rel-rep i)))
		       (multiple-value-bind (ok info) (test-prop calculus rest (substitute-var var r expression))
			 (unless default-info (setq default-info info))
			 (when ok
			   (return (values ok
					   (cons (list var '<- r) info)))))))))

		((and (eq range 'cl-user::baserel) ; forall baserel...
		      (eq qualif 'cl-user::forall))
		 (let ((default-info ()))
		   (dotimes (i n (values t default-info))
		     (let ((r (svref baserels i)))
		       (multiple-value-bind (ok info) (test-prop calculus rest (substitute-var var r expression))
			 (unless default-info (setq default-info info))
			 (unless ok
			   (return (values nil
					   (cons (list var '<- r) info)))))))))
	       
		((and (eq range 'cl-user::rel) ; forall rel...
		      (eq qualif 'cl-user::forall))
		 (let ((default-info ()))
		   (dotimes (i (expt 2 n) (values t default-info))
		     (let ((r (bitfield-to-relation rel-rep i)))
		       (multiple-value-bind (ok info) (test-prop calculus rest (substitute-var var r expression))
			 (unless default-info (setq default-info info))
			 (unless ok
			   (return (values nil
					   (cons (list var '<- r) info)))))))))

	      (t (signal-error "quantifier/range not recognized for variable ~a" var)))))))

(defcommand ("analyze-calculus" (c calculus) "test-property" &rest expression)
    "tests a property in context of a qualitative calculus"
    (let* ((exp  (read-from-string (format nil "~a" expression)))
	   (prop (car (last exp)))
	   (bindings (butlast exp)))
      ;; a little bit of syntax checking
      (unless (every #'(lambda (r)
			 (and (consp r) 
			      (= (length r) 3) 
			      (member (third r) '(cl-user::baserel cl-user::rel))
			      (member (first r) '(cl-user::exists cl-user::forall))))
		     bindings)
	(signal-error "malformed variable qualification"))

      (multiple-value-bind (ok info) (test-prop c bindings prop)
	(if info
	    (values (if ok 'yes 'no)
		    (format nil "~a: ~{~a ~}" (if ok "example" "counter-example") info))
	    (if ok 'yes 'no)))))