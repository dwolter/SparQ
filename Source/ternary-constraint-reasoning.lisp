;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
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
;; 2009-06-03 DW  added :check option to scenario-consistency
;; 2007-03-14 DW  improved scenario-consistency; splitted files
;; 2007-03-13 DW  ofunc-optimized path consistency
;; 2006-10-26 DW  initial version for SparQ V0.7

(in-package :constraint-reasoning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               ;;;
;;; Ternary constraint reasoning  ;;;
;;;                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun constraint-descr (calculus constraint)
  (let ((rel-rep (calculi:calculus-relation-representation calculus)))
    (with-output-to-string (descr)
      (if (listp (constraint-object-1 constraint))
	  (format descr "(~a ~a" (first (constraint-object-1 constraint)) (second (constraint-object-1 constraint)))
	  (format descr "(~a" (constraint-object-1 constraint)))
      (format descr " ~a ~a)" (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep (constraint-relation constraint)) (constraint-object-2 constraint)))))

(defun pathconsistency/ternary (calculus constraints)
  (flet ((composable? (c1 c2)
           (= 4 (length (union (cons (constraint-object-2 c1) (constraint-object-1 c1))
                               (cons (constraint-object-2 c2) (constraint-object-1 c2))))))
	 (show-rel (r)
	   (ofuncall (relations:relation-representation-decoder (calculi:calculus-relation-representation calculus)) 
		     (calculi:calculus-relation-representation calculus) r))
	 (same-relation? (r1 r2)
	   (ofuncall (relations:relation-representation-same-relation? (calculi:calculus-relation-representation calculus)) r1 r2))
	 (empty-relation? (r)
	   (ofuncall (relations:relation-representation-empty-relation? (calculi:calculus-relation-representation calculus)) r)))
    (let ((looping? t)
          (modified? nil))
      (loop while looping? do
	   (setq looping? nil)
	   (dolist (c1 constraints)
	     (dolist (c2 constraints)
	       (when (and (not (eq c1 c2)) (composable? c1 c2))
		 (let* ((os (union (cons (constraint-object-2 c1) (constraint-object-1 c1))
				   (cons (constraint-object-2 c2) (constraint-object-1 c2)))))
		   (dolist (c3 (remove-if-not #'(lambda (c) (and (not (eq c c1)) 
								 (not (eq c c2)) 
								 (subsetp (constraint-object-1 c) os)  ; c3 can be constructed by composing c1 and c2
								 (find (constraint-object-2 c) os)))
					      constraints))
		     (when *debug*
		       (format *error-output* "Reasoning about constraints ~a, ~a, and ~a.~%" (constraint-descr calculus c1) (constraint-descr calculus c2) (constraint-descr calculus c3))
		       (finish-output *error-output*)) 
		     (multiple-value-bind (co11 co12 co13 r1)
			 ;; We need to change constraint c1 such that the object not described by c3 is object-2
			 ;; And we need to order object-1 such that the second element is described by c2
			 (let ((c3-objs (cons (constraint-object-2 c3) (constraint-object-1 c3)))
				(c2-objs (cons (constraint-object-2 c2) (constraint-object-1 c2))))
			   
			   (cond ((not (find (constraint-object-2 c1) c3-objs))	; constraint-object-2 is OK
				  (if (find (second (constraint-object-1 c1)) c2-objs)
				      (values (first (constraint-object-1 c1)) (second (constraint-object-1 c1)) (constraint-object-2 c1) (constraint-relation c1))
				      (values (second (constraint-object-1 c1)) (first (constraint-object-1 c1)) (constraint-object-2 c1) (inverse calculus (constraint-relation c1)))))
				 
				 ((not (find (first (constraint-object-1 c1)) c3-objs)) ; (first constraint-object-1) becomes constraint-object-2
				  (if (find (constraint-object-2 c1) c2-objs)
				      (values (second (constraint-object-1 c1)) (constraint-object-2 c1) (first (constraint-object-1 c1)) (shortcut calculus (inverse calculus (constraint-relation c1))))
				      (values (constraint-object-2 c1) (second (constraint-object-1 c1)) (first (constraint-object-1 c1)) (inverse calculus (shortcut calculus (inverse calculus (constraint-relation c1)))))))
				 
				 (t ; (second constraint-object-1) becomes constraint-object-2
				  (if (find (constraint-object-2 c1) c2-objs)
				      (values (first (constraint-object-1 c1)) (constraint-object-2 c1) (second (constraint-object-1 c1)) (shortcut calculus (constraint-relation c1)))
				      (values (constraint-object-2 c1) (first (constraint-object-1 c1)) (second (constraint-object-1 c1)) (inverse calculus (shortcut calculus (constraint-relation c1))))))))
		       (when *debug*
			 (format *error-output* "~%;+ Changing c1 from ~a to (~a,~a ~a ~a)~%" (constraint-descr calculus c1) co11 co12 (show-rel r1) co13))
		       (let* ((r2 (cond ((and (eq (first (constraint-object-1 c2))   ; A,B r1 C  o  B,C r2 D -> r1 o r2 
						  co12)
					      (eq (second (constraint-object-1 c2))
						  co13))
					 (constraint-relation c2))
					
					((and (eq (first (constraint-object-1 c2))   ; A,B r1 C  o  C,B r2 D -> r1o(inverse r2)
						  co13)
					      (eq (second (constraint-object-1 c2))
						  co12))
					 (inverse calculus (constraint-relation c2)))
					
					((and (eq co13   ; A,B r1 C  o  B,D r2 C -> r1o(shortcut r2)
						  (constraint-object-2 c2))
					      (eq co12
						  (first (constraint-object-1 c2))))
					 (shortcut calculus (constraint-relation c2)))
					
					((and (eq co13   ; A,B r1 C  o  D,B r2 C -> r1o(shortcut (inverse r2))
						  (constraint-object-2 c2))
					      (eq co12
						  (second (constraint-object-1 c2))))
					 (shortcut calculus (inverse calculus (constraint-relation c2))))
					
					((and (eq (second (constraint-object-1 c2))   ; A,B r1 C  o  D,C r2 B -> r1o(inverse (shortcut (inverse r2)))
						  co13)
					      (eq co12
						  (constraint-object-2 c2)))
					 (inverse calculus (shortcut calculus (inverse calculus (constraint-relation c2)))))
					
					((and (eq co12   ; A,B r1 C  o  C,D r2 B -> r1o(inverse (shortcut (inverse r2)))
						  (constraint-object-2 c2))
					      (eq (first (constraint-object-1 c2))
						  co13))
					 (inverse calculus (shortcut calculus (constraint-relation c2))))
					(t (error "Internal error ((~a ~a) R ~a   o   ~a -> ???)" co11 co12 co13 c2))))                                                              
			      (r3 (let ((o1 co11)
					(o2 co12)
					(o3 (car (remove co12 (remove co13 (cons (constraint-object-2 c2) (constraint-object-1 c2))))))
					(r  (composition calculus r1 r2)))
				    (when *debug*
				      (format *error-output* "-> (~a ~a ~a ~a)~%" o1 o2 (show-rel r) o3))
					;(format t "~%constraint-objects 1 : ~a -> (~a,~a ~a) ~%constraint-objects 2 : ~a~%constraint-objects 3 : ~a~%"
					;        (cons (constraint-object-2 c1) (constraint-object-1 c1))
					;        co11 co12 co13
					;        (cons (constraint-object-2 c2) (constraint-object-1 c2))
					;        (cons (constraint-object-2 c3) (constraint-object-1 c3)))
					;(format t "~%o1=~a  o2=~a  o3=~a~%c1=~a~%c2=~a" o1 o2 o3 c1 c2)
				    ;; if we compose r1 and r2, we yield the relation r: o1,o2 r o3
				    ;; we need to adapt the composition to obtain in order of objects in constraint c3
				    (calculi:intersect calculus
						       (constraint-relation c3)
						       (cond ((eq o3 (constraint-object-2 c3))			; ?,? r o3 
							      (if (eq o1 (first (constraint-object-1 c3)))
								  r			   				; o1,o2 r o3 : r 
								  (inverse calculus r))) 				; o2,o1 r o3 : (inverse r)
							     
							     ((eq o1 (constraint-object-2 c3))		   		  ; ?,? r o1  
							      (if (eq o2 (first (constraint-object-1 c3)))
								  (shortcut calculus (inverse calculus r)) 			  ; o2,o3 r o1 : (shortcut (inverse r3))
								  (inverse calculus (shortcut calculus (inverse calculus r))))) ; o3,o2 r o1 : (inverse (shortcut (inverse r3)))
							     
							     ((eq o2 (constraint-object-2 c3))                 	; ?,? r o2 
							      (if (eq o1 (first (constraint-object-1 c3)))
								  (shortcut calculus r) 				; o1,o3 r o2 : (shortcut r3)
								  (inverse calculus (shortcut calculus r))))  	; o3,o2 r o1 : (inverse (shortcut r3))
							     
							     (t (error "Internal error (~a,~a R ~a -> ~a)" o1 o2 o3 c3)))))))
			 
			 (when *debug*
			   (format *error-output* "r1=~a; r2=~a; r3=~a~%" (show-rel r1) (show-rel r2) (show-rel r3))
			   (format *error-output* "; (~a ~a ~a ~a), (~a ~a ~a ~a) o (~a ~a ~a ~a) => (~a ~a ~a ~a)~%"
				   (first (constraint-object-1 c3)) (second (constraint-object-1 c3)) (show-rel (constraint-relation c3)) (constraint-object-2 c3)
				   (first (constraint-object-1 c1)) (second (constraint-object-1 c1)) (show-rel (constraint-relation c1)) (constraint-object-2 c1)
				   (first (constraint-object-1 c2)) (second (constraint-object-1 c2)) (show-rel (constraint-relation c2)) (constraint-object-2 c2)
				   (first (constraint-object-1 c3)) (second (constraint-object-1 c3)) (show-rel r3) (constraint-object-2 c3)))
			 (unless (same-relation? r3 (constraint-relation c3))
			   (setf (constraint-relation c3) r3
				 modified? t
				 looping? t))                        
			 (when (empty-relation? r3)
			   (return-from pathconsistency/ternary (values nil modified? constraints)))))))))))
      (values t modified? constraints))))

(defun expand-constraints/ternary (objects calculus constraints)
  "Expands a list of constraint such that the constraint network is complete" 
  (let ((accu constraints))
    (do ((o1s objects (cdr o1s)))
        ((null o1s))
      (do ((o2s (cdr o1s) (cdr o2s)))
          ((null o2s))
        (do ((o3s (cdr o2s) (cdr o3s)))
            ((null o3s))
          (let ((os (list (car o1s) (car o2s) (car o3s))))
            (unless (some #'(lambda (c)
                              (null (set-difference os (cons (constraint-object-2 c) (constraint-object-1 c)))))
                          accu)
              (when *debug*
                (format *error-output* "~%; Adding constraint ((~a ~a) * ~a)" (car o1s) (car o2s) (car o3s)))
              (push (make-constraint (list (car o1s) (car o2s))
				     (relations:relation-representation-universal-relation (calculi:calculus-relation-representation calculus))
                                     (car o3s))
                    accu))))))
    (when *debug*
      (finish-output *error-output*))
    accu))

(defun test-pathconsistency/ternary (calculus objects constraints)
  "Test a set of binary constraints for path-consistency" 
  (multiple-value-bind (consistent? modified? constraints) (pathconsistency/ternary calculus (expand-constraints/ternary objects calculus constraints))
    (values (if consistent? 
		(make-instance 'constraint-network 
			       :constraints constraints
			       :objects objects
			       :calculus calculus))
	    modified?)))

;; Searches constraints for a relation that is a disjunction and
;; returns the split into base-relations; similar to find-splitting-set used in 
;; binary constraint resoning.
;; THIS WILL BE CHANGED ONCE WE USE TRACTABLE SUBSETS
(defun find-splitting-set/constraints (rel-rep constraints)
  (dolist (c constraints)
    (let ((set (disjunction-split rel-rep (constraint-relation c))))
      (when (cdr set)
	(return (values set c))))))

(defun scenario-consistency/ternary (calculus constraints callback)  
  (let ((rel-rep (calculus-relation-representation calculus)))
    (labels ((enforce-path-consistency (constraints) 
	       (multiple-value-bind (ok? modified? constraints) (pathconsistency/ternary calculus (mapcar #'copy-constraint constraints))
		   (declare (ignore modified?))
		   (values ok? constraints)))
	     (recursive-search (constraints) 
	       (multiple-value-bind (set c) (find-splitting-set/constraints rel-rep constraints)
		 (if (cdr set)
		     ;; Try out all splits
		     (dolist (r set)
		       (setf (constraint-relation c) r)
		       (multiple-value-bind (ok? updated-matrix) (enforce-path-consistency constraints)
			 (when ok?
			   (recursive-search updated-matrix))))
		     ;; We have found a solution
		     (when (funcall callback constraints)
		       (return-from scenario-consistency/ternary))))))
      
      (multiple-value-bind (ok? modified? cs) (pathconsistency/ternary calculus constraints)
	(declare (ignore modified?))
	(when ok?
	  (recursive-search cs))))))


(defun consistency/ternary (calculus constraints callback)
  (flet ((single-rel? (r)
	   (let ((count 0))
	     (ofuncall (relations:relation-representation-mapper (calculi:calculus-relation-representation calculus))
		       #'(lambda (i)
			   (declare (ignore i))
			   (incf count))
		       r)
	     (<= count 1)))
	 (contained-relations (r)
	   (let ((rels nil)
		 (base-rels   (relations:relation-representation-base-relations (calculi:calculus-relation-representation calculus))))
	     (ofuncall (relations:relation-representation-mapper (calculi:calculus-relation-representation calculus))
		       #'(lambda (idx)
			   (push (svref base-rels idx) rels))
		       r)
	     rels)))
    (if (every #'(lambda (c) (single-rel? (constraint-relation c))) constraints)
	;; If there are no disjunctions then we only need to check for pathconsistency...
	(multiple-value-bind (ok? modified? network) (pathconsistency/ternary calculus
									      (mapcar #'(lambda (c)
											  (make-constraint (constraint-object-1 c)
													   (constraint-relation c)
													   (constraint-object-2 c)))
										      constraints))
	  (declare (ignore modified?))
	  (if ok?
	      (funcall callback network)))
	;; Otherwise, we select some disjunction and iterate over all its relations...
	(let ((c? (find-if #'(lambda (c) (not (single-rel? (constraint-relation c)))) constraints))
	      solution)
	  (do ((rs (contained-relations (constraint-relation c?)) (cdr rs)))
	      ((or solution (null rs)))
	    (setq solution (consistency/ternary calculus
						(cons (make-constraint (constraint-object-1 c?)
								       (car rs)
								       (constraint-object-2 c?))
						      (remove c? constraints))
						callback)))
	  solution))))

(defun test-consistency/ternary (calculus objects constraints solutions)
  "Checks a constraint network for global consistency" 
  (let ((solution? nil)
	(count 0)
	(stream sparq:*sparq-io*)
        (canceled? t))
    (flet ((interactive-callback (network)
             (dump-ternary-constraint-network stream calculus network) 
             (write-line "Continue search? (yes / no)" stream)
             (setq solution? t)
	     (incf count)
             (clear-input stream)
             (let ((in (string-downcase (read-line stream nil nil nil))))
	       (loop while (and (string/= "yes" in)
				(string/= "no" in)) do
		    (write-line "Please answer 'yes' or 'no'" stream)
		    (setq in (string-downcase (read-line stream nil nil nil))))
               (setq canceled? (string/= "yes" in))))
           (check-callback (network)
	     (declare (ignore network))
	     (format stream "~%Consistent.")
             (setq solution? t))
           (first-callback (network)
             (setq solution? t)
             (dump-ternary-constraint-network stream calculus network)
             t)
           (all-callback (network)
             (setq solution? t)
	     (incf count)
             (dump-ternary-constraint-network stream calculus network)
             nil))
      (scenario-consistency/ternary calculus (expand-constraints/ternary objects calculus constraints) 
                           (case solutions
                             (:first #'first-callback)
			     (:check #'check-callback)
                             (:interactive #'interactive-callback)
                             (:all #'all-callback)))
      (if solution?
        (when (or (eq solutions :all) (not canceled?))
          (format stream "~D scenario~:P found, no further scenarios exist.~%" count)
	  (make-instance 'sparq:silent-value :value nil))
        "Not consistent."))))


;; permutes a constraints such that the order of constraint objects is o1 o2 o3
(defun permute-relation (calculus constraint o1 o2 o3 &optional where)
  ;(declare (ignore o3))
  (let* ((c1 (first (constraint-object-1 constraint)))
	 (c2 (second (constraint-object-1 constraint)))
	 (c3 (constraint-object-2 constraint))
	 (r (constraint-relation constraint))
	 (r-neu (cond ((eq o1 c1) (if (eq o2 c2) 
			  r                                             ; o1 o2 r o3 -> r
			  (shortcut calculus r)))                       ; o1 o3 r o2 -> (shortcut r)
	  ((eq o1 c2) (if (eq o2 c1)
			  (inverse calculus r)                          ; o2 o1 r o3 -> (inverse r)
			  (shortcut calculus (inverse calculus r))))	; o3 o1 r o2 -> (shortcut (inverse r))
	  (t (unless (eq o1 c3)
	       (format *error-output* "fishy (~a): o1 = ~a, o2 = ~a, o3 = ~a; constraint=~a~%" where o1 o2 o3 constraint)
	       )
	   (if (eq o2 c1)
	       (inverse calculus (shortcut calculus r))	        ; o2 o3 r o1 -> (inverse (shortcut r))
	       (inverse calculus (homing calculus r))))))) 	; o3 o2 r o1 -> (inverse (homing r))

    ;(declare (ignore c3))
    (when *debug*
      (let ((rel-rep (calculus-relation-representation calculus)))
	(format t
		"~&  ;; (~a ~a ~a ~a) --> (~a ~a ~a ~a)  (reason: ~a)"
		(first (constraint-object-1 constraint))
		(second (constraint-object-1 constraint))
		(ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r)
		(constraint-object-2 constraint)
		o1
		o2
		(ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r-neu)
		o3
		where)
	(finish-output)))
    r-neu))

;;
;; (A B) r_1 F  * (A F) r_2 C  *  (F B) r_3 C  ~~  (A B) r_4 C
;; 
(defun ternary-closure (calculus constraints)
  (let ((rel-rep (calculus-relation-representation calculus)))
    (labels ((rel-str (r)
	       (ofuncall (relations:relation-representation-decoder rel-rep) rel-rep r))
	     (map-ternary-refinement ()
	       (let ((modified? nil))
		 (do ((c1s constraints (cdr c1s)))
		     ((null c1s))
		   (let* ((c1 (car c1s))
			  (objects (cons (constraint-object-2 c1) (constraint-object-1 c1))))
		     (do ((c2s (remove-if #'(lambda (c) ; remove all constraint that introduce more than 1 new object (i.e. giving us more than 4 objects in total)
					      (/= 4 (length (union objects (cons (constraint-object-2 c) (constraint-object-1 c))))))
					  constraints)
			       (cdr c2s)))
			 ((null c2s))		       
		     (let* ((c2 (car c2s))
			    (o2s (cons (constraint-object-2 c2) (constraint-object-1 c2)))
			    (new-obj (car (set-difference o2s objects))) ; das objekt 'c' was neu in constraint-2 auftaucht
			    (old-obj (car (set-difference objects o2s))) ; das objekt 'b' was bisher nur in contraint-1 aufaucht
			    (com-objs (remove old-obj objects)) ; die objekte die constraint 1 und 2 gemeinsam haben
			    (r1 (permute-relation calculus c1
						  (first com-objs)
						  old-obj
						  (second com-objs)
						  "remute-r1"))
			    (r2 (permute-relation calculus c2 
						  (first com-objs) 
						  (second com-objs) 
						  new-obj "r2-permute"))
			    (objects (cons new-obj objects)))
		       (do ((c3s (remove-if #'(lambda (c) ; remove all constraint that would give us more than 4 objects in total AND that have the object 'A' 
						(let ((o3s (cons (constraint-object-2 c) (constraint-object-1 c))))
						  (declare (dynamic-extent o3s))
						  (or (/= 4 (length (union objects o3s)))
						      (eq c c2)
						      (eq c c1))))
					    constraints)
				 (cdr c3s)))
			   ((null c3s))
			 (let* ((c3 (car c3s))
				(swap-c3/c4? (find (first com-objs) (cons (constraint-object-2 c3) (constraint-object-1 c3))))
				(r3 (if swap-c3/c4? ; c3 should rather be c4
					(permute-relation calculus c3
							  (first com-objs)
							  old-obj
							  new-obj "r3-permute swap")
					(permute-relation calculus c3
							  (second com-objs)
							  old-obj
							  new-obj "r3-permute"))))
			   (do ((c4s (remove-if #'(lambda (c) ; remove all constraint that would give us more than 4 objects in total
						    (or (/= 4 (length (union objects (cons (constraint-object-2 c) (constraint-object-1 c)))))
							(eq c c1)
							(eq c c2)
							(eq c c3)))
						constraints)
				     (cdr c4s)))
			       ((null c4s))
			     (let* ((c4 (car c4s)) ;  (A B) r_1 F  * (A F) r_2 C  *  (F B) r_3 C  ~~  (A B) r_4 C
				    (r4 (if swap-c3/c4?
					    (permute-relation calculus c4
							      (second com-objs)
							      old-obj
							      new-obj "r4-permute swap")
					    (permute-relation calculus c4
							      (first com-objs)
							      old-obj
							      new-obj "r4-permute"))))
			       ;(format t "~%c3 = ~a  c4 = ~a  swap?= ~a~%" c3 c4 swap-c3/c4?)
			       (when swap-c3/c4?
				 (rotatef c3 c4)
				 (rotatef r3 r4))
			       (let* ((r4-comp (ternary-composition calculus r1 r2 r3))
				      (new-r4 (intersect calculus r4 r4-comp)))
				 (when sparq:*debug*
				   (format *error-output* "~%; inspecting: (~a ~a ~a) *  (~a ~a ~a) *  (~a ~a ~a)  <-?->  (~a ~a ~a)~%; rels after permute: ~a * ~a * ~a~a = ~a <-?-> ~a => "
					   (constraint-object-1 c1)
					   (rel-str (constraint-relation c1))
					   (constraint-object-2 c1)
					   
					   (constraint-object-1 c2)
					   (rel-str (constraint-relation c2))
					   (constraint-object-2 c2)
					   
					   (constraint-object-1 c3)
					   (rel-str (constraint-relation c3))
					   (constraint-object-2 c3)
					   
					   (constraint-object-1 c4)
					   (rel-str (constraint-relation c4))
					   (constraint-object-2 c4)
					   
					   (rel-str r1) (rel-str r2) (rel-str r3) 
					   (if swap-c3/c4? 
					       "(c3 <-> c4)"
					       "")
					   (rel-str r4-comp) (rel-str r4))
				   (if (ofuncall (relations:relation-representation-same-relation? rel-rep) r4 new-r4)
				       (format *error-output* "OK~%")
				       (format *error-output* "refining to ~a, re-permuting to ~a~%" (rel-str new-r4) 
					       (rel-str (permute-relation calculus (make-constraint (list (first com-objs) old-obj) new-r4 new-obj)
										    (first (constraint-object-1 c4))
										    (second (constraint-object-1 c4))
										    (constraint-object-2 c4) "permute-back")))))
				 
				 (unless (ofuncall (relations:relation-representation-same-relation? rel-rep) r4 new-r4)
				   (setq modified? t)
				   ;; store relation back into constraint; we must reverse the order 
				   (setf (constraint-relation c4) (permute-relation calculus (make-constraint (list (first com-objs) old-obj) new-r4 new-obj)
										    (first (constraint-object-1 c4))
										    (second (constraint-object-1 c4))
										    (constraint-object-2 c4) "permute-back")))			       
				 (when (ofuncall (relations:relation-representation-empty-relation? rel-rep) new-r4)
				   (return-from map-ternary-refinement (values modified? t))))))))))))
		 (values modified? nil))))
      (multiple-value-bind (modified? inconsistent?) (map-ternary-refinement)
	(if (and modified? (not inconsistent?)) ; do we need to loop again (and again and ...)
	    (progn
	      (loop while (and modified? (not inconsistent?)) do
		   (multiple-value-setq (modified? inconsistent?) (map-ternary-refinement)))
	      (values inconsistent? t constraints))
	    (values inconsistent? modified? constraints))))))

(defun test-ternary-closure (calculus objects constraints)
  "Test a set of binary constraints for ternary algebraic closure"
  (multiple-value-bind (inconsistent? modified? constraints) (ternary-closure calculus (expand-constraints/ternary objects calculus constraints))
    (if (not inconsistent?)
      (progn  
        (write-line (if modified? "Modified network." "Unmodified network.") sparq:*sparq-io*)
        ;(dump-ternary-constraint-network sparq:*sparq-io* calculus constraints)
	(make-instance 'constraint-network
		       :calculus calculus
		       :objects objects
		       :constraints constraints))
      (progn
        "Not consistent." ;(write-line "Not consistent." sparq:*sparq-io*)
        ;;(dump-ternary-constraint-network stream calculus constraints)
	))))

(defun ternary-scenario-consistency (calculus constraints callback)  
  (let ((rel-rep (calculus-relation-representation calculus)))
    (labels ((enforce-ternary-closure (constraints) 
	       (multiple-value-bind (inconsistent? modified? constraints) (ternary-closure calculus (mapcar #'copy-constraint constraints))
		   (declare (ignore modified?))
		   (values inconsistent? constraints)))
	     (recursive-search (constraints) 
	       (multiple-value-bind (set c) (find-splitting-set/constraints rel-rep constraints)
		 (if (cdr set)
		     ;; Try out all splits
		     (dolist (r set)
		       (setf (constraint-relation c) r)
		       (multiple-value-bind (inconsistent? updated-constraints) (enforce-ternary-closure constraints)
			 (unless inconsistent?
			   (recursive-search updated-constraints))))		     
		     ;; We have found a solution
		     (progn 
#|
		       (when (ternary-closure calculus (sort (mapcar #'copy-constraint constraints) #'string< :key (lambda (c) (format nil "~a-~a" (constraint-object-1 c) (constraint-object-2 c)))))
		       (format t "~%OUCH!!~%"))
|#
		     (when (funcall callback constraints)
		       (return-from ternary-scenario-consistency)))))) )
      
      (multiple-value-bind (inconsistent? modified? cs) (ternary-closure calculus constraints)
	(declare (ignore modified?))
	(unless inconsistent?
	  (recursive-search cs))))))

(defun test-ternary-consistency (stream calculus objects constraints solutions)
  "Checks a constraint network for scenario consistency using ternary composition" 
  (let ((solution? nil)
	(count 0)
        (canceled? t))
    (flet ((interactive-callback (network)
             (dump-ternary-constraint-network stream calculus network) 
             (write-line "Continue search? (yes / no)" stream)
             (setq solution? t)
	     (incf count)
             (clear-input stream)
             (let ((in (string-downcase (read-line stream nil nil nil))))
	       (loop while (and (string/= "yes" in)
				(string/= "no" in)) do
		    (write-line "Please answer 'yes' or 'no'" stream)
		    (setq in (string-downcase (read-line stream nil nil nil))))
               (setq canceled? (string/= "yes" in))))
           (check-callback (network)
	     (declare (ignore network))
	     (format stream "~&Consistent.")
             (setq solution? t))
           (first-callback (network)
             (setq solution? t)
             (dump-ternary-constraint-network stream calculus network)
             t)
           (all-callback (network)
             (setq solution? t)
	     (incf count)
             (dump-ternary-constraint-network stream calculus network)
             nil))
      (ternary-scenario-consistency calculus (expand-constraints/ternary objects calculus constraints) 
                           (case solutions
                             (:first #'first-callback)
			     (:check #'check-callback)
                             (:interactive #'interactive-callback)
                             (:all #'all-callback)))
      (if solution?
        (when (or (eq solutions :all) (not canceled?))
          (format stream "~D scenario~:P found, no further scenarios exist.~%" count))
        (write-line "Not consistent." stream)))))

(export '(test-ternary-closure test-ternary-consistency))
