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
;; 2010-05-05 DW  updated matching
;; 2010-05-03 DW  migrating to new command architechture
;; 2009-06-03 DW  added :check option to scenario-consistency
;; 2008-05-30 DW  direct call to reasoner when tractable subset is available
;; 2007-10-10 DW  introduced ternary algebraic closure
;; 2007-03-14 DW  improved scenario-consistency; splitted files
;; 2007-03-13 DW  ofunc-optimized path consistency
;; 2006-10-26 DW  initial version for SparQ V0.7


(in-package :constraint-reasoning)

;; Enforcing algebraic closure
(defun algebraic-closure (calculus constraint-network)
  "enforces algebraic closure"
  (let ((objects (constraint-network-objects constraint-network))
	(constraints (constraints constraint-network)))
    (report-time "network parse/check")
    (values-list (prog1 (multiple-value-list (cond ((eql :binary (calculus-arity calculus)) (test-pathconsistency/binary calculus objects constraints))
						   ((eql :ternary (calculus-arity calculus)) (test-pathconsistency/ternary calculus objects constraints))
						   (t (signal-error "Unsupported arity of calculus '~a' ~a; :binary or :ternary are supported.~%" 
								    (calculus-name calculus)  (calculus-arity calculus)))))
		   (report-time "output result")))))

(defun n-ary-closure (calculus constraint-network)
  "enforces algebraic closure using n-ary composition"
  (let ((objects (constraint-network-objects constraint-network))
	(constraints (constraints constraint-network)))
    (report-time "network parse/check")
    (cond ((eql :binary (calculus-arity calculus)) (a-closure-helper calculus constraint-network))
	  ((eql :ternary (calculus-arity calculus)) (if (calculi:calculus-n-ary-composition calculus)
						       (test-ternary-closure calculus objects constraints)
						       (signal-error "No specification of ternary composition is calculus definition of '~a'" (calculus-name calculus))))
	  (t (signal-error "Unsupported arity of calculus '~a' ~a; :binary or :ternary are supported.~%" 
			   (calculus-name calculus)  (calculus-arity calculus))))))

;; Enforcing scenario-consistency
(defun scenario-consistency (calculus mode cn)
  "decides existence of algebraically closed scenario"
  (unless (member mode '(:first :interactive :all :check))
    (signal-error "§u1§Usage:§u0§ sparq constraint-reasoning <calculus> scenario-consistency { first | all | interactive }~%"))
  (let ((objects (constraint-network-objects cn))
	(constraints (constraints cn)))
    (when (null objects)
      (return-from scenario-consistency cn))
    (sparq:report-time "network parse/check")
    
    (cond
      ((and (eql :binary (calculus-arity calculus))
	    (calculus-tractable-subsets calculus)) (tset-consistency/binary calculus objects constraints mode))
      ((eql :binary (calculus-arity calculus)) (test-consistency/binary calculus objects constraints mode))
      ((eql :ternary (calculus-arity calculus)) (test-consistency/ternary calculus objects constraints mode))
      (t (signal-error "Unsupported arity of calculus '~a' ~a; :binary or :ternary are supported.~%" 
		       (calculus-name calculus)  (calculus-arity calculus))))))

(defun n-ary-consistency (calculus mode cn)
  "decides existence of algebraically closed scenario using n-ary composition"
  (unless (member mode '(:first :interactive :all :check))
    (signal-error "§u1§Usage:§u0§ sparq constraint-reasoning <calculus> n-ary-consistency { first | all | interactive }~%"))
  (let ((objects (constraint-network-objects cn))
	(constraints (constraints cn)))
    (report-time "network parse/check")
    (cond 
      ((eql :binary (calculus-arity calculus)) (test-consistency/binary calculus objects constraints mode))
      ((eql :ternary (calculus-arity calculus)) (test-ternary-consistency calculus objects constraints mode))
      (t (signal-error "Unsupported arity of calculus '~a' ~a; :binary or :ternary are supported.~%" 
		       (calculus-name calculus)  (calculus-arity calculus))))))

;; entry point for the "check-consistency" command
(defun check-consistency (calculus cn)
  "decides consistency of constraint-network cn"
  (let ((m (calculi:calculus-consistency-method calculus)))
    (cond ((or (eql m :a-closure)
	       (eql m :algebraic-closure))
           (algebraic-closure calculus cn))
	  
	  ((eql m :n-ary-closure)
	   (n-ary-closure calculus cn))
	  
	  ((eql m :scenario-consistency)
	   (scenario-consistency calculus :check cn))
	  
	  ((eql m :n-ary-scenario-consistency)
	   (n-ary-consistency calculus :check cn))
	  
	  (t
	   (signal-error "No decision method for consistency is known for calculus ~a" (calculi:calculus-name calculus))))))

(defcommand ("constraint-reasoning" (c calculus) "check-consistency/net" (cn constraint-network c))
  "Decides consistency of a given constraint-network"
  (let ((m (calculi:calculus-consistency-method c)))
    (cond ((or (eql m :a-closure)
	       (eql m :algebraic-closure))
           (multiple-value-list (algebraic-closure c cn)))
	  
	  ((eql m :n-ary-closure)
	   (n-ary-closure c cn))
	  
	  ((eql m :scenario-consistency)
	   (scenario-consistency c :check cn))
	  
	  ((eql m :n-ary-scenario-consistency)
	   (n-ary-consistency c :check cn))
	  
	  (t
	   (signal-error "No decision method for consistency is available for calculus ~a" (calculi:calculus-name c))))))

(defcommand ("constraint-reasoning" (c calculus) "check-consistency" (cn constraint-network c))
    "Decides consistency of a given constraint-network"
  (let ((result (check-consistency c cn)))
    (if (and result
	     (not (and (stringp result)
		       (string= result "Not consistent."))))
	"Consistent."
	"Not consistent.")))

(defun a-closure-helper (c cn)
  (multiple-value-bind (nw modified?) (algebraic-closure c cn)
    (if nw
      (values (if modified?
                "Modified network."
                "Unmodified network.")
              nw)
      (if modified?
	  (values "Not consistent." modified?)
	  "Not consistent."))))

#|
(defcommand ("constraint-reasoning" (c calculus) "propagate" (cn constraint-network c))
  "enforces algebraic closure, returning the fix point or in case of inconsistencies the step in which the inconsistency was discovered"
  (let ((objects (constraint-network-objects cn))
	(constraints (constraints cn)))
    (if (eq :binary (calculus-arity c))
	(multiple-value-bind (ok? modified? constraint-matrix) (pathconsistency/binary c objects constraints)
	  (make-instance 'constraint-network
			 :calculus c
			 :objects objects
			 :matrix constraint-matrix))
	(multiple-value-bind (inconsistent? modified? constraints) (ternary-closure c (expand-constraints/ternary objects c constraints))
	  (make-instance 'constraint-network
			 :calculus c
			 :objects objects
			 :constraints constraints)))))
|#

(defcommand ("constraint-reasoning" (c calculus) "algebraic-closure" (cn constraint-network c))
  "Enforces algebraic closure in a constraint-network"
  (a-closure-helper c cn))

(defcommand* ("constraint-reasoning" (c calculus) "a-closure" (cn constraint-network c))
    "Enforces algebraic closure in a constraint-network"
  (a-closure-helper c cn))

(defcommand* ("constraint-reasoning" (c calculus) "path-consistency" (cn constraint-network c))
    "Enforces algebraic closure in a constraint-network"
  (a-closure-helper c cn))

(defcommand ("constraint-reasoning" (c calculus) "ternary-closure" (cn constraint-network c))
    "Enforces closure with ternary composition operation (available for ternary calculi only)"
  (n-ary-closure c cn))

(defcommand ("constraint-reasoning" (c calculus) "nary-closure" (cn constraint-network c))
    "Enforces closure with calculus-dependent composition operation (binary composition for binary calculi, ternary for ternary)"
  (n-ary-closure c cn))

(defcommand ("constraint-reasoning" (c calculus) "scenario-consistency" (mode (member cl-user::all cl-user::check cl-user::interactive cl-user::first)) (cn constraint-network c))
  "Computes algebraically closed scenarios"
  (let ((mode (read-from-string (format nil "~a" mode))))
    (scenario-consistency c 
			  (cdr (assoc mode '((cl-user::all . :all)
					     (cl-user::first . :first)
					     (cl-user::check . :check)
					     (cl-user::interactive . :interactive))))
			  cn)))

(defcommand* ("constraint-reasoning" (c calculus) "a-closed-scenarios" (mode (member cl-user::all cl-user::check cl-user::interactive cl-user::first)) (cn constraint-network c))
  "Computes scenarios closed with respect to binary composition"
  (let ((mode (read-from-string (format nil "~a" mode))))
    (scenario-consistency c 
			  (cdr (assoc mode '((cl-user::all . :all)
					     (cl-user::first . :first)
					     (cl-user::check . :check)
					     (cl-user::interactive . :interactive))))
			  cn)))

(defcommand ("constraint-reasoning" (c calculus) "nary-closed-scenarios" (mode (member cl-user::all cl-user::check cl-user::interactive cl-user::first)) (cn constraint-network c))
    "Computes scenarios closed under n-ary composition"
    (let ((mode (read-from-string (format nil "~a" mode))))
      (n-ary-consistency c 
			 (cdr (assoc mode '((cl-user::all . :all)
					    (cl-user::first . :first)
					    (cl-user::check . :check)
					    (cl-user::interactive . :interactive))))
			 cn)))

;;;
;;; Querying with constraint networks
;;;

(defclass matching (sparq:primitive)
  ((associations :reader matching-associations
		 :initarg :associations)))

(defmethod print-object ((m matching) stream)
  (format stream "(")
  (dolist (a (matching-associations m))
    (format stream "(~a = ~a)" (car a) (cdr a)))
  (format stream ")"))

(defstruct matching-hypotheses
  associations
  variables
  rating
  heuristic)

(defun signatures (calculus objects constraints)
  (let ((rel-rep (calculi:calculus-relation-representation calculus)))
    (mapcar #'(lambda (o1)
		(let ((vec (make-array (list (relations:relation-representation-num-base-relations rel-rep)) :initial-element 0)))
		  (dolist (c constraints)
		    (cond ((eql o1 (constraint-object-1 c))
			   (incf (aref vec (- (integer-length (constraint-relation c)) 1))))
			  ((eql o1 (constraint-object-2 c))
			   (incf (aref vec (- (integer-length (calculi:converse calculus (constraint-relation c))) 1))))))
		  (cons o1 vec)))
	    objects)))
  
(defun signature-difference (s1 s2)
  (let ((dif 0))
    (dotimes (i (array-dimension s1 0))
      (incf dif (abs (- (aref s1 i) (aref s2 i)))))
    dif))

(defun best-scenario-match (calculus vars cn1 cn2 rating-fn)
  "searches for best match wrt. rating-fn using A*"
  (let* ((rel-rep (calculus-relation-representation calculus))
	 (rejects 0)
	 (expansions 0)
	 (objects-1 (constraint-network-objects cn1))
	 (constraints-1 (constraints cn1))
	 (objects-2 (constraint-network-objects cn2))
	 (constraints-2 (constraints cn2))
	 (o+signatures-1 (signatures calculus objects-1 constraints-1))
	 (o+signatures-2 (signatures calculus objects-2 constraints-2))
	 (best-hypotheses (list (multiple-value-bind (score h)
				    (funcall rating-fn (list (length objects-1) (make-instance 'matching :associations ())))
				  (make-matching-hypotheses 
				   :associations ()
				   :variables vars ;;objects-1
				   :rating score
				   :heuristic h))))
	 (best-rating (matching-hypotheses-rating (first best-hypotheses)))
	 (rel-rep (calculi:calculus-relation-representation calculus))
	 (id (ofuncall (relations:relation-representation-encoder rel-rep) rel-rep (calculi:calculus-identity-relation calculus)))
	 (search-tree (data:list->r/b-tree best-hypotheses
					   #'null)) ;; <- NB: this is no proper comparing function, but its not needed for 1-element lists!!
	 (searching? t))
    ;;(format t "~%~a" o+signatures-1)
    (assert (null (set-difference vars objects-1)) nil "(set-difference vars objects-1) = ~a (v:~a o-1:~a)" (set-difference vars objects-1) vars objects-1)

    
    (labels ((relation (v1 v2 constraints)
	       (if (eql v1 v2)
		   id
		   (dolist (c constraints (error "OUCH! NO CONSTRAINT FOUND!! v1=~w v2=~w  constraints=~w" v1 v2 constraints))
		     (when (and (eql v1 (constraint-object-1 c))
				(eql v2 (constraint-object-2 c)))
		       (return (constraint-relation c)))
		     (when (and (eql v2 (constraint-object-1 c))
				(eql v1 (constraint-object-2 c)))
		       (return (calculi:converse calculus (constraint-relation c)))))))
	     (hypotheses< (hy1 hy2)
	       (let ((h1 (matching-hypotheses-heuristic hy1))
		     (h2 (matching-hypotheses-heuristic hy2)))
		 (or (< h1 h2)
		     (and (= h1 h2) 
			  (< (matching-hypotheses-rating hy1)
			     (matching-hypotheses-rating hy2)))))))
      (loop while (and searching? search-tree) do
	   (let ((node-2-expand (data:r/b-tree-pop-max search-tree)))
             (incf expansions)
             (if (< (matching-hypotheses-heuristic node-2-expand) best-rating)
               (return)
               (when (matching-hypotheses-variables node-2-expand)
                 (multiple-value-bind (rating heuristic) (funcall rating-fn (list (length (rest (matching-hypotheses-variables node-2-expand))) (make-instance 'matching :associations (matching-hypotheses-associations node-2-expand))))
                   (setq search-tree (data:r/b-tree-insert search-tree (make-matching-hypotheses 
                                                                        :associations (matching-hypotheses-associations node-2-expand)
                                                                        :variables (rest (matching-hypotheses-variables node-2-expand))
                                                                        :rating (matching-hypotheses-rating node-2-expand)
                                                                        :heuristic (matching-hypotheses-heuristic node-2-expand))
                                                           #'hypotheses<)))
                 (let ((next-var (first (matching-hypotheses-variables node-2-expand))))		   
                   (dolist (var2 (mapcar #'car (sort (copy-list o+signatures-2) #'< :key #'(lambda (o.s)
											     (signature-difference (cdr o.s) (cdr (assoc next-var o+signatures-1)))))))
                     (let ((associations (cons (cons next-var var2) (matching-hypotheses-associations node-2-expand))))
                       (if (every #'(lambda (existing-association)
				      (equal (relation (car existing-association) next-var constraints-1) 
					     (relation (cdr existing-association) var2 constraints-2))) ;; equal is a hack for relation-rep's ofunc same-relation!
                                    (matching-hypotheses-associations node-2-expand))
                         (multiple-value-bind (rating heuristic) (funcall rating-fn (list (length (rest (matching-hypotheses-variables node-2-expand))) (make-instance 'matching :associations associations)))
                           (unless (< heuristic best-rating)
                             (let ((new-hypotheses (make-matching-hypotheses
                                                    :associations associations
                                                    :variables (rest (matching-hypotheses-variables node-2-expand))
                                                    :rating rating
                                                    :heuristic heuristic)))
                               (setq search-tree (data:r/b-tree-insert search-tree new-hypotheses #'hypotheses<))
                               (cond ((> rating best-rating)
                                      (setq best-rating rating
                                            best-hypotheses (list new-hypotheses)))
                                     ((= rating best-rating)
                                      (push new-hypotheses best-hypotheses))))))
			 (incf rejects))))))))))
    (mapcar #'(lambda (h) (make-instance 'matching :associations (matching-hypotheses-associations h))) best-hypotheses)))


(defcommand ("constraint-reasoning" (c calculus) "best-match" (vars list) (cn1 constraint-network c) (cn2 constraint-network c))
  "Computes maximal matchings of two constraint-networks"
  (best-scenario-match c vars cn1 cn2 ;#'(lambda (m) (values 0 0))

		       #'(lambda (matching)
					  (format *sparq-io* "~&~a~%" matching)
					  (finish-output *sparq-io*)
					  (let ((rating nil)
						(heuristic nil))
					    (ignore-errors 
					      (setq rating (read *sparq-io*)
						    heuristic (read *sparq-io*)))
					    (unless (and (realp rating)
							 (realp heuristic))
					      (signal-error "Node evaluation rating and heuristic require two integer/real numbers! (~w ~w)" rating heuristic))
					    (values rating heuristic)))

		       ))
  


;; Constraint network matching
(defun match-networks (calculus cn1 cn2)
  "computes possible matches of cn1 and cn"
  (let ((query-objects (constraint-network-objects cn1))
	(query-constraints (constraints cn1))
	(objects (constraint-network-objects cn2))
	(constraints (constraints cn2)))
    ;; resolve name conflicts by renaming objects in the query-network
    (mapc #'(lambda (a)
	      (let ((new-a (gensym (string-upcase (format nil "~a-" a)))))
		(sparq:debug-out 2 "Replacing ~a by ~a in query network" a new-a)
		(setq query-objects (mapcar #'(lambda (x)
						(if (eql x a)
						    new-a
						    x))
					    query-objects))
		(mapcar #'(lambda (x)
			    (when (eql (constraint-object-1 x) a)
			      (setf (constraint-object-1 x) new-a))
			    (if (listp (constraint-object-2 x))
				(setf (constraint-object-2 x) (mapcar #'(lambda (x)
									  (if (eql x a)
									      new-a
									      x))
								      (constraint-object-2 x)))
				(if (eql (constraint-object-2 x) a) 
				    (setf (constraint-object-2 x) new-a))))
			query-constraints)))
	  (intersection query-objects objects))
    ;; set up combined network
    (let* ((all-objects (append query-objects objects))
	   (all-constraints (append query-constraints constraints))
	   (rel-rep (calculi:calculus-relation-representation calculus))
	   (id (ofuncall (relations:relation-representation-encoder rel-rep) rel-rep (calculi:calculus-identity-relation calculus)))
	   (associations ()))

      ;; define callback to be used with scenario-consistency to handle matches as we enumerate
      ;; all consistent scenarios
      (flet ((binary-callback (objs matrix)
	       ;; 1.) determine matching indicated by scenario by collecting "eq" relations between
	       ;;     the two subnetworks
	       (let ((match ()))
		 (dolist (qo query-objects)
		   (let ((matched-to (find-if #'(lambda (o)
						  (let ((r (aref matrix (position qo objs) (position o objs))))
						    (equal id r)))
					      objects)))
		     (when matched-to (push (cons qo matched-to) match))))
		 
		 ;; 2.) lookup the matching to see if its new or extends ones already found
		 ;;     if this is just a sub-matching of another we already found, then drop it
		 (when match
		   (unless (some #'(lambda (a)
				     (subsetp match a :test #'equal))
				 associations)
		     (let ((ass2 (remove-if-not #'(lambda (a2)
						    (subsetp a2 match :test #'equal))
						associations)))
		       (if ass2
			   (setq associations (cons match (nset-difference associations ass2)))
			   (push match associations))))))
	       nil)
	     
	     (ternary-callback (constraints) ;; IMPLEMENT TERNARY MATCHING!!
	       t))
	(if (eql :binary (calculus-arity calculus)) 
	    (if (calculus-tractable-subsets calculus)
		(tset-scenario-consistency/binary calculus all-objects all-constraints #'binary-callback nil)
		(scenario-consistency/binary calculus all-objects all-constraints #'binary-callback))
	    (scenario-consistency/ternary calculus all-constraints #'ternary-callback)))
      associations)))

(defun compute-matches (calculus cn1 cn2) 	
  (let ((matchings (mapcar #'(lambda (m)
			       (make-instance 'matching :associations m))
			   (sort (match-networks calculus cn1 cn2) #'> :key #'length))))
    (format *sparq-io* "~&(~{~a~% ~}~a)~%~D maximal matching~:P." (butlast matchings) (car (last matchings)) (length matchings))
    (make-instance 'silent-value :value matchings)))

(defcommand ("constraint-reasoning" (c calculus) "match" (cn1 constraint-network c) (cn2 constraint-network c))
  "Computes maximal matchings of two constraint-networks"
  (compute-matches c cn1 cn2))

(export '(algebraic-closure n-ary-closure n-ary-consistency check-consistency scenario-consistency))
