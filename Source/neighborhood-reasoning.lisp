;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; NEIGHBORHOOD-REASONING  --- package implementing SparQ user
;;; commands for performing reasoning based on conceptual neighborhood
;;;

;;; Currently supported operations:
;;;
;;; neighborhood-reasoning <calculus> <neighborhood-structure> neighbors <relation> 
;;;   - returns conceptual neighbors of base relations in <relatio>n (excluding those already contained in the 
;;;     original relation) according to the specified neighborhood structure
;;;
;;; neighborhood-reasoning <calculus> <neighborhood-structure> neighbors-nth <n> <relation> 
;;;   - returns conceptual neighbors with minimal distance <n> to a base relation in <relation>
;;;
;;; neighborhood-reasoning <calculus> <neighborhood-structure> neighborhood-distance <relation1> <relation2>
;;;   - returns minimal distance (length of shortest path in the neighborhood structure) between two base relations from
;;;     <relation1> and <relation2>
;;;
;;; neighborhood-reasoning <calculus> <neighborhood-structure> relax <relation> 
;;;   - returns new relation with all conceptual neighbors added to the original relation <relation>
;;;
;;; neighborhood-reasoning <calculus> <neighborhood-structure> relax-nth <n> <relation> 
;;;   - returns new relation with all conceptual neighbors with distance at most <n> added to the original relation <relation>
;;;
;;; neighborhood-reasoning <calculus> <neighborhood-structure> similarity <constraint-aggregation-op> <network1> <network2> 
;;;   - returns similarity distance between <network1> and <network2> using <constraint-aggregation-op> to aggregate over
;;;     corresponding constraints ("sum" or "max")
;;;
;;; neighborhood-reasoning <calculus> <neighborhood-structure> merge <merge-operator> <constraint-aggregation-op>
;;; <network-aggreation-op> <networks>
;;;   - uses distance-based merging to determine a single consistent network from the input networks <networks>
;;;     using the general merging operators <merge-operator> (currently a dummy as there is only one appraoch),
;;;     <constraint-aggregation-op> to aggregate over constraints ("sum" or "max") and <network-aggreation-op>
;;;     to aggregate over networks (again "sum" or "max")


;; Change history (most recent first):
;; 2010-03-23 JOW merge and similarity added (still needs testing and wrap-up)
;; 2010-03-01 JOW all functions now operate on relation representations; new operations relax-nth and neighbors-nth added            
;; 2010-02-10 JOW added first prototypes of "neighborhood-distance" and "relax" operations 
;; 2010-02-08 JOW first rudementary version of neighborhood-reasoning supporting the "neighbors" operation


; next steps:
;
; - fine tune consistency checking (should not output anything and use best possible method)
; - make robust 
; - currently relaxation does not recognize if no further relaxation of a constraint / network is possible
; 
; ideas:
;
; - optimize by initializing patterns based on minimal possible distance
; - applying the consistency check to the union of the new intersection networks may speed things up when tractable subset is available
; - optimize large memory consumption by maintaining relaxation and tuple lists in relax-networks


(defpackage :neighborhood-reasoning
  (:use :common-lisp :sparq :calculi :ofunc :relations :constraint-reasoning)
  (:export :neighborhood-reasoning))

(in-package :neighborhood-reasoning)

;;
;; certain auxiliary functions
;;

;; creates matrix of relations representing a constraint network from a list of constraints given in "network" and a list
;; of variable names given in "objects"; only the cells c_ij  with i < j are filled and should be used;
;; it is assumed that the list of constraints passed as "network" is well-formed (if c_ji and c_ji are specified then
;; c_ij = converse(c_ji) )
(defun make-network-matrix (calculus rel-rep objects network)
  (let*((s (length objects))
	(matrix (make-array (list s s) :initial-element (relation-representation-universal-relation rel-rep))))
    (dolist (x network)
      (let ((i1 (position (constraint-reasoning:constraint-object-1 x) objects))
	    (i2 (position (constraint-reasoning:constraint-object-2 x) objects)))
	(if (< i1 i2)
	    (setf (aref matrix i1 i2)  (constraint-reasoning:constraint-relation x))
	  (setf (aref matrix i2 i1) (calculi:converse calculus (constraint-reasoning:constraint-relation x))))))
    matrix))

;; converts a constraint network "matrix" in matrix form into a list of constraints
(defun make-constraint-list-from-matrix (matrix)
  (let*((s (array-dimension matrix 0))
	(l nil))
    (do ((x 0 (+ x 1)))
	((= x s))
      (do ((y (+ x 1) (+ y 1)))
	  ((= y s))
	(setf l (cons (make-constraint x (aref matrix x y) y) l))))
    l))

;; print constraint network "matrix" as network string to "stream"
(defun print-network-matrix (calculus rel-rep objects matrix stream)
  (let* ((decoder (relations:relation-representation-decoder rel-rep))
	 (s (length objects)))
    (format stream "(")
    (do ((x 0 (+ x 1)))
	((= x s))
      (do ((y (+ x 1) (+ y 1)))
	  ((= y s))
	(format stream "(~a ~a ~a)" (nth x objects)  (ofuncall decoder rel-rep (aref matrix x y)) (nth y objects))))
    (format stream ")~%")
    (finish-output stream)))
					
;; swaps content of two variables
(defmacro swap (x y)
  `(let ((tmp ,x))
     (setf ,x ,y)
     (setf ,y tmp)))

;; yields aggregation function based on pased aggregation-operator-name (currently "sum" or "max")
(defun parse-operator (o)
  (cond ((eq o 'cl-user::sum)
	 #'+)
	((eq o 'cl-user::max)
	 #'max)))

;; computes all pairs from l1 and l2 (used by tuples to compute general cartesian product)  
(defun pairs (l1 l2)
  (let ((result nil))
    (dolist (e1 l1)
      (dolist (e2 l2)
	(setf result (cons (append (if (listp e1) e1 (list e1))
				   (if (listp e2) e2 (list e2)))
			   result))))
    result))
      
;; computes cartesian product l1 x l2...x ln from l = ( (l1) (l2) ... (ln)
(defun tuples (l) 
  (if (= (length l) 1)
      (mapcar #'list (car l))
    (reduce #'pairs l)))

;; auxiliary structure to generate relaxation patterns
(defstruct relax-pattern pattern pos posp size rem lastswaps indices permute? next-function)

;; create and initialize relaxation pattern for a given aggregation-operator
(defun relax-pattern-init (aggregate-op size d max stream)
  (let ((p (make-relax-pattern)))
    (setf (relax-pattern-pattern p) (make-array size :initial-element 0))
    (setf (relax-pattern-lastswaps p) (make-array size :initial-element -1))
    (setf (relax-pattern-indices p) (make-array size :initial-element 0))
    (setf (relax-pattern-posp p) 0)
    (setf (relax-pattern-size p) size)
    (setf (svref (relax-pattern-pattern p) 0) d)
    (setf (relax-pattern-permute? p) t)
    
    (cond ((eq aggregate-op 'cl-user::max)
	   (if (or (= d 0)
		   (= size 1))
	       (setf (relax-pattern-pos p) 0)
	     (setf (relax-pattern-pos p) 1))
	   (setf (relax-pattern-next-function p)  #'relax-pattern-max-next))

	  (t  ; if it's not max we assume it's sum
	   (let ((remaining d)    ;; shaky optimization, what happens if no suitable pattern can be generated for a given max?
                 (m (if (= max -1)
                      d
                      max)))
             (setf (relax-pattern-rem p) d)
             (setf (relax-pattern-pos p) 0)
             (setf (relax-pattern-next-function p)  #'relax-pattern-sum-next)
             
            ; (print "setting up sum pattern")
            ; (print m)
            ; (print max)
             (do ((i 0 (+ i 1)))
                 ((< remaining 1))
           ;    (print i)
           ;    (print remaining)
               (setf (svref (relax-pattern-pattern p) i) (min m remaining))
               (setf remaining (- remaining m))
               (setf (relax-pattern-pos p) i)
            ;   (print remaining)
             ))))

    p))

;; ugly iterative version of a permutation generator function for patterns "p" that returns t for each permutation 
;; generated until no further permutations exist; the content in "p" needs to be ordered; "indices" and "lastswaps"
;; need to arrays of the same size of "p" and are used to store intermediate state information; elements in "indices" 
;; need to be initialized to 0; elements in "lastswaps" to "-1" (assuming "p" contains numbers)
(defun relax-pattern-permute (p stream)
  (let ((pattern-generated? nil)
	(done? nil))
   (when *debug* (format stream "~&;;premuting~%"))
   (when *debug* (format stream "~&;;~A~%" p))
   (do ()
       ((or pattern-generated? done?) (not done?))
     (progn
       (if (= (relax-pattern-posp p) (- (relax-pattern-size p) 1))
	   (if (= (svref (relax-pattern-lastswaps p) (relax-pattern-posp p)) -1)
	       (progn 
		 (setf pattern-generated? t)
		 (setf (svref (relax-pattern-lastswaps p) (relax-pattern-posp p)) 0))
	     (progn
	       (setf (svref (relax-pattern-lastswaps p) (relax-pattern-posp p)) -1)
	       
	       (if (= (relax-pattern-size p) 1)
		   (progn
		     (setf done? t)
		     (setf (svref (relax-pattern-indices p) 0) 0))

		 (progn
		   (setf (relax-pattern-posp p) (- (relax-pattern-posp p) 1))
		   (swap (svref (relax-pattern-pattern p) (relax-pattern-posp p)) (svref (relax-pattern-pattern p) (svref (relax-pattern-indices p) (relax-pattern-posp p)))) 
		   (setf (svref (relax-pattern-indices p) (relax-pattern-posp p)) (+ (svref (relax-pattern-indices p) (relax-pattern-posp p)) 1))))))
	     
	 (if (= (svref (relax-pattern-indices p) (relax-pattern-posp p)) (relax-pattern-size p))
	     (if (= (relax-pattern-posp p) 0)
		 (progn 
		   (setf (svref (relax-pattern-lastswaps p) 0) -1)
		   (setf (svref (relax-pattern-indices p) 0) 0)
		   (setf done? t))
	       (progn
		 (setf (svref (relax-pattern-indices p) (relax-pattern-posp p)) 0)
		 (setf (svref (relax-pattern-lastswaps p) (relax-pattern-posp p)) -1)
		 (setf (relax-pattern-posp p) (- (relax-pattern-posp p) 1))
		     
		 (swap (svref (relax-pattern-pattern p) (relax-pattern-posp p)) (svref (relax-pattern-pattern p) (svref (relax-pattern-indices p) (relax-pattern-posp p))))
		 (setf (svref (relax-pattern-indices p) (relax-pattern-posp p)) (+ (svref (relax-pattern-indices p) (relax-pattern-posp p)) 1))))
	       
	   (if (= (svref (relax-pattern-lastswaps p) (relax-pattern-posp p)) (svref (relax-pattern-pattern p) (svref (relax-pattern-indices p) (relax-pattern-posp p))))
	       (setf (svref (relax-pattern-indices p) (relax-pattern-posp p)) (+ (svref (relax-pattern-indices p) (relax-pattern-posp p)) 1))
	     (progn
	       (setf (svref (relax-pattern-lastswaps p) (relax-pattern-posp p)) (svref (relax-pattern-pattern p) (svref (relax-pattern-indices p) (relax-pattern-posp p))))
	       (swap  (svref (relax-pattern-pattern p) (relax-pattern-posp p)) (svref (relax-pattern-pattern p) (svref (relax-pattern-indices p) (relax-pattern-posp p))))
	       (setf (relax-pattern-posp p) (+ (relax-pattern-posp p) 1))
	       (setf (svref (relax-pattern-indices p) (relax-pattern-posp p)) (relax-pattern-posp p))
	       (setf (svref (relax-pattern-lastswaps p) (relax-pattern-posp p)) -1)))))))))

;; generates next pattern for max aggregation operator (again ugly iterative version)
(defun relax-pattern-max-next (p stream)
  (if (relax-pattern-permute? p)
      (let ((continue? (relax-pattern-permute p stream)))
	(if (not continue?)
	    (progn
	      (setf (relax-pattern-permute? p) nil)
	      (relax-pattern-max-next p stream))
	  t))
    (if (= (relax-pattern-pos p)  0)
	nil
      (progn
	(when *debug* (format stream "~&;;generating next pattern (-> pattern)~%"))
	(when *debug* (format stream "~&;;~A~%" p))
	(setf (svref (relax-pattern-pattern p) (relax-pattern-pos p)) (+ (svref (relax-pattern-pattern p) (relax-pattern-pos p)) 1))
	
	(if (< (relax-pattern-pos p) (- (relax-pattern-size p) 1))
	    (progn
	      (setf (relax-pattern-pos p) (+ (relax-pattern-pos p) 1))
	      (loop for i from (relax-pattern-pos p) to (- (relax-pattern-size p) 1) do (setf (svref (relax-pattern-pattern p) i) 0)))
	  (do ()
	      ((or (= (relax-pattern-pos p) 0)
		   (< (svref (relax-pattern-pattern p) (relax-pattern-pos p)) (svref (relax-pattern-pattern p) (- (relax-pattern-pos p) 1)))))
	    (setf (relax-pattern-pos p) (- (relax-pattern-pos p) 1))))	
	(setf (relax-pattern-permute? p) t)
	(relax-pattern-max-next p stream)))))

;; generates next pattern for sum aggregation operator (again ugly iterative version)
(defun relax-pattern-sum-next (p stream)
  (if (relax-pattern-permute? p)
      (let ((continue? (relax-pattern-permute p stream)))
	(if (not continue?)
	    (progn
	      (setf (relax-pattern-permute? p) nil)
	      (relax-pattern-sum-next p stream))
	  t))
    (let ((done? nil)
	  (found? nil))
      (when *debug* (format stream "~&;;generating next pattern (-> pattern)~%"))
      (when *debug* (format stream "~&~A~%" p))
  
      (setf (svref (relax-pattern-pattern p) (relax-pattern-pos p)) (- (svref (relax-pattern-pattern p) (relax-pattern-pos p)) 1))
      (do ()
	  (done? found?)

	(if (< (svref (relax-pattern-pattern p) (relax-pattern-pos p)) 0)
	    ;; up
	    (if (= (relax-pattern-pos p) 0)
		(progn
		  (setf done? t)
		  (setf (svref (relax-pattern-pattern p) (relax-pattern-pos p)) 0))
	      (progn
		(setf (svref (relax-pattern-pattern p) (relax-pattern-pos p)) 0)
		(setf (relax-pattern-pos p) (- (relax-pattern-pos p) 1))
		(setf (relax-pattern-rem p) (+ (relax-pattern-rem p) (svref (relax-pattern-pattern p) (relax-pattern-pos p))))
		(setf (svref (relax-pattern-pattern p) (relax-pattern-pos p)) (- (svref (relax-pattern-pattern p) (relax-pattern-pos p)) 1))))
	      
	  ;; down?
	  (if (> (relax-pattern-rem p) 
		 (* (- (relax-pattern-size p)
		       (relax-pattern-pos p))
		    (svref (relax-pattern-pattern p) (relax-pattern-pos p))))
	      (setf (svref (relax-pattern-pattern p) (relax-pattern-pos p)) -1)
	    
	    (if (= (relax-pattern-pos p) (- (relax-pattern-size p) 1))
		;; pattern found
		(progn
		  (setf done? t)
		  (setf found? t))
	      ;; really going down
	      (let ((v (svref (relax-pattern-pattern p) (relax-pattern-pos p))))
		(setf (relax-pattern-rem p) (- (relax-pattern-rem p) v))
		(setf (relax-pattern-pos p) (+ (relax-pattern-pos p) 1))
		(setf (svref (relax-pattern-pattern p) (relax-pattern-pos p)) (min (relax-pattern-rem p) v)))))))
      (if found?
	  (progn
	    (setf (relax-pattern-permute? p) t)
	    (relax-pattern-sum-next p stream)) 
	nil))))

;; computes intersection of a list of networks in matrix form
(defun intersect-networks (calculus rel-rep networks l stream)
  (let*((result (make-array (list l l) :initial-element (relation-representation-universal-relation rel-rep)))
	(empty-relation-occurred? nil))
    (when *debug* (format stream "~&;;intersect-networks~%"))
    (do ((x 0 (+ x 1)))
	((= x l))
      (do ((y (+ x 1) (+ y 1)))
	  ((= y l))
	(dolist (n networks)
	  (setf (aref result x y) (funcall (relation-representation-intersect rel-rep) (aref result x y) (aref n x y)))
	  (if (ofuncall (relation-representation-empty-relation? rel-rep) (aref result x y))
	      (setf empty-relation-occurred? t)))))
    (when *debug* (format stream "~&;;~A~%" result))
    (values result empty-relation-occurred?)))

;; computes union of a list of networks in matrix form
(defun unite-networks (calculus rel-rep networks l stream)
  (let*((result (make-array (list l l) :initial-element (relation-representation-empty-relation rel-rep)))
	(empty-relation-occurred? nil))
    (when *debug* (format stream  "~&;;unite-networks~%"))
    (do ((x 0 (+ x 1)))
	((= x l))
      (do ((y (+ x 1) (+ y 1)))
	  ((= y l))
	(dolist (n networks)
	  (setf (aref result x y) (funcall (relation-representation-unite rel-rep) (aref result x y) (aref n x y)))
	  (if (ofuncall (relation-representation-empty-relation? rel-rep) (aref result x y))
	      (setf empty-relation-occurred? t)))))
    (when *debug* (format stream "~&;;~A~%" result))
    (values result empty-relation-occurred?)))


;; 
;; functions realizing main operations
;;

;; yields relation consisting of all conceptual neighbors of "r"
(defun neighbors (calculus cnh rel-rep r)
  (ofuncall (relation-representation-minus rel-rep) (ofuncall cnh calculus r) r)) 


;; recursive auxiliary function for neighbors-nth
(defun neighbors-nth-rec (calculus cnh rel-rep r p step)
  (if (and (> step 0)
	   (not (ofuncall (relation-representation-empty-relation? rel-rep) r)))
      (neighbors-nth-rec calculus cnh rel-rep (ofuncall (relation-representation-minus rel-rep) (neighbors calculus cnh rel-rep r) p) r (- step 1))
     r))

;; yields relation of conceptual neighbors with minimal distance "step" to a base relation in "r"
(defun neighbors-nth (calculus cnh rel-rep r step)
  (neighbors-nth-rec calculus cnh rel-rep r r step))


;;  yields new relation with all conceptual neighbors added to the original relation "r"
(defun relax (calculus cnh rel-rep r)
  (ofuncall (relation-representation-unite rel-rep) (ofuncall cnh calculus r) r))

;; yields new relation with all conceptual neighbors with distance at most "step" added to the original relation "r"
(defun relax-nth (calculus cnh rel-rep r step)
  (if (> step 0)
      (relax-nth calculus cnh rel-rep (relax calculus cnh rel-rep r) (- step 1))
    r))


;; recursive auxiliary function for neighborhood-distance
(defun neighborhood-distance-rec (calculus cnh rel-rep r1 r2 p)
  (if (funcall (relation-representation-empty-relation? rel-rep) r1)
      nil
    (if (funcall (relation-representation-empty-relation? rel-rep) (funcall (relation-representation-intersect rel-rep) r1 r2))
	(+ 1 (neighborhood-distance-rec calculus cnh rel-rep (neighbors-nth-rec calculus cnh rel-rep r1 p 1) r2 r1)) 
      0)))

;; yields minimal distance (length of shortest path in the neighborhood structure) between two base relations from "r1" and "r2"
;;
;; TODO: replace with a real all-to-all graph search which results are stored in a precompiled table;
;; currently will not terminate if r2 is not contained in an incremental relaxation of r1;
;; also there are several possibilities of how distance could be extended to general relations 
(defun neighborhood-distance (calculus cnh rel-rep r1 r2)
  (if (funcall (relation-representation-empty-relation? rel-rep) (funcall (relation-representation-intersect rel-rep) r1 r2))
	(+ 1 (neighborhood-distance-rec calculus cnh rel-rep (neighbors-nth-rec calculus cnh rel-rep r1 r1 1) r2 r1)) 
    0))


;; yields similarity distance between networks given by constraint lists "constraints1" and "constraints2"
;; using "constraint-aggregation-op" to aggregate over corresponding constraints
(defun similarity (calculus cnh rel-rep aggregate-constraints-op objects constraints1 constraints2)
  (let ((network1 (make-network-matrix calculus rel-rep objects constraints1))
	(network2 (make-network-matrix calculus rel-rep objects constraints2))
	(l (length objects))
	(result 0))
    (do ((x 0 (+ x 1)))
	((= x l))
      (do ((y (+ x 1) (+ y 1)))
	  ((= y l))
	(setf result (funcall aggregate-constraints-op result (neighborhood-distance calculus cnh rel-rep (aref network1 x y) (aref network2 x y))))))
    result))

(sparq:defcommand ("neighborhood-reasoning" (c calculus)  (cnh neighborhood c) "similarity" (option (member cl-user::sum cl-user::max)) (csp1 constraint-network c) (csp2 constraint-network c))
    (let ((*readtable* *lisp-readtable*))
      (similarity c (neighborhood-ofun cnh) (calculi:calculus-relation-representation c) (parse-operator option) (append (csp:constraint-network-objects csp1) (csp:constraint-network-objects csp2))
		  ;(read-from-string (format nil "~a" csp1)) (read-from-string (format nil "~a" csp2))
		  (csp:constraints csp1) (csp:constraints csp2)
		  )))


;; distance-based merging 

(defun relax-constraints (calculus cnh rel-rep network pattern stream)
  (let*((l (array-dimension network 0))
	(result (make-array (list l l) :initial-element (relation-representation-universal-relation rel-rep)))
	(i 0)
	(not-possible? nil))
    (when *debug* (format stream "~&;;relax-constraints (-> input network + pattern)~%"))
    (when *debug* (format stream "~&;;~A~%" network))
    (when *debug* (format stream "~&;;~A~%" pattern))
    (do ((x 0 (+ x 1)))
	((or not-possible? (= x l)))
      (do ((y (+ x 1) (+ y 1)))
	  ((or not-possible? (= y l)))
	(let ((r (neighbors-nth calculus cnh rel-rep (aref network x y) (svref pattern i))))
	  (if (funcall (relation-representation-empty-relation? rel-rep) r)
	      (setf not-possible? t)
	    (setf (aref result x y) r))
	  (setf i (+ i 1)))))
    (when *debug* (format stream "~&;;leaving relax-constraint (-> result)~%"))
    (when *debug* (format stream "~&;;not-possible: ~A result: ~A~%" not-possible? result))
    ;(when not-possible? (print "skipped"))
    (if not-possible?
	nil
      result)))

(defun relax-network (calculus cnh rel-rep aggregate-constraints-op network d max stream)
  (let*((s (array-dimension network 0))
	(pattern (relax-pattern-init aggregate-constraints-op (/ (- (* s s) s) 2) d max stream)) 
	(next-function (relax-pattern-next-function pattern))
	(N nil)
	(continue? t))
   ; (when (= d 4) (setf *debug* t))
    (when *debug* (format stream "~&;;relax-network~%"))
    (do ()
	((not (funcall next-function pattern stream)))
      (when *debug* (format stream "~&;;~A~%" pattern))
      (let ((net (relax-constraints calculus cnh rel-rep network (relax-pattern-pattern pattern) stream)))
	(if net
	    (setf N (cons net  N)))))
    (when *debug* (format stream "~&;;leaving relax-network~%"))
    (when *debug* (format stream "~&;;~A~%" N))
    N))

(defun compute-intersections (calculus cnh rel-rep networks network-patterns stream)
  (let ((l (length networks))
	(results nil)
	(not-possible? nil)
	(i 0))
    (dolist (x networks) ; could be optimized by stopping when first not-possible? is found
      (let ((net (relax-constraints calculus cnh rel-rep x (relax-pattern-pattern (svref network-patterns i)) stream)))
	(if net
	    (setf results (cons net results))
	  (setf not-possible? t))
	(setf i (+ i 1))))
    (if (not not-possible?)
	(multiple-value-bind (int empty?) (intersect-networks calculus rel-rep results (array-dimension (car networks) 0) stream)
	  (if (not empty?) 
	      (progn
		;(print "returning from compute intersection")
		;(print (list int))
		(list int))
	    nil))
      nil)))
     
	


(defun relax-networks-rec (calculus cnh rel-rep aggregate-constraint-op networks pattern network-patterns max stream d l)
  (let ((next-function (relax-pattern-next-function (svref network-patterns d)))
	(results nil)
	(s (array-dimension (car networks) 0)))
    (when *debug* (print "networks-rec"))
    (if (= (- l 1) d)
	(progn 
		  (do ()
	      ((not (funcall next-function (svref network-patterns d) stream)))
	    (setf results (append (compute-intersections calculus cnh rel-rep networks network-patterns stream) results)))
	  (setf (svref network-patterns d) (relax-pattern-init aggregate-constraint-op (/ (- (* s s) s) 2) (svref pattern d) max stream)))	;; could be optimized
      (progn
	
	(do ()
	    ((not (funcall next-function (svref network-patterns d) stream)))
	  (setf results (append (relax-networks-rec calculus cnh rel-rep aggregate-constraint-op networks pattern network-patterns max stream (+ d 1) l) results)))
	(setf (svref network-patterns d) (relax-pattern-init aggregate-constraint-op (/ (- (* s s) s) 2) (svref pattern d) max stream))))	 ; could be optimized
    results))
 
  
(defun relax-networks2 (calculus cnh rel-rep aggregate-constraint-op networks pattern max stream)
  (let*((l (length pattern))
	(s (array-dimension (car networks) 0))
	(intersections nil)
	(network-patterns (make-array l)))
    (when *debug* (format stream "~&;;relax-networks~%"))
    (when *debug* (format stream "~&;;~A~%" networks))
    (when *debug* (format stream "~&;;~A~%" pattern))
    (do ((j 0 (+ j 1))) ((= j l)) 
      (setf (svref network-patterns j) (relax-pattern-init aggregate-constraint-op (/ (- (* s s) s) 2) (svref pattern j) max stream))) 
    (relax-networks-rec calculus cnh rel-rep aggregate-constraint-op networks pattern network-patterns max stream 0 l)))
  

(defun relax-networks (calculus cnh rel-rep aggregate-constraints-op networks pattern max stream)
  (let*((l (length pattern))
	(relaxations nil)
	(i 0)
	(intersections nil))
    (when *debug* (format stream "~&;;relax-networks~%"))
    (when *debug* (format stream "~&;;~A~%" networks))
    (when *debug* (format stream "~&;;~A~%" pattern))
    (dolist (n networks)
      (setf relaxations (cons (relax-network calculus cnh rel-rep aggregate-constraints-op n (svref pattern i) max stream) relaxations))
      (setf i (+ i 1)))
    (when *debug* (format stream "~&;;interstection part~%"))
    (when *debug* (format stream "~&;;~A~%" relaxations))
    (print (mapcar #'length relaxations))
    (dolist (x (tuples relaxations))
      (multiple-value-bind (int empty?) (intersect-networks calculus rel-rep x (array-dimension (car networks) 0) stream)
	(if (not empty?)
	    (progn
	      (when *debug* (format stream "~&;;~A~%" int))
	      (setf intersections (cons int intersections))))))
    (when *debug* (format stream "~&;;leaving relax-networks~%"))
    (when *debug* (format stream "~&;;~A~%" intersections))
    intersections))

(defun relax-profile (calculus cnh rel-rep aggregate-constraints-op aggregate-networks-op networks d max stream)
  (let*((l (length networks))
        (m (if (and (eq aggregate-constraints-op 'cl-user::sum) ;; quick optimization only for sum sum
                    (eq aggregate-networks-op 'cl-user::sum))
             (* max (array-dimension (car networks) 0))
             -1))
	(pattern (relax-pattern-init aggregate-networks-op l d -1 stream))
	(next-function (relax-pattern-next-function pattern))
	(intersections nil); will contain all intersection networks
	(continue? t)) 
    (when *debug* (format stream "~&;;relax-profile~%"))
    (do ()
	((not (funcall next-function pattern stream)))
      (when *debug* (format stream "~&;;~A~%" pattern))
      (let ((relaxes (relax-networks2 calculus cnh rel-rep aggregate-constraints-op networks (relax-pattern-pattern pattern) max stream)))
;	(print relaxes)
;	(print intersections)
	(setf intersections (append intersections relaxes))))
    
    (when *debug* (format stream "~&;;leaving relax-profile~%"))
    (when *debug* (format stream "~&;;~A~%" intersections))
    intersections))
    
;; main function for distance based merging; currently merging-approach is ignored as there is only one variant
(defun distance-based-merge (calculus cnh rel-rep merging-approach aggregate-constraints-op aggregate-networks-op objects networks stream)
  (let*((l (length objects))                                                                           ; overall number of variables
	(objects-mod nil) 
	(P (mapcar #'(lambda (x)                                                                       ; input set (profile) in matrix form
		       (make-network-matrix calculus rel-rep objects x))
		   networks))
	(S (make-array (list l l) :initial-element (relation-representation-empty-relation rel-rep)))  ; resulting network in matrix form
        (startdistance (if (and (eq aggregate-networks-op 'cl-user::sum) ; first optimization for sum
                                (> (length networks) 1))
                         (similarity calculus cnh rel-rep (parse-operator aggregate-constraints-op) objects (first networks) (second networks))
                         0))
        (max 2) ; !!! huge hack for demo, not correct for other many calculi !!!
   	(consistent-found? nil))                                                                       ; flag to indicate when consistent network was found
    (loop for i from l downto 1 do (setf objects-mod (cons (- i 1) objects-mod)))
    (do ((d startdistance (+ d 1)))
	(consistent-found?)
      (when *debug* (format t "~&d is ~A~%" d))
      (let ((R (relax-profile calculus cnh rel-rep aggregate-constraints-op aggregate-networks-op P d max stream))) 
	(if R
	    (progn
	      (when *debug* (format t "~&;;checking consistency now~%"))
	      (dolist (x R)
	
		;; check consistency of R
		(let((now2 (get-internal-real-time))
		     (clist (make-constraint-list-from-matrix x)))
		  
		  (when *debug* (format stream "~&;;~A~%" clist))

		  (cond ((eq :binary (calculus-arity calculus)) 
			 (setf consistent-found? (or consistent-found? (not (eq "Not consistent." (test-pathconsistency/binary calculus objects-mod clist))))))
			((eq :ternary (calculus-arity calculus)) 
			 (if (calculi:calculus-n-ary-composition calculus)
			     (setf consistent-found? (or consistent-found? (test-ternary-closure calculus objects-mod clist)))
			   (signal-error "No specification of ternary composition is calculus definition of '~a'" (calculus-name calculus))))
			(t (signal-error "Unsupported arity of calculus '~a' ~a; :binary or :ternary are supported.~%" 
					 (calculus-name calculus)  (calculus-arity calculus))))
		  
		  (when *debug* (format stream "~&;;~A~%" consistent-found?))))
	      (setf S (unite-networks calculus rel-rep (cons S R) (array-dimension (car P) 0) stream))))
	(when *debug* (format stream "~&;;done for this d~%"))))
    (when *debug* (format stream "~&;;leaving merging~%"))
    (when *debug* (if consistent-found? (format stream "~&;;consistent network has been found~%")))
    S))
	

;;
;; dispatcher for neighborhood-reasoning module
;;


;;
;; class neighborhoods to ease command parsing/error checking
;;
(defclass neighborhood (primitive)
  ((ofun :reader neighborhood-ofun
	 :initarg :ofun)
   (names :reader neighborhood-names
	  :initarg :names)))

(defmethod parse-primitive ((n (eql 'neighborhood)) (expr symbol) &key calculus)
  (let* ((name (intern (string-upcase (symbol-name expr)))) ; strip away case-sensitivism
	 (neighborhood (find name (calculi:calculus-cnhs calculus)
			     :key #'first
			     :test #'member)))
    (if neighborhood
	(make-instance 'neighborhood
		       :ofun (second neighborhood)
		       :names (first neighborhood))
	(cons :FAIL (format nil "No conceptual neighborhood '~a' defined for ~a" expr calculus)))))


(defmethod print-object ((n neighborhood) stream)
  (format stream "<neighborhood '~a'>" (first (neighborhood-names n))))

;;;
;;; SparQ user commands in module 'neighborhood-reasoning'
;;;

(sparq:defcommand ("neighborhood-reasoning" (c calculus)  (cnh neighborhood c) "neighbors" (r relation c))
  "gives the conceptual neighbors of a relation"
  (make-instance 'calculi:relation
		 :value (neighbors c (neighborhood-ofun cnh) (calculi:calculus-relation-representation c) (calculi:relation-value r))
		 :calculus c))

(sparq:defcommand ("neighborhood-reasoning" (c calculus) (cnh neighborhood c) "merge" (option (member cl-user::sum cl-user::max)) (option2 (member cl-user::sum cl-user::max))
					    (csp1 csp:constraint-network c)  (csp2 csp:constraint-network c))
   (let ((merging-approach ())
	  (aggregate-constraints-op option)
	  (aggregate-networks-op option2)
	 (networks ())
	 (rel-rep (calculi:calculus-relation-representation c))
	 (allobjects ()))
     (dolist (net (let ((*readtable* *lisp-readtable*))
		    (list (read-from-string (format nil "~a" csp1)) (read-from-string (format nil "~a" csp2)))))
       (multiple-value-bind (objects constraints) (parse-constraint-network c (list net))
	 (setf networks (cons constraints networks))
	 (setf allobjects (union allobjects objects))))

     (setf allobjects (reverse allobjects))  ; just a hack to make result more readable (negates the effect of union in sbcl which inverses the order)
     (print-network-matrix c rel-rep allobjects (distance-based-merge c (neighborhood-ofun cnh) rel-rep merging-approach aggregate-constraints-op aggregate-networks-op allobjects networks *sparq-io*)
			   *sparq-io*))
)


(sparq:defcommand ("neighborhood-reasoning" (c calculus)  (cnh neighborhood c) "relax" (r relation c))
  "includes all conceptual neighbors of a relation"
  (make-instance 'calculi:relation
		 :value (relax c (neighborhood-ofun cnh) (calculi:calculus-relation-representation c) (calculi:relation-value r))
		 :calculus c))

(sparq:defcommand ("neighborhood-reasoning" (c calculus)  (cnh neighborhood c) "neighborhood-distance" (r1 relation c) (r2 relation c))
    "distance in the neighborhood graph"
  (let ((relrep (calculi:calculus-relation-representation c)))
    (neighborhood-distance c (neighborhood-ofun cnh) relrep (calculi:relation-value r1) (calculi:relation-value r2))))

; same with similarity

#|
(sparq:defcommand ("neighborhood-reasoning" (c calculus) (cnh neighborhood c) "merge" (dummy t) (option (member cl-user::sum cl-user::max)) (option2 (member cl-user::sum cl-user::max)) &rest input)
  (let ((merging-approach dummy)
	(aggregate-constraints-op option)
	(aggregate-networks-op option2)
	(networks nil)
	(rel-rep (calculi:calculus-relation-representation c))
	(allobjects nil))		  
    (dolist (net input)
      (multiple-value-bind (objects constraints) (parse-constraint-network c (list net))
	(setf networks (cons constraints networks))
	(setf allobjects (union allobjects objects))))
    
    (setf allobjects (reverse allobjects))  ; just a hack to make result more readable (negates the effect of union in sbcl which inverses the order)
    (print-network-matrix c rel-rep allobjects (distance-based-merge c (neighborhood-ofun cnh) rel-rep merging-approach aggregate-constraints-op aggregate-networks-op allobjects networks *sparq-io*)
			  *sparq-io*))
  (make-instance 'silent-value :value ()))
|#

(defun neighborhood-reasoning (stream calculus args)
  (let ((nh (and args
                       (pop args)))
	(cnhs (calculi:calculus-cnhs calculus)))
    (let ((cnh (second (find-if (lambda (l) (let ((f (first l))) (find-if (lambda (x) (equal nh x)) f))) cnhs)));; find correct neighborhood structure
	  (operator (and args
			 (pop args)))
	  (rel-rep (calculi:calculus-relation-representation calculus)))
      (labels ((print-relation (r)
			       (ofuncall (relations:relation-representation-printer rel-rep)
					 rel-rep r stream)
			       (finish-output stream)))
       
	(cond  ((or (eq operator 'cl-user::similarity)
		    (eq operator 'cl-user::sim))
		(let ((aggregate-constraints-op (parse-operator (and args
								     (pop args)))))
		  (multiple-value-bind (objects1 constraints1) (parse-constraint-network calculus args)
		    (pop args)
		    (multiple-value-bind (objects2 constraints2) (parse-constraint-network calculus args)
		      (let*((allobjects (union objects1 objects2)))
			(format stream "~A~%" (similarity calculus cnh rel-rep aggregate-constraints-op allobjects constraints1 constraints2))
			(finish-output stream))))))

	       ((or (eq operator 'cl-user::neighbors)
		    (eq operator 'cl-user::nbs))
		(print-relation (neighbors calculus cnh rel-rep (ofuncall (relation-representation-encoder rel-rep) rel-rep (first args)))))
	       
	       ((or (eq operator 'cl-user::neighbors-nth)
		    (eq operator 'cl-user::nbs-nth))
		(let ((steps (and args
				  (pop args))))
		  (print-relation (neighbors-nth calculus cnh rel-rep (ofuncall (relation-representation-encoder rel-rep) rel-rep (first args)) steps))))

	       ((or (eq operator 'cl-user::relax)
		    (eq operator 'cl-user::rel))
		(print-relation (relax calculus cnh rel-rep (ofuncall (relation-representation-encoder rel-rep) rel-rep (first args)))))

	       ((or (eq operator 'cl-user::relax-nth)
		    (eq operator 'cl-user::rel-nth))
		(let ((steps (and args
				  (pop args))))
		  (print-relation (relax-nth calculus cnh rel-rep (ofuncall (relation-representation-encoder rel-rep) rel-rep (first args)) steps))))

	       ((or (eq operator 'cl-user::neighborhood-distance)
		    (eq operator 'cl-user::ndist))
		(format stream "~A~%" (neighborhood-distance calculus cnh rel-rep 
							     (ofuncall (relation-representation-encoder rel-rep) rel-rep (first args)) 
							     (ofuncall (relation-representation-encoder rel-rep) rel-rep (second args)))))

	       ((eq operator 'cl-user::merge)
		(let ((merging-approach (and args
					     (pop args)))
		      (aggregate-constraints-op (and args
						     (pop args)))
		      (aggregate-networks-op (and args
						  (pop args)))
		      (networks nil)
		      (allobjects nil))		  
		  (do () 
		      ((not args))
		    (multiple-value-bind (objects constraints) (parse-constraint-network calculus args)
		      (pop args)
		      (setf networks (cons constraints networks))
		      (setf allobjects (union allobjects objects))))
		  (setf allobjects (reverse allobjects))  ; just a hack to make result more readable (negates the effect of union in sbcl which inverses the order)
		  (print-network-matrix calculus rel-rep allobjects (distance-based-merge calculus cnh rel-rep merging-approach aggregate-constraints-op aggregate-networks-op allobjects networks stream)
					 stream)))
	      
	       (t               
		(signal-error "Error: Operator ~a unknown!~%" operator)))))))
