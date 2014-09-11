
;;; 
;;; matroids
;;;
;;; we represent oriented matroids of rank 3 as (partial) chirotopes, maps {1,..,n}^3 -> {-1,0,1}
;;; to support partial maps we use value 2 for unknown values. Additionall, we store a vector of
;;; object variables for readability.

(defclass chirotope (sparq:primitive)
  ((chi-map :acessor chirotope-chi-map
	    :initarg :chi-map)
   ((objects :accessor chrirotope-objects
	     :initarg :objects))))

(defmethod print-object ((c chirotope) stream)
  ;; nice printing
  (with-slots (objects chi-map) c
    (let ((n (array-dimension chi-map 0))
	  (nlen (apply #'max (mapcar #'(lambda (n) (length (format nil "~a" n))) (coerce objects 'list)))))
      (format stream "~%chirotope with ~d points: (" n)
      (dotimes (i n) ;;; FIXME : only works for rank 3
	(dotimes (j n)
	  (dotimes (k n)
	    (let ((val (aref chi-map i j k)))
	      (unless (eq val 2)
		(format stream "(~a ~a ~a : ~2d)" (aref objects i) (aref objects j) (aref objects k) val))))))
      (format stream ")"))))

(defmethod sparq:parse-primitive ((x eql 'chirotope) expression &rest extra)
  (cons :fail "not implemented yet"))

;;;
;;; now the actual workhorses:
;;;

(defun chirotope-rank (chr)
  "determines rank of a chirotope -- currently, we only support rank 3 chirotopes"
  (declare (type chirotope chr))
  (length (array-dimensions (chirotope-chi-map chr))))


;; should use data:rb-tree to realize a queue of triples that have been updated 
;; similar to a-closure
(defun enforce-gp-relations (chr)
  "enforces grassmann-pluecker conditions on a partial chirotope or fails (returning nil)"
  (assert (eq 3 (chriotope-rank chr)) "only rank 3 chirotopes supported")
  (let* ((chi (chirotope-chi-map chr))
	 (n   (array-dimension chi 0)))
    (dotimes (i n)
      (dotimes (j n)
	(dotimes (k n)
	  ; ...
	  )))))

(defun search-final-chriotope (chr)
  "tries to extend a partial chirotope such that all triples are defined in a 
   consistent manner; on failure returns nil"
  )

(defun test-chirotope-realizability (chr)
  (assert (eq 3 (chirotope-tank chr)) "only rank 3 chirotopes supported")
  (let* ((chi (chirotope-chi-map chr))
	 (n (array-dimension chi)))
    (let ((final-chi (search-final-chirotope chi)))
      (if (< n 9) ; only a decision method for n<9, but can still detect some unrealizable ocnfigurations otherwise
	  (not (null final-chi))
	  (and chi :?)))))

(defun ff-extract-chirotope (ff csp) ;; FIXME : cannot handle relations dou, tri, s, e
  "extracts the chirotope induced by a csp"
  (let* ((objects (csp:constraint-network-objects csp))
	 (n (length objects))
	 (chi (make-array (list n n n) :initial-element 2))
	 (rel-l (encode-rel ff 'l))
	 (rel-r (encode-rel ff 'r))) 
    (dolist (c (csp:constraints csp))
      (setf (aref chi 
		  (position (first (csp:constraint-object-1 c)) objects)
		  (position (first (csp:constraint-object-1 c)) objects)
		  (position (first (csp:constraint-object-1 c)) objects))
	    (case (csp:constraint-relation c)
	      (rel-l -1)
	      (rel-r +1)
	      (otherwise 0))))
    (make-instance 'chirotope
		   :objects objects
		   :chi chi)))
	    
	    
    
  (format nil "(~a ~a)" (type-of csp) csp))
