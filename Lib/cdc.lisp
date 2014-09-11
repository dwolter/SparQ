(eval-when (:execute :load-toplevel :compile-toplevel)
  (sb-alien:load-shared-object "/Users/diedrich/Desktop/CDC/cdc.dylib"))

(defun translate-cdc-rel (relname)
  "translates a relation from SparQ's notation into that of the library"
  (let* ((n (length relname))
	 (str (subseq relname 1 (- n 1))))
    (decf n 2)
    (dotimes (i n)
      (cond ((char= (char str i) #\-) (setf (char str i) #\:))
	    ((char= (char str i) #\B) (setf (char str i) #\O))))
    str))

(defun check-atomic-cdc-csp (csp)
  "workhorse function for checking atomic CSPs of CDC relations"
  (let* ((cdc    (csp:constraint-network-calculus csp))
	 (relrep (calculi:calculus-relation-representation cdc))
	 (objs   (csp:constraint-network-objects csp))
	 (n      (length objs))
	 (cs     (csp:constraints csp)))
    
    (sb-alien:alien-funcall (sb-alien:extern-alien "CDCSetSize" (sb-alien:function sb-alien:void sb-alien:int)) n)
    (sb-alien:alien-funcall (sb-alien:extern-alien "CDCSetConnected" (sb-alien:function sb-alien:void sb-alien:int)) 1)
    (dolist (c cs)
      (let ((o1 (position (csp:constraint-object-1 c) objs))
	    (o2 (position (csp:constraint-object-2 c) objs))
	    (r  (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep (csp:constraint-relation c))))
	(sb-alien:alien-funcall (sb-alien:extern-alien "setConstraint" (sb-alien:function sb-alien:void sb-alien:int sb-alien:int sb-alien:c-string))
				o1 o2 (translate-cdc-rel r))))
    ;; add equality relations
    (dotimes (i n)
      (sb-alien:alien-funcall (sb-alien:extern-alien "setConstraint" (sb-alien:function sb-alien:void sb-alien:int sb-alien:int sb-alien:c-string))
				i i "O"))
    (let ((result (sb-alien:alien-funcall (sb-alien:extern-alien "checkConsistency" (sb-alien:function sb-alien:int)))))
      ;(sparq:print-out "~%checkConsistency() ----> ~a~%" result)
      (if (= 1 result)
	  "Consistent."
	  "Not consistent."))))
    

(defun check-3-csp (cdc r1 r1c r2 r2c r3 r3c)
  "checks consistency of a 'composition triple'"
  (let ((relrep (calculi:calculus-relation-representation cdc)))
    (ofunc:ofuncall (relations:relation-representation-mapper relrep)
		    #'(lambda (r1ci)
			(let ((r1c (svref (relations:relation-representation-br-encodings relrep) r1ci))) ; converse of r1
			  (ofunc:ofuncall (relations:relation-representation-mapper relrep)
					  #'(lambda (r2ci)
					      (let ((r2c (svref (relations:relation-representation-br-encodings relrep) r2ci))) ; converse of r2
						(ofunc:ofuncall (relations:relation-representation-mapper relrep)
								#'(lambda (r3ci)
								    (let ((r3c (svref (relations:relation-representation-br-encodings relrep) r3ci))) ; converse of r3
								      (when (equal "Consistent."
										   (check-atomic-cdc-csp (make-instance 'csp:constraint-network
															:objects '(a b c)
															:calculus cdc
															:constraints (list (csp:make-constraint 'a r1  'b)
																	   (csp:make-constraint 'b r1c 'a)
																	   (csp:make-constraint 'b r2  'c)
																	   (csp:make-constraint 'c r2c 'b)
																	   (csp:make-constraint 'a r3  'c)
																	   (csp:make-constraint 'c r3c 'a)))))
									;; if we have found a satifiable combination, no need to search on - just return 'true'
									(return-from check-3-csp t))))
								r3c)))
					  r2c)))
		    r1c)
    ;; if we haven't found a satisifiable combination, return 'false'
    nil))
 
(defun check-cdc-composition (cdc)
  "veryfies the cdc composition table"
  (let* ((relrep (calculi:calculus-relation-representation cdc))
	 (n      (relations::relation-representation-num-base-relations relrep)))
    (dotimes (r1-idx n)
      (let* ((r1 (svref (relations:relation-representation-br-encodings relrep) r1-idx))
	     (r1c (calculi:converse cdc r1)))
	(dotimes (r2-idx n)
	  (let* ((r2 (svref (relations:relation-representation-br-encodings relrep) r2-idx))
		 (r2c (calculi:converse cdc r2))
		 (r3 (calculi:composition cdc r1 r2))
		 (r3c (calculi:converse cdc r3)))
	    (sparq:print-out "~%~3a o ~3a  : " 
		    (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r1)
		    (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r2))
	    ;; Part #1: check whether all atomic relations contained in the composition give consistent CSPs
	    (ofunc:ofuncall (relations:relation-representation-mapper relrep)
			    #'(lambda (ri)
				(let ((r (svref (relations:relation-representation-br-encodings relrep) ri)))
				  (if (check-3-csp cdc r1 r1c r2 r2c r (calculi:converse cdc r))
				      (sparq:print-out "~3a " (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))
				      (sparq:print-out "§b1§~3a§b0§ " (format nil "-~a" (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))))))
			    r3)
	    ;; Part #2: check whether all atomic relations not contained in the composition give inconsistent CSPs
	    (ofunc:ofuncall (relations:relation-representation-mapper relrep)
			    #'(lambda (ri)
				(let ((r (svref (relations:relation-representation-br-encodings relrep) ri)))
				  (if  (check-3-csp cdc r1 r1c r2 r2c r (calculi:converse cdc r))
				      (sparq:print-out "§b1§~3a§b0§ " (format nil "+~a" (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))))))
			    (calculi:compl cdc r3))))))))

(defun check-cdc-converse (cdc)
  "veryfies the cdc composition table"
  (let* ((relrep (calculi:calculus-relation-representation cdc))
	 (n      (relations::relation-representation-num-base-relations relrep)))
    (dotimes (r1-idx n)
    ;;(progn
      (let* ((r1 (svref (relations:relation-representation-br-encodings relrep) r1-idx))
	     ;;(r1 (ofunc:ofuncall (relations:relation-representation-encoder relrep) relrep (format nil "~a" s)))
	     (r1c (calculi:converse cdc r1)))
	(sparq:print-out "~2%~a:~%" (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r1))
	;; Part #1: check that all atomic relations in the converse of r1 are indeed possible
	(ofunc:ofuncall (relations:relation-representation-mapper relrep)
			#'(lambda (ri)
			    (let ((r (svref (relations:relation-representation-br-encodings relrep) ri)))
			      (if (equal "Consistent." (check-atomic-cdc-csp (make-instance 'csp:constraint-network
											    :objects '(a b)
											    :calculus cdc
											    :constraints (list (csp:make-constraint 'a r1  'b)
													       (csp:make-constraint 'b r   'a)))))
				  (sparq:print-out "~3a " (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))
				  (sparq:print-out "§b1§~3a§b0§ " (format nil "[~a]" (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))))))
			r1c)
	;; Part #2: check that all atomic relations not declared in the converse of r1 are indeed not possible
	(let ((misses 0))
	  (ofunc:ofuncall (relations:relation-representation-mapper relrep)
			  #'(lambda (ri)
			      (let ((r (svref (relations:relation-representation-br-encodings relrep) ri)))
				(when (equal "Consistent." (check-atomic-cdc-csp (make-instance 'csp:constraint-network
												:objects '(a b)
												:calculus cdc
												:constraints (list (csp:make-constraint 'a r1  'b)
														   (csp:make-constraint 'b r   'a)))))
				  (incf misses)
				  (sparq:print-out "§b1§~3a§b0§ " (format nil "+~a+" (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))))))
			  (calculi:compl cdc r1c))
	  (sparq:print-out "~%--> ~d relations missing" misses))))))

(defun check-cdc-composition-single (cdc s1 s2)
  "veryfies the cdc composition table"
  (let* ((relrep (calculi:calculus-relation-representation cdc))
	 (n      (relations::relation-representation-num-base-relations relrep)))
    (progn 
      (let* ((r1  (ofunc:ofuncall (relations:relation-representation-encoder relrep) relrep s1))
	     (r1c (calculi:converse cdc r1)))
	(progn ;dotimes (r2-idx n)
	  (let* ((r2  (ofunc:ofuncall (relations:relation-representation-encoder relrep) relrep s2))
		 (r2c (calculi:converse cdc r2))
		 (r3  (calculi:composition cdc r1 r2))
		 (r3c (calculi:converse cdc r3)))
	    (sparq:print-out "~%~3a o ~3a  : " 
		    (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r1)
		    (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r2))
	    ;; Part #1: check whether all atomic relations contained in the composition give consistent CSPs
	    (ofunc:ofuncall (relations:relation-representation-mapper relrep)
			    #'(lambda (ri)
				(let ((r (svref (relations:relation-representation-br-encodings relrep) ri)))
				  (if (check-3-csp cdc r1 r1c r2 r2c r (calculi:converse cdc r))
				      (sparq:print-out "~3a " (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))
				      (sparq:print-out "§b1§~3a§b0§ " (format nil "-~a" (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))))))
			    r3)
	    ;; Part #2: check whether all atomic relations not contained in the composition give inconsistent CSPs
	    (ofunc:ofuncall (relations:relation-representation-mapper relrep)
			    #'(lambda (ri)
				(let ((r (svref (relations:relation-representation-br-encodings relrep) ri)))
				  (if  (check-3-csp cdc r1 r1c r2 r2c r (calculi:converse cdc r))
				      (sparq:print-out "§b1§~3a§b0§ " (format nil "+~a" (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r))))))
			    (calculi:compl cdc r3))))))))