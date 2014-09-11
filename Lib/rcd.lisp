;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006-2013 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/
;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; translating between the block-algebra (Allen * Allen) for cardinal direction relation 
;;; between regions (for which Siangjiang's decision procedure can be applied) and the 
;;; axis-aligned box calculus RCD which can be obtained by coarsening the block-algebra
;;;

(defparameter *aia* '(EQ B BI D DI O OI M MI S SI F FI)
  "relations of Allen's interval algebra")

(defun expand-ba (allenrel-x allenrel-y)
  "expands product of block-algebra convenience notation (x1 ... xn) (y1 ... yn) to (x1_y1 ... xn_y1 x2_y2 ... xn_yn)"
  (assert (subsetp allenrel-x *aia*)) ; must be Allen relations
  (assert (subsetp allenrel-y *aia*))
  (loop for r1 in allenrel-x appending
       (loop for r2 in allenrel-y collecting
	    (intern (format nil "~a_~a" r1 r2)))))

;; mapping RCD -> Block-algebra
(defparameter *rcd* (let ((tmp '((b                     (d s f eq) (d s f eq))  ;; <== check me!
				 (s                     (d s f eq) (m b))
				 (n                     (d s f eq) (mi bi))
				 (e                     (mi bi)    (d s f eq))
				 (w                     (m b)      (d s f eq))
				 (ne                    (mi bi)    (mi bi))
				 (nw                    (m b)      (mi bi))
				 (se                    (mi bi)    (m b))
				 (sw                    (m b)      (m b))
				 (s_sw                  (fi o)     (m b))
				 (s_se                  (si oi)    (m b))
				 (nw_n                  (fi o)     (mi bi))
				 (n_ne                  (si oi)    (mi bi))
				 (b_w                   (fi o)     (d s f eq))
				 (b_e                   (si oi)    (d s f eq))
				 (b_s                   (d s f eq) (fi o))
				 (b_n                   (d s f eq) (si oi))
				 (w_sw                  (m b)      (fi o))
				 (w_nw                  (m b)      (si oi))
				 (e_se                  (mi bi)    (fi o))
				 (ne_e                  (mi bi)    (si oi))
				 (s_sw_se               (di)       (m b))
				 (nw_n_ne               (di)       (mi bi))
				 (b_w_e                 (di)       (d s f eq))
				 (b_s_n                 (d s f eq) (di))
				 (sw_n_nw               (m b)      (di))
				 (ne_e_se               (mi bi)    (di))
				 (b_s_sw_w              (o fi)     (o fi))
				 (b_w_nw_n              (o fi)     (si oi))
				 (b_s_e_se              (si oi)    (o fi))
				 (b_n_ne_e              (si oi)    (si oi))
				 (b_s_sw_w_nw_n         (o fi)     (di))
				 (b_s_n_ne_e_se         (si oi)    (di))
				 (b_s_sw_w_e_se         (di)       (fi o))
				 (b_w_nw_n_ne_e         (di)       (si oi))
				 (b_s_sw_w_nw_n_ne_e_se (di)       (di)))))		      
		      (mapcar #'(lambda (line)
				  (cons (first line) (expand-ba (second line) (third line))))
			      tmp)))

(defun identify-rcd-rel (ba-rel)
  "identifies which rcd-relations cover a block-algebra relation"
  (let ((rcd-rel ())
	(searching? t))
    (loop while searching? do
	 (let ((rcd (find-if #'(lambda (rcd)
				 (intersection (cdr rcd) ba-rel))
			     *rcd*)))
	   (if rcd
	       (progn (push (car rcd) rcd-rel)
		      (setq ba-rel (set-difference ba-rel (cdr rcd))))
	       (setq searching? nil))))
    (unless (null ba-rel)
      (sparq:print-out "dat mookt wi gar nich, da blevt wat üver: ~a~%" ba-rel))
    (assert (null ba-rel))
    rcd-rel))

(defun translate-ba->rcd (ba csp rcd)
  (let* ((relrep-ba  (calculi:calculus-relation-representation ba))
	 (relrep-rcd (calculi:calculus-relation-representation rcd))
	 (ba-decoder (relations:relation-representation-decoder relrep-ba))
	 (rcd-encoder (relations:relation-representation-encoder relrep-rcd))
	 (rcd-constraints (mapcar #'(lambda (c)
				      (csp:make-constraint (csp:constraint-object-1 c)
							   (ofunc:ofuncall rcd-encoder relrep-rcd (identify-rcd-rel (read-from-string (ofunc:ofuncall ba-decoder relrep-ba (csp:constraint-relation c)))))
							   (csp:constraint-object-2 c)))
				  (csp:constraints csp))))
    (make-instance 'csp:constraint-network
		   :calculus    rcd
		   :objects     (csp:constraint-network-objects csp)
		   :constraints rcd-constraints)))

(defun identify-ba-rel (rcd-rel)
  (loop for r in rcd-rel appending (cdr (assoc r *rcd*))))

(defun translate-rcd->ba (rcd csp ba)
  (let* ((relrep-ba  (calculi:calculus-relation-representation ba))
	 (relrep-rcd (calculi:calculus-relation-representation rcd))
	 (ba-encoder (relations:relation-representation-encoder relrep-ba))
	 (rcd-decoder (relations:relation-representation-decoder relrep-rcd))
	 (ba-constraints (mapcar #'(lambda (c)
				      (csp:make-constraint (csp:constraint-object-1 c)
							   (ofunc:ofuncall ba-encoder relrep-ba (identify-ba-rel (read-from-string (ofunc:ofuncall rcd-decoder relrep-rcd (csp:constraint-relation c)))))
							   (csp:constraint-object-2 c)))
				  (csp:constraints csp))))
    (make-instance 'csp:constraint-network
		   :calculus    ba
		   :objects     (csp:constraint-network-objects csp)
		   :constraints ba-constraints)))

#|
;; some code that allows the RCD calculus definition to be obtained by a translation
;; from the block-algebra

(defun compute-rcd (ba)
  "generates rcd calculus description"
  (assert (not (some #'(lambda (rcd1) ; block-algebra relations obtained by translation must not overlap
			 (some #'(lambda (rcd2)
				   (not (null (intersection (cdr rcd1) (cdr rcd2)))))
			       (remove rcd1 *rcd*)))
		     *rcd*)))
  (let* ((relrep (calculi:calculus-relation-representation ba))
	 (encoder (relations:relation-representation-encoder relrep))
	 (decoder (relations:relation-representation-decoder relrep))
	 (rcd.ba-enc (mapcar #'(lambda (rcd)
				 (cons (car rcd) (ofunc:ofuncall encoder relrep (cdr rcd))))
			     *rcd*)))
    
    (sparq:print-out "(def-calculus \"RCD\"~%  :arity :binary~%  :parametric? nil~%  :consistency :unknown~%  :identity-relation E~%")
    (sparq:print-out "  :base-relations ~a~%" (mapcar #'car rcd.ba-enc))
    (sparq:print-out "  :converse-operation ~a~%" (mapcar #'(lambda (rcd)
							      (list (car rcd)
								    (identify-rcd-rel (read-from-string (ofunc:ofuncall decoder relrep (calculi:converse ba (cdr rcd)))))))
							  rcd.ba-enc))
    (sparq:print-out "  :composition-operation ~a)~%" (loop for rcd1 in rcd.ba-enc appending
							   (loop for rcd2 in rcd.ba-enc collecting
								(list (car rcd1) 
								      (car rcd2) 
								      (identify-rcd-rel (read-from-string (ofunc:ofuncall decoder relrep (calculi:composition ba (cdr rcd1) (cdr rcd2)))))))))))

|#