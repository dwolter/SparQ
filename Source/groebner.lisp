;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; groebner.lisp
;;;
;;; computing the groebner basis of multivirate polynomials
;;; used in SparQ's algebraic reasoning for deciding satisfiability
;;; This file provides the function "groebner-basis" that computes the Groebner basis
;;;

;; Change history (most recent first):
;; 2007-06-14 DW  started file from source in polynomials.lisp

(in-package :poly)

;;; 
;;; critical pairs
;;;
;;; data structure to represent a pair of polynomials alongside with
;;; their 'sugar' score (a fixnum - we use -1 to denote that no sugar
;;; score has been determined so far

;; total degree of a polynomial, e.g., x^3 + x^2y^2 --> 4
(defun total-degree (p)
  "Computes total degree of a poly"
  (declare (optimize (speed 3) (safety 0)))
  (reduce #'(lambda (accu term)
	      (let ((d (reduce #'(lambda (accu monom)
				   (+ (the fixnum accu) (the fixnum (second monom))))
			       (second term)
			       :initial-value 0)))
		(declare (type fixnum d))
		(if (< (the fixnum accu) d)
		    d
		    accu)))
	  p
	  :initial-value 0))


;; computes degree as used by the sugar selection strategy 
(defun sugar-degree (f g)
  "Computes the Sugar of a pair of polys, maxdegree being total degree of ideal"
  (declare (optimize (speed 3) (safety 0)))
  (let* ((lmf    (second (car f))) ; lead monomial of f
	 (td-lmf (apply #'+ (mapcar #'second lmf)))
	 (lmg    (second (car g))) ; lead monomial of g
	 (td-lmg (apply #'+ (mapcar #'second lmg))) ; total degree of 'homogenisiertem' lmg
	 (lcm    (monomial-lcm lmf lmg)) ; least common multiple from leading monomials
	 (td-lcm (apply #'+ (mapcar #'second lcm))))
    (declare (type fixnum td-lmf td-lcm td-lmg))
    (max (+ (- td-lcm td-lmf) (the fixnum (total-degree f)))
	 (+ (- td-lcm td-lmg) (the fixnum (total-degree g))))))


(defstruct (critical-pair (:constructor make-pair (f g)))
  f
  g
  (sugar -1))

(defun sugar-score (cp)
  (declare (optimize (speed 3) (safety 0)))
  (if (eq (the fixnum (critical-pair-sugar cp)) -1)
      (setf (critical-pair-sugar cp) (sugar-degree (critical-pair-f cp) (critical-pair-g cp)))
      (critical-pair-sugar cp)))

(declaim (inline pair-f))
(defun pair-f (cp)
  (critical-pair-f cp))

(defun (setf pair-f) (cp f)
  (setf (critical-pair-f cp) f
	(critical-pair-sugar cp) -1))

(declaim (inline pair-g))
(defun pair-g (cp)
  (critical-pair-g cp))

(defun (setf pair-g) (cp g)
  (setf (critical-pair-g cp) g
	(critical-pair-sugar cp) -1))





;;;
;;; utility functions
;;;

#|
(defun same-length? (lst1 lst2)
  "return T iff two lists have same length"
  (declare (type list lst1 lst2)
	   (optimize (speed 3)))
  (cond ((and lst1 lst2) (same-length? (cdr lst1) (cdr lst2)))
	((not (or lst1 lst2)) t)
	(t nil)))
|#

(defun least (list fn)
  "returns least obj in list with respect to (fn obj)"
  (declare (optimize (speed 3)))
  (let* ((least (car list))
	 (minv  (funcall fn least)))

    (dolist (o (cdr list))
      (let ((v (funcall fn o)))
	(when (< v minv)
	  (setq minv v
		least o))))
    (values least minv)))


(defun ninsert (obj lst <)
  "inserts obj into lst according to orderin <"
  (cond ((null lst) (list obj))
	((funcall < obj (car lst)) (cons obj lst))
	(t (let ((iter lst))
	     (loop while t do
		  (when (or (null (cdr iter))
			    (funcall < obj (cadr iter)))
		     (setf (cdr iter) (cons obj (cdr iter)))
		     (return-from ninsert lst))
		  (pop iter))))))

(defun insert (obj lst <)
  "inserts obj into lst according to orderin <"
  (if (or (null lst)
          (funcall < obj (car lst)))
    (cons obj lst)
    (cons (car lst) (insert obj (cdr lst) <))))

(defun reduce-all (r p g pairs test-hook time-out)
  (loop while r do
       (when (< time-out (get-internal-real-time))
	 (throw 'stop-buchberger :time-out))
       (let ((h (normalize (reduce-by-list (pop r) (append g p)))))
	 (when h
	   (let ((hres (funcall test-hook h)))
	     (when hres
	       (if (and (consp hres)
			(eq (car hres) :bind))
		   (let ((bds (cdr hres)))
		     (setq h (apply-bindings-to-poly bds h)
			   r (apply-bindings bds r)
			   g (apply-bindings bds g)
			   p (apply-bindings bds p))
		     (mapc #'(lambda (pair)
			       (setf (pair-f pair) (apply-bindings-to-poly bds (car pair))
				     (pair-g pair) (apply-bindings-to-poly bds (cdr pair))))
			   pairs))
		   (throw 'stop-buchberger (cons :hook (list h))))))
	   (let ((g0 ())
		 (p0 ())
		 (lph (cadar h)))
	     (setq g (delete-if #'(lambda (g)
				    (if (or (monomial> (cadar g) lph)
					    (equal (cadar g) lph))
					(push g g0)))
				g)
		   p (delete-if #'(lambda (p)
				    (if (or (monomial> (cadar p) lph)
					    (equal (cadar p) lph))
					(push p p0)))
				p)
		   r (nconc g0 p0 r)
		   pairs (delete-if #'(lambda (pair)
					(or (find (pair-f pair) g0 :test #'equal)
					    (find (pair-g pair) g0 :test #'equal)))
				    pairs)
		   p (cons h p))))))
  (values r p g pairs))

(defun new-basis (p g pairs test-hook time-out)
  (setq g (nconc g p))
  (dolist (f1 g)
    (dolist (f2 p)
      (unless (or (eq f1 f2)	
		  (equal (monomial-multiply (cadar f1) (cadar f2)) (monomial-lcm (cadar f1) (cadar f2)))
		  )
	(push (make-pair f1 f2) pairs))))
  (let ((k ()))
    (dolist (h g)
      (let ((hh (reduce-by-list h (remove h g))))
	(when (and hh (> 0 (caar hh)))
	  (setq hh (scalar-multiply (/ 1 (caar hh)) hh)))
	(push hh k)))
    (setq g k))
  (values g pairs))


#|
(defun new-basis (p g pairs test-hook time-out)
  "updates the basis 'g' with new polys 'p' and updates pairs 'pairs'"
  (declare (ignore test-hook time-out))  
  (setq g (nconc g p))
  (let ((k ())
	(pp (car p)) ;; pp marks beginning of "new" polys in basis g after concatenation
	(ppred ()))  ;; ppred is the reduced pp (set below in reduction loop)

    ;; reduce basis polys
    (dolist (h g)      
      (let ((hh (reduce-by-list h (remove h g))))
	(when hh	  
	  (when (> 0 (caar hh)) ; normalize poly
	    (setq hh (scalar-multiply (/ 1 (caar hh)) hh)))
	  (when (eq h pp) ; remember beginning of "new" polys for pair generation below
	    (setq ppred hh))
	  (push hh k))))
    (setq g k)
    
    (assert (member ppred g))
    (dolist (f1 g)
      (dolist (f2 (member ppred g))
	(unless (or (eq f1 f2)	
		  ;(equal (monomial-multiply (cadar f1) (cadar f2)) (monomial-lcm (cadar f1) (cadar f2))) 
		    )
	  (push (cons f1 f2) pairs)))))
  
  (values g pairs))
|#

;; Computes the S-polynomial of p and q reduced by the set g
;; If computation is not required wrt. forthcoming calls to red-spoly (-> listed in the list pairs),
;; computation is skipped and nil is returned which is the same if the reduction would yield 0
(defun red-spoly (p q g pairs)
  (declare (ignore pairs))
  (let* ((HM-p (cadar p))
         (HM-q (cadar q))
         (lcm-hm (monomial-lcm HM-p HM-q)))
    (if (or (equal lcm-hm (monomial-multiply HM-p HM-q))         ; Buchberger's criteria
	    ;(criterion p q lcm-hm g pairs)
	    )
      NIL
      (let ((h (reduce-by-list (s-polynomial-lcm  lcm-hm p q) g)))
	(when h
	  (let ((lc (caar h))) ; normalize h
	    (if (= 1 lc)
		h
		(scalar-multiply (/ 1 lc) h))))))))

#|
;;Das zweite Kriterium in dem Buchbergeralgorithmus
;;p und q sind zwei polynome, pairs ist eine menge B = {(f_i,f_j)|1<=i<j<s}
;;fuer alle Polynome der Ideal F=(f_1,...,f_s)
(defun criterion (f1 f2 lcm-hm g pairs)
  (some #'(lambda (p)
	    (and (not (equal p f1))
		 (not (equal p f2))
		 (not (monomial> (cadar p) lcm-hm))
		 
		 (not (some #'(lambda (pair)
				(or (and (equal (car pair) p)
					 (equal (cdr pair) f2))
				    (and (equal (car pair) f1)
					 (equal (cdr pair) p))))
			    pairs))))
	g))
|#

;; Computing the reduced Groebner basis
;; INPUT:
;; polys      list of polynomials for which basis shall be computed
;; test-hook  test function that is called during computation to see whether
;;            we really need to continue or the 'intermediate' basis already
;;            provides sufficient information. Whenever test-hook returns T
;;            groebner exists with 'intermediate' basis
;; time-out   time when to exit groebner computation 
;;
;; OUTPUT: 2 return values
;; 1st: groebner basis
;; 2nd: t, if exited abnormally (either test-hook returned t somewhere or we ran out of time)
;;
(defun groebner-basis (polys test-hook time-out)
  "Computes the Groebner basis of polys"
  (let ((gbasis (catch 'stop-buchberger
		  (multiple-value-bind (r p g pairs) (reduce-all (mapcar #'normalize polys) () () () test-hook time-out)
		    (multiple-value-setq (g pairs) (new-basis p g pairs test-hook time-out))
		    (loop while pairs do
			 (when (> (get-internal-real-time) time-out)
			   (throw 'stop-buchberger :time-out))			 
			 (let* ((cp (least pairs #'sugar-score))
				(f1 (pair-f cp))
				(f2 (pair-g cp)))
			   (setq pairs (delete cp pairs))
			   (let ((h (red-spoly f1 f2 g pairs)))
			     (when h
			       ;; test new poly in solver (via test-hook)
			       (let ((hook-res (funcall test-hook h)))
				 (when hook-res
				   (if (and (listp hook-res)
					    (eq (car hook-res) :bind))
				       (let ((bds (cdr hook-res)))
					 (format t "~%in gb binde: ~a" bds) (break)
					 (setq h (apply-bindings-to-poly bds h)
					       g (apply-bindings bds g))
					 (mapc #'(lambda (p)
						   (setf (car p) (apply-bindings-to-poly bds (car p))
							 (cdr p) (apply-bindings-to-poly bds (cdr p))))
					       pairs))
				       (throw 'stop-buchberger (cons :hook (list h))))))
			       (let ((g0 ())
				     (lph (cadar h)))
				 (setq g (delete-if #'(lambda (g) ; split g into g, g0 where g0 lists polys divided by h
							(if (or (monomial> (cadar g) lph)
								(equal (cadar g) lph))
							    (push g g0)))
						    g))
				 (setq r g0
				       p (list h)
				       pairs (delete-if #'(lambda (pair)
							    (or (find (pair-f pair) g0 :test #'equal)
								(find (pair-g pair) g0 :test #'equal)))
							pairs)))
			       (multiple-value-setq (r p g pairs) (reduce-all r p g pairs test-hook time-out))
			       (multiple-value-setq (g pairs) (new-basis p g pairs test-hook time-out))))))
		    g))))
    (cond ((eq gbasis :time-out) 
	   ;;(format t "G") 
	   (values nil :time-out))
	  ((and (consp gbasis)
		(eq (car gbasis) :hook))
	   (values (cdr gbasis) :hook))
	  (t (values gbasis nil)))))


#|
(defun test-basis (b)
  (format t "~%~a polynome in basis~%" (length b))
  (dolist (p1 b)
    (dolist (p2 b)
      (unless (eq p1 p2)
	(let ((r (reduce-by-list  (s-polynomial p1 p2) b)))
	  (when r
	    (print r))))))
  (write-line "Wenn keine Polynome dastehen, dann alles OK."))
|#


(export '(groebner-basis))


#|

(setf tbasis (mapcar #'poly::make-poly '(((1 ((5 1))) (-1 ((13 2))))
					 ((1 ((13 2)(25 2))) (-1 ()))
					 ((1 ((6 1))) (-1 ((14 2))))
					 ((1 ((14 2) (26 2))) (-1 ()))
					 ((-1 ((6 1))) (-1 ((15 2))) (1 ()))
					 ((1 ((15 2) (27 2))) (-1 ()))
					 ((-1 ((5 1) (8 1))) (1 ((5 1))) (1 ((6 1)(7 1))) (-1 ((7 1))) (-1 ((17 2))))
					 ((1 ((17 2) (29 2))) (-1 ()))
					 ((1 ((5 1) (7 1))) (1 ((6 1) (8 1))) (-1 ((6 1))) (-1 ((8 1))) (+1 ()))
					 ((-1 ((7 1))) (-1 ((21 2))))
					 ((1 ((21 2) (32 2))) (-1 ()))
					 ((1 ((8 1))) (-1 ((22 2))))
					 ((1 ((22 2) (33 2))) (-1 ()))
					 ((-1 ((8 1))) (-1 ((23 2))) (1 ()))
					 ((1 ((23 2) (34 2))) (-1 ())))))

(setf tbasis (mapcar #'poly::make-poly '(((-1 ((13 2) (25 2))) (1 ()))
					 ((-1 ((14 2) (26 2))) (1 ()))
					 ((-1 ((14 2))) (-1 ((15 2))) (1 ()))
					 ((-1 ((15 2) (27 2))) (1 ()))
					 ((-1 ((13 2) (22 2))) (1 ((13 2))) (-1 ((14 2) (21 2))) (-1 ((14 2) (21 2))) (-1 ((17 2))) (1 ((21 2))))
					 ((-1 ((17 2) (29 2))) (1 ()))
					 ((-1 ((13 2) (21 2))) (1 ((14 2) (22 2))) (-1 ((14 2))) (-1 ((22 2))) (1 ()))
					 ((-1 ((21 2) (32 2))) (1 ()))
					 ((-1 ((22 2) (33 2))) (1 ()))
					 ((-1 ((22 2))) (-1 ((23 2))) (1 ()))
					 ((-1 ((23 2) (34 2))) (1 ())))))


(setf tbasis (mapcar #'poly::make-poly '(((1 ((1 3) (2 1) (3 1))) (-1 ((1 1) (3 2))))
					 ((1 ((1 1) (2 2) (3 1))) (-1 ((1 1) (2 1) (3 1))))
					 ((1 ((1 2) (2 2))) (-1 ((3 2)))))))


|#
