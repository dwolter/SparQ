;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; poly-solver.lisp
;;;
;;; methods for deciding emptyness of an ideal over multivirate polynomials
;;; used in SparQ's algebraic reasoning for deciding satisfiability
;;;


;; Change history (most recent first):
;; 2009-10-29 DW  added search-model
;; 2009-02-09 DW  moved bindings out to polynomials.lisp
;; 2008-05-16 DW  bug fix: detecting illegal bindings when tracing alternatives
;; 2008-05-14 DW  updated solver to exploit constraints for sensibly
;; 2007-10-24 DW  added substitution capabilities to reasoner and exit condition
;; 2007-10-22 DW  moved pattern definition macro and helper functions (from old source) into here
;; 2007-06-14 DW  started file from old source

(in-package :poly)

;;(defvar *now* 0)

;; Compute time limit:
(defparameter *timeout* 10000
  "time out for algebraic reasoning in ms, 10000 equals 10 seconds") 

(defun bindings-admissible? (bindings slack-variables)
  "Predicate returning t iff bindings are admissible, i.e. no slack-variables are forced to 0."
  (not (find-if #'(lambda (slack)
                    (find-if #'(lambda (binding)
                                 (and (eq (car binding) slack)
                                      (= (cdr binding) 0)))
                             bindings))
                slack-variables)))


;;; Patterns for equation solver & pattern definition macros
;;;

(defvar *patterns* nil)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
    (if it ,then-form ,else-form)))

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
      (if (or it ,win) ,then ,else))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
	  (if (or ,val ,win)
	      (let ((it ,val)) 
                 ,@(cdr cl1))
	      (acond2 ,@(cdr clauses)))))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar #'(lambda (v)
                           `(,v (binding ',v it)))
                       (vars-in then #'atom))
           ,then)
         ,else))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun match (x y &optional binds)
  (acond2
    ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
    ((binding x binds) (match it y binds))
    ((binding y binds) (match x it binds))
    ((var? x) (values (cons (cons x y) binds) t))
    ((var? y) (values (cons (cons y x) binds) t))
    ((and (consp x) (consp y) (match (car x) (car y) binds))
     (match (cdr x) (cdr y) it))
    (t (values nil nil))))


(defun reply  (&key applicable? non-fullfillable? new-bindings alternatives constraints)
  "Result of pattern application; specify deduced implications"
  (values applicable? non-fullfillable? new-bindings alternatives constraints))

(defmacro defpattern (name pattern &body body)
  "Macro to define patterns to be used when solving equations."
  (let ((poly (gensym))
	(tried-constraints (gensym)))
    `(length (push (cons #'(lambda (,poly ,tried-constraints)
			     (flet ((filter-constraints (cs)
				      (set-difference cs ,tried-constraints :test #'equal)))
			       (if-match ,pattern ,poly
					 (progn ,@body))))
		    ,name)
	      *patterns*))))

(defun split-list (list split-fn)
  (let ((lst1 ())
	(lst2 ()))
    (dolist (x list)
      (if (funcall split-fn x)
	  (push x lst1)
	  (push x lst2)))
    (values (nreverse lst1) (nreverse lst2))))

;;;
;;; SUBSTITUTION STUFF
;;;

;; Return a list of substitutions (monom . poly)
(defun possible-substitutions (poly)
  (let ((single-monom-terms (remove-if #'(lambda (term) (cdr (second term))) poly))
	(substs ()))
    (dolist (term single-monom-terms)
      (let* ((monom (second term))
	     (index (first monom))) ; der variablenindex des monoms
	;; es sei denn die variable ist nicht einzelstehend
	(unless (find-if #'(lambda (term2) (and (not (eq term term2))
						(find index (second term2) :key #'first)))
			 poly)
	  (let ((coef (first term)))
	    (push (cons (first monom) (scalar-multiply (/ -1 coef) (remove term poly))) substs)))))
  substs))

;; returns the poly resulting when substitution is applied
;; if substitution does not eliminate the substituted monom
;; completely, nil is returned instead, i.e. the substitution is 
;; canceled
(defun substitute-monom (poly substitution)
  (declare (optimize (speed 3)))
  (let* ((monom2rpl (car substitution))
	 (var2rpl (car monom2rpl))
	 (var-exp (second monom2rpl))
	 (rpl (cdr substitution))
	 (did-change? nil)
	 (subst-poly ()))
    (catch 'wont-work
      (dolist (term poly)
	;;(format t "~&~% term = ~a" term)
	(let* ((monom (second term))
	       (var (find var2rpl monom :key #'car)))
	  (if (or (and (null monom2rpl) ; if falle von nil (d.h. ersetzen von einer konstante) klappt find nicht
		       (null monom))
		  var)
		(if (null monom2rpl)
		    ;; const ersetzen
		    (setq subst-poly (add subst-poly (scalar-multiply (first term) rpl)))
		    ;; variable in monom ersetzen
		    (multiple-value-bind (e rest) (ceiling (second var) var-exp)
		      (setq did-change? t)
		      (unless (= 0 rest) ; exponenten teilerfremd wollen wir nicht substituieren
			(setq subst-poly ()
			      did-change? nil)
			;;(print "wont work!")
			(throw 'wont-work nil))
		      (let ((mpoly rpl)) ; mpoly ist das subst-poly ^ e
			(do ((i 1 (+ i 1)))
			    ((>= i e))
			  (setq mpoly (multiply mpoly rpl)))
			;;(format t "~&mpoly = ~a" mpoly)
			;;(format t "~&zielpoly = ~a" (list (list (first term) (remove var monom))))
			(setq subst-poly (add subst-poly (multiply mpoly (list (list (first term) (remove var monom)))))))))
		(setq subst-poly (add subst-poly (list term)))))))

;;    (unless (or (not did-change?)
;;		(equal subst-poly (make-poly (copy-tree subst-poly))))
;;      (format t "~2%subst-poly = ~a~%make-poly  = ~a~%" subst-poly (make-poly (copy-tree subst-poly)))
;;					(break)
;;      )
    (if did-change?
	(values subst-poly t))))

(defun worth-adding? (poly poly1 poly2 gbase)
  (declare (ignore gbase))
  (let ((lpoly (length poly)))
    (and poly
	 (< lpoly 40)
	 (<= lpoly (+ (length poly1) (length poly2))))))

;; main entry point for subtitution
;; if required to apply this to lengthy ideal bases (> 500) we need to optimize
;; or the double dolist will blow up...
(defun apply-substitution (stream gbase indention timeout)
  "extends ideal base gbase with polynomials obtained by variable substitution -- computationally intensive!"
  (let ((new-polys ()))
    (labels ((make-substitution (subst orig-poly target-poly)
	       (mapc #'(lambda (subst)
			 (let ((p (normalize (substitute-monom target-poly subst))))
			   (when (worth-adding? p orig-poly target-poly gbase)
			     (pushnew p new-polys :test #'equal))))
		     subst)))
      (let ((psubst (mapcar #'possible-substitutions gbase)))
	(do ((p1s gbase (cdr p1s))
	     (p1subs psubst (cdr p1subs)))
	    ((null p1s))
	  (let* ((p1 (car p1s))
		 (p1sub (car p1subs))
		 (p1-len- (max 0 (1- (length p1)))))
	    (do ((p2s (cdr p1s) (cdr p2s))
		 (p2subs (cdr p1subs) (cdr p2subs)))
		((null p2s))
	      (when (< timeout (get-internal-real-time))
		(throw 'exit-proof :timeout))
	      
	      (let* ((p2 (car p2s))
		     (ltest (nthcdr p1-len- p2)))
		(if ltest  
		    (if (cdr ltest) ; p2 is longer, so we substitute only into p2
			(make-substitution p1sub p1 p2)
			(progn ; length is same: double substitution
			  (make-substitution p1sub p1 p2)
			  (make-substitution (car p2subs) p2 p1)))
		    (make-substitution (car p2subs) p2 p1))))))))

    ;; new-polys need some clean-up: normalization and removal of duplicates
    (setf new-polys (set-difference new-polys gbase :test #'equal))

    (when (and *debug* new-polys)
      (format stream "~&;; ~aadding new polynomials to basis after substitution:" indention)
      (let* ((count 0) 
	     (pad-str (format nil "~~~d<N~~d~~>" (ceiling (+ 1.1 (log (length new-polys) 10))))))
	(dolist (p new-polys)
	  (format stream "~%;; ~a(~@? :) " indention pad-str (incf count) (ceiling (+ 0.1 (log (length new-polys) 10))))
	  (poly:print-poly p stream))))
    (values new-polys (append new-polys gbase))))

#|
(defun slack-factorial (poly slacks)
  (let ((var-candidates (remove-if-not #'(lambda (v)
					   (find (first v) slacks))
				       (cadar poly))))
    (dolist (term (cdr poly))
      (let ((monom (second term)))
	(setq var-candidates (reduce #'(lambda (accu v)
					 (let ((v-in-monom (find (first v) monom :key #'first)))
					   (format t "~%v=~a    v-in-monom=~a" v v-in-monom)
					   (if v-in-monom
					       (cons (list (first v) (min (second v) (second v-in-monom))) accu)
					       accu)))
				     var-candidates
				     :initial-value nil))
	(when (null var-candidates)
	  (return-from slack-factorial poly))))
    var-candidates))
|#

#|
(defun derive-new-polys (stream gbase indention timeout)
  (let ((new-polys ()))
    (do ((p1s gbase (cdr p1s)))
	((null p1s))
      (let* ((p1 (car p1s))
	     (l1 (length p1)))
	(dolist (p2 (cdr p1s))
	  (unless (eq p1 p2)
	    (let ((dif (add p1 p2))
		  (l2 (length p2)))
	      (if (< (length dif) (+ l1 l2))
		  (push dif new-polys)
		  (let ((add (subtract p1 p2)))
		    (if (< (length add) (+ l1 l2))
			(push dif new-polys)))))))))
    (when (and *debug* new-polys)
      (format t "~%;; ~aadding new polys to basis:" indention)
      (let* ((count 0) 
	     (pad-str (format nil "~~~d<N~~d~~>" (ceiling (+ 1.1 (log (length new-polys) 10))))))
	(dolist (p new-polys)
	  (format stream "~%;; ~a(~@? :) " indention pad-str (incf count) (ceiling (+ 0.1 (log (length new-polys) 10))))
	  (poly:print-poly p stream))))
    (values new-polys (nconc new-polys gbase))))
|#

(defun find-var-substitution (stream indention gbase &optional dont-substitute)
  "scans our ideal base for variables we can resolve"
  (let ((sucess? nil)        ;; flag das t wird, wenn (mindestens) eine Ersetzung erfolgreich war
	(exhausted? nil)     ;; flag das t wird, wenn keine Ersetzung mehr gefunden wurde
	(vars-cancelled dont-substitute))  ;; Liste der Variablen, die schon eliminiert worden sind

    (loop until exhausted? do
	 (setq exhausted? t)
	 ;; iteriere ueber alle polynome...
	 (format t "~5%***************************~%*** LAENGE GBASE: ~a~%***************************" (length gbase))
	 (dolist (poly gbase)
	   ;; ...und all ihre teilterme die nur ein einzelnes monom haben
	   (format t "~&INSPIZIERE POLY #~a (~a)" (position poly gbase) (eq (first gbase) (second gbase)))
	   (when (dolist (term (remove-if #'(lambda (single-monomial-term)
					      (let ((vidx (first (first (second single-monomial-term)))))
						(some #'(lambda (term)
							  (and (not (eq term single-monomial-term))
							       (find vidx (second term) :key #'first)))
						      poly)))
					  (remove-if #'(lambda (term)
							 (or (null (second term))
							     (cdr (second term))
							     (find (first (second term)) vars-cancelled :test #'equal)))
						     poly)))
		   (format t "~3%Term ~a ist einzeln in: " term)
		   (print-poly poly)
		   ;; Ersetzung durchfuehren
		   (let ((vexp (cadar (second term)))
			 (vidx (caar (second term))))
		     ;; testen, ob eine Substitution in der gesamten Basis moeglich ist ohne
		     ;; das gebrochen-rationale Exponenten entstehen
		     (when (every #'(lambda (p2)
				      (every #'(lambda (term)
						 (every #'(lambda (monom)
							    (= 0 (mod (second monom) vexp)))
							(remove-if-not #'(lambda (m)
									   (eq (first m) vidx))
								       (second term))))
					     p2))
				  gbase)
		       ;; we have a possible monom to substitute
		       (setq sucess? t
			     exhausted? nil)
		       ;; now look for other polys in which we can isolate the same monomial
		       ;; we will then recombine the alternative polys in all possible ways
		       ;; (otherwise we won't be able to detect the relation between the polynomials
		       ;; anymore when we have substituted the single monomial by a - possibly- ugly term
		       (let ((alts (remove-if-not #'(lambda (p2)
						      (and (find-if #'(lambda (term2)
									(and (null (cdr (second term2)))
									     (eq vidx (caar (second term2)))
									     (eq vexp (cadar (second term2))))) ; here one could also handle other exponents
								    p2)
							   (not (eq p2 poly))))
						  gbase))
			     (new-polys ()))
			 (format t "~%WE HAVE ~a OCCURENCES OF x[~a]^~a!" (length alts) vidx vexp)
			 ;; generate all possible combinations of the 'alts' polys:
			 (do ((ps (cons poly alts) (cdr ps)))
			     ((null ps))
			   (let* ((p1 (car ps))
				  (t1 (find-if #'(lambda (term2)
						   (equal (second term2) (second term)))
					       p1))
				  (p1-rpl (scalar-multiply (/ -1 (first t1)) (remove t1 p1))))
			     (dolist (p2 (cdr ps))
			       ;; hier p1 und p2 nach x[vidx]^vexp aufloesen und gleichsetzen
			       (let* ((t2 (find-if #'(lambda (term2)
						       (equal (second term2) (second term)))
						   p2))
				      (p2-rpl (scalar-multiply (/ 1 (first t2)) (remove t2 p2)))) ;; NB not -1 here as we're going to add p1-rpl and p2-rpl!
				 (assert (and p1-rpl p2-rpl))
				 (pushnew (add p1-rpl p2-rpl) new-polys :test #'equal)))))
			 (setq new-polys (delete nil new-polys))
			 (format t "~%WE HAVE ~a NEW POLYS!" (length new-polys))
			 (assert (not (eq (first gbase) (second gbase))))
			 (pprint new-polys)
			 (setq gbase (nconc new-polys (delete poly gbase))) ; put new polys to gbase since there may be multiple occurences the monomial we're about to substitute in here

			 ;; NB: EIGENTLICH SOLLTEN WIR HIER *ALLE* MOEGLICHEN ERSETZUNGEN GEMAESS DER ALTERNATIVLISTE ALTS DURCHFUEHREN!!!
			 ;; substitute all occurences:
			 (assert (and t (not (eq (first gbase) (second gbase)))))
			 (let ((substitutions (mapcar #'(lambda (res-poly)
							  (let* ((res-term (find-if #'(lambda (tr)
											(equal (second tr) (second term)))
										    res-poly)))
							    (cons (car (second term)) (scalar-multiply (/ -1 (first res-term)) (remove res-term res-poly)))))
						      (cons poly alts))))
;;(cons (car (second term)) (scalar-multiply (/ -1 (first term)) (remove term poly)))))

			   (setq gbase (reduce #'(lambda (accu p)
						   (let ((tmp ()))
						     (dolist (s substitutions)
						       (multiple-value-bind (subst-poly changed?) (substitute-monom p s)
							 (when changed? (pushnew subst-poly tmp :test #'equal))))
						     (if tmp
							 (nconc tmp accu)
							 (if p
							     (cons p accu)
							     accu))))
					       gbase
					       :initial-value nil)))
			 (format t "~%1./2.: ~a / ~a, len=~a" (first gbase) (second gbase) (length gbase))
			 (assert (and t t(not (eq (first gbase) (second gbase)))))

			 ;; DEBUG:
			 (format t "~%ERSETZUNG DURCHGEFUEHRT: VARIABLE ~a IST WEG!" vidx )
			 ;;(let ((count 0)) 
			 ;;  (mapc #'(lambda (p)
			 ;;     (format t "~%(~2d)" (incf count))
			 ;;	     (print-poly p))
			 ;;	 gbase))
			 (assert (not (some #'(lambda (poly)
						(some #'(lambda (term)
							  (find vidx (second term) :key #'first))
						      poly))
					    gbase)))

			 (push vidx vars-cancelled)
					;(let ((count 0))
					; (dolist (p gbase)
					;  (format t "~%;; (~2d :) " (incf count))
					; (poly::print-poly p)))
			 
			 (return t)))))
	     (print "2. return")
	     (return t))
	   (print "war nicht!")))
    (when (and *debug* sucess?)
      (format stream "~&;; ~a~a variable substitution~:p applied:" indention (- (length vars-cancelled) (length dont-substitute)))     
      (let ((count 0)
	    (old (car dont-substitute)))
	(dolist (v vars-cancelled)
	  (when (eq v old) (return))
	  (format stream "x[~a] " indention (incf count) v ))))
    (values sucess? gbase vars-cancelled)))


;;;
;;; ORDER CONSTRAINTS
;;;

;; applies substitution constraint to basis if that 'appears to be promising'
;; 2 return values: updated basis, flag signalling that constraints have been applied successfully 
(defun handle-constraint (stream c basis indention more-slacks?)
  (cond ((eq (fourth c) '<->)
	 (destructuring-bind (f1 i1 e1 <-> f2 i2 e2) c
	   (declare (ignore <->))
	   (when (or (and more-slacks? (> i1 i2))
		     (and (not more-slacks?) (< i1 i2)))
	     (rotatef f1 f2)
	     (rotatef i1 i2)
	    (rotatef e1 e2))
	   
	   ;; Check applicability of substitution 
	   (let ((new-basis (catch 'wont-work 
			      (mapcar #'(lambda (orig-poly)
					  (let* ((p (poly:make-poly  ; <---- make-poly hier, da beim ersetzten irgendein fehler gemacht wird bzgl. der variablenordnung *DEBUG ME*
						     (reduce #'(lambda (poly term)
								 (add poly 
								      (list (let* ((i1? nil)
										   (i2? nil)
										   (f 1)
										   (monom (mapcar #'(lambda (m)
												      (if (eq (first m) i1)
													  (multiple-value-bind (e rest) (ceiling (second m) e1)
													    (when (/= rest 0)
													      (when *debug* 
														(destructuring-bind (c1 m1 e1 <-> c2 m2 e2) c
														  (declare (ignore <->))
														  (format stream "~&;; ~acan't apply substitution ~a*x[~a]^~a = ~a*x[~a]^~a to poly (would yield fraction in exponent, reverting...):~%;; ~a" indention c1 m1 e1 c2 m2 e2 indention))
														(print-poly orig-poly stream))
													      (throw 'wont-work nil))
													    (setq i1? t
														  f (* f (expt (/ f2 f1) e)))
													    (list i2 (* e2 e)))
													  (progn 
													    (when (eq (first m) i2)
													      (setq i2? t))
													    m)))
												  (second term))))
									      (list (* (first term) f)
										    (if (and i1? i2?) ;; clean up: we have two times the variable index i1 in the monom
											(multiple-value-bind (conflicts rest) (split-list monom #'(lambda (m) (eq i2 (first m))))
											  (cons (list i2 (+ (second (first conflicts)) (second (second conflicts)))) rest))
											monom))))))
							     orig-poly
							     :initial-value nil)) 
						   )
						 (lc (caar p)))
					    ;; normalize poly
					    (if (or (null p) (= 1 lc))
						p
						(scalar-multiply (/ 1 lc) p))))
				      basis))))
	     (when (and *debug* new-basis)
	       (format stream "~&;; ~aapplying substitution ~a*x[~d]^~d <- ~a*x[~d]^~d" indention f2 i2 e2 f1 i1 e1))
	     (if new-basis
		 (values new-basis t)
		 (values basis nil)))))
	;; Variablenbindung x_i^e = Zahl per Constraint, da da u.U.nicht auf alle Polynome andwendbar ist
	((eq (third c) '<-)
	 (destructuring-bind (var exp <- value) c
	   (declare (ignore <-))
	     (let ((new-basis (catch 'wont-work 
				(mapcar #'(lambda (orig-poly)
					    (let* ((p (poly:make-poly  ; <---- make-poly hier, da beim ersetzten irgendein fehler gemacht wird bzgl. der variablenordnung *DEBUG ME*
						       (reduce #'(lambda (poly term)
								   (add poly
									(if (null (second term))
									    (list term)
									    (let* ((coef (first term))
										   (monom (reduce #'(lambda (v monom)
												      (if (eq (first v) var)
													  ;; variable x_var ersetzen
													  (multiple-value-bind (e rest) (ceiling (second v) exp)
													    (when (/= 0 rest)
													      ;; Ersetzung geht nicht auf
													      (when *debug* 
														(format stream "~&;; ~acan't apply substitution x[~a]^~a = ~a to poly (would yield fraction in exponent, reverting...):~%;; ~a" indention var exp value indention)
														(print-poly orig-poly stream))
													      (throw 'wont-work nil))
													    (setq coef (* coef (expt value e)))
													    monom)
													  ;; andere variablen nicht aendern
													  (cons v monom)))
												  (second term)
												  :initial-value nil
												  :from-end t)))
									      (list (list coef monom))))))
							       orig-poly
							       :initial-value nil))))
					      ;; normalize poly
					      (if (or (null p) (= 1 (caar p)))
						  p
						  (scalar-multiply (/ 1 (caar p)) p))))
					basis))))
	       (when (and *debug* new-basis)
		 (format stream "~&;; ~aapplying substitution x[~d]^~d <- ~a" indention var exp value))
	       (if new-basis
		   (values new-basis t)
		   (values basis nil)))))
	;; other types of constraints are not implemented: just return basis
	(t basis)))

(defun report-bindings (stream bindings indention)
  (dolist (b bindings)
    (format stream "~&;;   ~ax[~d] <- ~a" indention (car b) (cdr b))))

(defun report-constraints (stream constraints indention)
  (let ((ctypes '((<-> . "substitution")
		  (<   . "order constraint")
		  (>   . "order constraint"))))		  
  (dolist (c constraints)
    (let ((type (cdr (assoc (fourth c) ctypes))))
      (if type
	  (format stream "~&;;   ~a~a: ~a*x[~d]^~a ~a ~a*x[~d]^~a" indention type
		  (first c) (second c) (third c) (fourth c)
		  (fifth c) (sixth c) (seventh c))
	  (format stream "~&;;   ~a~a" indention c))))
  (format stream "~%")))

;; prints out a diagram that makes it for me easier to see what's missing
(defun report-giveup (stream basis indention)
  (let ((vars (sort (remove-duplicates (mapcar #'first (apply #'append (mapcar #'second (apply #'append basis))))) #'<)))
    (format stream "~&;;~%;;        ~a" indention)
    (dolist (v vars)
      (format stream "~3d |" v))
    (let ((count 0))
      (dolist (p basis)
	(format stream "~%;;  ~a(~3d:)" indention (incf count))
	(let ((vassoc ()))
	  (if (some #'(lambda (term) (cdr (second term))) p) ; some monom with more than 1 var
	      (setq vassoc (mapcar #'(lambda (i)
				       (cons i '*))
				   (remove-duplicates (mapcar #'first (apply #'append (mapcar #'second p))))))
	      (dolist (term p) ;; terms only have monoms with at most 1 var
		(let ((i (caar (second term))))
		  (when i 
		    (push (cons i (if (< 0 (first term)) '+ '-)) vassoc)))))
	  (dolist (v vars)
	    (let ((val (cdr (assoc v vassoc))))
	      (if val 
		  (format stream "  ~a |" val)
		  (format stream "    |")))))))))


;; textual description of proof strand
(defun strand-description (strand)
  (let ((%strand (reverse strand)))
    (if (null %strand) "main" (format nil "~a~{-~a~}" (car %strand) (cdr %strand)))))

;; core method for deciding ideal emptyness
(defun test-basis (stream groebner-base slacks strand bindings original-base time-out)
  (declare (special *patterns*))
  (let ((working? t)
	(dont-substitute ()) ;; Liste der Vars die in find-var-substitution nicht substituiert werden sollen
	(loop-exit 4)
	(has-branched? nil) ; t iff proof has branched; in these cases we should silently give up (if we need to!)
	(reprint-base? nil)
	(emptiness-proven? nil)
	(satisfiability-proven? nil)
	(indention (coerce (make-list (ash (length strand) 1) :initial-element #\Space) 'string))
	(tried-constraints ())
	(gbase (remove-duplicates (delete nil groebner-base) :test #'equal)))

    (when *debug* 
      (format stream "~&;; ~a---- entering proof strand ~a ----~%" indention (strand-description strand))
      (when strand
	(setq reprint-base? t)))
    (let ((exit-reason (catch 'exit-proof
			 (loop while (and working? (< 0 (decf loop-exit))) do
			      ;;(format t "~%LOOP! #polys: ~a" (length gbase)) (finish-output)
			      ;;(format t "~%~a, timeout = ~a~%" (get-internal-real-time) time-out)
			      ;;(finish-output)
			      (when (null gbase)
				(setq satisfiability-proven? t)
				(throw 'exit-proof nil))
			      (when (> (get-internal-real-time) time-out)
				(throw 'exit-proof :timeout))
			      (setq working? nil)
			      (when reprint-base?
				(setq gbase (remove-duplicates (delete nil gbase) :test #'equal) ; when reprinting, we'll kill the zero polynomials first
				      reprint-base? nil)
				(when (null gbase) ; all polynomials reduced to zero
				  (setq satisfiability-proven? t)
				  (throw 'exit-proof nil))
				(when *debug*
				  (let ((count 0))
				    (format stream "~&;;~%;; ~athis is the updated set of polynomials:" indention)
				    (dolist (p gbase)
				      (format stream "~%;; ~a(~2d :) " indention (incf count))
				      (poly::print-poly p stream)))))
			      (dolist (pn *patterns*)
				(let ((pattern-fn (car pn))
				      (pattern-name (cdr pn)))
				  ;;(format stream "~&;; trying pattern ~a" pattern-name) (finish-output)
				  (do ((rest-polys gbase (cdr rest-polys)) ; durch basis iterieren
				       (pcount 1 (+ pcount 1)))
				      ((null rest-polys))
				    (let ((poly (car rest-polys)))
				      ;; GUARD TO TRACK DOWN BROKEN POLYGONS
				      ;;(unless (equal poly (poly:make-poly (copy-tree poly)))
				      ;;(format t "~2%poly kaputt: ~a" poly)
				      ;;(break))				   
				      ;;(format t "~%TRYING poly #~a (~a)" pcount (length poly)) (finish-output)
				      (multiple-value-bind (applicable? non-fullfillable? new-bindings alternatives constraints) 
					  (funcall pattern-fn poly tried-constraints)
					;;(when constraints
					  ;;(format t "~%;; gathering constraints: ~a" constraints))
					;;(format t "~%poly #~a  applicable? ~a non-fullfillable? ~a new-bindings ~a alternatives ~a constraints ~a (rest-polys: ~a)" pcount applicable? non-fullfillable? new-bindings alternatives constraints (not (null rest-polys))) (finish-output)
					(when applicable?
					  (when *debug* (format stream "~&;; ~aapplying pattern '~a' to polynomial #~d" indention pattern-name pcount))
					  (setq working? t))
					(when non-fullfillable?
					  (setq emptiness-proven? t)
					  (when *debug* 
					    (format stream "~&;; ~apolynomial cannot be satisfied:~%;;    " indention)
					    (poly::print-poly poly stream))
					  (throw 'exit-proof nil))
					(when new-bindings 
					  (when *debug*
					    (format stream "~&;; ~avariable bindings follow from polynomial:~%" indention)
					    (report-bindings stream new-bindings indention))
					  (setq reprint-base? t)
					  (unless (bindings-admissible? new-bindings slacks)
					    (when *debug* (format stream "~&;; ~aBINDINGS VIOLATE SIDE CONDITIONS!" indention))
					    (setq emptiness-proven? t)
					    (throw 'exit-proof nil))
					  (setq bindings (append new-bindings bindings))
					  (setq gbase (apply-bindings new-bindings gbase)))
					(when alternatives
					  (setq has-branched? t)
					  (when *debug* 
					    (format stream "~&;; ~a~d alternatives:" indention (length alternatives))
					    (let ((i 1))
					      (dolist (a alternatives)
						(format stream "~%;; ~aalternative #~d:~{~%;;   ~a~}" indention i (mapcar #'(lambda (binding)
															      (format nil "x[~d] <- ~a" (car binding) (cdr binding)))
															  a))
						(incf i))))
					  (let ((count 0))
					    (dolist (a alternatives)
					      (incf count)
					      (if (not (bindings-admissible? a slacks))
						  (when *debug* (format stream "~&;; ~aALTERNATIVE #~D VIOLATES SIDE CONDITIONS~%" indention count))
						  (let ((result (test-basis stream (apply-bindings a gbase) slacks (cons count strand) (append a bindings) original-base time-out)))
						    (if (eq result nil)
							(throw 'exit-proof :model) ; found model
							(when (eq result :?)
							  (when *debug* (format stream "~&;; ~aalternatives not exhaustive, moving on" indention))
							  (throw 'exit-proof nil))))))) ; dead end
					  (when *debug* (format stream "~&;; ~aexhaustive analysis of alternatives~%" indention))
					  (setq emptiness-proven? t)
					  (throw 'exit-proof nil))
					;; handle constraints like e.g., variable substitution due to term equality
					(when constraints
					  (unless (or new-bindings alternatives) ; unbehandelte constraints sind kein grund weiterzusuchen, bei erfolgreicher constraintbehandlung geht's weiter (s.u.)
					    (setq working? nil))
					  (when *debug* 
					    (format stream "~&;;   ~aderived constraints:~%" indention)
					    (report-constraints stream constraints indention))
					  (let ((gbase-changed? nil))
					    (dolist (c constraints)
					      (multiple-value-bind (new-gbase update?) (handle-constraint stream c gbase indention t)
						;;(format t "~%CONSTRAINT: update? = ~a" update?)
						(when update?
						  (setq working? t  ; bei erfolgreicher constraintbehandlung geht's weiter
							gbase-changed? t
							reprint-base? t
							gbase new-gbase))))
					    ;; after constraints are handled the gbase iteration needs to be re-synced (by some hacking)
					    (when gbase-changed?
					      (setq rest-polys nil) ;  (nthcdr (1- pcount) gbase))
					      ;(return)
					      ))))))))
			      (if working? ; nach substitutionen suchen wenn nichts anderes fruchtet, aber dann wird der durchlauf nicht gezaehlt...
				  (incf loop-exit)
				  (multiple-value-bind (changed? new-gbase substitutions) 
				      (and (null (nthcdr 100 gbase))
					   (apply-substitution stream gbase indention time-out))
				    ;;(find-var-substitution stream indention gbase dont-substitute)				    
				    ;;(when *debug*
				    ;;  (format stream "~&;; ~a~d new polynomial~:P obtained by term substitution" indention (- (length new-gbase) (length gbase))))
				    (when changed?
				      (setq working? t
					    gbase new-gbase
					    dont-substitute substitutions
					    reprint-base? t)))
				  ))
			 ;; we're stuck proofing unsatisfiability, maybe we find a model
			 (let ((model? (search-model stream indention original-base bindings slacks time-out)))
			   (when model?
			     (setq satisfiability-proven? t)
			     (throw 'exit-proof nil)))
			 (when *debug* 
			   (if (< 0 loop-exit)
			       (format stream "~&;; ~ano more applicable patterns found" indention)
			       (format stream "~&;; ~aaborting proof although patterns are still applicable - doesn't feel like continuing would help" indention))))))
      ;;(format t "~%strand: ~a reason = ~a   emptiness-proven?=~a satisfiability-proven?=~a" (strand-description strand) exit-reason emptiness-proven? satisfiability-proven?)
      ;(format t "~%exit-reason = ~a" exit-reason)
      ;;(when (eq exit-reason :timeout) (format stream "!"))
      (when *debug*
	(format stream "~&;;~%;;")
	(when (eq exit-reason :timeout)
	  (format stream "~%;;~%;; ABORTING REASONING: TIMEOUT~%")))
      (if emptiness-proven?
	  (progn
	    (when *debug* (format stream "~%;; ~aclosing proof strand ~a~%" indention (strand-description strand)))
	    t)
	  (if (or satisfiability-proven? (eq exit-reason :model))
	      nil
	      (let ((cnt 0))
		(when *debug*
		  (format stream "~&;; ~aaborting proof strand ~a, don't know how to go on..." indention (strand-description strand))
		  (unless has-branched? ; print system we're unable to decide only at leafs in the proof tree
		    (format stream "~%;; ~aCANNOT DECIDE SATISFIABILITY OF:" indention)	 
		    (setq gbase (delete nil gbase))
		    (dolist (p gbase)
		      (format stream "~%;; ~a(~2d:) " indention (incf cnt))
		      (poly:print-poly p stream))
		    (report-giveup stream gbase indention))
		  (format stream "~%;; ~a--- closing proof strand ~a ---" indention (strand-description strand)))
		:?))))))


(defun check-for-linear-equations (polys slacks)
  (every #'(lambda (poly)
	     (every #'(lambda (term)
			(or (null (second term)) ; constant
			    (and (null (cdr (second term))) ; or single variable
				 (or (find (first (first (second term))) slacks)
				     (eq 1 (second (first (second term))))))))
		    poly))
	 polys))

(defun linear-equation-solve (polys slacks)
  ;; Schritt 1, das lineare Gleichungssystem bauen
  (let* ((max-idx (reduce #'(lambda (accu poly) ; minimalen und maximalen variablenindex berechnen
			      (reduce #'(lambda (accu term)
					  (let ((idcs (set-difference (mapcar #'first (second term)) slacks)))
					    (if idcs
						(cons (min (car accu) (apply #'min idcs))
						      (max (cdr accu) (apply #'max idcs)))
						accu)))
				      poly
				      :initial-value accu))
			  polys
			  :initial-value (cons MOST-POSITIVE-FIXNUM MOST-NEGATIVE-FIXNUM)))
	 (numvars (+ 1 (- (cdr max-idx) (car max-idx)))) ; # der variablen
	 (offset (car max-idx))
	 (A (make-array (list (length polys) numvars) :initial-element 0))
	 (b (make-array (list (length polys)) :initial-element 0)))
    ;; Schritt 2, die Matrizen aufbauen
    (let ((row 0))
      (dolist (poly polys)
	(dolist (term poly)
	  (let ((monom (second term)))
	    (cond ((null monom) ; konstanter term
		   (decf (aref b row) (first term)))
		  ((and (null (cdr monom)) ; slack
			(find (first (first monom)) slacks))
		   (if (< 0 (first term)) ; f(...) < 0  =>  -f(...) > 0
		       (dotimes (i numvars)  ; NB: eigentlich müsste noch asserted werden, dass nach diesem Term nix mehr kommt! (sonst hätten wir nicht alles mit -1 invertiert)
			 (setf (aref a row i) (- (aref a row i))))))
		  (t (incf (aref a row (- (first (first monom)) offset)) (first term))))))
	(incf row)))
    (format t "~%A x <= b~%A = ~a~2%b = ~a" a b)
    ))

;; main entry point for testing an ideal for emptyness 
;; 
;; INPUT:
;; ideal-base    list of polynomials
;; slack-vars    list of variable indices 
;; stream        where to write the output
;; time-out      max. time allowed ins msec
;;
;; OUTPUT:
;; t / :? / :time-out nil  meaning "is empty", "don't know", :time-out, and "is not empty"

(defun test-emptyness (stream ideal-base slack-vars &key (time-out *timeout*) (initial-bindings '((1 . 0) (2 . 0) (3 . 0) (4 . 1))))
  (let* ((now (get-internal-real-time))
	 (then (+ now time-out))
	 (linear-eqns? (check-for-linear-equations ideal-base slack-vars))
	 (ibase (apply-bindings initial-bindings ideal-base)))
    (when *debug*
      (if (nthcdr 5 slack-vars)
	  (format stream "~&;; slack variables with side condition x_i /= 0: x[~a], x[~a], ..., x[~a]" (first slack-vars) (second slack-vars) (car (last slack-vars)))
	  (format stream "~&;; slack variables with side condition x_i /= 0: ~{x[~a], ~} x[~a]" (butlast slack-vars) (car (last slack-vars))))
      (format stream "~%;; binding free variables to ease computation")
      (report-bindings stream initial-bindings "")
      (unless linear-eqns?
	(format stream "~&;; computing reduced Groebner basis..."))
      (finish-output stream))

    (if (and nil linear-eqns?) ; <--- LINEAR EQUATION SOLVE NOT YET WORKING!!
	(linear-equation-solve ibase slack-vars)
	;; compute Groebner basis using preflight poly checker and timeout (then)
	(multiple-value-bind (basis abnormal-exit) (groebner-basis ibase 
								   (lambda (poly)
								     (some #'(lambda (pattern)
									       (multiple-value-bind (applicable? non-fullfillable? new-bindings alternatives constraints)
										   (funcall (car pattern) poly nil)
										 (declare (ignore constraints))
										 (and applicable?
										      (or non-fullfillable?
											  (not (bindings-admissible? new-bindings slack-vars))
											  (and alternatives
											       (every #'(lambda (alternative)
													  (not (bindings-admissible? alternative slack-vars)))
												      alternatives))
											  (if new-bindings
											      (cons :bind new-bindings))))))
									   *patterns*))
								   then)
	  (cond ((eq abnormal-exit :hook) 
		 ;;	     (format t "~%XX ~a~%" (- (get-internal-real-time) now))
		 (sparq:report-time "computing Groebner basis - aborted")
		 (when *debug* 
		   (format stream "~&;; -> detected unsatisfiable constraint while computing Groebner basis")
		   (format stream "~%;;    this is the unsatisifiable polynomial:~%;;    ")
		   (print-poly (first basis) stream))
		 t)
		((eq abnormal-exit :time-out)
		 ;;(format t "~%XX ~a~%" (- (get-internal-real-time) now))
		 (sparq:report-time "computing Groebner basis - aborted due to time-out")
		 ;;(format t "X")
		 (when *debug* (format stream "~&;; TIME-OUT - ABORTING COMPUTATION!~%;; (refer to SparQ manual for increasing time limit)"))
		 :?)
		(t ;; Print out basis
		 (when abnormal-exit
		   (format t "~%abnormal-exit = ~a" abnormal-exit)
		   (break))
		 (sparq:report-time "computed Groebner basis")
		 (when *debug* 
		   (format stream "~%;;~%;; this is the reduced Groebner Basis:")
		   (let ((i 0))
		     (dolist (p basis)
		       (format stream "~%;;(~2a:) " (incf i))
		       (print-poly p stream)))
		   (format stream "~%;;"))
		 ;;(setf *now* now)
		 (prog1 (test-basis stream basis slack-vars () () basis then)
		   ;;(when *now*
		   ;; (format t "~%XX ~a~%" (- (get-internal-real-time) now)))
		   (report-time "ideal basis analysis"))))))))

#|
testen:
awk '/seconds of total/ {time +=  10*$1 - 10*$9; count +=1} END {print "count = " count;  print "avg. time = " ((time*0.1)/count)}' /tmp/gzeit_kein_hook

ohne hook: count:351, time = 17,9117

((:A B |0_4|C) (BC |0_4| D) (A B |4_A| D))

|#

(defun search-model (stream indention gbase bindings slacks timeout)
  (declare (special *patterns*))
;;  (format t "~%XX ~a~%" (- (get-internal-real-time) *now*))
;;  (setf *now* nil)
  (sparq:report-time "unsatisfiability test")
  (let ((polys (remove-if #'null (apply-bindings bindings gbase)))	 
	(test-values (list 0 -1 1 -2 2 -3 3 -4 4 -5 5 ))) ; 6 -6 8 -8 -10 10
    (when *debug* 
      (format stream "~&;;~a Trying to compute a model for:" indention)
      (let ((count 0))
	(dolist (p polys)
	  (format stream "~%;;~a (~2d :) " indention (incf count))
	  (print-poly p stream))))
    (labels ((consequences (polys indention)
	       ;; bestimmt die Konsequenzen einer Variablenbindung mittels der Loesungsmuster
	       ;; Rueckgabe ist ein Flag ob wir nicht in einer Sackgasse sind (z.B. weil eine
	       ;; Schlupfvariable 0 werden muesste); zweiter Rueckgabewert ist die aktualisierte
	       ;; polyliste
	       (let ((working? t))
		 (loop while working? do
		      (setq working? nil)
		      (do ((rest-polys polys (cdr rest-polys)))
			  ((or (null rest-polys)
			       working?))
			(let ((poly (first rest-polys)))
			  (dolist (pn *patterns*)
			    (multiple-value-bind (applicable? non-fullfillable? new-bindings alternatives constraints)  (funcall (car pn) poly ())
			      (when (and applicable? *debug*)
				(format stream "~%;;~a applying pattern '~a' to poly: " indention (cdr pn))
				(print-poly poly stream)
				(when non-fullfillable? (return-from consequences nil)))
			      (when new-bindings			       
				(if (bindings-admissible? new-bindings slacks)
				    (progn (setq polys (apply-bindings new-bindings polys)
						 working? t)
					   (when *debug*
					     (format stream "~%;;~a bindings entailed: " indention)
					     (dolist (b new-bindings)
					       (format stream "x[~d] <- ~a " (car b) (cdr b))))
					   (return))
				    (return-from consequences nil)))			      
			      (when alternatives
				(let ((admissible-alternatives (remove-if-not #'(lambda (alternative)
										  (bindings-admissible? alternative slacks))
									      alternatives)))
				  ;;(format t "~%;;~a ~d alternatives : ~a" indention (length admissible-alternatives) admissible-alternatives)
				  (if (null admissible-alternatives)
				      (return-from consequences nil)
				      (if (null (cdr admissible-alternatives)) ;; nur eine alternative?
					  (progn (setq polys (apply-bindings (first admissible-alternatives) polys)
						       working? t)
						 (return))
					  (if (and (null (cddr admissible-alternatives)) ; bei slack vars brauchen wir kein +/- in der wurzel zu beruecksichtigen...
						   (null (cdr (first admissible-alternatives)))
						   (null (cdr (second admissible-alternatives)))
						   (eq (caar (first admissible-alternatives))
						       (caar (second admissible-alternatives)))
						   (member (caar (first admissible-alternatives)) slacks)
						   (= (cdar (first admissible-alternatives))
						      (- (cdar (second admissible-alternatives)))))
					      (progn (setq polys (apply-bindings (list (cons (caar (first admissible-alternatives)) (abs (cdar (first admissible-alternatives))))) polys)
							   working? t)
						     (return))
					      ;;(format stream "~%;;~a CANNOT HANDLE ALTERNATIVES: ~a" indention alternatives)
					      )))))
			      (when constraints
				(let ((polys-changed? nil))
				  (dolist (c constraints)
				    (multiple-value-bind (updated-polys update?) (handle-constraint stream c polys indention t)
				      (when update?
					;;(format t "~%POLYS AFTER SUBSTITUTION:~%=====================")
					;;(poly::print-poly-list updated-polys)
					(setq working? t  ; bei erfolgreicher constraintbehandlung geht's weiter
					      polys-changed? t
					      polys updated-polys))))
				  (when polys-changed?
				    (return))))))))))
	       (values t polys))
	     
	     (free-variable (polys)
	       ;; sucht eine Nicht-Schlupfvariable in polys und gibt diese zurueck - nil andernfalls
	       (labels ((free-var-in-term (term)
			  (find-if #'(lambda (v)
				       (not (member v slacks)))
				   (mapcar #'first (second term))))
			(free-var-in-poly (p)
			  (and p
			       (or (free-var-in-term (first p))
				   (free-var-in-poly (rest p))))))
		 (and polys
		      (or (free-var-in-poly (first polys))
			  (free-variable (rest polys))))))

	     (check-solution (polys test-bindings indention)
	       (when (< timeout (get-internal-real-time))
		 (sparq:report-time "searching model")
		 (throw 'exit-proof :timeout))
	       (let ((free-var (free-variable polys)))
		 (if free-var
		     (let ((vals-2-try (sort (copy-list test-values) #'(lambda (x y) (declare (ignore x y)) (= 1 (random 2))))))
		       (dolist (v vals-2-try)
			 (when *debug*
			   (format stream "~%;;~a trying variable binding x_~a <- ~a " indention free-var v)
			   (let ((count 0))
			     (dolist (p polys)
			       (format stream "~%;;~a (~2d :) " indention (incf count))
			       (print-poly p stream))))
			 (multiple-value-bind (possible? updated-polys) (consequences (apply-bindings (list (cons free-var v)) polys) indention)
			   (unless possible? 
			     (when *debug* (format stream "~%;;~a variable binding x_~a <- ~a  is not possible!" indention free-var v)))
			   (if possible?
			       (check-solution (remove nil updated-polys) 
					       (cons (cons free-var v) test-bindings) 
					       (concatenate 'string "  " indention))))))
		     ;; Keine freien Variablen mehr
		     (if (null polys)
			 (progn
			   (sparq:report-time "searching model")
			   (when *debug*
			     (format stream "~%;;~a Found a model: " indention)
			     (dolist (b (append bindings test-bindings))
			       (format stream "~%;;~a  x[~a] <- ~a" indention (car b) (cdr b))))
			   (throw 'exit-proof :model))
			 (progn
			   ;; All variables that represent objects have been bound, so we have now either one of the following:
			   ;; 1) reduced the set of polynomials to "1 = 0", i.e., we do not have a model (-> return nil)
			   ;; 2) our polynomials involve slack variables only and we need to check whether we can find a solution for that (-> try *once* again to find a model, this time for the slack vars)
			   ;; 
			   ;; ***FIX ME***
			   ;; We can check more easily whether we got a model if we's know all our variable bindings (including forcing some vars to 0 early in the process). Then we can 
			   ;; check by calling "qualify" to see whether we got a model.
			   ;;(format t "~2%polys:~%~a~2%" polys)

			   ;; FIXME: 
			   ;; WIR DUERFEN NICHT IN DIE REKURSION OHNE SLACKS GEHEN, DANN WERDEN EVTL. SLACKS AUF NULL GESETZT
			   ;; UND ES WIRD EIN VERBOTENES MODELL KONSTRUIERT
			   ;; -> WIR MÜSSEN ALSO ALLE BINDUNGEN SAMMELN UND ÜBER QUALIFY GEHEN !!
			   (when (and (some #'(lambda (poly)
						(second (first poly)))
					    polys)
				      slacks) ; "(and slacks" ensures that we're only going to recursion once
			     ;;(search-model stream indention polys test-bindings () timeout)
			     (throw 'exit-proof :?)
			     )
			   ))))))
      (prog1 (check-solution polys () indention)
	(sparq:report-time "searching model")))))
#|
    (unless vars
      (return-from search-model t))
    
    (labels ((check-basis (gbase)
	       (let* ((swallow (make-string-output-stream))
		      (result (test-basis swallow gbase slacks nil timeout)))
		 (null result)))
	     (test/rec (vars values bindings)
	       (if vars
		   (some #'(lambda (value)
			     (test/rec (cdr vars) values (cons (cons (car vars) value) bindings)))
			 values)
		   (let ((model? (check-basis (apply-bindings bindings gbase))))
		     (when model?
		       (format t "~3% FOUND MODEL FOR BINDINGS ~a" bindings))
		     model?))))
      
      (test/rec vars (list 0 -1 1 -2 2 -3 3 -4 4 -5 5) ()))))  
|#

(export '(test-emptyness))
