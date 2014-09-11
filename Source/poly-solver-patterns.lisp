; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;


;; Change history (most recent first):
;; 2009-10-20 DW   ouch: fixed a bug in computing necessary bindings
;; 2008-05-14 DW   reordered patterns such that inconsistencies are detected earlier
;; 2007-10-22 DW   included polynomial patterns into SparQ, start of this history




;;;   PATTERN DEFINITIONS
;;;

;; patterns return 4 multiple values:
;; - Is the pattern applicable? (t or nil)
;; - Equation cannot be solved (t or nil) (e.g., x^2 + 1 = 0)
;; - necessary bindings  ((var-index . number) ...)
;; - alternatives of bindings (list of above)

(in-package :poly)



(defpattern "x_i^n <-> x_j^m" ((?coef-1 ((?var-1 ?n))) (?coef-2 ((?var-2 ?m))))
  (let ((c (filter-constraints `((,?coef-1 ,?var-1 ,?n <-> ,(- ?coef-2) ,?var-2 ,?m )))))
    (reply :applicable? c
	   :constraints c)))

(defpattern "const" ((?const ()))
  (values t t))

(defpattern "x^n" ((?coefficient ((?var-index ?exponent))))
  (values t nil (list (cons ?var-index 0)) nil))


(defpattern "x^n + const" ((?coefficient ((?var-index ?exponent))) (?const ()))  
  (if (or (and  (< 0 ?coefficient)
		(evenp ?exponent) 
		(> 0 ?const))
	  (and  (> 0 ?coefficient)
		(evenp ?exponent) 
		(< 0 ?const)))
      (let ((root (expt (/ (- ?const) ?coefficient) (/ 1 ?exponent))))
	(if (realp root)
	    (let ((rroot (rationalize root)))
	      (if (= (- (expt rroot ?exponent)) ?const) 
		  ;; 'echte' Wurzel berechnet
		  (if (oddp ?exponent)
		      (reply :applicable? t :non-fullfillable? nil :new-bindings (list (cons ?var-index rroot)))
		      (reply :applicable? t :non-fullfillable? nil :alternatives (list (list (cons ?var-index rroot))
										      (list (cons ?var-index (- rroot))))
			     ;; Der zusaetzliche Constraint ist hier nur notwendig, da bei search-model keine Alternativen ausgenutzt werden koennen - bei test-basis macht das aber kein problem, da die variable vor abhandlung des constraints bereits eliminiert ist
			     :constraints `((,?var-index ,?exponent <- ,(/ (- ?const) ?coefficient)))))
		  ;; 'unechte' Wurzel: versuche var^exp durch -coeff ersetzen...
		  (reply :applicable? t :non-fullfillable? nil :constraints `((,?var-index ,?exponent <- ,(/ (- ?const) ?coefficient))))))
	  (reply :applicable? t
		 :non-fullfillable? t)))))

(defpattern "ax^2 + bx + c = 0" ((?a ((?x 2))) (?b ((?x 1))) (?c ()))
  (let* ((p (/ ?b ?a))
         (q (/ ?c ?a))
         (-p/2 (- (/ p 2)))
         (root2 (- -p/2 q)))
    (if (< root2 0)
      (values t t)
      (let ((rroot (rationalize (sqrt root2))))
	(when (= root2 (* rroot rroot))
	  (values t nil nil (list (list (cons ?x (- -p/2 rroot))) (list (cons ?x (+ -p/2 rroot))))))))))


;; given the list of list of variables' indices of a polynomial where each term
;; needs to be zero, the necessary and alternative bindings are computed.
;; E.g., "x1^2 + x1^2*x2^2 + x3^2*x4^2" 
;; input is ((1) (1 2) (3 4))
;; => necessary (1 . 0)
;; => alternatives ( ((3 . 0))  ((4 . 0)) )
;;
;; zero-bindings    computes the overall result returning multiple values
;; alternatives     used by the above, computes the alternatives given the lists of variables in the monomials
;;
;; gathers alternative var bindings from x[1]^2*x[2]^2 + x[1]^2*x[3]^2 + ... = 0
;; not quite optimal: some unnecessary alternatives are generated too
;;
(defun alternatives (vars)
  (labels ((gather-alternatives (vars)
	     (if vars
		 (mapcan #'(lambda (v)
			     (mapcar #'(lambda (alt)
					 (ninsert v alt #'<))
				     (alternatives (remove-if (?curry (find v ?alt))
							      (cdr vars)))))
			 (car vars))
		 (list nil))))
    (let ((alts (gather-alternatives vars)))
      (remove-if #'(lambda (a1)
		     (find-if #'(lambda (a2)
				  (and (not (eq a1 a2))
				       (subsetp a2 a1)))
			      alts))
		 alts))))

;; computes the binding entailed when the pattern 'sum of terms whose coefficitions ...' applies
(defun zero-bindings (vars)
  (let ((ncs ())    ;; for collecting vars that must take 0
	(mvars ())) ;; multiple vars: will take vars with *some* ncs removed

    ;; 1.) compute the variables necessarily 0 (single in monom or contained in every var)
    (setq ncs (reduce #'(lambda (ncs var)
			  (let ((var-occurences (remove-duplicates var))) ; we don't distinguish between x_i^2 and x_i^4 (^j has been stripped away already)
			    (if (cdr var-occurences) ; multiple vars?
				(progn
				  (push var-occurences mvars)
				  ncs)
				(pushnew (car var) ncs))))
		      vars
		      :initial-value nil))
    ;; 2.) remove all necessary 0-bindings and vars 'shadowed' by them (e.g.: if we know x<-0, then 
    ;;     we can't infer anything from x*y = 0
    (let ((z-bind (?curry (cons ?x 0))))
      (values (mapcar z-bind ncs)
	      (mapcar (?curry (mapcar z-bind ?lst)) (alternatives (delete-if #'(lambda (var)
										 (some (?curry (find ?v ncs)) var)) 
									     mvars)))))))

(defpattern "simple product" ((?coefficient-1 ?monom-1))
  (reply :applicable? t
	 :non-fullfillable? nil
	 :new-bindings nil
	 :alternatives (mapcar #'(lambda (x)
				   (list (cons (first x) 0)))
			       ?monom-1)))

(defpattern "sum of terms whose coeficients have same sign and even exponent" ((?coefficient-1 ?monom-1) . ?rest-terms)
  (if (and (null ?monom-1)
           (null ?rest-term)
           (/= ?coefficient-1 0))
    (reply  :applicable? t
            :non-fullfillable? t)
    (let ((sign (signum ?coefficient-1)))
      (if (and (every #'evenp (mapcar #'second ?monom-1))
               (every #'(lambda (term)
                          (eq sign (signum (first term))))
                      ?rest-terms)
               (every #'(lambda (term)
                          (every #'(lambda (var)
                                     (evenp (second var)))
                                 (second term)))
                      ?rest-terms))
        (if (and ?rest-terms (null (cadar (last ?rest-terms))))	; constant term? (last monomial is nil)
          (reply :applicable? t 
		 :non-fullfillable? t)
          (multiple-value-bind (necessary-bindings alternative-bindings) 
	      (zero-bindings (cons (mapcar #'first ?monom-1)
				   (mapcar #'(lambda (term)
					       (mapcar #'first (second term)))
					   ?rest-terms)))
            (reply :applicable? t
		   :non-fullfillable? nil
		   :new-bindings necessary-bindings
		   :alternatives alternative-bindings)))))))