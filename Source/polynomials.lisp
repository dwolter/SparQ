;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; Polynomials.lisp
;;;
;;; Defines a package for computing with multivariate polynomials
;;; 

;; Change history (most recent first):
;; 2009-02-09 DW  moved bindings here
;; 2007-06-14 DW  moved Groebner computation into groebner.lisp
;; 2007-02-22 DW  included my existing source into SparQ
;; 2005-09-01 DW  updated functions for printing polynomials
;; 2003-07-09 JHR changed function names (more consistent semantics)
;;		  rearrenged source-code
;;		  fixed "term-divides?" for degenerated cases
;;		  new function "s-polynomial-lcm" gets "lcm" as additional parameter
;;		  removed all uses of "subtract"
;;		  reimplemented Buchberger's algorithm "buchberger-alt". This one uses Buchberger's first criterion
;;		  implemented advanced version of Buchberger's algorithm "buchberger-neu"
;;		  test case included
;; 2002-03-08 DW  fixed bugs in substract
;; 2002-03-04 DW  first version of revised code 

(defpackage :polynomials
  (:use :common-lisp :sparq)
  (:nicknames :poly)
  (:export :make-poly :add :subtract :scalar-multiply :normalize :multiply :print-poly :groebner-basis :test-emptyness))

(in-package :poly)

;;
;; Implementation notes:
;;
;; Polynomials are represented as nested lists.
;; E.g., the polynomial 1*x1^3 + 2*x2^2*x5^8 is represented as ((1 ((1 3))) (2 ((2 2)(5 8)))).
;; The terms (and monomials, too) are supposed to be ordered.
;;
;; Definitions in this file:
;;
;; make-poly 		constructs a proper polynomial from a polynomial-like list by sorting the terms and monomials according to the term-order
;;
;; set-order            sets the order on polynomials (currently either :grlex (graded lexicographic) or :lex (lexicographic)
;; monomial>		predicate for order of monomials
;; term>		predicate for order of terms
;; polynomial>		predicate for order of polynomials
;;
;; add			adds two polynomials
;; scalar-multiply	multiplies a constant and a polynomial
;; scalar-multiply!	like above, but destructively modifies the polynomial
;; normalize		normalizes a polynomial by scalar multiplication such that the leading term's coefficient equals 1
;; subtract		subtracts two polynomials
;; multiply		multiplies two polynomials
;;
;; print-poly		prints a polynomial to a stream
;; print-poly-tex       prints tex source to display a polynomial to a stream


;;;
;;;	Ordering of monomials, terms, and polynomials
;;;

(let ((ordnung-lex? t))

  (defun monomial>/lex (var-1 var-2)
    "Compares two terms according to lexicographic order. Note, that zero is not allowed as factor."
    (declare (type list var-1 var-2))
    (cond ((null var-1) nil)
	  ((null var-2) t)
	  ((< (caar var-1) (caar var-2)) t)
	  ((= (caar var-1) (caar var-2))		; Variablenindex gleich, dann Exponenten vergleichen
	   (cond ((> (cadar var-1) (cadar var-2)) t)
		 ((= (cadar var-1) (cadar var-2)) (monomial> (cdr var-1) (cdr var-2)))
		 (t nil)))
	  (t nil)))
  

 (defun monomial>/gr-lex (var-1 var-2)
    (let ((grad1 0)
	  (grad2 0))
      (dolist (v1 var-1)
	(setf grad1 (+ grad1 (second v1))))
      (dolist (v2 var-2)
	(setf grad2 (+ grad2 (second v2))))
      (cond ((> grad1 grad2) t)
	    ((< grad1 grad2) nil)
	    ((= grad1 grad2) (monomial>/lex var-1 var-2)))))

  (defun monomial> (var-1 var-2)
    (if ordnung-lex?
	(monomial>/lex var-1 var-2)
	(monomial>/gr-lex var-1 var-2)))

  (defun set-order (x)
    (cond ((eq x :lex) (setq ordnung-lex? t))
	  ((eq x :gr-lex) (setq ordnung-lex? nil))
	  (t (error "Can't set order to ~a." x))))
)

(declaim (inline term>))
(defun term> (term-1 term-2)
  "Order of terms based on order of terms"
  (declare (type cons term-1 term-2))
  (monomial> (second term-1) (second term-2)))


(defun polynomial> (poly-1 poly-2)
  "Order of polynomials based on order of terms"
  (cond ((null poly-1) nil)
        ((null poly-2) t)
        ((term> (first poly-1) (first poly-2)) t)
        ((term> (first poly-2) (first poly-1)) nil)
        (t (polynomial> (cdr poly-1) (cdr poly-2)))))

;;Ordnung von Polynomenpaaren, basiert auf 
;;Kleinsten gemeinsamen vielfachen von deren Leitmonmen
(defun lcm> (pair1 pair2)
  (if pair1
      (if pair2

	  (let* ((poly1 (car pair1))
		 (poly2 (cdr pair1))
		 (poly3 (car pair2))
		 (poly4 (cdr pair2))
		 (HM-poly1 (cadar poly1))
		 (HM-poly2 (cadar poly2))
		 (HM-poly3 (cadar poly3))
		 (HM-poly4 (cadar poly4))
		 (lcm-hm-pair1 (monomial-lcm HM-poly1 HM-poly2))
		 (lcm-hm-pair2 (monomial-lcm HM-poly3 HM-poly4)))
	    
	    (monomial> lcm-hm-pair1 lcm-hm-pair2))
	  t)
      nil))


;;;
;;; Constructor 
;;;

(defun make-poly (terms)
  "Makes a polynom from terms by sorting terms, i.e. ((quot-1 ((x-1 exp-1) (x-1 exp-2) ...) ...), in lexicographic order of variables x-1; 
   terms will be destructively modified. Different terms with same variables and terms with a coefficient of zero are not allowed"
  (sort (mapc #'(lambda (term)
                  (setf (second term) (sort (second term) #'< :key #'car)))
              terms)
        #'term>))
	

;;;
;;; Skalare Multiplikation
;;;

(defun scalar-multiply (scalar polynomial)
  "Scalar multiplication" 
  (if (= 0 scalar)
    nil
    (mapcar #'(lambda (term)
                (cons (* scalar (car term)) (cdr term)))
            polynomial)))

(defun scalar-multiply! (scalar polynomial)
  "Scalar multiplication, polynomial gets destructively modified."
  (declare (type rational scalar)
	   (type list polynomial)
	   (optimize (speed 3) (safety 0)))
  (if (= 0 scalar)
    nil
    (mapc #'(lambda (term)
              (setf (car term) (* scalar (car term))))
          polynomial)))

(defun normalize (polynomial)
  "Normalizes a polynomial such that the leading-term's coefficient equals 1."
  (if (null polynomial)
    nil
    (let ((lc (caar polynomial)))
      (if (eq lc 1)
	  polynomial
	  (scalar-multiply (/ 1 (caar polynomial)) polynomial)))))


;;;
;;;	Addition & Subtraktion zweier Polynome
;;;

(defun add/accu (poly-1 poly-2 accu)
  (declare (type list poly-1 poly-2 accu)
	   (optimize (speed 3)))
  (cond ((and (null poly-1) (null poly-2)) (nreverse accu))
	;; Ist ein Polynom leer, so ist die Summe gleich dem anderen
	((null poly-1) (nconc (nreverse accu) poly-2))
	((null poly-2) (nconc (nreverse accu) poly-1))
	;; 4 Faelle sind zu unterscheiden: LT(p1) < LT(p2) und umgekehrt und LT(p1) = LT(p2)
	;; Bei letztem gibt es noch den Sonderfall, das durch Addition Ausloeschung von Termen eintritt
	(t (let ((lead-term-1 (first poly-1))
                 (lead-term-2 (first poly-2)))
             (cond ((term> lead-term-1 lead-term-2) (add/accu (rest poly-1) poly-2 (cons lead-term-1 accu)))
                   ((term> lead-term-2 lead-term-1) (add/accu (rest poly-2) poly-1 (cons lead-term-2 accu)))
		   (t (let ((new-coeff (+ (the rational (first lead-term-1)) (the rational (first lead-term-2)))))
			(declare (type rational new-coeff))
                        ;;(assert (equal (cdr lead-term-1) (cdr lead-term-2)))
                        (if (= 0 new-coeff)	; no floats, so we can compare using equality
			    (add/accu (rest poly-1) (rest poly-2) accu)
			    (add/accu (rest poly-1) (rest poly-2) 
				      (cons (cons new-coeff (rest lead-term-1)) accu))))))))))

(declaim (inline add))
(defun add (poly-1 poly-2)
  "Adds two polynomials."
  (add/accu poly-1 poly-2 nil))

(defun subtract (poly-1 poly-2)
  "Subtracts two polynomials."
  (add/accu poly-1 (scalar-multiply -1 poly-2) nil))

#|
(defun subtract (poly-1 poly-2)
  "Subtracts two polynomials." 
  (cond ((null poly-1) (scalar-multiply -1 poly-2))	; poly-1 = 0 => -1*poly-1
        ((null poly-2) poly-1)				; poly-2 = 0 => poly-1
        ; 4 Faelle sind zu unterscheiden: LT(p1) < LT(p2) und umgekehrt und LT(p1) = LT(p2)
	; Bei letztem gibt es noch den Sonderfall, das durch Addition Ausloeschung von Termen eintritt
        (t (let ((lead-term-1 (car poly-1))
                 (lead-term-2 (car poly-2)))
             (cond ((term> lead-term-1 lead-term-2) (cons lead-term-1 (subtract (cdr poly-1) poly-2)))
                   ((term> lead-term-2 lead-term-1) (cons (list (* -1 (first lead-term-2))
                                                                (second lead-term-2))
                                                          (subtract poly-1 (cdr poly-2))))
                   (t (let ((new-coeff (- (car lead-term-1) (car lead-term-2))))
                        (assert (equal (cdr lead-term-1) (cdr lead-term-2)))
                        (if (= 0 new-coeff)	; nix floats, dann geit dat ok...
                          (subtract (cdr poly-1) (cdr poly-2))
                          (cons (cons new-coeff (cdr lead-term-1)) (subtract (cdr poly-1) (cdr poly-2)))))))))))
|#


;;;
;;;	Alles zur Multiplikation zweier Polynome
;;;

(defun monomial-multiply (v1 v2)
  (cond ((null v1) v2)
        ((null v2) v1)
        (T (cond ((eq (caar v1) (caar v2))
                  (append (list (list (caar v1) (+ (cadar v1) (cadar v2))))
                         (monomial-multiply (cdr v1) (cdr v2))))
                 ((< (caar v1) (caar v2))
                  (append (list (car v1))
                         (monomial-multiply (cdr v1) v2)))
                 (T
                  (append (list (car v2))
                         (monomial-multiply v1 (cdr v2))))))))

(defun term-poly-multiply (term poly)
  (if (null poly) nil
      (cons (list (* (car term) (caar poly)) (monomial-multiply (cadr term) (cadar poly)))
	    (term-poly-multiply term (cdr poly)))))

(defun multiply (poly-1 poly-2)
  "Multiplies two polynomials." 
  (cond ((or (null poly-1) (null poly-2))
         nil)

        ((null (cdr poly-1))				; nur 1 term in poly-1
         (term-poly-multiply (car poly-1) poly-2))

         (t (add (term-poly-multiply (car poly-1) poly-2)
                 (multiply (cdr poly-1) poly-2)))))


;;;
;;;	Alles zur Berechnung des S-Polynoms
;;;

;; Das Kleinste Gemeinsame Vielfache von zwei Monomen

(defun monomial-lcm (f g)
  (cond ((and (null f) (null g)) nil)
        ((null f) g)
        ((null g) f)
        (T (cond ((< (caar f) (caar g)) (cons (car f) (monomial-lcm (cdr f) g)))
                 ((> (caar f) (caar g)) (cons (car g) (monomial-lcm f (cdr g))))
                 (T (if (< (cadar f) (cadar g))
		        (cons (car g) (monomial-lcm (cdr f) (cdr g)))
                        (cons (car f) (monomial-lcm (cdr f) (cdr g)))))))))

; Teilt zwei Monome durcheinander, Teilbarkeit ist vorausgesetzt

(defun monomial-div (v1 v2)
  (cond ((null v2) v1)
        ((< (caar v1) (caar v2))
         (cons (car v1)
               (monomial-div (cdr v1) v2)))
        (T (if (eq (cadar v1) (cadar v2)) ; Ausloeschung einer Variable?
             (monomial-div (cdr v1) (cdr v2))
             (cons (list (caar v1) (- (cadar v1) (cadar v2)))
                   (monomial-div (cdr v1) (cdr v2)))))))

(defun s-polynomial (poly-1 poly-2)
  "Computes the S-polynomial of two polyomials." 
  (let ((lcmv (monomial-lcm (cadar poly-1) (cadar poly-2))))	; least common multiple of variables 
    (subtract (multiply (list (list (/ 1 (caar poly-1))
                                    (monomial-div lcmv (cadar poly-1))))
                        poly-1)
              (multiply (list (list (/ 1 (caar poly-2))
                                    (monomial-div lcmv (cadar poly-2))))
                        poly-2))))


;;;
;;;	Polynomdivision
;;;


;; Fuehrt den Test auf nicht kleinere Exponenten durch

(defun monomial-divides? (var1 var2)
  (cond ((null var2) T)
        ((null var1) nil)
        (T (cond ((> (caar var1) (caar var2)) nil)
                 ((< (caar var1) (caar var2)) (monomial-divides? (cdr var1) var2))
		 (T (and (not (> (cadar var2) (cadar var1)))
                         (monomial-divides? (cdr var1) (cdr var2))))))))

;; Teilbarkeitspraedikat zweier Terme

(defun term-divides? (term-1 term-2)
  "Returns t, iff term-2 is a divider of term-1."
  (cond  ((null term-1) T)
	 ((= (car term-1) 0) T)                  ; should never occur
         ((null term-2) NIL)
	 ((= (car term-2) 0) NIL)                ; should never occur
         (t (monomial-divides? (second term-1) (second term-2)))))

;; Dividiert zwei Terme durcheinander, Teilbarkeit vorausgesetzt

(defun term-div (term1 term2)
  (cons (/ (car term1) (car term2))
        (list (monomial-div (cadr term1) (cadr term2)))))


;; Die eigentliche Polynomdivision, f ist Polynom, polylist die
;; Liste der Dividenden. Als Ergebnis ergibt sich der Rest gefolgt
;; von der Liste der Divisionsergebnisse. Direkte Umsetzung des in
;; Kaitel 1 beschriebenen Divisionsalgorithmus.

#|
(defun divide-by-list (polynomial polylist)
  "Divides a polynomial by a list of polynomials as defined in [Cox]. The result are the multiple-values rest and vector of individual factors."
  (let* ((num-of-polys (length polylist))
         (poly-vector (coerce polylist 'vector))
         (a (make-array num-of-polys))
         rest
         (poly polynomial))
    (loop while poly do
          (do ((i 0 (+ i 1))
               (divisionoccured? nil))
              ((or (eq i num-of-polys)
                   divisionoccured?
                   (null poly))
               (unless divisionoccured?
                 (let ((lead-term-poly (first poly)))
                   (setq rest (add rest lead-term-poly)
                         poly (subtract poly lead-term-poly)))))
            
            (when (divides? (first poly) (first (aref poly-vector i)))
              (let ((quotient (list (divterm (first poly) (first (aref poly-vector i))))))
                (setf (aref a i) (add (aref a i) quotient)
                      poly (subtract poly (multiply (aref poly-vector i) quotient))
                      divisionoccured? t)))))
    (values rest a)))
|# 

(defun reduce-by-list (polynomial polylist)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (type list polynomial polylist))
  "Divides a polynomial by a list of polynomials as defined in [Cox]. The rest is returned."  
  (let ((poly polynomial)		; working copy of polynomial
        (rest ()))			; the remains to be constructed
    (loop while poly do
	 (let ((lead-term (cons (- (caar poly)) (cdar poly))))  ; negate, since add is used for reduction below
	   (do ((division-occured? nil)                ; Iterate over polylist
		(rest-list polylist (cdr rest-list)))
	       ((or division-occured?                  ; Exit do-loop when division occured or iteration is finished
		    (null rest-list))
		(unless division-occured?              ; Upon return move head to rest if it didn't divide
		  (push (pop poly) rest)))
	     ;; Divide lead-term of poly by current poly from polylist (= car of restlist)
	     (let* ((divisor-poly (car rest-list))
		    (divisor-lead-term (car divisor-poly)))	       
	       (when (term-divides? lead-term divisor-lead-term)
		 (let ((quotient (list (term-div lead-term divisor-lead-term)))) 
		   (setq poly (add poly (multiply quotient divisor-poly))        
                          division-occured? t)))))))
    (nreverse rest)))

;;;
;;;	Buchberger
;;;
;;; 


; some magic function, what the heck is lcm-hm?
(defun s-polynomial-lcm (lcm-hm poly-1 poly-2)
  "Computes the S-polynomial of two polyomials." 
;    (format t "~A~%" lcm-hm)
    (add (term-poly-multiply (list (caar poly-1) (monomial-div lcm-hm (cadar poly-1)))
                        poly-1)
         (term-poly-multiply (list (- (caar poly-2 )) (monomial-div lcm-hm (cadar poly-2)))
                        poly-2)))

		
(defun pop-random (lst)
  (let ((x (nth (random (length lst)) lst)))
    (setq lst (delete x lst))
    x))

;;;prueft ob ein Monomterm einen positiven Vorzeichen hat
(defun ist-positiv? (monomterm)
   (< 0 (first monomterm)))

 ;;; diese funktion berechnet die Anzahl der "falschen" Vorzeichen in einem Polynom
(defun afv (poly)
  (let ((len 0)
	(pos 0))
    (declare (type fixnum len pos))
    (dolist (term poly)
      (when (< 0 (first term))
	(incf pos))
      (incf len))
    (min pos (- len pos))))
    

(defun prune-non-slack-polys (polys slack-variables)
  "Given a list of polys all polynomials are filtered out which include 
   at least some non-slack variables" 
  (remove-if #'(lambda (poly)
                 (some #'(lambda (term)
                           (some #'(lambda (var)
                                     (not (member (car var) slack-variables)))
                                 (second term)))
                       poly))
             polys))
    

(defun prove-basis (polylist slacks)
  (let ((slack-poly (remove-if #'null (prune-non-slack-polys polylist slacks))))
    
    (not (null (remove-if #'null (mapcar #'(lambda (x)(if (= 0 (poly::afv x)) x)) slack-poly))))
       ))


#|
(defun update-todo (todolist basis p)
  (append todolist
     (mapcan #'(lambda (q)
                  (let* ((head-monom-p (cadar q))
                         (head-monom-q (cadar p))
                         (head-monom-lcm (monomial-lcm head-monom-p head-monom-q)))
;          (format (equal head-monom-lcm (monomial-multiply head-monom-p head-monom-q)) "-")
 			 (if (equal head-monom-lcm (monomial-multiply head-monom-p head-monom-q))
                             NIL
                             (list (list head-monom-lcm p q)))))
           basis)
  ))

|# 





; Definition der Ausgabe fuer ein Polynom
(defun print-poly (p &optional (stream t))
  (if (null p)
      (format stream "0")
      (let ((start? t))
	(dolist (term p)
	  (if start?
	      (if (null (second term))
		  (format stream "~a" (first term))
		  (progn 
		   (unless (= 1 (car term))  ; suppress "1 *"
		     (if (= -1 (car term))
			 (format stream "-")
			 (format stream "~a" (car term))))
		   (setq start? nil)))
	      (if (and (= 1 (abs (car term))) (second term))
		  (format stream " ~a " (if (<= 0 (car term)) "+" "-"))
		  (format stream " ~a ~a" (if (<= 0 (car term)) "+" "-") (abs (car term)))))
	  (let ((start? t))
	    (dolist (m (second term))
	      (if (and start? (= 1 (abs (car term))))
		  (format stream "x[~a]" (first m)) 
		  (format stream "*x[~a]" (first m)))
	      (unless (= 1 (second m))
		(format stream "^~a" (second m)))
	      (setq start? nil)))))))

; Definition der Ausgabe fuer ein Polynom
(defun print-poly-tex (p &optional (stream t))
  (if (null p)	; Das "leere" Polynom ist 0
    (format stream "0")
    (termlist2tex p stream t)))

; Gibt eine Liste (von Polynomen) als aus
(defun print-poly-list (polylist &optional (stream t))
  (let ((i 1))
    (dolist (f polylist)
      (format stream "~%;; [~a:] " i )
      (print-poly f stream)
      (incf i))))



; Gibt eine Liste (von Polynomen) als TeX-Aufzaehlung aus

#|
(defun print-poly-list (polylist &optional (stream t))
  (format stream "~%\\begin\{enumerate\}")
  (dolist (f polylist)
    (format stream "~%\\item ")
    (print-poly-tex f stream))
  (format stream "~%\\end\{enumerate\}~%"))
|# 
#|
; Ausgabefunktion fuer das durch den Divisionsalgorithmus
; erzeugte Datenformat
(defun divErgebnisAus (result)
  (let* ((rest (car result))
        (koefar (cadr result))
        (koeflen (array-dimension koefar 0)))
    (progn
      (dotimes (i koeflen)
        (format t "~%~W" (polynom-termlist (aref koefar i))))
      (format t "~%mit Rest ~W~%" (polynom-termlist rest)))))

|# 

; Hilfsfunktionen, die den Export in TeX-Code vornehmen
(defun termlist2tex (terme stream first?)
  (if (null terme) ()
      (progn
        (cond
         ((and (not (null (cadar terme))) (eq (caar terme) -1))
          (format stream "  -"))
         ((and (not (null (cadar terme))) (eq (caar terme)  1))
          (unless first? (format stream "  +")))
         (T (if (< (caar terme) 0)
		(format stream "  ")
		(format stream "  +"))
            (format stream "~W" (caar terme))))
        (varlistout (cadar terme) stream)
        (termlist2tex (cdr terme) stream nil))))

(defun varlistout (vars stream)
  (if (not (null vars))
    (progn
      (if (< (caar vars) 10)
        (format stream " x_~W" (caar vars))
        (format stream " x_\{~W\}" (caar vars)))
      (if (not (or (null (cadar vars)) (eq (cadar vars) 1)))
        (format stream "^~W" (cadar vars)))
      (varlistout (cdr vars) stream))))

(defun reduce-factor(poly)
  (let ((f (apply #'min  (mapcar #'abs (mapcar #'first poly)))))
    
    (if (and (every #'(lambda (x) (= 0 (mod x f)))(mapcar #'first poly)) (not (= f 1)))
	(mapcar #'(lambda (x monom) (cons (/ x f) monom)) 
		(mapcar #'first poly)(mapcar #'rest poly))
        poly)))

;; Bindings are represented as dotted lists consiting of the variable's index and the number.
;; For example ((2 . 0) (3 . 1)) causes x2 to be 0 and x3 to be 1. Obviously, binding variables
;; to other variables is not possible.
(defun apply-bindings-to-term (bindings term)
  (let ((new-monomial ())
        (new-coefficient (first term)))
    (dolist (m (second term))
      (let ((binding (assoc (car m) bindings)))
        (if binding
	    (setq new-coefficient (* new-coefficient (expt (cdr binding) (second m))))
	    (push m new-monomial))))
    (list new-coefficient (nreverse new-monomial))))

(defun apply-bindings-to-poly (bindings poly)
  "Applies a list of bindings ((var-index . number) ...) to a polynomial." 
  (and poly
       (remove-if #'(lambda (term)
		      (= 0 (car term)))
		  (reduce #'(lambda (poly-sofar next-term)
			      (add poly-sofar (list (apply-bindings-to-term bindings next-term))))
			  (cdr poly)
			  :initial-value (list (apply-bindings-to-term bindings (car poly)))))))

(defun apply-bindings (bindings polys)
  "Applies a list of bindings ((var-index . number) ...) to a list of polynomials." 
  (mapcar #'(lambda (poly)
              (apply-bindings-to-poly bindings poly))
          polys))



#|_("Monaco" 9 :BOLD  (0.0 0.0 0.0))_|#;;;
;;; Test cases
;;;
#|_("Monaco" 9 :PLAIN  (0.0 0.0 0.0))_|#


; "mybasis2" is a reduced Groebner basis of "mybasis"
#|
(setf mybasis (mapcar #'make-poly
		'( ( ( 1 ((5 1))) (-1 ((9 2))) )
		 ( (-1 ((7 1))) (-1 ((10 2))) )
		 ( (-1 ((5 1))) (1 ((7 1))) (1 ((5 1) (8 1))) (-1 ((6 1) (7 1))) (-1 ((11 2))))
		 ( (-1 ((5 1) (8 1))) (1 ((6 1) (7 1))) (-1 ((12 2))) )
		 )))

(setf mybasis2 (mapcar #'make-poly
		'( ( ( 1 ((5 1))) (-1 ((9 2))) )
		 ( (-1 ((7 1))) (-1 ((10 2))) )
		 ( (1 ((6 1) (10 2))) (1 ((8 1) (9 2))) (1 ((12 2))))
                 ( (1 ((1 1) (10 2))) (1 ((8 3) (9 2))) (1 ((12 2))))
		 ( (-1 ((11 1))) (-1 ((10 2))) (-1 ((3 4) (2 8) (11 2))) (-1 ((12 2))))
		 ( (-1 ((9 2))) (-1 ((10 2))) (-1 ((11 2))) (-1 ((12 2))))
		 )))
 (setf mybasis (mapcar #'poly::make-poly 
			       '(((1 ((1 3))) (-2 ((1 1)(2 1))))
				 ((1 ((1 2)(2 1))) (-2 ((2 2))) (1 ((1 1)))))

|#



















