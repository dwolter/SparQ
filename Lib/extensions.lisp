;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006-2012 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/
;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;; import some useful stuff
(shadowing-import '(calculi:calculus          ;; TYPE: a qualiative calculus
		    calculi:relation          ;; TYPE: a qualitative relation
		                              ;; FUNC: (relation <calculus> <relation-specifier>) constructs a relation from its print name
		    calculi:relation->string  ;; FUNC: converts a relation into a string (e.g., for printing)
		    calculi:relation=         ;; FUNC: predicate for checking equality of relations
		    calculi:converse          ;; FUNC: (converse <calculus> relation)
		    calculi:composition       ;; FUNC: (converse <calculus> rel-1 rel-2)
		    calculi:intersect         ;; FUNC: (intersect <calculus> rel-1 rel-2) conjunction of relations: intersection of relation sets
		    calculi:unite             ;; FUNC: (unite <calculus> rel-1 rel-2) disjunction of relations: union of relation sets

		    csp:constraint-network    ;; TYPE: constraint-network (construction with make-instance)
		    csp:constraint-network-objects  ;; FUNC list of all object variables in a constraint network
		    csp:constraint-network-calculus ;; FUNC calculus from which the relations in the csp
		    csp:constraints           ;; FUNC: returns list of constraints contained in a constraint-network

		    csp:constraint            ;; TYPE: constraint
		    csp:constraint-relation   ;; PROP: relation of a constraint
		    csp:constraint-object-1   ;; PROP: first argument of the constraint relation
		    csp:constraint-object-2   ;; PROP: second/remaining arguments of the constraint relation (single object for binary calucli, a list otherwise)
		    csp:make-constraint       ;; FUNC: constraint constructor, args: object-1, relation, object-2
))

;; There's a convenience macro 'with-calculus' that allows you to ommit the calculus parameter:
;; (with-calculus c
;;   ... (composition (relation 'po) (relation 'ntpp)) ... )
;;
;;    (with-calculus c
;;      (format t "~%~w o ~w = ~w~%" 'l 'r (relation->string (composition (relation 'l) (relation 'r)))))

;; Ist ein 3er-Chirotop eine 'fast beliebige' Abbildung \chi V^3 -> {-1,0,+1} oder
;; muessen die Grassmann-Pluecker-Relationen unbedingt erfuellt sein??

(def-tool ("translate" (c (calculus flipflop)) (csp constraint-network c) "chirotope")
    :documentation "given a constraint-network over flip-flop relations extract oriented matroid as chirotope"
    :requires      "matroids/lr->matroids.lisp"
    ;; :result-type chirotope
    ;; :type homomorphism on (constraint-network (satisfies ONLY-L/R-RELS)) <-> (chirotope (satisfies ....))
    ;; :type uebertraegt (satifiability csp) nach (realizability von chirotope)
    ;;
    ;; ==> sollte man in der Lage sein, die Funktionseigenschaften (z.B. Homomorphismus, Entscheidungsprozedur,...)
    ;;     auf Subtypen spezialisiert ausdrücken zu können? Bsp.: eine Methode ist eine Entscheidungsverfahren
    ;;     für CSPs in denen eine bestimmte "böse" Relation nicht auftritt, ansonsten ist es nur eine untere 
    ;;     Approximation. Hätte man das nicht, müsste man den Typ des Tools beschränken, Tool-Code duplizieren (hässlich),
    ;;     oder man wuerde aus Faulheit Ausdrucksmaechtigkeit verlieren.
    ;;     Das geht allerdings auch bald schon fließend in die Ontologie von Problemklassen 
    ;;     über---alledings hat man es dort ggf. mit ganz anderen Klasseneinteilungen zu tun. Deshalb ist es
    ;;     gut, die Typdeklaration von der Taktikdeklaration zu trennen!
    (extract-chirotope c csp))

#|
(def-tool ("check-GP-relations" (chr chirotope))
    :documentation "testet kombinatorische Struktur des chirotops"
    :requires "translators/matroids.lisp"

    :result-type :proper-decision
    ODER:
    :type :proper-decision-method
    ^^^^^^^ 
    Typ der Funktion versus Typ des Rückgabewertes: am besten irgendwie integrieren, damit man eine Kurzfassung für 
    Entscheidungsmethode haben kann, "geht nach Bool" ist da 
    
)
|#


(def-tool ("translate" (c (calculus "2d-Block Algebra")) (csp constraint-network c) "rcc8")
    :documentation "mereotopological interpretation of a block algebra csp"
    :requires      ("translators/simple-rewrite.lisp" "translators/block->rcc.lisp")
    (block->rcc8 csp))

(def-tool ("translate" (c (calculus "2d-Block Algebra")) (csp constraint-network c) "rcc5")
    :documentation "mereotopological interpretation of a block algebra csp"
    :requires      ("translators/simple-rewrite.lisp" "translators/block->rcc.lisp")
    (block->rcc5 csp))

;; use Lisp's logical pathnames if loading external code to warrant portability:
;; - "SparQ:Lib" will be translated to your SparQ directory "Lib"
;; - separate sub-directories using ";" instead of "/"

(require :geometry "SparQ:Lib;qualifiers;geometry.lisp")

;;;
;;; QUALIFY : mapping geometric objects to qualitative descriptions
;;;

(def-tool ("qualify" (c (calculus rcc8)) (option (member first2all all)) (polys (sparq:list-of simple-polygon)))
    :documentation "Determines RCC8 relations holding between 2d polygons"
    :requires      "qualifiers/rcc.lisp"
    (qualify-rcc8-from-polygons c option polys))

(def-tool ("qualify" (c (calculus rcc5)) (option (member first2all all)) (polys (sparq:list-of simple-polygon)))
    :documentation "Determines RCC5 relations holding between 2d polygons"
    :requires      "qualifiers/rcc.lisp"
    (qualify-rcc5-from-polygons c option polys))

;; (trace sparq:parse-primitive) ; *debug only*

(def-tool ("nop")
   :documentation "computes meaningful answer"
   42)

(def-tool ("cdc-reasoning" (c calculus) "check" (csp constraint-network c :allow-multiple-definitions? t))
    :documentation "uses Sianjing's code to check consistency of CDC networks"
    :requires      "cdc.lisp"
    (check-atomic-cdc-csp csp))

(def-tool ("cdc-reasoning" (c calculus) "check-composition")
    :documentation "uses Sianjing's code to verify CDC composition table"
    :requires      "cdc.lisp"
    (check-cdc-composition c))

(def-tool ("cdc-reasoning" (c calculus) "check-comp-entry" (s1 t) (s2 t))
    :documentation "uses Sianjing's code to verify CDC composition table"
    :requires      "cdc.lisp"
    (check-cdc-composition-single c s1 s2))

(def-tool ("cdc-reasoning" (c calculus) "check-converse")
    :documentation "uses Sianjing's code to verify CDC converse table"
    :requires      "cdc.lisp"
    (check-cdc-converse c))

(def-tool ("translate" (ba (calculus "2d-Block Algebra")) (csp constraint-network ba) (rcd (calculus "RCD")))
    :documentation "maps a block-algebra csp to rcd (coarsening occurs)"
    :requires      "rcd.lisp"
    (translate-ba->rcd ba csp rcd))

(def-tool ("translate" (rcd (calculus "RCD")) (csp constraint-network rcd) (ba (calculus "2d-Block Algebra")))
    :documentation "maps a block-algebra csp to rcd (coarsening occurs)"
    :requires      "rcd.lisp"
    (translate-rcd->ba rcd csp ba))

(def-tool ("prune" (c calculus) (csp constraint-network c))
          :documentation "prunes away relations from a CSP that can be reconstructed by a-closure"

  (let* ((a2 (csp:algebraic-closure c csp))
	 (relrep (calculi:calculus-relation-representation c))
	 (u (relations:relation-representation-universal-relation relrep))
	 (decoder (relations:relation-representation-decoder relrep))
	 (m  (csp::constraint-network-matrix a2))
	 (objs (csp::constraint-network-objects a2))
	 (n  (array-dimension m 0)))
    (loop for i from 0 to (- n 1) do
	 (loop for j from 0 to (- i 1) do
	      (loop for k from 0 to (- n 1) do
		   (let ((rij (calculi:composition c (aref m i k) (aref m k j))))
		     (when (equal rij (aref m i j))
		       ;(format t "~% relation ~a->~a=~a ist ueberfluessig~%" i j rij)
		       (setf (aref m i j) u
			     (aref m j i) u))))))

    (loop for i from 0 to (- n 1) nconcing
	 (remove () (loop for j from 0 to (- i 1) collecting
			 (unless (eq u (aref m i j))
			   (list (nth i objs) (ofunc:ofuncall decoder relrep (aref m i j)) (nth j objs))))))))

;;;
;;; QUANTIFIER : computing a domain instantiation from a qualitative constraint network
;;;

(def-tool ("quantify" (pc (calculus "1D Point Calculus (PC)")) (csp constraint-network pc))
  :documentation "computes model for point-calculus network"
  :requires      "quantifier.lisp"
  (pc-model pc csp))

(def-tool ("quantify" (allen (calculus "Allen's Interval Algebra (AIA IA)")) (csp constraint-network allen))
  :documentation "computes model for Allen network"
  :requires      "quantifier.lisp"
  (allen-model allen csp))

(def-tool ("quantify" (c (calculus "Cardinal direction calculus (cardir)")) (csp constraint-network c))
  :documentation "computes model for cardir network"
  :requires      "quantifier.lisp"
  (cardir-model c csp))

(def-tool ("quantify" (ba (calculus "2d-Block Algebra")) (csp constraint-network ba))
  :documentation "computes model for block-algebra network"
  :requires      "quantifier.lisp"
  (block-algebra-model ba csp))