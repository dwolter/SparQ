;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; SparQ package for importing and exporting calculi definitions
;;;

;; Change history (most recent first):
;; 2007-07-12  DW merged JOW's SparQ 0.6 code for exportingg QRT style XML into this
;; 2007-01-31  DW updated to new ofunc representation
;; 2006-11-02  DW adapted to new relation representation in SparQ V0.7
;; 2006-10-04  DW initial version

(defpackage :interface
  (:use :common-lisp :sparq :relations)
  (:nicknames :export :import)
  (:export :export-calculus))

(in-package :interface)
  

;; gqr-export for the Freiburger reasoner (aka Guennis qualitativer reasoner)
;; writes two files - one containing the composition and one containing the 
;; converse table. Ideally, we'd provide a heuristic weighting too. This will be
;; done as soon we use heuristics ourselfes (or little later ;-)

(defun gqr-export (stream calculus filename)
  "Exports to 'the Freiburger reasoner' GQR"
  (declare (ignore stream))
  (setq *print-length* nil) ; Just to be sure SBCL won't linebreak...
  (let* ((rel-rep (calculi:calculus-relation-representation calculus))          ; the relation representation used for calculus
	 (decoder (relation-representation-decoder rel-rep))                    ; function that converts a relation to a string
	 (encoder (relation-representation-encoder rel-rep))                    ; function that constructs a representation from a (list of) symbol(s)
	 (ur  (coerce (relation-representation-base-relations rel-rep) 'list))) ; the universal relation
    (flet ((decode (r)
	     (let ((str (ofunc:ofuncall decoder rel-rep r)))
	       (subseq  str 1 (position #\) str))))
	   (encode (obj)
	     (ofunc:ofuncall encoder rel-rep obj)))
      ;; Export Composition table
      (with-open-file (out (concatenate 'string filename ".comp") :direction :output :if-exists :supersede)
	(dolist (r1 ur)
	  (dolist (r2 ur)
	    (format out "~(~a~) : ~(~a~) :: ( " r1 r2 )
	    (ofunc:ofuncall (relations:relation-representation-mapper rel-rep) 
			      #'(lambda (idx)
				  (format out "~(~a~) " (nth idx ur)))
			      (calculi:composition calculus (encode r1) (encode r2)))
	    (format out ")~%"))))
    
    ;; Export converse table
      (with-open-file (out (concatenate 'string filename ".conv") :direction :output :if-exists :supersede)
	(dolist (r ur)
	  (format out "~(~a~) :: ~a~%" r (decode (calculi:converse calculus (encode r)))))))))

(defun qat-xml-export (stream calculus filename)
  "Exports calculus to QAT xml format"
  (declare (ignore stream))
  (if (eq (calculi:calculus-arity calculus) :ternary)
      (sparq:signal-error "Only binary calculi can be exported to this format.")
      (with-open-file (out (concatenate 'string filename ".xml") :direction :output :if-exists :supersede)
	(let* ((relrep (calculi:calculus-relation-representation calculus))
	       (baserels (coerce (relations:relation-representation-base-relations relrep) 'list))
	       (baserel-enc (coerce (relations:relation-representation-br-encodings relrep) 'list)))
	  (flet ((decode (r)
		   (let ((str (ofunc:ofuncall (relations:relation-representation-decoder relrep) relrep r)))
		     (subseq  str 1 (position #\) str)))))
	    (format out "<qualitativeAlgebra algebraName=\"~a\" baseRelationsArity=\"~a\" baseRelationsNumber=\"~a\">~%" 
		    (calculi:calculus-name calculus) 
		    (case (calculi:calculus-arity calculus) 
		      (:binary 2)
		      (:ternary 3))
		    (length baserels))
	    
	    ;; write base relations
	    (format out "~T<baseRelations>~%")
	    (dolist (r baserels)
	      (format out "~T~T<baseRelation baseRelationId=\"~(~a~)\" fullName=\"~(~a~)\"/>~%" r r))
	    (format out "~T</baseRelations>~%")
	    
	    ;; write converse
	    (format out "~T<permutations>~%")
	    (dolist (r baserel-enc)
	      (format out "~T~T<permutation baseRelationId=\"~a\" permutationId=\"~a\">~%"
		      (decode r)
		      (decode (calculi:converse calculus r))))
	    (format out "~T</permutations>~%")
	    
	  ;; write composition
	  (format out "~T<compositions>~%")
	  (dolist (r1 baserel-enc)
	    (dolist (r2 baserel-enc)
	      (format out "~T~T<composition>~%")
	      (format out "~T~T~T<compositionElement>\"~a\"</compositionElement>~%" (decode r1))
	      (format out "~T~T~T<compositionElement>\"~a\"</compositionElement>~%" (decode r2))
	      (ofunc:ofuncall (relations:relation-representation-mapper relrep) 
			      #'(lambda (idx)
				  (format out "~T~T~T<compositionElement>\"~(~a~)\"</compositionElement>~%" (nth idx baserels)))
			      (calculi:composition calculus r1 r2))
	      (format out "~T~T</composition>~%"))) 
	  (format out "~T</compositions>~%")
	  
	  (format out "</qualitativeAlgebra>~%"))))))


(defun export-calculus (stream calculus args)
  "Exports a calculus definition to some external format"
  (let ((type (first args))
	(filename (second args)))

    (when (and filename (symbolp filename)) ; Allow ommitting the >>"<< where possible
      (setq filename (symbol-name filename)))

    (unless (stringp filename)
      (signal-error "No filename specified in call to calculus export (read ~a).~%" (if filename (type-of filename) "nothing")))

    (cond 
      ((eq type 'cl-user::gqr) (gqr-export stream calculus filename))
      ((eq type 'cl-user::qat) (qat-xml-export stream calculus filename))
      (t (signal-error "Export format '~a' is unknown.~%" type)))))
	  

(defcommand ("export-calculus" (c calculi:calculus) "gqr" (filename t))
  (gqr-export *sparq-io* c (symbol-name filename))
  (make-instance 'silent-value :value nil))

(defcommand ("export-calculus" (c calculi:calculus) "sparq" (filename t))
  (when (and filename (symbolp filename)) ; Allow ommitting the >>"<< where possible
    (setq filename (symbol-name filename)))

  (unless (stringp filename)
    (signal-error "No filename specified in call to calculus export (read ~a).~%" (if filename (type-of filename) "nothing")))
  
  (let* ((relrep (calculi:calculus-relation-representation c))
         (n     (relations:relation-representation-num-base-relations relrep))
         (rels  (relations:relation-representation-br-encodings relrep))
         (names (relations:relation-representation-base-relations relrep)))
    (with-open-file (out filename :direction :output)
      (format out "~((def-calculus \"~a\"~% :arity :~a~% :parametric? nil~% :identity-relation ~a~% :base-relations ~a~% :consistency ~w~% :converse-operation ~a~% :composition-operation ~a)~%~)"
              (calculi:calculus-name c) 
              (calculi:calculus-arity c)
              (calculi:calculus-identity-relation c)
              (coerce (relations:relation-representation-base-relations relrep) 'list)
              (or (calculi:calculus-consistency-method c) :unknown)
              (loop for i from 0 to (- n 1) collecting
                (list (aref names i) 
                      (let ((tmp (ofunc:ofuncall (relations:relation-representation-decoder relrep) 
						 relrep  
						 (calculi:converse c (aref rels i)))))
			(subseq tmp 1 (- (length tmp) 1)))))
              (loop for i from 0 to (- n 1) nconcing
                (loop for j from 0 to (- n 1) collecting
                  (list (aref names i) (aref names j) 
                        (ofunc:ofuncall (relations:relation-representation-decoder relrep) 
                                  relrep 
                                  (calculi:composition c (aref rels i) (aref rels j))))))
	      (make-instance 'sparq:silent-value :value "done")))))