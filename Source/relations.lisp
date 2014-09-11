;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; SparQ relation representation
;;;

;; Change history (most recent first):
;; 2007-04-16 DW  fixed missing call to parse-relation when enocing lofn-relations
;; 2007-03-16 DW  fixed universal relation for LOFN
;; 2007-05-07 DW  added ordering
;; 2007-03-01 DW  added ofuncs for represenations using lists of fixnums
;; 2007-01-25 DW  added combiner; some cleanup
;; 2006-11-24 DW  adapting to ofunc-aliases
;; 2006-10-26 DW  clean-up and implementing some function for calculi with more base-rels than fit in a fixnum
;;                changed lookups to represent relations' encodings rather than their indices
;; 2006-10-25 DW  changed callables to callable-code in ofunc definitions
;; 2006-10-17 DW  added iterator for general relations
;; 2006-10-13 DW  introduces ofunc code struct
;; 2006-10-12 DW  started file by copying parts from the old calculi.lisp and added compiler forms, etc.

(defpackage :relations
  (:use :common-lisp :sparq :ofunc)
  (:export :make-representation                        ;; function: converts list of base-relations into relation-representation
	   :relation-representation-unite              ;; ofunc: computes union of two relations
	   :relation-representation-intersect          ;; ofunc: computes intersection of two relations
	   :relation-representation-minus              ;; ofunc: computes set-theoretic subtraction of two relations
	   :relation-representation-encoder            ;; ofunc: list of symbols ->relation
	   :relation-representation-decoder            ;; ofunc: relation->string
	   :relation-representation-printer            ;; writes relation to a stream
	   :relation-representation-empty-relation?    ;; ofunc: test for the empty relation
	   :relation-representation-<                  ;; symbol: ordering function for relations
	   :relation-representation-same-relation?     ;; ofunc: test for equivalence of relations
	   :relation-representation-empty-relation     ;; constant: empty relation (either 0 or '(0 0 ... 0))
	   :relation-representation-mapper             ;; ofunc: maps a function over the base-relations' indices that make up some general relation
	   :relation-representation-universal-relation ;; constant: universal relation
	   :relation-representation-br-encodings       ;; vector: relation stored according to their index (in fixnum case 2^i)
	   :relation-representation-combine            ;; ofunc: combines two relations into a single index for lookup
	   :relation-representation-base-relations     ;; vector: base-relation print names
	   :relation-representation-num-base-relations ;; constant: # of base relations
	   :relation-representation-relation-table     ;; hash-table: relation print names as key, encoding as value
           :relation-representation-relation-idx-table ;; hash-table: relation print names as key, index as value
	   :with-all-general-relations                 ;; macro: iterates over all general relations in a relation-representation
	   :parse-relation))                           ;; (soon obsolete) function: handles wildcards, etc. - will be integrated in encode

(in-package :relations)

;; DOCUMENTATION:
;; ==============
;;
;; Relations are represented as bitfields, either using single fixnums
;; or using lists of fixnums. Each bit corresponds to a single base relation.
;; As it turns out this representation is much more efficient than directly
;; using Lisp's convenient bit-arrays - sigh!
;; There is one delicate difference: A relation's index is used when iterating
;; over relations (see below)!
;; Since the calculus compiler defined in calculi.lisp will select the appropriate
;; representation based on the decision by relation-representation we basically 
;; provide two sest of functions for either one of the underlying data formats.
;; Besides, there are some general functions that deal with parsing relations.
;;
;; There are basically five things we need to do with general relations:
;; - unite
;; - intersect
;; - iterate over base relations contained in a general relation
;;   this is somehow special: mapping is performed over the indices of
;;   the relations rather than relations themselves. This means, given
;;   a set of 8 base relations, any general relation is represented as
;;   bit field of 8 bits and if rel-i belongs to a general relation, the
;;   corresponding bit is set. However, in the context of iteration rel-i
;;   will be represented simply as number i (rather than by 2^i) as this is
;;   well-suited to the context of table lookups which make use the iterator.
;; - convert into a string (for printing)
;; - construct from a list of symbols (parsing)


;; The number of bits the Lisp system can stuff into a single fixnum (usually 29 or 60)
(defconstant +bits-per-fixnum+ (integer-length MOST-POSITIVE-FIXNUM))

;; The type SBCL wants to see to savely inline ash (Yes, I know that this is a hack and might
;; break in future releases of SBCL - can you do better?)
(deftype SHIFTTYPE ()
  `(unsigned-byte ,(floor (integer-length MOST-POSITIVE-FIXNUM) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   ;;;
;;; General functions ;;;
;;;                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expands a simple relation wrt. to base-relations
;; This is just a helper function for parse-relation further below
(defun expand-simple-relation (r base-relations signal-error?)
  "Expands an abbreviated base relation, e.g. ll?? -> (llll llrr llrl)" 
  (let* ((name (format nil "~:@(~a~)" r))
	 (l (length name)))
    (if (string= name "*")
	base-relations
	(let (pattern)
	  (dotimes (i l)
	    (let ((c (elt name i)))
	      (if (not (char= c #\?))
		  (push (cons i c) pattern))))
	  
	  (let ((matches (remove-if-not #'(lambda (rel)
					    (let* ((name (format nil "~a" rel))
						   (len (length name)))					      
					      (and (= l len)
						   (every #'(lambda (constraint)
							      (let ((pos (car constraint))
								    (chr (cdr constraint)))
								(and (< pos len)
								     (char= chr (elt name pos)))))
							  pattern))))
					base-relations)))
	    (if matches
		matches
		(if signal-error?
		    (sparq:signal-error "Notation ~w does not correspond to any relation in calculus, base relations are: ~{~a ~}~%" 
			   r
			   base-relations)
		    (values nil t))))))))

;; parse a relation from a given list of symbols as obtained by (read)-ing a relation specification
;; by Lisp. parse-relation expands the wild-cards and returns a list of symbols. This list will be
;; processed by the encoder of a relation representation to obtain the desired bit-field representation.
;; This function is mainly used by the specific encoders further below.
;; Parameters:
;; r               relation, either a symbol or a list of symbols
;; base-relations  list of base-relations (symbols)
;; signal-error?   boolean to denote whether an error should be reported to the user/shell
(defun parse-relation (r base-relations signal-error?)
  "Expands abbreviated universal relations, e.g. (ll?? llrr) -> (llll llrr llrl)" 
  (remove-duplicates (apply #'append (mapcar #'(lambda (r)
                                                 (expand-simple-relation r base-relations signal-error?))
                                             (if (listp r) r (list r))))))

;; Sets up a hash table to look up the position of a symbol in a list
(defun make-index-table (br-vector)
  (let ((ht (make-hash-table)))
    (dotimes (i (length br-vector) ht)
      (setf (gethash (svref br-vector i) ht) i))))
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       ;;;
;;; Fixnum representation ;;;
;;;                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro fixnump (x)
  #+SBCL `(sb-int:fixnump ,x)
  #+MCL  `(ccl:fixnump ,x))

(def-ofunc empty-rel?/fn (r)
  (eq 0 (the fixnum r)))

;; Intersection of general relations
(def-ofunc intersect/fn (r1 r2)
  (logand (the fixnum r1) (the fixnum r2)))

;; Intersection of general relations
(def-ofunc unite/fn (r1 r2)
  (logior (the fixnum r1) (the fixnum r2)))

(def-ofunc minus/fn (r1 r2)
  (logandc2 (the fixnum r1) (the fixnum r2)))

;; Printing to a stream
(def-ofunc print-relation/fn (rel-rep r stream)
  (let ((br-vector (relation-representation-base-relations rel-rep))
	(rel-index 0)
	(space? nil))
    ;; (declare (type fixnum rel-index))
    (write-char #\( stream)
    (loop until (eq r 0) do
	 (when (eq 1 (logand r 1))
	   (when space?
	     (write-char #\Space stream))
	   (setq space? t)
	   (format stream "~(~a~)" (svref br-vector rel-index)))
	 (setq rel-index (1+ rel-index)
	       r (ash r -1))))
  (write-char #\) stream))

;; Printing to a string
(def-ofunc relation-string/fn (rel-rep r)
  (with-output-to-string (s)
    (print-relation/fn rel-rep r s)))
  
;; Constructs the lookup for encode/fn, a hash-table that associates 
;; relation representation to relation symbol
(defun encoding-lookup/fn (base-relations)
  (let ((ht (make-hash-table))
	(i 1))
    (dolist (r base-relations ht)
      (setf (gethash r ht) i
	    i (ash i 1)))))

;; Converts a list of symbols to a fixnum representation
(def-ofunc encode/fn (rel-rep rel-or-list)
  (let ((lookup (relation-representation-relation-table rel-rep))
	(base-rels (coerce (relation-representation-base-relations rel-rep) 'list))
	(r 0))
    ;; (declare (type fixnum r)
    ;;    (optimize (speed 3) (safety 0)))
    (let ((list (if (listp rel-or-list) rel-or-list (list rel-or-list))))
      (dolist (rel-name list r)
	(dolist (expanded-rel (parse-relation rel-name base-rels t))
	  (let ((r2 (gethash expanded-rel lookup)))
	    ;;(declare (type (or null fixnum) r2))
	    (unless (fixnump r2)
	      (sparq:signal-error "'~a' is not a base relation" rel-name))
	    (setq r (logior r (the fixnum r2)))))))))

(def-ofunc/macro maprelation-idx/fn (fn relation)
  :function
  (let ((index 0))
    (declare (type fixnum index))
    (loop until (eq 0 relation) do
	 (when (eq 1 (logand relation 1))
	   (funcall fn index))
	 (setq relation (ash relation -1)
	       index (1+ index))))
  :macro
  (if (or (eq (car fn) 'function) 
	  (eq (car fn) 'lambda))      
      ;; inline a function #'(lambda (idx) ...)
      (let ((the-fn (cond ((and (eq (car fn) 'function)
				(eq (caadr fn) 'lambda)) ; (function (lambda ... ))
			   (second fn))
			  ((eq (car fn) 'lambda) ; (lambda ...)
			   fn)
			  (t (error "Can't handle function spec ~a while compiling maprelation-idx/fn macro.~%" fn)))))
	(let ((isym (car (second the-fn)))
	      (rsym (gensym)))
	  (unless (eq 1 (length (the list (second the-fn))))
	    (warn "Function passed to 'maprelation' expects not a single argument, lambda list is ~a." (second the-fn)))
	  `(let ((,isym 0)
		 (,rsym ,relation))
	     (declare (type fixnum ,isym ,rsym))
	     (loop until (eq 0 ,rsym) do
		  (when (eq 1 (logand ,rsym 1))
		    ,@(cddr the-fn)) ; stuff in function body
		  (setq ,rsym (ash ,rsym -1)
			,isym (1+ ,isym))))))
      ;; external function calls are not inlined
      (let ((isym (gensym))
	    (rsym (gensym)))
	(print "DAT WAARD EEN ORDINAER FUNCTION CALL!!!")
	`(let ((,isym 0)
	       (,rsym ,relation))
	     (declare (type fixnum ,isym ,rsym))
	     (loop until (eq 0 ,rsym) do
		  (when (eq 1 (logand ,rsym 1))
		    ,@fn)
		(setq ,rsym (ash ,rsym -1)
		      ,isym (1+ ,isym)))))))

(def-ofunc combine/fn (rel-rep r1 r2)
  (let ((combine-n (relation-representation-num-base-relations rel-rep)))
    (logior (the fixnum (ash (the SHIFTTYPE r1) (the SHIFTTYPE combine-n))) (the fixnum r2))))

(def-ofunc same-relation?/fn (r1 r2)
  (eq (the fixnum r1) (the fixnum r2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                ;;;
;;; list of fixnums representation ;;;
;;;                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructs the lookup for encode/lofn and - as a by-product - the
;; vector of relation representations that allows one to map a relation's index to 
;; its representation (used in conjuction with index-based mappers)
(def-ofunc encoding-lookup/lofn (base-relations nfixnums)
  (let ((ht (make-hash-table))
        enc-vec
	(i 0))
    (dolist (r base-relations (values ht (coerce (nreverse enc-vec) 'vector)))
      (setf (gethash r ht) (let ((tmp (make-list nfixnums :initial-element 0)))
                             (multiple-value-bind (pos bit) (floor i +bits-per-fixnum+)
                               (setf (nth pos tmp) (ash 1 bit))
                               (push tmp enc-vec)
                               tmp))
	    i (1+ i)))))

(def-ofunc encode/lofn (rel-rep list-or-rel)
  (let ((lookup (relation-representation-relation-table rel-rep))
	(base-rels (coerce (relation-representation-base-relations rel-rep) 'list))
	(er (relation-representation-empty-relation rel-rep)))
    (let ((r (copy-list er))
	  (list (if (listp list-or-rel) list-or-rel (list list-or-rel))))    
      (dolist (rel-name list r)
	(dolist (expanded-rel (parse-relation rel-name base-rels t))
	  (let ((r2 (gethash expanded-rel lookup)))
	    (declare (type list r2))
	    (unless r2
	      (error "'~a' is not a base relation" rel-name))
	    (do ((rr r (cdr rr))) ; same as (setq r (mapcar #'logior r r2)) but faster
		((null rr))
	      (setf (car rr) (logior (the fixnum (car rr))
				     (the fixnum (pop r2)))))))))))

(def-ofunc print-relation/lofn (rel-rep r str)
;;  (declare (optimize speed))
  (let ((rel-index 0)     ; Bit of the relation we're currently looking at   
        (i0 0)            ; First bit's index of the current fixnum
        (rr (first r))
	(br-vector (relation-representation-base-relations rel-rep))
	(space? nil))
    ;;(declare (type fixnum i0 rr rel-index))
    (write-char #\( str)
    (loop while r finally (write-char #\) str) do
          (if (eq 0 rr)
            (setq i0 (+ (the fixnum i0) +bits-per-fixnum+)
                  rel-index (the fixnum i0)
                  r (cdr r)
                  rr (the fixnum (if r (first r) 0)))
            (progn
              (when (eq 1 (logand rr 1))
	          (when space?
	            (write-char #\Space str))
	          (setq space? t)
	          (format str "~@:(~a~@:)" (svref br-vector rel-index)))
              (setq rel-index (1+ rel-index)
                    rr (ash rr -1)))))))

(def-ofunc relation-string/lofn (rel-rep r)
  (with-output-to-string (str)
    (print-relation/lofn rel-rep r str)))

(def-ofunc same-relation?/lofn (r1 r2)
  (equal r1 r2))

(def-ofunc empty-relation?/lofn (rel)
  (every #'(lambda (fn)
	     (declare (type fixnum fn))
	     (eq 0 fn))
	 rel))

; (def-ofunc/macro maprelation-ifx/lofn (fn relation)
;   :function

(def-ofunc maprelation-ifx/lofn (fn rel)
  (progn
    (let ((i 0)
	  (i0 0)
	  (r0  (first rel))
	  (r rel))
      ;;(declare (type fixnum i i0 r0)
      ;;	     (type fixnum r0)
      ;;	     (type list r))
      ;(format t "~% rel= ~a  :  " rel)
      (loop while r do
	   (if (eq r0 0)
	       (setq i0 (+ i0 +bits-per-fixnum+)
		     i i0
		     r (cdr r)
		     r0 (if r (car r) 0))
	       (progn
		 ;(format t " ~a " i)
		 (when (eq 1 (logand 1 r0))
		   (funcall fn i))
		 (setq i (1+ i)
		       r0 (ash r0 -1))))))))

(def-ofunc unite/lofn (r1 r2)
  (mapcar #'(lambda (fn1 fn2)
	      (declare (type fixnum fn1 fn2))
	      (logior fn1 fn2))
	  r1 r2))

(def-ofunc intersect/lofn (r1 r2)
  (mapcar #'(lambda (fn1 fn2)
	      (declare (type fixnum fn1 fn2))
	      (logand fn1 fn2))
	  r1 r2))

(def-ofunc minus/lofn (r1 r2)
  (mapcar #'(lambda (fn1 fn2)
	      (declare (type fixnum fn1 fn2))
	      (logandc2 fn1 fn2))
	  r1 r2))

(defun lofn-order (r1 r2)
  (declare (type list r1 r2)
	   (optimize (speed 3) (safety 0)))
  (or (null r1)
      (and r2 
	   (let ((r1head (car r1))
		 (r2head (car r2)))
	     (declare (type fixnum r1head r2head))
	     (or (< r1head r2head)
		 (and (eq r1head r2head)
		      (lofn-order (cdr r1) (cdr r2))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         ;;;
;;; Relation representation ;;;
;;;                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct relation-representation
  num-base-relations    ; number of base relations
  base-relations        ; vector of base relations
  relation-table        ; hash table for looking up a relation's representation given its symbol
  relation-idx-table    ; hash table for looking up a relation's index given its symbol
  br-encodings          ; vector that stores a relation's representation at the relation's index 
  empty-relation        ; representation of the empty relation
  universal-relation    ; representation of the universal
  empty-relation?       ; ofunc: test for empty relation
  same-relation?        ; ofunc: test for equivalence of two relations
  combine               ; ofunc: combines two relations into a single lookup value
  unite                 ; ofunc representation for uniting two general relations
  intersect             ; ofunc representation for intersecting two general relation
  minus                 ; ofunc representation for set-theoretic minus
  encoder               ; function for parsing and encoding a symbol-based (textual) relation specification
  decoder               ; function for converting a relation's representation into a string
  printer               ; function for printing a relation to a stream
  <                     ; ordering predicate for relations
  mapper)               ; ofunc representation of an iterator that iterates over base-relations in a general relation:
			; - the callable takes a function to be iterated and passes the relation's *INDEX* to the function
                        ; - the macro code for an iterator macro do-base-relation-idx that takes a body similar to
                        ;   dolist, also using the relation's *INDEX*

(defmethod make-load-form ((self relation-representation) &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots self))

;; Constructs the relation representation for a given list of base relations.
;; Parameters:
;; base-relations  list of symbols that represent all base-relations, e.g. (ec dc po ntpp ntppi tpp tppi eq)
(defun make-representation (base-relations)
  "Constructs a representation suitable to a set of base relations"
  (let* ((n (length base-relations))
         (br-vector (coerce base-relations 'vector))
         (idx-table (make-index-table br-vector)))
    (if (< n +bits-per-fixnum+)
	;; Case #1: general relations are fixnums
      (let ((br-indices (encoding-lookup/fn base-relations))) ; hash-table that maps symbols (e.g. 'ntpp) to their fixnum representation
        (make-relation-representation
         :relation-table     br-indices
         :relation-idx-table idx-table
         :num-base-relations n
         :base-relations     br-vector
         :br-encodings       (let ((tmp nil))
                               (dotimes (i n (apply #'vector (nreverse tmp)))
                                 (push (ash 1 i) tmp)))
         :empty-relation     0
	 :<                  '<
	 :same-relation?     'same-relation?/fn
	 :combine            'combine/fn
         :universal-relation (1- (expt 2 n))
         :empty-relation?    'empty-rel?/fn      ;(alias-ofunc empty-rel?/fn empty-relation?)
         :mapper             'maprelation-idx/fn ;(alias-ofunc maprelation-idx/fn maprelation-idx)
         :unite              'unite/fn           ;(alias-ofunc unite/fn unite)
         :intersect          'intersect/fn       ;(alias-ofunc intersect/fn intersect)
	 :minus              'minus/fn
         :encoder            'encode/fn          ;(alias-ofunc encode/fn encode)
         :decoder            'relation-string/fn ;(alias-ofunc relation-string/fn decode)
         :printer            'print-relation/fn  ;(alias-ofunc print-relation/fn print-relation)
	 ))
      ;; General relations are lists of fixnums


      ;;; NOTE: Representation for list-of-fixnums doesn't work yet since not adapted to new ofunc design


      (let ((fixnums (ceiling n +bits-per-fixnum+)))
        (multiple-value-bind (br-encodings enc-vector) (encoding-lookup/lofn base-relations fixnums)
          (make-relation-representation
           :relation-table     br-encodings
           :relation-idx-table idx-table
           :num-base-relations n
	   :<                  'lofn-order
           :base-relations     br-vector
           :br-encodings       enc-vector
           :empty-relation     (make-list fixnums :initial-element 0)	   
	   :same-relation?     'same-relation?/lofn
	   :empty-relation?    'empty-relation?/lofn
           :universal-relation (append (make-list (1- fixnums) :initial-element MOST-POSITIVE-FIXNUM)
				       (list (1- (expt 2 (- +bits-per-fixnum+
							    (- (* fixnums +bits-per-fixnum+) n))))))
	   :mapper             'maprelation-ifx/lofn
	   :unite              'unite/lofn
	   :intersect          'intersect/lofn
	   :minus              'minus/lofn
           :encoder            'encode/lofn
           :decoder            'relation-string/lofn
           :printer            'print-relation/lofn))))))

;;
;; simple macro to iterate over all general relations expressable in a relation-representation
;; Macro is used when precomputing lookups
;; Usage:
;; (with-all-general-relations (r rel-rep)
;;   (format "r=~a , CONV(r)=~a~%" r (CONVERSE r)))
(defmacro with-all-general-relations (r-spec &body body)
  (let ((r-var (first r-spec))
	(rel-rep (second r-spec)))
    `(progn
       (when (< 24 (relation-representation-num-base-relations ,rel-rep))
	 (error "Are you nuts iterating over all ~a general relation?! Must be!" (expt 2 (relation-representation-num-base-relations ,rel-rep))))
       (dotimes (,r-var (expt 2 (relation-representation-num-base-relations ,rel-rep)))
	 ,@body))))

