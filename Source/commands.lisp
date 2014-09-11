;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2010 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;; This file contains SparQ's command handling structure. Mainly, we have one macro
;;; for defining user commands at any place in the code:
;;;   (defcommand ARGUMENT-LIST &body COMMAND-BODY)
;;; ARGUMENT-LIST is pretty much like a lambda-list for defmethod, except (a) that we have
;;; strings (downcased!) to indicate keywords that need to match and (b) we have dependent
;;; types, i.e., a type that depends on some other variable. Here's an example:
;;;   (defcommand ("compute-relation" (c calculus) (op (member converse complement)) (r relation c))
;;;     (calculi:DO-SOME-COMPUTATION))
;;; In this example, "r" is meant to be a relation that fits to the calculus "c". The command
;;; dispatcher will automatically figure out the right order in which to parse expressions, but
;;; you need to supply parsing functions for all types not defined in Lisp. Thus, the example
;;; requires us to implement parsing for a calculus and for a relation. Parsing is performed
;;; by a method "parse-primitive" that needs to specialized. For new types it might be convenient
;;; to subclass sparq:primitive which also allows for specialization of the print-function.
;;; Declare parsing primitives as EQL specializer, e.g.:
;;;   (defmethod ((c (eql 'calculus)) expr &rest stuff) ...)
;;; For dependent types such as relations depending on calculi do the following:
;;;   (defmethod ((r (eql 'relation)) expr &key (calculus c)) ...)
;;; When this method is invoked, the extra arguments (here: c) will be bound according to the
;;; command definition. 
;;; NB: To accomodate for case-senstive arguments (e.g., file names), SparQ will READ input
;;; in a case-sensitive manner!
;;;



;; Change history (most recent first):
;; 2012-11-27 DW  fix: reconize and respect keyword arguments to dependent type specializations; won't work with standard type specialization though
;; 2011-10-27 DW  extended SparQ's type system to allow for specializing calculi
;; 2011-10-25 DW  added macros for SparQ extensions
;; 2010-10-28 DW  fixed that non-existing arguments were happily accepted as empty lists
;; 2010-04-20 DW  first version of commands.lisp
;;

(in-package :sparq)


;;;
;;; Base class for objects handled by SparQ
;;; 
(defclass primitive ()
  ())

(defclass silent-value ()
  ((value :initarg :value
	  :reader silent-value)))

(defmethod print-object ((sv silent-value) stream)
  (declare (ignore sv stream))
  ;;(format stream "[werde ignorieren: ~a]" (value sv))
  )

(defmethod value (v)
  v)

(defmethod value ((sv silent-value))
  (silent-value sv))

;; catch-all parser for all Lisp builtin types
(defmethod parse-primitive (type expr &rest stuff)
  (sparq:debug-out 9 "trying to parse type '~w' with exp '~w'" type expr)
  
  ;;(declare (ignore stuff))
  ;;(format t "~%~w -> ~w ~w" (type-of stuff) (mapcar #'type-of stuff) (not (null (caar stuff))))
  (cond ((and (listp type)
	      (eq 'and (car type)))
	 (if (cdr type)
	     (let ((p1 (apply #'parse-primitive (cons (second type) (cons expr stuff))))
		   (err ()))
	       (format t "~%test auf '~a' ~a:~a~a" (second type) (not (null stuff)) stuff (type-of stuff))
	       (if (and (consp p1)
			(eq (car p1) :FAIL))
		   p1
		   (progn
		     (dolist (type2 (cddr type))
		       ;;(format t "~%test auf '~a'" type2)
		       (let ((test (apply #'parse-primitive (cons type2 (cons expr stuff)))))
			 (when (and (consp test)
				    (eq :FAIL (car test)))
			   (setq err test)
			   (return))))
		     (or err
			 p1))))))
#|
  (format t "~%parse-primitive catch-all: type= ~a, expr = ~a, stuff=~a" type expr stuff)
  (if (typep expr type)
      (format t " --->   ACCEPT~%")
      (format t " --->   REJECT~%"))
|#
	((and (listp type)
	      (eq 'or (car type)))
	 (let ((fails ())
	       (p ()))
	   (dolist (ty (cdr type))
	     (setq p (apply #'parse-primitive (cons ty (cons expr stuff))))
	     ;;(format t "~%p = ~a" p)
	     (if (and (consp p)
		      (eq (car p) :FAIL))
		 (push (cdr p) fails)
		 (progn
		   (setq fails ())
		   (return))))
	   (if fails
	       (cons :FAIL fails)
	       p)))
	
	((and (listp type)
	      (eq 'list-of (car type)))
	 (if (listp expr)
	     (let ((pres (mapcar #'(lambda (v)
				     (apply #'parse-primitive (cons (second type) (cons v stuff))))
				 expr)))
	       (if (find-if #'(lambda (x) (and (consp x) (eq (first x) :FAIL))) pres)
		   (cons :FAIL (mapcar #'cdr (remove-if-not #'(lambda (x) (and (consp x) (eq (first x) :FAIL))) pres)))
		   pres))
	     (cons :FAIL "is not a list")))
		 

	(t (let ((expr (read-from-string (format nil "~a" expr)))) ; we re-read here to get a case-INsensitive expr
	     ;;(format t "lisptype = ~w~%" type)
	     (if (typep expr type)
		 expr
		 (cons :FAIL (format nil "~w is no ~w" expr type)))))))

;  (trace parse-primitive)


(defmethod initialize-primitive (p)
  p)

(defmethod describe-primitive (p stream)
  (print-object p stream))

#|
(defmethod parse-primitive ((x (eql 'list-of)) expression &rest what)
  (format t "~%list-of expr=~a  what=~a~%" expression what)
  (if (listp expression)
      (let ((vals (mapcar (if (consp (caar what)) ;; split up additional type specializers, e.g. (list-of (calculus 'ff)) -> "list-of" + "(caluclus 'ff)"
			      #'(lambda (x) (parse-primitive (caaar what) x (cdaar what)))
			      #'(lambda (x) (parse-primitive (caar what) x)) )
			  expression)))
	(or (some #'(lambda (val)
		      (if (and (consp val)
			       (eq :FAIL (car val)))
			  (cons :FAIL (format nil "list parsing failed on an element: ~a" (cdr val)))))
		  vals)
	    vals))))
|#	      

;;;
;;; Commands: definition macro and dispatch
;;;

(defstruct command
  args           ; lambda-liste des Befehls, z.B. ("compute-relation" (c calculus) (op rel-operation) (rel relation c))
  parse-order    ; Reihenfolge in der die Argumente parsiert werden muessen (im obigen Bsp. kaeme 'rel' zwingen nach 'c')
  function       ; die Funktion, die den Befehl implementiert
  obsolete?      ; flag to signal that the command is obsolete -- it will be surpressed in the online help
  rest-args?     ; whether function accepts &rest parameter or not
  documentation) ; guess what...

(defparameter *commands* ()
  "List of all commands provided by SparQ")

;;
;; command definition macro
;; all SparQ commands are defined by this command
;;
(defmacro defcommand (arg-list &body body)
;;  (format t "~%arglist = ~a~%" arg-list)
  (let ((args-but-rest (loop for arg in arg-list while (or (listp arg)
							   (not (eq '&rest arg)))
			  collecting arg)))
    (mapc #'(lambda (a)
	      (if (symbolp a)
		  (warn "Possible error in command declaration of ~a: symbol ~a interpreted as constant; to disambiguate use strings for constants and '(~a t)' to refer to untyped variables"
			arg-list a a)))
	  args-but-rest)
    (let ((fname (gensym))
	  (lambda-list (append (mapcar #'(lambda (x) 
					   (if (listp x) 
					       (first x) 
					       x)) 
				       (remove-if #'(lambda (x) 
						      (or (stringp x)
							  (symbolp x)))
						  args-but-rest))
			       (member '&rest arg-list))))      
      `(progn
	 (defun ,fname ,lambda-list
	   ,@(if (stringp (first body))
		 (rest body)
		 body))
	 (push (make-command 
		:args ',(mapcar #'(lambda (arg)
				    (if (symbolp arg)
					(string-downcase (symbol-name arg))
					(if (stringp arg)
					    (string-downcase arg)
					    arg)))
				args-but-rest)
		;; order in which to parse args: starting of with constants, then followed by simple types,
		;; and finally dependend types should work out (we should use proper topological sorting here
		;; so we can discover any problems as, e.g., circular dependencies of the args)
		:parse-order ',(mapcar #'second (sort (mapcar #'list
							      args-but-rest
							      (loop for i from 0 to (length arg-list) collecting i))
						      #'<
						      :key #'(lambda (x) 
							       (if (listp (first x))
								   (length (first x))
								   0))))
		:function ',fname
		:rest-args? ',(not (null (member '&rest arg-list)))
		:documentation ,(if (stringp (first body))
				    (first body)
				    ""))
	       sparq::*commands*)))))

(defmacro defcommand* (arg-list &body body)
  `(progn
     (defcommand ,arg-list
       ,@body)
     (setf (command-obsolete? (first *commands*)) t)))

;;
;; Describes the command syntax of some command,
;; printing out all args and translating (some) Lisp types
;; 
;;
(defun describe-command (command &optional parse-output)
  (let ((perr-count 0)
	(parse-errors ()))
    (with-output-to-string (str)
      (dolist (a (command-args command))
	(let ((po (if parse-output
		      (pop parse-output))))
	  (when (and (consp po)
		     (eq (car po) :FAIL))
	    (if (cdr po)
		(progn (format str "§b1§(~d)" (incf perr-count))
		       (push (cdr po) parse-errors))
		(format str "§b1§")))
	  (if (consp a)
	      (format str "<~a>" (cond ((eq (second a) t) "PARAMETER")
				       
				       ((and (listp (second a)) ;; 'or' types give alternative types
					     (eq 'or (first (second a))))
					(let ((alts (cdr (second a))))
					  (if (find 'null alts)
					      (let ((alts (remove 'null alts)))
						(format nil "optional: [~a~{ | ~a~}]" (first alts) (cdr alts)))
					      (format nil "~a~{ | ~a~}" (first alts) (cdr alts)))))
				       
				       ((and (listp (second a)) ;; 'member' types enumerate constants... (usually)
					     (eq 'member (first (second a))))
					(let ((alts (cdr (second a))))
					  (format nil "~(~a~{ | ~a~}~)" (first alts) (cdr alts))))					   
				       (t (second a))))
	      (format str "~a" a))
	  (if (and (consp po)
		   (eq (car po) :FAIL))
	      (format str "§b0§ ")
	      (format str " "))))
      (when (command-rest-args? command)
	(format str "..."))
      (when parse-errors
	(setq perr-count 0)
	(mapc #'(lambda (pe)
		  (format str "~&(~d): ~a" (incf perr-count) pe))
	      (nreverse parse-errors))))))


;; simple ordered list insertion
(defun insert (lst obj <)
  (if (null lst)
      (list obj)
      (if (funcall < (car lst) obj)
	  (cons (car lst) (insert (cdr lst) obj <))
	  (cons obj lst))))

;;
;; Invokes the appropriate command for list of arguments
;; This is where the whole magic (er, all bugs?!) in command
;; handling are likely to be found...
;;
;; args: list of (read)-in input using case-sensitive READ!
;; 
(defun dispatch-command (args)
;;  (format t "~%args = ~w // ~w~%" args (mapcar #'type-of args))
  (let ((the-command ()) ;; 'the' command that best fits args: ((command args) ...)
	(all-commands (mapcar #'(lambda (c)
				  (list 0 (command-parse-order c) c () ()))
			      *commands*))
    ;; all-commands: 
    ;;(#mismatches #args_parsed args_to_parse command arg-bindings parser-results)
	(parse-cache (make-hash-table :test #'equal)))
    
    (loop while (and (null the-command)
		     (some #'(lambda (ac)
			       (second ac))
			   all-commands)) do
      ;; get next command to try matching against input
      (destructuring-bind (misfits parse-order c args-bound parser-results) (let ((next-cmd (find-if #'identity all-commands :key #'second)))
                                                                              (setq all-commands (delete next-cmd all-commands))
                                                                              next-cmd)
        (dolist (n parse-order)
          (pop parse-order)

          (if (and (null (nthcdr n args))
		   (< 0 misfits))
            ;; not enough arguments in input
            (progn
              (incf misfits)
              (push (cons :FAIL ()) parser-results)
              (return))
            ;; try to parse
            (let ((arg (nth n (command-args c))))
              ;(format t "~%Trying to parse ~w as type ~w while parsing ~w..." (nth n args) arg (command-args c))
              (if (stringp arg) ; constant
		  (if (string= arg (format nil "~(~a~)" (nth n args)))
		      (push arg parser-results)
		      (progn (incf misfits)
			     (push (cons :FAIL ()) parser-results)	
			     (return)))
		  (if (null (cddr arg)) ;; ** FIXME: ** This doesn't allow for extra keyword args like with "(x some-type :some-extra-keyword )" like it does for dependend-types 
		      ;; simple typed arg, (second arg) gives the type
		      (let ((parse-output (let* ((specialized-type? (and (listp (second arg))
									 (not (find-if #'(lambda (s)
											   (string= s (symbol-name (first (second arg)))))
										       '("MEMBER" "AND" "OR" "NOT" "SATISFIESP" "EQL" "LIST-OF")))))
						 (ptype (if specialized-type? (first (second arg)) (second arg)))
						 (pextra (if specialized-type? (cdr (second arg))))
						 (arg.type (list (second arg) (nth n args)))) ; use full arg list for caching parse results
					    ;(format t "  specialized-type? ~a; ptype = ~a~%" specialized-type? ptype)
					    (multiple-value-bind (cached-val cached?) (gethash arg.type parse-cache)
					      (if cached?
						  cached-val
						  (setf (gethash arg.type parse-cache) (parse-primitive ptype (nth n args) pextra)))))))
			(push parse-output parser-results)
			(if (and (consp parse-output)
				 (eq (car parse-output) :FAIL))
			    (progn (incf misfits)
				   (push (cons :FAIL ()) parser-results)
				   (return))
			    (push (cons (first arg) parse-output) args-bound)))
		      ;; dependent arg, setup all type parameters for parsing
		      (let ((extra-parameters ())
			    (ok t))
			;; collect all extra parameters, e.g. from (... (c calculus) ... (r relation c) ...) parsing relation requires 
			;; keyword :calculus to be available and bound to 'c'
			(loop for a in (cddr arg) until (keywordp a) do ;was: dolist (a (cddr arg)) 
			  (let ((val (assoc a args-bound)))
			    (if val
				(setq extra-parameters (nconc (list (intern (symbol-name (let ((tmp (second (find-if #'(lambda (par)
															 (and (listp par)
															      (eq (first par) a)))
														     (command-args c)))))
											   (if (listp tmp)  ; handle type declaration with extra args, e.g., (calculus 'flip-flop)
											       (first tmp)
											       tmp)))
									    sb-int:*keyword-package*) 
								    (cdr val))
							      extra-parameters))
				(progn (setq ok nil) ; stop when we failed to collect some parameter FIXME: we should generate a parse-fail warning too
				       (return)))))
			(if ok
			    (let ((parse-output (let ((arg.type (append (cons (second arg) (cons (nth n args) extra-parameters))
									(member-if #'keywordp arg)))) ; append any keyword args present
						  (multiple-value-bind (cached-val cached?) (gethash arg.type parse-cache)
						    ;(format t "~%arg.type = ~w~%try = ~w~%arg = ~w~%" arg.type (append arg.type (member-if #'keywordp arg)) arg)
						    (if (and nil cached?) ;; **FIXME**
							cached-val
							(setf (gethash arg.type parse-cache) (apply #'parse-primitive arg.type)))))))
			      (push parse-output parser-results)
			      (if (and (consp parse-output)
				       (eq (car parse-output) :FAIL))
				  (progn (incf misfits)
					 (return))
				  (push (cons (first arg) parse-output) args-bound)))
			    (progn (incf misfits)
				   (return)))))))))
        ;; when "successfully parsed the command":
        (if (= 0 misfits)
	    (setq the-command (list c args-bound))
	    (setq all-commands (insert all-commands 
				       (list misfits parse-order c args-bound parser-results)
				       #'(lambda (ac1 ac2)
					   (< (first ac1) (first ac2))))))))
    
    ;; DEBUG-OUTPUT:
    ;;(format t "~%LOOP EXITED:~%the-command = ~w~%all-commands = ~w" the-command all-commands)
    ;;(format t "~%CACHE: ~%")
    ;;(maphash #'(lambda (key val)
    ;;		 (format t "'~w'-->'~w'~%" key val))
    ;;	     parse-cache)
    
    ;; invoke command or signal error
    (if (null the-command) 
      ;; no matching commands
      (labels ((cnt-matches (ac)
                 (count-if #'(lambda (x)
                               (or (not (consp x))
                                   (not (eq :fail (car x)))))
                           (fifth ac))))
        (setq all-commands (sort all-commands #'(lambda (ac1 ac2)
                                                  (let ((m1 (cnt-matches ac1))
                                                        (m2 (cnt-matches ac2)))
                                                    (or (> m1 m2)
                                                        (and (= m1 m2)
                                                             (< (first ac1) (first ac2))))))))
        (let ((candidates (remove-if #'(lambda (ac)
                                         (or (/= (first ac) (first (first all-commands)))
                                             (/= (cnt-matches ac) (cnt-matches (first all-commands)))))
                                     all-commands)))
          (signal-error "No matching command found or erroneous input. Suggestions:~%~{~a~%~}" 
                        (mapcar #'(lambda (cand)
                                    (destructuring-bind (misfits parse-order c args-bound parser-results) cand
				      (declare (ignore misfits parse-order args-bound))
                                      (describe-command c
                                                        (mapcar #'second (sort (mapcar #'list 
										       (command-parse-order c)
										       (nreverse parser-results))
                                                                               #'<
                                                                               :key #'first)))))
                               candidates))))
      
      ;; found exactly one command: invoke its function  
      (APPLY (command-function (first the-command))
             (append (mapcar #'(lambda (args) ; set up lambda list args
                                 (cdr (assoc (first args) (second the-command))))
                             (remove-if-not #'listp (command-args (first the-command))))
                     (if (command-rest-args? (first the-command)) ;; only append remaining args if command function has &rest keyword
			 (nthcdr (length (command-parse-order (first the-command))) args)))))))

;;(defcommand ("compute-relation" (c calculus) "foo" (x t) (y (member foo bar)) (r relation c))
;;  (print (list c x y r)))

(defcommand ("help" (module (or null symbol)))
  "SparQ online help"
  (let ((cmds (remove-if (if module
			     (lambda (c)
			       (or (not (stringp (first (command-args c))))
				   (string/= (first (command-args c)) (format nil "~(~a~)" module))
				   (command-obsolete? c)))
			     (lambda (c)
			       (command-obsolete? c)))
			 *commands*)))
    (if cmds
	(reduce #'(lambda (accu cmd)
		    (concatenate 'string 
				 (format nil "§b1§~a§b0§~%  > ~a~%" (describe-command cmd) (command-documentation cmd)) 
				 accu))
		cmds
		:initial-value (if (or module (not sparq:*running-interactive*)) "" (format nil "§b1§quit§b0§~%  > quits SparQ")))
	"No such command.")))

(defmacro cl-user::def-tool (arg-list &body body)
  (let* ((doc-string "")
	 (externals ())
	 (code ()))
    (loop while body do
	 (cond ((eq (car body) :documentation)
		(check-type (second body) string)
		(setq doc-string (second body))
		(setq body (cddr body)))
	       ((member (car body) '(:require :requires))
		(let ((files (if (listp (second body))
				 (second body)
				 (list (second body)))))
		  (let ((missing-files (remove-if #'(lambda (x)
						      (probe-file (make-local-pathname "Lib" x)))
						  files)))
		    (assert (null missing-files) () "some files required by extension '~a' are missing: ~{~a ~}" (describe-command (make-command :args arg-list)) missing-files))
		  (setq externals (mapcar #'(lambda (x)
					      (probe-file (make-local-pathname "Lib" x)))
					  files)))
		(setq body (cddr body)))
	       (t (setq code body
			body ()))))
    `(sparq::defcommand ,arg-list
	 ,doc-string
       (mapc #'(lambda (p) (sparq:load/compile () () :pathname p)) ',externals)
       ;(eval '(progn ,@code))
       ,@code
       )))

(defun register-extensions ()
  "loads extensions.lisp"
  (load/compile "Lib" "extensions" :verbose nil))

(export '(defcommand defcommand* dispatch-command primitive parse-primitive describe-primitive silent-value value register-extensions list-of))


;;;
;;; some utilities for tool extensions (see Lib/extensions.lisp)
;;;

(in-package :cl-user)

(defmacro with-calculus (c &body code)
  `(flet ((relation (r)
	    (relation ,c r))
	  (relation->string (r)
	    (relation->string ,c r))
	  (relation= (r)
	    (relation= ,c r))
	  (converse (r)
	    (converse ,c r))
	  (composition (r1 r2)
	    (composition ,c r1 r2))
	  (unite (r1 r2)
	    (unite ,c r1 r2))
	  (intersect (r1 r2)
	    (intersect ,c r1 r2))
	  )
     ,@code))
