;;; This file is part of SparQ, a toolbox for qualitative spatial reasoning.
;;; Copyright (C) 2006, 2007 SFB/TR 8 Spatial Cognition, Diedrich Wolter, Project R3-[Q-Shape]
;;; See http://www.sfbtr8.spatial-cognition.de/project/r3/sparq/

;;; SparQ is free software and has been released under the terms of the GNU
;;; General Public License version 3 or later. You should have received a
;;; copy of the GNU General Public License along with this program. If not,
;;; see <http://www.gnu.org/licenses/>.
;;;

;;;
;;;
;;; 'Main' function of SparQ - dispatcher
;;; 

;; Change history (most recent first):
;; 2023-11-28 DW   fixed recognizing and handling terminal output
;; 2011-11-09 DW   new batch mode added
;; 2010-04-20 DW   migrating to new command architecture
;; 2010-02-08 JOW  included neighborhood-reasoning support
;; 2006-10-27 DW   after 0.6 branch is split of we're heading towards 0.7: new calculi & relation representation
;

(in-package :sparq)

(defvar *batch-file* ()
  "if non-nil stores the pathname of a batch file to execute after startup")

(defun run-batch-file ()
  (when *batch-file*
    (with-open-file (batch *batch-file*)
      (debug-out 0 "opening batch file ~a" (namestring *batch-file*))
      (loop while (listen batch) do
	   (let ((cmd (read-line batch nil +nothing+)))
	     (unless (eq +nothing+ cmd)
	       (let ((cmd-line (make-string-input-stream cmd))
		       (cmd/args ()))
		 (let ((*readtable* *sparq-readtable*))
		   (loop while (listen cmd-line) do
			(let ((nxt (read cmd-line nil +nothing+)))
			  (unless (eq +nothing+ nxt)
			    (push nxt cmd/args)))))
		 ;; process and print
		 (debug-out 1 "invoking '~{~a ~}'" (reverse cmd/args))
		 (dolist (x (with-timing *sparq-io*
			      (multiple-value-list (dispatch-command (nreverse cmd/args)))))
		   (format *sparq-io* "~&")
		     (print-out "~a" x)))))))))

(defun run-test (file)
  "execute a file of commands for testing"
  (calculi:load-calculus-registry)
  (with-open-file (tests file)
    (let ((count 1)
	  (fails 0))
      (loop while (listen tests) do
	   (let (cmd reply output)
	     (loop until (or (eq cmd +nothing+)
			     (and (stringp cmd)
				  (not (string= cmd ""))
				  (not (char= #\; (char cmd 0))))) do
		  (setq cmd (read-line tests nil +nothing+)))
	     (setq reply (read-line tests nil +nothing+))
	     (when (and (stringp cmd)
			(stringp reply))
	       (print-out "~d: testing '~a'... " count cmd)
	       (let* ((*sparq-io* (make-string-output-stream))
		      (cmd-line (make-string-input-stream cmd))
		      (cmd/args ()))
		 (let ((*readtable* *sparq-readtable*))
		   (loop while (listen cmd-line) do
			(let ((nxt (read cmd-line nil +nothing+)))
			  (unless (eq +nothing+ nxt)
			    (push nxt cmd/args)))))
		 (setq cmd/args (nreverse cmd/args))
		 (debug-out 1 "~d: invoking: ~a" count cmd/args)
		 ;; process and print
		 (dolist (x (with-timing *sparq-io*
			      (multiple-value-list (ignore-errors (catch 'error (dispatch-command cmd/args))))))
		   (format *sparq-io* "~&")
		   (print-out "~a" x))
		 (setq output (get-output-stream-string *sparq-io*)))
	       (if (string= reply output)
		   (print-out "~%~d: OK~%" count)
		   (progn (print-out "~%~d: §b1§FAIL§b0§~%" count)
			  (debug-out 0 "~d: expected: ~a" count reply)
			  (debug-out 0 "~d: received: ~a" count output)
			  (incf fails)))
	       (incf count))))
      (decf count)
      (if (< 0 fails)
	  (progn (print-out "~%~d tests performed, §b1§~d test~:P failed§b0§.~%" count fails)
		 (unless *debug* (print-out "Turn on vervose mode (option -v [optional level]) to see details.~%")))
	  (print-out "~%~d tests performed, all tests passed.~%" count)))))


;; SparQ state

(defstruct sparq-state 
  (calculus nil)  ;; currently active calculus
  (results nil)   ;; keeps last 3 results
  (bindings nil)  ;; variable bindings
  (environment nil))

(defun variable-binding (state var)
  (let ((pair (assoc var (sparq-state-bindings state) :test #'(lambda (s1 s2) (equal (format nil "~(~a~)" s1) (format nil "~(~a~)" s2))))))
    (if pair
	(cdr pair)
	:unbound)))

(defun (setf variable-binding) (new-val state variable)
  (let ((pair (assoc variable (sparq-state-bindings state))))
    (if pair
	(setf (cdr pair) new-val)
	(push (cons variable new-val) (sparq-state-bindings state)))))

(defun activate-calculus (cmd-stream state stream)
  (let ((csym (let ((*readtable* sparq:*sparq-readtable*)) (read cmd-stream nil nil))) ;case-sensitive read since this might be a filename...
	(error nil)
	(calculus (sparq-state-calculus state)))
    (unless (symbolp csym)
      (signal-error "'~a' is no valid calculus specifier." csym))
    (if (eq csym 'cl-user::*)
	(setq error (if (not calculus) "No calculus loaded - use of '*' calculus identifier not allowed"))
	; Schoen waere, das Laden zu unterbinden, falls das Kalkuel schon geladen ist, also im current-state steht
	; Dafuer muesste man aber mit verschiedenen Namen Umgehen: DRA-24, dipole-coarse, "Dipol Relation Algebra (DRA)"
	; Das scheint etwas schwierig zu sein
	(unless nil ;(and calculus (string= (calculi:calculus-name calculus) (symbol-name csym)))
	  (progn
	    (setq error (catch 'error (prog1 nil (calculi:load-calculus (symbol-name csym)))))
	    (setq calculus calculi:*calculus*))))
    (if error
	(progn (format stream "An error occured: ~a" error)
	       nil)
	calculus)))

(defun split-string (string char)
  "returns a list of words by splitting string wherever char occurs - zero-length words are ignored"
  (let ((len (length string))
	(pos 0))
    (if (= len 0)
	()
	(progn
	  (loop while (and (< pos len) (char/= (char string pos) char)) do
	       (incf pos))
	  (if (= pos 0)
	      (split-string (subseq string 1) char)
	      (if (< pos len)
		  (cons (subseq string 0 pos) (split-string (subseq string (+ pos 1)) char))
		  (list (subseq string 0 pos))))))))

;; main function for performing single sparq commands in SparQ's interactive mode
(defun handle-command/interactive (command-line state)
  (let ((arg-expr ()))
    (multiple-value-bind (dummy error) (ignore-errors 
					 (let ((str (make-string-input-stream command-line))
					       (*readtable* *sparq-readtable*))
					   (loop while (listen str) do
						(let ((x (read str nil +nothing+)))
						  (unless (eq x +nothing+)
						    (when (and (symbolp x)
							       (char= #\$ (char (symbol-name x) 0))) ; variable?
						      (setq x (variable-binding state (intern (subseq (symbol-name x) 1)))))
						    (push x arg-expr))))))
      (declare (ignore dummy))
      (if error
	  (progn (print-out "§b1§ERROR:§b0§ Parse error: ~a" error)
		 state)
	  (let ((err (catch 'error (let ((result (multiple-value-list (dispatch-command (nreverse arg-expr)))))
				     (dolist (val result)
					(print-out "~a~%" val))
				     (finish-output *sparq-io*)
				     (return-from handle-command/interactive (value result))))))
	    (print-out "~&§b1§ERROR:§b0§ ~a" err)
	    :error)))))

(Defparameter *state* (make-sparq-state))

(defun sparq-interactive ()
  (setf *running-interactive* t)
  (format *sparq-io* ";; SparQ is at your service!")
  (run-batch-file)
  (let ((continue? t))
    (setf *state* (make-sparq-state))
    (loop while continue? do
	 (print-out "~%§b1§sparq>§b0§ ")
	 (finish-output *sparq-io*)
	 (let ((command ""))
	   (multiple-value-bind (dummy error) (ignore-errors (setq command (read-line *sparq-io* nil "")))
	     (declare (ignore dummy))
	     (when error
	       (format *sparq-io* "A reader error occured (e.g., unmatched paranthesis)~%")))
	   (debug-out 1 "Received request '~a'~%" command)
	   (if (and (stringp command)
		    (< 3 (length command))
		    (string= (string-downcase command) "quit" :end1 4))
	       (progn
		 (setq continue? nil)
		 (ignore-errors (format *sparq-io* "~%Tschuess!~%"))) ; ignore-errors since the stream may already be closed
	       (when (and (stringp command)
			  (string/= command ""))
		 (let ((result (handle-command/interactive command *state*)))
		   (unless (eq result :no-value)
		     (push result (sparq-state-results *state*))
		     (when (cddr (sparq-state-results *state*))
		       (setf (cddr (sparq-state-results *state*)) ())))))))))
  (ignore-errors (finish-output *sparq-io*))) ; stream may already be closed

(defun print-usage ()
  (write-line " Usage:")
  (write-line "      sparq [options] [command]")
  (write-line " Options:")
  (write-line "      -v / --verbose [optional level]  Turn on debugging output")
  (write-line "      -h / --help                      This help message")
  (write-line "      -i / --interactive               Start interactive mode")
  (write-line "      -p / --port                      Run sparQ via TCP/IP mode (interactive mode only)")
  (write-line "      --batch                          execute batch script after startup")
  (write-line " Some useful commands:")
  (write-line "       help [optional module]")
  (write-line "       compute-relation <CALCULUS> arg-1 ...")
  (write-line "       constraint-reasoning <CALCULUS> <COMMAND> arg-1 ...")
  (write-line "       qualify <CALCULUS> arg-1 ...")  
  (write-line "       quantify <CALCULUS> <OPTIONAL COMMAND> <SCENARIO>")  
  (write-line "       a-reasoning <CALCULUS> arg-1 ...")  
  (write-line "       export <CALCULUS> <format> ..."))

(defun posix-args->lisp (args)
  (let ((in (make-string-input-stream (format nil "~{~a ~}" args)))
        lisp-args
        (reading? t))
    (loop while reading? do
         (let ((sym (read in nil sparq:+nothing+)))
           (if (eq sym sparq:+nothing+)
               (setq reading? nil)
               (push sym lisp-args))))
    (nreverse lisp-args)))

(defun sparq-single-computation (args)
  (run-batch-file)
  (if (null args)
      (print-usage)
      (let ((arg-expr ()))
	(multiple-value-bind (dummy error) (ignore-errors (let ((*readtable* *sparq-readtable*)
								(str (make-string-input-stream (format nil "~{~a ~}" args)))
								(expr ()))
							    (loop until (eq +nothing+ (setq expr (read str nil +nothing+))) do
								 (push expr arg-expr))))
	  (declare (ignore dummy))
	  (if error
	      (signal-error "Parse error reading input")
	      (dolist (x (with-timing *sparq-io*
			   (multiple-value-list (dispatch-command (nreverse arg-expr)))))
		(format *sparq-io* "~&")
		(print-out "~a" x)))))))

(declaim (inline sfind))
(defun sfind (name list)
  (find name list :test #'string=)) 

(declaim (inline smember))
(defun smember (name list)
  (member name list :test #'string=))

(defun print-banner (stream)
  (format stream "~{;; ~a~%~};;~%;; SparQ ~a built using Lisp ~a~%;;~%"
	  '(""
	    "This is SparQ - SPAtial Reasoning done Qualitatively"
            "SparQ is free software, provided as is, with absolutely no warranty."
	    "(C) 2006-2014 Diedrich Wolter, SFB/TR 8"
            "See the file COPYING for details.")
          sparq:+sparq-version-name+ sparq:+built-info+))

#+SBCL
(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))

;; handler invoked when something goes truly wrong...
(defun sparq-quit (condition hook)
  (declare (ignore hook))
  (ignore-errors (close *sparq-io*))
  (unless sparq:*debug*
    (if (typep condition 'simple-condition)
	(print-out "~&--- abnormal exit (reason ~a) ---~%" (apply #'format (append (list nil (simple-condition-format-control condition))
										  (simple-condition-format-arguments condition))))
	(print-out "~&--- abnormal exit (reason ~a) ---~%" (type-of condition)))
    (finish-output *sparq-io*)
    (sb-ext:quit)))

(defun main ()
  (setf *debugger-hook* #'sparq-quit
	*random-state* (make-random-state t))
  (get-cpu-info)
  (cond ((and (string= (sb-ext:posix-getenv "TERM") "xterm" :start1 0 :end1 5)
	      (string= (sb-ext:posix-getenv "TERM") "color" :start1 (- (length (sb-ext:posix-getenv "TERM")) 5)))
	 (setf *print-mode* :xterm-color))
	((string= (sb-ext:posix-getenv "TERM") "xterm")
	 (setf *print-mode* :xterm)))
  (multiple-value-bind (dummy error) 
      (ignore-errors
	(progn
	  (let* ((args #+SBCL (cdr sb-ext:*posix-argv*)) ;(cdr (member "--end-toplevel-options" sb-ext:*posix-argv* :test #'string=)))
		 (error (catch 'error
			  ;; turn verbose mode on
			  (when (or (sfind "-v" args)
				    (sfind "--verbose" args))
			    (let* ((p (or (position "-v" args :test #'string=)
					  (position "--verbose" args :test #'string=)))
				   (x (read-from-string (nth (+ p 1) args))))
			      (if (integerp x)
				  (setq *debug-level* x
					args (delete (nth (+ p 1) args) args))
				  (setq *debug-level* 0)))
			    (setq args (delete "--verbose" (delete "-v" args :test #'string=) :test #'string=)
				  *debug* t))

			  ;; turn timing mode on
			  (when (or (sfind "-t" args)
				    (sfind "--timing" args))
			    (setq args (delete "--timing" (delete "-t" args :test #'string=) :test #'string=)
				  *timing* t))

			  (when (sfind "--fancy" args)
			    (setq *print-mode* :xterm-color))

			  (when (sfind "--plain" args)
			    (setq *print-mode* :plaintext))

			  ;; set calculi directory
			  (when (or (sfind "-c" args) (sfind "--calculidir" args))
			    (let* ((calculi-arg (cdr (member-if #'(lambda (arg)
								    (or (string= "-c" arg)
									(string= "--calculidir" arg)))
								args)))
				   (cdirs (list "-c" "--calculidir" (car calculi-arg))) ; list of all calculi directories scanned plus args
				   (next-c-arg calculi-arg))
			      (loop while next-c-arg do
				   (setq next-c-arg (member-if #'(lambda (arg)
								   (or (string= "-c" arg)
								       (string= "--calculidir" arg)))
							       calculi-arg))
				   (when next-c-arg
				     (setq calculi-arg (cdr next-c-arg))
				     (push (car calculi-arg) cdirs)))
			      (let ((cdir (car calculi-arg)))
				(setf sparq::*root-pathname* (make-pathname :directory (list :relative cdir))))
				;; remove consumed args
				(setq args (delete "--calculidir" (delete "-c" args :test #'string=) :test #'string=))
				(dolist (cd cdirs)
				  (setq args (delete cd args)))))

			  ;; batch mode: execute some commands at startup
			  (when (sfind "--batch" args)
			    (let* ((batch-filename (cadr (member-if #'(lambda (arg) (string= arg "--batch")) args)))
				   (batch-file (probe-file (format nil "~a" batch-filename))))
			      (if (and batch-filename (not batch-file))
				  (signal-error "the batch file ~a cannot be found" batch-filename)
				  (setq *batch-file* batch-file))
			      (setq args (delete "--batch" (delete batch-filename args :test #'string=) :test #'string=))))
			  ;; test mode: execute a test script similar to batch mode
			  (when (sfind "--test" args)
			    (let* ((filename (cadr (member-if #'(lambda (arg) (string= arg "--test")) args)))
				   (file (probe-file (format nil "~a" filename))))
			      (if (and filename (not file))
				  (signal-error "test file ~a cannot be found" filename)
				  (progn (run-test file)
					 (sb-ext:quit)))))
			  
			  ;; go into interactive mode?
			  (cond ((or (sfind "-i" args)
				     (sfind "-d" args)
				     (sfind "--interactive" args)
				     (sfind "--daemon" args))
				 (let* ((tcp? (or (smember "-p" args)
						  (smember "--port" args)))
					(port (if tcp?
						  (read-from-string (cadr tcp?) nil nil))))
				   (calculi:load-calculus-registry)
				   (sparq:register-extensions)
				   (if (and (integerp port)
					    (< 127 port 65535))
				       (progn
					 (format *standard-output* "~%Awaiting incoming connection on TCP/IP port ~a..." port)
					 (finish-output *standard-output*)
					 (multiple-value-bind (dummy error?) 
					     (ignore-errors
					       (let ((tcp-stream #+OPENMCL (ccl:accept-connection
									    (ccl:make-socket :address-family :internet 
											     :type           :stream 
											     :connect        :passive
											     :local-port     port
											     :reuse-address  t))
								 #+SBCL (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
									  (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
									  (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
									  (sb-bsd-sockets:socket-listen socket 2)
									  (sb-bsd-sockets:socket-make-stream (accept socket) :input t :output t :element-type 'character))))
						 (when tcp-stream
						   (write-line "Connected!")
						   (finish-output)
						   (unwind-protect
							(let ((*sparq-io* tcp-stream))							  
							  (print-banner tcp-stream)
							  (sparq-interactive))
						     (close tcp-stream)))))
					   (declare (ignore dummy))
					   (when error?
					     (format *error-output* "~%Error opening TCP socket for listening (~a)." error?)
					     (finish-output *error-output*))))
				       (if tcp?
					   (signal-error "Cannot use port specified as '~a'~%" port)
					   (progn					     
					     (print-banner *standard-output*)
					     (sparq-interactive))))))
				
				;; print usage?
				((or (sfind "-h" args)
				     (sfind "--help" args)
				     (null args))
				 (print-usage))
				
				(t (calculi:load-calculus-registry)
				   (sparq:register-extensions)
				   (sparq-single-computation (member-if-not #'(lambda (arg) (char= #\- (char arg 0))) args))))
			  nil)))
	    (when error
	      (error-out "~%An §b1§error§b0§ occured: ~a" error)))
	  (when (and (streamp *sparq-io*)
		     (open-stream-p *sparq-io*))
	    (format *sparq-io* "~&")
	    (finish-output *sparq-io*))
	  (finish-output *error-output*)))
    (declare (ignore dummy))
    (when (and error
	       (open-stream-p *error-output*))
      (error-out "~%An unhandled §b1§internal error§b0§ occured - developer's fault, sorry. Error = ~a ~%" error)))
  (cl-user::quit))

(defun varname? (sym)
  (eq #\? (char (symbol-name sym) 0)))
; (and symbol (satisfies varname?))

(defcommand ("let" (var (or symbol (list-of symbol))) "=" &rest command)
  "binds variable to value, e.g., let csp = ((x < y))"
  (let ((result (if (listp (car command)) 
		    command
		    (multiple-value-list (dispatch-command command)))))
    (unless (listp var)
      (setq var (list var)))
    (mapc #'(lambda (var val)
	      (unless (equal "_" (symbol-name var))
		(setf (variable-binding sparq::*state* (symbol-name var)) val)))
	  var result)

;    (if (listp (car command))
;	command
	(values-list result)))

(defcommand ("print" (whatever t))
  "prints out data, typically used with variables (-> let)"
  whatever)

(defcommand ("verbosity" (x (or (member cl-user::on cl-user::off) fixnum)))
  "turns debugging messages on or off, use number 0...10 to control verbosity level (0 = off)"
  (setf *debug-level*
	(cond ((eq x 'cl-user::on) 1)
	      ((eq x 'cl-user::off) 0)
	      ((and (realp x)
		    (<= 0 x 10))
	       x)
	      (t *debug-level*))
	*debug* (< 0 *debug-level*)))
