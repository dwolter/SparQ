

;; Change History (most recent first):
;; 2007-10-31 DW   started SparQ file from some old source file

(defpackage optimizer
  (:use :common-lisp)
  (:export :*rho* :*sigma* :*tau-1* :*tau-2* :*tau-3* :goldstein :w-p-f :gradient-descent))

(in-package :optimizer)


;;; Der Optimierer
;;; 
;;; (optimize function derivative x &key (steps 1)) fuehrt steps Optimierungsschritte
;;; aus, ausgehend von Initialloesung x. Liefert die neue Liesung, ohne x selbst zu
;;; veraendern

(defparameter *rho* 0.2)		; war 0.2
(defparameter *sigma* 0.75)
(defparameter *tau-1* 9)
(defparameter *tau-2* 0.1)
(defparameter *tau-3* 0.5)

; Schrittweitenbestimmung

; Schrittweitenbestimmung unter Beruecksichtigung der sog. Goldstein-Bedingungen,
; nachzulesen in Geigers Optimierungsscript oder einem Optimierungsbuch

(defun goldstein (func func-0 deriv deriv-0)
  "Function for calculation of optimizing steps, only to be used as an argument to optimize."
  (declare (ignore deriv))
  (let ((alpha 0.0)
        (beta 1.0)
        (func-zero func-0)
        (deriv-zero deriv-0)
        (func-beta (funcall func 1.0)))	; keep consistent with beta from above!

    (loop while (< func-beta (+ func-zero (* (- 1 *rho*) beta deriv-zero))) do
      (setf alpha beta
            beta (+ beta beta)
            func-beta (funcall func beta)))
    (if (> func-beta (+ func-zero (* *rho* beta deriv-zero)))
      (let* ((gamma 0.0)
             (func-gamma 0.0)
             (deviding t))
        (loop while deviding do
          (setf gamma (* 0.5 (+ alpha beta))
                func-gamma (funcall func gamma)
                deviding nil)
          (cond ((< func-gamma (+ func-zero (* (- 1 *rho*) gamma deriv-zero)))
                 (setf alpha gamma
                       deviding t))
                ((> func-gamma (+ func-zero (* *rho* gamma deriv-zero)))
                 (setf beta gamma
                       deviding t))))
        (setf alpha gamma))
      (setf alpha beta))
    alpha))

; Schrittlweitenbestimmung nach Wolfe-Powell-Fletscher
; soll u.U. besser sein als Goldstein

(defun w-p-f (func func-0 deriv deriv-0)
  "Function for calculation of optimizing steps, only to be used as an argument to optimize."
  (let* ((mue (/ (- func-0) (* *rho* deriv-0)))
         (alpha-alt 0)
         (alpha (* 0.5 mue))
         (phi-alpha (funcall func alpha))
         (phi-0 func-0)
         (phi-alpha-alt phi-0)
         (phi-a 0)
         (deriv-alpha (funcall deriv alpha))
         (a 0)
         (b 0)
         (klammer-berechnen t))
    ;(format t "mue=~w  phi-0=~w  deriv-0 = ~w ~%" mue phi-0 deriv-0)
    (loop while klammer-berechnen do
     ;(format t "deriv-alpha = ~w  phi-alpha = ~w " deriv-alpha phi-alpha)
     (cond ((or (>= phi-alpha phi-alpha-alt)
                (> phi-alpha (+ phi-0 (* *rho* alpha deriv-0))))
            (setf a alpha-alt
                  b alpha
                  klammer-berechnen nil))
           ((<= (abs deriv-alpha) (- *sigma* deriv-0))
            (setf klammer-berechnen nil
                  b alpha))	; Funktion verhaelt sich komisch...
           ((>= deriv-alpha 0)
            (setf a alpha
                  b alpha-alt
                  klammer-berechnen nil))
           ((<= mue (- (* 2 alpha) alpha-alt))
            (setf alpha-alt alpha
                   alpha mue))
           (t (setf alpha-alt alpha
                    alpha (* 0.5 (+ (- (* 2 alpha) alpha-alt) (min mue (+ alpha (* *tau-1* (- alpha alpha-alt)))))))))
     (setf phi-alpha-alt phi-alpha
           phi-alpha (funcall func alpha)
           deriv-alpha (funcall deriv alpha)))
    ;(format t "wolfe-powell-feltcher: klammer= ~w,~w~%" a b)
    ; Start von Phase zwei, ein "schoenes" Element aus dem Intervall (a,b) auswaehlen...
    (setf klammer-berechnen t
          phi-a (funcall func a))
    (loop while klammer-berechnen do
      (setf alpha (* 0.5 (+ a (* *tau-2* (- b a)) b (- (* *tau-3* (- b a)))))
            phi-alpha (funcall func alpha)
            deriv-alpha (funcall deriv alpha))
      (if (or (> phi-alpha (+ phi-0 (* *rho* alpha deriv-0)))
              (>= phi-alpha phi-a))
        (setf b alpha)
        (if (<= (abs deriv-alpha) (- (* *sigma* deriv-0)))
          (setf klammer-berechnen nil)
          (if (>= (* (- b a) deriv-alpha) 0)
            (setf b a)
            (setf a alpha
                  phi-a phi-alpha)))))
    (* 0.5 (+ a b))))

             
; Rechnen mit Vektoren, die als ein 1-dim. Array daherkommen

; Teil 1: Die Addition
(defun add-vec (x y)
  (let ((res (make-array (length x))))
    (dotimes (i (length x))
      (setf (aref res i) (+ (aref x i) (aref y i))))
    res))

; Teil 2: Die skalare Multiplikation
(defun scalar-mult-vec (l x)
  (let ((res (make-array (length x))))
    (dotimes (i (length x))
      (setf (aref res i) (* l (aref x i))))
    res))

; Teil 3: Das Skalarprodukt
(defun scalar-prod (x y)
  (let ((res 0))
    (dotimes (i (length x))
      (incf res (* (aref x i) (aref y i))))
    res))

; Der Gradientenabstieg
;param deriv-0 is the threshold where we stop optimizing because the derivative is too small
(defun gradient-descent (function derivative x &key (steps 1) (info nil) (deriv-0 1) (step-calc #'goldstein))
  "Gradient descent optimizer. Whereas function denotes the function to optimize and derivative its derivative, x is 
   a initial soloution."
  (format info "~%starting optimization...~%")
  (let ((calls-2-func 0)
        (calls-2-deriv 0)
        (deriv-norm 0)
        (funktionswert 0)
        (i steps))
    (loop while (> i 0) do
      (decf i)
      (let ((d (scalar-mult-vec -1 (funcall derivative x))))
        (incf calls-2-deriv)
        (flet ((f-lin (z)
                 (progn
                   (incf calls-2-func)
                   (funcall function (add-vec x (scalar-mult-vec z d)))))
               (d-lin (z)
                 (progn
                   (incf calls-2-deriv)
                   (scalar-prod d (funcall derivative (add-vec x (scalar-mult-vec z d)))))))
	  (setf deriv-norm (scalar-prod d d)
		funktionswert (funcall function x))
          (when info (format info "~%here we are:       #(~{~8e ~})  f = ~f, ||Df||^2 = ~f~%step in direction: #(~{~8e ~}) " (coerce x 'list) funktionswert deriv-norm (coerce d 'list)))
	  
          (if (< deriv-norm deriv-0)
            (progn
              (setf i 0) ; unschoen, aber hilft
              (when info
                (format t ">> quitting, derivative's norm is too low! <<~%")))
            (let ((schrittweite (funcall step-calc #'f-lin funktionswert #'d-lin (- (scalar-prod d d)))))
              #|
(when (< 0 schrittweite 1e-8)
                (setf schrittweite 0.5))
|#
              (when info
                (format t "step size=~8e~%" schrittweite))
              (setf x (add-vec x (scalar-mult-vec schrittweite d))))))


#|
        (when info
          (format t 
                  "~w. step starts at =~{~8e ~} with f=~f, ||Df||^2 = ~f, calls to derivative: ~w, calls to the function:~w ~%"
                  (if (< deriv-norm deriv-0) 'last (- steps i))
                  (coerce x 'list)
                  funktionswert
                  deriv-norm
                  calls-2-deriv
                  calls-2-func))
|#
)))
  x)

