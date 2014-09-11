;;;
;;; QTC-B22: Qualitative Trajectory Calculus Basic 2D Level 2 with 27 base relations
;;;

(def-calculus "QTC-B22: Qualitative Trajectory Calculus Basic 2D Level 2 with 27 base relations"
  :arity :binary
  :parametric? nil
  ;;:qualifier (external-lib "libqtc.dylib" "qtc_b22_qualify")
  :basis-entity :dipole ;; 5-tuple with two dipoles, and relative velocity
  :base-relations #'(lambda ()  ;; Function to compute the base relations 4 times {+,0,-}
		      (let ((rels nil))
			(dolist (z1 '(- O +))
			  (dolist (z2 '(- O +))
			    (dolist (z3 '(- O +))
			      (push (intern (format nil "~a~a~a" z1 z2 z3 )) rels))))
			(nreverse rels)))
  :identity-relation OOO
  :converse-operation (:external-lib "libqtc.dylib" "qtc_b22_converse")
  :composition-operation (:external-lib "libqtc.dylib" "qtc_b22_composition")
  
)


