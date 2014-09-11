;;;
;;; QTC-B21: Qualitative Trajectory Calculus Basic 2D Level 2 with 9 base relations
;;;

(def-calculus "QTC-B21: Qualitative Trajectory Calculus Basic 2D Level 2 with 9 base relations"
  :arity :binary
  :parametric? nil
  ;;:qualifier (external-lib "libqtc.dylib" "qtc_b21_qualify")
  :basis-entity :dipole ;; 4-tuple with two dipoles
  :base-relations #'(lambda ()  ;; Function to compute the base relations 2 times {+,0,-}
		      (let ((rels nil))
			(dolist (z1 '(- O +))
			  (dolist (z2 '(- O +))
			    (push (intern (format nil "~a~a" z1 z2 )) rels)))
			(nreverse rels)))
  :identity-relation OO
  :converse-operation (:external-lib "libqtc.dylib" "qtc_b21_converse")
  :composition-operation (:external-lib "libqtc.dylib" "qtc_b21_composition")
)


