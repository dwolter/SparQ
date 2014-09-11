;;;
;;; QTC-C21: Qualitative Trajectory Calculus DoubleCross 2D Level 1 with 81 base relations
;;;

(def-calculus "QTC-C21: Qualitative Trajectory Calculus DoubleCross 2D Level 1 with 81 base relations"
  :arity :binary
  :parametric? nil
  ;;:qualifier (external-lib "libqtc.dylib" "qualify_qtc_c21")
  :basis-entity :dipole ;; 4-tuple with two dipoles
  :base-relations #'(lambda ()  ;; Function to compute the base relations 4 times {+,0,-}
		      (let ((rels nil))
			(dolist (z1 '(- O +))
				(dolist (z2 '(- O +))
					(dolist (z3 '(- O +))
						(dolist (z4 '(- O +))
						  (push (intern (format nil "~a~a~a~a" z1 z2 z3 z4 )) rels)))))
			(nreverse rels)))
  :identity-relation OOOO
  :converse-operation (:external-lib "libqtc.dylib" "qtc_c21_converse")
  :composition-operation (:external-lib "libqtc.dylib" "qtc_c21_composition")
  
)


