;;;
;;; QTC-C22: Qualitative Trajectory Calculus DoubleCross 2D Level 2 with 209 base relations
;;;

(def-calculus "QTC-C22*: Qualitative Trajectory Calculus DoubleCross 2D Level 2 with 209 base relations (no angle constraint compared to original QTC-C22)"
  :arity :binary
  :parametric? nil
  ;;:qualifier (external-lib "libqtc.dylib" "qualify_qtc_c22")
  :basis-entity :dipole ;; 5-tuple with two dipoles, and relative velocity (relative angle left out)
  :base-relations #'(lambda ()  ;; Function to compute the base relations 5 times {+,0,-}
		      (let ((rels nil)
			    (dellist '(-O-OO -O-O- -OOOO -OOO- -O+OO -O+O- O-O-+ O-O-O O-OO+ O-OOO O-O++ O-O+O OO-OO OO-O- OOO-+ OOO-O OOOO+ OOOO- OOO++ OOO+O OO+OO OO+O- O+O-+ O+O-O O+OO+ O+OOO O+O++ O+O+O +O-OO +O-O- +OOOO +OOO- +O+OO +O+O-)))
			(dolist (z1 '(- O +))
			  (dolist (z2 '(- O +))
			    (dolist (z3 '(- O +))
			      (dolist (z4 '(- O +))
				(dolist (z5 '(- O +))
				  ;;(dolist (z6 '(- O +))
				  ;;(push (intern (format nil "~a~a~a~a~a~a" z1 z2 z3 z4 z5 z6)) rels)))))))
				  ;; only push if not in dellist
				  (let ((rel (intern (format nil "~a~a~a~a~a" z1 z2 z3 z4 z5)))) ;; jetzt ist rel ein Symbol/Atom
				    (unless (find rel dellist)
				      (push rel rels)
				      )))))))
			    ;; or delete if in dellist
			    rels ))
  :identity-relation OOOOO
  :converse-operation (:external-lib "libqtc.dylib" "qtc_c22_converse")
  :composition-operation (:external-lib "libqtc.dylib" "qtc_c22_composition")
  
)
