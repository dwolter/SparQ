;;; Absolute distance calculus with new ID relation "same"
;;;

(def-calculus "Absolute distance calculus with new ID relation 'same' (absdistcalculusPlusID)"
  :arity :binary
  :parametric? t ;; the parameter is interpreted as distance to differentiate close from far
  :basis-entity :2d-point
  :consistency :scenario-consistency
;  :qualifier (external-lib "TestLib.dylib" "test_compose")
  :qualifier #'(lambda (p1 p2)
		 (if (equal p1 p2)
		     'same
		     (if (< (point-distance2 p1 p2) (expt calculi:*calculus-parameter* 2))
			 'close
			 'far)))
  :converse-operation ((same same)
                       (close close)
                       (far far))
  :identity-relation same
  :base-relations (same close far)
  :composition-operation ((same same (same))
                          (same close (close))
                          (same far (far))
                          (close same (close))
                          (close close (same close far))
                          (close far (close far))
                          (far same (far))
                          (far close (close far))
                          (far far (same close far))))

