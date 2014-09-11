;;; RELDISTCALCULUS
;;;

(def-calculus  "Relative distance calculus (reldistcalculus)"
  :arity :ternary
  :parametric? nil
  :identity-relation same

  :basis-entity :2d-point
  :qualifier #'(lambda (p1 p2 p3)
		 (let ((d12 (point-distance2 p1 p2))
		       (d13 (point-distance2 p1 p3)))
		   (cond ((< d12 d13) 'closer)
			 ((> d12 d13) 'farther)
			 (t           'same))))

  :base-relations (same closer farther)

  :inverse-operation ((same (same closer farther))
                      (closer (same closer farther))
                      (farther (same closer farther)))
  
  :shortcut-operation ((same same)
		      (closer farther)
		      (farther closer))
  
  :homing-operation ((same (same closer farther))
                     (closer (same closer farther))
                     (farther (same closer farther)))

  :composition-operation ((same same (same closer farther))
                          (same closer (same closer farther))
                          (same farther (same closer farther))
                          (closer same (same closer farther))
                          (closer closer (same closer farther))
                          (closer farther (same closer farther))
                          (farther same (same closer farther))
                          (farther closer (same closer farther))
                          (farther farther (same closer farther))))
