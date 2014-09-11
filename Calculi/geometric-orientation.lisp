;;; 
;;; Geometric Orientation Calculus based on P,+,O,-
;;;

(def-calculus "Geometric Orientation Calculus based on P,+,O,- (OriCalc)"
  :arity :binary
  :parametric? nil 
  :identity-relation P
  :converse-operation ((P  P)
		       (+  -)
		       (O  O)
		       (-  +))  

  :base-relations   (P + - O )
  
  :basis-entity :2d-oriented-point
  ;;:qualifier #'(lambda (&rest stuff) (declare (ignore stuff)) nil)
  :composition-operation (
			  (O O    (P))
			  (O +    (-))
			  (O P    (O))
			  (O -    (+))

			  (+ O    (-))
			  (+ +    (- O +))
			  (+ P    (+))
			  (+ -    (+ P -))

			  (P O    (O))
			  (P +    (+))
			  (P P    (P))
			  (P -    (-))

			  (- O    (+))
			  (- +    (+ P -))
			  (- P    (-))
			  (- -    (- O +))
			  ))
