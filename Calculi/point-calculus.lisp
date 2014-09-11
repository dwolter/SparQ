;;;
;;; 1D Point Calculus with 3 base relatioons
;;;

(def-calculus "1D Point Calculus (PC)"
  :arity :binary
  :parametric? nil
  :consistency :algebraic-closure
  :qualifier #'(lambda (p1 p2)
		 (let ((x (first p1))
		       (y (first p2)))
		   (cond ((< x y) '<)
			 ((> x y) '>)
			 (t '=))))
  :basis-entity :1d-point 
  :identity-relation =
  :converse-operation ((< > )
		       (= = )
		       (> < ))
  
  :base-relations ( < = > )
  :composition-operation (( < <        < )
			  ( < =        < )
			  ( < >        (< = >) )
			  
			  ( = <        < )
			  ( = =        = )
			  ( = >        > )
			  
			  ( > <        (< = >) )
			  ( > =        > )
			  ( > >        > ))

  :algebraic-specification ((< ((1 ((ax 1))) < (1 ((bx 1)))))
			    (= ((1 ((ax 1))) = (1 ((bx 1)))))
			    (> ((1 ((ax 1))) > (1 ((bx 1))))))

  :cnhs ( ((default def) ((< (=))
	  	     (= (< >))
		     (> (=)))))
	  
  )


