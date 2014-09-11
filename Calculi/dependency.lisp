;;; 
;;; Dependency Calculus (Ragni, Scivos 2005) with 5 base relations
;;;

(def-calculus "Dependency Calculus with 5 base relations (DepCalc)"
  :arity :binary
  :parametric? nil
  :identity-relation =
  :converse-operation ((~  ~)
		       (^  ^)
		       (<  >)
		       (> <)
		       (=  =))  
  ;;:base-relations (eq dr po pp ppi)
  :base-relations   (=  ~  ^  <  >  )
  
;  :basis-entity :1d-point ;; 1D point resp. 1D interval
  :qualifier #'(lambda (&rest stuff) (declare (ignore stuff)) nil)
  :composition-operation (
			  (= =    =)
			  (= ~    ~)
			  (= ^    ^)
			  (= <    <)
			  (= >    >)

			  (~ =    ~)
			  (~ ~    (~ ^ < > = ))
			  (~ ^    (~ ^ < ))
			  (~ <    (~ ^ < ))
			  (~ >    ~)

			  (^ =    ^)
			  (^ ~    (~ ^ > ))
			  (^ ^    (= ~ ^ < > ))
			  (^ <    (^ < ))
			  (^ >    (~ ^ > ))
 
			  (< =    < )
			  (< ~    ~ )
			  (< ^    (~ ^ < ))
			  (< <    < )
			  (< >    (= ~ ^ < >))

			  (> =    >)
			  (> ~    (~ ^ > ))
			  (> ^    (^ > ))
			  (> <    (= ^ < >))
			  (> >    > )

                         ))
