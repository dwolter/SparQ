;;; 
;;; Region Connection Calculus with 5 base relations (RCC5)
;;;

(def-calculus "Region Connection Calculus with 5 base relations (RCC5)"
  :arity :binary
  :parametric? nil
  :consistency :scenario-consistency
  :identity-relation eq
  :converse-operation ((dr  dr)
		       (po  po)
		       (pp  ppi)
		       (ppi pp)
		       (eq  eq))  
  :base-relations (eq dr po pp ppi)
  :composition-operation (
			  (eq eq    eq)
			  (eq dr    dr)
			  (eq po    po)
			  (eq pp    pp)
			  (eq ppi   ppi)

			  (dr eq    dr)
			  (dr dr    (dr po pp ppi eq ))
			  (dr po    (dr po pp ))
			  (dr pp    (dr po pp ))
			  (dr ppi   dr)

			  (po eq    po)
			  (po dr    (dr po ppi ))
			  (po po    (eq dr po pp ppi ))
			  (po pp    (po pp dr))
			  (po ppi   (dr po ppi ))
 
			  (pp eq    pp )
			  (pp dr    dr )
			  (pp po    (dr po pp ))
			  (pp pp    pp )
			  (pp ppi  (eq dr po pp ppi))

			  (ppi eq   ppi)
			  (ppi dr   (dr po ppi ))
			  (ppi po   (po ppi ))
			  (ppi pp   (eq po pp ppi))
			  (ppi ppi  ppi )

                         )
  
  :tractable-subsets (  
		        ( 
			  (dr) (po) (dr po) (pp) (dr pp) (po pp) (dr po pp) (ppi) (dr ppi) (po ppi) (dr po ppi) (po pp ppi) (dr po pp ppi) (eq)
                          (dr eq) (po eq) (dr po eq) (pp eq) (dr pp eq) (po pp eq) (dr po pp eq) (ppi eq) (dr ppi eq) (po ppi eq) (dr po ppi eq)
                          (po pp ppi eq) (dr po pp ppi eq)
			)
#|			( 
                          (dr) (po) (dr po) (dr pp) (dr po pp)  (dr ppi) (dr po ppi) (dr pp ppi) (dr po pp ppi) (eq)  (dr eq) (po eq) (dr po eq)
                          (dr pp eq) (dr po pp eq) (dr ppi eq) (dr po ppi eq) (dr pp ppi eq)  (dr po pp ppi eq)
                        )

			(
			  (eq)  (dr eq) (po eq) (dr po eq) (pp eq) (dr pp eq) (po pp eq) (dr po pp eq) (ppi eq) (dr ppi eq) (po ppi eq) (dr po ppi eq)
			  (pp ppi eq) (dr pp ppi eq) (po pp ppi eq) (dr po pp ppi eq)
			)
			(
			  (pp) (ppi) (pp ppi) (dr pp ppi) (po pp ppi) (dr po pp ppi) (eq) (pp eq) (ppi eq) (pp ppi eq) (dr pp ppi eq) (po pp ppi eq)
			  (dr po pp ppi eq)
			)
|#
		     )
)




