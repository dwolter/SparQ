;;; 
;;; Region Connection Calculus
;;;

(def-calculus "Region Connection Calculus with 8 base relations (RCC8)"
  :arity :binary
  :parametric? nil
  :consistency :scenario-consistency
  :qualifier #'(lambda (o1 o2)
		 (declare (ignore o1 o2))
		 (error "Cannot qualify RCC8!"))
  :identity-relation eq
  :converse-operation ((dc dc)
                      (ec ec)
                      (po po) 
                      (tpp tppi)
		      (tppi tpp)
                      (ntpp ntppi)
                      (ntppi ntpp)
                      (eq eq))  
  :base-relations (dc ec po tpp tppi ntpp ntppi eq)
  :composition-operation (
			  (eq eq    eq)
			  (eq dc    dc)
			  (eq ec    ec)
			  (eq po    po)
			  (eq tpp   tpp)
			  (eq ntpp  ntpp)
			  (eq tppi  tppi)
			  (eq ntppi ntppi)

			  (dc eq     dc)
			  (dc dc    (dc ec po tpp tppi ntpp ntppi eq) )
			  (dc ec    (dc ec po tpp ntpp))
			  (dc po    (dc ec po tpp ntpp))
			  (dc tpp   (dc ec po tpp ntpp))
			  (dc ntpp  (dc ec po tpp ntpp))
			  (dc tppi  dc)
			  (dc ntppi dc)

			  (ec eq    ec)
			  (ec dc    (dc ec po tppi ntppi))
			  (ec ec    (dc ec eq po tpp tppi))
			  (ec po    (dc ec po tpp ntpp))
			  (ec tpp   (ec po tpp ntpp))
			  (ec ntpp  (po tpp ntpp))
			  (ec tppi  (dc ec))
			  (ec ntppi dc)

			  (po eq    po)
			  (po dc    (dc ec po tppi ntppi))
			  (po ec    (dc ec po tppi ntppi))
			  (po po    (dc ec po tpp tppi ntpp ntppi eq))
			  (po tpp   (po tpp ntpp))
			  (po ntpp  (po tpp ntpp))
			  (po tppi  (dc ec po tppi ntppi))
			  (po ntppi (dc ec po tppi ntppi))

			  (tpp eq    tpp)
			  (tpp dc    dc)
			  (tpp ec    (dc ec))
			  (tpp po    (dc ec po tpp ntpp))
			  (tpp tpp   (tpp ntpp))
			  (tpp ntpp  ntpp)
			  (tpp tppi  (eq dc ec po tpp tppi))
			  (tpp ntppi (dc ec po tppi ntppi ))

			  (ntpp eq    ntpp)
			  (ntpp dc    dc)
			  (ntpp ec    dc)
			  (ntpp po    (dc ec po tpp ntpp))
			  (ntpp tpp   ntpp)
			  (ntpp ntpp  ntpp)
			  (ntpp tppi  (dc ec po tpp ntpp))
			  (ntpp ntppi (dc ec po tpp tppi ntpp ntppi eq))

			  (tppi eq    tppi)
			  (tppi dc    (dc ec po tppi ntppi))
			  (tppi ec    (ec po tppi ntppi))
			  (tppi po    (po tppi ntppi))
			  (tppi tpp   (eq po tpp tppi))
			  (tppi ntpp  (po tpp ntpp))
			  (tppi tppi  (tppi ntppi))
			  (tppi ntppi ntppi)

			  (ntppi eq    ntppi)
			  (ntppi dc    (dc ec po tppi ntppi))
			  (ntppi ec    (po tppi ntppi))
			  (ntppi po    (po tppi ntppi))
			  (ntppi tpp   (po tppi ntppi))
			  (ntppi ntpp  (eq po tpp ntpp tppi ntppi))
			  (ntppi tppi  ntppi)
			  (ntppi ntppi ntppi)
                         )

  :tractable-subsets (((DC) (EC) (DC  EC) (PO) (DC  PO) (EC  PO) (DC  EC  PO) (TPP) (DC  TPP) (EC  TPP)  (DC  EC  TPP) (PO  TPP) (DC  PO  TPP) (EC  PO  TPP) (DC  EC  PO  TPP) (NTPP) (DC  NTPP)(EC  NTPP) (DC EC NTPP)(PO  NTPP) (DC  PO  NTPP) (EC  PO  NTPP) (DC  EC  PO  NTPP) (TPP NTPP)(DC  TPP  NTPP) (EC  TPP  NTPP)  (DC  EC  TPP  NTPP)  (PO  TPP  NTPP) (DC  PO  TPP  NTPP) (EC  PO  TPP  NTPP) (DC  EC  PO  TPP  NTPP)(TPPI) (DC  TPPI ) (EC  TPPI ) (DC  EC  TPPI )(PO  TPPI )(DC  PO  TPPI )(EC  PO  TPPI )(DC  EC  PO  TPPI ) (PO  TPP  TPPI ) (DC  PO  TPP  TPPI ) (EC  PO  TPP  TPPI ) (DC  EC  PO  TPP  TPPI ) (PO  NTPP  TPPI ) (DC  PO  NTPP  TPPI ) (EC  PO  NTPP  TPPI ) (DC  EC  PO  NTPP  TPPI ) (PO  TPP  NTPP  TPPI ) (DC  PO  TPP  NTPP  TPPI ) (EC  PO  TPP  NTPP  TPPI ) (DC  EC  PO  TPP  NTPP  TPPI )(NTPPI ) (DC  NTPPI )(EC  NTPPI )(DC  EC  NTPPI )(PO  NTPPI )(DC  PO  NTPPI )(EC  PO  NTPPI )(DC  EC  PO  NTPPI ) (PO  TPP  NTPPI )(DC  PO  TPP  NTPPI )(EC  PO  TPP  NTPPI )(DC  EC  PO  TPP  NTPPI )(PO  NTPP  NTPPI )(DC  PO  NTPP  NTPPI )(EC  PO  NTPP  NTPPI )(DC  EC  PO  NTPP  NTPPI )(PO  TPP  NTPP  NTPPI )(DC  PO  TPP  NTPP  NTPPI )(EC  PO  TPP  NTPP  NTPPI )(DC  EC  PO  TPP  NTPP  NTPPI )(TPPI   NTPPI )(DC  TPPI   NTPPI )(EC  TPPI   NTPPI )(DC  EC  TPPI   NTPPI )(PO  TPPI   NTPPI )(DC  PO  TPPI   NTPPI )(EC  PO  TPPI   NTPPI )(DC  EC  PO  TPPI   NTPPI )(PO  TPP  TPPI   NTPPI )(DC  PO  TPP  TPPI   NTPPI )(EC  PO  TPP  TPPI   NTPPI )(DC  EC  PO  TPP  TPPI   NTPPI )(PO  NTPP  TPPI   NTPPI )(DC  PO  NTPP  TPPI   NTPPI )(EC  PO  NTPP  TPPI   NTPPI )(DC  EC  PO  NTPP  TPPI   NTPPI )(PO  TPP  NTPP  TPPI   NTPPI )(DC  PO  TPP  NTPP  TPPI   NTPPI )(EC  PO  TPP  NTPP  TPPI   NTPPI )(DC  EC  PO  TPP  NTPP  TPPI   NTPPI )(EQ)(DC  EQ)(EC  EQ)(DC  EC  EQ)(PO  EQ)(DC  PO  EQ)(EC  PO  EQ)(DC  EC  PO  EQ)(TPP  EQ)(DC  TPP  EQ)(EC  TPP  EQ)(DC  EC  TPP  EQ)(PO  TPP  EQ)(DC  PO  TPP  EQ)(EC  PO  TPP  EQ) (DC  EC  PO  TPP  EQ) (TPP  NTPP  EQ)(DC  TPP  NTPP  EQ)(EC  TPP  NTPP  EQ)(DC  EC  TPP  NTPP  EQ)(PO  TPP  NTPP  EQ)(DC  PO  TPP  NTPP  EQ)(EC  PO  TPP  NTPP  EQ)(DC  EC  PO  TPP  NTPP  EQ)(TPPI   EQ)(DC  TPPI   EQ)(EC  TPPI   EQ)(DC  EC  TPPI   EQ)(PO  TPPI   EQ)(DC  PO  TPPI   EQ)(EC  PO  TPPI   EQ)(DC  EC  PO  TPPI   EQ)(PO  TPP  TPPI   EQ)(DC  PO  TPP  TPPI   EQ)(EC  PO  TPP  TPPI   EQ)(DC  EC  PO  TPP  TPPI   EQ)(PO  TPP  NTPP  TPPI   EQ)(DC  PO  TPP  NTPP  TPPI   EQ)(EC  PO  TPP  NTPP  TPPI   EQ)(DC  EC  PO  TPP  NTPP  TPPI   EQ) (TPPI   NTPPI   EQ)(DC  TPPI   NTPPI   EQ)(EC  TPPI   NTPPI   EQ)(DC  EC  TPPI   NTPPI   EQ)(PO  TPPI   NTPPI   EQ)(DC  PO  TPPI   NTPPI   EQ)(EC  PO  TPPI   NTPPI   EQ)(DC  EC  PO  TPPI   NTPPI   EQ)( PO  TPP  TPPI   NTPPI   EQ)(DC  PO  TPP  TPPI   NTPPI   EQ)(EC  PO  TPP  TPPI   NTPPI   EQ)(DC  EC  PO  TPP  TPPI   NTPPI   EQ)(PO  TPP  NTPP  TPPI   NTPPI   EQ)(DC  PO  TPP  NTPP  TPPI   NTPPI   EQ)(EC  PO  TPP  NTPP  TPPI   NTPPI   EQ)(DC  EC  PO  TPP  NTPP  TPPI   NTPPI   EQ)))

  :cnhs ( ((default def) ((DC (EC))
			  (EC (DC PO))
			  (PO (TPP TPPI EQ EC))
			  (TPP (PO EQ NTPP))
			  (TPPI (PO EQ NTPPI))
			  (EQ (TPP TPPI PO NTPP NTPPI))
			  (NTPP (TPP EQ))
			  (NTPPI (TPPI EQ)))
	   ))
)
