;;; 
;;; 9 Intersection
;;;
;;; taken from Max J. Egenhofer: Deriving the Composition of Binary Topological Relations. J. Vis. Lang. Comput. 5(2): 133-149 (1994)
;;; base relations: d=disjoint; m=meet; e=equal; i=inside; cB=coveredBy; ct=contains; cv=covers; o=overlap


(def-calculus "9 Intersection Calculus with 8 base relations (9I)"
  :arity :binary
  :parametric? nil
  :consistency :scenario-consistency
  :qualifier #'(lambda (o1 o2)
		 (declare (ignore o1 o2))
		 (error "Cannot qualify 9I!"))
  :identity-relation e
  :converse-operation ((d d)
		       (m m)
		       (e e) 
		       (cv cB)
		       (cB cv)
		       (ct i)
		       (i ct)
		       (o o))  
  :base-relations (d m e i cB ct cv o)
  :composition-operation (
			  (d d    (d m e i cB ct cv o))
			  (d m    (d m i cB o))
			  (d e    (d))
			  (d i    (d m i cB o))
			  (d cB   (d m i cB o))
			  (d ct   (d))
			  (d cv   (d))
			  (d o    (d m i cB o))

			  (m d    (d m ct cv o))
			  (m m    (d m e cB cv o))
			  (m e    (m))
			  (m i    (i cB o))
			  (m cB   (m i cB o))
			  (m ct   (d))
			  (m cv   (d m))
			  (m o    (d m i cB o))

			  (e d    (d))
			  (e m    (m))
			  (e e    (e))
			  (e i    (i))
			  (e cB   (cB))
			  (e ct   (ct))
			  (e cv   (cv))
			  (e o    (o))

			  (i d    (d))
			  (i m    (d))
			  (i e    (i))
			  (i i    (i))
			  (i cB   (i))
			  (i ct   (d m e i cB ct cv o))
			  (i cv   (d m i cB o))
			  (i o    (d m i cB o))

			  (cB d    (d))
			  (cB m    (d m))
			  (cB e    (cB))
			  (cB i    (i))
			  (cB cB   (i cB))
			  (cB ct   (d m ct cv o))
			  (cB cv   (d m e cB cv o))
			  (cB o    (d m i cB o))

			  (ct d    (d m ct cv o))
			  (ct m    (ct cv o))
			  (ct e    (ct))
			  (ct i    (e i cB ct cv o))
			  (ct cB   (ct cv o))
			  (ct ct   (ct))
			  (ct cv   (ct))
			  (ct o    (ct cv o))

			  (cv d    (d m ct cv o))
			  (cv m    (m ct cv o))
			  (cv e    (cv))
			  (cv i    (i cB o))
			  (cv cB   (e cB cv o))
			  (cv ct   (ct))
			  (cv cv   (ct cv))
			  (cv o    (ct cv o))

			  (o d    (d m ct cv o))
			  (o m    (d m ct cv o))
			  (o e    (o))
			  (o i    (i cB o))
			  (o cB   (i cB o))
			  (o ct   (d m ct cv o))
			  (o cv   (d m ct cv o))
			  (o o    (d m e i cB ct cv o))

                         )

  :cnhs ( ((default def) ((d (m))
			  (m (d o))
			  (o (cB cv e m))
			  (cB (o e i))
			  (cv (o e ct))
			  (e (cB cv o i ct))
			  (i (cB e))
			  (ct (cv e)))
	   ))
)
