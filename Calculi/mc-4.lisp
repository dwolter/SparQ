;;; 
;;; Spatial Congruence Algebra MC-4
;;; Matteo Cristiani. The Complexity of Reasoning about Spatial Congruence.
;;; Journal of Artificial Intelligence Research (JAIR), 11:361--390, 1999
;;;

(def-calculus "MC-4 2d region congruence calculus"
  :arity :binary
  :parametric? nil
  :consistency :scenario-consistency
  :identity-relation cg
  :converse-operation ((cg cg)
		       (cgpp cgppi)
		       (cgppi cgpp)
		       (cno  cno))
  :base-relations (cg cgpp cgppi cno)
  :composition-operation ((cg    (cg    cg))
			  (cg    (cgpp  cgpp))
			  (cg    (cgppi cgppi))
			  (cg    (cno   cno))
			  (cgpp  (cg    cgpp))
			  (cgpp  (cgpp  cgpp))
			  (cgpp  (cgppi cg cgpp cgppi cno))
			  (cgpp  (cno   cgpp cno))
			  (cgppi (cg    cgppi))
			  (cgppi (cgpp  cg cgpp cgppi cno))
			  (cgppi (cgppi cgppi))
			  (cgppi (cno   cgppi cno))
			  (cno   (cg    cno))
			  (cno   (cgpp  cgpp cno))
			  (cno   (cgppi cgppi cno))
			  (cno   (cno   cg cgpp cgppi cno)))
  :tractable-subsets (
                      ;; maximal tractable subset M_72 :
                      ((cg) (cg cgpp) (cg cgppi) (cg cno) (cg cgpp cgppi) (cg cgpp cno) (cg cgppi cno) (cg cgpp cgppi cno))
                      ;; M_99:
                      ((cg) (cgpp) (cgppi) (cno) (cg cgpp) (cg cgppi) (cg cno) (cgpp cno) (cgppi cno) (cg cgpp cno) (cg cgppi cno) (cg cgpp cgppi cno))
                      )
)




