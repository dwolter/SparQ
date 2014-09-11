;;; 
;;; Algebra of binary relations (Röhrig)
;;; preliminary to CYCORD(X,Y)
;;;

(def-calculus "Algebra of binary relations (ABR)"
  :arity :binary
  :basis-entity :1d-point ;; orientation
  :parametric? nil
  ;;:qualifier (external-lib "libdcc.dylib" "abr_qualify")
  :identity-relation e
  :converse-operation (( e e)
		       ( l r)
		       ( o o)
		       ( r l))
  
  :base-relations ( e l o r )
  
  :composition-operation (( e e (e) )
			  ( e l (l) )
			  ( e o (o) )
			  ( e r (r) )
			  ( l e (l) )
			  ( l l (l o r) )
			  ( l o (r) )
			  ( l r (e l r) )
			  ( o e (o) )
			  ( o l (r) )
			  ( o o (e) )
			  ( o r (l) )
			  ( r e (r) )
			  ( r l (e l r) )
			  ( r o (l) )
			  ( r r (l o r) )
			  )
) ;; end def-calculus
