;;; 
;;; Double cross calculus
;;;

(def-calculus "Double Cross Calculus - single number notation (DCC2)"
  :arity :ternary
  :basis-entity :2d-point
  :qualifier (external-lib "libdcc2.dylib" "dcc2_qualify")
  :identity-relation b
  :inverse-operation (( 0 6 )
                      ( 11 5 )
                      ( 10 4 )
                      ( 9 3 )
                      ( 8 2 )
                      ( 7 1 )
                      ( 6 0 )
                      ( 5 11 )
                      ( 4 10 )
                      ( 3 9 )
                      ( 2 8 )
                      ( 1 7 )
                      ( 12 12 )
                      ( a b )
                      ( b a )
                      ( Dou Dou )
                      ( Tri Tri ))
  
  :shortcut-operation (( 0   12 )
                       ( 11  3 )
                       ( 10  3 )
                       ( 9   (3 2 1) )
                       ( 8   4 )
                       ( 7   5 )
                       ( 6   6 )
                       ( 5   7 )
                       ( 4   8 )
                       ( 3   (9 10 11) )
                       ( 2   9 )
                       ( 1   9 )
                       ( 12  0 )
                       ( a   Dou )
                       ( b   b )
                       ( Dou a )
                       ( Tri Tri ))
  
  :homing-operation (( 0   6 )
                     ( 11  7 )
                     ( 10  8 )
                     ( 9   (9 10 11) )
                     ( 8   9 )
                     ( 7   9 )
                     ( 6   12 )
                     ( 5   3 )
                     ( 4   3 )
                     ( 3   (1 2 3) )
                     ( 2   4 )
                     ( 1   5 )
                     ( 12  0 )
                     ( a   b )
                     ( b   Dou )
                     ( Dou a )
                     ( Tri Tri ))
  
  :base-relations ( 0 11 10 9 8 7 6 5 4 3 2 1 12 a b Dou Tri )  
  
  :composition-operation (( 0 0 (0))
			  ( 0 1 (1))
			  ( 0 2 (1))
			  ( 0 3 (1))
			  ( 0 4 (2))
			  ( 0 5 (3 4 5))
			  ( 0 6 (6 12 a))
			  ( 0 7 (7 8 9))
			  ( 0 8 (10))
			  ( 0 9 (11))
			  ( 0 10 (11))
			  ( 0 11 (11))
			  ( 0 12 (0))
			  ( 0 a (b))
			  ( 0 b (0))
			  ( 0 dou NIL)
			  ( 0 tri NIL)
			  ( 1 0 (1))
			  ( 1 1 (1 2 3 4 5))
			  ( 1 2 (1 2 3 4 5))
			  ( 1 3 (1 2 3 4 5))
			  ( 1 4 (3 4 5))
			  ( 1 5 (3 4 5 6 7 8 9 12 a))
			  ( 1 6 (7 8 9))
			  ( 1 7 (7 8 9 10 11))
			  ( 1 8 (11))
			  ( 1 9 (0 1 11))
			  ( 1 10 (0 1 11))
			  ( 1 11 (0 1 11))
			  ( 1 12 (1))
			  ( 1 a (b))
			  ( 1 b (1))
			  ( 1 dou NIL)
			  ( 1 tri NIL)
			  ( 2 0 (2))
			  ( 2 1 (3 4 5))
			  ( 2 2 (3 4 5))
			  ( 2 3 (3 4 5))
			  ( 2 4 (6 12 a))
			  ( 2 5 (7 8 9))
			  ( 2 6 (10))
			  ( 2 7 (11))
			  ( 2 8 (0))
			  ( 2 9 (1))
			  ( 2 10 (1))
			  ( 2 11 (1))
			  ( 2 12 (2))
			  ( 2 a (b))
			  ( 2 b (2))
			  ( 2 dou NIL)
			  ( 2 tri NIL)
			  ( 3 0 (3 4 5))
			  ( 3 1 (3 4 5 6 7 8 9 12 a))
			  ( 3 2 (3 4 5 6 7 8 9 12 a))
			  ( 3 3 (3 4 5 6 7 8 9 12 a))
			  ( 3 4 (7 8 9))
			  ( 3 5 (7 8 9 10 11))
			  ( 3 6 (11))
			  ( 3 7 (0 1 11))
			  ( 3 8 (1))
			  ( 3 9 (1 2 3 ))
			  ( 3 10 (1 2 3 ))
			  ( 3 11 (1 2 3 4 5))
			  ( 3 12 (3))
			  ( 3 a (b))
			  ( 3 b (3))
			  ( 3 dou NIL)
			  ( 3 tri NIL)
			  ( 4 0 (5))
			  ( 4 1 (5 6 7))
			  ( 4 2 (5 6 7))
			  ( 4 3 (3 4 5 6 7 8 9 12 a))
			  ( 4 4 (7 8 9))
			  ( 4 5 (7 8 9 10 11))
			  ( 4 6 (11))
			  ( 4 7 (0 1 11))
			  ( 4 8 (1))
			  ( 4 9 (1 2 3))
			  ( 4 10 (1 2 3))
			  ( 4 11 (1 2 3 4 5))
			  ( 4 12 (3))
			  ( 4 a (b))
			  ( 4 b (4))
			  ( 4 dou NIL)
			  ( 4 tri NIL)
			  ( 5 0 (5))
			  ( 5 1 (5 6 7 ))
			  ( 5 2 (5 6 7 ))
			  ( 5 3 (3 4 5 6 7 8 9 12 a))
			  ( 5 4 (7 8 9))
			  ( 5 5 (7 8 9 10 11))
			  ( 5 6 (11))
			  ( 5 7 (0 1 11))
			  ( 5 8 (1))
			  ( 5 9 (1 2 3 4 5))
			  ( 5 10 (1 2 3 4 5))
			  ( 5 11 (1 2 3 4 5))
			  ( 5 12 (3 4 5))
			  ( 5 a (b))
			  ( 5 b (5))
			  ( 5 dou NIL)
			  ( 5 tri NIL)
			  ( 6 0 (6))
			  ( 6 1 (7))
			  ( 6 2 (7))
			  ( 6 3 (7 8 9))
			  ( 6 4 (10))
			  ( 6 5 (11))
			  ( 6 6 (0))
			  ( 6 7 (1))
			  ( 6 8 (2))
			  ( 6 9 (3 4 5))
			  ( 6 10 (5))
			  ( 6 11 (5))
			  ( 6 12 (6 12 a))
			  ( 6 a (b))
			  ( 6 b (6))
			  ( 6 dou NIL)
			  ( 6 tri NIL)
			  ( 7 0 (7))
			  ( 7 1 (7 8 9 10 11))
			  ( 7 2 (7 8 9 10 11))
			  ( 7 3 (7 8 9 10 11))
			  ( 7 4 (11))
			  ( 7 5 (0 1 11))
			  ( 7 6 (1))
			  ( 7 7 (1 2 3 4 5))
			  ( 7 8 (3 4 5))
			  ( 7 9 (3 4 5 6 7 8 9 12 a))
			  ( 7 10 (5 6 7))
			  ( 7 11 (5 6 7))
			  ( 7 12 (7 8 9))
			  ( 7 a (b))
			  ( 7 b (7))
			  ( 7 dou NIL)
			  ( 7 tri NIL)
			  ( 8 0 (7))
			  ( 8 1 (7 8 9 10 11))
			  ( 8 2 (9 10 11))
			  ( 8 3 (9 10 11))
			  ( 8 4 (11))
			  ( 8 5 (0 1 11))
			  ( 8 6 (1))
			  ( 8 7 (1 2 3 4 5))
			  ( 8 8 (3 4 5))
			  ( 8 9 (3 4 5 6 7 8 9 12 a))
			  ( 8 10 (5 6 7))
			  ( 8 11 (5 6 7))
			  ( 8 12 (9))
			  ( 8 a (b))
			  ( 8 b (8))
			  ( 8 dou NIL)
			  ( 8 tri NIL)
			  ( 9 0 (7 8 9))
			  ( 9 1 (7 8 9 10 11))
			  ( 9 2 (9 10 11))
			  ( 9 3 (9 10 11))
			  ( 9 4 (11))
			  ( 9 5 (0 1 11))
			  ( 9 6 (1))
			  ( 9 7 (1 2 3 4 5))
			  ( 9 8 (3 4 5))
			  ( 9 9 (3 4 5 6 7 8 9 12 a))
			  ( 9 10 (3 4 5 6 7 8 9 12 a))
			  ( 9 11 (3 4 5 6 7 8 9 12 a))
			  ( 9 12 (9))
			  ( 9 a (b))
			  ( 9 b (9))
			  ( 9 dou NIL)
			  ( 9 tri NIL)
			  ( 10 0 (10))
			  ( 10 1 (11))
			  ( 10 2 (11))
			  ( 10 3 (11))
			  ( 10 4 (0))
			  ( 10 5 (1))
			  ( 10 6 (2))
			  ( 10 7 (3 4 5))
			  ( 10 8 (6 12 a))
			  ( 10 9 (7 8 9))
			  ( 10 10 (7 8 9))
			  ( 10 11 (7 8 9))
			  ( 10 12 (10))
			  ( 10 a (b))
			  ( 10 b (10))
			  ( 10 dou NIL)
			  ( 10 tri NIL)
			  ( 11 0 (11))
			  ( 11 1 (0 1 11))
			  ( 11 2 (0 1 11))
			  ( 11 3 (0 1 11))
			  ( 11 4 (1))
			  ( 11 5 (1 2 3 4 5))
			  ( 11 6 (3 4 5))
			  ( 11 7 (3 4 5 6 7 8 9 12 a))
			  ( 11 8 (7 8 9))
			  ( 11 9 (7 8 9 10 11))
			  ( 11 10 (7 8 9 10 11))
			  ( 11 11 (7 8 9 10 11))
			  ( 11 12 (11))
			  ( 11 a (b))
			  ( 11 b (11))
			  ( 11 dou NIL)
			  ( 11 tri NIL)
			  ( 12 0 (6 12 a))
			  ( 12 1 (7 8 9))
			  ( 12 2 (9))
			  ( 12 3 (9))
			  ( 12 4 (10))
			  ( 12 5 (11))
			  ( 12 6 (0))
			  ( 12 7 (1))
			  ( 12 8 (2))
			  ( 12 9 (3))
			  ( 12 10 (3))
			  ( 12 11 (3 4 5))
			  ( 12 12 (12))
			  ( 12 a (b))
			  ( 12 b (12))
			  ( 12 dou NIL)
			  ( 12 tri NIL)
			  ( a 0 (6))
			  ( a 1 (7))
			  ( a 2 (8))
			  ( a 3 (9))
			  ( a 4 (10))
			  ( a 5 (11))
			  ( a 6 (0))
			  ( a 7 (1))
			  ( a 8 (2))
			  ( a 9 (3))
			  ( a 10 (4))
			  ( a 11 (5))
			  ( a 12 (12))
			  ( a a (b))
			  ( a b (a))
			  ( a dou NIL)
			  ( a tri NIL)
			  ( b 0 NIL)
			  ( b 1 NIL)
			  ( b 2 NIL)
			  ( b 3 NIL)
			  ( b 4 NIL)
			  ( b 5 NIL)
			  ( b 6 NIL)
			  ( b 7 NIL)
			  ( b 8 NIL)
			  ( b 9 NIL)
			  ( b 10 NIL)
			  ( b 11 NIL)
			  ( b 12 NIL)
			  ( b a NIL)
			  ( b b NIL)
			  ( b dou (0 1 2 3 4 5 6 7 8 9 10 11 12 a))
			  ( b tri (b))
			  ( dou 0 (DOU))
			  ( dou 1 (DOU))
			  ( dou 2 (DOU))
			  ( dou 3 (DOU))
			  ( dou 4 (DOU))
			  ( dou 5 (DOU))
			  ( dou 6 (DOU))
			  ( dou 7 (DOU))
			  ( dou 8 (DOU))
			  ( dou 9 (DOU))
			  ( dou 10 (DOU))
			  ( dou 11 (DOU))
			  ( dou 12 (DOU))
			  ( dou a (TRI))
			  ( dou b (DOU))
			  ( dou dou NIL)
			  ( dou tri NIL)
			  ( tri 0 NIL)
			  ( tri 1 NIL)
			  ( tri 2 NIL)
			  ( tri 3 NIL)
			  ( tri 4 NIL)
			  ( tri 5 NIL)
			  ( tri 6 NIL)
			  ( tri 7 NIL)
			  ( tri 8 NIL)
			  ( tri 9 NIL)
			  ( tri 10 NIL)
			  ( tri 11 NIL)
			  ( tri 12 NIL)
			  ( tri a NIL)
			  ( tri b NIL)
			  ( tri dou (DOU))
			  ( tri tri (TRI))
			  )
  )
