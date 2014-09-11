;;;
;;; OPRA
;;;

(def-calculus "OPRA"
    :arity :binary
    :parametric? t
    :consistency :unknown
    :basis-entity :2d-oriented-point
    :qualifier (external-lib "liboprac.dylib" "opra_qualify")
    :base-relations  #'(lambda ()  ;; Function to compute the base relations depending on the caluclus parameter
			 (let ((rels nil)
			       (n (read-from-string calculi:*calculus-parameter*)))
			   (assert (and (realp n) (< 0 n)))
			   (dotimes (i (* 4 n))
			     (push (intern (format nil "S_~a" i)) rels)
			     (dotimes (j (* 4 n))
			       (push (intern (format nil "~a_~a" i j)) rels)))
			   rels))

    :identity-relation s_0
    :converse-operation (:external-lib "liboprac.dylib" "opra_converse")
    :composition-operation (:external-lib "liboprac.dylib" "opra_compose"))
