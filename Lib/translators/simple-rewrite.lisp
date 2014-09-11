

(defun relation-rewrite-fn (source-calculus target-calculus relation-assoc)
  "creates a rewrite function from an assoc list ((source-rel . target-rel) ...) that unites all target-rels that correspond to a given source-rel"
  (let ((empty (relation source-calculus ())))
    #'(lambda (source-rel)
	(reduce #'(lambda (accu src.target)
		    (if (not (relation= source-calculus empty (intersect source-calculus source-rel (car src.target))))
			(unite target-calculus accu (cdr src.target))
		      accu))
		relation-assoc
		:initial-value (relation target-calculus ())))))

(defun simple-relation-rewrite (csp target-calculus rewrite-fn)
  "rewrites a csp by rewriting every relation"

      ;; resulting CSP obtained by changing every relation
      (make-instance 'constraint-reasoning::constraint-network
		     :calculus    target-calculus
		     :objects     (constraint-reasoning::constraint-network-objects csp)
		     :constraints (mapcar #'(lambda (constraint)
					      (constraint-reasoning::make-constraint (constraint-reasoning::constraint-object-1 constraint)
										     (funcall rewrite-fn (constraint-reasoning::constraint-relation constraint))
										     (constraint-reasoning::constraint-object-2 constraint)))
					  (constraint-reasoning::constraints csp))))