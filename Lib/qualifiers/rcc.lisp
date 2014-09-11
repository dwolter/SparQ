
(defun containment-relation (p1 p2 tp-rel ntp-rel)
  "disambiguates cases where one poly (p2) is inside the bounding box of another one (p1)"
  (let ((contact? nil)
	(disjoint? nil)
	(p1-inside-p2? t)
	(p2-inside-p1? t))
    (sparq:do-tuples/c (p1v1 p1v2) (polygon-vertices p1)
      (unless (point-inside-poly? p1v2 p2)
	;;(format t " ~a outside p2!~%" p1v1)
	(setq p1-inside-p2? nil))
      (sparq:do-tuples/c (p2v1 p2v2) (polygon-vertices p2)
	(unless (point-inside-poly? p2v2 p1)
	  ;;(format t " ~a outside p1!~%" p2v1)
	  (setq p2-inside-p1? nil))
	;;(format t "~%~a-~a  ~a-~a" p1v1 p1v2 p2v1 p2v2)
	(multiple-value-bind (isect? isect-inside?) (line-segments-intersect? p1v1 p1v2 p2v1 p2v2)
	  ;;(format t "~%~a->~a : ~a->~a :: ~a(~a)" p1v1 p1v2 p2v1 p2v2 isect? isect-inside?)
	  (when isect? ;; touch?
	    (when isect-inside?
	      (setq disjoint? t))
	    (setq contact? t)))))
    ;;(format t "p1-inside-p2? ~a  p2-inside-p1? ~a" p1-inside-p2? p2-inside-p1?)
    (cond ((and p1-inside-p2? p2-inside-p1?) 'eq)
	  (p1-inside-p2? (if contact? tp-rel ntp-rel))
	  (p2-inside-p1? (sparq:signal-error "Implementation error in RCC qualifier - this shouldn't happen!"))
	  (contact? 'po)
	  (t 'dc))))

(defun rcc8-constraint (p1 p2)
  "determines rcc8-relation that holds between two simple polygons"
  (let ((ibox (bounding-box-intersection (polygon-bb p1) (polygon-bb p2))))
    ;;(format t "~%ibox intersection : ~a : ~a : ~a ~%" ibox (bounding-box= ibox (polygon-bb p1)) (bounding-box= ibox (polygon-bb p2)))
    (if ibox
	(cond 
	  ((bounding-box= ibox (polygon-bb p1)) ;; p1 (n)tpp p2 (or eq)
	   (containment-relation p1 p2 'tpp 'ntpp))
	  ((bounding-box= ibox (polygon-bb p2)) ;; p2 (n)tpp p1 (or eq)
	   (containment-relation p2 p1 'tppi 'ntppi))
	  (t ;; overlapping bb : test for intersection
	   (let ((touch? nil))
	     (catch 'intersection-found
	       (sparq:do-tuples/c (p1v1 p1v2) (polygon-vertices p1)
		 (sparq:do-tuples/c (p2v1 p2v2) (polygon-vertices p2)
		   ;;(format t "~%schnitt ~a->~a mit ~a->~a? ?" p1v1 p1v2 p2v1 p2v2 )
		   (multiple-value-bind (isect? isect-inside?) (line-segments-intersect? p1v1 p1v2 p2v1 p2v2)
		     (when isect?
		       (if (collinear? (- p1v1 p1v2) (- p2v1 p2v2))
			   (setq touch? t)
			   (if isect-inside? 
			       (throw 'intersection-found 'po)
			       (setq touch? t)))))))
	       (if touch? 'ec 'dc)))))
	'dc)))

(defun qualify-rcc8-from-polygons (c option polys)
  "determines rcc relations between polygons"
  ;; c     is the rcc8 calculus
  ;; polys is a list which elements are of type simple-polygon (defined in geometry-types)
  (make-instance 'constraint-network 
		 :calculus c
		 :objects (mapcar #'polygon-name polys)
		 :constraints (if (eq option 'first2all)					
				  (loop for p2 in (rest polys) collecting 
				       (csp:make-constraint (polygon-name (first polys))
							    (relation c (rcc8-constraint (first polys) p2))
							    (polygon-name p2)))
				  ;; option 'all'
				  (loop for p1 in polys append 
				       (loop for p2 in (rest (member p1 polys)) collecting
					    (csp:make-constraint (polygon-name p1)
								 (relation c (rcc8-constraint p1 p2))
								 (polygon-name p2)))))))

(defun qualify-rcc5-from-polygons (c option polys)
  "determines rcc relations between polygons"
  ;; c     is the rcc5 calculus
  ;; polys is a list which elements are of type simple-polygon (defined in geometry-types)
  (let ((rcc-mapping '((DC . DR) ; map rcc8 relations to rcc5
		       (EC . DR)
		       (EQ . EQ)
		       (NTPP . PP)
		       (NTPPI . PPI)
		       (PO . PO)
		       (TPP . PP)
		       (TPPI . PPI))))
    (make-instance 'constraint-network 
		   :calculus c
		   :objects (mapcar #'polygon-name polys)
		   :constraints (if (eq option 'first2all)					
				    (loop for p2 in (rest polys) collecting 
					 (csp:make-constraint (polygon-name (first polys))
							      (relation c (cdr (assoc (rcc8-constraint (first polys) p2) rcc-mapping)))
							      (polygon-name p2)))
				    ;; option 'all'
				    (loop for p1 in polys append 
					 (loop for p2 in (rest (member p1 polys)) collecting
					      (csp:make-constraint (polygon-name p1)
								   (relation c (cdr (assoc (rcc8-constraint p1 p2) rcc-mapping)))
								   (polygon-name p2))))))))

