
;;;
;;; some basic geometric datatypes
;;;

(defstruct bounding-box
  xmin xmax ymin ymax)

;; simple polygon: no holes, no self-intersection, vertices are complex double-floats in CCW order
(defclass simple-polygon (sparq::primitive)
  ((name :reader polygon-name
	 :initarg :name)
   (vertices :reader polygon-vertices
	     :initarg :vertices)
   (bounding-box :reader polygon-bb
		 :initarg :bounding-box)))


(defconstant +eps+ 0.00001d0)

(defun ~ (x y)
  "numeric equality test"
  (< (abs (- x y)) +eps+))


(defun bounding-box (vertices)
  "computes bb struct from list of complex numbers representing vertices"
  (let ((xmin (realpart (first vertices)))
	(xmax (realpart (first vertices)))
	(ymin (imagpart (first vertices)))
	(ymax (imagpart (first vertices))))
    (dolist (v (rest vertices))
      (let ((x (realpart v)))
	(if (< x xmin)
	    (setq xmin x)
	    (if (> x xmax)
		(setq xmax x))))
      (let ((y (imagpart v)))
	(if (< y ymin)
	    (setq ymin y)
	    (if (> y ymax)
		(setq ymax y)))))
    (make-bounding-box :xmin xmin :xmax xmax :ymin ymin :ymax ymax)))

(defun interval-overlap (min1 max1 min2 max2)
  "determines jointly overlapped interval, nil if intevrals [min1,max1] and [min2,max2] are disjoint"
  (cond ((<= min2 min1 max2)
	 (values min1 (min max1 max2)))
	((<= min2 max1 max2)
	 (values (max min1 min2) max1))
	((<= min1 min2 max1)
	 (values min2 (min max1 max2)))
	((<= min1 max2 max1)
	 (values (max min1 min2) max2))))

(defun bounding-box-intersection (bb1 bb2)
  "computes intersection of two bounding-boxes, returns nil if none"
  (multiple-value-bind (xmin xmax) (interval-overlap (bounding-box-xmin bb1) (bounding-box-xmax bb1)
						     (bounding-box-xmin bb2) (bounding-box-xmax bb2))
    (multiple-value-bind (ymin ymax) (interval-overlap (bounding-box-ymin bb1) (bounding-box-ymax bb1)
						       (bounding-box-ymin bb2) (bounding-box-ymax bb2))
      (if (and xmin ymin)
	  (make-bounding-box :xmin xmin :xmax xmax :ymin ymin :ymax ymax)))))

(defun p-inside-bb? (p bb)
  (and (<= (bounding-box-xmin bb) (realpart p) (bounding-box-xmax bb))
       (<= (bounding-box-ymin bb) (imagpart p) (bounding-box-ymax bb))))

(defun bounding-box= (bb1 bb2)
  (and (~ (bounding-box-xmin bb1) (bounding-box-xmin bb2))
       (~ (bounding-box-xmax bb1) (bounding-box-xmax bb2))
       (~ (bounding-box-ymin bb1) (bounding-box-ymin bb2))
       (~ (bounding-box-ymax bb1) (bounding-box-ymax bb2))))

(defun lin-solve (m11 m12 m21 m22 lx ly)
  "Solves the equation M * x = l, M being a 2*2 matrix. The solution x1 and x2 is returned as multiple values." 
;;;  (declare (type double-float m11 m12 m21 m22 lx ly)
;;;          (optimize (speed 3) (safety 0)))
  (let ((det (- (* m11 m22) (* m12 m21))))
    ;;(declare (type double-float det)
    ;;(dynamic-extent det))
    (if (<  (abs det) +eps+)
      nil
      (let ((det1 (/ 1.0d0 det)))
;;        #(declare (type double-float det1)
;;                 (dynamic-extent det))
        (values (+ (* m22 det1 lx) (* (- m12) det1 ly))
                (+ (* (- m21) det1 lx) (* m11 det1 ly)))))))

(defun get-lambda-mue (l1-dir l1-anc l2-dir l2-anc)
  "Given a line-like object, this function computes the parameters lambda and mue, so that anchor1 + lambda * direction1 = anchor2 + mue * direction2 holds.
   lambda and mue are returnes as multiple values." 
;;  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (multiple-value-bind (lambda mue)
      (lin-solve (realpart l1-dir) (realpart l2-dir)
		 (imagpart l1-dir) (imagpart l2-dir)
		 (- (realpart l2-anc) (realpart l1-anc))
		 (- (imagpart l2-anc) (imagpart l1-anc)))
;;(declare (type double-float lambda mue))
    (values lambda (- mue))))

(defun collinear? (v w)
;; (declare (type (complex double-float) v w)           
;;         (type double-float +eps+)
;;           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (~ 0.0d0 (- (* (realpart v) (imagpart w))
	      (* (realpart w) (imagpart v)))))

(defun right-normal (vector)
  (complex (imagpart vector) (- (realpart vector))))

(defun left-normal (vector)
  (complex (- (imagpart vector)) (realpart vector)))

(defun point-on-line? (p s e)
  "determines whether p is positioned on line segment s->e; points are complex numbers"
  (if (~ 0.0d0 (- s e))
      (error "s = ~a e = ~a " s e))
  (let ((x (/ (- p s) (- e s)))) ; enjoy magic of complex number calculations
    (and (~ 0.0d0 (imagpart x))
	 (<= 0.0d0 (realpart x) 1.0d0))))

(defun line-segments-intersect? (l1-start l1-end l2-start l2-end)
  "intersection of line segments l1 and l2 (represented as start- and end-points as complex numbers), 2nd return value reads true if intersection occurs not at end points"
  (let ((dir-l1 (- l1-end l1-start))
	(dir-l2 (- l2-end l2-start)))
    (if (collinear? dir-l1 dir-l2)
	(and (collinear? dir-l1 (left-normal (- l1-start l2-start)))
	     (or (point-on-line? l1-start l2-start l2-end)
		 (point-on-line? l1-end   l2-start l2-end)
		 (point-on-line? l2-start l1-start l1-end)
		 (point-on-line? l2-end   l1-start l1-end)))
	(multiple-value-bind (lambda mue) (get-lambda-mue dir-l1 l1-start dir-l2 l2-start)
	  (values (and (<= 0.0d0 lambda 1.0d0)
		       (<= 0.0d0 mue    1.0d0))
		  (and (< +eps+ lambda (- 1.0d0 +eps+))
		       (< +eps+ mue    (- 1.0d0 +eps+))))))))

;(trace sparq:parse-primitive)
	
(defun point-inside-poly? (p poly)
  (sparq:do-tuples/c (a b) (polygon-vertices poly)
    (let ((dv (- b a))
	  (dp (- p a)))
      (when (> 0 (- (* (realpart dv) (imagpart dp)) (* (imagpart dv) (realpart dp))))
	(return-from point-inside-poly? nil))))
  t)
	     
;; FIXME: need to check that polygon is actually simple, i.e., there's no self-intersection
(defun setup-vertices (numbers) 
  "converts coordinate list (x1 y1 x2 y2 ...) to list of complex numbers"
  ;; NB: this function will signal an error if something goes wrong - that's intended to make parsing fail
  (let ((tmp ()))
    (loop while numbers do
	 (push (complex (coerce (pop numbers) 'double-float) (coerce (pop numbers) 'double-float)) tmp))
    ;; Now we need to remove collinear vertices... 
    ;; FIXME: looping is inefficient if many points need to be removed...
    (let ((points tmp)
	  (rem-points ())
	  (removing? t))
      (loop while removing? do
	   (setq removing? nil)
	   (sparq:do-tuples/c (a b c) points
	     (if (collinear? (- b a) (- c b))
		 (setq removing? t)
		 (push b rem-points)))
	   (setq points rem-points
		 rem-points ()))
      ;; making sure poly is in CCW order
      (let ((s 0.0d0))
	(sparq:do-tuples (a b) points
	  (incf s (* (- (realpart b) (realpart a)) (+ (imagpart a) (imagpart b)))))
	(if (< s 0)
	  (nreverse points)
	  points)))))

(defmethod sparq:parse-primitive ((x (eql 'simple-polygon)) expression &rest stuff)
  (declare (ignore stuff))
  (cond ((not (consp expression)) (cons :FAIL "polygons are list-based: (<name> x1 y1 x2 y2 ... xn yn)"))
	((evenp (length expression)) (cons :FAIL "odd-length list of points or missing name: (<name> x1 y1 x2 y2 ... xn yn)"))
	(t (let ((name (first expression))
		 (vertices (and (every #'realp (rest expression))
				(nthcdr 6 expression)
				(ignore-errors (setup-vertices (rest expression))))))
	     (sparq:debug-out 2 "vertices in CCW order: ~a" vertices)
	     (if vertices
		 (make-instance 'simple-polygon
				:name name
				:vertices vertices
				:bounding-box (bounding-box vertices))
		 (cons :FAIL "coordinate parsing failed"))))))

(defmethod print-object ((p simple-polygon) stream)
  (format stream "(~a ~{~a ~})" (polygon-name p) (polygon-vertices p)))

(provide :geometry) 
