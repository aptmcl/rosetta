#lang typed/racket/no-check
(require (except-in rosetta/autocad circle))

(struct circle
  ([center : Loc]
   [radius : Real]
   [radius-squared : Real])
  #:transparent)

(struct triangle
  ([p0 : Index]
   [p1 : Index]
   [p2 : Index]
   [circumcircle : circle])
  #:transparent)

(struct edge
  ([i0 : Index]
   [i1 : Index])
  #:transparent)

(define (triangle-edges [t : triangle]) : (Listof edge)
  (list (edge (triangle-p0 t) (triangle-p1 t))
	(edge (triangle-p1 t) (triangle-p2 t))
	(edge (triangle-p2 t) (triangle-p0 t))))

(define (edge=? [a0 : edge] [a1 : edge]) : Boolean
  (or (and (= (edge-i0 a0) (edge-i0 a1))
	   (= (edge-i1 a0) (edge-i1 a1)))
      (and (= (edge-i0 a0) (edge-i1 a1))
	   (= (edge-i1 a0) (edge-i0 a1)))))

(define (get-min-max [pts : Locs]) : (Values Loc Loc)
  (let ((pt (car pts))
        (pts (cdr pts)))
    (let-values
        ([(minx maxx miny maxy minz maxz)
          (for/fold ([minx : Real (cx pt)]
                     [maxx : Real (cx pt)]
                     [miny : Real (cy pt)]
                     [maxy : Real (cy pt)]
                     [minz : Real (cz pt)]
                     [maxz : Real (cz pt)])
                    ([p : Loc (in-list pts)])
            (values (min minx (cx p))
                    (max maxx (cx p))
                    (min miny (cy p))
                    (max maxy (cy p))
                    (min minz (cz p))
                    (max maxz (cz p))))])
      (values (xyz minx miny minz)
              (xyz maxx maxy maxz)))))

(define fudge-factor (make-parameter 10))

(define (get-bounding-triangle-points [pts : Locs])
  (let-values ([(min max) (get-min-max pts)])
    (let ((dx (* (fudge-factor) (- (cx max) (cx min))))
          (dy (* (fudge-factor) (- (cy max) (cy min)))))
      (list (xyz (- (cx min) dx) (- (cy min) (* dy 3)) 0.0)
            (xyz (- (cx min) dx) (+ (cy max) dy)       0.0)
            (xyz (+ (cx max) (* dx 3)) (+ (cy max) dy) 0.0)))))

(define epsilon (make-parameter 0.00001))

(define (circumcircle [v0 : Loc] [v1 : Loc] [v2 : Loc])
  (let* ((v1-v0 (-c v1 v0))
         (v2-v0 (-c v2 v0))
         (v2-v1 (-c v2 v1))
         (v1+v0 (+c v1 v0))
         (v2+v0 (+c v2 v0))
         (a (cx v1-v0))
         (b (cy v1-v0))
         (c (cx v2-v0))
         (d (cy v2-v0))
         (e (+ (* a (cx v1+v0))
               (* b (cy v1+v0))))
         (f (+ (* c (cx v2+v0))
               (* d (cy v2+v0))))
         (g (* 2.0 (- (* a (cy v2-v1))
                      (* b (cx v2-v1)))))
         (colinear? (< (abs g) (epsilon))))
    (let-values ([(cx cy dx dy)
                  (if colinear?
                      (let ((minx (min (cx v0) (cx v1) (cx v2)))
                            (miny (min (cy v0) (cy v1) (cy v2)))
                            (maxx (max (cx v0) (cx v1) (cx v2)))
                            (maxy (max (cy v0) (cy v1) (cy v2))))
                        (let ((x (/ (+ minx maxx) 2))
                              (y (/ (+ miny maxy) 2)))
                          (values x y (- x minx) (- y miny))))
                      (let ((x (/ (- (* d e) (* b f)) g))
                            (y (/ (- (* a f) (* c e)) g)))
                        (values x y (- x (cx v0)) (- y (cy v0)))))])
      (let* ((radius-squared (+ (* dx dx) (* dy dy)))
             (radius (cast (sqrt radius-squared) Real)))
        (circle (xy cx cy)
                radius
                radius-squared)))))

(define (new-triangle [vi0 : Index] [vi1 : Index] [vi2 : Index] [points : (Vectorof Loc)]) : triangle
  (let ((v0 (vector-ref points vi0))
        (v1 (vector-ref points vi1))
        (v2 (vector-ref points vi2)))
    (triangle vi0 vi1 vi2
              (circumcircle v0 v1 v2))))

(define (in-circumcircle? [p : Loc] [tri : triangle]) : Boolean
  (let* ((circumcircle (triangle-circumcircle tri))
         (center (circle-center circumcircle))
         (dist-squared (sqr-distance center p)))
    (<= dist-squared (+ (circle-radius-squared circumcircle)))))

(define (unique-edge? [edges : (Listof edge)] [a : edge]) : Boolean
  (let loop ((edges edges) (count 0))
    (and (<= count 1)
         (or (null? edges)
             (loop (cdr edges)
                   (if (edge=? (car edges) a)
                       (+ count 1)
                       count))))))

(define (shared-vertices? [t0 : triangle] [t1 : triangle]) : Boolean
  (or (= (triangle-p0 t0) (triangle-p0 t1))
      (= (triangle-p0 t0) (triangle-p1 t1))
      (= (triangle-p0 t0) (triangle-p2 t1))
      (= (triangle-p1 t0) (triangle-p0 t1))
      (= (triangle-p1 t0) (triangle-p1 t1))
      (= (triangle-p1 t0) (triangle-p2 t1))
      (= (triangle-p2 t0) (triangle-p0 t1))
      (= (triangle-p2 t0) (triangle-p1 t1))
      (= (triangle-p2 t0) (triangle-p2 t1))))

(define (add-vertex [vi : Index] [triangles : (Listof triangle)] [points : (Vectorof Loc)])
  (let-values
      ([(edges unaffected-tris)
        ;; For each triangle in the list we take the edges
        ;; of any triangle where vert is inside it's circumcircle
        ;; and append it to the edges list. Otherwise the triangle
        ;; is collected and stored in unaffected-tris
        (for/fold ([edges : (Listof edge) (list)]
                   [unaffected-triangles : (Listof triangle) (list)])
            ([tri : triangle (in-list triangles)])
          (if (in-circumcircle? (vector-ref points vi) tri)
              (values (append edges (triangle-edges tri))
                      unaffected-triangles)
              (values edges (append unaffected-triangles (list tri)))))])
    ;; Remove any edges that are duplicate so that the edge
    ;; list only contains the boundary edges.
    (let ((edges (filter (curry unique-edge? edges) edges)))
      ;; Using the remaining edges and our input vert create
      ;; new triangles and return them appended to our unaffected-tris list
      (append unaffected-tris
              (for/list ((edge : edge (in-list edges)))
                : (Listof triangle)
                (new-triangle (edge-i0 edge)
                              (edge-i1 edge)
                              vi
                              points))))))

(define (triangulate [points : Locs])
  (let* (;; Add the coords for a large bounding triangle to the point set
         (st-coords (get-bounding-triangle-points points))
         (sti0 (length points))
         (sti1 (cast (add1 sti0) Index))
         (sti2 (cast (add1 sti1) Index))
         (points (list->vector (append points st-coords)))
         ;; Create the bounding triangle instance
         (supertri (new-triangle sti0 sti1 sti2 points))
         ;; Initialize our triangle list
         (triangles (list supertri)))
    ;; For each point in the list we get an updated set
    ;; of triangles by retesselating using the new point
    (for ([i (vector-length points)])
      (set! triangles (add-vertex i triangles points)))
    ;; Remove any triangles that share points with the super triangle
    (map (lambda ([t : triangle])
           (triangle-face (vector-ref points (triangle-p0 t))
                          (vector-ref points (triangle-p1 t))
                          (vector-ref points (triangle-p2 t))))
         (filter (compose not (curry shared-vertices? supertri)) triangles))))

;(triangulate (list (xyz 0 0 0) (xyz 1 2 0) (xyz 2 1 0)))
;(triangulate (append* (map-division (lambda ([u : Real] [v : Real]) (xyz u v (sin (* u v)))) 0 4pi 2 0 4pi 2)))

#|

(struct triangle
  ([p0 : Loc]
   [p1 : Loc]
   [p2 : Loc]
   [circumcircle : circle]))

(define (shares-vertex? [t0 : triangle] [t1 : triangle])
  (or (eq? (triangle-p0 t0) (triangle-p0 t1))
      (eq? (triangle-p0 t0) (triangle-p1 t1))
      (eq? (triangle-p0 t0) (triangle-p2 t1))
      (eq? (triangle-p1 t0) (triangle-p0 t1))
      (eq? (triangle-p1 t0) (triangle-p1 t1))
      (eq? (triangle-p1 t0) (triangle-p2 t1))
      (eq? (triangle-p2 t0) (triangle-p0 t1))
      (eq? (triangle-p2 t0) (triangle-p1 t1))
      (eq? (triangle-p2 t0) (triangle-p2 t1))))

(struct edge
  ([p0 : Loc]
   [p1 : Loc]))

(define (triangle-edges [t : triangle])
  (list (edge (triangle-p0 t) (triangle-p1 t))
	(edge (triangle-p1 t) (triangle-p2 t))
	(edge (triangle-p2 t) (triangle-p0 t))))

(define (edge=? [a0 : edge] [a1 : edge])
  (or (eq? a0 a1)
      (and (loc=? (edge-p0 a0) (edge-p1 a1))
	   (loc=? (edge-p1 a0) (edge-p0 a1)))
      (and (loc=? (edge-p0 a0) (edge-p0 a1))
	   (loc=? (edge-p1 a0) (edge-p1 a1)))))





(define (new-triangle p0 p1 p2)
  (triangle (list p0 p1 p2)
            (circuncircle p0 p1 p2)))

#;
(define (super-triangle pts)
  (let* ((xmin (cx (argmin cx pts)))
         (xmax (cx (argmax cx pts)))
         (ymin (cy (argmin cy pts)))
         (ymax (cy (argmax cy pts)))
         (dx (- xmax xmin))
         (dy (- ymax ymin))
         (dmax (max dx dy))
         (xmed (/ (+ xmin xmax) 2.0))
         (ymed (/ (+ ymin ymax) 2.0)))
    (triang (xyz (- xmed (* 2 dmax)) (- ymed dmax)       0.0)
            (xyz xmed                (+ ymed (* 2 dmax)) 0.0)
            (xyz (+ xmed (* 2 dmax)) (- ymed dmax)       0.0))))


(define epsilon (make-parameter 0.000001))

(define (circuncircle triang)
  (let* ((p1 (triangle-p0 triang))
         (p2 (triangle-p1 triang))
         (p3 (triangle-p2 triang))
         (cp (cross-product p1 p2 p3))
         (p1x (cx p1))
         (p1y (cy p1))
         (p2x (cx p2))
         (p2y (cy p2))
         (p3x (cx p3))
         (p3y (cy p3))
         (center (if (< (abs cp) epsilon)
                     (let ((minx (min (cx p0) (cx p1) (cx p2)))
                           (miny (min (cy p0) (cy p1) (cy p2)))
                           (maxx (max (cx p0) (cx p1) (cx p2)))
                           (maxy (max (cy p0) (cy p1) (cy p2))))
                       (setf cx (/ (+ minx maxx) 2)
                             cy (/ (+ miny maxy) 2)
                             dx (- cx minx)
                             dy (- cy miny)))
                     (let ((p1Sq (+ (* p1x p1x) (* p1y p1y)))
                           (p2Sq (+ (* p2x p2x) (* p2y p2y)))
                           (p3Sq (+ (* p3x p3x) (* p3y p3y))))
                       (xy (/ (+ (* p1Sq (- p2y p3y))
                                 (* p2Sq (- p3y p1y))
                                 (* p3Sq (- p1y p2y)))
                              (* 2.0 cp))
                           (/ (+ (* p1Sq (- p3x p2x))
                                 (* p2Sq (- p1x p3x))
                                 (* p3Sq (- p2x p1x)))
                              (* 2.0 cp))))))
         (radius (plane-distance center p1)))
    (circle center radius (sqr radius) (* radius 2))))




(defun unique-edge? (edges a)
  (let ((instance-count (length (remove-if-not (lambda (b) (edge= a b)) edges))))
    (<= instance-count 1)))


(define (add-vertex [vi : Index] [triangles : (Listof triangle)] [points : (Vectorof Loc)])
  (let* ((edges ())
         (unaffected-tris ()))
    ;; For each triangle in the list we take the edges
    ;; of any triangle where vert is inside it's circumcircle
    ;; and append it to the edges list. Otherwise the triangle
    ;; is collected and stored in unaffected-tris
    (setf unaffected-tris
          (loop for tri in triangles
             if (in-circumcircle? tri (vector-ref points vi))
             do (let* ((verts (slot-value tri 'verts))
                       (e0 (list (vector-ref verts 0) (vector-ref verts 1)))
                       (e1 (list (vector-ref verts 1) (vector-ref verts 2)))
                       (e2 (list (vector-ref verts 2) (vector-ref verts 0))))
                  (setf edges (append edges (list e0 e1 e2))))
             else collect tri))
    
    ;; Remove any edges that are duplicate so that the edge
    ;; list only contains the boundary edges.
    (setf edges (remove-if-not (curry #'unique-edge? edges) edges))
    
    ;; Using the remaining edges and our input vert create
    ;; new triangles and return them appended to our unaffected-tris list
    (append unaffected-tris (loop for edge in edges
                               collect (let ((vi0 (first edge))
                                             (vi1 (second edge)))
                                         (new-triangle vi0 vi1 vi points))))))

(define (triangulate [pts : Locs])
  (let* ((coords (get-bounding-triangle pts))
         (i0 (length points))
         (i1 (add1 i0))
         (i2 (add1 i1))
         (points (vector->list (append pts coords)))
         ;; Create the bounding triangle instance
         (supertri (new-triangle i0 i1 i2 points))
         ;; Initialize our triangle list
         (triangles (list supertri)))
    ;; For each point in the list we get an updated set
    ;; of triangles by retesselating using the new point
    (for ([i (in-range (length points))])
      (set! triangles (add-vertex i triangles points)))
    ;; Remove any triangles that share points with the super triangle
    (remf (curry #'has-shared-verts? supertri) triangles)))





(define (sort-xy pts)
  (sort pts
        (lambda (p0 p1)
          (or (< (cx p0) (cx p1))
              (and (= (cx p0) (cx p1))
                   (< (cy p0) (cy p1)))))))

(struct circ
  (c r))

(define (plane-distance p0 p1)
  (sqrt (+ (sqr (- (cx p0) (cx p1)))
	   (sqr (- (cy p0) (cy p1))))))

(define (inside-circ? p circ)
  (<= (plane-distance p (circ-c circ))
      (circ-r circ)))

(struct triang
  (p0 p1 p2))

(define (super-triangle pts)
  (let* ((xmin (cx (argmin cx pts)))
         (xmax (cx (argmax cx pts)))
         (ymin (cy (argmin cy pts)))
         (ymax (cy (argmax cy pts)))
         (dx (- xmax xmin))
         (dy (- ymax ymin))
         (dmax (max dx dy))
         (xmed (/ (+ xmin xmax) 2.0))
         (ymed (/ (+ ymin ymax) 2.0)))
    (triang (xyz (- xmed (* 2 dmax)) (- ymed dmax)       0.0)
            (xyz xmed                (+ ymed (* 2 dmax)) 0.0)
            (xyz (+ xmed (* 2 dmax)) (- ymed dmax)       0.0))))

(define (cross-product p1 p2 p3)
  (let ((u1 (- (cx p2) (cx p1)))
        (v1 (- (cy p2) (cy p1)))
        (u2 (- (cx p3) (cx p1)))
        (v2 (- (cy p3) (cy p1))))
    (- (* u1 v2) (* v1 u2))))

(define (circuncircle triang)  
  (let* ((p1 (triangle-p0 triang))
	(p2 (triangle-p1 triang))
	(p3 (triangle-p2 triang))
	(cp (cross-product p1 p2 p3))
	(p1x (cx p1))
        (p1y (cy p1))
	(p2x (cx p2))
        (p2y (cy p2))
	(p3x (cx p3))
        (p3y (cy p3))
	(centro (if (= cp 0.0)
		 (begin
		   (print (list p1 p2 p3))
		   ;(erro ": pontos colineares")
		   #f)
                 (let ((p1Sq (+ (* p1x p1x) (* p1y p1y)))
                       (p2Sq (+ (* p2x p2x) (* p2y p2y)))
                       (p3Sq (+ (* p3x p3x) (* p3y p3y))))
                   (xy (/ (+ (* p1Sq (- p2y p3y))
                             (* p2Sq (- p3y p1y))
                             (* p3Sq (- p1y p2y)))
                          (* 2.0 cp))
                       (/ (+ (* p1Sq (- p3x p2x))
                             (* p2Sq (- p1x p3x))
                             (* p3Sq (- p2x p1x)))
                          (* 2.0 cp)))))))
    (and centro
         (circ centro (plane-distance centro p1)))))

(define (remove-duplicate-edges edges)
  (remove-duplicates edges edge=?))

(define (delaunay pts)
  (let* ((pts (sort-xy pts))
         (super-triang (super-triangle pts))
         (triangles (list super-triang))
         (complete-triangles (list)))
    (for ([pt (in-list pts)])
      (let ((edges (list))
            (new-triangles (list)))
        (for ([triangle triangles])
          (let ((circle (circuncircle triangle)))
            (if (or (not circle) (inside-circ? pt circle))
                (set! edges (append edges (triangle-edges triangle)))
                (set! new-triangles (cons triangle new-triangles)))))
        (set! edges (remove-duplicate-edges edges))
        (for ([edge edges])
          (set! new-triangles
                (cons (triang (edge-p0 edge)
                              (edge-p1 edge)
                              pt)
                      new-triangles)))
        (set! triangles new-triangles)))
    (filter (lambda (triang)
              (not (shares-vertex? triang super-triang)))
            triangles)))


(define (triangulate pts)
  (map draw-triangle (delaunay pts)))

(triangulate (append* (map-division (lambda (u v) (xyz u v (sin (* u v)))) 0 4pi 8 0 4pi 8)))
|#
