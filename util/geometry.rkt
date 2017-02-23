#lang typed/racket/no-check
(require "../base/coord.rkt")
(require "../base/utils.rkt")
(provide circle-from-three-points-2d
         circle-from-three-points
         nearest-point-from-lines
         xy-segments-intersection
         xy-segment-parametric-curve-intersection)
(provide epsilon)
(define epsilon (make-parameter 1e-8))

(define (nearest-point-from-lines [l0p0 : Loc] [l0p1 : Loc] [l1p0 : Loc] [l1p1 : Loc]) : Loc
  (let* ((u (p-p l0p1 l0p0))
         (v (p-p l1p1 l1p0))
         (w (p-p l0p0 l1p0))
         (a (v.v u u))
         (b (v.v u v))
         (c (v.v v v))
         (d (v.v u w))
         (e (v.v v w))
         (D (- (* a c) (* b b))))
    (let-values
        ([(sc tc)
          (if (< D (epsilon)) ;almost parallel
              (values 0.0
                      (if (> b c) (/ d b) (/ e c)))
              (values (/ (- (* b e) (* c d)) D)
                      (/ (- (* a e) (* b d)) D)))])
      (let ((p0 (p+v l0p0 (v*r u sc)))
            (p1 (p+v l1p0 (v*r v tc))))
        (intermediate-point p0 p1)))))

(define (xy-segments-intersection p0 p1 p2 p3)
  (let ((x0 (cx p0)) (x1 (cx p1)) (x2 (cx p2)) (x3 (cx p3))
        (y0 (cy p0)) (y1 (cy p1)) (y2 (cy p2)) (y3 (cy p3)))
    (let ((denominator (- (* (- y3 y2) (- x1 x0))
                          (* (- x3 x2) (- y1 y0)))))
      (and (> (abs denominator) 0) ;;not parallel
           (let ((u (/ (- (* (- x3 x2) (- y0 y2))
                          (* (- y3 y2) (- x0 x2)))
                       denominator))
                 (v (/ (- (* (- x1 x0) (- y0 y2))
                          (* (- y1 y0) (- x0 x2)))
                       denominator))
                 (e (epsilon)))
             (and (<= 0 u 1)
                  (<= 0 v 1)
                  (xy (+ x0 (* u (- x1 x0)))
                      (+ y0 (* u (- y1 y0))))))))))


(define (circle-from-three-points-2d [v0 : Loc] [v1 : Loc] [v2 : Loc]) : (Values Loc Real)
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
        (values (xy cx cy) radius)))))

(define (circle-from-three-points [p0 : Loc] [p1 : Loc] [p2 : Loc]) : (Values Loc Real)
  (let ([cs (cs-from-o-vx-vy p0 (p-p p1 p0) (p-p p2 p0))])
    (parameterize ((current-cs cs))
      (let-values ([(c r)
                    (circle-from-three-points-2d
                     (loc-in-cs p0 cs)
                     (loc-in-cs p1 cs)
                     (loc-in-cs p2 cs))])
        (values c r)))))

(define (xy-segment-parametric-curve-intersection f t0 t1 p0 p1 n epsilon)
  ;First sample the curve into different segments
  (let ((ts (division t0 t1 n #t)))
    ;Second, identify the segment
    (let ((t0-t1 (for/or ((t0 (in-list ts))
                          (t1 (in-list (append (cdr ts) (list (car ts))))))
                   (and (xy-segments-intersection p0 p1 (f t0) (f t1))
                        (list t0 t1)))))
      ;Third, fine tune
      (let loop ((t0 (first t0-t1))
                 (fp0 (f (first t0-t1)))
                 (t1 (second t0-t1))
                 (fp1 (f (second t0-t1))))
        (let ((tm (/ (+ t0 t1) 2)))
          (if (< (- t1 t0) epsilon)
              (f tm)
              (let ((fpm (f tm)))
                (if (xy-segments-intersection p0 p1 fp0 fpm)
                    (loop t0 fp0 tm fpm)
                    (loop tm fpm t1 fp1)))))))))

(define (arc-from-two-points-angle p0 p1 angle)
  (let* ((v (p-p p1 p0))
         (d2 (+ (sqr (cx v)) (sqr (cy v))))
         (r2 (/ d2 (* 2 (- 1 (cos angle)))))
         (l (sqrt (- r2 (/ d2 4))))
         (m (p+v p0 (v*r v 0.5)))
         (phi (pol-phi v)))
    (values (p+v m (vpol l (+ phi pi/2))) (sqrt r2)))) 


#|
(define (circle-from-sphere-intersection [p0 : Loc] [r0 : Real] [p1 : Loc] [r1: Real])
  (let ((cs (cs-from-o-n p0 (p-p p1 p0))))
    ;;Intersect spheres with xy plane

  
def sss_int(p1, r1, p2, r2, p3, r3):
    """Intersect three spheres, centered in p1, p2, p3 with radius r1,r2,r3 respectively. 
       Returns a list of zero, one or two solution points.
    """
    solutions = []
    # plane though p1, p2, p3
    n = vector.cross(p2-p1, p3-p1)
    n = n / vector.norm(n)
    # intersect circles in plane
    cp1 = vector.vector([0.0,0.0]) 
    cp2 = vector.vector([vector.norm(p2-p1), 0.0])
    cpxs = cc_int(cp1, r1, cp2, r2)
    if len(cpxs) == 0:
        return []
    # px, rx, nx is circle 
    px = p1 + (p2-p1) * cpxs[0][0] / vector.norm(p2-p1)
    rx = abs(cpxs[0][1])
    # plane of intersection cicle
    nx = p2-p1
    nx = nx / vector.norm(nx)
    # print "px,rx,nx:",px,rx,nx
    # py = project p3 on px,nx
    dy3 = vector.dot(p3-px, nx)
    py = p3 - (nx * dy3)
    if tol_gt(dy3, r3):
        return []
    ry = math.sin(math.acos(abs(dy3/r3)))*r3
    # print "py,ry:",py,ry
    cpx = vector.vector([0.0,0.0]) 
    cpy = vector.vector([vector.norm(py-px), 0.0])
    cp4s = cc_int(cpx, rx, cpy, ry)
    for cp4 in cp4s:
        p4 = px + (py-px) * cp4[0] / vector.norm(py-px) + n * cp4[1] 
        solutions.append(p4)  
    return solutions

(define (circle-intersection p1 r1 p2 r2)
  (let ((d (distance p1 p2)))
    (if (< d 
def cc_int(p1, r1, p2, r2):
	"""
	Intersect circle (p1,r1) circle (p2,r2)
	where p1 and p2 are 2-vectors and r1 and r2 are scalars
	Returns a list of zero, one or two solution points.
	"""
	d = vector.norm(p2-p1)
	if not tol_gt(d, 0):
		return []
	u = ((r1*r1 - r2*r2)/d + d)/2
	if tol_lt(r1*r1, u*u):
		return []
        elif r1*r1 < u*u:
            v = 0.0
        else:
            v = math.sqrt(r1*r1 - u*u)
	s = (p2-p1) * u / d
	if tol_eq(vector.norm(s),0):
	        p3a = p1+vector.vector([p2[1]-p1[1],p1[0]-p2[0]])*r1/d
	        if tol_eq(r1/d,0):
                    return [p3a]
                else:
                    p3b = p1+vector.vector([p1[1]-p2[1],p2[0]-p1[0]])*r1/d
                    return [p3a,p3b]
	else:
	        p3a = p1 + s + vector.vector([s[1], -s[0]]) * v / vector.norm(s) 
                if tol_eq(v / vector.norm(s),0):
                    return [p3a]
                else:
                    p3b = p1 + s + vector.vector([-s[1], s[0]]) * v / vector.norm(s)
    	            return [p3a,p3b]
|#

