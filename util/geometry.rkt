#lang typed/racket/no-check
(require "../base/coord.rkt")
(provide circle-from-three-points-2d
         circle-from-three-points
         nearest-point-from-lines)

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
            (p1 (p+v l1p0 (v*r v sc))))
        (intermediate-point p0 p1)))))

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

