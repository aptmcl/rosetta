#lang racket
(require "backend.rkt")


(init 10)

(define (ship p size)
  (let ([p0 p]
        [p1 (+x p (* size 2))]
        [p2 (+x p (* size 1))]
        [p3 (+x p (* size -2))]
        [rhos (list (/ size 2) (/ size 2) (/ size 2))]
        [phis (list pi/2 0.0 3pi/2)]
        )
    (let ([p10 (+z p1 (/ size -1))]
          [p11 (+z p (/ size -1))]
          [p12 (+y p1 (/ size 3))] 
          [p13 (+y p1 (/ size -3))]
          [base-pts-1 (for/list
                          ((rho rhos) (phi phis))
                        (+pol p1 rho phi))]
          [base-pts-2 (for/list
                          ((rho rhos) (phi phis))
                        (+pol p2 rho phi))])
      (let ([p20 (+x (first base-pts-1) (* size -2))]
            [p21 (+x (last base-pts-1) (* size -2))])
        (point p10 0.5 0.0) (point p 0.5 1.0 0.0 0.0) (point p1  0.5 0.0 0.5 0.0)
        (mirror
         (list (point (+pol p1 size pi/2) 0.5 0.0 0.5 1.0)(point (+pol p1 size 3pi/2) 0.5 0.0 0.5 1.0)(point (+pol p1 size 0.0) 0.5 0.0 0.5 1.0)
               (polygon-surface (list p20 (first base-pts-1) (last base-pts-1) p21) 0.3 0.5 0.5)
               (polygon-surface (list p20 p11 p10 (first base-pts-1)) 0.3 0.5 0.5)
               (polygon-surface (list p21 p11 p10 (last base-pts-1)) 0.3 0.5 0.5)
               (irregularPyramid p1 rhos phis p10 0.3 0.5 0.5)
               (cylinder p2 (/ size 3) (+z p2 size) 0.3 0.5 0.5))
         p (ux))
        (cylinder p (/ size 3) (+z p size) 0.3 0.5 0.5)
        
        ))))



(ship (xyz 5.0 0.0 0.0) 10.0)
(time
 (begin
   (send_data)
   (thread while)))

(read)