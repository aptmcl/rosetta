#lang racket
(require rosetta/glfast)
#;(require
  "backend.rkt"
  "sliders.rkt")

(init 100)


#;(let ((p1 (xyz 0 0 0))
      (p2 (xyz 10 0 0))
      (p3 (xyz 10 10 0))
      (p31 (xyz 05 10 0))
      (p4 (xyz 0 10 0))
      (p41 (xyz 0 5 0))
      (p5 (xyz 0 0 1)))
  (point p1)  (point p2)
  (line (list p1 p2 p3 p4 p1)))
; p3 p4 p1

(define (p-1 p)
  (+y p 0)
  )

(let ((p0 (xyz 14 1 0)) (p1 (xyz 14 5 6)) (p2 (xyz -4 -2 -3)))
    (point p0 0.5) (point p1 0.5 0.0 1.0 0.0) (point p2 0.5)
    (line (map p-1 (list p0 p1 p2 p0)))
    (let-values (((c r) (circle-from-three-points p0 p1 p2)))
      (display "center ")(displayln c)(display "raio ")(displayln r)
      (display "mat ")(displayln (loc->transformation-matrix c))
      ;(reg-line c 12 r r)
      (circle c r 1.0 0.0 0.0)
      ;(transform (circle (xy (cx c)(cy c)) r 0.0 1.0 0.0 0.0) (loc->transformation-matrix c))
      ;(circle (u0) 10.0)
      ))


#;(let ((p0 (xyz 11 1 0)) (p1 (xyz 11 5 2)) (p2 (xyz 11 -2 -3)))
    (point p0 0.5) (point p1 0.5 1.0 1.0 0.0) (point p2 0.5)
    (line (list p0 p1 p2 p0))
    (let-values (((c r) (circle-from-three-points p0 p1 p2)))
      (display "center ")(displayln c)(display "raio ")(displayln r)
      (display "mat ")(displayln (loc->transformation-matrix c))
      ;(reg-line c 12 r r)
      (circle c r 1.0 1.0 1.0 0.0)
      ;(transform (circle (xy (cx c)(cy c)) r 0.0 1.0 0.0 0.0) (loc->transformation-matrix c))
      ;(circle (u0) 10.0)
      ))

(time
 (begin
   (send_data)
   (thread while)))
(read)