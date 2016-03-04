#lang racket
(require (except-in math random-integer slice?)
         "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt"
         "../util/geometry.rkt")

(provide (all-from-out "../base/coord.rkt"))
(provide (all-from-out "../base/utils.rkt"))
(provide (all-from-out "../base/shapes.rkt"))
(provide (all-from-out "../util/geometry.rkt"))

(require (prefix-in ffi: "fffidefiner.rkt"))
;(provide (all-from-out "fffidefiner.rkt"))

;(define 2pi (* pi 2))
;;;;;; Obejects functions

(provide city
         end_cycle?
         box cylinder
         right-cuboid
         pyramid
         sphere
         sphere2
         prism
         trunk
         reg-surface
         circle
         reg-line
         irregularPyramid
         irregularPyramid3
         irregularPyramid-pts
         floats<-pts
         line
         polygon-surface
         polygon
         point
         mirror
         view
         rotate)
(define city ffi:city)
(define (end_cycle?)
  (if (< (ffi:end_cycle2) 0)
      #f
      #t))

(define box ffi:box)

(define (cylinder p1 radius p2 [r 1.0] [g 1.0] [b 1.0])
  (let* ([comp (distance p1 p2)]
         [args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) radius radius comp 20.0 r g b))])
    (apply ffi:prismpts args)
    ))

(define (right-cuboid p1 w h p2 [r 1.0] [g 1.0] [b 1.0])
  (let* ([comp (distance p1 p2)]
         [args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) w h comp 4.0 r g b))])
    (apply ffi:prismpts args)
    )
  )

(define (pyramid p1 w l p2 [sides 4.0] [r 1.0] [g 1.0] [b 1.0])
  (let* ([comp (distance p1 p2)]
         [args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) w l comp sides r g b))])
    (apply ffi:pyramidpts args)
    )
  )
(define (sphere p1 radius [r 1.0] [g 1.0] [b 1.0])
  (let* ([args (map exact->inexact (list (cx p1) (cy p1) (cz p1) radius r g b))])
    (apply ffi:sphere args)
    ))
(define (sphere2 p1 radius [r 1.0] [g 1.0] [b 1.0])
  (let* ([args (map exact->inexact (list (cx p1) (cy p1) (cz p1) radius r g b))])
    (apply ffi:sphere2 args)
    ))

(define (prism p1 w h p2 sides [r 1.0] [g 1.0] [b 1.0])
  (let* ([comp (distance p1 p2)]
         [args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) w h comp sides r g b))])
    (apply ffi:prismpts args)
    )
  )

(define (trunk p1 w0 h0 w1 h1 p2 sides [r 1.0] [g 1.0] [b 1.0])
  (let* ([comp (distance p1 p2)]
         [args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) w0 h0 w1 h1 comp sides r g b))])
    (apply ffi:trunkpts args)
    )
  )

(define (circle center rad [alpha 0.0] [r 1.0] [g 1.0] [b 1.0])
  ;  (reg-line center 20 rad rad (uz) alpha r g b))
  (transform (reg-line (u0 world-cs) 20 (* 2 rad) (* 2 rad)) (loc->transformation-matrix center)))

(define (reg-surface p1 sides w l [v1 (uz)] [alpha 0.0] [r 1.0] [g 1.0] [b 1.0])
  (let* ([p2 (+c p1 v1)]
         [args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) sides w l r g b alpha))])
    (apply ffi:regSurface args)
    )
  )
(define (reg-line p1 sides w l [v1 (uz)] [alpha 0.0] [r 1.0] [g 1.0] [b 1.0])
  (let* ([p2 (+c p1 v1)]
         [args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) sides w l r g b alpha))])
    (displayln v1)
    (apply ffi:regLine args)
    )
  )

(define (irregularPyramid-pts base-pts top [r 1.0] [g 1.0] [b 1.0])
   (list (polygon-surface (list (first base-pts) (last base-pts) top) r g b)
          (polygon-surface base-pts r g b)
          (let aux ((pts base-pts))
            (if
             (> (length pts) 2)
             (cons (polygon-surface (append (take pts 2) (list top)) r g b)
                   (aux (drop pts 1)))
             (list (polygon-surface (append pts (list top)) r g b))))))

(define (irregularPyramid base-center rhos phis top [r 1.0] [g 1.0] [b 1.0])
  (let ([base-pts (for/list
                      ((rho rhos) (phi phis))
                    (+pol base-center rho phi))])
    (list (polygon-surface (list (first base-pts) (last base-pts) top) r g b)
          (polygon-surface base-pts r g b)
          (let aux ((pts base-pts))
            (if
             (> (length pts) 2)
             (cons (polygon-surface (append (take pts 2) (list top)) r g b)
                   (aux (drop pts 1)))
             (list (polygon-surface (append pts (list top)) r g b))))))             
  )


(define (irregularPyramid3 p1 p2 l1 a1 l2 a2 l3 a3 [r 1.0] [g 1.0] [b 1.0])
  (let ([args (map exact->inexact (list (cx p1) (cy p1) (cz p1) (cx p2) (cy p2) (cz p2) l1 a1 l2 a2 l3 a3 r g b))])
    (apply ffi:irregularPyramid3 args)
    )
  )

(define (floats<-pts pts)
  (if (null? pts)
      (list)
      (let ((pt (car pts)))
        (cons (exact->inexact (cx pt))
              (cons (exact->inexact (cy pt))
                    (cons (exact->inexact (cz pt))
                          (floats<-pts (cdr pts))))))))

(define (line pts [r 1.0] [g 1.0] [b 1.0])
  (displayln pts)
  (if
   (> (length pts) 4) 
   (let* ([args (append (list 4 (floats<-pts (take pts 4))) (map exact->inexact (list r g b)))])
     (cons (apply ffi:line args) (line (drop pts 3) r g b)))
   (let* ([args (append (list (length pts) (floats<-pts pts)) (map exact->inexact (list r g b)))])
     (list (apply ffi:line args)))))

(define (polygon-surface pts [r 1.0] [g 1.0] [b 1.0])
  (if (> (length pts) 3)
      (cons (ffi:triangle (floats<-pts (list (first pts) (second pts) (third pts))) r g b ) 
            (polygon-surface (append (list (first pts)) (rest (rest pts))) r g b))
      (list (ffi:triangle (floats<-pts (list (first pts) (second pts) (third pts))) r g b ))))

#;(define (line pts [r 1.0] [g 1.0] [b 1.0])
    (define result (list))
    (let aux ((pts pts))
      (if
       (> (length pts) 4)
       (let* ([args (append (list 4 (floats<-pts (take pts 4))) (map exact->inexact (list r g b)))]
              [id (apply ffi:line args)])
         (set! result (cons id result))
         (aux (drop pts 3)))   
       (let* ([args (append (list (length pts) (floats<-pts pts)) (map exact->inexact (list r g b)))]
              [id (apply ffi:line args)])
         (set! result (cons id result)))
       )
      )
    result)




#;(define (polygon pts [r 1.0] [g 1.0] [b 1.0])
    (define result (list))
    (let aux ((pts pts))
      (when (> (length pts) 2)
        (let ([id (ffi:triangle (floats<-pts (list (first pts) (second pts) (third pts))) r g b )])   
          (set! result (cons id result))
          (aux (append (list (first pts)) (rest (rest pts)))))))
    result)

(define (polygon pts [r 1.0] [g 1.0] [b 1.0])
  #;(line pts)
  (displayln pts)
  (if (> (length pts) 2)
      (cons (line (list (first pts) (second pts))) (polygon (rest pts) r g b))
      (line pts)
      )
  )

(define (point p1 [rad 0.5] [r 1.0] [g 1.0] [b 1.0])
  (let* ([args (map exact->inexact (list (cx p1) (cy p1) (cz p1) rad r g b))])
    (apply ffi:point args)
    )
  #;(let* ([args (map exact->inexact (list (cx p1) (cy p1) (cz p1) 1.5 1.5 1.5 r g b 0.0 0.0 0.0 0.0))])
      (apply ffi:box args)
      )
  )

(define (mirror shapes pt vec)
  (append (for/list ([shape (flatten shapes)])
            (ffi:mirror shape (floats<-pts (list pt)) (floats<-pts (list vec))))(flatten shapes) ))

;;;;;;;;;;;;;;;;;;;;; Transformations         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide view rotate transform)
(define (view camera-pos camera-look-at)
  (let ([args (map exact->inexact (list (cx camera-pos) (cy camera-pos) (cz camera-pos) (cx camera-look-at) (cy camera-look-at) (cz camera-look-at)))])
    (apply ffi:setView args)
    )
  )

(define (rotate id angle vec)
  (display "rotate ")
  (displayln id)
  (let ([args (list id angle (cx vec) (cy vec) (cz vec))])
    (displayln args)
    (apply ffi:rotate args)
    )
  )

(define (transform id mat)
  (display "transform ")
  (displayln id)
  (let ((mat-lst (map exact->inexact (flatten (map vector->list (vector->list mat))))))
    (ffi:transform id mat-lst)))

;;;;;;;;;;;;;;;;;;;;; rendering functions     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide init init2 pool clean end_cycle end_cycle2 cycle cycle2 send_data close start startth)
(define init ffi:init)
(define init2 ffi:init2)
(define pool ffi:pool)
(define clean ffi:clean)
(define cycle ffi:cycle)
(define cycle2 ffi:cycle2)
(define close ffi:close)
(define end_cycle ffi:end_cycle)
(define end_cycle2 ffi:end_cycle2)
(define send_data ffi:send_data)
(define start ffi:start)
(define startth ffi:startth)

(define args #f)
(define fn #f)
(define changed #f)

(provide setup update while)
(define (setup fun arg)
  (displayln arg)
  ;(init 100)
  (set! changed #t)
  (set! fn fun)
  (set! args arg))

(define (update arg)
  ;(displayln arg)
  (set! changed #t)
  (set! args arg))

(define (render-scene fn args)
  (begin
    (clean)
    ;(displayln args)
    (apply fn args)
    (send_data)))

(define (while)
  (unless (end_cycle?)
    (begin
      (when changed
        (begin 
          (render-scene fn args)
          (set! changed #f))
        )
      (pool)
      (cycle)
      (while))))
;;;;;;;;;;;;


;;;;;;;; Auxiliary functions
(define (media a b)
  (/ (+ a b) 2.0))

(provide mid-point)
(define (mid-point p0 p1)
  (xyz (media (cx p0) (cx p1))
       (media (cy p0) (cy p1))
       (media (cz p0) (cz p1))))

;;;;;;;;;;;;
(provide loc->transformation-matrix)
(define-syntax-rule (mf m i j) (real->double-flonum (matrix-ref m i j)))

(define (loc->transformation-matrix p)
  (let ((m (world-transformation p)))
    (vector (vector (mf m 0 0) (mf m 0 1) (mf m 0 2) (mf m 0 3))
            (vector (mf m 1 0) (mf m 1 1) (mf m 1 2) (mf m 1 3))
            (vector (mf m 2 0) (mf m 2 1) (mf m 2 2) (mf m 2 3))
            (vector (mf m 3 0) (mf m 3 1) (mf m 3 2) (mf m 3 3)))))