#lang typed/racket/base
(require racket/math math/matrix)
(require "utils.rkt")

(provide Cs
         Loc
         Vec
         Locs
         Vecs
         world-loc
         world-transformation
         world-cs
         world-cs?
         current-cs
         translated-cs
         scaled-cs
         x-rotated-cs
         y-rotated-cs
         z-rotated-cs
         xyz
         xy
         yz
         xz
         x
         y
         z
         cx cy cz
         vxyz
         vxy
         vyz
         vxz
         vx
         vy
         vz
         -vx
         -vy
         -vz
         u-vxyz
         unitize
         v*v
         v.v
         loc-in
         loc-in-world
         loc-in-cs
         vec-in-world
         =c?
         loc=?
         loc?
         +x +y +z +xy +xz +yz +xyz
         +vx +vy +vz +vxy +vxz +vyz +vxyz
         pol +pol
         vpol +vpol
         pol-rho pol-phi
         cyl +cyl
         vcyl +vcyl
         cyl-rho cyl-phi cyl-z
         sph +sph
         vsph +vsph
         sph-rho sph-phi sph-psi
         p+v p-v
         p-p v*r v/r
         v+v v-v
         +c -c *c /c
         intermediate-point ;;find better name?
         pi/6
         pi/5
         pi/4
         pi/3
         pi/2
         3pi/2
         2pi
         3pi
         4pi
         -pi/6
         -pi/5
         -pi/4
         -pi/3
         -pi/2
         -3pi/2
         -pi
         -2pi
         -3pi
         -4pi         
         coterminal
         distance
         sqr-distance
         position-and-height
         inverted-position-and-height
         Xyz
         VXyz
         u0 ux uy uz uxy uxz uyz uxyz
         
         translating
         scaling
         x-rotating
         y-rotating
         z-rotating
         with-cs

         cs-from-o-vx-vy-vz
         cs-from-o-vx-vy
         cs-from-o-vz
         cs-from-pts
         cs-from-o-n
         cs-from-o-phi
         loc-from-o-vx-vy
         loc-from-o-vz
         loc-from-pts
         loc-from-o-n
         loc-from-o-phi
         vf
         vlength
         perpendicular-vector)

;;The circle system
(define pi/6 (/ pi 6))
(define pi/5 (/ pi 5))
(define pi/4 (/ pi 4))
(define pi/3 (/ pi 3))
(define pi/2 (/ pi 2))
(define 3pi/2 (* 3 pi/2))
(define 2pi (* 2 pi))
(define 3pi (* 3 pi))
(define 4pi (* 4 pi))

(define -pi/6 (/ pi -6))
(define -pi/5 (/ pi -5))
(define -pi/4 (/ pi -4))
(define -pi/3 (/ pi -3))
(define -pi/2 (/ pi -2))
(define -3pi/2 (* -3 pi/2))
(define -pi (* -1 pi))
(define -2pi (* -2 pi))
(define -3pi (* -3 pi))
(define -4pi (* -4 pi))

;; Matrix operations

;;This should be useful to optimize code. Think about combining it with syntax/parse.
#;
(define-syntax (define/inline-macro stx)
  (syntax-case stx ()
    [(_ name pat inline-fun typed:fun)
     (syntax/loc stx
       (define-syntax (name inner-stx)
         (syntax-case inner-stx ()
           [(_ . pat)  (syntax/loc inner-stx (inline-fun . pat))]
           [(_ . es)   (syntax/loc inner-stx (typed:fun . es))]
           [_          (syntax/loc inner-stx typed:fun)])))]))  

;; Vector

(struct Cs
  ([transformation : (Matrix Real)]))

(define (translation-matrix [x : Real] [y : Real] [z : Real]) : (Matrix Real)
  (matrix [[1 0 0 x] 
           [0 1 0 y]
           [0 0 1 z]
           [0 0 0 1]]))

(define (scaling-matrix [x : Real] [y : Real] [z : Real]) : (Matrix Real)
  (matrix [[x 0 0 0]
           [0 y 0 0]
           [0 0 z 0]
           [0 0 0 1]]))

(define (x-rotation-matrix [phi : Real]) : (Matrix Real)
  (let ((s (sin phi))
        (c (cos phi)))
    (matrix [[1 0   0   0]
             [0 c (- s) 0]
             [0 s   c   0]
             [0 0   0   1]])))

(define (y-rotation-matrix [phi : Real]) : (Matrix Real)
  (let ((s (sin phi))
        (c (cos phi)))
    (matrix [[  c   0 s 0]
             [  0   1 0 0]
             [(- s) 0 c 0]
             [  0   0 0 1]])))

(define (z-rotation-matrix [phi : Real]) : (Matrix Real)
  (let ((s (sin phi))
        (c (cos phi)))
    (matrix [[c (- s) 0 0]
             [s   c   0 0]
             [0   0   1 0]
             [0   0   0 1]])))

(define world-transformation-matrix : (Matrix Real) (identity-matrix 4))
(define world-cs : Cs (Cs world-transformation-matrix))

(define (world-cs? [cs : Cs]) : Boolean
  (eq? (Cs-transformation cs) world-transformation-matrix))

(define (=cs? [cs0 : Cs] [cs1 : Cs]) : Boolean
  (or (eq? (Cs-transformation cs0) (Cs-transformation cs1))
      (matrix= (Cs-transformation cs0) (Cs-transformation cs1))))

(define (=c? [p0 : Loc] [p1 : Loc]) : Boolean
  (let ((p0 (loc-in-world p0))
        (p1 (loc-in-world p1)))
    (and (= (cx p0) (cx p1))
         (= (cy p0) (cy p1))
         (= (cz p0) (cz p1)))))

(define (loc=? [p0 : Loc] [p1 : Loc]) : Boolean
  (or (eq? p0 p1)
      (=c? p0 p1)))

(define current-cs : (Parameterof Cs) (make-parameter world-cs))

(define-syntax-rule
  (with-cs expr body ...)
  (parameterize ([current-cs expr]) body ...))

(define (translated-cs [dx : Real] [dy : Real] [dz : Real] [cs : Cs]) : Cs
  (Cs (matrix* (Cs-transformation cs) (translation-matrix dx dy dz))))

(define (scaled-cs [sx : Real] [sy : Real] [sz : Real] [cs : Cs]) : Cs
  (Cs (matrix* (Cs-transformation cs) (scaling-matrix sx sy sz))))

(define (x-rotated-cs [phi : Real] [cs : Cs]) : Cs
  (Cs (matrix* (Cs-transformation cs) (x-rotation-matrix phi))))

(define (y-rotated-cs [phi : Real] [cs : Cs]) : Cs
  (Cs (matrix* (Cs-transformation cs) (y-rotation-matrix phi))))

(define (z-rotated-cs [phi : Real] [cs : Cs]) : Cs
  (Cs (matrix* (Cs-transformation cs) (z-rotation-matrix phi))))

(define-syntax-rule
  (translating dx dy dz body ...)
  (parameterize ((current-cs (translated-cs dx dy dz (current-cs))))
    body ...))

(define-syntax-rule
  (scaling s body ...)
  (let ((r s))
    (parameterize ((current-cs (scaled-cs r r r (current-cs))))
      body ...)))

(define-syntax-rule
  (x-rotating phi body ...)
  (parameterize ((current-cs (x-rotated-cs phi (current-cs))))
    body ...))

(define-syntax-rule
  (y-rotating phi body ...)
  (parameterize ((current-cs (y-rotated-cs phi (current-cs))))
    body ...))

(define-syntax-rule
  (z-rotating phi body ...)
  (parameterize ((current-cs (z-rotated-cs phi (current-cs))))
    body ...))

;;Location in Cartesian Coordinates

(define (fprint-Xyz [xyz : Xyz] [port : Output-Port] [mode : Any]) : Output-Port
  (fprintf port "#<xyz:~A ~A ~A" (cx xyz) (cy xyz) (cz xyz))
  (unless (world-cs? xyz)
    (write-string " @ " port)
    (write "..." port)) ; Perhaps a Cs id would be good
  (write-string ">" port)
  port)

(define-type Loc Xyz)
(define-type Vec VXyz)
(define-type Locs (Listof Loc))
(define-type Vecs (Listof Vec))

(struct Xyz Cs
  ([loc : (Matrix Real)])
  #:property
  prop:custom-write fprint-Xyz)

(define loc? Xyz?)

;;Let's speedup a little by using syntax instead of a function
(define-syntax-rule
  (col-idx m i)
  (matrix-ref m i 0))

(define (cx [p : Loc]) : Real
  (col-idx (Xyz-loc p) 0))

(define (cy [p : Loc]) : Real
  (col-idx (Xyz-loc p) 1))

(define (cz [p : Loc]) : Real
  (col-idx (Xyz-loc p) 2))

(define (xyz [x : Real] [y : Real] [z : Real] [cs : Cs (current-cs)]) : Loc
  (Xyz (Cs-transformation cs)
       (col-matrix [x y z 1])))

(define (xy [x : Real] [y : Real] [cs : Cs (current-cs)]) : Loc (xyz x y 0 cs))
(define (xz [x : Real] [z : Real] [cs : Cs (current-cs)]) : Loc (xyz x 0 z cs))
(define (yz [y : Real] [z : Real] [cs : Cs (current-cs)]) : Loc (xyz 0 y z cs))
(define (x [x : Real 1] [cs : Cs (current-cs)]) : Loc (xyz x 0 0 cs))
(define (y [y : Real 1] [cs : Cs (current-cs)]) : Loc (xyz 0 y 0 cs))
(define (z [z : Real 1] [cs : Cs (current-cs)]) : Loc (xyz 0 0 z cs))

(define (u0 [cs : Cs (current-cs)]) : Loc (xyz 0 0 0 cs))
(define (ux [cs : Cs (current-cs)]) : Loc (xyz 1 0 0 cs))
(define (uy [cs : Cs (current-cs)]) : Loc (xyz 0 1 0 cs))
(define (uz [cs : Cs (current-cs)]) : Loc (xyz 0 0 1 cs))
(define (-ux [cs : Cs (current-cs)]) : Loc (xyz -1 0 0 cs))
(define (-uy [cs : Cs (current-cs)]) : Loc (xyz 0 -1 0 cs))
(define (-uz [cs : Cs (current-cs)]) : Loc (xyz 0 0 -1 cs))
(define (uxy [cs : Cs (current-cs)]) : Loc (xyz 1 1 0 cs))
(define (uyz [cs : Cs (current-cs)]) : Loc (xyz 0 1 1 cs))
(define (uxz [cs : Cs (current-cs)]) : Loc (xyz 1 0 1 cs))
(define (-uxy [cs : Cs (current-cs)]) : Loc (xyz -1 -1 0 cs))
(define (-uyz [cs : Cs (current-cs)]) : Loc (xyz 0 -1 -1 cs))
(define (-uxz [cs : Cs (current-cs)]) : Loc (xyz -1 0 -1 cs))
(define (uxyz [cs : Cs (current-cs)]) : Loc (xyz 1 1 1 cs))
(define (-uxyz [cs : Cs (current-cs)]) : Loc (xyz -1 -1 -1 cs))

(define (+xyz [p : Loc] [dx : Real] [dy : Real] [dz : Real]) : Loc
  (Xyz (Cs-transformation p)
       (matrix+ (Xyz-loc p) (col-matrix [dx dy dz 0]))))

(define (+x [p : Loc] [dx : Real]) : Loc
  (+xyz p dx 0 0))

(define (+y [p : Loc] [dy : Real]) : Loc
  (+xyz p 0 dy 0))

(define (+z [p : Loc] [dz : Real]) : Loc
  (+xyz p 0 0 dz))

(define (+xy [p : Loc] [dx : Real] [dy : Real]) : Loc
  (+xyz p dx dy 0))

(define (+xz [p : Loc] [dx : Real] [dz : Real]) : Loc
  (+xyz p dx 0 dz))

(define (+yz [p : Loc] [dy : Real] [dz : Real]) : Loc
  (+xyz p 0 dy dz))


;;Vector in Cartesian Coordinates

(define (fprint-VXyz [xyz : Vec] [port : Output-Port] [mode : Any]) : Output-Port
  (fprintf port "#<vxyz:~A ~A ~A" (cx xyz) (cy xyz) (cz xyz))
  (unless (world-cs? xyz)
    (write-string " @ " port)
    (write "..." port))
  (write-string ">" port)
  port)

(struct VXyz Xyz
  ()
  #:property
  prop:custom-write fprint-VXyz)

(define (vxyz [x : Real] [y : Real] [z : Real] [cs : Cs (current-cs)]) : Vec
  (VXyz (Cs-transformation cs)
        (col-matrix [x y z 0])))

(define (vxy [x : Real] [y : Real] [cs : Cs (current-cs)]) (vxyz x y 0 cs))
(define (vxz [x : Real] [z : Real] [cs : Cs (current-cs)]) (vxyz x 0 z cs))
(define (vyz [y : Real] [z : Real] [cs : Cs (current-cs)]) (vxyz 0 y z cs))
(define (vx [x : Real 1] [cs : Cs (current-cs)]) (vxyz x 0 0 cs))
(define (vy [y : Real 1] [cs : Cs (current-cs)]) (vxyz 0 y 0 cs))
(define (vz [z : Real 1] [cs : Cs (current-cs)]) (vxyz 0 0 z cs))
(define (-vx [x : Real 1] [cs : Cs (current-cs)]) (vxyz (- x) 0 0 cs))
(define (-vy [y : Real 1] [cs : Cs (current-cs)]) (vxyz 0 (- y) 0 cs))
(define (-vz [z : Real 1] [cs : Cs (current-cs)]) (vxyz 0 0 (- z) cs))


(define (+vxyz [p : Vec] [dx : Real] [dy : Real] [dz : Real]) : Vec
  (VXyz (Cs-transformation p)
        (matrix+ (Xyz-loc p) (col-matrix [dx dy dz 0]))))

(define (+vx [p : Vec] [dx : Real]) : Vec
  (+vxyz p dx 0 0))

(define (+vy [p : Vec] [dy : Real]) : Vec
  (+vxyz p 0 dy 0))

(define (+vz [p : Vec] [dz : Real]) : Vec
  (+vxyz p 0 0 dz))

(define (+vxy [p : Vec] [dx : Real] [dy : Real]) : Vec
  (+vxyz p dx dy 0))

(define (+vxz [p : Vec] [dx : Real] [dz : Real]) : Vec
  (+vxyz p dx 0 dz))

(define (+vyz [p : Vec] [dy : Real] [dz : Real]) : Vec
  (+vxyz p 0 dy dz))



(define (world-loc [p : Xyz]) : (Matrix Real)
  (if (world-cs? p)
      (Xyz-loc p)
      (matrix* (Cs-transformation p)
               (Xyz-loc p))))

(define (world-transformation [p : Xyz]) : (Matrix Real)
  (if (world-cs? p)
      (translation-matrix (cx p) (cy p) (cz p))
      (matrix* (Cs-transformation p)
               (translation-matrix (cx p) (cy p) (cz p)))))

;;Convert points to world
(define (loc-in-world [p : Loc]) : Loc 
  (Xyz (Cs-transformation world-cs) (world-loc p)))

;;Convert points to other cs
(define (loc-in-cs [p : Loc] [cs : Cs]) : Loc
  (Xyz (Cs-transformation cs)
       (matrix* (matrix-inverse (Cs-transformation cs))
                (world-loc p))))

(define (loc-in [p : Loc] [q : Loc]) : Loc
  (loc-in-cs p q))

;;Convert vectors to world
(define (vec-in-world [v : Vec]) : Vec
  (VXyz (Cs-transformation world-cs) (world-loc v)))

(define (vec-in-cs [v : Vec] [cs : Cs]) : Vec
  (VXyz (Cs-transformation cs)
        (matrix* (matrix-inverse (Cs-transformation cs))
                 (world-loc v))))

(define (sqr-distance [p0 : Loc] [p1 : Loc]) : Real
  (let ((c0 (world-loc p0))
        (c1 (world-loc p1)))
    (+ (sqr (- (col-idx c1 0) (col-idx c0 0)))
       (sqr (- (col-idx c1 1) (col-idx c0 1)))
       (sqr (- (col-idx c1 2) (col-idx c0 2))))))

(define (distance [p0 : Loc] [p1 : Loc]) : Real
  (cast (sqrt (sqr-distance p0 p1)) Real))

(define (p-p [p0 : Loc] [p1 : Loc]) : Vec
  (let-values (((c0 c1)
                (if (=cs? p0 p1)
                    (values (Xyz-loc p0) (Xyz-loc p1))
                    (values (world-loc p0) (world-loc p1)))))
    (vxyz (- (col-idx c0 0) (col-idx c1 0))
          (- (col-idx c0 1) (col-idx c1 1))
          (- (col-idx c0 2) (col-idx c1 2))
          (if (=cs? p0 p1)
              p0
              world-cs))))

(define (p+v [p : Loc] [v : Vec]) : Loc
  (let-values (((c0 c1)
                (if (=cs? p v)
                    (values (Xyz-loc p) (Xyz-loc v))
                    (values (world-loc p) (world-loc v)))))
    (xyz (+ (col-idx c0 0) (col-idx c1 0))
         (+ (col-idx c0 1) (col-idx c1 1))
         (+ (col-idx c0 2) (col-idx c1 2))
         (if (=cs? p v)
             p
             world-cs))))

(define (p-v [p : Loc] [v : Vec]) : Loc
  (let-values (((c0 c1)
                (if (=cs? p v)
                    (values (Xyz-loc p) (Xyz-loc v))
                    (values (world-loc p) (world-loc v)))))
    (xyz (- (col-idx c0 0) (col-idx c1 0))
         (- (col-idx c0 1) (col-idx c1 1))
         (- (col-idx c0 2) (col-idx c1 2))
         (if (=cs? p v)
             p
             world-cs))))

(define (v+v [v0 : Vec] [v1 : Vec]) : Vec
  (let-values (((c0 c1)
                (if (=cs? v0 v1)
                    (values (Xyz-loc v0) (Xyz-loc v1))
                    (values (world-loc v0) (world-loc v1)))))
    (vxyz (+ (col-idx c0 0) (col-idx c1 0))
          (+ (col-idx c0 1) (col-idx c1 1))
          (+ (col-idx c0 2) (col-idx c1 2))
          (if (=cs? v0 v1)
              v0
              world-cs))))

(define (v-v [v0 : Vec] [v1 : Vec]) : Vec
  (let-values (((c0 c1)
                (if (=cs? v0 v1)
                    (values (Xyz-loc v0) (Xyz-loc v1))
                    (values (world-loc v0) (world-loc v1)))))
    (vxyz (- (col-idx c0 0) (col-idx c1 0))
          (- (col-idx c0 1) (col-idx c1 1))
          (- (col-idx c0 2) (col-idx c1 2))
          (if (=cs? v0 v1)
              v0
              world-cs))))

(define (v*r [v : Vec] [r : Real]) : Vec
  (let ((c (Xyz-loc v)))
    (vxyz (* (col-idx c 0) r)
          (* (col-idx c 1) r)          
          (* (col-idx c 2) r)
          v)))

(define (v/r [v : Vec] [r : Real]) : Vec
  (let ((c (Xyz-loc v)))
    (vxyz (/ (col-idx c 0) r)
          (/ (col-idx c 1) r)          
          (/ (col-idx c 2) r)
          v)))

;;The following ones are semantically incorrect but those who know what they are doing...

(define (-c [p0 : Loc] [p1 : Loc]) : Loc
  (let-values (((c0 c1)
                (if (=cs? p0 p1)
                    (values (Xyz-loc p0) (Xyz-loc p1))
                    (values (world-loc p0) (world-loc p1)))))
    (xyz (- (col-idx c0 0) (col-idx c1 0))
         (- (col-idx c0 1) (col-idx c1 1))
         (- (col-idx c0 2) (col-idx c1 2))
         (if (=cs? p0 p1)
             p0
             world-cs))))

(define (+c [p : Loc] [v : Loc]) : Loc
  (let-values (((c0 c1)
                (if (=cs? p v)
                    (values (Xyz-loc p) (Xyz-loc v))
                    (values (world-loc p) (world-loc v)))))
    (xyz (+ (col-idx c0 0) (col-idx c1 0))
         (+ (col-idx c0 1) (col-idx c1 1))
         (+ (col-idx c0 2) (col-idx c1 2))
         (if (=cs? p v)
             p
             world-cs))))

(define (*c [v : Loc] [r : Real]) : Loc
  (let ((c (Xyz-loc v)))
    (xyz (* (col-idx c 0) r)
         (* (col-idx c 1) r)          
         (* (col-idx c 2) r)
         v)))

(define (/c [v : Loc] [r : Real]) : Loc
  (let ((c (Xyz-loc v)))
    (xyz (/ (col-idx c 0) r)
         (/ (col-idx c 1) r)          
         (/ (col-idx c 2) r)
         v)))

(define-syntax-rule
  (let-coords ([(x y z) p] ...) body ...)
  (let ([x (cx p)] ...
        [y (cy p)] ...
        [z (cz p)] ...)
    body ...))

(define (v*v [v0 : Vec] [v1 : Vec]) : Vec
  (let ((v0 (vec-in-world v0))
        (v1 (vec-in-world v1)))
    (let-coords (((x0 y0 z0) v0)
                 ((x1 y1 z1) v1))
      (vxyz (- (* y0 z1) (* z0 y1))
            (- (* z0 x1) (* x0 z1))
            (- (* x0 y1) (* y0 x1))
            world-cs))))

(define (v.v [v0 : Vec] [v1 : Vec]) : Real
  (let ((v0 (vec-in-world v0))
        (v1 (vec-in-world v1)))
    (let-coords (((x0 y0 z0) v0)
                 ((x1 y1 z1) v1))
      (+ (* x0 x1) (* y0 y1) (* z0 z1)))))

(define (vlength [v : Vec]) : Real
  (let ((x (cx v)) (y (cy v)) (z (cz v)))
    (cast (sqrt (+ (* x x) (* y y) (* z z))) Real)))

(define (u-vxyz [x : Real] [y : Real] [z : Real] [cs : Cs (current-cs)]) : Vec
  (let ((l (cast (sqrt (+ (* x x) (* y y) (* z z))) Real)))
    (vxyz (/ x l) (/ y l) (/ z l) cs)))

(define (unitize [v : Vec]) : Vec
  (u-vxyz (cx v) (cy v) (cz v) v))

(define (cs-from-o-vx-vy-vz [o : Loc] [ux : Vec] [uy : Vec] [uz : Vec]) : Cs
  (Cs (matrix [[(cx ux) (cx uy) (cx uz) (cx o)]
               [(cy ux) (cy uy) (cy uz) (cy o)]
               [(cz ux) (cz uy) (cz uz) (cz o)]
               [      0       0       0      1]])))

(define (cs-from-o-vx-vy [o : Loc] [vx : Vec] [vy : Vec]) : Cs
  (let ((o (loc-in-world o))
        (vx (vec-in-world vx))
        (vy (vec-in-world vy)))
    (let ((vx (unitize vx))
          (vz (unitize (v*v vx vy))))
      (let ((vy (v*v vz vx)))
        (cs-from-o-vx-vy-vz o vx vy vz)))))

;;HACK We should not need this but Typed Racket asks for it.
(: vpol (->* (Real Real) (Cs) Vec))

(define (cs-from-o-vz [o : Loc] [n : Vec])
  (if (and (=cs? o n) (= (cx n) 0) (= (cy n) 0)) ;n is o's Z?
      o
      (let ((o (loc-in-world o))
            (n (vec-in-world n)))
        (let ((vx (vpol 1 (+ (sph-phi n) pi/2))))
          (let ((vy (unitize (v*v vx n))))
            (let ((vz (unitize n)))
              (cs-from-o-vx-vy-vz o vx vy vz)))))))

(define (~zero? [x : Real]) : Boolean
  (< (abs x) 1e-14))

;;Is this still needed?
(define (perpendicular-vector [v : Vec]) : Vec
  (let-coords (((x y z) v))
    (cond ((~zero? x)
           (vxyz 1 0 0 v))
          ((~zero? y)
           (vxyz 0 -1 0 v))
          ((~zero? z)
           (vxyz 0 0 1 v))
          (else
           (let ((x z)
                 (y z)
                 (z (- (+ x y))))
             (vxyz x y z v))))))

(define (cs-from-pts [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc]) : Cs
  (let ((p0 (loc-in-world p0))
        (p1 (loc-in-world p1))
        (p2 (loc-in-world p2))
        (p3 (loc-in-world p3)))
    (let ((n0 (p-p p1 p0))
          (n1 (p-p p2 p0)))
      (let ((n2 (v*v n0 n1)))
        (if (< (v.v n2 (p-p p3 p0)) 0)
            (cs-from-o-vx-vy p0 n0 n1)
            (cs-from-o-vx-vy p0 n1 n0))))))

;;Should we eliminate cs-from-o-n?
(define (cs-from-o-n [o : Loc] [n : Vec])
  (cs-from-o-vz o n))

(define (cs-from-o-phi [o : Loc] [phi : Real]) : Cs
  (let ((o (loc-in-world o))
        (vx (vec-in-world (vcyl 1 phi 0 o)))
        (vy (vec-in-world (vcyl 1 (+ phi pi/2) 0 o))))
    (let ((vz (v*v vx vy)))
      (cs-from-o-vx-vy-vz o vx vy vz))))

(define (loc-from-o-vx-vy [o : Loc] [vx : Vec] [vy : Vec]) : Loc
  (u0 (cs-from-o-vx-vy o vx vy)))

(define (loc-from-o-vz [o : Loc] [vz : Vec]) : Loc
  (u0 (cs-from-o-vz o vz)))

(define (loc-from-pts [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc]) : Loc
  (u0 (cs-from-pts p0 p1 p2 p3)))

;;Should we eliminate loc-from-o-n?
(define (loc-from-o-n [o : Loc] [n : Vec]) : Loc
  (u0 (cs-from-o-n o n)))

(define (loc-from-o-phi [o : Loc] [phi : Real]) : Loc
  (u0 (cs-from-o-phi o phi)))

;;;

(define (cyl [rho : Real] [phi : Real] [z : Real] [cs : Cs (current-cs)])
  (xyz (* rho (cos phi))
       (* rho (sin phi))
       z
       cs))

(define (vcyl [rho : Real] [phi : Real] [z : Real] [cs : Cs (current-cs)])
  (vxyz (* rho (cos phi))
        (* rho (sin phi))
        z
        cs))

(define (+cyl [p : Loc] [rho : Real] [phi : Real] [z : Real]) : Loc
  (p+v p (vcyl rho phi z p)))

(define (+vcyl [v : Vec] [rho : Real] [phi : Real] [z : Real]) : Vec
  (v+v v (vcyl rho phi z v)))

(define (cyl-rho [p : Loc]) : Real
  (let ((x (cx p))
        (y (cy p)))
    (cast (sqrt (+ (* x x) (* y y))) Real)))

(define (cyl-phi [p : Loc]) : Real
  (sph-phi p))

(define (cyl-z [p : Loc]) : Real
  (cz p))

(define (pol [rho : Real] [phi : Real] [cs : Cs (current-cs)]) : Loc
  (cyl rho phi 0 cs))

(define (vpol [rho : Real] [phi : Real] [cs : Cs (current-cs)]) : Vec
  (vcyl rho phi 0 cs))



(define (+pol [p : Loc] [rho : Real] [phi : Real]) : Loc
  (+cyl p rho phi 0))

(define (+vpol [v : Vec] [rho : Real] [phi : Real]) : Vec
  (+vcyl v rho phi 0))


(define pol-rho cyl-rho)

(define pol-phi cyl-phi)

(define (sph [rho : Real] [phi : Real] [psi : Real] [cs : Cs (current-cs)]) : Loc
  (let ((sin-psi (sin psi)))
    (xyz (* rho (cos phi) sin-psi)
         (* rho (sin phi) sin-psi)
         (* rho (cos psi))
         cs)))

(define (vsph [rho : Real] [phi : Real] [psi : Real] [cs : Cs (current-cs)]) : Vec
  (let ((sin-psi (sin psi)))
    (vxyz (* rho (cos phi) sin-psi)
          (* rho (sin phi) sin-psi)
          (* rho (cos psi))
          cs)))

(define (+sph [p : Loc] [rho : Real] [phi : Real] [psi : Real]) : Loc
  (p+v p (vsph rho phi psi p)))

(define (+vsph [v : Vec] [rho : Real] [phi : Real] [psi : Real]) : Vec
  (v+v v (vsph rho phi psi v)))

(define (sph-rho [p : Loc]) : Real
  (let ((x (cx p))
        (y (cy p))
        (z (cz p)))
    (cast (sqrt (+ (* x x) (* y y) (* z z))) Real)))

(define (sph-phi [p : Loc]) : Real
  (let ((x (cx p))
        (y (cy p)))
    (if (= 0 x y)
        0
        (atan (cy p) (cx p)))))

(define (sph-psi [p : Loc]) : Real
  (let ((x (cx p))
        (y (cy p))
        (z (cz p)))
    (if (= 0 x y z)
        0
        (atan (cast (sqrt (+ (* x x) (* y y))) Real)
              z))))

(define (position-and-height [p0 : Loc] [h/p1 : (U Loc Real)]) : (Values Loc Real)
  (if (number? h/p1)
      (values p0 h/p1)
      (let ((p1 h/p1))
        (values (loc-from-o-n p0 (p-p p1 p0))
                (distance p0 p1)))))

(define (inverted-position-and-height  [p0 : Loc] [h/p1 : (U Loc Real)]) : (Values Loc Real)
  (if (number? h/p1)
      (values (loc-from-o-n (+z p0 h/p1) (vz -1 p0))
              h/p1)
      (values (loc-from-o-n h/p1 (p-p p0 h/p1))
              (distance p0 h/p1))))

(define (intermediate-point [p0 : Loc] [p1 : Loc] [t : Real 1/2]) : Loc
  (p+v p0 (v*r (p-p p1 p0) t)))

(define (coterminal [radians : Real]) : Real
  (let ((k (truncate (/ radians 2pi))))
    (- radians (* k 2pi))))

;;Conversions

;;vector of doubles
(provide loc->vector-double-flonum)
(define (loc->vector-double-flonum [p : Loc]) : (Vector Float Float Float)
  (let ((loc (world-loc p)))
    (vector (real->double-flonum (col-idx loc 0))
            (real->double-flonum (col-idx loc 1))
            (real->double-flonum (col-idx loc 2)))))


(provide vector-double-flonum->loc)
(define (vector-double-flonum->loc [v : (Vector Float Float Float)]) : Loc
  (xyz (vector-ref v 0)
       (vector-ref v 1)
       (vector-ref v 2)
       world-cs))

(provide vector-double-flonum->vec)
(define (vector-double-flonum->vec [v : (Vector Float Float Float)]) : Vec
  (vxyz (vector-ref v 0)
        (vector-ref v 1)
        (vector-ref v 2)
        world-cs))


(define-syntax-rule (mf m i j) (real->double-flonum (matrix-ref m i j)))
(define-syntax-rule (vf v i j) (vector-ref (vector-ref v i) j))

;;array of doubles
(provide loc->matrix-double-flonum)
(define (loc->matrix-double-flonum [p : Loc]) 
  : (Vector (Vector Float Float Float Float)
            (Vector Float Float Float Float)
            (Vector Float Float Float Float)
            (Vector Float Float Float Float))
  (let ((m (world-transformation p)))
    (vector (vector (mf m 0 0) (mf m 0 1) (mf m 0 2) (mf m 0 3))
            (vector (mf m 1 0) (mf m 1 1) (mf m 1 2) (mf m 1 3))
            (vector (mf m 2 0) (mf m 2 1) (mf m 2 2) (mf m 2 3))
            (vector (mf m 3 0) (mf m 3 1) (mf m 3 2) (mf m 3 3)))))

(provide matrix-double-flonum->loc)
(define (matrix-double-flonum->loc
         [m : (Vector (Vector Float Float Float Float)
                      (Vector Float Float Float Float)
                      (Vector Float Float Float Float)
                      (Vector Float Float Float Float))]) : Loc
      (Xyz (matrix [[(vf m 0 0) (vf m 0 1) (vf m 0 2) (vf m 0 3)]
                    [(vf m 1 0) (vf m 1 1) (vf m 1 2) (vf m 1 3)]
                    [(vf m 2 0) (vf m 2 1) (vf m 2 2) (vf m 2 3)]
                    [(vf m 3 0) (vf m 3 1) (vf m 3 2) (vf m 3 3)]])
           (col-matrix [0 0 0 1])))

(provide loc->linear-matrix-double-flonum)
(define (loc->linear-matrix-double-flonum [p : Loc]) 
  : (Vector Float Float Float
            Float Float Float
            Float Float Float
            Float Float Float)
  (let ((m (world-transformation p)))
    (vector (mf m 0 0) (mf m 0 1) (mf m 0 2)
            (mf m 1 0) (mf m 1 1) (mf m 1 2)
            (mf m 2 0) (mf m 2 1) (mf m 2 2)
            (mf m 3 0) (mf m 3 1) (mf m 3 2))))


(provide loc->)
(define #:forall (R) (loc-> [p : Loc]
                            [f : (-> Float Float Float Float
                                     Float Float Float Float
                                     Float Float Float Float
                                     Float Float Float Float
                                     R)])
                  : R
  (let ((m (world-transformation p)))
    (f (mf m 0 0) (mf m 0 1) (mf m 0 2) (mf m 0 3)
       (mf m 1 0) (mf m 1 1) (mf m 1 2) (mf m 1 3)
       (mf m 2 0) (mf m 2 1) (mf m 2 2) (mf m 2 3)
       (mf m 3 0) (mf m 3 1) (mf m 3 2) (mf m 3 3))))

;;vector of doubles
(provide locs->vector-3-double-flonums)
(define (locs->vector-3-double-flonums [cs : (Listof Loc)]) : (Vectorof Float)
  (let ((v ((inst make-vector Float) (* (length cs) 3) 0.0))
        (i 0))
    (for ((c (in-list cs)))
      (let ((c (loc-in-world c)))
        (let ((x (real->double-flonum (cx c)))
              (y (real->double-flonum (cy c)))
              (z (real->double-flonum (cz c))))
          (vector-set! v (+ i 0) x)
          (vector-set! v (+ i 1) y)
          (vector-set! v (+ i 2) z)
          (set! i (+ i 3)))))
    v))

(provide vector-3-double-flonums->locs)
(define (vector-3-double-flonums->locs [v : (Vectorof Float)]) : (Listof Loc)
  (for/list ((i (in-range 0 (vector-length v) 3)))
    (xyz
     (vector-ref v (+ i 0))
     (vector-ref v (+ i 1))
     (vector-ref v (+ i 2)))))

(provide locs->vector-2-double-flonums)
(define (locs->vector-2-double-flonums [cs : (Listof Loc)]) : (Vectorof Float)
  (let ((v ((inst make-vector Float) (* (length cs) 2) 0.0))
        (i 0))
    (for ((c (in-list cs)))
      (let ((c (loc-in-world c)))
        (let ((x (real->double-flonum (cx c)))
              (y (real->double-flonum (cy c))))
          (vector-set! v (+ i 0) x)
          (vector-set! v (+ i 1) y)
          (set! i (+ i 2)))))
    v))

(provide vector-2-double-flonums->locs)
(define (vector-2-double-flonums->locs [v : (Vectorof Float)]) : (Listof Loc)
  (for/list ((i (in-range 0 (vector-length v) 2)))
    (xy (vector-ref v (+ i 0))
        (vector-ref v (+ i 1)))))


;;vector of vector of doubles
(provide locs->vector-vector-double-flonum)
(define (locs->vector-vector-double-flonum [cs : (Listof Loc)]) : (Vectorof (Vector Float Float Float))
  (let ((v ((inst make-vector (Vector Float Float Float)) (length cs) (vector 0.0 0.0 0.0))))
    (for ((c (in-list cs))
          (i (in-naturals)))
      (vector-set! v i (loc->vector-double-flonum c)))
    v))

(provide vector-vector-double-flonum->locs)
(define (vector-vector-double-flonum->locs [v : (Vectorof (Vector Float Float Float))]) : (Listof Loc)
  (for/list ((e (in-vector v)))
    (vector-double-flonum->loc e)))

(provide loc->list-real)
(define (loc->list-real [p : Loc]) : (List Real Real Real)
  (let ((loc (world-loc p)))
    (list (col-idx loc 0)
          (col-idx loc 1)
          (col-idx loc 2))))

(provide regular-polygon-vertices)
(define (regular-polygon-vertices [edges : Integer 3]
                                  [center : Loc (u0)]
                                  [radius : Real 1]
                                  [angle : Real 0]
                                  [inscribed? : Boolean #f]) : (Listof Loc)
  (let ((r (if inscribed? radius (/ radius (cos (/ pi edges)))))) ;;from inscribed to circumscribed 
    (for/list : (Listof Loc) ((a (division angle (+ angle 2pi) edges #f))) 
      (+pol center r a))))

