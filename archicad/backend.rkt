#lang typed/racket/base/no-check
(require racket/math
         racket/list
         racket/function)
(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt")
(provide (all-from-out "../base/coord.rkt"))
(provide (all-from-out "../base/utils.rkt"))
(provide (all-from-out "../base/shapes.rkt"))
(require (prefix-in % "geometry.rkt"))
(require (prefix-in % "objects.rkt"))
(require (prefix-in % "communication.rkt"))
(provide immediate-mode?
         current-backend-name
         (rename-out [%disconnect disconnect]
                     [%send send]))
;;This needs to be fixed to only provide what is relevant
(require "objects.rkt")
(provide (all-from-out "objects.rkt"))
         ;;This needs to be fixed to only provide what is relevant
(require "geometry.rkt")
(provide (all-from-out "geometry.rkt"))

(provide
 #|         mt
         ft
         box
         boxb
         cylinder
         cylinderb
         cylinder-metric
         sphere
         sphere-metric
         wall-h
         wall-l
         curtain-wall
         mass-wall
         insert-door
         insert-door-relative
         insert-window
         delete-element
         create-level
         upper-level
         current-level
         get-level
         delete-level
         create-round-floor
         create-floor
         create-floor-opening
         create-stairs-run
         intersect-wall-floor
         default-level-to-level-height
         disconnect-from-revit
         create-wall
         slab
         roof
         create-walls-from-slab
         create-hole-slab
         column
         intersect-wall
         current-level-elevation
         create-railings
         get-wall-volume
         create-stairs
         levels-info
         walls-info
         create-topo-surface
         create-building-pad
         get-level-by-name
         highlight-element
         get-selected-element
         mass-sweep
         extrusion-mass
         import-dwg
         move-element
         rotate-element
         beam
         load-family
         family-element
|#         
#|         all-shapes
         bounding-box
         delete-shape
         delete-shapes
|#
         delete-all-shapes
#|         curve-start-point
         curve-end-point
         curve-domain
         curve-length
         ;curve-tangent-at
         ;curve-normal-at
         ;curve-point-at
         curve-frame-at
         curve-frame-at-length
|#
         enable-update
         disable-update
#|         prompt-point
         prompt-integer
         prompt-real
         prompt-shape
         view
         view-top
         render-view
         select-shape
         select-shapes
         zoom-extents
|#)

(define-type Ref Any)
(define-type Shape Any)
(define-type Shapes Any)

(provide Ref Shape Shapes)

(require racket/include)
;(include "../base/common.rkc")
(include "../base/turtle.rkc")

(define (current-backend-name) "ArchiCAD")

#|
;;Start now
(%start)

;;References, in AutoCAD, are Com-Objects

(define-type Ref Com-Object)
(define ref? com-object?)

(define (copy-ref [r : Ref]) : Ref
  (%copy r))

;;The empty shapes
(define-values (empty-shape-ref empty-shape-ref?)
  (let ((v : RefOp (failed-union (list))))
    (values (lambda () : RefOp v)
            (lambda ([r : RefOp]) : Boolean (eq? r v)))))

(define-values (universal-shape-ref universal-shape-ref?)
  (let ((v : RefOp (failed-union (list))))
    (values (lambda () : RefOp v)
            (lambda ([r : RefOp]) : Boolean (eq? r v)))))

;;Now, the operations

(define (delete-basic-shape [shape : Shape]) : Void
  (for-each %safe-delete (shape-refs shape))
  (void))

|#
(define (delete-all-shapes) : Void
  (%delete-levels)
  (void))

#|
(define (shape<-ref [r : Ref]) ;HACK Bug in typed/racket : Shape
  (define (coordinates [r : Ref])
    (let ((pts
           (cond ((%line? r)
                  (list (%start-point r) (%end-point r)))
                 ((%lightweight-polyline? r) ;;This is not right, we need to convert coordinates
                  (let ((h (%elevation r)))
                    (map (lambda ([p : Loc]) (+z p h)) (%2d-coordinates r))))
                 ((or (%2d-polyline? r) (%3d-polyline? r))
                  (%coordinates r))
                 (else
                  (error 'coordinates "Can't compute vertices of ~A" (%object-name r))))))
      (if (or (%closed r)
              (< (distance (car pts) (last pts)) 1.0e-015)) ;AutoCAD tolerance
          (drop-right pts 1)
          pts)))
  (let ((geometry (%object-geometry r)))
    (case geometry
      ((line)
       (new-line (thunk r)
                 (coordinates r)))
      ((closed-line)
       (new-closed-line (thunk r)
                        (coordinates r)))
      ((point)
       (new-point (thunk r)
                  (%point-coordinates r)))
      ((text)
       (new-text (thunk r)
                 (%text-string r)
                 (%insertion-point r)
                 (%height r)))
      (else
       (new-unknown (thunk r))))))

(define (all-shapes)
  (map shape<-ref (%all-objects)))


(define #:forall (T) (singleton? [l : (Listof T)]) : Boolean
  (and (not (null? l))
       (null? (cdr l))))

(define #:forall (T) (singleton-ref [l : (Listof T)]) : T
  (cond ((null? l)
         (error 'singleton-ref "Not a singleton ~A" l))
        ((null? (cdr l))
         (car l))
        (else
         (error 'singleton-ref "Not a singleton ~A" l))))

(def-shape (point [position : Loc (u0)])
  (%add-point position))

|#

(def-shape (circle [center : Loc (u0)] [radius : Real 1])
  ;;Validate transformation matrix only include translation and Z-rotation
  (%circle (loc-in-world center) radius))

(def-shape (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (cond ((= radius 0)
         (error "Finish this") #;(%add-point center))
        ((= amplitude 0)
         (error "Finish this") #;(%add-point (+pol center radius start-angle)))
        ((>= (abs amplitude) 2pi)
         ;;Validate transformation matrix only include translation and Z-rotation
         (%circle (loc-in-world center) radius))
        (else
         (let ((end-angle (+ start-angle amplitude)))
           ;;Validate transformation matrix only include translation and Z-rotation
           (if (> end-angle start-angle)
               (%arc (loc-in-world center) radius 0.0 start-angle end-angle)
               (%arc (loc-in-world center) radius 0.0 end-angle start-angle))))))

#|
(def-shape (ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1])
  (%transform
   (if (> radius-x radius-y)
       (%add-ellipse (u0 world-cs) (xyz radius-x 0 0) (/ radius-y radius-x))
       (%add-ellipse (u0 world-cs) (xyz 0 radius-y 0) (/ radius-x radius-y)))
   center))

(define (%add-surface-circle [center : Loc] [radius : Real])
  (%transform
   (let ((circ (%add-circle (u0 world-cs) radius)))
     (begin0
       (singleton-ref (%add-region (list circ)))
       (%delete circ)))
   center))

(def-shape (surface-circle [center : Loc (u0)] [radius : Real 1])
  (%add-surface-circle center radius))

(def-shape (surface-arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (cond ((= radius 0)
         (%add-point center))
        ((= amplitude 0)
         (%add-point (+pol center radius start-angle)))
        ((>= (abs amplitude) 2pi)
         (%add-surface-circle center radius))
        (else
         (let ((end-angle (+ start-angle amplitude)))
           (let ((curves
                  (list
                   (%transform
                    (if (> end-angle start-angle)
                        (%add-arc (u0 world-cs) radius start-angle end-angle)
                        (%add-arc (u0 world-cs) radius end-angle start-angle))
                    center)
                   (%add-line center (+pol center radius start-angle))
                   (%add-line center (+pol center radius end-angle)))))
             (begin0
               (singleton-ref (%add-region curves))
               (for ((c (in-list curves))) (%delete c))))))))
|#

(def-shape* (line [pts : Loc *])
  (%line pts))

#|
(def-shape* (closed-line [pts : Loc *])
  (let ((com (%add-3d-poly (append pts (list (car pts))))))
    (%closed com #t)
    com))

(def-shape* (polygon [pts : Loc *])
  (let ((com (%add-3d-poly (append pts (list (car pts))))))
    (%closed com #t)
    com))

(def-shape* (spline [pts : Loc *]); (list (u0) (ux) (uy))] [v0 : (U #f Vec) #f] [v1 : (U #f Vec) #f])
  ;;HACK: apparently, there's no difference
  ;(ac:spline-command cs v0 v1)
  (let ((v0 #f) (v1 #f))
  (let ((v0 (or v0 (p-p (cadr pts) (car pts))))
        (v1 (or v1 
                (let ((end (take-right pts 2)))
                  (p-p (cadr end) (car end))))))
    (%add-spline pts v0 v1))))

(def-shape* (closed-spline [pts : Loc *])

   #|
The following example does not work as intended. Rotating the args to closed-spline solves the problem
(define (campo p c pista  a-palco a-pista)
  (define l (* (/ 67 105) c))
  (define p1 (+xy p (- (/ l 2)) (/ c 2)))
  (define p2 (+xy p (/ l 2) (/ c 2)))
  (define p3 (+xy p (/ l 2) (- (/ c 2))))
  (define p4 (+xy p (- (/ l 2)) (- (/ c 2))))
  (closed-spline (+pol p1 pista (+ pi/4 pi/2))                 
                 (+y p (+ (* 2 pista) (/ c 2)))                  
                 (+pol p2 pista pi/4)     
                 (+x p (+ (/ l 2) (* 3 pista)))            
                 (+pol p3 pista (- pi/4))                  
                 (+y p (- (- (/ c 2)) (* 2 pista)))                
                 (+pol p4 pista (- (- pi/4) pi/2))                 
                 (+x p (- (- (/ l 2)) (* 3 pista)))))

(campo (xyz 0 0 0) 105 6.3 0.02 1)
|#
  (let ((v0 #f) (v1 #f))
   (let ((cs (append pts (list (car pts)))))
     (let ((v0* (or v0 (p-p (cadr cs) (car cs))))
           (v1* (or v1 
                    (let ((end (take-right cs 2)))
                      (p-p (cadr end) (car end))))))
       (let ((v (v/r (v+v v0* v1*) 2)))
         (let ((v0 (or v0 v #;(pol (pol-rho v0*) (pol-phi v))))
               (v1 (or v1 v #;(pol (pol-rho v1*) (pol-phi v)))))
           (let ((sp (%add-spline (append cs (list (car cs))) v0 v1)))
             ;   (ac:closed sp)  ;;HACK SHOULDN'T WE CLOSE THIS?
             sp)))))))

;;Selectors
(define (curve-start-point [curve : Shape]) : Loc
  (%curve-start-point (shape-ref curve)))

(define (curve-end-point [curve : Shape]) : Loc
  (%curve-end-point (shape-ref curve)))

(define (curve-domain [curve : Shape]) : (Values Real Real)
  (let ((r (shape-ref curve)))
    (values (%curve-start-param r) (%curve-end-param r))))

(define (curve-frame-at [curve : Shape] [t : Real]) : Loc
  (%curve-frame-at (shape-ref curve) t))

(define (curve-frame-at-length [curve : Shape] [t : Real]) : Loc
  (%curve-frame-at-length (shape-ref curve) t))

(define (curve-length [curve : Shape]) : Real
  (%curve-length (shape-ref curve)))

(def-shape* (surface-polygon [pts : Loc *])
  (let ((com (%add-3d-poly (append pts (list (car pts))))))
    (%closed com #t)
    (begin0
      (single-ref-or-union (%add-region (list com)))
      (%delete com))))

(def-shape (rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1])
   (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
     (or ;(degenerate-rectangle c dx dy)
         (%add-3d-poly (list c (+x c dx) (+xy c dx dy) (+y c dy) c)))))

(def-shape (surface-rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1])
  (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
    (or ;(degenerate-rectangle c dx dy)
     (let ((rect (%add-3d-poly (list c (+x c dx) (+xy c dx dy) (+y c dy) c))))
       (begin0
         (singleton-ref (%add-region (list rect)))
         (%delete rect))))))

(def-shape (regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
    (%add-3d-poly (append pts (list (car pts))))))

(def-shape (surface-regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
    (error "TO BE DONE")))

(define (surface-boundary [shape : Shape]) : Shape
  (let ((refs (shape-refs shape)))
    (let ((rs (append* (map %explode refs))))
      (cond ((null? rs)
             (error 'surface-boundary "Can't compute boundary of ~A" shape))
            ((null? (cdr rs))
             (delete-shape shape)
             (new-unknown (thunk (car rs))))
            ((andmap %line? rs)
             (let ((poly (%add-3d-poly (%closed-lines-points rs))))
               (%closed poly #t)
               (for ((s (in-list rs))) (%delete s))
               (new-unknown (thunk poly))))
            (else
             (delete-shape shape)
             (new-unknown (thunk (%join-curves rs))))))))

(def-shape (loft-curve-point [curve : Shape] [point : (Point-Shape Ref)] [solid? : Boolean #f])
  (let ((boundary (surface-boundary curve)))
    (begin0
      (%loft-command
       (%loft-to-point-string (shape-ref boundary) (point-position point) solid?)
       #t
       #f)
      (delete-shape boundary)
      (delete-shape point))))

(def-shape (loft-surface-point [surface : Shape] [point : (Point-Shape Ref)] [solid? : Boolean #t])
  (begin0
    (%loft-command
     (%loft-to-point-string (shape-ref (surface-boundary surface)) (point-position point) solid?)
     #t
     #f)
    (delete-shape surface)
    (delete-shape point)))

(def-shape (loft-curves [curves : Shapes] [ruled? : Boolean #f] [solid? : Boolean #t] [closed? : Boolean #f])
   (begin0
     (%loft-command (%loft-objects-string (map shape-ref curves) solid?)
                    ruled?
                    closed?)
     (delete-shapes curves)))

(def-shape (loft-surfaces [shapes : Shapes] [ruled? : Boolean #f] [solid? : Boolean #t] [closed? : Boolean #f])
  (let ((sections (if solid? (map surface-boundary shapes) shapes)))
    (begin0
      (%loft-command (%loft-objects-string (map shape-ref sections) solid?)
                     ruled?
                     closed?)
      (delete-shapes sections))))

;;HACK> Bug in Typed Racket. Using (inst curve? Ref) in andmap
(define (curve?? [s : Shape]) : Boolean
  (curve? s))

(define (loft [profiles : Shapes]
              [ruled? : Boolean #f] [solid? : Boolean #f] [closed? : Boolean #f]) : Shape
  (cond ((null? (cdr profiles))
         (car profiles))
        ((andmap point? profiles)
         (begin0
           ((if ruled?
                (if closed? polygon line)
                (if closed? closed-spline spline))
            (map point-position profiles))
           (delete-shapes profiles)))
        ((andmap curve?? profiles) ;;HACK This is probably bogus due to TypedRacket optimizer
         (loft-curves profiles ruled? solid? closed?))
        ((andmap surface-region? profiles)
         (loft-surfaces profiles ruled? #t closed?))
        ((null? (cddr profiles))
         (let-values ([([p : (Point-Shape Ref)]
                        [s : Shape])
                       (cond ((point? (car profiles))
                              (values (car profiles) (cadr profiles)))
                             ((point? (cadr profiles))
                              (values (cadr profiles) (car profiles)))
                             (else
                              (error 'loft-shapes "cross sections are not either points or curves or surfaces" profiles)))])
           (cond ((curve? s)
                  (loft-curve-point s p solid?))
                 ((surface-region? s)
                  (loft-surface-point s p #t))
                 (else
                  (error 'loft-shapes "can't loft the shapes ~A" profiles)))))
        (else
         (error 'loft-shapes "cross sections are neither points nor curves nor surfaces  ~A" profiles))))

(def-shape (regular-pyramid-frustum [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [rt : Real 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-n cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (shape-ref
     (loft ;;don't use loft-curves because one of the polygons might be degenerate
      (list (regular-polygon edges cb rb a inscribed?)
            (regular-polygon edges ct rt a inscribed?))
      #t
      #t))))

(def-shape (regular-pyramid [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (shape-ref (regular-pyramid-frustum edges cb rb a h/ct 0 inscribed?)))

(def-shape (regular-prism [edges : Integer 4] [cb : Loc (u0)] [r : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (shape-ref (regular-pyramid-frustum edges cb r a h/ct r inscribed?)))

(def-shape (irregular-pyramid [cbs : Locs (list (ux) (uy) (uxy))] [ct : Loc (uz)])
  (shape-ref (loft-curve-point (polygon cbs) (point ct) #t)))


(def-shape (right-cuboid [cb : Loc (u0)] [width : Real 1] [height : Real 1] [h/ct : LocOrZ 1])
  (let-values ([(cb dz) (position-and-height cb h/ct)])
    (%transform
     (%add-box (+z (u0 world-cs) (/ dz 2.0)) width height dz)
     cb)))

(def-shape (cylinder [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c r h %add-point %add-circle %add-line)
         (%transform
          (%add-cylinder (+z (u0 world-cs) (/ h 2.0)) r h)
          c))))
|#

(def-shape (box [c : Loc (u0)] [dx/c1 : LocOrZ 1] [dy : Real (if (number? dx/c1) dx/c1 1)] [dz : Real dy])
  (let-values ([(dx dy dz)
                (if (number? dx/c1)
                    (values dx/c1 dy dz)
                    (let ((v (p-p (loc-in dx/c1 c) c)))
                      (values (cx v) (cy v) (cz v))))])
    (or #;(degenerate-box c dx dy dz)
        (begin #;%transform
         (%box c dx dy dz)
         #;c))))
#;
(def-shape (cone [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
  (let-values ([(c h) (position-and-height cb h/ct)])
    (or #;(axial-morph c r h
                     %add-point 
                     %add-circle 
                     %add-line2)
        (begin #;%transform
         (%add-cone (+z (u0 world-cs) (/ h 2)) r h)
         #;c))))

#|
(def-shape (cone-frustum [cb : Loc (u0)] [rb : Real 1] [h/ct : LocOrZ 1] [rt : Real 1])
  (let-values ([(c h) (position-and-height cb h/ct)])
    (%transform
     (%add-cone-frustum (u0 world-cs) rb rt h)
     c)))

(def-shape (cuboid [b0 : Loc (u0)]
                   [b1 : Loc (+x b0 1)]
                   [b2 : Loc (+y b1 1)]
                   [b3 : Loc (+y b0 1)]
                   [t0 : Loc (+z b0 1)]
                   [t1 : Loc (+x t0 1)]
                   [t2 : Loc (+y t1 1)]
                   [t3 : Loc (+y t0 1)])
   (let ((pm (%add-polyface-mesh 
              (list b0 b1 b2 b3 t0 t1 t2 t3) 
              (vector 1 4 3 2
                      5 6 7 8 
                      1 2 6 5 
                      2 3 7 6 
                      3 4 8 7
                      1 5 8 4))))
     (let ((m (%mesh-smooth-command pm)))
       (%delete pm)
       (begin0
         (%conv-to-solid-command m)
         (%delete m)))))

(def-shape (sphere [c : Loc (u0)] [r : Real 1])
  (%add-sphere c r))

(def-shape (surface-grid [ptss : (Listof (Listof Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f])
  (single-ref-or-union
   (let rec ([ptss : (Listof (Listof Loc)) ptss]
             [nu : Integer (length ptss)]
             [nv : Integer (length (car ptss))]
             [closed-u? : Boolean closed-u?]
             [closed-v? : Boolean closed-v?])
     (cond ((> nu 256)
            (let ([q (quotient nu 2)])
              (append (rec (take ptss q) q nv #f closed-v?)
                      (rec (drop ptss q) (- nu q) nv #f closed-v?))))
           ((> nv 256)
            (let ([q (quotient nu 2)])
              (append (rec (map (lambda ([pts : Locs]) (take pts q)) ptss)
                        nu q closed-u? #f)
                      (rec (map (lambda ([pts : Locs]) (drop pts q)) ptss)
                        nu (- nv q) closed-u? #f))))
           (else
            (let ((r 
                   (%add-3d-mesh
                    (if closed-u? (+ nu 1) nu)
                    (if closed-v? (+ nv 1) nv)
                    (append*
                     (let ((ptss (if closed-v?
                                     (map (lambda ([pts : Locs]) (append pts (list (car pts))))
                                          ptss)
                                     ptss)))
                       (if closed-u?
                           (append ptss (list (car ptss)))
                           ptss))))))
              (when closed-u?
                (%m-close r #t))
              (when closed-v?
                (%n-close r #t))
              ;(ac:type r ac:ac-bezier-surface-mesh) BUG??
              (list r)))))))

(def-shape (text [str : String ""] [p : Loc (u0)] [h : Real 1])
  (%transform
   (%add-text str (u0 world-cs) h)
   p))

(define (text-length [str : String ""] [h : Real 1]) : Real
  ;;HACK conservative approach
  (* (string-length str) h 0.7))

(def-shape (text-centered [str : String ""] [p : Loc (u0)] [h : Real 1])
  (%transform
   (%add-text str (u0 world-cs) h)
   (+xy p (/ (text-length str h) -2) (/ h -2))))

(def-shape (torus [center : Loc (u0)] [re : Real 1] [ri : Real 1/2])
  (%transform
   (%add-torus (u0 world-cs) re ri)
   center))

(def-shape (surface [profile : (Curve-Shape RefOp)])
  (let ((refs (shape-refs profile)))
    (if (singleton? refs)
        (let ((ref (car refs)))
          (if (%point? ref)
              ref
              (begin0
                (singleton-ref (%add-region refs))
                (delete-shape profile))))
        (let ((curves (%convert-3dpolylines refs)))
          (mark-deleted! profile)
          (begin0
            (single-ref-or-union
             (%add-region curves))
            (for-each %delete curves))))))

(define (union-refs [rs : Refs]) : Refs
  (maximize-combination
   (lambda ([r0 : Ref] [r1 : Ref]) : (Option Ref)
     (%boolean-union r0 r1)
     r0)
   rs))

(define (intersect-ref [r0 : Ref] [r1 : Ref]) : RefOp
  (%boolean-intersection r0 r1)
  r0)

(define (subtract-ref [r0 : Ref] [r1 : Ref]) : RefOp
  (%boolean-subtraction r0 r1)
  r0)

(def-shape (join-curves [shapes : Shapes])
  (begin0
    (%join-curves (shapes-refs shapes))
    (for-each (inst mark-deleted! RefOp) shapes)))

(def-shape (revolve [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1)] [start-angle : Real 0] [amplitude : Real 2pi])
  (let ((p (loc-from-o-n p n)))
    (begin0
      (map-ref ([r shape])
        (%revolve-command r p (+z p 1) start-angle (+ start-angle amplitude) (surface-region? shape)))
      (delete-shape shape))))

(define (surface-region? [s : Shape]) : Boolean
  (or (surface? s)
      (surface-circle? s)
      (surface-arc? s)
      (surface-rectangle? s)
      (surface-polygon? s)
      ;;ask the backend
      (andmap %loftable-surface? (shape-refs s))))

(def-shape (extrusion [profile : (Extrudable-Shape RefOp)] [dir : VecOrZ 1])
  (begin0
    (single-ref-or-union
     (if (number? dir)
         (%extrude-command-length (shape-refs profile) dir (surface-region? profile))
         (%extrude-command-direction (shape-refs profile) (u0 world-cs) dir (surface-region? profile))))
     (delete-shape profile)))

(def-shape (mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)] [copy? : Boolean #t])
  (let ((p (loc-from-o-n p n)))
    (begin0
      (map-ref ([r shape])
        (%mirror3d r p (+x p 1) (+y p 1)))
      (unless copy?
        (delete-shape shape)))))

(def-shape (sweep [path : (Curve-Shape RefOp)] [profile : (Extrudable-Shape RefOp)] [rotation : Real 0] [scale : Real 1])
  (let ((surface? (surface-region? profile)))
    (begin0
      (map-ref ([profile profile])
        (map-ref ([path path])
          (let ((frame (%curve-frame-at path (%curve-start-param path))))
            (%transform profile frame)
            (%sweep-command profile #f path surface? frame rotation scale))))
      (delete-shapes (list profile path)))))

(def-shape (slice [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1 p)])
  (begin0
    (map-ref ([r shape])
             (%slice-command r p n))
    (mark-shape-deleted! shape)))

(def-shape (triangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc])
  (%add-3d-face p0 p1 p2 p2))

(def-shape (quadrangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc])
  (%add-3d-face p0 p1 p2 p3))

(def-shape (move [shape : Shape] [v : Vec (vx)])
  (let ((o (u0 world-cs)))
    (do-ref ([r shape])
      (%move r o v))
    (begin0
      (shape-reference shape)
      (mark-shape-deleted! shape))))

(def-shape (rotate [shape : Shape] [a : Real pi/2] [p0 : Loc (u0)] [p1 : Loc (+z p0 1)])
  (do-ref ([r shape])
    (%rotate3d r p0 p1 a))
  (begin0
    (shape-reference shape)
    (mark-shape-deleted! shape)))

(def-shape (scale [shape : Shape] [s : Real 1] [p : Loc (u0)])
  (do-ref ([r shape])
    (%scale-entity r p s))
  (begin0
    (shape-reference shape)
    (mark-shape-deleted! shape)))


(provide bounding-box)
(define (bounding-box [s : Shape]) : Locs
  (define (combine [bb0 : (List Loc Loc)] [bb1 : (List Loc Loc)])
    : (List Loc Loc)
    (let ([p0 (car bb0)] [p1 (cadr bb0)] [p2 (car bb1)] [p3 (cadr bb1)])
      (list (xyz (min (cx p0) (cx p2))
                 (min (cy p0) (cy p2))
                 (min (cz p0) (cz p2)))
            (xyz (max (cx p1) (cx p3))
                 (max (cy p1) (cy p3))
                 (max (cz p1) (cz p3))))))
  (let ([rs : Refs (shape-refs s)])
    (let loop ([bb : (List Loc Loc) (%bounding-box (car rs))] [rs : Refs (cdr rs)])
      (if (null? rs)
          (let ((p0 (car bb)) (p1 (cadr bb)))
            (let ((dx (- (cx p1) (cx p0)))
                  (dy (- (cy p1) (cy p0)))
                  (dz (- (cz p1) (cz p0))))
              (list p0 (+x p0 dx) (+xy p0 dx dy) (+y p0 dy)
                    p1 (+x p1 dx) (+xy p1 dx dy) (+y p1 dy))))
          (loop (combine bb (%bounding-box (car rs)))
                (cdr rs))))))

(provide fast-view)
(define (fast-view) : Void
  (view-top)
  (void))

(provide view)
(define (view [camera : (Option Loc) #f] [target : (Option Loc) #f] [lens : (Option Real) #f]) : (Values Loc Loc Real)
  (cond ((and camera target lens)
         ;;(%set-view camera target lens)
         (%view-conceptual)
         (%perspective 1)
         (%dview-zoom-command camera target lens (distance camera target))
         (values camera target lens))
        (else
         (%get-view))))

(provide view-top)
(define (view-top) : Void
  (%view-top)
  (%view-wireframe)
  (void))

(provide render-view)
(define (render-view name) : Void
  ;(%renderView (prepare-for-file (render-pathname name)) (render-width) (render-height))
  (void))

(provide render-stereo-view)
(define (render-stereo-view name) : Void
  (displayln "render-stereo-view needs to be finished")
  #;#;(%RenderResolution (vector (render-width) (render-height)))
   (%render-view (prepare-for-file (render-pathname name)))
  (void))

(define (zoom-extents) : Void
  (%zoom-extents))

|#
(define (disable-update)
  ;Is there a way of disabling updates in AutoCAD
  (%visual-feedback-off))

(define (enable-update)
   ;Is there a way of disabling updates in AutoCAD
  (%visual-feedback-on))
#|
(define (prompt-point [str : String "Select position"]) : Loc
  (%get-point (u0 world-cs) str))

(define (prompt-integer [str : String "Integer?"]) : Integer
  (%get-integer str))

(define (prompt-real [str : String "Real?"]) : Real
  (%get-real str))

(define (prompt-shape [str : String "Select shape"]) : Shape
  (shape<-ref
   (%get-entity str)))

(define (select-shape [s : Shape]) : Void
  (%clear-selection-command)
  (%select-shapes-command (shape-refs s))
  (void))

(define (select-shapes [ss : Shapes]) : Void
  (%clear-selection-command)
  (%select-shapes-command (shapes-refs ss))
  (void))
|#

(define short-curve-tolerance : Float 0.0025602455729167)

(def-shape (polygonal-mass [points : Locs] [height : Real])
  (define (loop [points : Locs]) : Locs
    (if (or (null? points) (null? (cdr points)))
        points
        (let ([p0 : Loc (car points)] [p1 : Loc (cadr points)])
          (let ((p0 (loc-in-world p0))
                (p1 (loc-in-world p1)))
            (if (< (distance p0 p1) short-curve-tolerance)
                (let ((p (intermediate-point p0 p1)))
                  (loop (cons p (loop (cddr points)))))
                (cons p0 (loop (cdr points))))))))
  (let ((points (loop points)))
    (%extrusion points height)))

;(def-shape (rectangular-mass [center : Loc] [width : Real] [length : Real] [height : Real]))

(require (for-syntax racket/base racket/list racket/syntax))

(provide ;;BIM extensions
 level
 current-level
 upper-level
 default-level-to-level-height
 create-layer
 shape-layer
 (rename-out [%delete-levels delete-levels]))

(define (level height)
  (%level height))

(require (only-in "communication.rkt" current-level default-level-to-level-height create-layer shape-layer))

(define (upper-level [lvl : Level (current-level)]
                     [height : Real (default-level-to-level-height)])
  (%upper-level #:level lvl
                #:height height))

(require "../base/bim-families.rkt")
(provide (all-from-out "../base/bim-families.rkt"))

(define (vertical? p0 p1)
  (< (cyl-rho (p-p p0 p1)) 0.1))

;;Added angle must propagate to other backends.
(def-shape (beam [p0 : Loc] [p1 : Loc] [angle : Real 0] [family : Beam-Family (default-beam-family)])
  (if (vertical? p0 p1)
      (if (< (cz p0) (cz p1))
          (%column-two-points
           p0 p1
           #:width (beam-family-width family)
           #:depth (beam-family-height family)
           #:angle angle)
          (%column-two-points
           p1 p0
           #:width (beam-family-width family)
           #:depth (beam-family-height family)
           #:angle angle))
      (%beam (loc-in-world p0) (loc-in-world p1)
             #:beam-width (beam-family-width family)
             #:beam-height (beam-family-height family)
             #;#; #:angle angle)))

(def-shape (column [center : Loc]
                   [bottom-level : Level (current-level)]
                   [top-level : Level (upper-level bottom-level)]
                   [family : Any (default-column-family)])
  (%column (loc-in-world center)
           #:bottom-level bottom-level
           #:top-level top-level
           #:width (column-family-width family)
           #:depth (or (column-family-depth family)
                       (column-family-width family))
           #:circle-based? (column-family-circular-section? family)))

(def-shape (slab [vertices : Locs] [level : Any (current-level)] [family : Slab-Family (default-slab-family)])
  (%slab (map loc-in-world vertices)
         #:bottom-level level
         #:thickness (slab-family-thickness family)))

(def-shape (roof [vertices : Locs] [level : Any (current-level)] [family : Roof-Family (default-roof-family)])
  (%roof (map loc-in-world vertices)
         #:bottom-level level
         #:thickness (roof-family-thickness family)))

(def-shape (wall [p0 : Loc] [p1 : Loc]
                 [bottom-level : Level (current-level)]
                 [top-level : Level (upper-level bottom-level)]
                 [family : Any (default-wall-family)])
  (%wall (list p0 p1)
         #:bottom-level bottom-level
         #:top-level top-level
         #:thickness (or (wall-family-thickness family)
                         (%default-wall-thickness))))

(def-shape (walls [vertices : Locs]
                 [bottom-level : Level (current-level)]
                 [top-level : Level (upper-level bottom-level)]
                 [family : Any (default-wall-family)])
  (%wall vertices
         #:bottom-level bottom-level
         #:top-level top-level
         #:thickness (or (wall-family-thickness family)
                         (%default-wall-thickness))))

(def-shape (door [wall : Any] [loc : Loc] [family : Any (default-door-family)])
  (%door (shape-reference wall)
         (loc-in-world loc)
         #:width (or (door-family-width family)
                     -10000)
         #:height (or (door-family-height family)
                      -10000)))

(def-shape (panel [vertices : Locs] [level : Any (current-level)] [family : Panel-Family (default-panel-family)])
  (let ((p0 (second vertices))
        (p1 (first vertices))
        (p2 (third vertices)))
    (let ((n (vz (panel-family-thickness family)
                 (cs-from-o-vx-vy p0 (p-p p1 p0) (p-p p2 p0)))))
      (%solid (map loc-in-world
                   (append (map (lambda (v) (p+v v n)) vertices)
                           (map (lambda (v) (p-v v n)) vertices)))
              (panel-family-material family)
              #:level level))))

;;This should be moved to a different place (probably, an independent unit)
(provide slab-rectangle roof-rectangle slab-path slab-opening slab-opening-path)

(define (slab-rectangle [p : Loc] [length : Real] [width : Real] [level : Level (current-level)] [family : Slab-Family (default-slab-family)])
  (slab (list p (+x p length) (+xy p length width) (+y p width))
        level
        family))

(define (roof-rectangle [p : Loc] [length : Real] [width : Real] [level : Level (current-level)] [family : Roof-Family (default-roof-family)])
  (roof (list p (+x p length) (+xy p length width) (+y p width))
        level
        family))

(define (locs-and-arcs path)
  (let loop ((p path) (vs (list)) (arcs (list)))
    (if (null? p)
        (values vs arcs)
        (let ((e (car p)))
          (cond ((line? e)
                 (let ((line-vs (line-vertices e)))
                   (loop (cdr p)
                         (append vs (drop-right line-vs 1))
                         (append arcs (make-list (length (cdr line-vs)) 0)))))
                ((arc? e)
                 (loop (cdr p)
                       (append vs (list (+pol (arc-center e) (arc-radius e) (arc-start-angle e))))
                       (append arcs (list (arc-amplitude e)))))
                ((circle? e)
                 (loop
                  (virtual
                   (cons (arc (circle-center e) (circle-radius e) 0 pi)
                         (cons (arc (circle-center e) (circle-radius e) pi pi)
                               (cdr p))))
                  vs
                  arcs))
                (else
                 (error "Unknown path component" e)))))))

#|

Slabs should be updated to support paths instead of vertices. Vertices are just a particular case that can trivially generate a path
|#
(define (slab-path [path : path] [level : Level (current-level)] [family : Slab-Family (default-slab-family)])
  (let-values ([(locs arcs) (locs-and-arcs path)])
    (new-slab
     (lambda ()
       (%slab (map loc-in-world locs)
              #:parcs arcs
              #:bottom-level level
              #:thickness (slab-family-thickness family)))
     locs level family)))

(define (slab-opening [slab : Any] [vertices : Locs])
  (%hole-slab (shape-reference slab)
              (map loc-in-world vertices))
  slab)

(define (slab-opening-path [slab : Any] [path : path])
  (let-values ([(locs arcs) (locs-and-arcs path)])
    (%hole-slab (shape-reference slab)
                (map loc-in-world locs)
                arcs)
    slab))

