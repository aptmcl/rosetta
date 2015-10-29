#lang typed/racket/base #:no-optimize
(require racket/math
         racket/list
         racket/function)
(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt"
         "../base/typed-com.rkt")
(require (prefix-in % "ac-com.rkt"))
(provide (all-from-out "../base/coord.rkt"))
(provide (all-from-out "../base/utils.rkt"))
(provide (all-from-out "../base/shapes.rkt"))
(provide immediate-mode?
         current-backend-name
         all-shapes
         bounding-box
         delete-shape
         delete-shapes
         delete-all-shapes
         curve-start-point
         curve-end-point
         curve-domain
         curve-length
         ;curve-tangent-at
         ;curve-normal-at
         ;curve-point-at
         curve-frame-at
         curve-frame-at-length
         enable-update
         disable-update
         prompt-point
         prompt-integer
         prompt-real
         prompt-shape
         view
         view-top
         render-view
         select-shape
         select-shapes
         zoom-extents)

(require racket/include)
(include "../base/common.rkc")

(define (current-backend-name) "AutoCAD")
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


(define (delete-all-shapes) : Void
  (%erase-all)
  (void))

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

(def-shape (circle [center : Loc (u0)] [radius : Real 1])
  (%transform
   (%add-circle (u0 world-cs) radius)
   center))

(def-shape (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (cond ((= radius 0)
         (%add-point center))
        ((= amplitude 0)
         (%add-point (+pol center radius start-angle)))
        ((>= (abs amplitude) 2pi)
         (%transform
          (%add-circle (u0 world-cs) radius)
          center))
        (else
         (let ((end-angle (+ start-angle amplitude)))
           (%transform
            (if (> end-angle start-angle)
                (%add-arc (u0 world-cs) radius start-angle end-angle)
                (%add-arc (u0 world-cs) radius end-angle start-angle))
            center)))))

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

(def-shape* (line [pts : Loc *])
  (%add-3d-poly pts))

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

(def-shape (box [c : Loc (u0)] [dx/c1 : LocOrZ 1] [dy : Real (if (number? dx/c1) dx/c1 1)] [dz : Real dy])
  (let-values ([(dx dy dz)
                (if (number? dx/c1)
                    (values dx/c1 dy dz)
                    (let ((v (p-p (loc-in dx/c1 c) c)))
                      (values (cx v) (cy v) (cz v))))])
    (or #;(degenerate-box c dx dy dz)
        (%transform
         (%add-box (xyz (/ dx 2) (/ dy 2) (/ dz 2))
                   (abs dx) (abs dy) (abs dz))
         c))))

(def-shape (cone [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
  (let-values ([(c h) (position-and-height cb h/ct)])
    (or #;(axial-morph c r h
                     %add-point 
                     %add-circle 
                     %add-line2)
        (%transform
         (%add-cone (+z (u0 world-cs) (/ h 2)) r h)
         c))))

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

(define (disable-update)
  ;Is there a way of disabling updates in AutoCAD
  #f)

(define (enable-update)
   ;Is there a way of disabling updates in AutoCAD
  (%regen-active-viewport))

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
