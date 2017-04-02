#lang typed/racket/base/no-check
(require racket/math
         racket/list
         racket/function)
(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt"
         "../base/typed-com.rkt"
         "../util/geometry.rkt")
(require (prefix-in % "primitives.rkt"))
(provide (all-from-out "primitives.rkt"))
(provide (all-from-out "../base/coord.rkt"))
(provide (all-from-out "../base/utils.rkt"))
(provide (all-from-out "../base/shapes.rkt"))
(provide (all-from-out "../util/geometry.rkt"))
(provide immediate-mode?
         current-backend-name
         #;all-shapes
         bounding-box
         delete-shape
         delete-shapes
         delete-all-shapes
         create-layer
         current-layer
         curve-start-location
         #;curve-closest-location
         curve-domain
         curve-end-location
         curve-frame-at
         curve-frame-at-length
         curve-length
         curve-start-location
         enable-update
         disable-update
         #;hide-shape
         #;loft
         #;loft-ruled
         map-curve-division
         map-curve-length-division
         #;prompt-point
         #;prompt-integer
         #;prompt-real
         #;prompt-shape
         render-view
         #;select-shape
         #;select-shapes
         shape-layer
         shape-color
         #;show-shape
         view
         view-top
         zoom-extents
         )

(require racket/include)
(include "../base/common.rkc")

(define (current-backend-name) "AutoCAD")

;;References, in AutoCAD, are Com-Objects

(define-type Ref Integer)
(define ref? integer?)

(define (copy-ref [r : Ref]) : Ref
  (%Copy r))

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
  (%DeleteMany (shape-refs shape))
  (void))


(define (delete-all-shapes) : Void
  (%DeleteAll)
  (void))

#;
(define (shape<-ref [r : Ref]) ;HACK Bug in typed/racket : Shape
  (define (coordinates [r : Ref])
    (if (%line? r)
        (list (%start-point r) (%end-point r))
        (let ((pts
               (cond ((%lightweight-polyline? r) ;;This is not right, we need to convert coordinates
                      (let ((h (%elevation r)))
                        (map (lambda ([p : Loc]) (+z p h)) (%2d-coordinates r))))
                     ((or (%2d-polyline? r) (%3d-polyline? r))
                      (%coordinates r))
                     (else
                      (error 'coordinates "Can't compute vertices of ~A" (%object-name r))))))
          (if (or (%closed r)
                  (< (distance (car pts) (last pts)) 1.0e-015)) ;AutoCAD tolerance
              (drop-right pts 1)
              pts))))
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
      ((circle)
       (new-circle (thunk r)
                   (%center r)
                   (%radius r)))
      (else
       (new-unknown (thunk r))))))

#;
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
  (%Point (loc-in-world position)))

(def-shape (circle [center : Loc (u0)] [radius : Real 1])
  (%Circle center (vz 1 center) radius)
  #;
  (%transform
   (%add-circle (u0 world-cs) radius)
   center))

(def-shape (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (cond ((= radius 0)
         (%Point center))
        ((= amplitude 0)
         (%Point (+pol center radius start-angle)))
        ((>= (abs amplitude) 2pi)
         (%Circle center (vz 1 center) radius))
        (else
         (let ((end-angle (+ start-angle amplitude)))
           (if (> end-angle start-angle)
               (%Arc center (vz 1 center) radius start-angle end-angle)
               (%Arc center (vz 1 center) radius end-angle start-angle))))))

(def-shape (ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1])
  (if (> radius-x radius-y)
      (%Ellipse center (vz 1 center) (xyz radius-x 0 0) (/ radius-y radius-x))
      (%Ellipse center (vz 1 center) (xyz 0 radius-y 0) (/ radius-x radius-y))))

(define (%add-surface-from-curve [curve : Ref]) : Ref
  (singleton-ref (%SurfaceFromCurves (list curve))))
  
(define (%add-surface-circle [center : Loc] [radius : Real])
  (%SurfaceCircle center (vz 1 center) radius))

(def-shape (surface-circle [center : Loc (u0)] [radius : Real 1])
  (%SurfaceCircle center (vz 1 center) radius))

(def-shape (surface-arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (cond ((= radius 0)
         (%Point center))
        ((= amplitude 0)
         (%Point (+pol center radius start-angle)))
        ((>= (abs amplitude) 2pi)
         (%add-surface-circle center radius))
        (else
         (let ((end-angle (+ start-angle amplitude)))
           (if (> end-angle start-angle)
               (%SurfaceArc center (vz 1 center) radius start-angle end-angle)
               (%SurfaceArc center (vz 1 center) radius end-angle start-angle))))))

(def-shape (surface-ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1])
  (if (> radius-x radius-y)
      (%SurfaceEllipse center (vz 1 center) (xyz radius-x 0 0) (/ radius-y radius-x))
      (%SurfaceEllipse center (vz 1 center) (xyz 0 radius-y 0) (/ radius-x radius-y))))

(def-shape* (line [pts : Loc *])
  (%PolyLine pts))

(def-shape* (closed-line [pts : Loc *])
  (%ClosedPolyLine pts))

(def-shape* (polygon [pts : Loc *])
  (%ClosedPolyLine pts))

(def-shape* (spline [pts : Loc *]); (list (u0) (ux) (uy))] [v0 : (U #f Vec) #f] [v1 : (U #f Vec) #f])
  (%Spline pts))

(def-shape (spline* [pts : Locs] [v0 : (U #f Vec) #f] [v1 : (U #f Vec) #f])
  (%InterpSpline pts v0 v1))

(def-shape* (closed-spline [pts : Loc *])
  (%ClosedSpline pts))

;;Selectors
(define (curve-start-location [curve : Shape]) : Loc
  (%CurveFrameAt (shape-ref curve) (first (%CurveDomain (shape-ref curve)))))

(define (curve-end-location [curve : Shape]) : Loc
  (%CurveFrameAt (shape-ref curve) (second (%CurveDomain (shape-ref curve)))))

#;
(define (curve-closest-location [curve : Shape] [p : Loc]) : Loc
  (%curve-closest-point (shape-ref curve) p))

(define (curve-domain [curve : Shape]) : (Values Real Real)
  (let ((d (%CurveDomain (shape-ref curve))))
    (values (first d) (second d))))

(define (curve-frame-at [curve : Shape] [t : Real]) : Loc
  (%CurveFrameAt (shape-ref curve) t))

(define (curve-frame-at-length [curve : Shape] [t : Real]) : Loc
  (%CurveFrameAtLength (shape-ref curve) t))

(define (curve-length [curve : Shape]) : Real
  (%CurveLength (shape-ref curve)))

;;HACK These two functions require the default initialization on last? but Typed Racket has a bug and prevents the use of #:forall (A)
(: map-curve-division (All (A) (->* ((-> Loc A) Shape Integer) (Boolean) (Listof A))))
(: map-curve-length-division (All (A) (->* ((-> Loc A) Shape Integer) (Boolean) (Listof A))))

(define (map-curve-division [f : (-> Loc A)] [curve : Shape] [n : Integer] [last? : Boolean #t]) : (Listof A)
  (let-values ([(start end) (curve-domain curve)])
    (map-division (lambda ([t : Real])
                    (f (curve-frame-at curve t)))
                  start end n last?)))

(define (map-curve-length-division [f : (-> Loc A)] [curve : Shape] [n : Integer] [last? : Boolean #t]) : (Listof A)
  (map-division (lambda ([t : Real])
                  (f (curve-frame-at-length curve t)))
                0.0 (curve-length curve) n last?))

(def-shape* (surface-polygon [pts : Loc *])
  (%SurfaceClosedPolyLine pts))

(def-shape (rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1])
   (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
     (or ;(degenerate-rectangle c dx dy)
         (%ClosedPolyLine (list c (+x c dx) (+xy c dx dy) (+y c dy) c)))))

(def-shape (surface-rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1])
  (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
    (or ;(degenerate-rectangle c dx dy)
     (%SurfaceClosedPolyLine (list c (+x c dx) (+xy c dx dy) (+y c dy) c)))))

(def-shape (regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
    (%ClosedPolyLine pts)))

(def-shape (surface-regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
    (%SurfaceClosedPolyLine pts)))

#;#;#;#;#;#;#;#;#;
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

(define (loft-curve-point [curve : Shape] [point : (Point-Shape Ref)])
  (let ((boundary (surface-boundary curve)))
    (begin0
      (%loft-command
       (%loft-to-point-string (shape-ref boundary) (point-position point) #f)
       #t
       #f)
      (delete-shape boundary)
      (delete-shape point))))

(define (loft-surface-point [surface : Shape] [point : (Point-Shape Ref)])
  (begin0
    (%loft-command
     (%loft-to-point-string (shape-ref (surface-boundary surface)) (point-position point) #t)
     #t
     #f)
    (delete-shape surface)
    (delete-shape point)))

(define (loft-profiles [profiles : Shapes] [rails : (Listof (Curve-Shape RefOp))]
                       [solid? : Boolean] [ruled? : Boolean] [closed? : Boolean])
  (begin0
    (%loft-command (cond ((null? rails)
                          (%loft-objects-string (map shape-ref profiles) solid?))
                         ((null? (cdr rails))
                          (%loft-objects-path-string (map shape-ref profiles) (shape-ref (car rails)) solid?))
                         (else
                          (%loft-objects-guides-string (map shape-ref profiles) (map shape-ref rails) solid?)))
                   ruled?
                   closed?)
    (delete-shapes profiles)
    (delete-shapes rails)))


(define (loft-curves [shapes : Shapes] [rails : (Listof (Curve-Shape RefOp))]
                     [ruled? : Boolean #f] [closed? : Boolean #f])
  (loft-profiles shapes rails #f ruled? closed?))

(define (loft-surfaces [shapes : Shapes] [rails : (Listof (Curve-Shape RefOp))]
                       [ruled? : Boolean #f] [closed? : Boolean #f])
  (loft-profiles (map surface-boundary shapes) rails #t ruled? closed?))

;;HACK> Bug in Typed Racket. Using (inst curve? Ref) in andmap
(define (curve?? [s : Shape]) : Boolean
  (curve? s))

(define (loft [profiles : (Listof (Extrudable-Shape Ref))] [rails : (Listof (Curve-Shape Ref)) (list)]
              [ruled? : Boolean #f] [closed? : Boolean #f]) : Shape
  (cond ((null? (cdr profiles))
         (error 'loft "just one cross section"))
        ((andmap point? profiles)
         (assert (null? rails))
         (begin0
           ((if ruled?
                (if closed? polygon line)
                (if closed? closed-spline spline))
            (map point-position profiles))
           (delete-shapes profiles)))
        (else
         (new-loft
          (thunk
           (cond 
             ((andmap curve?? profiles) ;;HACK This is probably bogus due to TypedRacket optimizer
              (loft-curves profiles rails ruled? closed?))
             ((andmap surface-region? profiles)
              (loft-surfaces profiles rails ruled? closed?))
             ((null? (cddr profiles))
              (assert (null? rails))
              (let-values ([([p : (Point-Shape Ref)]
                             [s : Shape])
                            (cond ((point? (car profiles))
                                   (values (car profiles) (cadr profiles)))
                                  ((point? (cadr profiles))
                                   (values (cadr profiles) (car profiles)))
                                  (else
                                   (error 'loft-shapes "cross sections are not either points or curves or surfaces ~A" profiles)))])
                (cond ((curve? s)
                       (loft-curve-point s p))
                      ((surface-region? s)
                       (loft-surface-point s p))
                      (else
                       (error 'loft-shapes "can't loft the shapes ~A" profiles)))))
             (else
              (error 'loft-shapes "cross sections are neither points nor curves nor surfaces  ~A" profiles))))
          profiles rails ruled? closed?))))

(define (loft-ruled [profiles : (Listof (Extrudable-Shape Ref))])
  (loft profiles (list) #t))

(def-shape (irregular-pyramid [cbs : Locs (list (ux) (uxy) (uy))] [ct : Loc (uz)])
  (%IrregularPyramid cbs ct))

(def-shape (regular-pyramid-frustum [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [rt : Real 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-vz cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (%IrregularPyramidFrustum
     (regular-polygon-vertices edges cb rb a inscribed?)
     (regular-polygon-vertices edges ct rt a inscribed?))))
 
(def-shape (regular-pyramid [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-vz cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (%IrregularPyramid
     (regular-polygon-vertices edges cb rb a inscribed?)
     ct)))

(def-shape (regular-prism [edges : Integer 4] [cb : Loc (u0)] [r : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (shape-ref (regular-pyramid-frustum edges cb r a h/ct r inscribed?)))

(def-shape (irregular-prism [cbs : Locs (list (ux) (uxy) (uy))] [dir : VecOrZ 1])
  (let ((dir (if (number? dir) (vz dir (car cbs)) dir)))
    (%IrregularPyramidFrustum
     cbs
     (map (lambda ([p : Loc]) (p+v p dir)) cbs))))

(def-shape (right-cuboid [cb : Loc (u0)] [width : Real 1] [height : Real 1] [h/ct : LocOrZ 1] [angle : Real 0])
  (let-values ([(cb dz) (position-and-height cb h/ct)])
    (%CenteredBox cb (vx 1 cb) (vy 1 cb) width height dz)))

(def-shape (cylinder [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
  (let-values ([(cb dz) (position-and-height cb h/ct)])
    (%Cylinder cb r (+z cb dz))))

(def-shape (box [c : Loc (u0)] [dx/c1 : LocOrZ 1] [dy : Real (if (number? dx/c1) dx/c1 1)] [dz : Real dy])
  (let-values ([(dx dy dz)
                (if (number? dx/c1)
                    (values dx/c1 dy dz)
                    (let ((v (p-p (loc-in dx/c1 c) c)))
                      (values (cx v) (cy v) (cz v))))])
    (%Box c (vx 1 c) (vy 1 c) dx dy dz)))

(def-shape (cone [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
  (let-values ([(cb dz) (inverted-position-and-height cb h/ct)])
    (%Cone cb r (+z cb dz))))

(def-shape (cone-frustum [cb : Loc (u0)] [rb : Real 1] [h/ct : LocOrZ 1] [rt : Real 1])
  (let-values ([(cb dz) (position-and-height cb h/ct)])
    (%ConeFrustum cb rb (+z cb dz) rt)))

(def-shape (cuboid [b0 : Loc (u0)]
                   [b1 : Loc (+x b0 1)]
                   [b2 : Loc (+y b1 1)]
                   [b3 : Loc (+y b0 1)]
                   [t0 : Loc (+z b0 1)]
                   [t1 : Loc (+x t0 1)]
                   [t2 : Loc (+y t1 1)]
                   [t3 : Loc (+y t0 1)])
  (%IrregularPyramidFrustum
   (list b0 b1 b2 b3)
   (list t0 t1 t2 t3)))

(def-shape (sphere [c : Loc (u0)] [r : Real 1])
  (let ((p (loc-in-world c)))
    (if (= r 0)
        (error "Unfinished") ;(%addPoint (cx p) (cy p) (cz p))
        (%Sphere p r))))

(def-shape (surface-grid [ptss : (Listof (Listof Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f])
  (%SurfaceFromGrid (length ptss) (length (first ptss))
                    (append* ptss)
                    closed-u?
                    closed-v?
                    2))

(def-shape (text [str : String ""] [p : Loc (u0)] [h : Real 1])
  (%Text str p (vx 1 p) (vy 1 p) h))

(define (text-length [str : String ""] [h : Real 1]) : Real
  ;;HACK conservative approach
  (* (string-length str) h 0.85))

(def-shape (text-centered [str : String ""] [p : Loc (u0)] [h : Real 1])
  (%Text str (+xy p (/ (text-length str) -2) (/ h -2)) (vx 1 p) (vy 1 p) h))

(def-shape (torus [center : Loc (u0)] [re : Real 1] [ri : Real 1/2])
  (%Torus center (vz 1 center) re ri))

(def-shape* (surface [profiles : (Curve-Shape RefOp) *])
  (let ((refs (shapes-refs profiles)))
    (let ((regions (%SurfaceFromCurves refs)))
      (for-each (inst mark-deleted! RefOp) profiles)
      (single-ref-or-union regions))))

#;
(define (union-refs [rs : Refs]) : Refs
  (maximize-combination
   (lambda ([r0 : Ref] [r1 : Ref]) : (Option Ref)
     (%boolean-union r0 r1)
     r0)
   rs))

(define (intersect-ref [r0 : Ref] [r1 : Ref]) : RefOp
  (%Intersect r0 r1)
  r0)

(define (subtract-ref [r0 : Ref] [r1 : Ref]) : RefOp
  (%Subtract r0 r1)
  r0)

#;
(def-shape (join-curves [shapes : Shapes])
  (begin0
    (%join-curves (shapes-refs shapes))
    (for-each (inst mark-deleted! RefOp) shapes)))

(def-shape (revolve [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1)] [start-angle : Real 0] [amplitude : Real 2pi])
  (begin0
    (map-ref ([r shape])
      (%Revolve r p (+z p 1) start-angle (+ start-angle amplitude)))
    (delete-shape shape)))

#;#;
(define (curve? [s : Shape]) : Boolean
  (or (line? s)
      (closed-line? s)
      (spline? s)
      (closed-spline? s)
      (circle? s)
      (arc? s)
      ;;ask the backend
      (andmap %curve? (shape-refs s))))

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
    (%Extrude (shape-ref profile) (if (number? dir) (vz dir) dir))
    (delete-shape profile)))

(def-shape (sweep [path : (Curve-Shape RefOp)] [profile : (Extrudable-Shape RefOp)] [rotation : Real 0] [scale : Real 1])
  (begin0
    (map-ref ([profile profile])
      (map-ref ([path path])
        (%Sweep path profile rotation scale)))
    (delete-shapes (list profile path))))

(def-shape (thicken [surf : (Extrudable-Shape RefOp)] [h : Real 1])
  (begin0
    (map-ref ([surf surf])
      (%Thicken surf h))
    (delete-shape surf)))

(def-shape (slice [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1 p)])
  (do-ref ([r shape])
    (%Slice r p n))
  (shape-reference shape))

#;
(def-shape (triangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc])
  (%add-3d-face p0 p1 p2 p2))
#;
(def-shape (quadrangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc])
  (%add-3d-face p0 p1 p2 p3))

(def-shape (move [shape : Shape] [v : Vec (vx)])
  (do-ref ([r shape])
    (%Move r v))
  (begin0
    (shape-reference shape)
    (mark-shape-deleted! shape)))

(def-shape (rotate [shape : Shape] [a : Real pi/2] [p : Loc (u0)] [v : Vec (z p 1)])
  (do-ref ([r shape])
    (%Rotate r p v a))
  (begin0
    (shape-reference shape)
    (mark-shape-deleted! shape)))

(def-shape (scale [shape : Shape] [s : Real 1] [p : Loc (u0)])
  (do-ref ([r shape])
    (%Scale r p s))
  (begin0
    (shape-reference shape)
    (mark-shape-deleted! shape)))

(def-shape (mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)] [copy? : Boolean #t])
  (let ((p (loc-from-o-vz p n)))
    (begin0
      (map-ref ([r shape])
        (%Mirror r p n copy?))
      (unless copy?
        (delete-shape shape)))))

(provide union-mirror)
(define (union-mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)])
  (union shape (mirror shape p n)))

(provide bounding-box)
(define (bounding-box [s : Shape]) : BBox
  (%BoundingBox (shape-refs s)))

;;Color
(define shape-color
  (case-lambda
    [([shape : Shape])
     (error "Unfinished")
     #;(%ShapeColor (shape-ref shape))]
    [([shape : Shape] [new-color : Color])
     (do-ref ([r shape])
       (%SetShapeColor
        r
        (rgb-red new-color)
        (rgb-green new-color)
        (rgb-blue new-color)))
     (void)]))


;;Layers&Materials
(define-type Layer String)
(define-type Material String)

(define (create-layer [name : String] [color : (Option Color) #f]) : Layer
  (let ((layer (%CreateLayer name)))
    (when color
      (%SetLayerColor layer (rgb-red color) (rgb-green color) (rgb-blue color)))
    layer))  

(define current-layer
  (case-lambda
    [()
     (%CurrentLayer)]
    [([new-layer : Layer])
     (%SetCurrentLayer new-layer)]))

(define shape-layer
  (case-lambda
    [([shape : Shape])
     (%ShapeLayer (shape-ref shape))]
    [([shape : Shape] [new-layer : Layer])
     (do-ref ([r shape])
       (%SetShapeLayer r new-layer))
     (void)]))
#;#;
(define (create-material [name : String]) : Material
  (%add-material name)
  name)

(define shape-material
  (case-lambda
    [([shape : Shape])
     (%material (shape-ref shape))]
    [([shape : Shape] [new-material : Material])
     (do-ref ([r shape])
       (%material r new-material))
     (void)]))

;;

(provide fast-view)
(define (fast-view) : Void
  (view-top)
  (void))

(provide view)
(define (view [camera : (Option Loc) #f] [target : (Option Loc) #f] [lens : (Option Real) #f]) : (Values Loc Loc Real)
  (cond ((and camera target lens)
         ;;(%set-view camera target lens)
         (%View camera target lens)
         (values camera target lens))
        (else
         (values (%ViewCamera) (%ViewTarget) (%ViewLens)))))

(provide view-top)
(define (view-top) : Void
  (%ViewTop)
  (void))

(provide render-view)
(define (render-view [name : String]) : Void
  (%SetSystemVariableInt "SKYSTATUS" 2)
  (%Render (render-width)
           (render-height)
           (prepare-for-saving-file (render-pathname name)))
  (void))

(provide render-stereo-view)
(define (render-stereo-view name) : Void
  (displayln "render-stereo-view needs to be finished")
  #;#;
  (%RenderResolution (vector (render-width) (render-height)))
  (%render-view (prepare-for-saving-file (render-pathname name)))
  (void))

(provide save-film-frame)
(define (save-film-frame [obj : Any (void)]) : Any
  (parameterize ((render-kind-dir "Film"))
    (render-view (frame-filename (film-filename) (film-frame)))
  (film-frame (+ (film-frame) 1))
  obj))

(define (zoom-extents) : Void
  (%ZoomExtents))

(define (disable-update)
  (%DisableUpdate))

(define (enable-update)
  (%EnableUpdate))

(define (prompt-point [str : String "Select position"]) : Loc
  (%GetPoint str))

#;#;#;#;#;#;#;
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

(define (hide-shape [s : Shape]) : Void
  (for ([r (shape-refs s)])
    (%visible r #f)))

(define (show-shape [s : Shape]) : Void
  (for ([r (shape-refs s)])
    (%visible r #t)))


(require racket/unit)
(require "../base/bim-operations.rkt")
(provide (all-from-out "../base/bim-operations.rkt"))
(define-values/invoke-unit/infer bim-levels@)
(provide-signature-elements bim-levels^)

#;(define-unit autocad-bim-ops@
  (import bim-shape-ops^ bim-levels^)
  (export bim-ops^)
  (define-values/invoke-unit bim-ops@ (import bim-shape-ops^ bim-levels^) (export (except bim-ops^ polygonal-mass)))
  (def-shape/no-provide (polygonal-mass [pts : Locs] [height : Real])
    (let ((com (%add-3d-poly (append pts (list (car pts))))))
      (single-ref-or-union
       (map (lambda ([r : Ref])
              (let ((height (if (< (cz (%normal r)) 0) (- height) height)))
                (%add-extruded-solid r height 0.0)))
            (%add-region (list com)))))))

(def-shape (polygonal-mass [pts : Locs] [height : Real])
  (%IrregularPyramidFrustum pts (map (lambda (p) (+z p height)) pts)))

#;(define-values/invoke-unit/infer autocad-bim-ops@)
(define-values/invoke-unit/infer (export (rename bim-ops^ [dummy polygonal-mass])) bim-ops@) ;;This is odd, but 'except' cannot be used in an export clause.
;(define-values/invoke-unit bim-ops@ (import bim-shape-ops^) (export (except bim-ops^ polygonal-mass))) ;;This is odd, but 'except' cannot be used in an export clause.

(define-values/invoke-unit/infer bim-extra-ops@)
(provide-signature-elements bim-levels^)
(provide-signature-elements bim-ops^)
(provide-signature-elements bim-extra-ops^)
