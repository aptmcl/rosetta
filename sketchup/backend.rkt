#lang typed/racket/base/no-check
(require racket/math racket/list racket/match racket/function)
(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt"
         "../util/geometry.rkt"
         (prefix-in % "primops.rkt"))

(provide (all-from-out "../base/coord.rkt"))
(provide (all-from-out "../base/utils.rkt"))
(provide (all-from-out "../base/shapes.rkt"))
(provide (all-from-out "../util/geometry.rkt"))
(provide immediate-mode?
         current-backend-name
         all-shapes
         bounding-box
         delete-shape
         delete-shapes
         delete-all-shapes
         create-layer
         current-layer
         curve-start-point
         curve-end-point
         enable-update
         disable-update
         prompt-point
         ;prompt-integer
         ;prompt-real
         ;prompt-shape
         ;select-shape
         ;select-shapes
         shape-layer
         shape-color
         view
         view-top
         render-view
         zoom-extents
         )

(define (current-backend-name) "SketchUp")


;;References, in Sketchup, are Strings

(define-type Ref String)
(define ref? string?)
(define-type Refs (Listof Ref))

(define-type Shape (Base-Shape Ref))
(define-type Shapes (Base-Shapes Ref))

;;From one shape, we might want to retrieve its (unique) reference
(define (shape-ref [shape : Shape]) : Ref
  (shape-reference shape))

;;or its multiple references
(define (shape-refs [shape : Shape]) : Refs
  (list (shape-reference shape)))

;;From a list of shapes, we might want to retrieve all references
(define (shapes-refs [shapes : Shapes]) : Refs
  (if (null? shapes)
      (list)
      (append (shape-refs (car shapes))
              (shapes-refs (cdr shapes)))))

(define-syntax-rule
  (with-ref ([r expr]) body ...)
  (let ((r (shape-reference expr)))
    (begin body ...)))

;;The empty shapes
(define-values (empty-shape-ref empty-shape-ref?)
  (let ((v : Ref "empty-shape"))
    (values (lambda () : Ref v)
            (lambda ([r : Ref]) : Boolean (eq? r v)))))
(define-values (universal-shape-ref universal-shape-ref?)
  (let ((v : Ref "universal-shape"))
    (values (lambda () : Ref v)
            (lambda ([r : Ref]) : Boolean (eq? r v)))))

(def-shape (empty-shape) (empty-shape-ref))
(define (empty-shape? [s : Shape]) : Boolean (empty-shape-ref? (shape-reference s)))
(def-shape (universal-shape) (universal-shape-ref))
(define (universal-shape? [s : Shape]) : Boolean (universal-shape-ref? (shape-reference s)))

(define (delete-shape [shape : Shape]) : Void
  (%deleteShape (shape-reference shape))
  (mark-deleted! shape))

(define (delete-shapes [shapes : Shapes (list)]) : Void
  (for ([s : Shape (in-list shapes)])
    (%deleteShape (shape-reference s))
    (mark-deleted! s)))

(define (delete-all-shapes) : Void
  (%deleteAllShapes))


(define (shape<-ref [r : Ref]) ;HACK Improve this
  (new-unknown (thunk r)))

(define (all-shapes)
  (map shape<-ref (%allShapes)))

(define-syntax-rule
  (uncurry-xform c (call arg ...))
  (loc-> c
         (lambda ([m00 : Float] [m01 : Float] [m02 : Float] [m03 : Float]
                  [m10 : Float] [m11 : Float] [m12 : Float] [m13 : Float]
                  [m20 : Float] [m21 : Float] [m22 : Float] [m23 : Float]
                  [m30 : Float] [m31 : Float] [m32 : Float] [m33 : Float])
           (call arg ... m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))))

(def-shape (point [position : Loc (u0)])
  (let ((center (loc-in-world position)))
    (%addPoint (cx center) (cy center) (cz center))))


(def-shape (circle [center : Loc (u0)] [radius : Real 1])
  (if (world-cs? center)
      (%addCircleZ (cx center) (cy center) (cz center) radius)
      (let ((center (loc-in-world center))
            (normal (vec-in-world (vz 1 center))))
        (%addCircle (cx center) (cy center) (cz center)
                    radius
                    (cx normal) (cy normal) (cz normal)))))

(def-shape (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (cond ((= radius 0)
         (shape-reference (point center)))
        ((= amplitude 0)
         (shape-reference (point (+pol center radius start-angle))))
        ((>= (abs amplitude) 2pi)
         (shape-reference (circle center radius)))
        ((world-cs? center)
         (%addArcZ (cx center) (cy center) (cz center) radius start-angle (+ start-angle amplitude)))
        (else
         (let ((center (loc-in-world center))
               (xaxis (vec-in-world (vx 1 center)))
               (normal (vec-in-world (vz 1 center))))
           (%addArc (cx center) (cy center) (cz center)
                    (cx xaxis) (cy xaxis) (cz xaxis)
                    (cx normal) (cy normal) (cz normal)
                    radius
                    start-angle (+ start-angle amplitude))))))

(def-shape (ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1])
  (let ((center (loc-in-world center))
        (xaxis (vec-in-world (vx 1 center)))
        (yaxis (vec-in-world (vy 1 center))))
    (%addEllipse (cx center) (cy center) (cz center)
                 (cx xaxis) (cy xaxis) (cz xaxis)
                 (cx yaxis) (cy yaxis) (cz yaxis)
                 radius-x radius-y)))


(def-shape (surface-circle [center : Loc (u0)] [radius : Real 1])
  (if (world-cs? center)
      (%addSurfaceCircleZ (cx center) (cy center) (cz center) radius)
      (let ((center (loc-in-world center))
            (normal (vec-in-world (vz 1 center))))
        (%addSurfaceCircle (cx center) (cy center) (cz center)
                           radius
                           (cx normal) (cy normal) (cz normal)))))

(def-shape (surface-arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (if (world-cs? center)
      (%addSurfaceArcZ (cx center) (cy center) (cz center) radius start-angle (+ start-angle amplitude))
      (let ((center (loc-in-world center))
            (xaxis (vec-in-world (vx 1 center)))
            (normal (vec-in-world (vz 1 center))))
        (%addSurfaceArc (cx center) (cy center) (cz center)
                        (cx xaxis) (cy xaxis) (cz xaxis)
                        (cx normal) (cy normal) (cz normal)
                        radius
                        start-angle (+ start-angle amplitude)))))

(def-shape* (line [pts : Loc *])
  (%addLine (map loc->list-real pts)))

(def-shape* (closed-line [pts : Loc *]) ;(Listof Loc) (list (u0) (ux) (uy))])
  (%addPolygon (map loc->list-real pts)))

(def-shape* (polygon [pts : Loc *])
  (%addPolygon (map loc->list-real pts)))

(def-shape* (spline [pts : Loc *]) ;(Listof Loc) (list (u0) (ux) (uy))] #;#;[v0 : (U Boolean Vec) #f] [v1 : (U Boolean Vec) #f])
  #;(assert (and (not v0) (not v1)))
  (%addCubicBezier (map loc->list-real pts) 3))

(def-shape* (closed-spline [pts : Loc *]) ;(Listof Loc)])
  (%addCubicBezier (map loc->list-real (append pts (list (car pts)))) 3))

;;Selectors
(define (curve-start-point [curve : Shape]) : Loc
  (match (%start (shape-ref curve))
    ((list x y z) (xyz x y z))))

(define (curve-end-point [curve : Shape]) : Loc
  (match (%end (shape-ref curve))
    ((list x y z) (xyz x y z))))

(def-shape* (surface-polygon [pts : Loc *])
  (%addSurfacePolygon (map loc->list-real pts)))

(def-shape (rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1])
   (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
     (or ;(degenerate-rectangle c dx dy)
         (%addLine (map loc->list-real (list c (+x c dx) (+xy c dx dy) (+y c dy) c))))))

(def-shape (surface-rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1])
   (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
     (or ;(degenerate-rectangle c dx dy)
         (%addSurfacePolygon (map loc->list-real (list c (+x c dx) (+xy c dx dy) (+y c dy) c))))))

(def-shape (regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((center (loc-in-world center)))
    (if (and (= radius 0) (allow-degenerate-radius?))
        (%addPoint (cx center) (cy center) (cz center))
        (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
          (%addLine (map loc->list-real (append pts (list (car pts)))))))))

(def-shape (surface-regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
    (error "TO BE DONE")))


(def-shape (regular-pyramid-frustum [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [rt : Real 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-vz cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (let ((pbs (regular-polygon-vertices edges cb rb a inscribed?))
          (pts (regular-polygon-vertices edges ct rt a inscribed?)))
      (%addFrustum (map loc->list-real pbs)
                   (map loc->list-real pts)))))

(def-shape (regular-pyramid [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (values (loc-from-o-vz cb (p-p h/ct cb)) h/ct))])
    (let ((pbs (regular-polygon-vertices edges cb rb a inscribed?)))
      (%addPyramid (map loc->list-real pbs)
                   (loc->list-real ct)))))

(def-shape (regular-prism [edges : Integer 4] [cb : Loc (u0)] [r : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-vz cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (let ((pbs (regular-polygon-vertices edges cb r a inscribed?))
          (pts (regular-polygon-vertices edges ct r a inscribed?)))
      (%addFrustum (map loc->list-real pbs)
                   (map loc->list-real pts)))))

(def-shape (irregular-prism [cbs : Locs (list (ux) (uxy) (uy))] [dir : VecOrZ 1])
  (let ((dir (if (number? dir) (vz dir) dir)))
    (%addFrustum (map loc->list-real cbs)
                 (map (lambda ([p : Loc]) (loc->list-real (p+v p dir))) cbs))))

(def-shape (irregular-pyramid [cbs : Locs (list (ux) (uy) (uxy))] [ct : Loc (uz)])
  (%addPyramid (map loc->list-real cbs)
               (loc->list-real ct)))

(def-shape (right-cuboid [cb : Loc (u0)] [width : Real 1] [height : Real 1] [h/ct : LocOrZ 1])
  (let-values ([(cb length)
                (if (number? h/ct)
                    (values cb h/ct)
                    (values (loc-from-o-vz cb (p-p h/ct cb)) (distance cb h/ct)))])
    (let ((c (+xy cb (/ width -2) (/ height -2))))
      (uncurry-xform c (%addBoxTrans width height length)))))

(def-shape (cylinder [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
  (if (number? h/ct)
      (if (world-cs? cb)
          (%addCylinderZ (cx cb) (cy cb) (cz cb) r h/ct)
          (let ((p0 (loc-in-world cb))
                (p1 (loc-in-world (+z cb h/ct))))
            (%addCylinder (cx p0) (cy p0) (cz p0) r (cx p1) (cy p1) (cz p1))))
      (let ((p0 (loc-in-world cb))
            (p1 (loc-in-world h/ct)))
        (%addCylinder (cx p0) (cy p0) (cz p0) r (cx p1) (cy p1) (cz p1)))))

(def-shape (box [c : Loc (u0)] [dx/c1 : LocOrZ 1] [dy : Real (if (number? dx/c1) dx/c1 1)] [dz : Real dy])
  (let-values ([(dx dy dz)
                (if (number? dx/c1)
                    (values dx/c1 dy dz)
                    (let ((v (p-p (loc-in dx/c1 c) c)))
                      (values (cx v) (cy v) (cz v))))])
    (if (world-cs? c)
        (%addBoxD (cx c) (cy c) (cz c) dx dy dz)
        (uncurry-xform c (%addBoxTrans dx dy dz)))))

(def-shape (cone [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
  (let ((pb (loc-in-world cb)))
    (let ((pt (loc-in-world (if (number? h/ct) (+z cb h/ct) h/ct)))) 
      (%addCone (cx pb) (cy pb) (cz pb) r (cx pt) (cy pt) (cz pt)))))

(def-shape (cone-frustum [cb : Loc (u0)] [rb : Real 1] [h/ct : LocOrZ 1] [rt : Real 1])
  (let ((pb (loc-in-world cb)))
    (let ((pt (loc-in-world (if (number? h/ct) (+z cb h/ct) h/ct)))) 
      (%addConeFrustum (cx pb) (cy pb) (cz pb) rb (cx pt) (cy pt) (cz pt) rt))))

(def-shape (cuboid [b0 : Loc (u0)]
                   [b1 : Loc (+x b0 1)]
                   [b2 : Loc (+y b1 1)]
                   [b3 : Loc (+y b0 1)]
                   [t0 : Loc (+z b0 1)]
                   [t1 : Loc (+x t0 1)]
                   [t2 : Loc (+y t1 1)]
                   [t3 : Loc (+y t0 1)])
   (%addFrustum
    (map loc->list-real (list b0 b1 b2 b3))
    (map loc->list-real (list t0 t1 t2 t3))))

(def-shape (sphere [c : Loc (u0)] [r : Real 1])
  (let ((p (loc-in-world c)))
    (if (= r 0)
        (%addPoint (cx p) (cy p) (cz p))
        (%addSphere (cx p) (cy p) (cz p) r))))

(def-shape (surface-grid [pointss : (Listof (Listof Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f])
  (let ((ptss (map (lambda ([points : (Listof Loc)])
                     (let ((pts (map loc->list-real points)))
                       (if closed-u?
                           (append pts (list (car pts)))
                           pts)))
                   pointss)))
    (let ((ptss (if closed-v? (append ptss (list (car ptss))) ptss)))
      (%addSurfaceGrid ptss))))

(def-shape (text [str : String ""] [p : Loc (u0)] [h : Real 1])
  (let ((h (exact->inexact h)))
    (if (world-cs? p)
        (%addText str (cx p) (cy p) (cz p) h)
        (uncurry-xform p (%addTextTrans str h)))))

(def-shape (text-centered [str : String ""] [p : Loc (u0)] [h : Real 1])
  (let ((h (exact->inexact h)))
    (if (world-cs? p)
        (%addTextCentered str (cx p) (cy p) (cz p) h)
        (uncurry-xform p (%addTextCenteredTrans str h)))))

(def-shape (torus [center : Loc (u0)] [re : Real 1] [ri : Real 1/2])
  (if (world-cs? center)
      (%addTorus (cx center) (cy center) (cz center) 0 0 1 re ri)
      (let ((center (loc-in-world center))
            (normal (vec-in-world (vz 1 center))))
        (%addTorus (cx center) (cy center) (cz center)
                   (cx normal) (cy normal) (cz normal)
                   re ri))))

(def-shape* (surface [profiles : (Curve-Shape Ref) *])
  (%addSurface (shapes-refs profiles)))

(def-shape* (join-curves [shapes : (Curve-Shape Ref) *])
  (%joinCurves (shapes-refs shapes)))

(def-shape (revolve [shape : Shape] [c : Loc (u0)] [v : Vec (vz 1 c)] [start-angle : Real 0] [amplitude : Real 2pi])
  (displayln "Go finish revolve")
  (empty-shape-ref) #;
  (let ((v (vec-in v c)))
    (if (world-cs? c)
        (%addRevolve (shape-reference shape) )
        (uncurry-xform c (%addBoxTrans dx dy dz)))))

(def-shape (extrusion [profile : (Extrudable-Shape Ref)] [v : VecOrZ 1])
  (if (number? v)
      (%addExtrusionZ (shape-reference profile) v)
      (let ((v (vec-in-world v)))
        (%addExtrusion (shape-reference profile) (cx v) (cy v) (cz v)))))

(def-shape (mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)] [copy? : Boolean #t])
  (with-ref ([r shape])
    (let ((r (if copy? (%copy r) r)))
      (%mirror r (cx p) (cy p) (cz p) (cx n) (cy n) (cz n)))))

(def-shape (sweep [path : (Curve-Shape Ref)] [profile : (Extrudable-Shape Ref)] [rotation : Real 0] [scale : Real 1])
  (assert (= rotation 0))
  (assert (= scale 1))
  (%addSweep (shape-reference path) (shape-reference profile)))

(def-shape* (union [shapes : ShapeTree *])
  (let ((shapes (filter-not empty-shape? (remove-duplicates (flatten shapes)))))
    (cond ((null? shapes)
           (empty-shape-ref))
          ((null? (cdr shapes))
           (shape-reference (car shapes)))
          ((ormap universal-shape? shapes)
           (universal-shape-ref))
          (else
           (let ([%s0 (shape-reference (car shapes))])
             (for ([s1 : Shape (in-list (cdr shapes))])
               (set! %s0 (%addUnion %s0 (shape-reference s1))))
             %s0)))))

(def-shape* (intersection [shapes : ShapeTree *])
  (let ((shapes (filter-not universal-shape? (remove-duplicates (flatten shapes)))))
    (cond ((null? shapes)
           (universal-shape-ref))
          ((null? (cdr shapes))
           (shape-reference (car shapes)))
          ((ormap empty-shape? shapes)
           (delete-shapes shapes)
           (empty-shape-ref))
          (else
           (let ([%s0 (shape-reference (car shapes))])
             (for ([s1 : Shape (in-list (cdr shapes))])
               (set! %s0 (%addIntersection %s0 (shape-reference s1))))
             %s0)))))

(def-shape* (subtraction [shapes : ShapeTree *])
  (if (null? shapes)
      (error "No shapes to subtract")
      (let ((shapes (cons (union (first shapes) (flatten (cdr shapes))))))
        (let ((s (car shapes))
              (ss (filter-not empty-shape? (cdr shapes))))
          (cond ((null? ss)
                 (shape-reference s))
                ((or (empty-shape? s)
                     (ormap (lambda ([o : Shape])
                              (or (universal-shape? o)
                                  (eq? s o))) ;;Perhaps we should use a equal-shape? test
                            ss))
                 (empty-shape-ref))
                (else
                 (let ([%s0 (shape-reference (car shapes))])
                   (for ([s1 : Shape (in-list (cdr shapes))])
                     (set! %s0 (%addSubtraction %s0 (shape-reference s1))))
                   %s0)))))))

(provide union-mirror)
(define (union-mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)])
  (union shape (mirror shape p n)))

(def-shape (slice [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1)])
  (let ((p (loc-in-world p))
        (n (vec-in-world n)))
    (%addSlice (shape-reference shape)
               (cx p) (cy p) (cz p)
               (cx n) (cy n) (cz n))))

(def-shape (triangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc])
  (%addSurfacePolygon (map loc->list-real (list p0 p1 p2))))

(def-shape (quadrangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc])
  (%addSurfacePolygon (map loc->list-real (list p0 p1 p2 p3))))



(define (bounding-box [s : Shape]) : Locs
  (let ((bb (%boundingBox (shape-reference s))))
    (match bb
      ((list (list x0 y0 z0) (list x1 y1 z1))
       (let ((p0 (xyz x0 y0 z0))
             (p1 (xyz x0 y0 z1))
             (dx (- x1 x0))
             (dy (- y1 y0))
             (dz (- z1 z0)))
         (list p0 (+x p0 dx) (+xy p0 dx dy) (+y p0 dy)
               p1 (+x p1 dx) (+xy p1 dx dy) (+y p1 dy)))))))

#;
(defnew-from (extrusion profile [n : Pt])
  (addExtrusion (sketchup-shape-ref profile)
                (cx n) (cy n) (cz n)))




;;Layers and Colors

;;Color
(define shape-color
  (case-lambda
    [([shape : Shape])
     (%shapeRGBA (shape-ref shape))]
    [([shape : Shape] [new-color : Color])
     (%setShapeRGB (shape-ref shape) (rgb-red new-color) (rgb-green new-color) (rgb-blue new-color))
     (void)]))


;;Layers&Materials
(define-type Layer String)
(define-type Material String)

(define (create-layer [name : String] [color : (Option Color) #f]) : Layer
  (%addLayer name)
  (when color
    (%setLayerRGB name (rgb-red color) (rgb-green color) (rgb-blue color)))
  name)

(define current-layer
  (case-lambda
    [()
     (%currentLayer)]
    [([new-layer : Layer])
     (%setCurrentLayer new-layer)]))

(define shape-layer
  (case-lambda
    [([shape : Shape])
     (%shapeLayer (shape-ref shape))]
    [([shape : Shape] [new-layer : Layer])
     (%setShapeLayer (shape-ref shape) new-layer)
     (void)]))

(define (create-material [name : String]) : Material
  (%addMaterial name))

(define shape-material
  (case-lambda
    [([shape : Shape])
     (%shapeMaterial (shape-ref shape))]
    [([shape : Shape] [new-material : Material])
     (%setShapeMaterial (shape-ref shape) new-material)
     (void)]))

;;


(provide fast-view)
(define (fast-view) : Void
  (void))

(define (view [camera : (Option Loc) #f] [target : (Option Loc) #f] [lens : (Option Real) #f]) : (Values Loc Loc Real)
   (cond ((and camera target lens)
          (%view (cx camera) (cy camera) (cz camera) (cx target) (cy target) (cz target) lens)
          (values camera target lens))
         (else
          (let ((camera (%camera))
                (target (%target))
                (lens (%lens)))
            (values (xyz (car camera) (cadr camera) (caddr camera))
                    (xyz (car target) (cadr target) (caddr target))
                    lens)))))

(define (view-top) : Void
  (%viewTop)
  (void))


(define (render-view name) : Void
  ;(%renderView (prepare-for-file (render-pathname name)) (render-width) (render-height))
  (void))

(provide render-stereo-view)
(define (render-stereo-view name) : Void
  (displayln "render-stereo-view needs to be finished")
  #;#;(%RenderResolution (vector (render-width) (render-height)))
   (%render-view (prepare-for-file (render-pathname name)))
  (void))

(define zoom-extents %zoomExtents)
(provide prompt-point)
(define (prompt-point [str : String "Select point"])
  (%startGetPoint str)
  (let loop : Loc ([p : (Listof Real) (%getPoint)])
    (if (equal? p '(12345 54321 98765))
        (begin
          (sleep 0.2)
          (loop (%getPoint)))
        (xyz (car p) (cadr p) (caddr p)))))

(define (disable-update)
  ;Is there a way of disabling updates?
  #f)

(define (enable-update)
   ;Is there a way of enabling updates?
  #t)

;;BIM
(def-shape (polygonal-mass [pts : Locs] [height : Real])
  (%addFrustum (map loc->list-real pts)
               (map (lambda ([pt : Loc]) (loc->list-real (+z pt height))) pts)))

(include "../base/bim.rkc")
