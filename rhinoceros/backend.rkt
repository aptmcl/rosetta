#lang typed/racket/base
(require racket/math racket/list racket/function)
(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt"
         "../util/geometry.rkt")
(require (prefix-in % "rh-com.rkt"))

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
         curve-start-location
         curve-end-location
         curve-domain
         curve-end-location
         curve-frame-at
         curve-frame-at-length
         curve-length
         curve-start-location
         enable-update
         disable-update
         map-curve-division
         map-curve-length-division
         prompt-point
         prompt-integer
         prompt-real
         prompt-shape
         render-view
         view
         view-top
         select-shape
         select-shapes
         zoom-extents
)

(require racket/include)
(include "../base/common.rkc")


(define (current-backend-name) "Rhino5")
;;Start now
;;(%start) NOT YET. For some strange reason, Rhino closes immediately after.

;;References, in Rhino, are Strings

(define-type Ref String)
(define ref? string?)

(define (copy-ref [r : Ref]) : Ref
  (%copy-object r))

;;The empty shapes
(define-values (empty-shape-ref empty-shape-ref?)
  (let ((v : Ref "empty-shape"))
    (values (lambda () : Ref v)
            (lambda ([r : RefOp]) : Boolean (eq? r v)))))

(define-values (universal-shape-ref universal-shape-ref?)
  (let ((v : Ref "universal-shape"))
    (values (lambda () : Ref v)
            (lambda ([r : RefOp]) : Boolean (eq? r v)))))

;;Now, the operations
(define (delete-basic-shape [shape : Shape]) : Void
  (%delete-objects (shape-refs shape))
  (void))

;;TODO: Update this to follow the approach taken for autocad
(define (shape<-ref [r : Ref]) ;HACK Bug in typed/racket : Shape
  (let ((f (thunk r)))
    (cond ((%is-point r)
           (new-point f (%point-coordinates r)))
         ((%is-circle r)
          (new-circle f (%circle-center-point r) (%circle-radius r)))
         ((%is-curve r)
          (if (%is-curve-closed r)
              (new-closed-spline f (%curve-points r))
              (new-spline f (%curve-points r))))
         ((or (%is-line r)
              (%is-polyline r))
          (if (%is-curve-closed r)
              (new-closed-line f (%curve-points r))
              (new-line f (%curve-points r))))
         ((and (%is-object r)
               (%is-object-solid r))
          (new-unknown f))
         ((or (%is-surface r)
              (%is-polysurface r))
          (new-unknown f)) ;;HACK Can we do better?
         #;((%is-mesh r)
          (new-mesh f))
         (else
          (error 'shape<-ref "Unknown Rhino object ~A" r)))))

(define (all-shapes)
  (map shape<-ref (%all-objects)))



(def-shape (point [position : Loc (u0)])
  (%add-point position))

(def-shape (circle [center : Loc (u0)] [radius : Real 1])
  (%add-circle-plane center radius))

(def-shape (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (cond ((= radius 0)
         (%add-point center))
        ((= amplitude 0)
         (%add-point (+pol center radius start-angle)))
        ((>= (abs amplitude) 2pi)
         (%add-circle-plane center radius))
        (else
         (%add-arc
          (if (= start-angle 0)
              center
              (loc-from-o-phi center start-angle))
          radius
          (radians->degrees (coterminal amplitude))))))

(def-shape (ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1])
  (%add-ellipse center radius-x radius-y))

(def-shape (surface-circle [center : Loc (u0)] [radius : Real 1])
  (%add-surface-circle center (cast radius Positive-Real)))

(define (%add-surface-circle [center : Loc] [radius : Positive-Real]) : Ref
  (let ((circ (%add-circle center radius)))
    (begin0
      (singleton-ref (%add-planar-srf (list circ)))
      (%delete-object circ))))

(def-shape (surface-arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (cond ((= radius 0)
         (%add-point center))
        ((= amplitude 0)
         (%add-point (+pol center radius start-angle)))
        ((>= (abs amplitude) 2pi)
         (%add-surface-circle center (cast radius Positive-Real)))
        (else
         (let ((curves
                (list
                 (%add-arc
                  (if (= start-angle 0)
                      center
                      (loc-from-o-phi center start-angle))
                  radius
                  (radians->degrees (coterminal amplitude)))
                 (%add-line center (+pol center radius start-angle))
                 (%add-line center (+pol center radius (+ start-angle amplitude))))))
           (begin0
             (singleton-ref (%add-planar-srf curves))
             (%delete-objects curves))))))

(def-shape* (line [pts : Loc *])
  (%add-polyline pts))

(def-shape* (closed-line [pts : Loc *])
  (%add-polyline (append pts (list (car pts)))))

(def-shape* (polygon [pts : Loc *])
  (%add-polyline (append pts (list (car pts)))))

(def-shape* (spline [pts : Loc *]) ;Locs (list (u0) (ux) (uy))] [v0 : (U #f Vec) #f] [v1 : (U #f Vec) #f])
  (let ((v0 #f) (v1 #f))
  (if (and v0 v1) ;;HACK This should be solved by dealing with optionals
      (%add-interp-curve-ex pts 3 %knot-style-chord-length-spacing #t v0 v1)
      (%add-interp-curve-ex2 pts 3 %knot-style-chord-length-spacing #t))))

(def-shape* (closed-spline [pts : Loc *])
  (%add-interp-curve-ex2
   (append pts (list (car pts)))
   3
   %knot-style-chord-length-spacing
   #f))

(def-shape* (surface-polygon [pts : Loc *])
  (if (or (null? (cdddr pts))
          (null? (cddddr pts)))
      (%add-srf-pt pts)
      (let ((id (%add-polyline (append pts (list (car pts))))))
        (begin0
          (singleton-ref (%add-planar-srf (list id)))
          (%delete-object id)))))

(def-shape (rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1])
   (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
     (or ;(degenerate-rectangle c dx dy)
         (%add-polyline (list c (+x c dx) (+xy c dx dy) (+y c dy) c)))))

(def-shape (surface-rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1])
  (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
    (or ;(degenerate-rectangle c dx dy)
     (let ((rect (%add-polyline (list c (+x c dx) (+xy c dx dy) (+y c dy) c))))
       (begin0
         (singleton-ref (%add-planar-srf (list rect)))
         (%delete-object rect))))))

(def-shape (regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
    (%add-polyline (append pts (list (car pts))))))

(def-shape (surface-regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
    (error "TO BE DONE")))


(def-shape (regular-pyramid-frustum [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [rt : Real 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-vz cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (%irregular-pyramid-frustum
     (regular-polygon-vertices edges cb rb a inscribed?)
     (regular-polygon-vertices edges ct rt a inscribed?))))

(def-shape (regular-pyramid [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-vz cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (%irregular-pyramid
     (regular-polygon-vertices edges cb rb a inscribed?)
     ct)))

(def-shape (regular-prism [edges : Integer 4] [cb : Loc (u0)] [r : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-vz cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (%irregular-pyramid-frustum
     (regular-polygon-vertices edges cb r a inscribed?)
     (regular-polygon-vertices edges ct r a inscribed?))))

(def-shape (irregular-pyramid [cbs : Locs (list (ux) (uy) (uxy))] [ct : Loc (uz)])
  (%irregular-pyramid cbs ct))

(def-shape (right-cuboid [cb : Loc (u0)] [width : Real 1] [height : Real 1] [h/ct : LocOrZ 1])
  (let-values ([(cb dz) (position-and-height cb h/ct)])
    (let ((c (+xy cb (/ width -2) (/ height -2)))
          (dx width)
          (dy height))
      (%add-box
       (list c
             (+x c dx)
             (+xy c dx dy)
             (+y c dy)
             (+z c dz)
             (+xz c dx dz)
             (+xyz c dx dy dz)
             (+yz c dy dz))))))

(def-shape (cylinder [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c r h %add-point %add-circle %add-line)
         (%add-cylinder-plane c h r #t))))

(def-shape (box [c : Loc (u0)] [dx/c1 : LocOrZ 1] [dy : Real (if (number? dx/c1) dx/c1 1)] [dz : Real dy])
  (let-values ([(dx dy dz)
                (if (number? dx/c1)
                    (values dx/c1 dy dz)
                    (let ((v (p-p (loc-in dx/c1 c) c)))
                      (values (cx v) (cy v) (cz v))))])
    (or #;(degenerate-box c dx dy dz)
        (%add-box
         (list c
               (+x c dx)
               (+xy c dx dy)
               (+y c dy)
               (+z c dz)
               (+xz c dx dz)
               (+xyz c dx dy dz)
               (+yz c dy dz))))))

(def-shape (cone [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1])
  (let-values ([(c h) (inverted-position-and-height cb h/ct)])
    (or #;(axial-morph c r h
                     %add-point 
                     %add-circle 
                     %add-line2)
        (%add-cone-plane c h r #t))))

(def-shape (cone-frustum [cb : Loc (u0)] [rb : Real 1] [h/ct : LocOrZ 1] [rt : Real 1])
  (let-values ([(c h) (position-and-height cb h/ct)])
    (%add-truncated-cone-plane c rb h rt #t)))

(def-shape (cuboid [b0 : Loc (u0)]
                   [b1 : Loc (+x b0 1)]
                   [b2 : Loc (+y b1 1)]
                   [b3 : Loc (+y b0 1)]
                   [t0 : Loc (+z b0 1)]
                   [t1 : Loc (+x t0 1)]
                   [t2 : Loc (+y t1 1)]
                   [t3 : Loc (+y t0 1)])
   (%add-box (list b0 b1 b2 b3 t0 t1 t2 t3)))

(def-shape (sphere [c : Loc (u0)] [r : Real 1])
  (%add-sphere c r))

(def-shape (surface-grid [ptss : (Listof (Listof Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f])
  ;;Rhino requires at least 3 points in each dimension
  (if (and (null? (cddr ptss))
           (null? (cddr (car ptss))))
      (%add-srf-pt (append (car ptss) (cadr ptss)))
      (let ((ptss
              (cond ((null? (cddr ptss))
                     (list (car ptss)
                           (map intermediate-point
                                (car ptss)
                                (cadr ptss))
                           (cadr ptss)))
                    ((null? (cddr (car ptss)))
                     (map (lambda ([cs : (Listof Loc)])
                            (list (car cs)
                                  (intermediate-point (car cs) (cadr cs))
                                  (cadr cs)))
                          ptss))
                    (else
                     ptss))))
        (%add-srf-pt-grid
         (vector (length ptss) (length (car ptss)))
         (apply append ptss) ;;should use foldr here but Typed Racket complains
         %com-omit
         (vector closed-u? closed-v?)))))

(def-shape (text [str : String ""] [p : Loc (u0)] [h : Real 1])
  (%add-text-plane str p h))

(define (text-length [str : String ""] [h : Real 1]) : Real
  ;;HACK conservative approach
  (* (string-length str) h 0.7))

(def-shape (text-centered [str : String ""] [p : Loc (u0)] [h : Real 1])
  (%add-text-plane str (+xy p (/ (text-length str h) -2) (/ h -2)) h))

(def-shape (torus [center : Loc (u0)] [re : Real 1] [ri : Real 1/2])
  (%add-torus-plane center re ri))

(def-shape* (surface [profiles : (Curve-Shape RefOp) *])
  (begin0
    (let ((refs (shapes-refs profiles)))
      (if (singleton? refs)
          (let ((ref (car refs)))
            (if (%is-point ref)
                ref
                (singleton-ref (%add-planar-srf refs))))
          (%add-edge-srf refs)))
    (delete-shapes profiles)))

(define (union-refs [rs : Refs]) : Refs
  (maximize-combination
   (lambda ([r0 : Ref] [r1 : Ref]) : (Option Ref)
     (let ((res (%boolean-union (list r0 r1) #t)))
       (cond ((null? res) #f)
             ((null? (cdr res)) (car res))
             (else
              (error "Check this: boolean union returned more than one result")))))
   rs))

(define (intersect-ref [r0 : Ref] [r1 : Ref]) : RefOp
  (let ((res (%boolean-intersection (list r0) (list r1) #t)))
    (if (null? res)
        (let ((c1 (show "Point 1:" (%point-in-surface r1))))
          (if (or (%is-point-in-surface r0 c1) (%is-point-on-surface r0 c1))
              ;;nothing in common and one point of r1 is inside r0 => r0 contains r1 => r1
              (begin (%delete-object r0) r1)
              ;;nothing in common and one point of r1 is not inside r0 => r0 does not contain r1
              (let ((c0 (show "Point 2:" (%point-in-surface r0))))
                (if (or (%is-point-in-surface r1 c0) (%is-point-on-surface r1 c0))
                    ;;nothing in common and one point of r0 is inside r1 => r1 contains r0 => r0
                    (begin (%delete-object r1) r0)
                    ;;nothing in common and one point of r0 is not inside r1 => r0 does not contain r1 and r1 does not contain r0 => r0
                    (begin (%delete-objects (list r0 r1)) (empty-shape-ref))))))
        ;;r0 and r1 have something in common
        (single-ref-or-union (show "Boolean intersection:" res)))))

(define (subtract-ref [r0 : Ref] [r1 : Ref]) : RefOp
  (let ((res (%boolean-difference (list r0) (list r1) #t)))
    (if (null? res)
        (let ((c1 (show "Point 1:" (%point-in-surface r1))))
          (if (or (%is-point-in-surface r0 c1) (%is-point-on-surface r0 c1))
              ;;nothing in common and one point of r1 is inside r0 => r0 contains r1
              (show "R0 contains R1:" (failed-subtraction (list r0 r1)))
              ;;nothing in common and one point of r1 is not inside r0 => r0 does not contain r1
              (let ((c0 (show "Point 2:" (%point-in-surface r0))))
                (if (or (%is-point-in-surface r1 c0) (%is-point-on-surface r1 c0))
                    ;;nothing in common and one point of r0 is inside r1 => r1 contains r0 => empty shape
                    (begin
                      (%delete-objects (list r0 r1))
                      (show "R0 is contained in R1, both disappear" (empty-shape-ref)))
                    ;;nothing in common and one point of r0 is not inside r1 => r0 does not contain r1 and r1 does not contain r0 => r0
                    (show "R0 is outside R1, delete R1:"
                          (begin
                            (%delete-object r1)
                            r0))))))
        ;;r0 and r1 have something in common
        (single-ref-or-union (show "Boolean difference:" res)))))


(define (revolve-borders [profile : Ref]
                         [axis : (List Loc Loc)]
                         [start : Real]
                         [end : Real]
                         [out? : Boolean]) : Refs
  (map (lambda ([border : String])
         (begin0
           (%capped-planar-holes
            (%add-rev-srf border axis start end))
           (%delete-object border)))
       (%duplicate-surface-border profile (if out? 1 2))))

(def-shape (revolve [shape : Shape] [c : Loc (u0)] [v : Vec (vz 1)] [start-angle : Real 0] [amplitude : Real 2pi])
  (begin0
    (let ((axis (list c (p+v c v)))
          (start (radians->degrees start-angle))
          (end (radians->degrees (+ start-angle amplitude))))
      (map-ref ([r shape])
        (cond ((%is-curve r)
               ;;HACK: there's a problem in Rhino when the curve
               ;;touches the revolution axis
               ;;Apparently, the revolve command doesn't have
               ;;this problem.
               (%add-rev-srf r axis start end))
              ((or (%is-surface r) (%is-polysurface r))
               (let ((out-refs (revolve-borders r axis start end #t))
                     (in-refs (revolve-borders r axis start end #f)))
                 (subtract-refs (single-ref-or-union out-refs) in-refs)))
              (else
               (error 'revolve "Can't revolve the shape ~A" shape)))))
    (delete-shape shape)))

(def-shape (extrusion [profile : (Extrudable-Shape RefOp)] [dir : VecOrZ 1])
  (let ((dir (if (number? dir) (vz dir) dir)))
    (begin0
      (map-ref ([r profile])
        (if (%is-curve r)
          (%extrude-curve-direction r dir)
          (let ((c (car (%surface-area-centroid r))))
            (let ((%curve (%add-line c (p+v c dir))))
              (begin0
                (%extrude-surface r %curve #t)
                (%delete-object %curve))))))
      (delete-shape profile))))

(def-shape (join-curves [shapes : Shapes])
  (begin0
    (single-ref-or-union (%join-curves (shapes-refs shapes) #t))
    (delete-shapes shapes)))

(define (sweep-borders [profile : Ref] [path : Ref] [out? : Boolean]) : Refs
  (map (lambda ([border : Ref])
         (begin0
           (%capped-planar-holes
            (singleton-ref
             (%add-sweep1 path (list border))))
           (%delete-object border)))
       (%duplicate-surface-border profile (if out? 1 2))))

(def-shape (sweep [path : (Curve-Shape RefOp)] [profile : (Extrudable-Shape RefOp)] [rotation : Real 0] [scale : Real 1])
  (assert (= rotation 0))
  (assert (= scale 1))
  (maybe-delete-shapes
   (list path profile)
   (map-ref ([%path path])
    (map-ref ([%profile profile])
      (let* ((plane (%curve-perp-frame %path (%curve-parameter %path 0.0))))
        (%transform-object %profile plane #f)
        (cond ((%is-curve %profile)
               (singleton-ref (%add-sweep1 %path (list %profile))))
              ((%is-surface %profile)
               ;;This cannot be done using extrude-surface because it does not continuously
               ;;align the profile with the path 
               #;(%extrude-surface %profile %path #t)
               ;;This must be handle by separating the surface border into out and in parts,
               ;;sweeping separately, capping all of them, and subtracting the inner shells
               ;;from the outer shell
               (let ((out-refs (sweep-borders %profile %path #t))
                     (in-refs (sweep-borders %profile %path #f)))
                 (subtract-refs (single-ref-or-union out-refs) in-refs)))
              (else
               (error "Continue this"))))))))

(def-shape (slice [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1 p)])
  (let ([p (loc-from-o-vz p n)])
    (begin0
      (let rec : RefOp ([r : RefOp (shape-reference shape)])
        (cond ((string? r)
               (let ((cutter (%add-cut-plane (list r) p (+x p 1) (vy 1 p))))
                 (begin0
                   (let ((rs (%split-brep r cutter #t)))
                     (let ((rs (if (null? rs) (list r) rs)))
                       (for-each %cap-planar-holes rs)
                       (let-values (([keep clear]
                                     (partition
                                      (lambda ([r : Ref])
                                        (let ((c (car (%surface-volume-centroid r))))
                                          (< (v.v (p-p c p) n) 0)))
                                      rs)))
                         (show "Keep/Clear:" (list keep clear))
                         (%delete-objects clear)
                         (if (null? keep)
                             (empty-shape-ref)
                             (single-ref-or-union keep)))))
                   (%delete-object cutter))))
              ((failed-union? r)
               (failed-union ;;Should we attempt union again?
                (map rec (failed-union-refs r))))
              ((failed-subtraction? r)
               (subtract-refs
                (rec (car (failed-subtraction-refs r))) ;;slice only the first
                (cdr (failed-subtraction-refs r))))))
      (mark-deleted! shape))))


(def-shape (triangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc])
  (%add-srf-pt (list p0 p1 p2)))

(def-shape (quadrangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc])
  (%add-srf-pt (list p0 p1 p2 p3)))

(def-shape (move [shape : Shape] [v : Vec (vx)])
  (let ((refs (shape-refs shape)))
    (%move-objects refs v)
    (mark-deleted! shape)
    (single-ref-or-union refs)))

(def-shape (rotate [shape : Shape] [a : Real pi/2] [p0 : Loc (u0)] [p1 : Loc (+z p0 1)])
  (let ((refs (shape-refs shape)))
    (%rotate-objects refs p0 (radians->degrees (coterminal a)) p1 #f)
    (mark-deleted! shape)
    (single-ref-or-union refs)))

(def-shape (scale [shape : Shape] [s : Real 1] [p : Loc (u0)])
  (let ((refs (shape-refs shape)))
    (%scale-objects refs p (xyz s s s) #f)
    (mark-deleted! shape)
    (single-ref-or-union refs)))

(def-shape (mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)] [copy? : Boolean #t])
  ;;Stupid mirror bug
  (%postpone-redraw
   (let ((maximized? (%is-view-maximized "Perspective")))
     (unless maximized?
       (%maximize-restore-view "Perspective"))
     (let ((xform (%xform-mirror p n)))
       (begin0
         (map-ref ([r shape])
                  (%transform-object r xform copy?))
         (unless copy?
           (mark-shape-deleted! shape))
         (unless maximized?
           (%maximize-restore-view "Perspective")))))))

(provide union-mirror)
(define (union-mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)])
  (union shape (mirror shape p n)))

(define (bounding-box [s : Shape]) : Locs
  (%bounding-box (shape-refs s)))

;;Selectors
(define (curve-start-location [curve : Shape]) : Loc
  (%curve-start-point (shape-ref curve)))

(define (curve-end-location [curve : Shape]) : Loc
  (%curve-end-point (shape-ref curve)))

(define (curve-domain [curve : Shape]) : (Values Real Real)
  (let ((d (%curve-domain (shape-ref curve))))
    (values (vector-ref d 0) (vector-ref d 1))))

(define (curve-frame-at [curve : Shape] [t : Real]) : Loc
  (%curve-perp-frame (shape-ref curve) t))

(define (curve-length [curve : Shape]) : Real
  (%curve-length (shape-ref curve)))

(define (curve-frame-at-length [curve : Shape] [l : Real]) : Loc
  (let ((r (shape-ref curve)))
    (%curve-perp-frame r (%curve-closest-point r (%curve-arc-length-point r l)))))

;;HACK These two functions require the default initialization on last? but Typed Racket has a bug and prevents the use of #:forall (A)
(: map-curve-division (All (A) (->* ((-> Loc A) Shape Integer) (Boolean) (Listof A))))
(: map-curve-length-division (All (A) (->* ((-> Loc A) Shape Integer) (Boolean) (Listof A))))

(define (map-curve-division [f : (-> Loc A)] [curve : Shape] [n : Integer] [last? : Boolean #t]) : (Listof A)
  (let* ((r (shape-ref curve))
         (d (%curve-domain r))
         (start (vector-ref d 0))
         (end (vector-ref d 1)))
    (map-division (lambda ([t : Real])
                    (f (%curve-perp-frame r t)))
                  start end n last?)))

(define (map-curve-length-division [f : (-> Loc A)] [curve : Shape] [n : Integer] [last? : Boolean #t]) : (Listof A)
  (let ((r (shape-ref curve)))
    (displayln (%curve-length r))
    (map-division (lambda ([l : Real])
                    (displayln l)
                    (f (%curve-perp-frame r (%curve-closest-point r (%curve-arc-length-point r l)))))
                  0 (%curve-length r) n last?)
    #;
    (let ((params (%divide-curve-length r (/ (%curve-length r) n) #f #f)))
      (let ((limit (- (vector-length params) (if last? 0 1))))
        (for/list : (Listof A) ((i : Integer (in-range 0 limit)) ;;HACK needed to prevent a bug in Typed Racket
                                (t : Real (in-vector params)))
          (f (%curve-perp-frame r t)))))))

(define (delete-all-shapes) : Void
  (%delete-objects (%all-objects #f #f #f))
  (void))

(provide fast-view)
(define (fast-view) : Void
  (view-top)
  (void))

(define (view [camera : (Option Loc) #f] [target : (Option Loc) #f] [lens : (Option Real) #f]) : (Values Loc Loc Real)
  (cond ((and camera target lens)
         (unless (%is-view-maximized "Perspective")
           (%maximize-restore-view "Perspective"))
         (%view-projection "Perspective" 2) ;;perspective
         (%view-camera-lens "Perspective" lens)
         (%view-camera-target "Perspective" camera target)
         (%view-display-mode "Perspective" 2) ;;render
         (values camera target lens))
        (else
         (error "Finish this") #;#;
         (%current-view "Perspective")
         (let ((camera (%view-camera))
               (target (%view-target))
               (lens (%view-camera-lens)))
           (values (xyz (cx camera) (cy camera) (cz camera))
                   (xyz (cx target) (cy target) (cz target))
                   lens)))))

(define (view-top) : Void
  (unless (%is-view-maximized "Top")
    (%maximize-restore-view "Top"))
  (%view-projection "Top" 1) ;;parallel
  (%view-display-mode "Top" 0) ;;wireframe
  (void))

(provide render-view)
(define (render-view [name : String]) : Void
  (%render-resolution (vector (render-width) (render-height)))
  (%command "_-Render" #f)
#|
- Colocar o viewport em 'Perspective' e modo de visualização 'Rendered'

Command: _setactiveviewport / _perspective
Command: _setdysplaymode / _mode / _rendered

- Ir às 'Grid Options' e desligar 'Show grid lines', 'Show grid axes' e 'Show world axes icon'

Command: _gridoptions / showgrid=no / showgridaxes=no / showworldaxes=no

- Fazer a captura de imagem

Command: _viewcapturetofile

- Escolher  a pasta, dar um nome ao ficheiro, mudar o tipo de ficheiro para '.png' e assinalar 'Transparent background'
|#
  ;(sleep 10) ;;Should we wait?
  ;(%command (string-append "_-SaveRenderWindowAs \n\"" (cast (prepare-for-saving-file (render-pathname name)) String) "\"\n") #f)
  ;(%command "_-CloseRenderWindow" #f)
  (void))

(provide render-stereo-view)
(define (render-stereo-view name) : Void
  (displayln "render-stereo-view needs to be finished")
  #;#;
  (%render-resolution (vector (render-width) (render-height)))
  (%render-view (prepare-for-file (render-pathname name)))
  (void))

(provide zoom-extents)
(define (zoom-extents) : Void
  (%zoom-extents %com-omit #t))

(define (prompt-point [str : String "Select point"])
  (%get-point str))

(define (prompt-integer [str : String "Integer?"])
  (%get-integer str))

(define (prompt-real [str : String "Real?"])
  (%get-real str))

;HACK Fix when typed racket is fixed (provide prompt-existing-shape)
(define (prompt-existing-shape [shapes : Shapes] [str : String "Select shape"])
  (let ((ref (%get-object str 0 #t #f (map shape-ref shapes))))
    (findf (lambda ([s : Shape])
             (string=? (shape-ref s) ref))
           shapes)))

(define (prompt-shape [str : String "Select shape"])
  (let ((ref (%get-object str 0)))
    (new-unknown (lambda () ref))))

(define (select-shape [s : Shape]) : Void
  (%select-objects (shape-refs s))
  (void))

(define (select-shapes [ss : Shapes]) : Void
  (%select-objects (shapes-refs ss))
  (void))

(define (disable-update)
  (%enable-redraw #f))

(define (enable-update)
  (%enable-redraw))

;;This will be the same on all backends. Can we improve this?
(provide save-film-frame)
(define #:forall (T) (save-film-frame [obj : T]) : T
  (parameterize ((render-kind-dir "Film"))
    (render-view (frame-filename (film-filename) (film-frame)))
  (film-frame (+ (film-frame) 1))
  obj))

(provide saving-film-frame)
(define-syntax-rule (saving-film-frame expr)
  (begin0
    expr
    (save-film-frame)))

(def-shape (polygonal-mass [pts : Locs] [height : Real])
  (%irregular-pyramid-frustum
   pts
   (map (lambda ([pt : Loc]) (+z pt height)) pts)))