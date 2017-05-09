#lang typed/racket/base/no-check
(require racket/math
         racket/list
         racket/function)
(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt"
         "../base/typed-com.rkt"
         "../util/geometry.rkt")
(require (prefix-in % "robot-com.rkt"))
(provide (all-from-out "../base/coord.rkt"))
(provide (all-from-out "../base/utils.rkt"))
(provide (all-from-out "../base/shapes.rkt"))
(provide (all-from-out "../util/geometry.rkt"))
#;(provide immediate-mode?
         current-backend-name
         all-shapes
         bounding-box
         delete-shape
         delete-shapes
         delete-all-shapes
         create-layer
         current-layer
         curve-start-location
         curve-closest-location
         curve-domain
         curve-end-location
         curve-frame-at
         curve-frame-at-length
         curve-length
         curve-start-location
         surface-domain
         map-surface-division
         map-inner-surface-division
         enable-update
         disable-update
         hide-shape
         loft
         loft-ruled
         map-curve-division
         map-curve-length-division
         prompt-point
         prompt-integer
         prompt-real
         prompt-shape
         render-view
         select-shape
         select-shapes
         shape-layer
         shape-color
         show-shape
         view
         view-top
         zoom-extents
         )
(provide immediate-mode?
         current-backend-name
         delete-all-shapes
         )

(define (delete-all-shapes) : Void
  (%new-3d-project!)
  (void))




(require "../base/bim-families.rkt")
(provide (all-from-out "../base/bim-families.rkt"))

(def-shape (truss-node [p : Loc] [family : Truss-Node-Family (default-truss-node-family)])
  (let ((support (truss-node-family-support family)))
    (if support
        (match support
          ((node-support name ux uy uz rx ry rz created?)
           (unless created?
             (%create-node-support-label name ux uy uz rx ry rz)
             (set-node-support-created?! support #t))
           (%add-support-node! p name)))
        (%add-node! p))))

(def-shape (truss-bar [p0 : Loc] [p1 : Loc] [angle : Real #f] [family : Truss-Bar-Family (default-truss-bar-family)])
  (let ((bar (%add-bar! p0 p1)))
    (when angle
      (%set-bar-rotation! bar angle))
    (when (truss-bar-family-section family)
      (let ((b (truss-bar-family-created? family)))
        (unless (unbox b)
          (match (truss-bar-family-material family)
            ((list name _type _Name _Nuance _E _NU _Kirchoff _RO _LX _DumpCoef _RE _RT)
             (%create-bar-material-label name _type _Name _Nuance _E _NU _Kirchoff _RO _LX _DumpCoef _RE _RT)))
          (match (truss-bar-family-section family)
            ((list name material-name wood? specs)
             (%create-bar-tube-section-label name material-name wood? specs)))
          (set-box! b #t)))
      (match (truss-bar-family-section family)
        ((list name material-name wood? specs)      
         (%set-bar-section! bar name))))
    bar))

(require "robot-enums.rkt")
(provide (all-from-out "robot-enums.rkt"))
(provide evaluate-node-load-case)
(define case-counter 0)
(define (evaluate-node-load-case v)
  (set! case-counter (+ case-counter 1))
  (%new-case case-counter
             (format "Test-~A" case-counter)
             I_CN_PERMANENT ; I_CN_EXPLOATATION I_CN_WIND I_CN_SNOW I_CN_TEMPERATURE I_CN_ACCIDENTAL I_CN_SEISMIC
             I_CAT_STATIC_LINEAR ;I_CAT_STATIC_NONLINEAR I_CAT_STATIC_FLAMBEMENT
             (lambda (records)
               (%new-node-loads records (%current-nodes) v))
             (lambda (results)
               (displayln results))))
            
            



(define (surface-domain s) (error "Unimplemented"))
(define (map-inner-surface-division . args) (error "Unimplemented"))
(define (map-surface-division . args) (error "Unimplemented"))


#;#;(require racket/include)
(include "../base/common.rkc")

(define (current-backend-name) "Robot")

;;References, in Robot, are Com-Objects

(define-type Ref Com-Object)
(define ref? com-object?)

#;(begin
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

(define (%add-surface-from-curve [curve : Ref]) : Ref
  (begin0
    (singleton-ref (%add-region (list curve)))
    (%delete curve)))
  
(define (%add-surface-circle [center : Loc] [radius : Real])
  (%transform
   (%add-surface-from-curve (%add-circle (u0 world-cs) radius))
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

(def-shape (surface-ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1])
  (%transform
   (if (> radius-x radius-y)
       (%add-surface-from-curve
        (%add-ellipse (u0 world-cs) (xyz radius-x 0 0) (/ radius-y radius-x)))
       (%add-surface-from-curve
        (%add-ellipse (u0 world-cs) (xyz 0 radius-y 0) (/ radius-x radius-y))))
   center))

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

(def-shape (spline* [pts : Locs] [v0 : (U #f Vec) #f] [v1 : (U #f Vec) #f])
  ;;HACK: apparently, there's no difference
  ;(ac:spline-command cs v0 v1)
  (let ((v0 (or v0 (p-p (cadr pts) (car pts))))
        (v1 (or v1 
                (let ((end (take-right pts 2)))
                  (p-p (cadr end) (car end))))))
    (%add-spline pts v0 v1)))

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
(define (curve-start-location [curve : Shape]) : Loc
  (%curve-start-point (shape-ref curve)))

(define (curve-end-location [curve : Shape]) : Loc
  (%curve-end-point (shape-ref curve)))

(define (curve-closest-location [curve : Shape] [p : Loc]) : Loc
  (%curve-closest-point (shape-ref curve) p))

(define (curve-domain [curve : Shape]) : (Values Real Real)
  (let ((r (shape-ref curve)))
    (values (%curve-start-param r) (%curve-end-param r))))

(define (curve-frame-at [curve : Shape] [t : Real]) : Loc
  (%curve-frame-at (shape-ref curve) t))

(define (curve-frame-at-length [curve : Shape] [t : Real]) : Loc
  (%curve-frame-at-length (shape-ref curve) t))

(define (curve-length [curve : Shape]) : Real
  (%curve-length (shape-ref curve)))

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
    (let ((poly (%add-3d-poly (append pts (list (car pts))))))
      (begin0
        (singleton-ref (%add-region (list poly)))
        (%delete poly)))))

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
  (%irregular-pyramid cbs ct))

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
  (shape-ref (regular-pyramid-frustum edges cb r a h/ct r inscribed?)))

(def-shape (irregular-prism [cbs : Locs (list (ux) (uxy) (uy))] [dir : VecOrZ 1])
  (let ((dir (if (number? dir) (vz dir (car cbs)) dir)))
    (%irregular-pyramid-frustum
     cbs
     (map (lambda ([p : Loc]) (p+v p dir)) cbs))))

(def-shape (right-cuboid [cb : Loc (u0)] [width : Real 1] [height : Real 1] [h/ct : LocOrZ 1] [angle : Real 0])
  (let-values ([(cb dz) (position-and-height cb h/ct)])
    (let ((cb (if (= angle 0) cb (loc-from-o-phi cb angle))))
      (%transform
       (%add-box (+z (u0 world-cs) (/ dz 2.0)) width height dz)
       cb))))

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
  (%irregular-pyramid-frustum
   (list b0 b1 b2 b3)
   (list t0 t1 t2 t3))
  ;;HACK it seems impossible to avoid the anoying dialog, so it is better to use the previous approach
  #;
  (let ((pm (%add-polyface-mesh 
             (list b0 b1 b2 b3 t0 t1 t2 t3) 
             (list 1 4 3 2
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
              (append (rec (take ptss (+ q 1)) (+ q 1) nv #f closed-v?)
                      (rec (drop ptss q) (- nu q) nv #f closed-v?))))
           ((> nv 256)
            (let ([q (quotient nu 2)])
              (append (rec (map (lambda ([pts : Locs]) (take pts (+ q 1))) ptss)
                        nu (+ q 1) closed-u? #f)
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

(def-shape* (surface [profiles : (Curve-Shape RefOp) *])
  (let ((refs (shapes-refs profiles)))
    (if (singleton? refs)
        (let ((ref (car refs)))
          (if (%point? ref)
              ref
              (begin0
                (singleton-ref (%add-region refs))
                (delete-shapes profiles))))
        (let ((curves (%convert-3dpolylines refs)))
          (for-each (inst mark-deleted! RefOp) profiles)
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
  (let ((p (loc-from-o-p/v p n)))
    (begin0
      (map-ref ([r shape])
        (%revolve-command r p (+z p 1) start-angle (+ start-angle amplitude) (surface-region? shape)))
      (delete-shape shape))))

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
    (single-ref-or-union
     (if (number? dir)
         (%extrude-command-length (shape-refs profile) dir (surface-region? profile))
         (%extrude-command-direction (shape-refs profile) (u0 world-cs) dir (surface-region? profile))))
    (delete-shape profile)))

(def-shape (sweep [path : (Curve-Shape RefOp)] [profile : (Extrudable-Shape RefOp)] [rotation : Real 0] [scale : Real 1])
  (let ((surface? (surface-region? profile)))
    (begin0
      (map-ref ([profile profile])
        (map-ref ([path path])
          (let ((frame (%curve-frame-at path (%curve-start-param path))))
            (%transform profile frame)
            (%sweep-command profile #f path surface? frame rotation scale))))
      (delete-shapes (list profile path)))))

(def-shape (thicken [surf : (Extrudable-Shape RefOp)] [h : Real 1])
  (begin0
    (map-ref ([surf surf])
      (let ((s (%as-surface surf)))
        (begin0
          (%thicken-command s h)
          (%delete s))))
    (mark-deleted! surf)))

(provide offset) ;;AML Fix this
(define (offset [curve : Shape] [d : Real])
  (map-ref ([r curve])
           (%offset r d)))

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

(def-shape (mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)] [copy? : Boolean #t])
  (let ((p (loc-from-o-vz p n)))
    (begin0
      (map-ref ([r shape])
        (%mirror3d r p (+x p 1) (+y p 1)))
      (unless copy?
        (delete-shape shape)))))

(provide union-mirror)
(define (union-mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)])
  (union shape (mirror shape p n)))

(provide bounding-box)
(define (bounding-box [s : Shape]) : BBox
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
          (bbox (car bb) (cadr bb))
          (loop (combine bb (%bounding-box (car rs)))
                (cdr rs))))))

(require racket/trace)
(trace bounding-box)

;;Color
(define shape-color
  (case-lambda
    [([shape : Shape])
     (%true-color (shape-ref shape))]
    [([shape : Shape] [new-color : Color])
     (do-ref ([r shape])
       (%true-color r new-color))
     (void)]))


;;Layers&Materials
(define-type Layer String)
(define-type Material String)

(define (create-layer [name : String] [color : (Option Color) #f]) : Layer
  (let ((layer (%add-layer name)))
    (when color
      (%true-color layer color)))
  name)

(define current-layer
  (case-lambda
    [()
     (%clayer)]
    [([new-layer : Layer])
     (%clayer new-layer)]))

(define shape-layer
  (case-lambda
    [([shape : Shape])
     #;
     (%get-layer (%layer (shape-ref shape)))
     (%layer (shape-ref shape))]
    [([shape : Shape] [new-layer : Layer])
     (do-ref ([r shape])
       (%layer r new-layer))
     (void)]))

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
(define (render-view [name : String]) : Void
  (%skystatus %skystatus:background-and-illumination)
  (%render-command (render-width)
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

(define (hide-shape [s : Shape]) : Void
  (for ([r (shape-refs s)])
    (%visible r #f)))

(define (show-shape [s : Shape]) : Void
  (for ([r (shape-refs s)])
    (%visible r #t)))


(provide document-path)
(define (document-path)
  (build-path (string->path (%dwgprefix)) (string->path (%dwgname))))



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
  (let ((com (%add-3d-poly (append pts (list (car pts))))))
    (single-ref-or-union
     (map (lambda ([r : Ref])
            (let ((height (if (< (cz (%normal r)) 0) (- height) height)))
              (%add-extruded-solid r height 0.0)))
          (%add-region (list com))))))

#;(define-values/invoke-unit/infer autocad-bim-ops@)
(define-values/invoke-unit/infer (export (rename bim-ops^ [dummy polygonal-mass])) bim-ops@) ;;This is odd, but 'except' cannot be used in an export clause.
;(define-values/invoke-unit bim-ops@ (import bim-shape-ops^) (export (except bim-ops^ polygonal-mass))) ;;This is odd, but 'except' cannot be used in an export clause.

(define-values/invoke-unit/infer bim-extra-ops@)
(provide-signature-elements bim-levels^)
(provide-signature-elements bim-ops^)
(provide-signature-elements bim-extra-ops^)
)