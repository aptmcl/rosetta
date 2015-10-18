#lang typed/racket/base
(require racket/math racket/list racket/function)
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
         delete-shape
         delete-shapes
         delete-all-shapes
         curve-start-point
         curve-end-point
         enable-update
         disable-update
         prompt-point
         prompt-integer
         prompt-real
         prompt-shape
         )

(define (current-backend-name) "AutoCAD")

;;References, in AutoCAD, are Com-Objects

(define-type Ref Com-Object)
(define ref? com-object?)
(define-type Refs (Listof Ref))

#|
Some operations produce more than one shape as a result,
which we will artificially unite.

We might also need to represent other kinds of
pseudo-operations.
|#

(define-type Failed-Operation (U failed-union failed-intersection failed-subtraction))
(define-type RefOp (U Ref Failed-Operation))
;;When we flatten Refs, we get a list of Com-Objects 
(define-type RefOps (Listof RefOp))

(struct failed-union
  ([refs : RefOps]))

(struct failed-intersection
  ([refs : RefOps]))

(struct failed-subtraction
  ([refs : RefOps]))

;;Shapes will contain non-flattened Refs
(define-type Shape (Base-Shape RefOp))
(define-type Shapes (Base-Shapes RefOp))
(provide Shape Shapes)

;;From one shape, we might want to retrieve its (unique) reference
(define (shape-ref [shape : Shape]) : Ref
  (let ((ref (shape-reference shape)))
    (if (ref? ref)
        ref
        (error 'shape-ref "Shape without definite reference ~a" shape))))

;;or its multiple references
(define (shape-refs [shape : Shape]) : Refs
  (let rec : Refs ([ref : RefOp (shape-reference shape)])
    (cond ((failed-union? ref)
           (apply append (map rec (failed-union-refs ref))))
          ((failed-intersection? ref)
           (apply append (map rec (failed-intersection-refs ref))))
          ((failed-subtraction? ref)
           (apply append (map rec (failed-subtraction-refs ref))))
          (else
           (list ref)))))

;;From a list of shapes, we might want to retrieve all references
(define (shapes-refs [shapes : Shapes]) : Refs
  (if (null? shapes)
      (list)
      (append (shape-refs (car shapes))
              (shapes-refs (cdr shapes)))))

;;We also need to check if a particular ref is present
(define (member-ref? [r0 : Ref] [r1 : RefOp]) : Boolean
  (let rec : Boolean ([ref : RefOp r1])
    (cond ((failed-union? ref)
           (ormap rec (failed-union-refs ref)))
          ((failed-intersection? ref)
           (ormap rec (failed-intersection-refs ref)))
          ((failed-subtraction? ref)
           (ormap rec (failed-subtraction-refs ref)))
          (else
           (eq? r0 ref)))))

;;For transforming a list of ref into an artificial union or intersection
(define (single-ref-or [f : (-> RefOps RefOp)] [refs : RefOps]) : RefOp
  (if (null? (cdr refs))
      (car refs)
      (f refs)))

(define (single-ref-or-union [refs : RefOps]) : RefOp
  (single-ref-or failed-union refs))

(define (single-ref-or-intersection [refs : RefOps]) : RefOp
  (single-ref-or failed-intersection refs))

(define (ref-or-union [ref : (U RefOp RefOps)]) : RefOp
  (if (list? ref)
      (single-ref-or-union (cast ref RefOps)) ;; HACK Revise this cast
      ref))

(define-syntax-rule
  (with-ref ([r expr]) body ...)
  (let ((s expr))
    (let rec : RefOp ([r : RefOp (shape-reference s)])
      (cond ((ref? r)
             (ref-or-union
              (begin body ...)))
            ((failed-union? r)
             (failed-union
              (map rec (failed-union-refs r))))
            ((failed-intersection? r)
             (failed-intersection
              (map rec (failed-intersection-refs r))))
            ((failed-subtraction? r)
             (failed-subtraction
              (map rec (failed-subtraction-refs r))))))))

;;The empty shapes
(define-values (empty-shape-ref empty-shape-ref?)
  (let ((v : RefOp (failed-union (list))))
    (values (lambda () : RefOp v)
            (lambda ([r : RefOp]) : Boolean (eq? r v)))))
(define-values (universal-shape-ref universal-shape-ref?)
  (let ((v : RefOp (failed-intersection (list))))
    (values (lambda () : RefOp v)
            (lambda ([r : RefOp]) : Boolean (eq? r v)))))

#|
(define empty-shape-str : RefOp "empty-shape")
(define (empty-shape-ref) : RefOp empty-shape-str)
(define (empty-shape-ref? [r : RefOp]) : Boolean (eq? r empty-shape-str))
(define universal-shape-str : RefOp "universal-shape")
(define (universal-shape-ref) : RefOp universal-shape-str)
(define (universal-shape-ref? [r : RefOp]) : Boolean (eq? r universal-shape-str))
|#

(def-shape (empty-shape) (empty-shape-ref))
(define (empty-shape? [s : Shape]) : Boolean (empty-shape-ref? (shape-reference s)))
(def-shape (universal-shape) (universal-shape-ref))
(define (universal-shape? [s : Shape]) : Boolean (universal-shape-ref? (shape-reference s)))

;;Now, the operations
(define (delete-shape [shape : Shape]) : Void
  (unless (or (empty-shape? shape) (universal-shape? shape))
    (for-each %delete (shape-refs shape))
    (mark-deleted! shape)
    (void)))

(define (delete-shapes [shapes : Shapes (list)]) : Void
  (for-each %delete (shapes-refs shapes))
  (for-each (inst mark-deleted! RefOp) shapes))

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
      (else
       (error "Unknown object geometry" geometry)))))

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

(define (listify-ref [e : (U Ref (Listof Ref))]) : Refs
  (if (list? e)
      (cast e Refs) ;;HACK: Solve this
      (list e)))

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
           (%add-line (car cs) (p+v (car cs) v0))
           (%add-line (car cs) (p+v (car cs) v1))
           (let ((sp (%add-spline (append cs (list (car cs))) v0 v1)))
             ;   (ac:closed sp)  ;;HACK SHOULDN'T WE CLOSE THIS?
             sp)))))))

;;Selectors
(define (curve-start-point [curve : Shape]) : Loc
  (%start-point (shape-ref curve)))

(define (curve-end-point [curve : Shape]) : Loc
  (%end-point (shape-ref curve)))


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
#;        ((andmap (inst curve? Ref) profiles)
         (loft-curves profiles ruled? solid? closed?))
#;        ((andmap surface-region? profiles)
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
                 #; ((surface-region? s)
                     (loft-surface-point s p #t))
                 (else
                  (error 'loft-shapes "can't loft the shapes ~A" profiles)))))
        (else
         (error 'loft-shapes "cross sections are not either points or curves or surfaces" profiles))))

(define (surface-boundary [shape : Shape]) : Shape
  (let ((refs (shape-refs shape)))
    (let ((rs (append* (map %explode refs))))
      (cond ((null? rs)
             (error 'surface-boundary "Can't compute boundary of ~A" shape))
            ((null? (cdr rs))
             (delete-shape shape)
             (new-unknown (lambda () (car rs))))
            ((andmap %line? rs)
             (let ((poly (%add-3d-poly (%closed-lines-points rs))))
               (%closed poly #t)
               (for ((s (in-list rs))) (%delete s))
               (new-unknown (lambda () poly))))
            (else
             (delete-shape shape)
             (new-unknown (lambda () (%join-curves rs))))))))

(def-shape (loft-curve-point [curve : Shape] [point : (Point-Shape Ref)] [solid? : Boolean #f])
  (begin0
    (%loft-command
     (%loft-to-point-string (shape-ref (surface-boundary curve)) (point-position point) solid?)
     #t
     #f)
    (delete-shape curve)
    (delete-shape point)))

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

(def-shape (regular-pyramid-frustum [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [rt : Real 1] [inscribed? : Boolean #f])
  (let-values ([(cb ct)
                (if (number? h/ct)
                    (values cb (+z cb h/ct))
                    (let ((new-cb (loc-from-o-n cb (p-p h/ct cb))))
                      (values new-cb (+z new-cb (distance cb h/ct)))))])
    (loft ;;don't use loft-curves because one of the polygons might be degenerate
     (list (regular-polygon edges cb rb a inscribed?)
           (regular-polygon edges ct rt a inscribed?))
     #t
     #t)))

(def-shape (regular-pyramid [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (regular-pyramid-frustum edges cb rb a h/ct 0 inscribed?))

(def-shape (regular-prism [edges : Integer 4] [cb : Loc (u0)] [r : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f])
  (regular-pyramid-frustum edges cb r a h/ct r inscribed?))

(def-shape (irregular-pyramid [cbs : Locs (list (ux) (uy) (uxy))] [ct : Loc (uz)])
  (loft-curve-point (polygon cbs) (point ct) #t))


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
  (let-values ([(c h) (inverted-position-and-height cb h/ct)])
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

(def-shape (join-curves [shapes : Shapes])
  (begin0
    (%join-curves (shapes-refs shapes))
    (for-each (inst mark-deleted! RefOp) shapes)))

#;
(def-shape (revolve [shape : Shape] [c : Loc (u0)] [v : Vec (vz 1)] [start-angle : Real 0] [amplitude : Real 2pi])
  (let ((v (vec-in v c)))
    (if (world-cs? c)
        (%addRevolve (shape-reference shape) )
        (uncurry-xform c (%addBoxTrans dx dy dz)))))

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
      (with-ref ([r shape])
        (%mirror3d r p (+x p 1) (+y p 1)))
      (unless copy?
        (delete-shape shape)))))

(def-shape (sweep [path : (Curve-Shape RefOp)] [profile : (Extrudable-Shape RefOp)] [rotation : Real 0] [scale : Real 1])
  (let ((surface? (surface-region? profile)))
    (begin0
      (with-ref ([profile profile])
        (with-ref ([path path])
          (%sweep-command profile #t path surface? rotation scale)))
      (delete-shapes (list profile path)))))

(define (union-refs [rs : Refs]) : Refs
  (maximize-combination
   (lambda ([r0 : Ref] [r1 : Ref]) : (Option Ref)
     (%boolean-union r0 r1)
     r0)
   rs))


(define (intersect-refs [rs : Refs]) : Refs
  (maximize-combination
   (lambda ([r0 : Ref] [r1 : Ref]) : (Option Ref)
     (%boolean-intersection r0 r1)
     r0)
   rs))

(def-shape (union [shapes : (Listof Shape)])
  (maybe-delete-shapes
   shapes
   (let ((shapes (filter-not empty-shape? (remove-duplicates shapes))))
     (cond ((null? shapes)
            (empty-shape-ref))
           ((null? (cdr shapes))
            (shape-reference (car shapes)))
           ((ormap universal-shape? shapes)
            (universal-shape-ref))
           (else
            (let*-values
                ([(rs) (map (inst shape-reference RefOp) shapes)]
                 [(failed-unions rs) (partition failed-union? rs)]
                 [(failed-intersections rs) (partition failed-intersection? rs)]
                 [(failed-subtractions rs) (partition failed-subtraction? rs)]
                 [(united)
                  #;(union-refs ;;We don't actually unite them
                     (cast (append rs (apply append (map failed-union-refs failed-unions)))
                           Refs))
                  (append rs (apply append (map failed-union-refs failed-unions)))])
              (single-ref-or-union
               (append united failed-intersections failed-subtractions))))))))

(define (maybe-delete-shapes [ss : Shapes] [rs : RefOp]) : RefOp
  (for ([s : Shape (in-list ss)])
    (unless (for/or : Boolean ([r : Ref (in-list (shape-refs s))])
              (member-ref? r rs))
      (delete-shape s)))
  rs)


(def-shape (intersection [shapes : Shapes])
  (maybe-delete-shapes
   shapes
   (let ((shapes (filter-not universal-shape? (remove-duplicates shapes))))
     (cond ((null? shapes)
            (universal-shape-ref))
           ((null? (cdr shapes))
            (shape-reference (car shapes)))
           ((ormap empty-shape? shapes)
            (delete-shapes shapes)
            (empty-shape-ref))
           (else
            (let*-values
                ([(rs) (map (inst shape-reference RefOp) shapes)]
                 [(failed-unions rs) (partition failed-union? rs)]
                 [(failed-intersections rs) (partition failed-intersection? rs)]
                 [(failed-subtractions rs) (partition failed-subtraction? rs)]
                 [(intersected)
                  (intersect-refs
                   (cast (append rs (apply append (map failed-intersection-refs failed-intersections)))
                         Refs))])
              (cond ((and (null? failed-unions) (null? failed-subtractions))
                     (single-ref-or-intersection intersected))
                    (else
                     (error "Finish this")))))))))

(def-shape (subtraction [shapes : Shapes])
  (maybe-delete-shapes
   shapes
   (if (null? shapes)
       (error "No shapes to subtract")
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
                (let ([r (shape-reference s)]
                      [rs (map (inst shape-reference RefOp) ss)])
                  (subtract-refs r rs))))))))

(define (subtract-refs [r : RefOp] [rs : RefOps]) : RefOp
  (if (null? rs)
      r
      (let*-values ([(failed-unions rs) (partition failed-union? rs)]
                    [(failed-intersections rs) (partition failed-intersection? rs)]
                    [(failed-subtractions rs) (partition failed-subtraction? rs)]
                    [(united)
                     (union-refs
                      (cast (append rs (apply append (map failed-union-refs failed-unions)))
                            Refs))])
        (assert failed-intersections null?)
        (assert failed-subtractions null?)
        (subtract-1-* r united))))

;;The following function ensures that the subtraction is actually done
(define (subtract-1-1 [r0 : RefOp] [r1 : RefOp]) : RefOp
  (cond ((and (ref? r0) (ref? r1))
         (%boolean-subtraction r0 r1))
        ((and (ref? r0) (failed-union? r1))
         (subtract-1-* r0 (failed-union-refs r1)))
        ((and (ref? r0) (failed-subtraction? r1))
         (error "Finish this")
         #;
         (union-refs (subtract-1-1 r0 (car (failed-subtraction-refs r1)))
                     (map (lambda ([r : Ref])
                            (intersection-ref r0 r))
                          (cdr (failed-subtraction-refs r1)))))
        ((and (failed-union? r0) (ref? r1))
         (single-ref-or-union
          (subtract-*-1 (failed-union-refs r0) r1)))
        ((and (failed-union? r0) (failed-union? r1))
         (single-ref-or-union
          (subtract-*-* (failed-union-refs r0) (failed-union-refs r1))))
        ((and (failed-union? r0) (failed-subtraction? r1))
         (error "Finish this")
         #;
         (let ((extra (map (lambda ([r : Ref])
                             (intersection-ref (copy-ref r0) r))
                           (cdr (failed-subtraction-refs r1)))))
           (union-refs (subtract-*-1 (failed-union-refs r0) (car (failed-subtraction-refs r1)))
                       extra)))
        ((and (failed-subtraction? r0) (ref? r1))
         (subtract-1-*
          (car (failed-subtraction-refs r0))
          (cons r1 (cdr (failed-subtraction-refs r0)))))
        ((and (failed-subtraction? r0) (failed-union? r1))
         (subtract-refs
          (subtract-1-* (car (failed-subtraction-refs r0)) (failed-union-refs r1))
          (cdr (failed-subtraction-refs r0))))
        ((and (failed-subtraction? r0) (failed-subtraction? r1))
         (subtract-refs
          (subtract-1-* (car (failed-subtraction-refs r0)) (failed-subtraction-refs r1))
          (cdr (failed-subtraction-refs r0))))
        (else
         (error "Finish this" r0 r1))))

(define (subtract-*-1 [rs : RefOps] [r : RefOp]) : RefOps
  (if (null? rs)
      (list)
      (let ((res
             (subtract-1-1
              (car rs)
              (if (null? (cdr rs)) r (copy-ref r)))))
        (let ((rest (subtract-*-1 (cdr rs) r)))
          (if res
              (cons res rest)
              rest)))))

(define (copy-ref [r : RefOp]) : RefOp
  (cond ((failed-union? r)
         (failed-union (map copy-ref (failed-union-refs r))))
        ((failed-intersection? r)
         (failed-intersection (map copy-ref (failed-intersection-refs r))))
        ((failed-subtraction? r)
         (failed-subtraction (map copy-ref (failed-subtraction-refs r))))
        (else
         (%copy r))))

(define (subtract-1-* [r : RefOp] [rs : RefOps]) : RefOp
  (if (null? rs)
      r
      (let ((res (subtract-1-1 r (car rs))))
        (if (empty-shape-ref? res)
            res
            (subtract-1-* res (cdr rs))))))

(define (subtract-*-* [r0s : RefOps] [r1s : RefOps]) : RefOps
  (cond ((null? r0s)
         (list))
        ((null? r1s)
         r0s)
        (else
         (subtract-*-*
          (subtract-*-1 r0s (car r1s))
          (cdr r1s)))))

(def-shape (slice [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1 p)])
  (let ([p (loc-from-o-n p n)])
    (begin0
      (with-ref ([r shape])
        (%slice-command r p (vz)))
     (mark-deleted! shape))))

(def-shape (triangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc])
  (%add-3d-face p0 p1 p2 p2))

(def-shape (quadrangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc])
  (%add-3d-face p0 p1 p2 p3))


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

(provide zoom-extents)
(define (zoom-extents) : Void
  (%zoom-extents))

#|
(provide prompt-point)
(define (prompt-point [str : String "Select point"])
  (%startGetPoint str)
  (let loop : Loc ([p : (Listof Real) (%getPoint)])
    (if (equal? p '(12345 54321 98765))
        (begin
          (sleep 0.2)
          (loop (%getPoint)))
        (xyz (car p) (cadr p) (caddr p)))))
|#

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
