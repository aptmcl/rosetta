#lang typed/racket/base
(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/list))
(require "coord.rkt")
(require "utils.rkt")
(provide Loc
         Vec
         def-base-shape
         def-shape
         def-shape*
         def*
         immediate-mode?
         allow-degenerate-radius?
         mark-deleted!
         shape-reference
         realized?
         with-references
         rectangle-deltas
         rectangle-morph
         axial-morph
         regular-polygon-vertices)


(define immediate-mode? : (Parameter Boolean)
  (make-parameter #t))
(define allow-degenerate-radius? : (Parameter Boolean)
  (make-parameter #f))

(provide Base-Shape Base-Shapes
         Boolean-Shape Generated-Shape
         Point-Shape Curve-Shape Surface-Shape Solid-Shape
         Extrudable-Shape)

;;Classify these according to the different uses (curves, surfaces, solids)
;;HACK Typed Racked seems to take inordinate amounts of time when we split the types. For the time being, we will use just one type.

#|

(define-type (Boolean-Shape R)
  (U (union-shape R)
     (intersection-shape R)
     (subtraction-shape R)))


(define-type (Generated-Shape R)
  (U (Boolean-Shape R)
     (extrusion-shape R)
     (sweep-shape R)
     #;(loft-shape R)
     (slice-shape R)
     (revolve-shape R)
     (unknown-shape R)))

(define-type (Curve-Shape R)
  (U (rectangle-shape R)
     (closed-line-shape R)
     (line-shape R)
     (polygon-shape R)
     (spline-shape R)
     (closed-spline-shape R)
     (circle-shape R)
     (ellipse-shape R)
     (arc-shape R)
   #;  (join-curves-shape R)
     (unknown-shape R)))

(define-type (Surface-Shape R)
  (U (surface-circle-shape R)
     (surface-arc-shape R)
     (surface-rectangle-shape R)
     (surface-polygon-shape R)
     (surface-grid-shape R)
     (surface-shape R)
     (Boolean-Shape R)
     (unknown-shape R)))

(define-type (Solid-Shape R)
  (U (cylinder-shape R)
     (cone-shape R)
     (cone-frustum-shape R)
     (cuboid-shape R)
     (right-cuboid-shape R)
     (box-shape R)
     (sphere-shape R)
     (torus-shape R)
     (regular-pyramid-shape R)
     (regular-pyramid-frustum-shape R)
     (irregular-pyramid-shape R)
     (regular-prism-shape R)
     (irregular-prism-shape R)
     (Generated-Shape R)))

(define-type (Text-Shape R)
  (U (text-shape R)
     (text-centered-shape R)))

(define-type (Extrudable-Shape R)
  (U (Curve-Shape R)
     (Surface-Shape R)
     (Boolean-Shape R)
     (unknown-shape R)))

(define-type (Base-Shape R)
  (U (empty-shape-shape R)
     (universal-shape-shape R)
     (Text-Shape R)
     (Curve-Shape R)
     (Surface-Shape R)
     (Solid-Shape R)
     (Generated-Shape R)))
|#
#|

(define-type (Base-Shape R)
  (U (empty-shape-shape R)
     (universal-shape-shape R)
     (text-shape R)
     (text-centered-shape R)
     (rectangle-shape R)
     (closed-line-shape R)
     (line-shape R)
     (polygon-shape R)
     (spline-shape R)
     (closed-spline-shape R)
     (circle-shape R)
     (ellipse-shape R)
     (arc-shape R)
     (surface-circle-shape R)
     (surface-arc-shape R)
     (surface-rectangle-shape R)
     (surface-polygon-shape R)
     (surface-grid-shape R)
     (surface-shape R)
     (cylinder-shape R)
     (cone-shape R)
     (cone-frustum-shape R)
     (cuboid-shape R)
     (right-cuboid-shape R)
     (box-shape R)
     (sphere-shape R)
     (torus-shape R)
     (regular-pyramid-shape R)
     (regular-pyramid-frustum-shape R)
     (irregular-pyramid-shape R)
     (regular-prism-shape R)
     (irregular-prism-shape R)
     (union-shape R)
     (intersection-shape R)
     (subtraction-shape R)
     (extrusion-shape R)
     (sweep-shape R)
     #;(loft-shape R)
     (slice-shape R)
     (revolve-shape R)
     (unknown-shape R))
|#

(define-type (Boolean-Shape R) (Base-Shape R))
(define-type (Generated-Shape R) (Base-Shape R))
(define-type (Point-Shape R) (point R))
(define-type (Curve-Shape R) (Base-Shape R))
(define-type (Surface-Shape R) (Base-Shape R))
(define-type (Solid-Shape R) (Base-Shape R))
(define-type (Text-Shape R) (Base-Shape R))
(define-type (Extrudable-Shape R) (Base-Shape R))
(define-type (Base-Shape R) (shape R))

(define-type (Base-Shapes R) (Listof (Base-Shape R)))

;;The shared shape part
(struct (R) shape
  ([name : Symbol]
   [id : Integer]
   [realizer : (-> R)]
   [deleter : (-> Void)]
   [realized? : (-> Boolean)])
  #:property prop:custom-write (lambda (s port mode)
                                 (fprintf port "#<~A~A ~A>" 
                                          (if ((shape-realized? s)) "" "virtual ")
                                          (shape-name s)
                                          (shape-id s))))

(: mark-deleted! (All (R) (-> (shape R) Void)))
(define (mark-deleted! sh)
  ((shape-deleter sh)))

(: shape-reference (All (R) ((shape R) -> R)))
(define (shape-reference sh)
  ((shape-realizer sh)))

(: realized? (All (R) (-> (shape R) Boolean)))
(define (realized? sh)
  ((shape-realized? sh)))

(define-syntax-rule
  (with-references ([r s] ...) body ...)
  (let ([r (shape-reference s)] ...)
    body ...))

(: incr-shape-counter (-> Integer))
(define incr-shape-counter
  (let ((counter -1))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))

(define #:forall (R) (create-realizer-deleter [realize : (-> R)]) : (Values (-> R) (-> Void) (-> Boolean))
  (let ((ref : (Option R) #f)
        (created : Integer 0)
        (deleted : Integer 0))
    (define (realizer)
      (cond ((= created deleted) ;;Not realized
             (let ((r : R (realize)))
               (set! ref r)
               (set! created (add1 created))
               r))
            ((= created (add1 deleted))
             (or ref
                 (error "The shape was created but doesn't have a reference")))
            (else
             (error (format "Inconsistent creation (~A) and deletion (~A)!" created deleted)))))
    (define (deleter) : Void
      (cond ((= created (add1 deleted))
             (set! ref #f)
             (set! deleted (add1 deleted)))
            (else
             (error (format "Inconsistent creation (~A) and deletion (~A)!" created deleted)))))
    (define (realized?)
      (= created (add1 deleted)))
    (values realizer deleter realized?)))

(define-syntax (def-base-shape stx)
  (syntax-case stx ()
    [(_ type ((name constructor-name) param ...))
     (with-syntax* ([R (datum->syntax stx 'R)]
                    [([param-name param-type] ...)
                     (map (lambda (p)
                            (syntax-case p (:)
                              [[name : type default] #'[name type]]
                              [[name : type] #'[name type]]))
                          (syntax->list #'(param ...)))])
       (syntax/loc stx
         (begin
           (provide (except-out (struct-out name) name)) ;;Exclude constructor
           (provide constructor-name)
           (struct (R) name shape ([param-name : param-type] ...))
           (define #:forall (R) (constructor-name [realize : (-> R)] [param-name : param-type] ...) : (name R)
             (let-values ([(realizer deleter realized?) (create-realizer-deleter realize)])
               (let ((s (name 'name
                              (incr-shape-counter)
                              realizer
                              deleter
                              realized?
                              param-name ...)))
                 (when (immediate-mode?)
                   (realizer))
                 s))))))]
    [(def type (name param ...))
     (with-syntax ([constructor-name (build-name #'name "new-~A")])
       (syntax/loc stx
         (def type ((name constructor-name) param ...))))]
    [(def (name param ...))
     (with-syntax ([type #'shape])
       (syntax/loc stx
         (def type (name param ...))))]))

(define-syntax (def-shape stx)
  (syntax-case stx (:)
    [(_ ((name constructor) param ...) body ...)
     (with-syntax ([(param-name ...)
                    (map (lambda (p)
                           (syntax-case p (:)
                             [[name : type default] #'name]
                             [[name : type] #'name]))
                         (syntax->list #'(param ...)))])
       (syntax/loc stx
         (begin
           (provide name)
           (define (name param ...)
             (constructor (lambda () body ...)
                          param-name ...)))))]
    [(def (name param ...) body ...)
     (with-syntax ([constructor (build-name #'name "new-~A")])
       (syntax/loc stx
         (def ((name constructor) param ...) body ...)))]))

(define-syntax (def* stx)
  (syntax-case stx (: *)
    [(_ (name [t : type *]) body ...)
     (syntax/loc stx
       (define (name [t-or-ts : (U type (Listof type))] . [ts : type *])
         (let ((t (if (list? t-or-ts)
                      (if (null? ts)
                          t-or-ts
                          (append t-or-ts ts))
                      (cons t-or-ts ts))))
           body ...)))]))

;;For the moment, we will force these functions to accept just a list of arguments
#;(define-syntax (def* stx)
  (syntax-case stx ()
    [(_ (name [t : type *]) body ...)
     (syntax/loc stx
       (define (name [t : (Listof type)])
         body ...))]))

(define-syntax (def-shape* stx)
  (syntax-case stx (: *)
    [(_ ((name func-name) [ts : type *]) body ...)
     (syntax/loc stx
       (begin
         (provide name)
         (def* (name [ts : type *])
           (func-name (lambda () body ...)
                      ts))))]
    [(def (name [ts : type *]) body ...)
     (with-syntax ([constructor (build-name #'name "new-~A")])
       (syntax/loc stx
         (def ((name constructor) [ts : type *]) body ...)))]))


#;
(define-syntax (def/provide/key stx)
  (syntax-case stx ()
    [(_ (name param ...) body ...)
     (with-syntax ([name: (build-name #'name "~A:")]
                   [(key&param ...) (append-map (lambda (param)
                                                  (syntax-case param (:)
                                                    [[name : type ...]
                                                     (list (string->keyword (symbol->string (syntax-e #'name)))
                                                           param)]))
                                                (syntax->list #'(param ...)))])
       (syntax/loc stx
         (begin
           (provide name)
           (define (name param ...) body ...)
           (provide name:)
           (define (name: key&param ...) body ...))))]))

(define-for-syntax (build-name id fmt)
  (format-id id #:source id fmt (syntax-e id)))

(define-for-syntax (without-defaults params)
  (map (lambda (param)
         (let ((datum (syntax-e param)))
           (if (pair? datum)
               (car datum)
               param)))
       (syntax->list params)))

(def-base-shape (empty-shape))
(def-base-shape (universal-shape))

(struct (R) 0D-shape shape
  ())

(struct (R) 1D-shape shape
  ())

(struct (R) 2D-shape shape
  ())

(struct (R) 3D-shape shape
  ())

(def-base-shape 0D-shape (point [position : Loc (u0)]))

(def-base-shape 1D-shape (line [vertices : (Listof Loc) (list (u0) (ux))]))
#|
(def-base-shape (bounding-box s))
(def-base-shape (closed shape))
|#
(def-base-shape 1D-shape (closed-line [vertices : (Listof Loc) (list (u0) (ux) (uy))]))
(def-base-shape 1D-shape (spline [pts : (Listof Loc) (list (u0) (ux) (uy))]))
;;TODO IN ALL BACKENDS
(def-base-shape 1D-shape (spline* [pts : (Listof Loc) (list (u0) (ux) (uy))] [v0 : (U Boolean Vec) #f] [v1 : (U Boolean Vec) #f]))
(def-base-shape 1D-shape (closed-spline [pts : (Listof Loc)]))
(def-base-shape 1D-shape (circle [center : Loc (u0)] [radius : Real 1]))
(def-base-shape 1D-shape (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi]))
(def-base-shape 1D-shape (elliptic-arc [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1] [start-angle : Real 0] [amplitude : Real pi]))
(def-base-shape 1D-shape (ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1]))
(def-base-shape 1D-shape (polygon [vertices : (Listof Loc) (list (u0) (ux) (uy))]))
(def-base-shape 1D-shape (regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f]))
(def-base-shape 1D-shape (rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1]))

(def-base-shape 2D-shape (surface-circle [center : Loc (u0)] [radius : Real 1]))
(def-base-shape 2D-shape (surface-arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi]))
(def-base-shape 2D-shape (surface-polygon [vertices : (Listof Loc) (list (u0) (ux) (uy))]))
(def-base-shape 2D-shape (surface-regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f]))
(def-base-shape 2D-shape (surface-rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1]))
(def-base-shape (text [str : String ""] [c : Loc (u0)] [h : Real 1]))
(def-base-shape (text-centered [str : String ""] [c : Loc (u0)] [h : Real 1]))
(def-base-shape 3D-shape (sphere [c : Loc (u0)] [r : Real 1]))
(def-base-shape 3D-shape (torus [c : Loc (u0)] [re : Real 1] [ri : Real 1/2]))
(def-base-shape 3D-shape
  (cuboid [b0 : Loc (u0)]
          [b1 : Loc (+x b0 1)]
          [b2 : Loc (+y b1 1)]
          [b3 : Loc (+y b0 1)]
          [t0 : Loc (+z b0 1)]
          [t1 : Loc (+x t0 1)]
          [t2 : Loc (+y t1 1)]
          [t3 : Loc (+y t0 1)]))
(def-base-shape 3D-shape (regular-pyramid-frustum [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [rt : Real 1] [inscribed? : Boolean #f]))
(def-base-shape 3D-shape (regular-pyramid [edges : Integer 3] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : LocOrZ 1] [inscribed? : Boolean #f]))
(def-base-shape 3D-shape (irregular-pyramid [cbs : Locs (list (ux) (uy) (uxy))] [ct : Loc (uz)]))
(def-base-shape 3D-shape (regular-prism [edges : Integer 3] [cb : Loc (u0)] [r : Real 1] [a : Real 0] [h/ct : LocOrZ 1] [inscribed? : Boolean #f]))
(def-base-shape 3D-shape (irregular-prism [cbs : Locs (list (ux) (uy) (uxy))] [h/ct : LocOrZ 1] [solid? : Boolean #t]))
(def-base-shape 3D-shape (right-cuboid [cb : Loc (u0)] [width : Real 1] [height : Real 1] [h/ct : LocOrZ 1]))
(def-base-shape 3D-shape (box [c : Loc (u0)] [dx/c1 : LocOrZ 1] [dy : Real (if (number? dx/c1) dx/c1 1)] [dz : Real dy]))
(def-base-shape 3D-shape (cone [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1]))
(def-base-shape 3D-shape (cone-frustum [cb : Loc (u0)] [rb : Real 1] [h/ct : LocOrZ 1] [rt : Real 1]))
(def-base-shape 3D-shape (cylinder [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1]))

(def-base-shape (extrusion [profile : (Extrudable-Shape R)] [dir : VecOrZ 1]))
(def-base-shape (move [shape : (shape R)] [v : Vec]))
(def-base-shape (mirror [shape : (shape R)] [p : Loc (u0)] [n : Vec (vz)] [copy? : Boolean #t]))
(def-base-shape (scale [shape : (shape R)] [s : Real 1] [p : Loc (u0)]))
(def-base-shape (rotate [shape : (shape R)] [a : Real pi/2] [p0 : Loc (u0)] [p1 : Loc (+z p0 1)]))

#|
(def-base-shape (offset shape distance))
(def-base-shape (offset-curve shape direction distance normal))
|#
(def-base-shape (revolve [shape : (shape R)] [p0 : Loc (u0)] [v : Loc (vz 1)] [start-angle : Real 0] [amplitude : Real 2pi]))
(def-base-shape (thicken [shape : (shape R)] [h : Real 1]))
(def-base-shape (join-curves [shapes : (Listof (Curve-Shape R))]))
#|(def-base-shape (join-surfaces shapes))
(def-base-shape (inner-solid shapes))
(def-base-shape (solid shapes))
|#
(def-base-shape 2D-shape (surface [profiles : (Listof (Curve-Shape R))]))
#|
(def-base-shape (planar-surface shapes))
|#
(def-base-shape 2D-shape (surface-grid [points : (Listof (Listof Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f]))
#|
(def-base-shape (mesh-grid [points : (Listof (List Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f]))
|#

(def-base-shape (sweep [path : (Curve-Shape R)] [profile : (Extrudable-Shape R)] [rotation : Real 0] [scale : Real 1]))
(def-base-shape (loft [profiles : (Listof (Extrudable-Shape R))]
                      [paths : (Listof (Curve-Shape R))]
                      [ruled? : Boolean #f]
                      [closed? : Boolean #f]))
(def-base-shape (slice [shape : (shape R)] [p : Loc (u0)] [n : Vec (vz 1)]))
#|
(def-base-shape (half-space p n))
(def-base-shape (contour s p spacing))
|#
(def-base-shape (union [shapes : (Base-Shapes R)]))
(def-base-shape (intersection [shapes : (Base-Shapes R)]))
(def-base-shape (subtraction [shapes : (Base-Shapes R)]))

(def-base-shape 2D-shape (triangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc]))
(def-base-shape 2D-shape (quadrangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc]))
(def-base-shape (unknown))

(define #:forall (R)
  (rectangle-morph [c : Loc] [dx : Real] [dy : Real]
                   [point : (Loc -> R)]
                   [line : (Loc Loc -> R)])
  : (Option R)
  (cond ((= dx dy 0)
         (point c))
        ((= dx 0)
         (line c (+y c dy)))
        ((= dy 0)
         (line c (+x c dx)))
        (else
         #f)))

(define #:forall (R)
  (axial-morph [c : Loc] [r : Real] [l : Real]
               [point : (Loc -> R)]
               [circle : (Loc Real -> R)]
               [line : (Loc Loc -> R)])
  : (Option R)
  (cond ((and (= l 0) (= r 0))
         (point c))
        ((= l 0)
         (circle c r))
        ((= r 0)
         (line c (+z c l)))
        (else
         #f)))

(define (rectangle-deltas [c : Loc] [dx/c1 : (U Real Loc)] [dy : Real]) : (Values Real Real)
  (if (number? dx/c1)
      (values dx/c1 dy)
      (let ((d (p-p dx/c1 c)))
        (values (cx d) (cy d)))))

;;Mass modeling
;;These are operations more suitable for BIM

(def-base-shape 3D-shape (polygonal-mass [points : Locs] [height : Real]))
(def-base-shape 3D-shape (rectangular-mass [center : Loc] [width : Real] [length : Real] [height : Real]))

;;BIM

(def-base-shape 3D-shape (beam [p0 : Loc] [p1 : Loc] [width : Real] [height : Real]))
