#lang typed/racket/base/no-check
(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/list))
(require racket/math)
(require "base/coord.rkt")
(require "base/utils.rkt")
(require "base/shapes.rkt")
(require (prefix-in autocad: "autocad/backend.rkt"))
(require (prefix-in rhino: "rhinoceros/backend.rkt"))
(require (prefix-in revit: "revit/backend.rkt"))
(require (prefix-in sketchup: "sketchup/backend.rkt"))

(provide (all-defined-out))
(provide (all-from-out "base/coord.rkt"))
(provide (all-from-out "base/utils.rkt"))
(provide (all-from-out "base/shapes.rkt"))

(provide current-out-backend)
(provide current-in-backend)
(provide current-in-out-backend)

(define-type Backend String)
(define undecided : Backend "Undecided")
(define autocad : Backend (autocad:current-backend-name))
(define rhino : Backend (rhino:current-backend-name))
(define sketchup : Backend (sketchup:current-backend-name))
(define revit : Backend (revit:current-backend-name))

;;The current out backend is the one active at
;;a given moment, used only during shape creation
(define current-out-backend : (Parameterof (Option Backend)) (make-parameter undecided))

;;The current in backend is the one active at
;;a given moment, used only during shape acquisition
(define current-in-backend : (Parameterof (Option Backend)) (make-parameter undecided))

(define (current-in-out-backend [in : Backend] [out : Backend in] [op : Symbol 'none]) : Void
  (current-in-backend in)
  (current-out-backend out)  
#|  (render-backend-dir (backend-name out))
  (start-backend in)
  (unless (eq? in out)
    (start-backend out))
  (case op
    [(none) (void)]
    [(delete)
     (delete-all-shapes)
     (void)])
  #;(delayed-mode)
  (immediate-mode)
|#
  (void))


(define (backend [b : Backend])
  (current-in-out-backend b))

#|;;The realize function is called whenever we try to visualize
;;(print, in Racket terms) the delayed shape
(define default-current-print (current-print))

(define (rosetta-print value)
  (default-current-print
    (parameterize ((immediate-mode? #f))
      (realize value))))

(provide immediate-mode)
(define (immediate-mode)
;  (delayed-mode? #f)
  (current-print default-current-print))

(provide delayed-mode)
(define (delayed-mode)
;  (delayed-mode? #t)
  (current-print rosetta-print))

;;Now, the operations that depend on the backend
|#
(provide Shape Shapes)

(define-type Shape (U autocad:Shape rhino:Shape sketchup:Shape revit:Shape))
(define-type Shapes (U autocad:Shapes rhino:Shapes sketchup:Shapes revit:Shapes))


;;Second, a macro for defining operations that wrap the results of the
;;backend operations

;;The most generic
(define-syntax (case-backend stx)
  (syntax-case stx ()
    ((case-backend expr name clause ...)
     (syntax/loc stx
       (let ((backend expr))
         (case-backend* backend name clause ...))))))

(define-syntax (case-backend* stx)
  (syntax-case stx (else)  ;;In the future, optimize this with a jump table
    ((_ b name)
     (syntax/loc stx
       (error 'name "Unknown backend ~a" b)))
    ((_ b name (else body ...))
     (syntax/loc stx
       (begin body ...)))
    ((_ b name ((backend ...) body ...) clause ...)
     (syntax/loc stx
       (if (or (eq? b backend) ...)
           (begin body ...)
           (case-backend* b name clause ...))))
    ((_ b name (backend body ...) clause ...)
     (syntax/loc stx
       (case-backend* b name ((backend) body ...) clause ...)))))

;;Using the current-out-backend
(define-syntax (case-current-backend stx)
  (syntax-case stx ()
    ((_ name clause ...)
     (syntax/loc stx
       (case-backend (current-out-backend) name clause ...)))))

;;Using a shape
(define-syntax (case-shape-backend stx)
  (syntax-case stx ()
    ((_ expr name clause ...)
     (syntax/loc stx
       (case-backend (shape-backend expr) name clause ...)))))  


(define-syntax (delegate-backend stx)
  (syntax-case stx ()
    [(_ backend-expr (backend ...) (name param ...))
     (with-syntax ([((backend (backend-call arg ...)) ...)
                    (map (lambda (backend)
                           (with-syntax ([backend-shape
                                          (format-id backend
                                                     #:source backend
                                                     "~A:Shape"
                                                     (syntax-e backend))]
                                         [backend-shapes
                                          (format-id backend
                                                     #:source backend
                                                     "~A:Shapes"
                                                     (syntax-e backend))]
                                         [backend-func
                                          (format-id backend
                                                     #:source backend
                                                     "~A:~A"
                                                     (syntax-e backend)
                                                     (syntax-e #'name))])
                             (list backend
                                   (if (identifier-binding #'backend-func)
                                       (cons #'backend-func
                                             (map (lambda (param)
                                                    (syntax-case param (: Shape Shapes Listof)
                                                      [[name : Shape . default] #'(cast name backend-shape)]
                                                      [[name : Shapes . default] #'(cast name backend-shapes)]
                                                      [[name : (Listof Shape) . default] #'(cast name backend-shapes)]
                                                      [[name : type . default] #'name]))
                                                  (syntax->list #'(param ...))))
                                       (with-syntax ([backend backend])
                                         #'(error (format "Operation ~A is not available in backend ~A"
                                                          'name 'backend)))))))
                         (syntax->list #'(backend ...)))])
       (syntax/loc stx
         (case-backend
          backend-expr
          name
          [backend (backend-call arg ...)] ...)))]
    [(delegate backend-expr (name param ...))
     (syntax/loc stx
       (delegate backend-expr (autocad rhino sketchup revit) (name param ...)))]))

(define-syntax (def-backend stx)
  (syntax-case stx ()
    [(_ backend-expr (name param ...))
     (syntax/loc stx
       (begin
         (provide name)
         (define (name param ...)
           (delegate-backend backend-expr (name param ...)))))]
    [(def (name param ...))
     (syntax/loc stx
       (def (current-out-backend) (name param ...)))]))

(define-syntax (def-new-shape-op stx)
  (syntax-case stx ()
    [(_ (name param ...))
     (with-syntax ([[target : type]
                    (findf (lambda (param)
                             (syntax-case param (: Shape)
                               [[name : Shape] #'name]
                               [_ #f]))
                           (syntax->list #'(param ...)))])
       (syntax/loc stx
         (def-backend
           (name param ...)
           #;(shape-backend target) ;;Can't use this with the current architecture
           (current-out-backend))))]))

(define-syntax (def-backend* stx)
  (syntax-case stx (: *)
    [(_ (name [t : type *]))
     (syntax/loc stx
       (define (name [t-or-ts : (U type (Listof type))] . [ts : type *])
         (let ((t (if (list? t-or-ts)
                      (if (null? ts)
                          t-or-ts
                          (append t-or-ts ts))
                      (cons t-or-ts ts))))
           (delegate-backend (current-out-backend) (name [t : (Listof type)])))))]))

(def-backend (empty-shape))
(def-backend (universal-shape))
(def-backend (point [position : Loc (u0)]))
(def-backend (circle [center : Loc (u0)] [radius : Real 1]))
(def-backend (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi]))
(def-backend (ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1]))
(def-backend (surface-circle [center : Loc (u0)] [radius : Real 1]))
(def-backend (surface-arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi]))
(def-backend (surface-ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1]))
(def-backend* (line [pts : Loc *]))
(def-backend* (closed-line [pts : Loc *]))
(def-backend* (polygon [pts : Loc *]))
(def-backend* (spline [pts : Loc *])); (list (u0) (ux) (uy))] [v0 : (U #f Vec) #f] [v1 : (U #f Vec) #f]))
(def-backend (spline* [pts : Locs] [v0 : (U #f Vec) #f] [v1 : (U #f Vec) #f]))
(def-backend* (closed-spline [pts : Loc *]))
(def-backend (curve-start-location [curve : Shape]))
(def-backend (curve-end-location [curve : Shape]))
(def-backend (curve-domain [curve : Shape]))
(def-backend (curve-frame-at [curve : Shape] [t : Real]))
(def-backend (curve-frame-at-length [curve : Shape] [t : Real]))
(def-backend (curve-length [curve : Shape]))
;;HACK These two functions require the default initialization on last? but Typed Racket has a bug and prevents the use of #:forall (A)
(: map-curve-division (All (A) (->* ((-> Loc A) Shape Integer) (Boolean) (Listof A))))
(: map-curve-length-division (All (A) (->* ((-> Loc A) Shape Integer) (Boolean) (Listof A))))
(def-backend (map-curve-division [f : (-> Loc A)] [curve : Shape] [n : Integer] [last? : Boolean #t]))
(def-backend (map-curve-length-division [f : (-> Loc A)] [curve : Shape] [n : Integer] [last? : Boolean #t]))
(def-backend* (surface-polygon [pts : Loc *]))
(def-backend (rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1]))
(def-backend (surface-rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1]))
(def-backend (regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f]))
(def-backend (surface-regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f]))
(def-backend (surface-boundary [shape : Shape]))
(def-backend (loft-curve-point [curve : Shape] [point : Shape]))
(def-backend (loft-surface-point [surface : Shape] [point : Shape]))
#;
(def-backend (loft-profiles [profiles : Shapes] [rails : Shapes] [solid? : Boolean] [ruled? : Boolean] [closed? : Boolean]))
#;#;
(def-backend (loft-curves [shapes : Shapes] [rails : Shapes]))
(def-backend (loft-surfaces [shapes : Shapes] [rails : Shapes]))
(def-backend (loft [profiles : Shapes] [rails : Shapes (list)] [ruled? : Boolean #f] [closed? : Boolean #f]))
(def-backend (loft-ruled [profiles : Shapes]))
(def-backend (irregular-pyramid [cbs : Locs (list (ux) (uy) (uxy))] [ct : Loc (uz)]))
(def-backend (regular-pyramid-frustum [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [rt : Real 1] [inscribed? : Boolean #f]))
(def-backend (regular-pyramid [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f]))
(def-backend (regular-prism [edges : Integer 4] [cb : Loc (u0)] [r : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [inscribed? : Boolean #f]))
(def-backend (right-cuboid [cb : Loc (u0)] [width : Real 1] [height : Real 1] [h/ct : LocOrZ 1]))
(def-backend (cylinder [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1]))
(def-backend (box [c : Loc (u0)] [dx/c1 : LocOrZ 1] [dy : Real (if (number? dx/c1) dx/c1 1)] [dz : Real dy]))
(def-backend (cone [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1]))
(def-backend (cone-frustum [cb : Loc (u0)] [rb : Real 1] [h/ct : LocOrZ 1] [rt : Real 1]))
(def-backend (cuboid [b0 : Loc (u0)]
                   [b1 : Loc (+x b0 1)]
                   [b2 : Loc (+y b1 1)]
                   [b3 : Loc (+y b0 1)]
                   [t0 : Loc (+z b0 1)]
                   [t1 : Loc (+x t0 1)]
                   [t2 : Loc (+y t1 1)]
                   [t3 : Loc (+y t0 1)]))
(def-backend (sphere [c : Loc (u0)] [r : Real 1]))
(def-backend (surface-grid [ptss : (Listof (Listof Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f]))
(def-backend (text [str : String ""] [p : Loc (u0)] [h : Real 1]))
(def-backend (text-length [str : String ""] [h : Real 1]))
(def-backend (text-centered [str : String ""] [p : Loc (u0)] [h : Real 1]))
(def-backend (torus [center : Loc (u0)] [re : Real 1] [ri : Real 1/2]))
(def-backend* (surface [profiles : Shape *]))
(def-backend (join-curves [shapes : Shapes]))
(def-backend (revolve [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1)] [start-angle : Real 0] [amplitude : Real 2pi]))
(def-backend (curve? [s : Shape]))
(def-backend (surface-region? [s : Shape]))
(def-backend (extrusion [profile : Shape] [dir : VecOrZ 1]))
(def-backend (sweep [path : Shape] [profile : Shape] [rotation : Real 0] [scale : Real 1]))
(def-backend (thicken [surf : Shape] [h : Real 1]))
(def-backend (slice [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1 p)]))
(def-backend (triangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc]))
(def-backend (quadrangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc]))
(def-backend (move [shape : Shape] [v : Vec (vx)]))
(def-backend (rotate [shape : Shape] [a : Real pi/2] [p0 : Loc (u0)] [p1 : Loc (+z p0 1)]))
(def-backend (scale [shape : Shape] [s : Real 1] [p : Loc (u0)]))
(def-backend (mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)] [copy? : Boolean #t]))
(def-backend (union-mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)]))
(def-backend (bounding-box [s : Shape]))

#;
(def-backend shape-color
  (case-lambda
    [([shape : Shape])
     (%true-color (shape-ref shape))]
    [([shape : Shape] [new-color : Color])
     (do-ref ([r shape])
       (%true-color r new-color))
     (void)]))
(def-backend (create-layer [name : String] [color : (Option Color) #f]))
(provide current-layer)
(define current-layer
  (case-lambda
    [()
     (delegate-backend (current-out-backend) (current-layer))]
    [([new-layer : Layer])
     (delegate-backend (current-out-backend) (current-layer [new-layer : Layer]))]))
#;
(def-backend shape-layer
  (case-lambda
    [([shape : Shape])
     (%get-layer (%layer (shape-ref shape)))]
    [([shape : Shape] [new-layer : Layer])
     (do-ref ([r shape])
       (%layer r new-layer))
     (void)]))
(def-backend (create-material [name : String]))
#;
(def-backend shape-material
  (case-lambda
    [([shape : Shape])
     (%material (shape-ref shape))]
    [([shape : Shape] [new-material : Material])
     (do-ref ([r shape])
       (%material r new-material))
     (void)]))
(def-backend (fast-view))
(def-backend (view [camera : (Option Loc) #f] [target : (Option Loc) #f] [lens : (Option Real) #f]))
(def-backend (view-expression))
(def-backend (view-top))
(def-backend (render-view [name : String]))
(def-backend (save-film-frame [obj : Any (void)]))
(def-backend (zoom-extents))
(def-backend (disable-update))
(def-backend (enable-update))
(def-backend (prompt-point [str : String "Select position"]))
(def-backend (prompt-integer [str : String "Integer?"]))
(def-backend (prompt-real [str : String "Real?"]))
(def-backend (prompt-shape [str : String "Select shape"]))
(def-backend (select-shape [s : Shape]))
(def-backend (select-shapes [ss : Shapes]))
(def-backend (polygonal-mass [pts : Locs] [height : Real]))
(def-backend* (union [shapes : Shape *]))
(def-backend* (intersection [shapes : Shape *]))
(def-backend* (subtraction [shapes : Shape *]))

(def-backend (current-in-backend) (all-shapes))
(def-backend (delete-all-shapes))
(def-backend (delete-shape [s : Shape]))
(def-backend (delete-shapes [s : Shapes]))

(def-backend (current-out-backend) (connect-to-revit))
