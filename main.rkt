#lang typed/racket/base
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

(define-type Shape (U autocad:Shape rhino:Shape))

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

(define-syntax (def-backend stx)
  (syntax-case stx ()
    [(_ (backend ...) (name param ...) backend-expr)
     (with-syntax ([((backend (backend-call arg ...)) ...)
                    (map (lambda (backend)
                           (with-syntax ([backend-shape
                                          (format-id backend
                                                     #:source backend
                                                     "~A:Shape"
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
                                                    (syntax-case param (: Shape)
                                                      [[name : Shape] #'(cast name backend-shape)]
                                                      [[name : type default] #'name]
                                                      [[name : type] #'name]))
                                                  (syntax->list #'(param ...))))
                                       (with-syntax ([backend backend])
                                         #'(error (format "Operation ~A is not available in backend ~A"
                                                          'name 'backend)))))))
                         (syntax->list #'(backend ...)))])
       (syntax/loc stx
         (begin
           (provide name)
           (define (name param ...)
             (case-backend
              backend-expr
              name
              [backend (backend-call arg ...)] ...)))))]
    [(def (name param ...) backend-expr)
     (syntax/loc stx
       (def (autocad rhino revit) (name param ...) backend-expr))]))

(define-syntax (def-new-shape stx)
  (syntax-case stx ()
    [(_ (name param ...))
     (syntax/loc stx
       (def-backend (name param ...) (current-out-backend)))]))

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

(def-new-shape (empty-shape))
(def-new-shape (universal-shape))
(def-new-shape (point [position : Loc (u0)]))
(def-new-shape (circle [center : Loc (u0)] [radius : Real 1]))
(def-new-shape (surface-circle [center : Loc (u0)] [radius : Real 1]))
(def-new-shape (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi]))
(def-new-shape (surface-arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi]))
;(def-shape (elliptic-arc [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1] [start-angle : Real 0] [amplitude : Real pi]))
(def-new-shape (ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1]))
(def-new-shape (surface-ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1]))
(def-new-shape (line [pts : (Listof Loc) (list (u0) (ux))]))
#|
(def-shape (bounding-box s))
(def-shape (closed shape))
|#
(def-new-shape (closed-line [pts : (Listof Loc) (list (u0) (ux) (uy))]))
(def-new-shape (spline [pts : (Listof Loc) (list (u0) (ux) (uy))]))
;;TODO IN ALL BACKENDS
;(def-shape (spline* [pts : (Listof Loc) (list (u0) (ux) (uy))] [v0 : (U Boolean Vec) #f] [v1 : (U Boolean Vec) #f]))
(def-new-shape (closed-spline [pts : (Listof Loc)]))
(def-new-shape (polygon [pts : (Listof Loc) (list (u0) (ux) (uy))]))
(def-new-shape (surface-polygon [pts : (Listof Loc) (list (u0) (ux) (uy))]))
(def-new-shape (regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f]))
(def-new-shape (surface-regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f]))
(def-new-shape (rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1]))
(def-new-shape (surface-rectangle [c : Loc (u0)] [dx/c1 : (U Real Loc) 1] [dy : Real 1]))
(def-new-shape (text [str : String ""] [c : Loc (u0)] [h : Real 1]))
(def-new-shape (text-centered [str : String ""] [c : Loc (u0)] [h : Real 1]))
(def-new-shape (sphere [c : Loc (u0)] [r : Real 1]))
(def-new-shape (torus [c : Loc (u0)] [re : Real 1] [ri : Real 1/2]))
(def-new-shape (cuboid [b0 : Loc (u0)]
                        [b1 : Loc (+x b0 1)]
                        [b2 : Loc (+y b1 1)]
                        [b3 : Loc (+y b0 1)]
                        [t0 : Loc (+z b0 1)]
                        [t1 : Loc (+x t0 1)]
                        [t2 : Loc (+y t1 1)]
                        [t3 : Loc (+y t0 1)]))
(def-new-shape (regular-pyramid-frustum [edges : Integer 4] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : (U Real Loc) 1] [rt : Real 1] [inscribed? : Boolean #f]))
(def-new-shape (regular-pyramid [edges : Integer 3] [cb : Loc (u0)] [rb : Real 1] [a : Real 0] [h/ct : LocOrZ 1] [inscribed? : Boolean #f]))
(def-new-shape (irregular-pyramid [cbs : Locs (list (ux) (uy) (uxy))] [ct : Loc (uz)]))
(def-new-shape (regular-prism [edges : Integer 3] [cb : Loc (u0)] [r : Real 1] [a : Real 0] [h/ct : LocOrZ 1] [inscribed? : Boolean #f]))
(def-new-shape (irregular-prism [cbs : Locs (list (ux) (uy) (uxy))] [h/ct : LocOrZ 1] [solid? : Boolean #t]))
(def-new-shape (right-cuboid [cb : Loc (u0)] [width : Real 1] [height : Real 1] [h/ct : LocOrZ 1]))
(def-new-shape (box [c : Loc (u0)] [dx/c1 : LocOrZ 1] [dy : Real (if (number? dx/c1) dx/c1 1)] [dz : Real dy]))
(def-new-shape (cone [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1]))
(def-new-shape (cone-frustum [cb : Loc (u0)] [rb : Real 1] [h/ct : LocOrZ 1] [rt : Real 1]))
(def-new-shape (cylinder [cb : Loc (u0)] [r : Real 1] [h/ct : LocOrZ 1]))
(def-new-shape-op (extrusion [profile : Shape] [dir : VecOrZ 1]))
(def-new-shape-op (move [shape : Shape] [v : Vec]))
(def-new-shape-op (mirror [shape : Shape] [p : Loc (u0)] [n : Vec (vz)] [copy? : Boolean #t]))
(def-new-shape-op (scale [shape : Shape] [s : Real 1] [p : Loc (u0)]))
(def-new-shape-op (rotate [shape : Shape] [a : Real pi/2] [p0 : Loc (u0)] [p1 : Loc (+z p0 1)]))

#|
(def-shape (offset shape distance))
(def-shape (offset-curve shape direction distance normal))
|#
(def-new-shape-op (revolve [shape : Shape] [p0 : Loc (u0)] [v : Vec (vz 1)] [start-angle : Real 0] [amplitude : Real 2pi]))
#|
(def-shape (thicken shape [h 1]))
|#
;(def-new-shape (join-curves [shapes : (Listof Shape)]))
#|(def-shape (join-surfaces shapes))
(def-shape (inner-solid shapes))
(def-shape (solid shapes))
|#
(def-new-shape (surface [profile : Shape]))
#|
(def-shape (planar-surface shapes))
|#
(def-new-shape (surface-grid [points : (Listof (Listof Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f]))
#|
(def-shape (mesh-grid [points : (Listof (List Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f]))
|#

(def-new-shape (sweep [path : Shape] [profile : Shape] [rotation : Real 0] [scale : Real 1]))
#|
(def-shape (loft [profiles : (Listof Shape)]
                      [ruled? : Boolean #f] [solid? : Boolean #f] [closed? : Boolean #f]))
(def-shape (loft-guided [profiles : (Listof Shape)]
                             [paths : (Listof Shape)]
                             [ruled? : Boolean #f] [solid? : Boolean #f] [closed? : Boolean #f]))
(def-shape (loft-curves [profiles : (Listof Shape)]
                             [ruled? : Boolean #f] [solid? : Boolean #f] [closed? : Boolean #f]))
(def-shape (loft-curve-point [profile : Shape]
                                  [point : Shape]
                                  [solid? : Boolean #f]))
(def-shape (loft-surfaces [profiles : (Listof Shape)]
                               [ruled? : Boolean #f] [solid? : Boolean #f] [closed? : Boolean #f]))
(def-shape (loft-surface-point [profile : Shape]
                                    [point : Shape]
                                    [solid? : Boolean #f]))
|#
(def-new-shape (slice [shape : Shape] [p : Loc (u0)] [n : Vec (vz 1)]))
#|
(def-shape (half-space p n))
(def-shape (contour s p spacing))
|#
#|
(def-new-shape (union [shapes : Shape]))
(def-new-shape (intersection [shapes : Shape]))
(def-new-shape (subtraction [shapes : Shape]))
|#
(def-new-shape (triangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc]))
(def-new-shape (quadrangle-face [p0 : Loc] [p1 : Loc] [p2 : Loc] [p3 : Loc]))
;(def-shape (unknown))


#|
;;Choose backend
(provide select-backend)
(define (select-backend backend [op 'none])
  (in-out-backend backend backend op))

|#

(def-backend (all-shapes) (current-in-backend))
(def-backend (delete-all-shapes) (current-out-backend))
(def-backend (connect-to-revit) (current-out-backend))


(def-new-shape (polygonal-mass [points : Locs] [height : Real]))