#lang typed/racket/base
(require racket/function)
(require "../base/typed-com.rkt"
         "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/enums.rkt"
         (except-in math random-integer))

(provide (all-defined-out) com-omit)

(define-syntax-rule
  (def-com name (param ...) rtype)
  (def-com-method name (rhino) (param ...) rtype))

#|
; constants

(define create-preview-image-honor-flag 1)
(define create-preview-image-draw-flag 2)
(define create-preview-image-ghosted-flag 4)

(define domain-direction-u 0)
(define domain-direction-v 1)

(define optional com-omit)

(define view-display-mode-wireframe 0)
(define view-display-mode-shaded 1)
(define view-display-mode-render-preview 2)

(define view-perspective "Perspective")

(define view-projection-mode-parallel 1)
(define view-projection-mode-perspective 2)

|#
; initialization

(define (load-rhino-com [progids : (Listof String)]) : Com-Object
  (define (safe-progid->clsid [progid : String]) : (Option CLSID)
    (with-handlers ((exn? (λ (e) #f)))
      (progid->clsid progid)))
  (let ((clsid 
         (or (ormap safe-progid->clsid progids)
             (error "Couldn't find Rhinoceros"))))
    (let ((coclass
           (with-handlers
               ((exn?
                 (λ (e)
                   (display "Starting Rhinoceros...")
                   (flush-output)
                   (begin0
                       (com-create-instance clsid)
                     (displayln "done!")))))
             (cast (com-get-active-object clsid) Com-Object))))
      (com-set-property! coclass "Visible" #t)
      (let retry ([count 3])
        (let ((result (com-invoke coclass "GetScriptObject")))
          (if (void? result)
              (cond ((> count 0)
                     ;(displayln "Waiting for Rhinoceros...")
                     (sleep 1)
                     (retry (- count 1)))
                    (else
                     (error "Couldn't access Rhinoceros")))
              (cast result Com-Object)))))))

(define-cached (rhino) : Com-Object
  (load-rhino-com '("Rhino5x64.Interface" "Rhino5.Interface" "Rhino4.Interface")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;We need to convert from the basic types expected by Rhino's COM interface and the types used in Rosetta

;;Rhino uses Ids (which is a vector of strings or void) but we use Refs (which is a list (possibly empty) of Strings)
(define-type Ref String)
(define-type Refs (Listof Ref))

;;To simplify type checking
(define-type Point (Vector Float Float Float))
(define-type Direction (Vector Float Float Float))
(define-type Plane Type-Described)
(define-type OutPlane (Vector (Vector Float Float Float)
                              (Vector Float Float Float)
                              (Vector Float Float Float)
                              (Vector Float Float Float)))
(define-type Points (Vectorof Point))
(define-type Integers (Vectorof Integer))
(define-type Booleans (Vectorof Boolean))
(define-type Double Float)
(define-type ArrDouble (Vectorof Float))

(define-syntax-rule (mf m i j) (real->double-flonum (matrix-ref m i j)))
(define-syntax-rule (vf v i j) (vector-ref (vector-ref v i) j))

(define (loc->plane [p : Xyz]) : Plane
  (let ((m (world-transformation p)))
    (type-describe
     (vector (vector (mf m 0 3) (mf m 1 3) (mf m 2 3))
             (vector (mf m 0 0) (mf m 1 0) (mf m 2 0))
             (vector (mf m 0 1) (mf m 1 1) (mf m 2 1))
             (vector (mf m 0 2) (mf m 1 2) (mf m 2 2)))
     '(array 4 (variant (array 3 double))))))

(define (matrix->rh-matrix [m : Real-Matrix]) : Rh-Matrix
  (type-describe
   (vector (vector (mf m 0 0) (mf m 1 0) (mf m 2 0) 0)
           (vector (mf m 0 1) (mf m 1 1) (mf m 2 1) 0)
           (vector (mf m 0 2) (mf m 1 2) (mf m 2 2) 0)
           (vector 0 0 0 1))
   '(array 4 (array 4 any))))

(define (matrix<-nested-plane [pl : OutPlane]) : Real-Matrix
  (matrix [[(vf pl 1 0) (vf pl 1 1) (vf pl 1 2) 0]
           [(vf pl 2 0) (vf pl 2 1) (vf pl 2 2) 0]
           [(vf pl 3 0) (vf pl 3 1) (vf pl 3 2) 0]
           [(vf pl 0 0) (vf pl 0 1) (vf pl 0 2) 1]]))

#|
;;This is so wrong. This only works with curves with a single intersection.
(define (list<-intersection-array v)
  (coord<-vector (vector-ref (vector-ref v 0) 1)))

;   (flat-plane<-matrix matrix)

(define (array-4array-int vals)
  (begin #;type-describe
   (apply vector-append (vector->list vals))
   #;`(array ,(* (vector-length vals) 4) int)))
|#
(define-type Id String)
(define-type IdOrVoid (U Id Void))
(define-type Ids (Vectorof String))
(define-type IdsOrVoid (U Ids Void))
(define-type BoolOrVoid (U Boolean Void))

(define (ids-or-void->refs [c : IdsOrVoid]) : Refs
  (if (void? c)
      (list)
      (vector->list c)))

(define (boolean-or-void->boolean [b : BoolOrVoid]) : Boolean
  (if (void? b)
      #f
      b))

(define-type Rh-Matrix Type-Described)
(define-type Real-Matrix (Matrix Real))

;;We upgrade types automatically, from the types that Rhino's COM requires and the types that Rosetta provides

(with-upgrade-types ([BoolOrVoid Boolean]
                     [Double Real]
                     [Point Loc]
                     [Direction Vec]
                     [Plane Loc]
                     [Points Locs]
                     [ArrDouble Locs]
                     [Ids Refs]
                     [IdsOrVoid Refs]
                     [ArrMeshIndexes MeshIndexes]
                     [Rh-Matrix Real-Matrix]
                     [OutPlane Real-Matrix]))

;;In most cases, upgrading from one type to another requires a conversion function for the type values

(with-conversions
    ([Real      Double       real->double-flonum]
     [Double    Real         identity]
     [Loc       Point        loc->vector-double-flonum]
     [Point     Loc          vector-double-flonum->loc]
     [Direction Vec          vector-double-flonum->vec]
     [Loc       Plane        loc->plane]
     [OutPlane  Loc          matrix-double-flonum->loc]
     [OutPlane  Real-Matrix  matrix<-nested-plane]
     [Locs      Points       locs->vector-vector-double-flonum]
     [Points    Locs         vector-vector-double-flonum->locs]
     [Locs      ArrDouble    locs->vector-3-double-flonums]
     [Refs      Ids          list->vector]
     [Ids       Refs         vector->list]
     [IdsOrVoid Refs         ids-or-void->refs]
     [BoolOrVoid Boolean     boolean-or-void->boolean]
     [Real-Matrix Rh-Matrix  matrix->rh-matrix]
     [MeshIndexes ArrMeshIndexes mesh-indexes->arr-mesh-indexes]))


(def-com add-arc ([center Plane] [radius Double] [angle Double]) Id)
;(def-com AddArc3Pt ([start Point] [end Point] [point Point]) Id)
(def-com add-box ([corners ArrDouble]) Id)
(def-com add-circle ([center Point] [radius Double]) Id)
(def-com (add-circle-plane AddCircle) ([center Plane] [radius Double]) Id)
(def-com add-cone ([base Point] [top Point] [radius Double] [cap? Boolean]) Id)
(def-com (add-cone-plane AddCone) ([base Plane] [height Double] [radius Double] [cap? Boolean]) Id)
(def-com add-curve ([points ArrDouble] #:opt [degree Integer]) Id)
(def-com add-cut-plane ([shapes Ids] [start Point] [end Point] #:opt [normal Point]) Id)
(def-com add-cylinder ([base Point] [top Point] [radius Double] [cap? Boolean]) Id)
(def-com (add-cylinder-plane AddCylinder) ([base Plane] [height Double] [radius Double] [cap? Boolean]) Id)

(def-com add-edge-srf ([curves Ids]) Id)
(def-com add-ellipse ([plane Plane] [radiusX Double] [radiusY Double]) Id)
(def-com add-extrusion ([curve Id] [height Double] #:opt [cap? Boolean] [plane Plane]) Id)

#|
(def-com Add-hatch (Id #:opt name real angle) Id)
|#

(define-incr-enum Knot-Style
  knot-style-uniform-knots
  knot-style-chord-length-spacing
  knot-style-sqrt
  knot-style-periodic-uniform-spacing
  knot-style-periodic-chord-length-spacing
  knot-style-periodic-sqrt)

(def-com add-interp-curve ([points ArrDouble] #:opt [degree Integer] [knot-style Knot-Style] [start-tangent Point] [end-tangent Point]) Id)
(def-com add-interp-curve-ex ([points ArrDouble] #:opt [degree Integer] [knot-style Knot-Style] [sharp Boolean] [start-tangent Point] [end-tangent Point]) Id)
(def-com (add-interp-curve-ex2 AddInterpCurveEx) ([points ArrDouble] #:opt [degree Integer] [knot-style Knot-Style] [sharp Boolean]) Id)

#|
; edit: color marshaling
(def-com Add-layer (#:opt name integer Boolean Boolean name) Identity)
|#

(def-com add-line ([start Point] [end Point]) Id)

(define-incr-enum LoftType
  lt-normal 
  lt-loose
  lt-straight
  lt-tight
  lt-developable)

(define-incr-enum LoftStyle
  ls-none 
  ls-rebuild
  ls-refit)

;(define loft-simplify integer)
(def-com add-loft-srf ([objects Ids]
                       #:opt
                       [start Point] 
                       [end Point] 
                       [loft-type LoftType] 
                       [loft-style LoftStyle]
                       [n Number]
                       [closed? Boolean])
  Ids)

;(def-com Add-material-to-layer (string) integer)

(define-type ArrMeshIndexes Type-Described)
(define-type MeshIndexes (Listof (Vector Integer Integer Integer Integer)))

(define (mesh-indexes->arr-mesh-indexes [idxss : MeshIndexes])
  (type-describe
   (list->vector idxss)
   `(array ,(length idxss) (variant (array 4 int)))))

(def-com add-mesh ([points ArrDouble] [vertices ArrMeshIndexes]) Id)

#|
(def-com Add-nurbs-surface
  ((arr-point-count list->vector) 
   arr-pointss 
   (arr-knot-u arr-reals) (arr-knot-v arr-reals)
   (arr-degree list->vector)
   #:opt (arr-weights arr-realss))
  Id)
|#

(def-com add-planar-srf ([objects Ids]) Ids)

#|

(def-com Add-plane-surface (plane real real) Id)
|#
(def-com add-point ([Center Point]) Id)
(def-com add-polyline ([locs ArrDouble]) Id)
(def-com add-rev-srf ([shape Id] [axis ArrDouble] #:opt [start Number] [end Number]) Id)
(def-com add-sphere ([center Point] [radius Double]) Id)
(def-com (add-sphere-plane AddSphere) ([center Plane] [radius Double]) Id)

;(def-com Add-srf-contour-crvs (Id plane real) Ids)
(def-com add-srf-pt ([points ArrDouble]) Id)
(def-com add-srf-pt-grid ([dims Integers] [points ArrDouble] #:opt [degrees Integers] [closed? Booleans]) Id)
#|
(def-com Add-srf-section-crvs (Id plane) maybe-Ids)

(define text-style integer)
|#

(define-incr-enum FontStyle 
  fs-Normal
  fs-Bold
  fs-Italic)

(define-incr-enum Justification
  j-Bottom-Left
  j-Bottom-Center
  j-Bottom-Right
  j-Middle-Left
  j-Middle-Center
  j-Middle-Right
  j-Top-Left
  j-Top-Middle
  j-Top-Right)
 
(def-com add-text ([text String] [point Point] #:opt [height Double] [font String] [font-style FontStyle] [justification Justification]) Id)
(def-com (add-text-plane AddText) ([text String] [point Plane] #:opt [height Double] [font String] [font-style FontStyle] [justification Justification]) Id)
(def-com add-torus ([base Point] [major-radius Double] [minor-radius Double] [direction Point]) Id)
(def-com (add-torus-plane AddTorus) ([base Plane] [major-radius Double] [minor-radius Double]) Id)
(def-com add-truncated-cone ([base Point] [base-radius Double] [height Double] [top-radius Double] [cap? Boolean]) Id)
(def-com (add-truncated-cone-plane AddTruncatedCone) ([base Plane] [base-radius Double] [height Double] [top-radius Double] [cap? Boolean]) Id)
(def-com all-objects (#:opt [select? Boolean] [include-lights? Boolean] [include-grips? Boolean]) IdsOrVoid)
(def-com boolean-difference ([ids0 Ids] [ids1 Ids] #:opt [delete? Boolean]) IdsOrVoid)
(def-com boolean-intersection ([ids0 Ids] [ids1 Ids] #:opt [delete? Boolean]) IdsOrVoid)
(def-com boolean-union ([ids Ids] #:opt [delete? Boolean]) IdsOrVoid)
(def-com bounding-box ([ids Ids]) Points)
(def-com brep-closest-point ([shape Id] [p Point] #:opt [faces? Boolean])
  (U Void
     (Vector (Vector Double Double Double)
             (Vector Double Double)
             (Vector Integer Integer)
             (Vector Double Double Double))))

(def-com cap-planar-holes ([shape Id]) Boolean)

(define (capped-planar-holes [id : Id]) : Id
  (if (or (is-object-solid id)
          (cap-planar-holes id))
      id
      (error 'capped-planar-holes "couldn't cap planar holes of shape ~A" id)))

(def-com circle-center-point ([id Id]) Point)
(def-com circle-radius ([id Id]) Real)
(def-com clear-command-history () Void)
(def-com close-curve ([id Id] #:opt [tolerance Double]) Id)
(def-com command ([str String] #:opt [echo? Boolean]) Boolean)
(def-com copy-object ([id Id] #:opt [start Point] [end Point]) Id)
(def-com copy-objects ([ids Ids] #:opt [start Point] [end Point]) IdsOrVoid)
#|
(define bitmap-creation-flags integer)
(def create-preview-image 
  ((file string) #:opt (view string) (size list->vector) bitmap-creation-flags (wireframe? Boolean)) 
  Boolean)

(def create-solid (arr-ids #:opt delete?) singleton-id)
(def current-layer (#:opt name) name)
|#
(def-com current-view (#:opt [name String]) String)
#|
(def curve-Boolean-difference (id id) ids)
(def curve-Boolean-intersection (id id) ids)
(def curve-Boolean-union (id id) ids)
(def curve-curve-intersection (id #:opt id real) list<-intersection-array)
(def curve-closest-point (id point #:opt integer) real)
(def curve-domain (id) vector->list)
|#
(def-com curve-end-point ([curve Id]) Point)
#|
(def curve-evaluate (id real #:opt integer) coords<-vector)
(def curve-frame (id real) matrix<-nested-plane)
|#
(def-com curve-parameter ([curve Id] [t Double]) Double)
(def-com curve-perp-frame ([curve Id] [t Double]) OutPlane)
(def-com curve-normal ([curve Id]) Direction)
(def-com curve-points ([curve Id]) Points)
#|(def curve-seam (id real) Boolean)
|#
(def-com curve-start-point ([curve Id]) Point)
#|
(def curve-tangent (id real #:opt integer) coord<-vector)
(def delete-layer (name) Boolean)
|#

(def-com delete-object ([id Id]) Boolean)
(def-com delete-objects ([ids Ids]) (U Integer Void))

#|
(provide delete-objects)
(define (delete-objects ids)
  (if (null? ids)
      0
      (delete-existing-objects ids)))
|#

(def-com duplicate-edge-curves ([shape Id] #:opt [select? Boolean]) Ids)
(def-com duplicate-surface-border ([shape Id] #:opt [type Integer]) IdsOrVoid)

#|
(def ellipse-center-point (id) coord<-vector)
|#
(def-com enable-redraw (#:opt [redraw? Boolean]) Boolean)
(def-com evaluate-curve ([curve Id] [param Real] #:opt [index Integer]) Point)
(def-com evaluate-surface ([surface Id] [parameters (Vector Real Real)]) Point)
;(def-com extract-iso-curve (id arr-reals integer) ids)
(def-com extrude-curve ([curve Id] [path Id]) Id)
(def-com extrude-curve-normal ([surface Id] [curve Id] [distance Double]) Id)
(def-com extrude-curve-point ([curve Id] [p Point]) Id)
(def-com extrude-curve-straight ([curve Id] [start Point] [end Point]) Id)
(def-com (extrude-curve-direction ExtrudeCurveStraight) ([curve Id] [v Point]) Id)
(def-com extrude-curve-tapered ([curve Id] [distance Double] [direction Point] [base Point] [angle Double] #:opt [corner-type Integer]) Id)
(def-com extrude-surface ([surface Id] (curve Id) #:opt [cap Boolean]) Id)
(def-com get-integer (#:opt [message String] [default Integer] [min Integer] [max Integer]) Integer)
(def-com get-object (#:opt [message String] [type Integer] [pre-select? Boolean] [select? Boolean] [objects Ids]) Id)
(def-com get-point (#:opt [message String] [point Point] [radius Double] [plane? Boolean]) Point)
(def-com get-real (#:opt [message String] [default Double] [min Double] [max Double]) Double)
(def-com intersect-breps ([id0 Id] [id1 Id] #:opt [tolerance Double]) IdsOrVoid)

(def-com (primitive-intersects? intersectBreps)
  ([id0 Id] [id1 Id] #:opt [tolerance Double] [returnBoolean? Boolean]) BoolOrVoid)

(provide intersects?)
(define (intersects? [id0 : Id] [id1 : Id]) : Boolean
  (primitive-intersects? id0 id1 1e-20 #t))


(def-com is-circle ([shape Id]) BoolOrVoid)
(def-com is-curve ([shape Id] #:opt [index Integer]) BoolOrVoid)
(def-com is-curve-closable ([shape Id] #:opt [tolerance Number]) BoolOrVoid)
(def-com is-curve-closed ([shape Id] #:opt [index Integer]) BoolOrVoid)
(def-com is-ellipse ([shape Id]) BoolOrVoid)
(def-com is-layer ([shape Id]) BoolOrVoid)
(def-com is-line ([shape Id]) BoolOrVoid)
(def-com is-mesh ([shape Id]) BoolOrVoid)
(def-com is-object ([shape Id]) BoolOrVoid)
(def-com is-object-in-box ([shape Id] [box Points] [strict? Boolean]) BoolOrVoid)
(def-com is-object-solid ([shape Id]) BoolOrVoid)
(def-com is-point-in-surface ([shape Id] [point Point] #:opt [strict-in? Boolean] [tolerance Double]) BoolOrVoid)
(def-com is-point-on-surface ([shape Id] [point Point] #:opt [tolerance Double]) BoolOrVoid)
(def-com is-polycurve ([shape Id]) BoolOrVoid)
(def-com is-polyline ([shape Id]) BoolOrVoid)
(def-com is-polysurface ([shape Id]) BoolOrVoid)
(def-com is-polysurface-closed ([shape Id]) BoolOrVoid)
(def-com is-polysurface-planar ([shape Id]) BoolOrVoid)
(def-com is-point ([shape Id]) BoolOrVoid)
(def-com is-surface ([shape Id]) BoolOrVoid)
(def-com is-view-maximized ([name String]) BoolOrVoid)
#|
; edit: create an enum for flags
(def last-created-objects (#:opt Boolean integer) ids)
|#
(def-com join-curves ([shapes Ids] #:opt [delete? Boolean] [tolerance Real]) Ids)
(def-com join-surfaces ([shapes Ids] #:opt [delete? Boolean]) Id)
;(def material-name (integer #:opt string) string)
(def-com maximize-restore-view ([name String]) Void)
#|
;;HACK Test this
(def mesh-faces (id Boolean) coords<-vector)
(def mesh-face-normals (id) coords<-vector)
(def mesh-offset (id real) id)
|#
(def-com mirror-object ([shape Id] [start Point] [end Point] #:opt [copy? Boolean]) Id)
(def-com mirror-objects ([shapes Ids] [start Point] [end Point] #:opt [copy? Boolean]) Ids)
;(def-com MoveObject ([shape Id] [start Point] #:opt [end Point]) Id)
;(def-com MoveObjects ([shapes Ids] [start Point] #:opt [end Point]) Ids)
(def-com move-object ([shape Id] [translation Point]) Id)
(def-com move-objects ([shapes Ids] [translation Point]) Ids)
#|
(def object-layer (id #:opt name) name)
(def offset-curve (id point real #:opt point integer) ids)
(def offset-surface (id real) id)
(def plane-from-frame ((o point) (x point) (y point)) rh-plane)
(def plane-from-normal (point normal) matrix<-nested-plane #;rh-plane)
(def plane-from-points ((o point) (x point) (y point)) rh-plane)
|#
(def-com point-coordinates ([point Id] #:opt [new-point Point]) Point)
;(def point-in-planar-closed-curve (point id #:opt plane real) integer)
(provide point-in-surface)
(define (point-in-surface [id : Id]) : Loc
  (let ((v (brep-closest-point id (u0))))
    (if (vector? v)
        (vector-double-flonum->loc (vector-ref v 0))
        ;;Failed, let's use a different method.
        (let ((u (surface-domain id 0))
              (v (surface-domain id 1)))
          (evaluate-surface id (vector (vector-ref u 0) (vector-ref v 0)))))))
#|
(def purge-layer (name) name)
(def rename-layer ((old-name name) (new-name name)) name)
|#
(def-com render-antialias ([style Integer]) Integer)
(def-com render-mesh-quality ([quality Integer]) Integer)
(def-com render-resolution (#:opt [size (Vector Integer Integer)]) (Vector Integer Integer))
#|
(provide revolve)
(define (revolve id p0 p1 a0 a1)
  (cond ((IsCurve id)
         ;;HACK: there's a problem in Rhino when the curve
         ;;touches the revolution axis
         ;;Apparently, the revolve command doesn't have
         ;;this problem.
         (add-rev-srf id
                      (list p0 p1)
                      (radians->degrees a0)
                      (radians->degrees a1)))
        ((or (is-surface id) (IsPolysurface id))
         (let ((border (singleton-id (duplicate-surface-border id))))
           (begin0
               (capped-planar-holes
                (add-rev-srf border
                             (list p0 p1)
                             (radians->degrees a0)
                             (radians->degrees a1)))
             (delete-object border))))
        (else
         (error 'revolve "Can't revolve the shape ~A" id))))

;; (require racket/trace)
;; (trace revolve add-rev-srf)
(def-com rotate-plane ([plane Plane] [angle Real] [axis Point]) Plane)
|#
(def-com rotate-object ([object Id] [p Point] [angle Double] #:opt [axis Point] [copy? Boolean]) Id)
(def-com rotate-objects ([objects Ids] [p Point] [angle Double] #:opt [axis Point] [copy? Boolean]) Ids)
(def-com scale-object ([object Id] [p Point] [scale Point] #:opt [copy? Boolean]) Id)
(def-com scale-objects ([objects Ids] [p Point] [scale Point] #:opt [copy? Boolean]) Ids)
(def-com select-object ([object Id]) Boolean)
(def-com (select-existing-objects selectObjects) ([objects Ids]) Integer)
(provide select-objects)
(define (select-objects [objects : Refs]) : Integer
  (if (null? objects)
      0
      (select-existing-objects objects)))
(def-com selected-objects (#:opt [include-lights? Boolean] [include-grips? Boolean]) IdsOrVoid)
(def-com split-brep ([brep Id] [cutter Id] #:opt [delete? Boolean]) IdsOrVoid)
#|
(provide split-brep-sloppy-n)
(define (split-brep-sloppy-n brep cutter n)
  (if (= n 0)
      #f
      (let ((res (split-brep brep cutter)))
        (or res
            (let ((tol (unit-absolute-tolerance)))
              (printf "Split failed for tolerance ~a" tol)
              (unit-absolute-tolerance (* tol 10))
              (let ((res (split-brep-sloppy-n brep cutter (- n 1))))
                (unit-absolute-tolerance tol)
                res))))))
|#
(def-com surface-area-centroid ([id Id]) Points)
(def-com surface-closest-point ([id Id] [p Point]) Point)

#|
(define (surface-curvature object uv)
  (let ((curvature
         (rhino-check-invoke
          "SurfaceCurvature"
          object
          (vector (real (first uv)) (real (second uv))))))
    (list
     (coord<-vector (vector-ref curvature 0))
     (coord<-vector (vector-ref curvature 1))
     (vector-ref curvature 2)
     (coord<-vector (vector-ref curvature 3))
     (vector-ref curvature 4)
     (coord<-vector (vector-ref curvature 5))
     (vector-ref curvature 6)
     (vector-ref curvature 7))))
|#

(def-com surface-domain ([id Id] [direction Integer]) (Vector Double Double))
(def-com surface-frame ([id Id] [parameters (Vector Real Real)]) OutPlane)
(def-com surface-normal ([id Id] [parameters (Vector Real Real)]) Direction)

; edit: what a very complicated function... is it really necessary?
;; (define (surface-surface-intersection surface-a surface-b (tolerance com-omit) (create? com-omit))
;;   (rhino-check-invoke "SurfaceSurfaceIntersection" surface-a surface-b tolerance create?))

(def-com surface-volume ([id Id]) (Vector Double Double))
(def-com surface-volume-centroid ([id Id]) Points)

(def-com add-sweep1 ([rail Id] [shapes Ids]#| #:opt [start Point] [end Point] [closed? Boolean] [style Integer] [up Point] [simplify Integer] [arg Number] |#) Ids)
(def-com add-sweep2 ([rails Ids] [shapes Ids] #| #:opt [start Point] [end Point] [closed? Boolean] [simple? Boolean] [maintain-height? Boolean] [simplify Integer] [arg Number] |#) Ids)

(def-com transform-object ([shape Id] [matrix Rh-Matrix] #:opt [copy? Boolean]) Id)
(def-com transform-objects ([shapes Ids] [matrix Rh-Matrix] #:opt [copy? Boolean]) Ids)
;(def (transform-object-special "TransformObject") (id rh-matrix #:opt Boolean) id)
#|
(def unit-absolute-tolerance (#:opt tolerance Boolean) number)
(def unselect-all-objects () integer)
(def unselect-object (id) Boolean)
(def unselect-objects (arr-ids) integer)
(def unselected-objects (#:opt (include-lights? Boolean) (include-grips? Boolean)) maybe-ids)

#;(define (unselected-objects . args)
  (with-handlers ((com-exn? (λ (e) (list))))
    (vector->list
     (match args
       ((list) (rhino-check-invoke "UnselectedObjects"))
       ((list include-lights?) (rhino-check-invoke "UnselectedObjects" include-lights?))
       ((list include-lights? include-grips?) (rhino-check-invoke "UnselectedObjects" include-lights? include-grips?))))))

(def vector-create (point point) coord<-vector)
(def vector-unitize (point) coord<-vector)
(def view-c-plane (#:opt string plane) matrix<-nested-plane)
|#
(def-com view-camera (#:opt (view String) (camera Point)) Point)
(def-com view-camera-lens (#:opt (view String) (length Real)) Real)
(def-com view-camera-target (#:opt (view String) (camera Point) (target Point)) Points)
;;AML: I guess there's a bug in the return type of the first two cases 
#;
(define view-camera-target
  (case-lambda
    (() (rhino-check-invoke "ViewCameraTarget"))
    ((view) (rhino-check-invoke "ViewCameraTarget" view))
    ((view camera)
     (list<coord><-vector<vector<real>>
      (rhino-check-invoke
       "ViewCameraTarget"
       view
       (point camera))))
    ((view camera target)
     (list<coord><-vector<vector<real>>
      (rhino-check-invoke
       "ViewCameraTarget"
       view
       (point camera)
       (point target))))))

(def-com view-display-mode (#:opt [view String] [mode Integer]) Integer)
(def-com view-projection (#:opt [view String] [mode Integer]) Integer)
#|
(def view-radius (#:opt (view string) (radius radius)) number)
(def view-size (#:opt string) vector->list)
|#
(def-com view-target (#:opt (view String) (target Point)) Point)
#|
(def xform-change-basis (plane plane) identity #;matrix<-nested-plane)
(def (xform-change-basis2 "XformChangeBasis") (identity identity identity identity identity identity) identity)
(def window-handle () identity)
(def world-x-y-plane () #;matrix<-nested-plane rh-plane)
(def world-y-z-plane () matrix<-nested-plane #;rh-plane)
(def world-z-x-plane () matrix<-nested-plane #;rh-plane)

|#
;;HACK: We need to provide com-omit to the view parameter
(def-com zoom-extents (#:opt (view String) (all? Boolean)) Void)
#|
(def zoom-selected (#:opt string Boolean) void)

; commands

(provide thicken)
(define (thicken object distance)
  (unselect-all-objects)
  (select-object object)
  (command (format "OffsetSrf Solid=Yes ~A _Enter" distance))
  (begin0
      (singleton-id (last-created-objects))
    (unselect-all-objects)))


(provide create-solid-command)
(define (create-solid-command ids)
  (unselect-all-objects)
  (select-objects ids)
  (command (format "CreateSolid"))
  (begin0
      (singleton-id (last-created-objects))
    (unselect-all-objects)))

;;HACK brep-closest-point should probably use a single value memoization
;(provide brep-closest-point)
;(define (brep-closest-point id p)
;  (coord<-vector (vector-ref (brep-closest-point id p) 0)))

(provide brep-closest-uv)
(define (brep-closest-uv id p)
  (vector->list (vector-ref (brep-closest-point id p) 1)))

(provide brep-closest-normal)
(define (brep-closest-normal id p)
  (coord<-vector (vector-ref (brep-closest-point id p) 3)))


(provide point-in-surface)
(define (point-in-surface id)
  (coord<-vector (vector-ref (brep-closest-point id (u0)) 0)))
  ;; (let ((u (surface-domain id 0))
  ;;       (v (surface-domain id 1)))
  ;;   (evaluate-surface id (list (car u) (car v)))))


(provide render-view)
(define (render-view path)
  (command "_-Render")
  ;HACK avoid while we wait for a non-evaluation version 
  (command (format "_-SaveRenderWindowAs ~A" path))
  (command "_-CloseRenderWindow"))


#|
Call Rhino.Command("-_SelNone",False)
 
    Call Rhino.SelectObjects(curveSets(0))
    Call Rhino.Command("-_Loft",False)
    Call Rhino.EnableRedraw(False)
    For i = 1 To uBound(curveSets) Step 1
        Call Rhino.SelectObjects(curveSets(i))
        Call Rhino.Command("-_Loft _enter _enter",False)
        Call Rhino.Command("-_SelNone",False)
    Next
    Call Rhino.EnableRedraw(True)

|#
|#

(define #:forall (T) (trace [msg : String] [v : T]) : T
  (display msg)
  (displayln v)
  v)

#;#;
(provide irregular-frustum)
(define (irregular-frustum [n : Integer] [pts0 : Locs] [pts1 : Locs]) : Id
  (add-mesh
   (append pts0 pts1)
   (for/list : (Listof (Vector Integer Integer Integer Integer))
     ((i : Integer (in-range 0 n)))
     (let* ((i0 i)
            (i1 (modulo (+ i0 1) n))
            (j1 (+ i1 n))
            (j0 (+ i0 n)))
       (vector i0 i1 j1 j0)))))

(define-syntax-rule
  (postpone-redraw expr ...)
  (begin
    (enable-redraw #f)
    (begin0
      (begin expr ...)
      (enable-redraw #t))))

(provide irregular-pyramid-frustum)
(define (irregular-pyramid-frustum [pts0 : Locs] [pts1 : Locs]) : Id
  (postpone-redraw
   (let ((id
          (join-surfaces
           (let ([pts0 (map loc-in-world pts0)]
                 [pts1 (map loc-in-world pts1)])
             (for/list : (Listof Id)
               ([pt00 : Loc (in-list pts0)]
                [pt01 : Loc (in-list (append (cdr pts0) (list (car pts0))))]
                [pt10 : Loc (in-list pts1)]
                [pt11 : Loc (in-list (append (cdr pts1) (list (car pts1))))])
               (add-srf-pt (list pt00 pt01 pt11 pt10))))
           #t)))
     (cap-planar-holes id)
     id)))

(provide irregular-pyramid)
(define (irregular-pyramid [pts0 : Locs] [pt1 : Loc]) : Id
  (postpone-redraw
   (let ((id
          (join-surfaces
           (let ([pts0 (map loc-in-world pts0)]
                 [pt1 (loc-in-world pt1)])
             (for/list : (Listof Id)
               ([pt00 : Loc (in-list pts0)]
                [pt01 : Loc (in-list (append (cdr pts0) (list (car pts0))))])
               (add-srf-pt (list pt00 pt01 pt1))))
           #t)))
     (cap-planar-holes id)
     id)))
