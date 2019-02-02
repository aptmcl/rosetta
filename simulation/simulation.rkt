#lang typed/racket/base/no-check
(require (for-syntax racket/base))
(require "../base/shapes.rkt")
;#;#;#;
(require (prefix-in % "../rhinoceros/rh-com.rkt"))
(require "../rhinoceros/backend.rkt")
(provide (all-from-out "../rhinoceros/backend.rkt"))
#;#;#;#;
(require "../autocad/backend.rkt")
(provide (all-from-out "../autocad/backend.rkt"))
(define (%object-name . args) (error "Unimplemented!"))
(define (%command . args) (error "Unimplemented!"))

(require racket/include)
(include "../base/macros.rkc")
(require "../base/bim-operations.rkt")
(provide (all-from-out "../base/bim-operations.rkt"))
(require racket/file)
(require racket/runtime-path)
(require racket/system)
(require racket/date)
(require net/url)
(require (only-in math/base sum))

(provide shape-material)

(require "materials.rkt")
(provide (all-from-out "materials.rkt"))

(define (simulation-path)
  (or (document-path)
      (make-temporary-file)))

(define analysis-nodes-height : (Parameterof Real) (make-parameter 0.5))
(define analysis-nodes-separation-u : (Parameterof (Option Real)) (make-parameter #f))
(define analysis-nodes-separation-v : (Parameterof (Option Real)) (make-parameter #f))
(define analysis-nodes-separation : (Parameterof Real) (make-parameter 4.0))

(define (analysis-nodes-u-separation)
  (or (analysis-nodes-separation-u)
      (analysis-nodes-separation)))

(define (analysis-nodes-v-separation)
  (or (analysis-nodes-separation-v)
      (analysis-nodes-separation)))

;;Export materials file

;;Default materials used for specific building elements
(define material/default (make-parameter material-white))
(define material/ground (make-parameter generic-floor-20))
(define material/beam (make-parameter generic-interior-wall-50))
(define material/wall (make-parameter generic-interior-wall-50))
(define material/panel (make-parameter generic-interior-wall-50))
(define material/slab-floor (make-parameter generic-floor-20))
(define material/slab-ceiling (make-parameter generic-ceiling-70))
(define material/roof-floor (make-parameter generic-floor-20))
(define material/roof-ceiling (make-parameter generic-ceiling-70))
(define material/column (make-parameter generic-interior-wall-50))
(define material/window (make-parameter generic-glass-80))

(define (all-materials)
  (remove-duplicates
   #;
   (list (material/default)
         (material/ground)
         (material/beam)
         (material/wall)
         (material/panel)
         (material/slab-floor)
         (material/slab-ceiling)
         (material/roof-floor)
         (material/roof-ceiling)
         (material/column)
         (material/window))
   (map shape-material (append (radiance-shapes) (radiance-surfaces)))))

(define (all-materials-names)
  (remove-duplicates
   (map material-name
        (all-materials))))      

(provide simulation-path
         current-location
         analysis-nodes-height
         analysis-nodes-separation
         analysis-nodes-separation-u
         analysis-nodes-separation-v
         sensors
         material/default
         material/ground
         material/beam
         material/wall
         material/panel
         material/slab-floor
         material/slab-ceiling
         material/roof-floor
         material/roof-ceiling
         material/column
         material/window
         slab-floor-layer
         slab-ceiling-layer
         roof-floor-layer
         roof-ceiling-layer
         window-layer
         ground-layer
         sensor-layer
         create-plastic-material
         read-sensors
         show-colors-of-sensors
         remap-surfaces-and-grids-colors)

#|

It seems that DIVA saves all files except the <file>.pts and the <file>uv.dat

The nodes file <file>.pts contains, for each node, a line
x y z nx ny nz

The uv file <file>uv.dat contains, for each surface, a record

#nodegroup00                
Z-axis unit vector        > 0 0 1                        
U-axis unit vector         > 0 1 0                        
V-axis unit vector         > 1 0 0                                
U-axis spacing between nodes         > 3.33333333        
V-axis spacing between nodes         > 3.33333333        
U-divisions - 1 (not used in loading data)        > 1                        
V-divisions - 1 (not used in loading data)        > 1                        
number of points - 1 (array bound)         > 3

|#

(define radiance-polygons (make-parameter (list)))
(provide radiance-polygons add-radiance-polygon!)
(define (add-radiance-polygon! ps)
  (radiance-polygons (append (radiance-polygons) (list ps)))
  ps)


(define radiance-surfaces (make-parameter (list)))
(define analyze-surfaces (make-parameter #t))
(provide radiance-surfaces add-radiance-surface! analyze-surfaces)
(define (add-radiance-surface! s)
  (when (analyze-surfaces)
    (radiance-surfaces (cons s (radiance-surfaces))))
  s)

(define radiance-grids (make-parameter (list)))
(provide radiance-grids add-radiance-grid!)
(define (add-radiance-grid! s)
  (radiance-grids (cons s (radiance-grids)))
  s)

(define radiance-shapes (make-parameter (list)))
(provide radiance-shapes add-radiance-shape!)
(define (add-radiance-shape! s)
  (radiance-shapes (cons s (radiance-shapes)))
  s)

(provide with-simulation)
(define-syntax-rule
  (with-simulation expr ...)
  (parameterize
      ([radiance-polygons (list)]
       [radiance-surfaces (list)]
       [radiance-shapes (list)]
       [radiance-grids (list)])
    expr ...))

(define (call-with-daysim-simulation path f)
  (parameterize ([radiance-surfaces (list)])
    (f)
    (export-to-daysim (or path (make-temporary-file)))
    #;(let-values ([(dir name dir?) (split-path path)])
        (let ((pure-name (path-replace-suffix name ""))
              (pts-name (path-replace-suffix name ".pts"))
              (obj-name (path-replace-suffix name ".obj"))
              (temp (string->path "C:\\DIVA\\Temp")))
          (let ((file-path
                 (build-path temp pure-name pts-name))
                (uvfile-path
                 (build-path temp pure-name (string->path (format "~Auv.dat" (path->string pure-name)))))
                (obj-path
                 (build-path temp pure-name obj-name)))
            (call-with-output-file file-path #:mode 'text #:exists 'replace
              (lambda (file)
                (call-with-output-file uvfile-path #:mode 'text #:exists 'replace
                  (lambda (uvfile)
                    (parameterize ((file.pts file)
                                   (fileuv.dat uvfile)
                                   (nodegroup 0)
                                   (analyzed-surfaces (list)))
                      (begin0
                        (f)
                        #;(export-for-radiance path)
                        #;(obj2rad obj-path)
                        (export-to-daysim))))))))))))
  
(provide daysim-simulation)
(define-syntax-rule
  (daysim-simulation expr ...)
  (call-with-daysim-simulation
   (simulation-path)
   (lambda () expr ...)))



(require racket/format)

#|

(define (write-diva-nodegroup p du dv nu nv)
  (define (write-vector v)
    (fprintf (fileuv.dat) "~A ~A ~A~%"
             (real->decimal-string (cx v) 4)
             (real->decimal-string (cy v) 4)
             (real->decimal-string (cz v) 4)))
  (let ((x (vec-in-world (vx 1 p)))
        (y (vec-in-world (vy 1 p)))
        (z (vec-in-world (vz 1 p))))
    (fprintf (fileuv.dat) "# nodegroup~A~%"
             (~r (nodegroup) #:min-width 2 #:pad-string "0"))
    (nodegroup (+ 1 (nodegroup)))
    (write-vector z)
    (write-vector x)
    (write-vector y)
    (fprintf (fileuv.dat) "~A~%~A~%~A~%"
             du dv (- (* nu nv) 1))))

(define (create-analysis-nodes [surface : 2D-shape]
                               [height : Real (analysis-nodes-height)]
                               [nu : Integer (analysis-nodes-nu)]
                               [nv : Integer (analysis-nodes-nv)])
  (when (file.pts)
    (let ((nodes
           (map-surface-division (lambda (pt)
                                   (+z pt height))
                                 surface
                                 nu #t
                                 nv #t)))
      (write-diva-nodegroup
       (caar nodes)
       (distance (caar nodes) (caadr nodes))
       (distance (caar nodes) (cadar nodes))
       (- nu 1) (- nv 1)) ;;We remove the extreme nodes to avoid hitting walls
      (with-current-layer
          diva-nodes-layer
        (map (lambda (row)
               (map (lambda (p)
                      (write-diva-node p)
                      (point p))
                    (drop-right (cdr row) 1)))
             (drop-right (cdr nodes) 1))))))

;;The second iteration is to recover the results:
(define (read-diva-result) #f)

(define (show-surface-analysis [surface : 2D-shape]
                               [nu : Integer (analysis-nodes-nu)]
                               [nv : Integer (analysis-nodes-nv)])
  (let ((nodes (map-surface-division identity surface nu #t nv #t)))
    (iterate-quads (lambda (p0 p1 p2 p3)
                     (let ((results (read-diva-result)))
                       (shape-color (surface-polygon p0 p1 p2 p3) results))))))

(define (show-surfaces-analysis [surfaces (analyzed-surfaces)])
  (for ([(surface height nu nv) (in-list surfaces)])
    (show-surface-analysis surface nu nv)))

#;#;#;
(define (draw-ref p)
    (line p (+x p 1))
    (line p (+y p 2))
    (line p (+z p 3)))

(delete-all-shapes)
(let ((p0 (loc-from-o-vx-vy (xyz 1 2 3) (vpol 1 0.2) (vpol 1 (+ 0.2 pi/2))))
      (p1 (loc-from-o-vx-vy (xyz 2 2 3) (vsph 1 0.6 (+ pi/2 0.3)) (vpol 1 (+ 0.6 pi/2)))))
  (draw-ref p0)
  (draw-ref p1)
  (draw-ref (average-pt p0 p1)))
|#  

(define (iterate-quads f ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss)))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0)))
      (f p0 p1 p2 p3))))

(define-values/invoke-unit/infer bim-levels@)

(define slab-floor-layer (make-parameter (create-layer "SlabFloor")))
(define slab-ceiling-layer (make-parameter (create-layer "SlabCeiling")))

(define roof-floor-layer (make-parameter (create-layer "RoofFloor")))
(define roof-ceiling-layer (make-parameter (create-layer "RoofCeiling")))

(define window-layer (make-parameter (create-layer "Window")))

(define ground-layer (make-parameter (create-layer "Ground")))

(define sensor-layer (make-parameter (create-layer "Sensors")))

(define (create-surface-layer [e : Any] [height : Real] [layer : Layer] [material : Material] [add? : Boolean #t])
  (let ((vertices
         (if (list? e)
             e
             (if (circle? e)
                 (regular-polygon-vertices
                  32
                  (+z (circle-center e) (- (cz (circle-center e))))
                  (circle-radius e))
                 (error "Unrecognized shape")))))
    (let ((s (surface-polygon
              (map (lambda ([p : Loc])
                     (+z p (+ height)))
                   vertices))))
      (when add?
        (shape-layer s layer)
        (shape-material s material)
        (add-radiance-shape! s))
      s)))

(define-unit bim-surface-ops@
  (import bim-ops-dependencies^ bim-levels^)
  (export bim-ops^)

  ;This should not be done like this. Lexical scope does not seem like the correct solution here.
  (define truss-node #f)
  (define truss-bar #f)

  
  (define create-bim-layers? (make-parameter #t))

  (define bim-shape-layer
    (case-lambda
      ((shape layer)
       (when (create-bim-layers?)
         (shape-layer shape layer))
       shape)
      ((shape)
       (shape-layer shape))))
  
  (def-shape/no-provide (polygonal-mass [pts : Locs] [height : Real])
    (irregular-prism pts height))

  (define (bim-family-layer [bim-family : BIM-Family])
    (or (unbox (bim-family-layer-ref bim-family))
        (let ((layer (create-layer (bim-family-layer-name bim-family))))
          (set-box! (bim-family-layer-ref bim-family) layer)
          layer)))
  
  (def-shape/no-provide (beam [p0 : Loc] [p1 : Loc] [angle : Real 0] [family : Beam-Family (default-beam-family)])
    (let ((h (beam-family-height family)))
      (let ((s (right-cuboid (+z p0 (/ h -2))
                             (beam-family-width family) h
                             (+z p1 (/ h -2)))))
        (bim-shape-layer s (bim-family-layer family))
        (shape-material s (material/beam))
        (add-radiance-shape! s)
        s)))
  
  (def-shape/no-provide (column [center : Loc]
                                [bottom-level : Level (current-level)]
                                [top-level : Level (upper-level bottom-level)]
                                [family : Column-Family (default-column-family)])
    (let ((width (column-family-width family))
          (circular-section? (column-family-circular-section? family))
          (depth (or (column-family-depth family) (column-family-width family))))
      (let ((s (if circular-section?
                   (cylinder
                    (+z center (level-height bottom-level))
                    width
                    (- (level-height top-level) (level-height bottom-level)))
                   (box (+xyz center (/ width -2) (/ depth -2) (level-height bottom-level))
                        width
                        depth
                        (- (level-height top-level) (level-height bottom-level))))))
        (bim-shape-layer s (bim-family-layer family))
        (shape-material s (material/column))
        (add-radiance-shape! s)
        s)))
  
  (def-shape/no-provide (slab [vertices : Locs] [level : Level (current-level)] [family : Slab-Family (default-slab-family)])
    (if (and (list? vertices) (loc? (car vertices)))
        (let ((surfaces
               (for/list ((height (in-list (list (- (slab-family-thickness family)) 0)))
                          (layer (in-list (list (slab-ceiling-layer) (slab-floor-layer))))
                          (material (in-list (list (material/slab-ceiling) (material/slab-floor)))))
                 (create-surface-layer vertices (+ (level-height level) height) layer material))))
          (add-radiance-surface! (second surfaces))
          surfaces)
        (let ((path (if (list? vertices) vertices (list vertices))))
          (let loop ((p path))
            (if (null? p)
                (list)
                (let ((e (car p)))
                  (unless (null? (cdr p)) (error "Unfinished"))
                  (cond ((line? e)
                         (error "Unfinished"))
                        ((arc? e)
                         (error "Unfinished"))
                        ((circle? e)
                         (let ((surfaces
                                (for/list ((height (in-list (list (- (slab-family-thickness family)) 0)))
                                           (layer (in-list (list (slab-ceiling-layer) (slab-floor-layer))))
                                           (material (in-list (list (material/slab-ceiling) (material/slab-floor)))))
                                  (create-surface-layer
                                   (regular-polygon-vertices 32
                                                             (+z (circle-center e) (- (cz (circle-center e))))
                                                             (circle-radius e))
                                   (+ (level-height level) height)
                                   layer
                                   material))))
                           (add-radiance-surface! (second surfaces))
                           surfaces))
                        (else
                         (error "Unknown path component" e)))))))))

  
  (def-shape/no-provide (slab-with-openings [vertices : Any] [paths : Any] [level : Level (current-level)] [family : Slab-Family (default-slab-family)])
    (define (create-surface-layer-with-openings vertices paths height layer material)
      (let ((s (create-surface-layer vertices height layer material)))
        (for ((path (in-list paths)))
          (set! s (subtraction s (create-surface-layer path height layer material #f))))
        s))
    (if (and (list? vertices) (loc? (car vertices)))
        (let ((surfaces
               (for/list ((height (in-list (list (- (slab-family-thickness family)) 0)))
                          (layer (in-list (list (slab-ceiling-layer) (slab-floor-layer))))
                          (material (in-list (list (material/slab-ceiling) (material/slab-floor)))))
                 (create-surface-layer-with-openings vertices paths (+ (level-height level) height) layer material))))
          (add-radiance-surface! (second surfaces))
          surfaces)
        (let ((path (if (list? vertices) vertices (list vertices))))
          (let loop ((p path))
            (if (null? p)
                (list)
                (let ((e (car p)))
                  (unless (null? (cdr p)) (error "Unfinished"))
                  (cond ((line? e)
                         (error "Unfinished"))
                        ((arc? e)
                         (error "Unfinished"))
                        ((circle? e)
                         (let ((surfaces
                                (for/list ((height (in-list (list (- (slab-family-thickness family)) 0)))
                                           (layer (in-list (list (slab-ceiling-layer) (slab-floor-layer))))
                                           (material (in-list (list (material/slab-ceiling) (material/slab-floor)))))
                                  (create-surface-layer-with-openings
                                   (regular-polygon-vertices 32
                                                             (+z (circle-center e) (- (cz (circle-center e))))
                                                             (circle-radius e))
                                   (+ (level-height level) height)
                                   layer
                                   material))))
                           (add-radiance-surface! (second surfaces))
                           surfaces))
                        (else
                         (error "Unknown path component" e)))))))))

  (def-shape/no-provide (slab-opening [slab : Any] [path : Any])
    (let ((level (slab-level slab))
          (family (slab-family slab))
          (surfaces (shape-reference slab)))
      (radiance-surfaces (remq (second surfaces) (radiance-surfaces)))
      (let loop ((p path))
        (if (null? p)
            (list)
            (let ((e (car p)))
              (unless (null? (cdr p)) (error "Unfinished"))
              (cond ((line? e)
                     (error "Unfinished"))
                    ((arc? e)
                     (error "Unfinished"))
                    ((circle? e)
                     (let ((surfaces
                            (for/list ((surface (in-list surfaces))
                                       (height (in-list (list 0 (slab-family-thickness family))))
                                       (layer (in-list (list (slab-ceiling-layer) (slab-floor-layer))))
                                       (material (in-list (list (material/slab-ceiling) (material/slab-floor)))))
                              (subtraction
                               surface
                               (create-surface-layer
                                (regular-polygon-vertices 32
                                                          (+z (circle-center e) (- (cz (circle-center e))))
                                                          (circle-radius e))
                                (+ (level-height level) height)
                                layer
                                material)))))
                       (add-radiance-surface! (second surfaces))
                       surfaces))
                    (else
                     (error "Unknown path component" e))))))))

  (def-shape/no-provide (roof [vertices : Locs] [level : Level (current-level)] [family : Roof-Family (default-roof-family)])
    (for/list ((height (in-list (list (- (roof-family-thickness family)) 0)))
               (layer (in-list (list (roof-ceiling-layer) (roof-floor-layer))))
               (material (in-list (list (material/roof-ceiling) (material/roof-floor)))))
      (create-surface-layer vertices (+ (level-height level) height) layer material)))
  
  (define (wall [p0 : Loc] [p1 : Loc]
                [bottom-level : Level (current-level)]
                [top-level : Level (upper-level bottom-level)]
                [family : Wall-Family (default-wall-family)])
    (let ((height (- (level-height top-level) (level-height bottom-level))))
      (let ((h/2 (/ height 2)))
        (let ((ref (shape-reference
                    (right-cuboid (+z p0 (+ (level-height bottom-level) h/2))
                                  (wall-family-thickness family)
                                  height
                                  (+z p1 (+ (level-height bottom-level) h/2))))))
          (let ((s (new-wall (lambda () ref)
                             p0 p1
                             bottom-level
                             top-level
                             family)))
            (bim-shape-layer s (bim-family-layer family))
            (shape-material s (material/wall))
            (add-radiance-shape! s)
            s)))))

  (def-shape/no-provide (walls [vertices : Locs]
                               [bottom-level : Level (current-level)]
                               [top-level : Level (upper-level bottom-level)]
                               [family : Wall-Family (default-wall-family)])
    (let ((s 
           (thicken (extrusion (line (map (lambda ([p : Loc])
                                            (+z p (level-height bottom-level)))
                                          vertices))
                               (- (level-height top-level) (level-height bottom-level)))
                    (wall-family-thickness family))))
      (bim-shape-layer s (bim-family-layer family))
      (shape-material s (material/wall))
      (add-radiance-shape! s)
      (shape-reference s)))

  
(define (wall*-family w)
  (if (wall? w)
      (wall-family w)
      (walls-family w)))

(define (wall*-bottom-level w)
  (if (wall? w)
      (wall-bottom-level w)
      (walls-bottom-level w)))

(define (wall*-top-level w)
  (if (wall? w)
      (wall-top-level w)
      (walls-top-level w)))

(define (wall*-vertices w)
  (if (wall? w)
      (list (wall-p0 w) (wall-p1 w))
      (walls-vertices w)))

(define (loc-from-p0-p1 p0 p1)
  (let ((v (p-p p1 p0)))
    (loc-from-o-vx-vy p0 v (vpol (pol-rho v) (+ (pol-phi v) pi/2)))))
  
(define (wall-loc w)
  (+z (if (wall? w)
          (loc-from-p0-p1 (wall-p0 w) (wall-p1 w))
          (loc-from-p0-p1 (car (walls-vertices w)) (cadr (walls-vertices w))))
      (level-height (wall*-bottom-level w))))

  
(define (door [wall : Any] [loc : Loc] [flip-x : Boolean #f] [flip-y : Boolean #f] [family : Any (default-door-family)])
  (let ((wall-e (wall-family-thickness (wall*-family wall))))
    (let ((ref
           (shape-reference
            (subtraction
             wall
             (box (+xyz (wall-loc wall) (cx loc) (- wall-e) (cy loc))
                  (door-family-width family)
                  (* 2 wall-e)
                  (door-family-height family))))))
      (new-walls (lambda () ref)
                 (wall*-vertices wall)
                 (wall*-bottom-level wall)
                 (wall*-top-level wall)
                 (wall*-family wall)))))

(define (window [wall : Any] [loc : Loc] [family : Any (default-window-family)])
  (shape-material wall) 
  (let ((wall-e (wall-family-thickness (wall*-family wall))))
    (let ((ref
           (shape-reference
            (subtraction
             wall
             (box (+xyz (wall-loc wall) (cx loc) (- wall-e) (cy loc))
                  (window-family-width family)
                  (* 2 wall-e)
                  (window-family-height family))))))
      (let ((corner (+xyz (wall-loc wall) (cx loc) 0 (cy loc))))
        (let ((vertices (list corner
                              (+x corner (window-family-width family))
                              (+xz corner (window-family-width family) (window-family-height family))
                              (+z corner  (window-family-height family)))))
          (create-surface-layer vertices 0 (window-layer) (material/window))))
      (let ((w
             (shape-material
              (new-walls (lambda () ref)
                         (wall*-vertices wall)
                         (wall*-bottom-level wall)
                         (wall*-top-level wall)
                         (wall*-family wall))
              (shape-material wall))))
        (add-radiance-shape! w)
        w))))
  
  (def-shape/no-provide (panel [vertices : Locs] [level : Any (current-level)] [family : Panel-Family (default-panel-family)])
    (let ((p0 (second vertices))
          (p1 (first vertices))
          (p2 (third vertices)))
      (let ((n (vz (/ (panel-family-thickness family) 2)
                   (cs-from-o-vx-vy p0 (p-p p1 p0) (p-p p2 p0)))))
        (let ((s (irregular-prism
                  (map (lambda (v) (loc-in-world (p-v v n))) vertices)
                  (vec-in-world (v*r n 2)))))
          (bim-shape-layer s (bim-family-layer family))
          (shape-material s (material/panel))
          (shape-reference s))))
    ;Remove when proved unnecessary
    #;(let ((p0 (second vertices))
          (p1 (first vertices))
          (p2 (third vertices)))
      (let ((n (vz (panel-family-thickness family)
                   (cs-from-o-vx-vy p0 (p-p p1 p0) (p-p p2 p0)))))
        (let ((s (cuboid (map loc-in-world
                              (append (map (lambda (v) (p+v v n)) vertices)
                                      (map (lambda (v) (p-v v n)) vertices))))))
          (bim-shape-layer s (bim-family-layer family))
          (shape-material s (material/panel))
          (shape-reference s)))))
  )

(define-values/invoke-unit/infer bim-surface-ops@)
(define-values/invoke-unit/infer bim-extra-ops@)
(provide-signature-elements bim-levels^)
(provide-signature-elements bim-ops^)
(provide-signature-elements bim-extra-ops^)

(define cmd-raypath #<<END
SET RAYPATH=.;C:\DIVA\Radiance\lib;C:\DIVA\Radiance\bin_64;C:\DIVA\Radiance\bin;C:\DIVA\DaysimBinaries;
END
  )

(define cmd-path #<<END
SET PATH=.;C:\DIVA\Radiance\lib;C:\DIVA\Radiance\bin_64;C:\DIVA\Radiance\bin;C:\DIVA\DaysimBinaries;
END
  )

(define (radiance-command str)
  (let ((cmd (string-append cmd-raypath " & " cmd-path " & " str)))
    (displayln str)
    (assert (system cmd))))

(define (surface-nu-nv surface sep-u sep-v)
  (let-values ([(u0 u1 v0 v1) (surface-domain surface)])
    (let ((nu (inexact->exact (round (/ (- u1 u0) sep-u))))
          (nv (inexact->exact (round (/ (- v1 v0) sep-v)))))
      (values nu nv))))

;;Two options: surfaces or grids (lists of lists of locations)

(provide sensors-from-surfaces
         sensors-from-grids
         show-colors-of-sensors
)

(define (sensors-from-polygons [polygons (radiance-polygons)]
                               [height : Real (analysis-nodes-height)])
  (for/list ((polygon (in-list polygons)))
    (let ((p (apply points-center polygon))
          (n (apply points-normal polygon)))
      (+z (loc-from-o-vz p n) height))))


(define (sensors-from-surfaces
         [surfaces (radiance-surfaces)]
         [height : Real (analysis-nodes-height)]
         [sep-u : Real (analysis-nodes-u-separation)]
         [sep-v : Real (analysis-nodes-v-separation)])
  (append*
   (for/list ([surface (in-list surfaces)])
     (let-values ([(nu nv) (surface-nu-nv surface sep-u sep-v)])
       (let ((nodes
              (map-inner-surface-division
               (lambda (pt) (and pt (+z pt height)))
               surface nu nv)))
         (filter
          identity
          (append*
           nodes)))))))

(define (sensors-from-grids
         [ptsss (radiance-grids)]
         [height : Real (analysis-nodes-height)])
  (append*
   (for/list ([ptss (in-list ptsss)])
     (append*
      (iterate-quads (lambda (p0 p1 p2 p3)
                       (and p0 p1 p2 p3
                            (let ((p (points-center p0 p1 p2 p3))
                                  (n (points-normal p0 p0 p2 p3)))
                              ;(line p (p+v p (* n height)))
                              (+z (loc-from-o-vz p n) height))))
                     ptss)))))

(define (points-center p0 p1 p2 [p3 p2])
  (intermediate-point
   (intermediate-point p0 p2)
   (intermediate-point p1 p3)))

(define (polygon-normal vs)
  (unitize
   (cross-products
    (append vs (list (car vs))))))

(define (cross-products vs)
  (if (null? (cdr vs))
    (vxyz 0 0 0)
    (v+v (v*v (car vs) (cadr vs))
         (cross-products (cdr vs)))))

(provide points-normal)
(define (points-normal p0 p1 p2 [p3 p2])
  (polygon-normal (list (p-p p1 p0) (p-p p2 p1) (p-p p3 p2) (p-p p0 p3))))

(provide colored-panels-height)
(define colored-panels-height (make-parameter 0.1))

(define (sensors)
  (append (sensors-from-polygons) (sensors-from-surfaces)(sensors-from-grids)))

(define-syntax-rule
  (pop! var)
  (let ((e (car var)))
    (set! var (cdr var))
    e))

(define (show-colors-of-sensors
         colors
         [polygons (radiance-polygons)]
         [surfaces (radiance-surfaces)]
         [grids (radiance-grids)]
         [height : Real (analysis-nodes-height)]
         [sep-u : Real (analysis-nodes-u-separation)]
         [sep-v : Real (analysis-nodes-v-separation)])
  (for/list ((polygon (in-list polygons)))
    (let ((n (v*r (apply points-normal polygon) (colored-panels-height))))
      (shape-color (surface-polygon (map (lambda (p) (p+v p n)) polygon)) (pop! colors))))
  (for/list ([surface (in-list surfaces)])
    (let-values ([(nu nv) (surface-nu-nv surface sep-u sep-v)])
      (map-inner-surface-division
       (lambda (p)
         (when p
           (shape-color
            (surface-rectangle
             (+xyz p (/ sep-u -2) (/ sep-v -2) height)
             sep-u sep-v)
            (pop! colors))))
       surface nu nv)
      #;
      (let ((nodes (map-surface-division identity surface nu #t nv #t)))
        (iterate-quads (lambda (p0 p1 p2 p3)
                         (when (and p0 p1 p2 p3)
                           (shape-color
                            (surface-polygon
                             (+z p0 height)
                             (+z p1 height)
                             (+z p2 height)
                             (+z p3 height))
                            (pop! colors))))
                       nodes))))
  (for/list ([ptss (in-list grids)])
    (iterate-quads (lambda (p0 p1 p2 p3)
                     (when (and p0 p1 p2 p3)
                       (let ([n (v*r (points-normal p0 p1 p2 p3) (colored-panels-height))])
                         (shape-color
                          (surface-grid (list (list (p+v p0 n) (p+v p1 n)) (list (p+v p3 n) (p+v p2 n))))
                          (pop! colors)))))
                   ptss))
  (assert (null? colors)))

;;To export all sensors

(define (export-sensors [path : Path] [sensors (sensors)])
  (let ((ptspath (path-replace-suffix path ".pts")))
    (call-with-output-file ptspath #:mode 'text #:exists 'replace
      (lambda (port)
        (with-current-layer (sensor-layer)
          (for ([sensor (in-list sensors)])
            (write-sensor-node port sensor)))
        ptspath))))

(define (average-pt p0 p1)
  (let ((wp0 (loc-in-world p0))
        (wp1 (loc-in-world p1))
        (wx0 (vec-in-world (vx 1 p0)))
        (wx1 (vec-in-world (vx 1 p1)))
        (wy0 (vec-in-world (vy 1 p0)))
        (wy1 (vec-in-world (vy 1 p1))))
    (let ((o (p+v wp0 (v*r (p-p wp1 wp0) 1/2)))
          (vx (v*r (v+v wx0 wx1) 1/2))
          (vy (v*r (v+v wy0 wy1) 1/2)))
      (loc-from-o-vx-vy o vx vy))))


(define (quad-center p0 p1 p2 p3)
  (average-pt (average-pt p0 p2)
              (average-pt p1 p3)))

(define (write-sensor-node port p)
  (let ((wp (loc-in-world p))
        (wv (vec-in-world (vz 1 p))))
    ;;Just to see the sensor location
    (point p)
;    (line p (+z p 3))
    (fprintf port
             "~A ~A ~A ~A ~A ~A~%"
             (real->decimal-string (cx wp) 2)
             (real->decimal-string (cy wp) 2)
             (real->decimal-string (cz wp) 2)
             (real->decimal-string (cx wv) 2)
             (real->decimal-string (cy wv) 2)
             (real->decimal-string (cz wv) 2))))

(define (read-sensors
         [datpath : Path]
         [polygons (radiance-polygons)]
         [surfaces (radiance-surfaces)]
         [grids (radiance-grids)]
         [sep-u : Real (analysis-nodes-u-separation)]
         [sep-v : Real (analysis-nodes-v-separation)])
  (call-with-input-file datpath #:mode 'text
    (lambda (port)
      (append
       (read-n-sensors-node-color port (length polygons))
       (append*
        (for/list ([surface (in-list surfaces)])
          (let-values ([(nu nv) (surface-nu-nv surface sep-u sep-v)])
            (filter
             identity
             (append*
              (map-inner-surface-division
               (lambda (pt) (and pt (read-color port)))
               surface nu nv))))))
       (append*
        (for/list ([grid (in-list grids)])
          (let ((nu (- (length grid) 1))
                (nv (- (length (car grid)) 1)))
            (read-n-sensors-node-color port (* nu nv)))))))))

(define (read-n-sensors-node-color port n)
  (for/list ([i (in-range 0 n)])
    (read-color port)))

(provide export-sensors-colors)
(define (export-sensors-colors
         [datpath : Path]
         [sensors (sensors)])
  (let ((colors
         (call-with-input-file datpath #:mode 'text
           (lambda (port)
             (read-n-sensors-node-color port (length sensors))))))
    (let ((csvpath (path-replace-suffix datpath ".csv")))
      (call-with-output-file csvpath #:mode 'text #:exists 'replace
        (lambda (port)
          (fprintf port "X,Y,Z,NX,NY,NZ,R,G,B~%")
          (for ([sensor (in-list sensors)]
                [color (in-list colors)])
            (let ((wp (loc-in-world sensor))
                  (wv (vec-in-world (vz 1 sensor))))
            (fprintf port
                     "~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
                     (real->decimal-string (cx wp) 2)
                     (real->decimal-string (cy wp) 2)
                     (real->decimal-string (cz wp) 2)
                     (real->decimal-string (cx wv) 2)
                     (real->decimal-string (cy wv) 2)
                     (real->decimal-string (cz wv) 2)
                     (rgb-red color)
                     (rgb-green color)
                     (rgb-blue color)))))))))

(define (count-over limit list)
  (count (lambda (r) (> r limit)) list))

(define (count-under limit list)
  (count (lambda (r) (< r limit)) list))
  
(define (radiance-statistics values
                             #:min-value min-value
                             #:max-value max-value)
  (let ((over (count (lambda (v) (> v max-value)) values))
        (under (count (lambda (v) (< v min-value)) values))
        (total (length values)))
    (let* ((over% (/ over total))
           (under% (/ under total))
           (within% (- 1 over% under%))
           (mean (/ (sum values) total)))
      (values over% under% within% mean))))


(define (create-ground-plane shapes [material (material/ground)])
  (if (null? shapes)
      (error "No shapes selected for analysis. Use add-radiance-shape!.")
      (let ((bb (bounding-box (union shapes))))
        (match bb
          ((list p0 p1 p2 p3 p4 _ _ _)
           (let ((center (quad-center p0 p1 p2 p3))
                 (ratio (/ (distance p0 p4) (distance p0 p2))))
             (if (= ratio 0)
                 (error "Couldn't compute height. Use add-radiance-shape!.")
                 (let ((pts (map (lambda (p)
                                   (intermediate-point center p (* ratio 10)))
                                 (list p0 p1 p2 p3))))
                   (create-surface-layer pts 0 (ground-layer) material)))))))))

(provide export-geometry)
(define (export-geometry
         [path : Path (simulation-path)]
         #:materials [materials : (Listof Material) (all-materials)]
         #:write-precision [write-precision : Integer 15]
         #:polygon-density [polygon-density : Integer 100]) ;Probably should use some dependency from the node separation.
  (let ((objpath (path-replace-suffix path ".obj"))
        (radpath (path-replace-suffix path ".rad"))
        (mappath (path-replace-suffix path ".map"))
        (shapes (append (radiance-shapes) (radiance-surfaces))))
    (if (string=? (current-backend-name) "AutoCAD")
        (printf "AutoCAD can't export in obj format (yet)~%")
        (begin
          ;;Inject ground plane
          (let ((shapes (cons (create-ground-plane shapes) shapes)))
            (export-obj objpath shapes write-precision polygon-density))
          ;;Write the map file
          (call-with-output-file #:mode 'text #:exists 'replace
            mappath
            (lambda (port)
              (for ([material (in-list materials)])
                (fprintf port "~A (Object \"~A\");~%" (material-name material) (material-name material)))))
          ;;Now, convert the obj to rad
          (radiance-command
           (format "obj2rad -f -m \"~A\" \"~A\" > \"~A\""
                   (path->string mappath)
                   (path->string objpath)
                   (path->string radpath)))))
    radpath))

(define (export-obj objpath shapes write-precision polygon-density)
  (when (null? shapes) (error "No shapes can be exported. Did you forget to assign materials?"))
  (for ((shape (in-list shapes)))
    (for ((ref (in-list (shape-refs shape))))
      (%object-name ref (material-name (shape-material shape)))))
  ;;Write the obj file
  (select-shapes shapes)
  (%command
   #;(string-append
      "_-Export " (path->string objpath) " x x r=No YUp=No Enter P=" (format "~A" polygon-density) " Enter")
   
   (string-append
    "_-Export " (path->string objpath) " "
    "_Geometry=_Mesh "
    "_EndOfLine=CRLF "
    ;"_ExportRhinoObjectNames=_ExportObjectsAsOBJGroups "
    "_ExportRhinoObjectNames=_ExportObjectsAsOBJObjects "
    "_ExportMeshTextureCoordinates=_Yes "
    "_ExportMeshVertexNormals=_No "
    "_CreateNGons=_No "
    "_ExportMaterialDefinitions=_No "
    "_YUp=_No "
    "_WrapLongLines=Yes "
    ;"_VertexWelding=_Welded "
    "_VertexWelding=_Unmodified "
    (format "_WritePrecision=~A " write-precision)
    "Enter "
    (format "_PolygonDensity=~A " polygon-density)
    "Enter "))
  (select-shapes (list)))

(define (export-materials [path : Path (simulation-path)] [materials (all-materials)])
  (let ((matpath (path-replace-suffix path "_materials.rad")))
    (call-with-output-file matpath #:mode 'text #:exists 'replace
      (lambda (port)
        (for ([mat (in-list materials)])
          (displayln (radiance-string mat) port))))
    matpath))


(define extra-sky.rad-contents #<<END
skyfunc glow sky_mat
0
0
4 1 1 1 0

sky_mat source sky
0
0
4 0 0 1 180

skyfunc glow ground_glow
0
0
4 1 .8 .5 0

ground_glow source ground
0
0
4 0 0 -1 180
END
  )

;This is only for daylight factor.
(define CIE.Overcast.Sky.rad-contents #<<END
!gensky 12 21 12.00 -c -a 42.300 -o 71.100 -m 75.000 -B 100

skyfunc glow sky_mat
0
0
4 1 1 1 0

sky_mat source sky
0
0
4 0 0 1 180

skyfunc glow ground_glow
0
0
4 1 1 1  0

ground_glow source ground
0
0
4 0 0 -1 180
END
  )

#|

1. Go to https://www.energyplus.net/weather
2. Click on the location
3. Copy the link "Download Weather File"

;;E.g., for Anchorage, USA, we get:

https://www.energyplus.net/weather-download/north_and_central_america_wmo_region_4/USA/AK/USA_AK_Anchorage.Intl.AP.702730_TMY3/USA_AK_Anchorage.Intl.AP.702730_TMY3.epw

|#

(define current-location
  (make-parameter
   "https://www.energyplus.net/weather-download/north_and_central_america_wmo_region_4/USA/AK/USA_AK_Anchorage.Intl.AP.702730_TMY3/USA_AK_Anchorage.Intl.AP.702730_TMY3.epw"))

(define (get-weather-for-location [path : Path] [location (current-location)])
  (if (regexp-match? #rx"^http" location)
      (get-weather-from-bytes path (port->bytes (get-pure-port (string->url location))))
      (get-weather-from-bytes path (file->bytes location))))

(define (get-weather-from-bytes [path : Path] bytes)
  (let ((epwpath (path-replace-suffix path ".epw"))
        (weapath (path-replace-suffix path ".wea")))
    (call-with-output-file epwpath #:mode 'text #:exists 'replace
      (lambda (port) (write-bytes bytes port)))
    (radiance-command
     (format "epw2wea \"~A\" \"~A\"" epwpath weapath))
    (call-with-input-file weapath #:mode 'text
      (lambda (port)
        (define (wea-line rx)
          (car (regexp-match* rx (read-line port 'any) #:match-select cadr)))
        (let* ((place (wea-line #rx"place (.+)"))
               (latitude (wea-line #rx"latitude (.+)"))
               (longitude (wea-line #rx"longitude (.+)"))
               (time_zone (wea-line #rx"time_zone (.+)"))
               (site_elevation (wea-line #rx"site_elevation (.+)")))
          (values place latitude longitude time_zone site_elevation weapath))))))


(define (export-sky [path : Path (simulation-path)]
                    #:date [date : Date (date 0 0 9 21 9 0 0 0 #f 0)]
                    #:latitude [latitude : Real 61]
                    #:longitude [longitude : Real 150]
                    #:meridian [meridian : Real 135]
                    #:sun? [sun? : Boolean #t])
  (define (2digits n) (~r n #:min-width 2 #:pad-string "0"))
  (let ((skypath (path-replace-suffix path "_sky.rad")))
    (call-with-output-file skypath #:mode 'text #:exists 'replace
      (lambda (port)
        (fprintf port
                 "!gensky ~A ~A ~A:~A ~A -a ~A -o ~A -m ~A~%"
                 (2digits (date-month date)) (2digits (date-day date))
                 (2digits (date-hour date)) (2digits (date-minute date))
                 (if sun? "+s" "-s")
                 latitude longitude
                 meridian)
        (displayln extra-sky.rad-contents port)
        skypath))))

(define (export-sky-for-date-location
         [path : Path]
         [date : Date]
         [location : String])
  (let-values ([(place latitude longitude time-zone site-elevation weapath)
                (get-weather-for-location path location)])
    (export-sky path
                #:date date
                #:latitude latitude
                #:longitude longitude
                #:meridian time-zone)))

(define (export-CIE.Overcast.Sky [path : Path])
  (let ((skypath (path-replace-suffix path "_sky.rad")))  
    (call-with-output-file skypath #:mode 'text #:exists 'replace
      (lambda (port)
        (displayln CIE.Overcast.Sky.rad-contents port)
        skypath))))

;;DAYSIM

(define (write-daysim-project-files port name directory)
  (fprintf port
           #<<END
# DAYSIM header file generated by Rosetta
project_name         ~A
#project_directory    ~A
bin_directory        ~A
tmp_directory        ~A
Template_File        ~A~%
END
          name
          directory
          "C:\\DIVA\\DaysimBinaries\\"
          directory
          "C:\\DIVA\\Scripts\\DIVATemplate.htm"))

(define (write-daysim-site-for-location
         port
         [path : Path]
         [location : String])
  (let-values ([(place latitude longitude time-zone site-elevation weapath)
                (get-weather-for-location path location)])
    (fprintf port
             #<<END
# Site information
place                ~A
latitude             ~A
longitude            ~A
time_zone            ~A
site_elevation       ~A
first_weekday        1
time_step            60
wea_data_short_file  ~A
wea_data_short_file_units 1
lower_direct_threshold    2
lower_diffuse_threshold   2
output_units              2~%
END
            place latitude longitude time-zone site-elevation weapath)))

(define (write-daysim-building
         port
         name occupancy-file material-file geometry-file sensor-file viewpoint-file [adaptive-zone-applies 0])
  (let ((base (path-replace-suffix geometry-file "")))
    (fprintf port
             #<<END
# Building information
occupancy-file       ~A
material_file        ~A_material_daysim.rad
geometry_file        ~A_daysim.rad
radiance_source_files 2, ~A, ~A
scene_rotation_angle 00
sensor_file          ~A
viewpoint_file       ~A
AdaptiveZoneApplies  ~A
dgp_image_x_size     500
dgp_image_y_size     500~%
END
             occupancy-file base base
             material-file geometry-file
             sensor-file viewpoint-file
             adaptive-zone-applies)))

(define (write-daysim-radiance-simulation-parameters
         port
         [ambient-bounces 2]
         [ambient-resolution 300]
         [ambient-accuracy 0.1]
         [ambient-divisions 1000]
         [ambient-super-samples 20])
  (fprintf port
          #<<END
# Radiance simulation parameters
ab ~A
ar ~A
aa ~A
ad ~A
as ~A
lr 6
st 0.150
sj 1.000
lw 0.004
dj 0.000
ds 0.200
dr 2
dp 512
dt 0

END
          ambient-bounces
          ambient-resolution
          ambient-accuracy
          ambient-divisions
          ambient-super-samples))

#;
(define (write-daysim-daylighting-results port path ellpath)
  (let ((name (path-replace-suffix path "")))
    (fprintf port
             #<<END
# Daylighting Results
UDI_100_active_RGB ~A_UDI_100.DA
UDI_100_2000_active_RGB ~A_UDI_100_2000.DA
UDI_2000_active_RGB ~A_UDI_2000.DA
electric_lighting ~A
direct_sunlight_file ~A.dir
thermal_simulation ~A_intgain.csv
DDS_sensor_file ~A.dds
DDS_file ~A.sen~%
END
             name name name ellpath name name name name)))

;;Daylight autonomy
(define (write-daysim-daylighting-results port path ellpath)
  (let ((name (path-replace-suffix path "")))
    (fprintf port
             #<<END
# Daylighting Results
daylight_autonomy_active_RGB ~A_autonomy.DA
electric_lighting ~A
direct_sunlight_file ~A.dir
thermal_simulation ~A_intgain.csv
DDS_sensor_file ~A.dds
DDS_file ~A.sen~%
END
             name (~a name "_electriclighting.htm") name ellpath name name)))


(define (write-daysim-dynamic-simulation
         port
         name
         occupancy
         [illuminance 300 #;3000 #;500] ;By Luis' recommendation. Isn't this going to affect UDI?
         [dgp-schedule? #f])
  (fprintf port
           #<<END
# Dynamic Simulation Options
==========================
= User Description
==========================
occupancy 5 ~A
minimum_illuminance_level ~A
daylight_savings_time 1
user_profile 1
active 100 1 ~A
==========================
= Shading Control System
==========================
shading 1
static_shading_system ~A.dc ~A.ill
PNGScheduleExists 0~%
END
  occupancy illuminance
  (if dgp-schedule? 3 1)
  name name))

(define (write-daysim-sensors port sensors)
  (display "sensor_file_info " port)
  (for ([sensor (in-list sensors)])
    ;;This must be improved to deal with lighting and shading
    (display "0 " port)))

(define-runtime-path occupancy-directory "occupancy")


(define (export-viewpoint [path : Path (simulation-path)]
                          [x-resolution : Integer 800]
                          [y-resolution : Integer 600])
  (let ((vfpath (path-replace-suffix path ".vf")))
    (let-values ([(camera target lens) (view)])
      (let ((dir (p-p target camera))
            (vv (* 2.25 (atan (/ 24.0 2 lens)) (/ 180 pi)))
            (vh (* 2.25 (atan (/ 36.0 2 lens)) (/ 180 pi))))
        (call-with-output-file vfpath #:mode 'text #:exists 'replace
          (lambda (port)
            (fprintf port
                     "rview Perspective -vt~A -vp ~A ~A ~A -vd ~A ~A ~A -vu 0 0 1 -vh ~A -vv ~A -vs 0 -vl 0 -x ~A -y ~A~%"
                     "v"
                     (cx camera) (cy camera) (cz camera)
                     (cx dir) (cy dir) (cz dir)
                     vh vv
                     x-resolution
                     y-resolution)
            vfpath))))))

(define daysim-min-udi (make-parameter 100.0))
(define daysim-max-udi (make-parameter 2000.0))

(define (export-to-daysim
         [path : Path (simulation-path)]
         #:sensors [sensors (sensors)]
         #:date [date : Date (current-date)]
         #:location [location : String (current-location)]
         #:occupancy [occupancy-file : String "weekdays9to5withDST.60min.occ.csv"]
         #:parameters [parameters : String "-ab 2 -ad 1000 -as 20 -ar 300 -aa 0.1"]
         #:min-udi [min-udi : Real (daysim-min-udi)]
         #:max-udi [max-udi : Real (daysim-max-udi)])
  ;(append-ies-xform path)
  (let ((radpath (export-geometry path))
        (matpath (export-materials path))
        (vfpath (export-viewpoint path))
        (ptspath (export-sensors path sensors))
        (occpath (build-path occupancy-directory occupancy-file))
        (heapath (path-replace-suffix path ".hea"))
        (emppath (path-replace-suffix path ""))
        (ellpath (path-replace-suffix path "_electriclighting.htm")))
    (let-values ([(dir name dir?) (split-path emppath)])
      (call-with-output-file heapath #:mode 'text #:exists 'replace
        (lambda (port)
          (write-daysim-project-files port name dir)
          (write-daysim-site-for-location port path location)
          (write-daysim-building port name occpath matpath radpath ptspath vfpath)
          (write-daysim-radiance-simulation-parameters port)
          (write-daysim-daylighting-results port path ellpath)
          (write-daysim-dynamic-simulation port emppath occpath)
          (write-daysim-sensors port sensors)))
      (radiance-command (format "radfiles2daysim \"~A\" -m -g" heapath))
      (radiance-command (format "gen_dc \"~A\" -dif" heapath))
      (radiance-command (format "gen_dc \"~A\" -dir" heapath))
      (radiance-command (format "gen_dc \"~A\" -paste" heapath))
      (radiance-command (format "ds_illum \"~A\"" heapath))
      (radiance-command (format "gen_directsunlight \"~A\"" heapath))
      (if (and (= min-udi 100.0) (= max-udi 2000.0))
          (radiance-command (format "ds_el_lighting \"~A\"" heapath))
          (radiance-command (format "ds_el_lighting \"~A\" ~A ~A" heapath min-udi max-udi)))
      (extract-daysim-results ellpath))))

(define (extract-daysim-results path)
  (let ((str (call-with-input-file path #:mode 'text port->string)))
    (define (match-% rexp)
      (/ (string->number (second (regexp-match rexp str))) 100))
    (values (match-% #rx"Daylight Factor \\(DF\\) Analysis:</u> ([0-9]+)%")
            (match-% #rx"Daylight Autonomy \\(DA\\) Analysis:</u>\nThe mean daylight autonomy is ([0-9]+)%")
            (match-% #rx"Continuous Daylight Autonomy \\(DA\\) Analysis:</u>\nThe mean continuous daylight autonomy is ([0-9]+)%") 
            (match-% #rx"Useful Daylight Illuminance \\(UDI\\):</u>\nThe percentage of the space with a UDI<sub><100-2000lux</sub> larger than 50% is ([0-9]+)%"))))

(provide radiation-map)
(define (radiation-map
         [path : Path (simulation-path)]
         #:sensors [sensors (sensors)]
         #:date [date : Date (current-date)]
         #:location [location : String (current-location)]
         #:parameters [parameters : String "-ab 2 -ad 1000 -as 20 -ar 300 -aa 0.1"])
  ;(append-ies-xform path)
  (let ((radpath (export-geometry path))
        (matpath (export-materials path))
        (skypath (export-CIE.Overcast.Sky path) #;(export-sky-for-date-location path date location))
        (octpath (path-replace-suffix path ".oct"))
        (ptspath (export-sensors path sensors))
        (datpath (path-replace-suffix path ".dat")))
    (if (string=? (current-backend-name) "AutoCAD")
        (printf "AutoCAD can't make the computation (yet)~%")
        (begin
          (radiance-command
           (format "oconv \"~A\" \"~A\" \"~A\" > \"~A\"" matpath skypath radpath octpath))
          (radiance-command
           (format "rtrace -I -h -dp 2048 -ms 0.063 -ds .2 -dt .05 -dc .75 -dr 3 -st .01 -lr 12 -lw .0005 ~A \"~A\" < \"~A\" > \"~A\""
                   parameters octpath ptspath datpath))))
    (present-sensors-colors datpath)))

(provide saved-radiation-map)
(define (saved-radiation-map [path : Path (simulation-path)])
  (let ((datpath (path-replace-suffix path ".dat")))
    (present-sensors-colors datpath)))

(provide radiation-map-file)
(define (radiation-map-file [path : Path (simulation-path)])
  (path-replace-suffix path ".dat"))   

(provide present-sensors-colors)
(define (present-sensors-colors
         datpath
         [scheme blue-red-color-scheme]
         [min-radiance #f]
         [max-radiance #f])
  (let ((colors (read-sensors datpath)))
    (show-colors-of-sensors (remap-surfaces-and-grids-colors colors scheme min-radiance max-radiance))
    (export-sensors-colors datpath)
    colors))

(define blue-red-color-scheme (list (rgb 74 103 161) (rgb 253 239 80) (rgb 234 47 0)))
(define funny-color-scheme (list (rgb 118 10 140) (rgb 10 22 183) (rgb 48 172 66) (rgb 223 24 15) (rgb 254 245 44)))
(define blue-red-color-scheme2 (list (rgb 74 103 161) (rgb 74 103 161) (rgb 253 239 80) (rgb 234 47 0) (rgb 234 47 0)))

(define (combine-rgb op c0 c1)
  (rgb (op (rgb-red c0) (rgb-red c1))
       (op (rgb-green c0) (rgb-green c1))
       (op (rgb-blue c0) (rgb-blue c1))))

(define (max-rgb c0 c1) (combine-rgb max c0 c1))
(define (min-rgb c0 c1) (combine-rgb min c0 c1))

(define (remap-surfaces-and-grids-colors
         colors
         [scheme blue-red-color-scheme]
         [min-radiance #f]
         [max-radiance #f])
  (let* ((radiances (map radiance<-color colors))
         (scheme-radiances (map radiance<-color scheme))
         (min-radiance (or min-radiance (foldl min (first radiances) (rest radiances))))
         (max-radiance (or max-radiance (foldl max (first radiances) (rest radiances))))
         (radiance-range (- max-radiance min-radiance)))
    (printf "radiance (min max): ~a ~a~%" min-radiance max-radiance)
    (for/list ((radiance (in-list radiances)))
      (let* ((norm (/ (- radiance min-radiance) radiance-range)))
        (remap-color norm scheme)))))

(define (remap-color norm scheme)
  (let* ((steps (- (length scheme) 1))
         (idx (inexact->exact (floor (* (min norm 0.99) steps))))
         (subscheme (drop scheme idx))
         (subnorm (* (- norm (* (floor (* (min norm 0.99) steps)) (/ 1 steps))) steps)))
    (combine-rgb (lambda (v0 v1)
                   (inexact->exact (round (+ v0 (* subnorm (- v1 v0))))))
                 (first subscheme)
                 (second subscheme))))

(provide analyze)
(define (analyze start evaluate improve n)
  (define (loop data i)
    (printf "~A:~A->~A~%" i data (evaluate data))
    (if (= i n)
        #t
        (loop (improve data) (+ i 1))))
  (loop start 0))

(provide maximize)
(define (maximize start evaluate improve n)
  (define (loop data value i)
    (if (= i n)
        (values data value)
        (let ((new-data (improve data)))
          (let ((new-value (evaluate new-data)))
            (if (> new-value value)
                (begin
                  (printf "~A:~A->~A~%" i new-data new-value)
                  (loop new-data new-value (+ i 1)))
                (loop data value (+ i 1)))))))
  (loop start (evaluate start) 0))

