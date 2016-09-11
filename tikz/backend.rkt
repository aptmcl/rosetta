#lang typed/racket/base/no-check
(require racket/math racket/list racket/file)

#|;;; Generates a pdf using the tikz backend
 |(define (generateTikz [file-name "tmp"] [scale 1] [pdf-viewer "evince"])
 |  (define (tikz->tex str out)
 |    (let ((scale (* scale 0.024)))
 |      (fprintf out
 |               "\\documentclass[]{article}\n\\usepackage{tikz}\n\\begin{document}\n\\begin{tikzpicture}[yscale=-~a,xscale=~a]\n~a\n\\end{tikzpicture}\n\\end{document}"
 |               scale scale str)))
 |  (define (display-tikz-to-string)
 |    (let ([output-port (open-output-string)])
 |      (parameterize ([current-output-port output-port])
 |        (display-tikz))
 |      (get-output-string output-port)))
 |
 |  (define out
 |    (open-output-file (string-append file-name ".tex") #:exists 'replace))
 |
 |  (tikz->tex (display-tikz-to-string) out)
 |  (close-output-port out)
 |  (system (string-append "pdflatex " file-name ".tex"))
 |  (system (string-append pdf-viewer " " file-name ".pdf"))
 |  (system "rm *.tex *.log *.aux -f"))
 |#

(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt")
(require (prefix-in % "primops.rkt"))
(provide (all-from-out "../base/coord.rkt"))
(provide (all-from-out "../base/utils.rkt"))
(provide immediate-mode? current-backend-name tikz-output)

(define (current-backend-name) "TikZ")


(define (tikz-output)
  (%get-accumulated-tikz))

(define-syntax-rule
  (with-3d (c) expr)
  (let ((c c))
    (if (world-cs? c)
        expr
        (%transform
         (lambda () : Void
           (let ((c (u0 world-cs)))
             expr
             (void)))
         c))))

(def-shape (point [position : Loc (u0)])
  (let ((center (loc-in-world position)))
    (%point center)))

(def-shape (circle [center : Loc (u0)] [radius : Real 1])
  (with-3d (center)
    (%circle center radius #f)))

(def-shape (surface-circle [center : Loc (u0)] [radius : Real 1])
  (with-3d (center)
    (%circle center radius #t)))

;; arc

(define (create-arc [c : Loc (u0)] [r : Real 1] [beg-a : Real 0] [a : Real pi] [fill? : Boolean #f])
  (cond ((= r 0)
         (%point c))
        ((= a 0)
         (%point (+pol c r beg-a)))
        ((>= (abs a) 2pi)
         (%circle c r fill?))
        (else
         (let ((end-a (+ beg-a a)))
           (if (> end-a beg-a)
               (%arc c r beg-a end-a fill?)
               (%arc c r end-a beg-a fill?))))))

(def-shape (arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (with-3d (center)
    (create-arc center radius start-angle amplitude #f)))

(def-shape (surface-arc [center : Loc (u0)] [radius : Real 1] [start-angle : Real 0] [amplitude : Real pi])
  (with-3d (center)
    (create-arc center radius start-angle amplitude #t)))

(def-shape (ellipse [center : Loc (u0)] [radius-x : Real 1] [radius-y : Real 1])
  (with-3d (center)
    (%ellipse center radius-x radius-y 0 #f)))

(def-shape* (line [pts : Loc *])
  (%line (map loc-in-world pts)))

(def-shape* (closed-line [pts : Loc *])
  (%closed-line (map loc-in-world pts)))

;;A polygon should have all its vertices in the same plane
;;but we don't check that
(def-shape* (polygon [pts : Loc *])
  (%closed-line (map loc-in-world pts)))

(def-shape* (surface-polygon [pts : Loc *])
  (%closed-line (map loc-in-world pts) #t))


(def-shape* (spline [pts : Loc *]) ; [v0 : (U #f Vec) #f] [v1 : (U #f Vec) #f])
  (if #f ;(and v0 v1) ;;HACK This should be solved by dealing with optionals
      (error "Finish this")
      (%hobby-spline pts #f)))

(def-shape* (closed-spline [pts : Loc *])
  (%hobby-closed-spline pts #f))

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

(def-shape (regular-polygon [edges : Integer 3] [center : Loc (u0)] [radius : Real 1] [angle : Real 0] [inscribed? : Boolean #f])
  (let ((center (loc-in-world center)))
    (if (and (= radius 0) (allow-degenerate-radius?))
        (%point center)
        (let ((pts (regular-polygon-vertices edges center radius angle inscribed?)))
          (%line (map loc-in-world (append pts (list (car pts)))))))))

#|
(def-shape (surface-regular-polygon [edges : Integer] [c : Loc (u0)] [r : Real 1] [a : Real 0] [inscribed? : Boolean #f])
  (if (= r 0)
      (%point c)
      (%region-from-curves 
       (list (%closed-line (regular-polygon-vertices edges c r a inscribed?))))))

(def-shape (surface [shapes : (Listof shape)])
  (%region-from-curves (shapes-refs shapes)))
|#
;; rectangle

(def-shape (rectangle [c : Loc (u0)] [dx/c1 : (U Loc Real) 1] [dy : Real 1])
   (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
     (or ;(degenerate-rectangle c dx dy)
         (with-3d (c)
           (%rectangle c dx dy #f)))))

(def-shape (surface-rectangle [c : Loc (u0)] [dx/c1 : (U Loc Real) 1] [dy : Real 1])
   (let-values ([(dx dy) (rectangle-deltas c dx/c1 dy)])
     (or ;(degenerate-rectangle c dx dy)
         (with-3d (c)
           (%rectangle c dx dy #t)))))

; text

(def-shape (text [str : String] [p : Loc (u0)] [h : Real 1])
  (with-3d (p)
    (%text str p h)))

(define (text-length [str : String] [h : Real 1]) : Real
  ;;HACK conservative approach
  (* (string-length str) h 0.7))

(def-shape (text-centered [str : String ""] [p : Loc (u0)] [h : Real 1])
  (with-3d (p)
    (%text str (+xy p (/ (text-length str h) -2) (/ h -2)) h)))

#|  

; Predicates

(define (surface-region? [s : shape]) : Boolean
  (or (surface? s) 
      (surface-circle? s)
      (surface-arc? s)
      (surface-rectangle? s)
      (surface-polygon? s)
      (for/and ([r (shape-refs s)]) (%region? r))))

#;
(define (smooth-curve? s)
  (or (spline? s) (closed-spline? s) (circle? s))) ;;HACK ellipse?

#;
(define (closed-curve? s)
  (or (closed-line? s) (closed-spline? s)))

(def-shape (extrusion [profile : shape] [dir : (U Loc Real) 1])
  (begin0
    ;;When dir is a number, there is an AutoCAD bug 
    ;;that causes some profiles to be extruded in 
    ;;the wrong direction,
    (%extrude-command-direction 
     (shape-refs profile)
     (u0 world-cs)
     (if (number? dir) (z dir) dir)
     (surface-region? profile))
    (delete-shape profile)))

(provide move)
(define (move [sh : shape] [v : Loc])
  (unless #f;(empty-shape? s)
    (with-ref (r sh)
      (%move r (u0 world-cs) v)))
  sh)

(def-shape (mirror [sh : shape] [p : Loc (u0)] [n : Loc (uz)])
  (let ((p (u0 (cs-from-o-n p (-c n (u0 world-cs)))))) ;;Convert to vector. HACK: Is this a good idea?
    (let ((sm (map-ref (r sh) (%mirror3d r p (+x p 1) (+y p 1)))))
      (append sm (shape-refs sh)))))

#;#;
(provide scale)
(define (scale [sh : shape] [p : Loc (u0)] [s : Real 1])
  (with-ref (r sh)
    (%scale-entity r p s))
  sh)


#;#;(provide rotate)
(define (rotate [sh : shape] [a : Real] [p0 : Loc (u0)] [p1 : Loc (+z p0 1)])
  (with-ref (r sh)
    (%rotate3d r p0 p1 a))
  sh)

(define (create-layer [name : String]) : String
  (ac:add-layer name)
  name)

;;Default layers
(define white-floor-layer "Floor")
(define black-floor-layer "FloorBlack")
(define dark-gray-layer "DarkGray")
(define white-layer "White")
(define black-layer "Black")
(define floor-layer (make-parameter white-floor-layer))
(define shapes-layer (make-parameter dark-gray-layer))

(define (create-layers)
  (create-layer white-floor-layer)
  (create-layer black-floor-layer)
  (create-layer dark-gray-layer)
  (create-layer white-layer)
  (create-layer black-layer)
  (floor-layer white-floor-layer)
  (shapes-layer dark-gray-layer))


(def-shape-op (shape-layer shape [new-layer #f])
  (autocad
   (if new-layer
       (begin
         (ac:layer (shape-impl shape) new-layer)
         shape)
       (ac:layer (shape-impl shape))))
  (rhino
   (if new-layer
       (begin
         (rh:add-layer new-layer)
         (rh:object-layer (shape-impl shape) new-layer)
         shape)
       (rh:object-layer (shape-impl shape)))))

(provide hack-shapes-layer)
(define (hack-shapes-layer ss layer)
  (map-failed-operation
   (lambda (s)
     (if (or (empty-shape? s) (universal-shape? s))
         s
         (shape-layer s layer)))
   ss))

(def-shape-op (shape-material shape [new-material #f])
  (autocad
   (if new-material
       (begin
         (ac:add-material new-material)
         (ac:material (shape-impl shape) new-material)
         shape)
       (ac:material (shape-impl shape))))
  #;(rhino
   (if new-material
       (begin
         (rh:object-material (shape-impl shape) new-material)
         shape)
       (rh:object-material (shape-impl shape)))))

;; (def-shape-op (shapes-layer shapes [new-layer #f])
;;   (autocad
;;    (if new-layer
;;        (begin
;;          (ac:add-layer new-layer)
;;          (for ((shape (in-list shapes)))
;;            (ac:layer (shape-impl shape) new-layer)))
;;        (remove-duplicates (map (lambda (s) (ac:layer (shape-impl s))) shapes)))))

(define (current-layer [new-layer #f])
  (autocad
   (if new-layer
       (ac:clayer new-layer)
       (ac:clayer)))
  (rhino
   (if new-layer
       (rh:current-layer new-layer)
       (rh:current-layer)))
  (opengl
   ;;HACK This must be improved
   #f))

(define (layer new-layer)
  (autocad
   (ac:add-layer new-layer)
   (ac:clayer new-layer))
  (rhino
   (rh:add-layer new-layer)
   (rh:current-layer new-layer)))


(provide with-current-layer)
(define-syntax (with-current-layer stx)
  (syntax-case stx ()
    ((_ new-layer body ...)
     (syntax/loc stx
       (let ((old-layer (current-layer)))
         (dynamic-wind
           (lambda () (layer new-layer))
           (lambda () body ...)
           (lambda () (layer old-layer))))))))

(def-new-shape-op* (join-curves shapes)
  (rhino
   (begin0
       (rh:join-curves (map shape-impl shapes) #t)
     (delete-shapes shapes)))
  (autocad
   (begin0
       (ac:join-curves (map shape-impl shapes))
     (mark-deleted! shapes)))
  (opengl
   (begin0
       (gl:join-curves (map shape-impl shapes))
     (delete-shapes shapes))))

(def-new-shape-op* (join-surfaces shapes)
  (rhino
   (begin0
       (rh:join-surfaces (map shape-impl shapes) #f)
     (delete-shapes shapes)))
  (autocad
   (begin0
       (ac:join-command (map shape-impl shapes))
     (delete-shapes shapes))))


(def-new-shape-op (fillet shape0 shape1 [radius 0])
  (autocad
   (let-shapes ((r0 shape0) (r1 shape1))
     (begin0
       (ac:join-curves
        (list r0
              (ac:fillet-command r0 r1 radius)
              r1))
       (mark-deleted! shape0 shape1)))))

(def-new-shape-op* (inner-solid shapes)
  (autocad
   (let ((ss 
          (map (lambda (shape) 
                 (ac:as-surface (shape-impl shape)))
               shapes)))
     (begin0
       (ac:interior-solid ss)
       (delete-shapes shapes))))
  (rhino
   (let ((ss (map shape-impl shapes)))
     (begin0
       (rh:create-solid ss)
       (delete-shapes shapes)))))

(def-new-shape-op* (solid shapes)
  (autocad
   (let ((ss 
          (map (lambda (shape) 
                 (ac:as-surface (shape-impl shape)))
               shapes)))
     (begin0
       (ac:interior-solid ss)
       (delete-shapes shapes))))
  (rhino
   (let ((ss (map shape-impl shapes)))
     (begin0
       (rh:create-solid ss #t) ;;HACK rh:join-surfaces is not enough for Market Hall.
       (mark-deleted! shapes) #;(delete-shapes shapes)))))

(provide select-shapes)
(def-shape-op (select-shapes s)
  (rhino
   (rh:select-objects (collect-all-shapes s))))

(provide select-shape)
(define select-shape select-shapes)


(define (split-brep id cutter)
  (begin0
    (match (cons id cutter)
      ((cons (string-id str) (string-id cutter-str))
       (map make-string-id (com:split-brep str cutter-str #t))))
    (delete-object cutter)))

; lines


(define (nurbs-curve controls knots)
  (define (uniform-knots? knots) #t) ;;AML: must be finished
  (if (uniform-knots? knots)
      (add-curve controls (min 3 (- (length controls) 1)))
      (error "Rhino curve does not support non uniform knots")))


; surface

(define (generate-knot-vector order total)
  (let ((knots (make-vector (+ total order))))
    (let iter ((i 0))
      (if (< i order)
          (begin
            (vector-set! knots i 0.0)
            (iter (+ i 1)))
          (let iter ((j 1) (i i))
            (if (<= j (- total order))
                (begin
                  (vector-set! knots i (/ j (+ (- total order) 1.0)))
                  (iter (+ j 1) (+ i 1)))
                (let iter ((j 0) (i i))
                  (if (< j order)
                      (begin 
                        (vector-set! knots i 1.0)
                        (iter (+ j 1) (+ i 1)))
                      knots))))))))

(define (nurbs-surface controls u-knots v-knots)
  (define (degree-from-n-controls n)
    (cond ((< n 3)
           1)
          ((< n 6)
           3)
          (else
           5)))
  (let ((n-rows (length controls))
        (n-cols (length (car controls))))
    (let ((u-degree (degree-from-n-controls n-rows))
          (v-degree (degree-from-n-controls n-cols)))
      (make-string-id
       (com:add-nurbs-surface 
        (list n-rows n-cols)
        controls
        (generate-knot-vector (- u-degree 1) n-rows)
        (generate-knot-vector (- v-degree 1) n-cols)
        (list u-degree v-degree)
        #;(map (lambda (row) (map (lambda (col) 1.0) row)) controls))))))

#;#;
(provide planar-surface)
(define planar-surface surface)

(def-new-shape-op* (planar-surface shapes)
  (rhino
   (begin0
     (rh:add-planar-srf (map shape-impl shapes))
     (delete-shapes shapes))))
|#

(define (transpose-ptss [ptss : (Listof (Listof Loc))]) : (Listof (Listof Loc))
  (if (null? (car ptss))
      (list)
      (cons (map (inst car Loc (Listof Loc)) ptss)
            (transpose-ptss (map (inst cdr Loc (Listof Loc)) ptss)))))


(def-shape (surface-grid [ptss : (Listof (Listof Loc))] [closed-u? : Boolean #f] [closed-v? : Boolean #f])
  (let ((ptss
         (for/list : (Listof (Listof Loc)) ([pts (in-list ptss)])
           (for/list : (Listof Loc) ((pt (in-list pts)))
             (loc-in-world pt)))))
    (for ((pts (in-list ptss)))
      (%closed-line pts))
    (for ((pts (in-list (transpose-ptss ptss))))
      (%closed-line (map loc-in-world pts))))
  #;
  (for ((pts0 (in-list ptss))
        (pts1 (in-list (cdr ptss))))
    (for ((pt0 (in-list pts0))
          (pt1 (in-list (cdr pts0)))
          (pt2 (in-list (cdr pts1)))
          (pt3 (in-list pts1)))
      (%closed-line (map loc-in-world (list pt0 pt1 pt2 pt3))))))

#|
(def-new-shape (mesh-grid css [closed-u? #f] [closed-v? #f])
  (rhino
   (rh:add-mesh
    css
    (let ((rows (length css))
          (cols (length (car css))))
      (for*/vector ([row0 (in-range 0 (- rows 1))]
                    [col0 (in-range 0 (- cols 1))])
        (let ((row1 (+ row0 1))
              (col1 (+ col0 1)))
          (let ((c0 (+ (* row0 cols) col0))
                (c1 (+ (* row0 cols) col1))
                (c2 (+ (* row1 cols) col1))
                (c3 (+ (* row1 cols) col0)))
            (vector c0 c1 c2 c3))))))))

(def-shape-op (mesh-faces s)
  (rhino
   (let loop ((l (rh:mesh-faces (shape-impl s) #t))) ;;only triangles
     (if (null? l)
         (list)
         (cons (take l 4)
               (loop (drop l 4)))))))

(def-shape-op (mesh-normals s)
  (rhino
   (rh:mesh-face-normals (shape-impl s))))


(def-new-shape-op (mesh-offset s d)
  (rhino
   (rh:mesh-offset (shape-impl s) d)))

; transformations

; sweep

(def-shape (sweep [path : shape] [profile : shape] [rotation : Real 0] [scale : Real 1])
  (begin0
    (ac:singleton-or-union
     (ac:sweep-command (shape-ref profile) #t (shape-ref path) (surface-region? profile) rotation scale))
    (delete-shapes (list profile path))))

; loft

(def-new-shape-op* (loft-ruled shapes)
  (else
   (loft shapes #t)))

(def-new-shape-op ((guided-loft* guided-loft) shapes rails [ruled? #f] [solid? #f] [closed? #f])
  (rhino
   (if (> (length rails) 2)
       (error 'guided-loft "Rhino only supports two rails but were passed ~A" rails)
       (let ((r (rh:add-sweep2 (map shape-impl rails) (map shape-impl shapes))))
         (when solid?
           (rh:cap-planar-holes r))
         (delete-shapes shapes)
         (delete-shapes rails)
         r)))
  (autocad
   (begin0
       (ac:loft-command (ac:loft-objects-guides-string (map shape-impl shapes) (map shape-impl rails) solid?)
                        (if ruled? ac:loftnormals-ruled ac:loftnormals-smooth-fit)
                        closed?)
     ;(delete-shapes shapes) ;For some reason, AutoCAD does not close the solid...
     (delete-shapes rails))))

(define (curve? s)
  (or (line? s)
      (closed-line? s)
      (spline? s)
      (closed-spline? s)
      (circle? s)
      (arc? s)))

(provide guided-loft)
(define (guided-loft shapes rails [ruled? #f] [solid? #f] [closed? #f])
  (cond ((null? (cdr shapes))
         (delete-shapes rails)
         (car shapes))
        ((andmap curve? shapes)
         (guided-loft* shapes rails ruled? solid? closed?))
        ((andmap surface-region? shapes)
         (guided-loft* shapes rails ruled? #t closed?))
        (else
         (error 'loft-shapes "cross sections are not either curves or surfaces"))))

(provide loft)
(define (loft shapes [ruled? #f] [solid? #f] [closed? #f])
  (cond ((null? (cdr shapes))
         (car shapes))
        ((andmap point? shapes)
         (begin0
           ((if ruled?
                (if closed? polygon line)
                (if closed? closed-spline spline))
            (map point-position shapes))
           (delete-shapes shapes)))
        ((andmap curve? shapes)
         (loft-curves shapes ruled? solid? closed?))
        ((andmap surface-region? shapes)
         (loft-surfaces shapes ruled? #t closed?))
        ((and (null? (cddr shapes))
              (ormap point? shapes))
         (let-values ([(p s)
                       (if (point? (car shapes))
                           (values (car shapes) (cadr shapes))
                           (values (cadr shapes) (car shapes)))])
           (cond ((curve? s)
                  (loft-curve-point s p solid?))
                 ((surface-region? s)
                  (loft-surface-point s p #t))
                 (else
                  (error 'loft-shapes "can't loft the shapes ~A" shapes)))))
        (else
         (error 'loft-shapes "cross sections are not either curves or surfaces"))))


(def-new-shape-op (loft-curves shapes [ruled? #f] [solid? #f] [closed? #f])
  (rhino
   (let ((rs (map shape-impl shapes)))
     ;;Adjust seams
     #;(let ((ref-p (rh:evaluate-curve (car rs) 0.0)))
       (for ((r (in-list (cdr rs))))
         (rh:curve-seam r (rh:curve-closest-point r ref-p))))
     #;(let ((ref-p (xyz -100000 -10000 -10000)))
       (for ((r (in-list rs)))
         (rh:curve-seam r (rh:curve-closest-point r ref-p))))
     (let ((r (rh:add-loft-srf rs rh:com-omit rh:com-omit
                               (if ruled? rh:loft-type-straight rh:loft-type-normal)
                               rh:com-omit rh:com-omit closed?)))
       (when solid?
         (rh:cap-planar-holes r))
       (delete-shapes shapes)
       r)))
  (autocad
   (begin0
       (ac:loft-command (ac:loft-objects-string (map shape-impl shapes) solid?)
                        (if ruled? ac:loftnormals-ruled ac:loftnormals-smooth-fit)
                        closed?)
     (delete-shapes shapes)))
  (opengl
   (begin0
     (gl:add-loft (map shape-impl shapes) ruled? solid? closed? (andmap line? shapes))
     (delete-shapes shapes))))

(def-new-shape-op (loft-curve-point shape point [solid? #f])
  (rhino
   (let-shapes ((r shape)
                (p point))
     (let ((r (rh:extrude-curve-point r (rh:point-coordinates p))))
       (when solid?
         (rh:cap-planar-holes r))
       (delete-shape shape)
       (delete-shape point))))
  (autocad
   (let-shapes ((r shape)
                (p point))
     (begin0
         (ac:loft-command (ac:loft-to-point-string
                           (shape-ref (surface-boundary shape))
                           (car (ac:coordinates p))
                           solid?)
                          ac:loftnormals-ruled
                          #f)
       (delete-shape shape)
       (delete-shape point))))
  (opengl
   (let-shapes ((s shape)
                (p point))
     (begin0
         (gl:add-loft-curve-point s p solid? (line? shape))
       (delete-shape shape)
       (delete-shape point)))))

(def-new-shape-op (loft-surfaces shapes [ruled? #f] [solid? #t] [closed? #f])
  (autocad
   (begin0
     (ac:loft-command (ac:loft-objects-string (map shape-impl shapes) solid?)
                      (if ruled? ac:loftnormals-ruled ac:loftnormals-smooth-fit)
                      closed?)
     #;(delete-shapes shapes)))
  (else
    (begin0
        (loft-curves (map surface-boundary shapes) ruled? solid? closed?)
      (delete-shapes shapes))))

;;HACK: rename this after removing the previous one
(def-new-shape-op (loft-surfaces2 shapes [ruled? #f] [solid? #t] [closed? #f])
  (autocad
   (let ((boundary (map surface-boundary shapes)))
     (begin0
       (ac:loft-command (ac:loft-objects-string 
                         (map shape-impl boundary)
                         solid?)
                        (if ruled? ac:loftnormals-ruled ac:loftnormals-smooth-fit)
                        closed?)
       (delete-shapes boundary))))
  (else
    (begin0
        (loft-curves (map surface-boundary shapes) ruled? solid? closed?)
      (delete-shapes shapes))))

(def-new-shape-op (loft-surfaces3 shapes [ruled? #f] [solid? #t] [closed? #f])
  (autocad
   (let ((boundary (map surface-boundary shapes)))
     (begin0
       (ac:loft-command (ac:loft-objects-string 
                         (map shape-impl boundary)
                         solid?)
                        (if ruled? ac:loftnormals-ruled ac:loftnormals-smooth-fit)
                        closed?)
       (delete-shapes shapes)
       (delete-shapes boundary))))
  (else
    (begin0
        (loft-curves (map surface-boundary shapes) ruled? solid? closed?)
      (delete-shapes shapes))))


(def-new-shape-op (loft-surface-point shape point [solid? #t])
  (else
   (begin0
     (loft-curve-point (surface-boundary shape) point solid?)
     (delete-shape shape))))

(provide slice)
(define (slice shape [p (u0)] [n (uz)])
  (cond ((empty-shape? shape)
         shape)
        ((failed-union? shape)
         (union
          (map (lambda (s) (slice s p n))
               (failed-operation-shapes shape))))
        ((failed-subtraction? shape)
         (subtraction
          (map (lambda (s) (slice s p n))
               (failed-operation-shapes shape))))
        (else
         (slice* shape (loc-from-normal p n)))))

(def-new-shape-op ((slice* slice) shape plane)
  (autocad
   (begin0
       (ac:slice-command (shape-ref shape) plane (uz))
     (mark-deleted! shape)))
  (rhino
   (begin0
       (let ((p plane)
             (r (shape-ref shape)))
         (let ((pw (as-world p)))
           (let ((n (-c (as-world (+z p 1)) pw)))
             (let ((cutter (rh:add-cut-plane
                            r pw
                            (as-world (+x p 1))
                            (-c (as-world (+y p 1)) pw))))
               (let ((rs (rh:split-brep r cutter #f)))
                 (begin0
                     (if (null? rs)
                         r
                         (begin
                           (rh:delete-object r)
                           (let-values (([keep clear]
                                         (partition rh:cap-planar-holes rs)))
                             (rh:delete-objects clear)
                             (let-values (([keep clear]
                                           (partition
                                            (lambda (r)
                                              (let ((c (car (rh:surface-volume-centroid r))))
                                                (< (dot-c (-c c pw) n) 0)))
                                            keep)))
                               (rh:delete-objects clear)
                               ;;HACK Check this
                               (singleton-value keep)))))
                   (rh:delete-objects cutter)))))))
     (mark-deleted! shape))))


(def-shape-op (slice-with-surface shape surface)
  (autocad
   (let-shapes ((sh shape) (su surface))
     (let ((s (ac:as-surface su)))
       (begin0
         (map (lambda (r) (new-shape 'slice autocad r))
              (cons sh (ac:slice-with-surface (shape-impl shape) s)))
         (ac:delete s)
         (mark-deleted! shape)))))
  (rhino
   (let-shapes ((sh shape) (su surface))
     (begin0
       (map (lambda (id)
              (new-shape 'slice rhino id))
            (rh:split-brep-sloppy-n sh su 5))
       ))))

;;Surfaces

(def-shape-op (surface-domain-u surface)
  (rhino
   (rh:surface-domain (shape-impl surface) 0)))

(def-shape-op (surface-domain-v surface)
  (rhino
   (rh:surface-domain (shape-impl surface) 1)))


;;Iso curves

(def-new-shape-op (isoparametric-curve-u surface v)
  (rhino
   (let-shapes ((s surface))
     (rh:singleton-id (rh:extract-iso-curve s (vector 0.0 v) 0)))))


(def-new-shape-op (isoparametric-curve-v surface u)
  (rhino
   (let-shapes ((s surface))
     (rh:singleton-id (rh:extract-iso-curve s (vector u 0.0) 1)))))


;;half spaces

(def-new-shape (half-space p n)
  (else
   (cons p n)))

(def-shape-op (half-space-point hs)
  (else
   (car (shape-ref hs))))

(def-shape-op (half-space-normal hs)
  (else
   (cdr (shape-ref hs))))

;;HACK to be continued, to implement
;;slices (or cuts)
;;

(provide union)
(define (union . shapes-tree)
  (let ((ss (filter-not empty-shape? (remove-duplicates (flatten shapes-tree)))))
    (cond ((null? ss)
           (empty-shape))
          ((null? (cdr ss))
           (car ss))
          ((ormap universal-shape? ss)
           (delete-shapes ss)
           (universal-shape))
          (else
           (let-values ([(failed ss) (partition failed-operation? ss)])
             (let-values ([(failed-unions failed-subtractions) (partition failed-union? failed)])
               (failed-union (append (append-map failed-operation-shapes failed-unions) ss failed-subtractions))
               ;; (let ((s (pure-union (append (append-map failed-operation-shapes failed-unions) ss))))
               ;;   (cond ((null? failed-subtractions)
               ;;          s)
               ;;         ((universal-shape? s) ;;This never happens (I suppose)
               ;;          (delete-shapes failed)
               ;;          s)
               ;;         ((empty-shape? s)
               ;;          (failed-union failed-subtractions))
               ;;         (else
               ;;          (distribute-operation
               ;;           union
               ;;           s
               ;;           failed-subtractions))))
               ))))))

(def-shape-op (pure-union2 sh0 sh1)
  (let-shapes ((r0 sh0) (r1 sh1))
    (new-shape 'union autocad (ac:boolean-union r0 r1))))

(define (pure-union ss)
  (cond ((null? ss)
         (empty-shape))
        ((null? (cdr ss))
         (car ss))
        (else
         ;;We don't have multi-argument union in autocad, but unions do not fail
         (let loop ((curr (car ss)) (rest (cdr ss)))
           (if (null? rest)
               curr
               (let ((r (pure-union2 curr (car rest))))
                 (loop r (cdr rest))))))))

(provide intersection)
(define (intersection . shapes-tree)
  (let ((ss (filter-not universal-shape? (remove-duplicates (flatten shapes-tree)))))
    (cond ((null? ss)
           (universal-shape))
          ((null? (cdr ss))
           (car ss))
          ((ormap empty-shape? ss)
           (delete-shapes ss)
           (empty-shape))
          (else
           (let-values ([(failed ss) (partition failed-operation? ss)])
             (let ((s (pure-intersection ss)))
               (cond ((null? failed)
                      s)
                     ((empty-shape? s)
                      (delete-shapes failed)
                      s)
                     (else
                      (distribute-operation
                       intersection
                       (car failed)
                       (if (universal-shape? s)
                           (cdr failed)
                           (cons s (cdr failed))))))))))))

(define (copy-shapes-n ss n)
  (if (= n 1)
      (list ss)
      (cons (copy-shapes ss)
            (copy-shapes-n ss (- n 1)))))

(define (distribute-operation oper s ss)
  (cond ((failed-union? s)
         (let ((failed-shapes (failed-operation-shapes s)))
           (union
            (map (lambda (u ss)
                   (oper (cons u ss)))
                 failed-shapes
                 (copy-shapes-n ss (length failed-shapes))))))
        ((failed-subtraction? s)
         (let ((failed-shapes (failed-operation-shapes s)))
           (subtraction
            (map (lambda (u ss)
                   (oper (cons u ss)))
                 failed-shapes
                 (copy-shapes-n ss (length failed-shapes))))))
        ((failed-subtraction? (car ss))
         (let ((failed-shapes (failed-operation-shapes (car ss))))
           (oper (map subtraction
                      (copy-shapes-n s (length failed-shapes))
                      failed-shapes)
                 (cdr ss))))
        (else
         (error "To be treated"))))


(define (pure-intersection ss)
  (if (null? ss)
      (universal-shape)
      (if (null? (cdr ss))
          (car ss)
          (let loop ((curr (car ss)) (rest (cdr ss)))
            (cond ((null? rest)
                   curr)
                  ((empty-shape? curr)
                   (delete-shapes rest)
                   curr)
                  ;; ((half-space? (car rest))
                  ;;  (loop (slice curr (car rest))
                  ;;        (cdr rest)))
                  (else
                   (loop (pure-intersection2 curr (car rest))
                         (cdr rest))))))))

(def-shape-op (pure-intersection2 sh0 sh1)
  (let-shapes ((r0 sh0) (r1 sh1))
    (new-shape 'intersection autocad (ac:boolean-intersection r0 r1))))

(provide subtraction)
(define (subtraction . shapes-tree)
  (let ((ss (flatten shapes-tree)))
    (if (null? ss)
        (empty-shape) ;(error "No shapes to subtract")
        (let ((s (car ss))
              (ss (filter-not empty-shape? (cdr ss))))
          (cond ((null? ss)
                 s)
                ((empty-shape? s)
                 s)
                ((ormap (lambda (o)
                          (or (universal-shape? o)
                              (eq? s o))) ;;Perhaps we should use a equal-shape? test
                        ss)
                 (delete-shapes (cons s ss))
                 (empty-shape))
                ((failed-union? s)
                 (let ((failed (failed-operation-shapes s)))
                   (union (map (lambda (u s)
                                 (subtraction (cons u s)))
                               failed
                               (copy-shapes-n ss (length failed))))))
                ((failed-subtraction? s)
                 (let ((ss1 (failed-operation-shapes s)))
                   (subtraction (car ss1)
                                (union (cdr ss1) ss))))
                (else
                 (let-values ([(failed ss) (partition failed-operation? ss)])
                   (let-values ([(failed-unions failed-subtractions) (partition failed-union? failed)])
                     (let ((s (pure-subtraction
                               (cons s (append (append-map failed-operation-shapes failed-unions) ss)))))
                       (cond ((null? failed-subtractions)
                              s)
                             ((empty-shape? s)
                              (delete-shapes failed-subtractions)
                              s)
                             (else
                              (distribute-operation
                               subtraction
                               s
                               (cdr failed)))))))))))))

#;(define (debug tag . val)
  (display tag) (displayln val) 
  (car val))

(define (pure-subtraction ss)
  (if (null? ss)
      (empty-shape)
      (if (null? (cdr ss))
          (car ss)
          ;;First, for just two shapes
          (if (null? (cddr ss))
              (pure-subtraction2 (car ss) (cadr ss))
              (let loop ((curr (car ss)) (rest (cdr ss)))
                (if (null? rest)
                    curr
                    (let ((r (pure-subtraction2 curr (car rest))))
                      (cond ((empty-shape? r)
                             (delete-shapes (cdr rest))
                             (empty-shape))
                            (else
                             (loop r (cdr rest)))))))))))

(def-shape-op (pure-subtraction2 sh0 sh1)
   (let-shapes ((r0 sh0) (r1 sh1))
     (mark-deleted! sh0 sh1)
     (new-shape 'subtraction autocad (ac:boolean-subtraction r0 r1))))

(def-shape-op (resolve shape)
   (cond ((failed-union? shape)
          (let ((shapes (failed-operation-shapes shape)))
            (if (andmap curve? shapes)
                (ac:join-curves (map shape-ref shapes))
                (let ((ss (map shape-impl (failed-operation-shapes shape))))
                  (foldl ac:boolean-union (car ss) (cdr ss))))))
         (else
          (shape-ref shape))))

(def-shape-op (cross-intersection shapes0 shapes1)
   (let ((shapes0 (collect-all-united-shapes shapes0))
         (shapes1 (collect-all-united-shapes shapes1)))
     (cond ((and (singleton? shapes0) (singleton? shapes1))
            (intersection (append shapes0 shapes1)))
           ((singleton? shapes0)
            (begin0
              (union (map (lambda (s1)
                            (intersection (copy-shape (car shapes0)) s1))
                          shapes1))
              (delete-shapes shapes0)))
           ((singleton? shapes1)
            (begin0
              (union (map (lambda (s0)
                            (intersection s0 (copy-shape (car shapes1))))
                          shapes0))
              (delete-shapes shapes1)))
           (else
            (begin0
              (union (map (lambda (s0)
                            (cross-intersection s0 (copy-shapes shapes1)))
                          shapes0))
              (delete-shapes shapes1))))))

(def-shape-op (contains? shape p)
  (rhino
   ;;HACK is this right?
   (rh:point-in-planar-closed-curve p (shape-impl shape))))

(def-shape-op (closed-curve-contains? shape p plane)
  (rhino
   (> (rh:point-in-planar-closed-curve p (shape-impl shape) plane)
      0)))


(def-shape-op (transform shape matrix)
  (rhino
   (rh:transform-object (shape-impl shape) matrix)))

(def-shape-op (closest-point shape p)
  (rhino
   (if (curve? shape)
       (rh:curve-closest-point (shape-impl p))
       (rh:brep-closest-point (shape-impl shape) p))))

(def-shape-op (surface-frame shape u v)
  (rhino
   (rh:surface-frame (shape-impl shape) (vector u v))))

(def-shape-op (surface-normal shape u v)
  (rhino
   (rh:surface-normal (shape-impl shape) (vector u v))))
 
(def-shape-op (closest-point-uv shape p)
  (rhino
   (rh:brep-closest-uv (shape-impl shape) p)))

(def-shape-op (closest-point-t shape p)
  (rhino
   (rh:curve-closest-point (shape-impl shape) p)))

(def-new-shape-op (extract-u-edge shape u v)
  (rhino
   (rh:singleton-id (rh:extract-iso-curve (shape-impl shape) (vector u v) 0))))

(def-shape-op (curve-tangent shape t)
  (rhino
   (second (rh:curve-evaluate (shape-impl shape) t 1))))


;;Compatibility operations

(define (view [camera #f] [target #f] [lens #f])
  (autocad
   (cond ((and camera target lens)
          ;;(ac:set-view camera target lens)
          (ac:view-conceptual)
          (ac:perspective 1)
          (ac:dview-zoom-command camera target lens (distance camera target)))
         (else
          (ac:get-view))))
  (rhino
   (cond ((and camera target lens)
          (unless (rh:is-view-maximized "Perspective")
            (rh:maximize-restore-view "Perspective"))
          (rh:view-projection "Perspective" 2) ;;perspective
          (rh:view-camera-lens "Perspective" lens)
          (rh:view-camera-target "Perspective" camera target)
          (rh:view-display-mode "Perspective" 2)) ;;render
         (else
          (rh:current-view "Perspective")
          (let ((camera (rh:view-camera))
                (target (rh:view-target))
                (lens (rh:view-camera-lens)))
            (values (xyz (loc-x camera) (loc-y camera) (loc-z camera))
                    (xyz (loc-x target) (loc-y target) (loc-z target))
                    lens)))))
  (opengl
   (cond ((and camera target lens)
          (gl:set-view camera target lens))
         (else
          (error "Unfinished!")))))

(provide view-expression)
(define (view-expression)
  (let-values (((c t l) (view)))
    `(view (xyz ,(cx c) ,(cy c) ,(cz c))
           (xyz ,(cx t) ,(cy t) ,(cz t))
           ,l)))

(define (2d-top)
  (rhino
   (unless (rh:is-view-maximized "Top")
     (rh:maximize-restore-view "Top"))
   (rh:view-projection "Top" 1) ;;parallel
   (rh:view-display-mode "Top" 0)) ;;wireframe
  (autocad
   (ac:view-wireframe)
   (ac:view-top))
  (opengl
   (gl:set-view-top))
  (tikz
   ;;Top is the default in TikZ
   #t))

(define (zoom-extents)
  (rhino
   (rh:zoom-extents rh:com-omit #t))
  (autocad
   (ac:zoom-extents))
  (opengl
   (gl:zoom-extents))
  (tikz
   ;;TikZ zooms automatically
   #t))

(define (refresh)
  (opengl
   (gl:refresh))
  (rhino
   #t)
  (autocad
   #t)
  (tikz
   #t))

;;Renders and Films
(provide render-dir render-backend-dir render-kind-dir render-color-dir render-ext)

;;There is a render directory
(define render-dir (make-parameter (find-system-path 'home-dir)))
;;with a backend-specific subdirectory
(define render-backend-dir (make-parameter 'same))
;;and with subdirectories for static images, movies, etc
(define render-kind-dir (make-parameter "Render"))
;;and with subdirectories for white, black, and colored renders
(define render-color-dir (make-parameter 'same))
;;containing files with different extensions
(define render-ext (make-parameter "png"))

(define (render-pathname name)
  (simplify-path
   (build-path (render-dir)
               (render-backend-dir)
               (render-kind-dir)
               (render-color-dir)
               (format "~A.~A" name (render-ext)))
   #f))

(define render-height (make-parameter 300))
(define render-width (make-parameter 300))
(define render-floor-width (make-parameter 1000))
(define render-floor-height (make-parameter 1000))

#;(define film-dir (make-parameter #f))

(provide set-render-dir)
(define (set-render-dir val)
  (render-dir (path->directory-path (build-path val))))

(provide render-size)
(define (render-size width heigth)
  (render-width width)
  (render-height heigth))

(render-size 1024 768)

(provide white-renders)
(define (white-renders)
  (create-layers)
  (render-dir (build-path "C:\\Users\\aml" "Dropbox" "Renders"))
  (render-size 1920 1080)
  (shapes-layer dark-gray-layer)
  (floor-layer white-floor-layer)
  (render-color-dir "White")
  (current-layer (shapes-layer)))

(provide black-renders)
(define (black-renders)
  (create-layers)
  (render-dir (build-path "C:\\Users\\aml" "Dropbox" "Renders"))
  (render-size 1920 1080)
  (shapes-layer dark-gray-layer)
  (floor-layer black-floor-layer)
  (render-color-dir "Black")
  (current-layer (shapes-layer)))

(define floor-distance (make-parameter 0))

(define floor-extra-width (make-parameter 2000))

(define floor-extra-factor (make-parameter 20))

(define last-make-floor-for-bounding-box #f)

(provide make-floor-for-bounding-box)
(define (make-floor-for-bounding-box bb)
  (set!	last-make-floor-for-bounding-box bb)
  (let ((p0 (bbox-min bb))
	(p1 (bbox-max bb)))
    (let ((w (max (* (floor-extra-factor)
                     (distance p0 p1))
                  (floor-extra-width))))
      (with-current-layer (floor-layer)
        #;(let ((c (*c (+c p0 (xyz (cx p1) (cy p1) (cz p0))) 1/2))
              (h (* (- (cz p1) (cz p0)) 10)))
          (cylinder (+z c (- (floor-distance) 1)) w 1)
          (subtraction (cylinder c w h)
                       (cylinder c (- w 1) h)))
        
        (box
         (xyz (- (min (loc-x p0) (loc-x p1)) w)
              (- (min (loc-y p0) (loc-y p1)) w)
              (- (loc-z p0) 1 (floor-distance)))
         (xyz (+ (max (loc-x p0) (loc-x p1)) w)
              (+ (max (loc-y p0) (loc-y p1)) w)
              (- (loc-z p0) 0 (floor-distance))))))))

(provide make-floor)
(define (make-floor)
  (make-floor-for-bounding-box (all-shapes-bounding-box)))

(provide make-background)
(define (make-background c t f w h d)
  (let ((wp (/ (* w d) f))
        (hp (/ (* h d) f)))
    (let ((dp (*c (norm-c (-c t c)) d)))
      #;#;#;
      (sphere c 1)
      (sphere t 4)
      (sphere (+c c dp) 2)
      (let ((p (loc-from-normal (+c c dp) (-c c t)))
            (l (* 2 (max wp hp))))
        (with-current-layer (floor-layer)
          #;(cylinder p (min wp hp) 1)
          (box (+xy p (/ l -2) (/ l -2))
               l
               l
               -0.001))))))

(provide view-with-background)
(define (view-with-background camera target lens)
  (let ((bb (all-shapes-bounding-box)))
    (make-floor-for-bounding-box bb)
    (when (eq? (floor-layer) black-floor-layer) ;;Add a background
      (let ((farthest-p
             (argmax (lambda (p) (distance camera p))
                     (bbox-corners bb))))
        (make-background camera target lens 50 40 (distance camera farthest-p)))))
  (view camera target lens))


;;Bounding box
(define (combine-bb bb0 bb1)
  (bbox (xyz (min (loc-x (bbox-min bb0)) (loc-x (bbox-min bb1)))
	     (min (loc-y (bbox-min bb0)) (loc-y (bbox-min bb1)))
	     (min (loc-z (bbox-min bb0)) (loc-z (bbox-min bb1))))
	(xyz (max (loc-x (bbox-max bb0)) (loc-x (bbox-max bb1)))
	     (max (loc-y (bbox-max bb0)) (loc-y (bbox-max bb1)))
	     (max (loc-z (bbox-max bb0)) (loc-z (bbox-max bb1))))))

(define (shapes-bounding-box objs)
  (let ((bb (bounding-box (car objs))))
    (for ((obj (cdr objs)))
      (set! bb (combine-bb bb (bounding-box obj))))
    bb))

(define (points-bounding-box pts)
  (let ((bb (bbox (car pts) (car pts))))
    (for ((pt (cdr pts)))
      (set! bb (combine-bb bb (bbox pt pt))))
    bb))

(provide all-shapes-bounding-box)
(define (all-shapes-bounding-box)
  (shapes-bounding-box
    (all-shapes)))

(define (prepare-for-file path)
  (make-directory* (path-only path))
  (when (file-exists? path)
    (delete-file path))
  path)

(define (render-view name)
  (autocad
   (ac:skystatus ac:sky-status-background-and-illumination)
     ;; (shapes-layer (all-shapes) (shapes-layer))
     ;; (shape-layer (make-floor) (floor-layer))
   (ac:render-command "P" 
                      (render-width)
                      (render-height)
                      (prepare-for-file (render-pathname name))))
  (rhino
   ;(rh:create-preview-image path "Perspective")
   (rh:render-resolution (list (render-width) (render-height)))
   (rh:render-view (prepare-for-file (render-pathname name))))
  (tikz
   (%save-tikz
    (prepare-for-file
     (parameterize ((render-ext "tikz"))
       (render-pathname name))))))

(define (render-stereo-view name)
  (autocad
   #t
   ;; (match (get-view)
   ;;   ((list camera target lens)
   ;;    (let ((direction (-c target camera))
   ;;          (l (distance camera target)))
   ;;      (let ((d (/ (if (< l 1) (/ l 10.0) 0.1) 2.0)))
   ;;        (let ((d+ (pol d (+ (pol-phi direction) pi/2)))
   ;;              (d- (pol d (- (pol-phi direction) pi/2))))
   ;;          (view (+c camera d+) (if (< l 1) target (+c target d+)) lens)
   ;;          (render-view (strcat filename "L"))
   ;;          (view (+c camera d-) (if (< l 1) target (+c target d-)) lens)
   ;;          (render-view (strcat filename "R")))))))
   ))

(provide film-filename)
(provide film-frame)
(define film-filename (make-parameter #f))
(define film-frame (make-parameter #f))

(provide start-film)
(define (start-film name)
  (film-filename name)
  (film-frame 0))


(define (frame-filename filename i)
  (~a filename "-frame-" (~r i #:min-width 3  #:pad-string "0")))

(provide save-film-frame)
(define (save-film-frame [obj #t])
  (parameterize ((render-kind-dir "Film"))
    (render-view (frame-filename (film-filename) (film-frame)))
  (film-frame (+ (film-frame) 1))
  obj))

(provide saving-film-frame)
(define-syntax-rule (saving-film-frame expr)
  (begin0
    expr
    (save-film-frame)))

#|
Utilities: think about moving them to a different file
|#

(define (prompt-point [str "Select position"])
  (autocad
   (ac:get-point (u0 world-cs) str))
  (rhino
   (rh:get-point str)))

(define (prompt-integer [str "Integer?"])
  (autocad
   (ac:get-integer str))
  (rhino
   (rh:get-integer str)))

(define (prompt-real [str "Real?"])
  (autocad
   (ac:get-real str))
  (rhino
   (rh:get-real str)))

(define (prompt-shape [str "Select shape"])
  (autocad
   (ac:shape<-ref
    (ac:get-entity str)))
  (rhino
   (rh:shape<-ref
    (rh:get-object str))))

;;TODO: Update this to follow the approach taken for autocad
(define (rh:shape<-ref r)
  (new-shape
   (cond ((rh:is-point r) 'point)
         ((rh:is-circle r) 'circle)
         ((rh:is-curve r) 
          (if (rh:is-curve-closed r) 'closed-spline 'spline))
         ((or (rh:is-line r)
              (rh:is-polyline r))
          (if (rh:is-curve-closed r) 'closed-line 'line))
         ((and (rh:is-object r) (rh:is-object-solid r)) 'solid)
         ((rh:is-surface r) 'surface)
         ((rh:is-polysurface r) 'surface)
         ((rh:is-mesh r) 'mesh)
         (else
          (error 'rh:shape<-ref "Unknown Rhino object ~A" r)))
    rhino r))

;;HACK: To be finished
(define (gl:shape<-ref r)
  (new-shape
   (cond ;((gl:is-point r) 'point)
         ;((rh:is-circle r) 'circle)
         ;((rh:is-curve r) 'spline)
         ;((rh:is-line r) 'line)
         ;((rh:is-polyline r) 'line)
         ;((and (rh:is-object r) (rh:is-object-solid r)) 'solid)
         ;((rh:is-surface r) 'surface)
         ;((rh:is-polysurface r) 'surface)
         (else
          'solid #;
          (error 'gl:shape<-ref "Unknown OpenGL object ~A" r)))
    opengl r))


(define (insert-objects-from filename [predicate #f])
  (autocad
   (displayln filename)
   (let ((doc (ac:open-dbx-doc filename)))
     (let ((objs (ac:all-objects (ac:modelspace doc))))
       (let ((filtered-objs (if predicate (filter predicate objs) objs)))
         (ac:copy-objects doc filtered-objs))))))

(def-new-shape (conic-helix p0 r0 p1 r1 turns)
  (autocad
   (ac:conic-helix p0 r0 p1 r1 turns)))

(define (save-screen-png filename)
  (autocad
   (ac:save-screen-png filename)))

(define (save-screen-eps filename)
  (autocad
   (ac:save-screen-eps filename)))

;(require racket/trace)
;(trace union mirror copy-shape copy-shapes map-failed-operation resolve shape-ref shape-impl)
;; (trace slice slice* mirror move rotate scale map-failed-operation)
;(trace shape-impl failed-operation? union pure-union pure-union2 failed-union intersection subtraction pure-subtraction pure-subtraction2 distribute-operation fully-contained? fully-not-contained? partially-contained? delete-shape delete-shapes surface? point?)
;(trace loft-curve-point)
;(trace guided-loft guided-loft*)


(define (pause)
  (autocad
   (read-char))
  (rhino
   (read-char))
  (opengl
   (read-char)))

(define (disable-update)
  (rhino
   (rh:enable-redraw #f)))

(define (enable-update)
  (rhino
   (rh:enable-redraw #t)))

(define (show-backend)
  (rhino
   #t #;(bring-to-front (rh:window-handle))))

(def-new-shape (contour s p spacing)
  (rhino
   (rh:add-srf-contour-crvs (shape-impl s) p spacing)))

(provide erase-2d-top)
(define (erase-2d-top) 
  (delete-all-shapes)
  (2d-top))

(provide zoom-2d-top)
(define (zoom-2d-top)
  (2d-top)
  (zoom-extents))

(provide zoom-3d-conceptual)
(define (zoom-3d-conceptual)
  "to be implemented"
  )

(provide create-wireframe-model)
(define (create-wireframe-model [shapes (all-shapes)])
  (autocad
   (begin0
     (map ac:shape<-ref (ac:explode (ac:flatshot-command (map shape-impl shapes))))
     (delete-shapes shapes)
     (zoom-2d-top))))


;;Introspection

(def-shape-op (curve-start-point c)
  (rhino
   (rh:curve-start-point (shape-impl c)))
  (autocad
   (ac:curve-start-point (shape-impl c))))

(def-shape-op (curve-end-point c)
  (rhino
   (rh:curve-end-point (shape-impl c)))
  (autocad
   (ac:curve-end-point (shape-impl c))))

(def-shape-op (curve-closest-point c p)
  (rhino
   (rh:curve-closest-point (shape-impl c) p)))

;(require racket/trace)
;(trace fully-contained? rh:fully-contained?)
;;(trace intersection cross-subtraction)

;;Utilities

(provide rotate-current-view)
(define (rotate-current-view dphi)
  (let-values (((camera target lens) (view)))
    (rotated-view dphi (bbox-center (all-shapes-bounding-box)) camera target lens)))

(provide rotated-view)
(define (rotated-view dphi p camera target lens)
  (let ((v0 (-c camera p))
        (v1 (-c target p)))
    (view (+sph p (sph-rho v0) (+ (sph-phi v0) dphi) (sph-psi v0))
          (+sph p (sph-rho v1) (+ (sph-phi v1) dphi) (sph-psi v1))
          lens)))

(provide set-centered-view)
(define (set-centered-view rho phi psi lens)
  (let* ((bb (all-shapes-bounding-box))
         (target (bbox-center bb))
         (camera (+sph target rho phi psi)))
    (view camera target lens)))

(define (enumerate-param spec)
  (define (enumerate a b n)
    (if (< a b)
        (division a b n)
        (reverse (division b a n))))
  (if (null? (cdr spec))
      (list (car spec))
      (append (enumerate (car spec) (cadr spec) (caddr spec))
              (cdr (enumerate-param (cons (cadr spec) (cdddr spec)))))))

;;To make movies
;;(film "sergelsChange" ((e 2.5 3.5 10 1.5 20)) ...(f ... e ...) ...)

(provide film)
(define-syntax-rule
  (film name ((param init next ...) ...) body ...)
  (let ((param init) ...)
    (start-film name)
    body ...
    (save-film-frame)
    (for ((e (cdr (enumerate-param (list init next ...)))))
      (set! param e)
      body ...
      (retrying (save-film-frame)))
    ...))

(define (new-shape [name : Symbol])
  (shape name (incr)))

(provide start-backend)
(define (start-backend backend)
  ;;Don't need to do anything
  #t)

(def-current-backend-op (delete-all-shapes)
  ;;Nothing to do here
  #t)


(def-shape (point position)
  (rhino
   (rh:add-point position))
  (autocad
   (ac:add-point position))
  (opengl
   (gl:add-point position)))

(def-shape-op (point-position p)
  (delayed
   (delayed-point-position p))
  (rhino
   (let-shapes ((p p))
     (rh:point-coordinates p)))
  (autocad
   (let-shapes ((p p))
     (car (ac:coordinates p))))
  (opengl
   (let-shapes ((p p))
     (gl:point-coordinates p))))



;; arc

(define (arc-morph c r beg-a a point circle)
  (cond ((= r 0)
         (point c))
        ((= a 0)
         (point (+pol c r beg-a)))
        ((>= (abs a) 2pi)
         (circle c r))
        (else
         #f)))

;;HACK the morph case doesn't seem to be correct because it doesn't respect the coordinate system of c
;;It also seems to prevent the use of primitives (e.g. gl:add-circle) that always create shapes at the
;;origin
(def-new-shape (arc [c (u0)] [r 1] [beg-a 0] [a pi])
  (rhino
   (or (arc-morph c r beg-a a rh:add-point rh:add-circle)
       (rh:add-arc
        (if (= beg-a 0)
            c
            (rh:rotate-plane c (radians->degrees beg-a) (uz)))
        r
        (radians->degrees (coterminal a)))))
  (autocad
   (let ((end-a (+ beg-a a)))
     (or (arc-morph c r beg-a a ac:add-point ac:add-circle)
         (ac:transform-from
          (if (> end-a beg-a)
              (ac:add-arc (u0 world-cs) r beg-a end-a)
              (ac:add-arc (u0 world-cs) r end-a beg-a))
          c))))
  (opengl
   (let ((end-a (+ beg-a a)))
     (or (arc-morph c r beg-a a gl:add-point gl:add-circle)
         (gl:transform-from
          (if (> end-a beg-a)
              (gl:add-arc (u0 world-cs) r beg-a end-a)
              (gl:add-arc (u0 world-cs) r end-a beg-a))
          c))))
  (tikz
   (let ((end-a (+ beg-a a)))
     (or (arc-morph c r beg-a a %point %circle)
         (begin
           #;(%transform c)
           (if (> end-a beg-a)
               (%arc c r beg-a end-a)
               (%arc c r end-a beg-a))
           #;(%end-transform))))))


(define (surface-arc-morph c r beg-a a point line surface-circle)
  (cond ((= r 0)
         (point c))
        ((= a 0)
         (line c (+pol c r beg-a)))
        ((>= a 2pi)
         (surface-circle c r))
        (else
         #f)))

(def-new-shape (surface-arc [c (u0)] [r 1] [beg-a 0] [end-a pi])
  (rhino
   (let ((a (- beg-a end-a)))
     (or (surface-arc-morph c r beg-a a rh:add-point rh:add-line rh:add-surface-circle)
         (let ((curves
                (list (rh:add-arc
                       (if (= beg-a 0)
                           c
                           (rh:rotate-plane c (radians->degrees beg-a) (uz)))
                       r
                       (radians->degrees (coterminal a)))
                      (rh:add-line c (+pol c r beg-a))
                      (rh:add-line c (+pol c r end-a)))))
           (begin0
               (rh:singleton-id (rh:add-planar-srf curves))
             (rh:delete-objects curves))))))
  (autocad
   (or (arc-morph c r beg-a (- beg-a end-a) ac:add-point ac:add-surface-circle)
       (let ((curves
              (list (ac:transform-from
                     (if (> end-a beg-a)
                         (ac:add-arc (u0 world-cs) r beg-a end-a)
                         (ac:add-arc (u0 world-cs) r end-a beg-a))
                     c)
                    (ac:add-line c (+pol c r beg-a))
                    (ac:add-line c (+pol c r end-a)))))
         (begin0
           (car (ac:add-region curves))
           (for ((c (in-list curves))) (ac:delete c))))))
  (opengl
   (or (arc-morph c r beg-a (- beg-a end-a) gl:add-point gl:add-surface-circle)
       (gl:transform-from
        (if (> end-a beg-a)
            (gl:add-surface-arc (u0 world-cs) r beg-a end-a)
            (gl:add-surface-arc (u0 world-cs) r end-a beg-a))
        c))))

(define (ellipse-morph c xr yr)
  (cond ((= xr yr 0)
         (point c))
        ((= xr 0)
         (line (+y c (- yr)) (+y c yr)))
        ((= yr 0)
         (line (+x c (- xr)) (+x c xr)))
        (else
         #f)))

(def-new-shape (ellipse [c (u0)] [xr 1] [yr 1/2])
  (rhino
   (or (ellipse-morph c xr yr)
       (rh:add-ellipse c xr yr)))
  (autocad
   (or (ellipse-morph c xr yr)
       (ac:transform-from
        (if (> xr yr)
            (ac:add-ellipse (u0 world-cs) (xyz xr 0 0) (/ yr xr))
            (ac:add-ellipse (u0 world-cs) (xyz 0 yr 0) (/ xr yr)))
        c))))

(def-new-shape* (line cs)
  (rhino
   (rh:add-polyline cs))
  (autocad
   (ac:add-3d-poly cs))
  (opengl
   (gl:add-line #f cs))
  (tikz
   (%line cs)))

(def-shape-op (line-vertices l)
  (rhino
   (let-shapes ((r l))
     (let ((pts (rh:curve-points r)))
       (if (or (rh:is-curve-closed r)
               (< (distance (car pts) (last pts)) 1.0e-15)) ;Rhino tolerance
           (drop-right pts 1)
           pts))))
  (autocad
   (let-shapes ((r l))
     (let ((pts
            (cond ((ac:line? r)
                   (list (ac:start-point r) (ac:end-point r)))
                  ((ac:lightweight-polyline? r) ;;This is not right, we need to convert coordinates
                   (let ((h (ac:elevation r)))
                     (map (lambda (p) (+z p h)) (ac:2d-coordinates r))))
                  ((ac:3d-polyline? r)
                   (ac:coordinates r))
                  (else
                   (error 'line-vertices "Can't compute vertices of ~A" l)))))
       (if (or (ac:closed r)
               (< (distance (car pts) (last pts)) 1.0e-015)) ;AutoCAD tolerance
           (drop-right pts 1)
           pts)))))
 
; predicates

(define (angle? a)
  (number? a))

(define (distance? d)
  (and (number? d) (positive? d)))

(define (nonnegative-number? r)
  (and (number? r) (not (negative? r))))

(define (nonnegative-integer? r)
  (and (integer? r) (not (negative? r))))

(define (coord-list? lst)
  (and (list? lst) (every is-coord lst)))

(define (node? expr)
  (is-a? expr node%))

;HACK should this be applicable to empty-shapes? It doesn't seem possible
(def-shape-op (bounding-box s)
  (rhino
   (let ((ss (collect-all-shapes s)))
     (apply bbox (rh:bounding-box-corners (map shape-impl ss)))))
  (autocad
   (let ((ss (collect-all-shapes s)))
     (let ((bbs (map (lambda (s) (apply bbox (ac:bounding-box (shape-impl s)))) ss)))
       (foldl combine-bb (car bbs) (cdr bbs)))))
  (opengl
   (let ((ss (collect-all-shapes s)))
     (let ((bbs (map (lambda (s) (apply bbox (gl:bounding-box (shape-impl s)))) ss)))
       (foldl combine-bb (car bbs) (cdr bbs))))))

(def-new-shape-op (closed shape)
  (else
   (begin0
       (error 'closed "Finish this. Dispatch on shape type and create closed shape"))))

(def-new-shape* (closed-line cs)   
  (rhino
   (let ((id (rh:add-polyline (append cs (list (car cs))))))
     ;; (cond ((rh:is-curve-closed id)
     ;;        id)
     ;;       ((rh:is-curve-closable id)
     ;;        (rh:close-curve id))
     ;;       (else
     ;;         (delete-object id)
     ;;         (add-polyline (append-first coords)))))
     id))
  (autocad
   (let ((com (ac:add-3d-poly (append cs (list (car cs))))))
     (ac:closed com #t)
     com))
  (opengl
   (gl:add-line #t (map as-world cs)))
  (tikz
   (%closed-line (map as-world cs))))


(def-new-shape* (spline cs [v0 #f] [v1 #f])
  (rhino
   (rh:add-interp-curve-ex
    cs
    3
    rh:knot-style-chord-length-spacing
    #t
    (or v0 rh:com-omit)
    (or v1 rh:com-omit)))
  (autocad
   ;;HACK: apparently, there's no difference
   ;(ac:spline-command cs v0 v1)
   (let ((v0 (or v0 (-c (cadr cs) (car cs))))
         (v1 (or v1 
                 (let ((end (take-right cs 2)))
                   (-c (cadr end) (car end))))))
     (ac:add-spline cs v0 v1)))
  (opengl
   (gl:add-spline #f cs v0 v1))
  (tikz
   (%spline (map as-world cs))))

(def-new-shape* (closed-spline cs [v0 #f] [v1 #f])
  (rhino
   (rh:add-interp-curve-ex
    (append cs (list (car cs)))
    3
    rh:knot-style-chord-length-spacing
    #f
    (or v0 rh:com-omit)
    (or v1 rh:com-omit)))
  (autocad

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

   
   (let ((cs (append cs (list (car cs)))))
     (let ((v0* (or v0 (-c (cadr cs) (car cs))))
           (v1* (or v1 
                    (let ((end (take-right cs 2)))
                      (-c (cadr end) (car end))))))
       (let ((v (/c (+c v0* v1*) 2)))
         (let ((v0 (or v0 v #;(pol (pol-rho v0*) (pol-phi v))))
               (v1 (or v1 v #;(pol (pol-rho v1*) (pol-phi v)))))
           (ac:add-line (car cs) (+c (car cs) v0))
           (ac:add-line (car cs) (+c (car cs) v1))
           (let ((sp (ac:add-spline (append cs (list (car cs))) v0 v1)))
             ;   (ac:closed sp)  ;;HACK SHOULDN'T WE CLOSE THIS?
             sp))))))
  (opengl
   (gl:add-spline #t cs v0 v1))
  (tikz
   (%closed-spline (map as-world cs))))


(def-new-shape* (surface-polygon cs)
  (rhino
   (if (null? (cddddr cs))
       (rh:add-srf-pt cs)
       (let ((id (rh:add-polyline (append cs (list (car cs))))))
         (begin0
             (rh:singleton-id (rh:add-planar-srf id))
           (rh:delete-object id)))))
  (autocad
   (let ((com (ac:add-3d-poly (append cs (list (car cs))))))
     (ac:closed com #t)
     (begin0
         (car (ac:add-region com))
       (ac:delete com))))
  (opengl
   (gl:add-surface-from-points 
    (map as-world (append cs (list (car cs))))))
  (tikz
   (%closed-line (map as-world cs) #t)))

(def-new-shape (regular-polygon edges [c (u0)] [r 1] [a 0] [inscribed? #f])
  (else
   (if (= r 0)
       (point c)
       (polygon (regular-polygon-vertices edges c r a inscribed?)))))

(def-new-shape (surface-regular-polygon edges [c (u0)] [r 1] [a 0] [inscribed? #f])
  (opengl
   (gl:add-surface-from-points-pivot
    (regular-polygon-vertices edges c r a inscribed?)
    c))
  (else
   (surface (regular-polygon edges c r a inscribed?))))

; solids

(def-new-shape (sphere [c (u0)] [r 1])
  (autocad
   (ac:add-sphere c r))
  (rhino
   (rh:add-sphere c r))
  (opengl
   (gl:add-sphere c r)))

(def-shape-op (sphere-center s)
  (delayed
   (delayed-sphere-c s))
#;  (rhino ;Doesn't exist
   (rh:sphere-center s)))

(def-shape-op (sphere-radius s)
  (delayed
   (delayed-sphere-r s))
#;  (rhino ;Doesn't exist
   (rh:sphere-radius s)))


(def-new-shape (torus [c (u0)] [re 1] [ri 1/2])
  (rhino
   (rh:add-torus2 c re ri))
  (autocad
   (ac:transform-from (ac:add-torus (u0 world-cs) re ri) c))
  (opengl
   (gl:transform-from (gl:add-torus (u0 world-cs) re ri) c)))


(def-new-shape (regular-pyramid-frustum [edges 4] [cb (u0)] [rb 1] [a 0] [h/ct 1] [rt 1] [inscribed? #f])
  (opengl
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c (max rb rt) h
                      point
                      (lambda (c r) (surface-regular-polygon edges c r a inscribed?))
                      line)
         (gl:transform (if (= rt 0)
                           (gl:add-pyramid (u0 world-cs) rb h a edges)
                           (gl:add-pyramid-frustum (u0 world-cs) rb rt h a edges))
                       (tr-matrix (position-cs (as-origin c)))))))   
  (else
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c (max rb rt) h
                      point
                      (lambda (c r) (surface-regular-polygon edges c r a inscribed?))
                      line)
         (loft ;;don't use loft-curves because one of the polygons might be degenerate
          (list (regular-polygon edges c rb a inscribed?)
                (regular-polygon edges (+z c h) rt a inscribed?))
          #t
          #t)))))

(def-new-shape (regular-pyramid [edges 4] [cb (u0)] [rb 1] [a 0] [h/ct 1] [inscribed? #f])
  ((rhino autocad)
   (regular-pyramid-frustum edges cb rb a h/ct 0 inscribed?))
  (opengl
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c rb h gl:add-point gl:add-circle gl:add-line2) ;;HACK circle? WRONG!
         (gl:transform (gl:add-pyramid (u0 world-cs) rb h a edges)
                       (tr-matrix (position-cs (as-origin c))))))))

(def-new-shape (irregular-pyramid pts ct [solid? #t])
  (else
   (loft-curve-point (polygon pts) (point ct) solid?)))

(def-new-shape (regular-prism edges [cb (u0)] [r 1] [a 0] [h/ct 1])
  (else
   (regular-pyramid-frustum edges cb r a h/ct r)))

(def-new-shape (irregular-prism cbs [h/ct 1] [solid? #t])
  (else
   (let ((v (if (number? h/ct) (z h/ct) h/ct)))
     (loft-curves
      (list (polygon cbs)
            (polygon (map (lambda (p) (+c p v)) cbs)))
      #t
      solid?))))

(def-new-shape (right-cuboid [cb (u0)] [width 1] [height 1] [h/ct 1])
  (else
   (let-values ([(c h) (position-and-height cb h/ct)])
     (irregular-prism
      (list (+xy c (/ width -2) (/ height -2))
            (+xy c (/ width +2) (/ height -2))
            (+xy c (/ width +2) (/ height +2))
            (+xy c (/ width -2) (/ height +2)))
      h))))

Using right-cuboid2 in AutoCAD speeds the escada-n-caracol
example from
cpu time: 1763 real time: 11930 gc time: 109
to
cpu time: 327 real time: 614 gc time: 0

or from 
cpu time: 13088 real time: 154478 gc time: 1138
to
cpu time: 2355 real time: 5130 gc time: 327

a 20~30x speedup

(def-new-shape (right-cuboid [cb (u0)] [width 1] [height 1] [h/ct 1])
  (rhino
   ;;HACK Improve this
   (let-values ([(c h) (position-and-height cb h/ct)])
     (irregular-prism
      (list (+xy c (/ width -2) (/ height -2))
            (+xy c (/ width +2) (/ height -2))
            (+xy c (/ width +2) (/ height +2))
            (+xy c (/ width -2) (/ height +2)))
      h))
   #;(let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c r h rh:add-point rh:add-circle rh:add-line)
         (rh:add-cylinder-from-plane c h r))))
  (autocad
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or ; finish this (axial-morph c r h ac:add-point ac:add-circle ac:add-line)
         (ac:transform (ac:add-box (+z (u0 world-cs) (/ h 2.0)) width height h)
                       (tr-matrix (position-cs (as-origin c)))))))
  (opengl
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or ; finish this (axial-morph c r h ac:add-point ac:add-circle ac:add-line)
         (gl:transform (gl:add-box (+z (u0 world-cs) (/ h 2.0)) width height h)
                       (tr-matrix (position-cs (as-origin c))))))))

; box

(define (box-morph c dx dy dz point line rectangle)
  (cond ((= dx dy dz 0)
         (point c))
        ((= dx dy 0)
         (line c (+z c dz)))
        ((= dx dz 0)
         (line c (+y c dy)))
        ((= dy dz 0)
         (line c (+x c dx)))
         ; ((= dx 0) (rectangle (line (u0 world-cs) (y dy)) (line (u0 world-cs) (z dz))) ; edit: not possible (yet?)
         ; ((= dy 0) (rectangle (line (u0 world-cs) (x dx)) (line (u0 world-cs) (z dz))) ; edit: not possible (yet?)
         ; ((= dz 0) (rectangle (line (u0 world-cs) (x dx)) (line (u0 world-cs) (y dy))) ; edit: not possible (yet?)
        (else
         #f)))

(define (box-deltas c dx/c1 dy dz)
  (if (number? dx/c1)
      (values dx/c1 dy dz)
      (let ((d (-c dx/c1 c)))
        (values (loc-x d) (loc-y d) (loc-z d)))))

(def-new-shape (box [c (u0)] [dx/c1 1] [dy (if (number? dx/c1) dx/c1 1)] [dz dy])
  (rhino
   (let-values ([(dx dy dz) (box-deltas c dx/c1 dy dz)])
     (or (box-morph c dx dy dz rh:add-point rh:add-line #f)
         (rh:add-box
          (list c
                (+x c dx)
                (+xy c dx dy)
                (+y c dy)
                (+z c dz)
                (+xz c dx dz)
                (+xyz c dx dy dz)
                (+yz c dy dz))))))
  (autocad
   (let-values ([(dx dy dz) (box-deltas c dx/c1 dy dz)])
     (or (box-morph c dx dy dz ac:add-point ac:add-line #f)
         (ac:transform-from (ac:add-box (xyz (/ dx 2) (/ dy 2) (/ dz 2))
                                        (abs dx) (abs dy) (abs dz))
                            c))))
  (opengl
   (let-values ([(dx dy dz) (box-deltas c dx/c1 dy dz)])
     (or (box-morph c dx dy dz gl:add-point gl:add-line2 #f)
         (gl:transform-from (gl:add-box (xyz (/ dx 2) (/ dy 2) (/ dz 2))
                                        (abs dx) (abs dy) (abs dz))
                            c)))))

(def-new-shape (cuboid b0 b1 b2 b3 t0 t1 t2 t3)
  (rhino
   (rh:add-box (list b0 b1 b2 b3 t0 t1 t2 t3)))
  (autocad
   (let ((pm (ac:add-polyface-mesh 
              (list b0 b1 b2 b3 t0 t1 t2 t3) 
              (vector 1 4 3 2
                      5 6 7 8 
                      1 2 6 5 
                      2 3 7 6 
                      3 4 8 7
                      1 5 8 4))))
     (let ((m (ac:mesh-smooth pm)))
       (ac:delete pm)
       (begin0
         (ac:conv-to-solid m)
         (ac:delete m))))))

;; cone

(define (axial-morph c r l point circle line)
  (cond ((and (= l 0) (= r 0))
         (point c))
        ((= l 0)
         (circle c r))
        ((= r 0)
         (line c (+z c l)))
        (else
         #f)))

(def-new-shape (cone [cb (u0)] [r 1] [h/ct 1])
  (rhino
   (let-values ([(c h) (inverted-position-and-height cb h/ct)])
     (or (axial-morph cb r h rh:add-point rh:add-circle rh:add-line)
         (rh:add-cone-from-plane c h r))))
  (autocad
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c r h ac:add-point ac:add-circle ac:add-line)
         (ac:transform (ac:add-cone (+z (u0 world-cs) (/ h 2.0)) r h)
                       (tr-matrix (position-cs (as-origin c)))))))
  (opengl
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c r h gl:add-point gl:add-circle gl:add-line2)
         (gl:transform (gl:add-cone (u0 world-cs) r h)
                       (tr-matrix (position-cs (as-origin c))))))))

; cone-frustum

(def-new-shape (cone-frustum [cb (u0)] [rb 1] [h/ct 1] [rt 1])
  (rhino
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c (max rb rt) h rh:add-point rh:add-circle rh:add-line)
         (let ((circs (list (rh:add-circle c rb) (rh:add-circle (+z c h) rt))))
           (let ((srf (rh:add-loft-srf circs rh:com-omit rh:com-omit rh:loft-type-straight)))
             (rh:capped-planar-holes srf)
             (rh:delete-objects circs)
             srf)))))
  (autocad
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c (max rb rt) h ac:add-point ac:add-circle ac:add-line)
         (ac:transform (ac:add-cone-frustum (u0 world-cs) rb rt h)
                       (tr-matrix (position-cs (as-origin c)))))))
  (opengl
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c (max rb rt) h gl:add-point gl:add-circle gl:add-line2)
         (gl:transform (gl:add-cone-frustum (u0 world-cs) rb rt h)
                       (tr-matrix (position-cs (as-origin c))))))))

; cylinder

(def-new-shape (cylinder [cb (u0)] [r 1] [h/ct 1])
  (rhino
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c r h rh:add-point rh:add-circle rh:add-line)
         (rh:add-cylinder-from-plane c h r))))
  (autocad
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c r h ac:add-point ac:add-circle ac:add-line)
         (ac:transform (ac:add-cylinder (+z (u0 world-cs) (/ h 2.0)) r h)
                       (tr-matrix (position-cs (as-origin c)))))))
  (opengl
   (let-values ([(c h) (position-and-height cb h/ct)])
     (or (axial-morph c r h gl:add-point gl:add-circle gl:add-line2)
         (gl:transform (gl:add-cylinder (u0 world-cs) r h)
                       (tr-matrix (position-cs (as-origin c))))))))


;; Predicates

; transformations
(define (contains-pivot? s)
  (or (circle? s)
      (arc? s))) ;;HACK what else?

#;
(def-new-shape-op (extrusion profile [dir 1])
  (rhino
   (begin0
       (rh:extrude (shape-impl profile)
                   (if (number? dir) (z dir) dir))
     (delete-shape profile)))
  (autocad
   (begin0
       (ac:singleton-or-union
        (if (number? dir)
            (ac:extrude-command-length (shape-impl profile) dir (surface-region? profile))
            (ac:extrude-command-direction (shape-impl profile) (u0 world-cs) dir (surface-region? profile))))
       ;; (if (number? dir)
       ;;     (ac:add-extruded-solid (shape-impl profile) dir)
       ;;     (let ((path (ac:add-line (u0 world-cs) dir)))
       ;;       (begin0
       ;;           (ac:add-extruded-solid-along-path (shape-impl profile) path)
       ;;         (ac:delete path))))
     (delete-shape profile)))
  (opengl
   (begin0
       (gl:add-extrusion (shape-impl profile)
                         (if (number? dir) (z dir) dir)
                         (surface-region? profile)
                         (contains-pivot? profile)
                         (smooth-curve? profile))
     (delete-shape profile))))

(provide extrusion)
(define (extrusion profile [dir 1])
  (map-failed-operation
   (lambda (profile)
     (if (empty-shape? profile)
         (error "Boing") ;(empty-shape)
         (new-shape
          'extrusion
          (shape-backend profile)
          (case-shape-backend
           profile extrusion
           (rhino
            (begin0
              (rh:extrude (shape-impl profile)
                          (if (number? dir) (z dir) dir))
              (delete-shape profile)))
           (autocad
            (begin0
              (ac:singleton-or-union
               ;;When dir is a number, there is an AutoCAD bug 
               ;;that causes some profiles to be extruded in 
               ;;the wrong direction,
               (if (number? dir)
                   #;(ac:extrude-command-length (shape-impl profile) dir (surface-region? profile))
                   (ac:extrude-command-direction (shape-impl profile) (u0 world-cs) (z dir) (surface-region? profile))
                   (ac:extrude-command-direction (shape-impl profile) (u0 world-cs) dir (surface-region? profile))))
              ;; (if (number? dir)
             ;;     (ac:add-extruded-solid (shape-impl profile) dir)
              ;;     (let ((path (ac:add-line (u0 world-cs) dir)))
              ;;       (begin0
              ;;           (ac:add-extruded-solid-along-path (shape-impl profile) path)
              ;;         (ac:delete path))))
              (delete-shape profile)))
           (opengl
            (begin0
              (gl:add-extrusion (shape-impl profile)
                                (if (number? dir) (z dir) dir)
                                (surface-region? profile)
                                (contains-pivot? profile)
                                (smooth-curve? profile))
              (delete-shape profile)))))))
     profile))

(provide move)
(define (move shape v)
  (map-failed-operation
   (lambda (s)
     (if (empty-shape? s)
         s
         (case-shape-backend
          s move
          (rhino
           (rh:move-objects (shape-ref s) v)
           s)
          (autocad
           (ac:move (shape-ref s) (u0 world-cs) v)
           s)
          (opengl
           (gl:move (shape-ref s) v)
           s))))
   shape))

(provide mirror)
(define (mirror shape [p (u0)] [n (uz)] [copy? #t])
  (let ((p (loc-from-normal p n)))
    (map-failed-operation
     (lambda (s)
       (case-shape-backend
        s mirror
        (rhino
         (if copy?
             (new-shape (shape-name s)
                        rhino
                        (rh:mirror-object (shape-impl s) p (+x p 1) #t))
             (begin
               (rh:mirror-object (shape-impl s) p (+x p 1) #f)
               s)))
        (autocad
         (if copy?
             (new-shape (shape-name s)
                        autocad
                        (ac:mirror3d (shape-impl s) p (+x p 1) (+y p 1)))
             (begin0
               (new-shape (shape-name s)
                          autocad
                          (ac:mirror3d (shape-impl s) p (+x p 1) (+y p 1)))
               (delete-shape s))))))
     shape)))

(provide scale)
(define (scale shape s [p (u0)])
  (map-failed-operation
   (lambda (sh)
     (case-shape-backend
      sh scale
      (autocad
       (ac:scale-entity (shape-ref sh) p s)
       sh)
      (rhino
       ;;HACK must change the active construction plane to the perspective?????
       (rh:scale-object (shape-ref sh) p (xyz s s s) #f)
       sh)
      (opengl
       (gl:move
        (gl:scale 
         (gl:move (shape-ref sh) (*c p -1)) 
         s s s)
        p)
       sh)))
   shape))

(provide rotate)
(define (rotate shape a [p0 (u0)] [p1 (+z p0 1)])
  (map-failed-operation
   (lambda (s)
     (case-shape-backend
      s rotate
      (autocad
       (ac:rotate3d (shape-ref s) p0 p1 a)
       s)
      (rhino
       (rh:rotate-object (shape-ref s) p0 (radians->degrees (coterminal a)) p1 #f)
       s)))
   shape))

(def-new-shape-op (offset shape distance)
  (autocad
   (begin0
     (let-shapes ((r shape))
       (cond ((ac:acceptable-surface? r)
              (ac:offset-surface r distance))
             ((ac:3d-polyline? r)
              (let ((2d-r (ac:2dpoly<-3dpoly (shape-impl shape))))
                (begin0
                  (singleton-value (ac:offset 2d-r distance))
                  (ac:delete 2d-r))))
             (else
              (singleton-value (ac:offset r distance)))))
     (delete-shape shape)))
  (rhino
   (begin0
     (rh:offset-surface (shape-impl shape) distance)
     (delete-shape shape))))

(def-new-shape-op (offset-curve shape direction distance normal)
  (rhino
   (begin0
     (rh:singleton-id (rh:offset-curve (shape-impl shape) direction distance normal))
     (delete-shape shape))))


(def-new-shape-op (revolve shape [p0 (u0)] [p1 (+z p0 1)] [a0 0] [a 2pi])
  (rhino
   (begin0
       (rh:revolve (shape-impl shape) p0 p1 a0 (+ a0 a))
     (delete-shape shape)))
  (autocad
   (begin0
       (ac:revolve-command (shape-impl shape) p0 p1 a0 (+ a0 a) (surface-region? shape))
     (delete-shape shape))))
|#

(define (thicken [surf : Any] [h : Real 1])
  (displayln "This must be finished!")
  (void))

#|
;;Use something like singleton-or-union

(provide section)
(define (section s p)
  (map-failed-operation
   (lambda (s)
     (case-shape-backend
      s section
      (rhino
       (let ((curves (rh:add-srf-section-crvs (shape-impl s) p)))
         (if (null? curves)
             (empty-shape)
             (begin0
               (failed-union
                (map rh:shape<-ref (rh:add-planar-srfs curves))
                #;(foldl append 
                         (list)
                         (map (lambda (c) (map rh:shape<-ref (rh:add-planar-srfs c)))
                              curves)))
               (rh:delete-objects curves)
               #;(delete-shape s)))))))
   s))

(provide copy-shapes)
(define (copy-shapes ss [translation #f])
  (map-failed-operation
   (lambda (s)
     (if (or (empty-shape? s) (universal-shape? s))
         s
         (case-shape-backend 
          s copy-shapes
          (delayed
           s)
          (rhino 
           (new-shape
            (shape-name s)
            rhino
            (if translation
                (rh:copy-object (shape-ref s) translation)
                (rh:copy-object (shape-ref s)))))
          (autocad
           (new-shape
            (shape-name s)
            autocad
            (let ((r (ac:copy (shape-ref s))))
              (when translation
                (ac:move r (u0 world-cs) translation))
              r))))))
   ss))

(provide copy-shape)
(define copy-shape copy-shapes)

(provide delete-shapes)
(define (delete-shapes ss)
  (let ((shapes (collect-all-shapes ss)))
    (if (null? shapes)
        #t
        (begin
          (mark-deleted! shapes)
          (case-shape-backend 
           shapes delete-shapes
           (rhino
            (rh:delete-objects (map shape-ref shapes)))
           (autocad
            (for ((s (in-list shapes)))
              (let ((r (shape-ref s)))
                (if (list? r) ;;Special case for surface-grid
                    (for ((s (in-list r)))
                      (ac:delete s))
                    (ac:delete r)))))
           (opengl
            (for ((s (in-list shapes)))
              (gl:erase-actor (shape-ref s)))))))))

(provide delete-shape)
(define delete-shape delete-shapes)

(def-new-shape-op (surface-boundary shape)
  (rhino
   (let-shapes ((r shape))
     (begin0
         ;;HACK to be completed for the case of multiple
         ;;borders. Probably, return a failed union of curves
         (rh:singleton-id (rh:duplicate-surface-border r))
       (delete-shape shape))))
  (autocad
   (let-shapes ((r shape))
     (let ((rs (ac:explode r)))
       (cond ((null? rs)
              (error 'surface-boundary "Can't compute boundary of ~A" shape))
             ((null? (cdr rs))
              (delete-shape shape)
              (car rs))
             ((andmap ac:line? rs)
              (let ((poly (ac:add-3d-poly (ac:closed-lines-points rs))))
                (ac:closed poly #t)
                (for ((s (in-list rs))) (ac:delete s))
                poly))
             (else
              (delete-shape shape)
              (ac:join-curves rs)))))))

(def-shape-op (shape-centroid shape)
  (rhino
   (car (rh:surface-volume-centroid (shape-impl shape)))))

(def-shape-op (shape-normal shape p)
  (rhino
   (let-shapes ((s shape))
     (rh:surface-normal s p))))

(def-current-backend-op (create-layer name)
  (autocad
   (ac:add-layer name)
   name)
  (rhino
   (rh:add-layer name)
   name)
  (opengl
   ;;HACK what should we do??
   name)
  (tikz
   ;;HACK what should we do??
   name))

(def-shape-op (shape-layer shape [new-layer #f])
  (autocad
   (if new-layer
       (begin
         (ac:layer (shape-impl shape) new-layer)
         shape)
       (ac:layer (shape-impl shape))))
  (rhino
   (if new-layer
       (begin
         (rh:add-layer new-layer)
         (rh:object-layer (shape-impl shape) new-layer)
         shape)
       (rh:object-layer (shape-impl shape)))))

(provide hack-shapes-layer)
(define (hack-shapes-layer ss layer)
  (map-failed-operation
   (lambda (s)
     (if (or (empty-shape? s) (universal-shape? s))
         s
         (shape-layer s layer)))
   ss))

(def-shape-op (shape-material shape [new-material #f])
  (autocad
   (if new-material
       (begin
         (ac:add-material new-material)
         (ac:material (shape-impl shape) new-material)
         shape)
       (ac:material (shape-impl shape))))
  #;(rhino
   (if new-material
       (begin
         (rh:object-material (shape-impl shape) new-material)
         shape)
       (rh:object-material (shape-impl shape)))))

;; (def-shape-op (shapes-layer shapes [new-layer #f])
;;   (autocad
;;    (if new-layer
;;        (begin
;;          (ac:add-layer new-layer)
;;          (for ((shape (in-list shapes)))
;;            (ac:layer (shape-impl shape) new-layer)))
;;        (remove-duplicates (map (lambda (s) (ac:layer (shape-impl s))) shapes)))))

(def-current-backend-op (current-layer [new-layer #f])
  (autocad
   (if new-layer
       (ac:clayer new-layer)
       (ac:clayer)))
  (rhino
   (if new-layer
       (rh:current-layer new-layer)
       (rh:current-layer)))
  (opengl
   ;;HACK This must be improved
   #f))

(def-current-backend-op (layer new-layer)
  (autocad
   (ac:add-layer new-layer)
   (ac:clayer new-layer))
  (rhino
   (rh:add-layer new-layer)
   (rh:current-layer new-layer)))


(provide with-current-layer)
(define-syntax (with-current-layer stx)
  (syntax-case stx ()
    ((_ new-layer body ...)
     (syntax/loc stx
       (let ((old-layer (current-layer)))
         (dynamic-wind
           (lambda () (layer new-layer))
           (lambda () body ...)
           (lambda () (layer old-layer))))))))

(def-new-shape-op* (join-curves shapes)
  (rhino
   (begin0
       (rh:join-curves (map shape-impl shapes) #t)
     (delete-shapes shapes)))
  (autocad
   (begin0
       (ac:join-curves (map shape-impl shapes))
     (mark-deleted! shapes)))
  (opengl
   (begin0
       (gl:join-curves (map shape-impl shapes))
     (delete-shapes shapes))))

(def-new-shape-op* (join-surfaces shapes)
  (rhino
   (begin0
       (rh:join-surfaces (map shape-impl shapes) #f)
     (delete-shapes shapes)))
  (autocad
   (begin0
       (ac:join-command (map shape-impl shapes))
     (delete-shapes shapes))))


(def-new-shape-op (fillet shape0 shape1 [radius 0])
  (autocad
   (let-shapes ((r0 shape0) (r1 shape1))
     (begin0
       (ac:join-curves
        (list r0
              (ac:fillet-command r0 r1 radius)
              r1))
       (mark-deleted! shape0 shape1)))))

(def-new-shape-op* (inner-solid shapes)
  (autocad
   (let ((ss 
          (map (lambda (shape) 
                 (ac:as-surface (shape-impl shape)))
               shapes)))
     (begin0
       (ac:interior-solid ss)
       (delete-shapes shapes))))
  (rhino
   (let ((ss (map shape-impl shapes)))
     (begin0
       (rh:create-solid ss)
       (delete-shapes shapes)))))

(def-new-shape-op* (solid shapes)
  (autocad
   (let ((ss 
          (map (lambda (shape) 
                 (ac:as-surface (shape-impl shape)))
               shapes)))
     (begin0
       (ac:interior-solid ss)
       (delete-shapes shapes))))
  (rhino
   (let ((ss (map shape-impl shapes)))
     (begin0
       (rh:create-solid ss #t) ;;HACK rh:join-surfaces is not enough for Market Hall.
       (mark-deleted! shapes) #;(delete-shapes shapes)))))

(provide select-shapes)
(def-shape-op (select-shapes s)
  (rhino
   (rh:select-objects (collect-all-shapes s))))

(provide select-shape)
(define select-shape select-shapes)

(define (split-brep id cutter)
  (begin0
    (match (cons id cutter)
      ((cons (string-id str) (string-id cutter-str))
       (map make-string-id (com:split-brep str cutter-str #t))))
    (delete-object cutter)))

; lines


(define (nurbs-curve controls knots)
  (define (uniform-knots? knots) #t) ;;AML: must be finished
  (if (uniform-knots? knots)
      (add-curve controls (min 3 (- (length controls) 1)))
      (error "Rhino curve does not support non uniform knots")))


; surface

(define (generate-knot-vector order total)
  (let ((knots (make-vector (+ total order))))
    (let iter ((i 0))
      (if (< i order)
          (begin
            (vector-set! knots i 0.0)
            (iter (+ i 1)))
          (let iter ((j 1) (i i))
            (if (<= j (- total order))
                (begin
                  (vector-set! knots i (/ j (+ (- total order) 1.0)))
                  (iter (+ j 1) (+ i 1)))
                (let iter ((j 0) (i i))
                  (if (< j order)
                      (begin 
                        (vector-set! knots i 1.0)
                        (iter (+ j 1) (+ i 1)))
                      knots))))))))

(define (nurbs-surface controls u-knots v-knots)
  (define (degree-from-n-controls n)
    (cond ((< n 3)
           1)
          ((< n 6)
           3)
          (else
           5)))
  (let ((n-rows (length controls))
        (n-cols (length (car controls))))
    (let ((u-degree (degree-from-n-controls n-rows))
          (v-degree (degree-from-n-controls n-cols)))
      (make-string-id
       (com:add-nurbs-surface 
        (list n-rows n-cols)
        controls
        (generate-knot-vector (- u-degree 1) n-rows)
        (generate-knot-vector (- v-degree 1) n-cols)
        (list u-degree v-degree)
        #;(map (lambda (row) (map (lambda (col) 1.0) row)) controls))))))

(def-new-shape-op* (surface shapes)
  (rhino
   (if (singleton? shapes)
       (let-shapes ((r (car shapes)))
         (if (rh:is-point r)
             r
             (begin0
                 (rh:singleton-id (rh:add-planar-srf r))
               (delete-shapes shapes))))
       (begin0
           (rh:add-edge-srf (map shape-impl shapes))
         (delete-shapes shapes))))
  (autocad
   (if (singleton? shapes)
       (let-shapes ((r (car shapes)))
         (if (ac:point? r)
             r
             (begin0
                 (car (ac:add-region r))
               (delete-shapes shapes))))
       ;;AddRegion only accepts Line, Arc, Circle, Elliptical Arc, LightweightPolyline, Spline.
       (let ((cs (ac:convert-3dpolylines (map shape-impl shapes))))
         (mark-deleted! shapes)
         (begin0
           (ac:singleton-or-union
            (ac:add-region cs))
           (map ac:delete cs)))))
  (opengl ;;HACK Unfinished (e.g. point, but this should be handled on the abstract level)
   (if (singleton? shapes)
       (let ((shape (car shapes)))
         (begin0
             (gl:add-surface-from-curve (shape-impl shape))
           (delete-shapes shapes)))
       (begin0
           (gl:add-surface-from-curves (map shape-impl shapes))
         (delete-shapes shapes)))))

#;#;
(provide planar-surface)
(define planar-surface surface)

(def-new-shape-op* (planar-surface shapes)
  (rhino
   (begin0
     (rh:add-planar-srf (map shape-impl shapes))
     (delete-shapes shapes))))
  
(def-new-shape (surface-grid css [closed-u? #f] [closed-v? #f])
  (rhino
   ;;Rhino requires at least 3 points in each dimension
   (if (and (null? (cddr css))
            (null? (cddr (car css))))
       (rh:add-srf-pt (append (car css) (cadr css)))
       (let ((css
              (cond ((null? (cddr css))
                     (list (car css)
                           (map (lambda (p0 p1) (/c (+c p0 p1) 2)) (car css) (cadr css))
                           (cadr css)))
                    ((null? (cddr (car css)))
                     (map (lambda (cs)
                            (/c (+c (car cs) (cadr cs)) 2))
                          css))
                    (else
                     css))))
         (rh:add-srf-pt-grid
          (vector (length css) (length (car css)))
          (foldr append (list) css)
          rh:com-omit
          (vector closed-u? closed-v?)))))
  (autocad
   #;(loft-curves (map (if closed-v? closed-spline spline) css) #f #f closed-u?)
   ;;HACK: This must be seriously improved!!!
   (let ((nu (length css))
         (nv (length (car css))))
     (unless (and (<= nu 256) (<= nv 256))
       (error "Too many elements (more than 256x256)"))
     (define (maybe-singleton l)
       (if (null? (cdr l))
           (car l)
           l))
     (maybe-singleton
      (cond ((> nu 256)
             (append
              (shape-ref
               (surface-grid (take css 256) #f closed-v?))
              (shape-ref
               (surface-grid (drop css 255) #f closed-v?))))
            ((> nv 256)
             (append
              (shape-ref
               (surface-grid (map (lambda (cs) (take cs 256)) css) closed-u? #f))
              (shape-ref
               (surface-grid (map (lambda (cs) (drop cs 255)) css) closed-u? #f))))
            (else
             (let ((r 
                    (ac:add-3d-mesh
                     (if closed-u? (+ nu 1) nu)
                     (if closed-v? (+ nv 1) nv)
                     (foldr append 
                            (list)
                            (let ((css (if closed-v?
                                           (map (lambda (l) (append l (list (car l))))
                                                css)
                                           css)))
                              (if closed-u?
                                  (append css (list (car css)))
                                  css))))))
               (when closed-u?
                 (ac:m-close r #t))
               (when closed-v?
                 (ac:n-close r #t))
               ;(ac:type r ac:ac-bezier-surface-mesh) BUG??
               (list r))))))
  #;(for ((pts0 (in-list css))
         (pts1 (in-list (cdr css))))
     (for ((pt0 (in-list pts0))
           (pt1 (in-list (cdr pts0)))
           (pt2 (in-list (cdr pts1)))
           (pt3 (in-list pts1)))
       (ac:add-3d-face pt0 pt1 pt2 pt3))))
  (opengl
   (gl:add-grid-surface
    (map (lambda (pts)
           (map as-world pts))
         css)
    closed-u? #t
    closed-v? #t)))

(def-new-shape (mesh-grid css [closed-u? #f] [closed-v? #f])
  (rhino
   (rh:add-mesh
    css
    (let ((rows (length css))
          (cols (length (car css))))
      (for*/vector ([row0 (in-range 0 (- rows 1))]
                    [col0 (in-range 0 (- cols 1))])
        (let ((row1 (+ row0 1))
              (col1 (+ col0 1)))
          (let ((c0 (+ (* row0 cols) col0))
                (c1 (+ (* row0 cols) col1))
                (c2 (+ (* row1 cols) col1))
                (c3 (+ (* row1 cols) col0)))
            (vector c0 c1 c2 c3))))))))

(def-shape-op (mesh-faces s)
  (rhino
   (let loop ((l (rh:mesh-faces (shape-impl s) #t))) ;;only triangles
     (if (null? l)
         (list)
         (cons (take l 4)
               (loop (drop l 4)))))))

(def-shape-op (mesh-normals s)
  (rhino
   (rh:mesh-face-normals (shape-impl s))))


(def-new-shape-op (mesh-offset s d)
  (rhino
   (rh:mesh-offset (shape-impl s) d)))


; transformations

; sweep

(define (surface-region? s)
  (or (surface? s) 
      (surface-circle? s)
      (surface-arc? s)
      (surface-rectangle? s)
      (surface-polygon? s)
      ;;ask the backend
      (case-shape-backend 
       s surface-region?
       (autocad
        (ac:region? (shape-impl s)))
       (rhino
        (or (rh:is-surface (shape-impl s))
            (rh:is-polysurface (shape-impl s)))))))

(define (smooth-curve? s)
  (or (spline? s) (closed-spline? s) (circle? s))) ;;HACK ellipse?

(define (closed-curve? s)
  (or (closed-line? s) (closed-spline? s)))

(def-new-shape-op (sweep path profile [rotation 0] [scale 1])
  (rhino
   (unless (= scale 1) (error "Rhino doesn't handle scale in sweep"))
   (unless (= rotation 0) (error "Rhino doesn't handle rotation in sweep"))
   (begin0
       (rh:sweep (shape-impl path) (shape-impl profile))
     (delete-shapes (list profile path))))
  (autocad
   (begin0
       (ac:singleton-or-union
        (ac:sweep-command (shape-impl profile) #t (shape-impl path) (surface-region? profile) rotation scale))
     (delete-shapes (list profile path))))
  (opengl
   (unless (= scale 1) (error "OpenGL doesn't handle scale in sweep"))
   (unless (= rotation 0) (error "OpenGL doesn't handle rotation in sweep"))
   (begin0
       (gl:add-sweep (shape-impl profile) 
                     (shape-impl path) 
                     #f #;(smooth-curve? path)
                     #f #;(surface-region? profile))
     (delete-shapes (list profile path)))))


; loft
(define (curve? s)
  (or (line? s)
      (closed-line? s)
      (spline? s)
      (closed-spline? s)
      (circle? s)
      (arc? s)))


;Compatibility operations

(def-current-backend-op (view [camera #f] [target #f] [lens #f])
  (autocad
   (cond ((and camera target lens)
          ;;(ac:set-view camera target lens)
          (ac:view-conceptual)
          (ac:perspective 1)
          (ac:dview-zoom-command camera target lens (distance camera target)))
         (else
          (ac:get-view))))
  (rhino
   (cond ((and camera target lens)
          (unless (rh:is-view-maximized "Perspective")
            (rh:maximize-restore-view "Perspective"))
          (rh:view-projection "Perspective" 2) ;;perspective
          (rh:view-camera-lens "Perspective" lens)
          (rh:view-camera-target "Perspective" camera target)
          (rh:view-display-mode "Perspective" 2)) ;;render
         (else
          (rh:current-view "Perspective")
          (let ((camera (rh:view-camera))
                (target (rh:view-target))
                (lens (rh:view-camera-lens)))
            (values (xyz (loc-x camera) (loc-y camera) (loc-z camera))
                    (xyz (loc-x target) (loc-y target) (loc-z target))
                    lens)))))
  (opengl
   (cond ((and camera target lens)
          (gl:set-view camera target lens))
         (else
          (error "Unfinished!")))))

(provide view-expression)
(define (view-expression)
  (let-values (((c t l) (view)))
    `(view (xyz ,(cx c) ,(cy c) ,(cz c))
           (xyz ,(cx t) ,(cy t) ,(cz t))
           ,l)))

(def-current-backend-op (2d-top)
  (rhino
   (unless (rh:is-view-maximized "Top")
     (rh:maximize-restore-view "Top"))
   (rh:view-projection "Top" 1) ;;parallel
   (rh:view-display-mode "Top" 0)) ;;wireframe
  (autocad
   (ac:view-wireframe)
   (ac:view-top))
  (opengl
   (gl:set-view-top))
  (tikz
   ;;Top is the default in TikZ
   #t))

(def-current-backend-op (zoom-extents)
  (rhino
   (rh:zoom-extents rh:com-omit #t))
  (autocad
   (ac:zoom-extents))
  (opengl
   (gl:zoom-extents))
  (tikz
   ;;TikZ zooms automatically
   #t))

(def-current-backend-op (refresh)
  (opengl
   (gl:refresh))
  (rhino
   #t)
  (autocad
   #t)
  (tikz
   #t))
|#
