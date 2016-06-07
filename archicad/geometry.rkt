#lang racket
(provide (except-out (all-defined-out)))

(require "protobuf1/protobuf.rkt")
(require "protobuf1/encoding.rkt")
(require "messages.rkt")
(require "communication.rkt")
(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/connection.rkt")
(require srfi/26)
;(require math/array)
(require math/matrix)
;(require htdp/matrix)

(define (rotate-p-x p a)
  (xyz (cx p)
       (- (* (cy p) (cos a))
          (* (cz p) (sin a)))
       (+ (* (cy p) (sin a))
          (* (cz p) (cos a)))))

(define (rotate-p-y p a)
  (xyz (+ (* (cx p) (cos a))
          (* (cz p) (sin a)))
       (cy p)
       (+ (* (cx p) (- (sin a)))
          (* (cz p) (cos a)))))

(define (rotate-p-z p a)
  (xyz (- (* (cx p) (cos a))
          (* (cy p) (sin a)))
       (+ (* (cx p) (sin a))
          (* (cy p) (cos a)))
       (cz p)))

#|
Function used to create a circle
 
 p: center of circle
 radius: radius of circle
 
 returns: circle id

Example of usage: 
(send (circle (xy 0 0) 1))
|#
(define (circle p radius)
  (send/rcv-id "Circle"
               (circlemessage* #:p0x (cx p)
                               #:p0y (cy p)
                               #:radius radius)))

#| 
Function used to create an arc

Examples of usage:
create an arc with center (0,0), radius 1,
 with an amplitude of 90o, on the 1o quadrant (x+, y+)
(send (arc (xy 0 0) 1 0 0 (* 90 DEGRAD)))

by using begang instead of endang, we get the opposite result,
the arc will be the opposite of the previous result. 
(send (arc (xy 0 0) 1 0 (* 90 DEGRAD) 0))

|#
(define (arc p radius angle begang endang)
  (send/rcv-id "Arc" (arcmessage* #:p0x (cx p)
                                  #:p0y (cy p)
                                  #:radius radius
                                  #:angle angle
                                  #:begang begang
                                  #:endang endang)))
;Line is not used, instead we use a poly-lines for line
#;(define (line p0 p1)
  (write-msg "Line" (linemsg* #:pts (prepare-points-to-send (list p0 p1))))
  (read-guid))

(define (line pts [arcs (list)])
  (write-msg "PolyLine" (polylinemsg* #:pts (prepare-points-to-send pts)
                                      #:arcs (prepare-arcs-to-send arcs)))
  (read-guid))

(define (spline pts [closed? #f])
  (write-msg "Spline" (splinemsg* #:points (prepare-points-to-send pts)
                                  #:closed closed?))
  (read-guid))

;;Auxiliar Function
(define (get-sub-polys sub-poly-list)
  (cond 
   [(null? sub-poly-list) (list)]
   [(list? (car sub-poly-list)) (append (list (length (car sub-poly-list))) (get-sub-polys (cdr sub-poly-list))) ]
   [else (list (length sub-poly-list))]))

#| 
Function used to create a sphere

Example of usage: 
(send (sphere (xy 0 0) 1))
|#
(define (sphere p radius #:level [level (current-level)])
  (send/rcv-id "Sphere" (spheremessage* #:c0x (cx p)
                                        #:c0y (cy p)
                                        #:c0z (cz p)
                                        #:radius radius
                                        #:level (storyinfo-index level))))

#|
Function to create a Complex Shell
Example of usage: (complex-shell tmat lstpoints lstarcs 1 hpoints harcs hheight htmat 1.0 0.0)
|#
(define (complex-shell transmat listpoints listarcs numholes holepoints holearcs holeheight holetransmat reflectx reflecty)
  (let ((shell-to-create (shellcomplexmessage* #:numpoints (length listpoints)
                                               #:numarcs (length listarcs)
                                               #:numholes numholes
                                               #:numhpoints (length holepoints)
                                               #:numharcs (length holearcs)
                                               #:holeheight holeheight
                                               #:reflectx reflectx
                                               #:reflecty reflecty)))
    (write-msg "ComplexShell" shell-to-create)
    (for-each send-double transmat)
    (send-list-points listpoints)
    (let ((output (connection-out (bim-connection))))
      (for-each (lambda (arc)
                  (write-sized serialize (polyarcmessage* #:begindex (car arc) #:endindex (car (cdr arc)) #:arcangle (car (cdr (cdr arc)))) output)) 
                listarcs)
      (send-list-points holepoints)
      (for-each (lambda (arc)
                  (write-sized serialize (polyarcmessage* #:begindex (car arc) #:endindex (car (cdr arc)) #:arcangle (car (cdr (cdr arc)))) output)) 
                holearcs)
      (for-each send-double holetransmat)
      (read-guid))))

#|
Function to create a Simple Shell
Example of usage: (simple-shell lstpoints)
|#
(define (simple-shell listpoints)
  (shell listpoints (list)))

#|
Function to create a Shell
This is the most primitive shell we can create 
Example of usage: (shell lstpoints lstarcs)
|#
(define (shell listpoints listarcs)
  (send/no-rcv "Shell" (shellmessage* #:numpoints (length listpoints)
                                      #:numarcs (length listarcs)))
  (send-points listpoints)
  (send-arcs-complex listarcs)
  (read-guid))

#|(send (rev-shell (list (x 0)(x 5)(xy 5 5)(xy 0 5)(x 0))
                   #:revolution-angle (- pi)))
|#
(define default-shell-type-of-material (make-parameter "Basic"))
(define default-shell-thickness (make-parameter 0.3))
;TODO Give access to rev-axis-base
(define (rev-shell pts
                   [arcs (list)]
                   #:level [level (current-level)]
                   #:flipped? [flipped? #f]
                   #:slant-angle [slant-angle 0]
                   #:revolution-angle [revolution-angle 2pi]
                   #:distortion-angle [distortion-angle pi/2]
                   #:begin-angle [begin-angle 0]
                   #:axis [axis (list 0 0 0 0
                                      0 0 0 0
                                      0 0 0 0)]
                   #:type-of-material [type-of-material (default-shell-type-of-material)]
                   #:material [material (cond [(eq? type-of-material "Basic") "GENERIC - STRUCTURAL"]
                                              [(eq? type-of-material "Composite") "Generic Wall/Shell"])]
                   #:thickness [thickness (default-shell-thickness)])
  (send/rcv-id "RevShell" (revshellmsg* #:pts (prepare-points-to-send pts)
                                        #:arcs (prepare-arcs-to-send arcs)
                                        #:level (storyinfo-index level)
                                        #:flipped (not flipped?)
                                        #:slantangle slant-angle
                                        #:revangle revolution-angle
                                        #:distortionangle distortion-angle
                                        #:begangle begin-angle
                                        #:axis axis
                                        #:material material
                                        #:type type-of-material
                                        #:thickness thickness)))
#|
(send (ext-shell (list (x 0)(x 5)(xy 5 5)(xy 0 5))
                   (xyz 3 3 3)
                   #:extrusion-center (xy 10 10)))
|#
(define (ext-shell pts
                   extrusion-vector
                   [arcs (list)]
                   #:level [level (current-level)]
                   #:flipped? [flipped? #f]
                   #:extrusion-center [extrusion-center (u0)]
                   #:visible-edges (visible-edges (list))
                   #:type-of-material [type-of-material (default-shell-type-of-material)]
                   #:material [material (cond [(eq? type-of-material "Basic") "GENERIC - STRUCTURAL"]
                                              [(eq? type-of-material "Composite") "Generic Wall/Shell"])]
                   #:thickness [thickness (default-shell-thickness)])
  (send/rcv-id "ExtShell" (extshellmsg* #:pts (prepare-points-to-send (close-guide pts (car pts)))
                                        #:arcs (prepare-arcs-to-send arcs)
                                        #:level (storyinfo-index level)
                                        #:flipped (not flipped?)
                                        #:cextx (cx extrusion-center)
                                        #:cexty (cy extrusion-center)
                                        #:extx (cx extrusion-vector)
                                        #:exty (cy extrusion-vector)
                                        #:extz (cz extrusion-vector)
                                        #:visible visible-edges
                                        #:material material
                                        #:type type-of-material
                                        #:thickness thickness)))

#|
Function to rotate a Shell
Receives the axis in which the shell will rotate, the angle and the shellId
Example of usage: (rotate-shell "x" 90 shellId)
|#
(define (rotate-shell axis angle shellId)
  (send/rcv-id "RotateShell" (rotshellmessage* #:axis axis
                                               #:angle angle
                                               #:guid shellId)))

#|
Function to translate a Shell
Receives a point that represents the translation and the shell ID
Example of usage: (translate-shell (list 0 5 0) shellId)
|#
(define (translate-shell point shellId)
  (send/rcv-id "TranslateShell" (tshellmessage* #:tx (car point)
                                                #:ty (car (cdr point))
                                                #:tz (car (cdr (cdr point)))
                                                #:guid shellId)))

#|
Function to create a hole on a Shell
Receives a point that represents the translation and the shell ID
Example of usage: (hole-on-shell hpoints harcs hheight shellId)
|#
(define (hole-on-shell listpoints listarcs height shellId)
  (let ((hole-msg (oldholemessage* #:height height
                                #:guid shellId)))
    (write-msg "Hole" hole-msg)
    (send-points listpoints)
    (send-arcs-complex listarcs)
    (read-guid)
    ))
#|
;does not work
(send (morph (u0) (list (xyz 0 0 0)(xyz 2 0 0)(xyz 2 2 0)(xyz 0 2 0)
                        (xyz 1 0 2)(xyz 3 0 2)(xyz 3 2 2)(xyz 1 2 2)
                        (xyz 0 0 4)(xyz 2 0 4)(xyz 2 2 4)(xyz 0 2 4))
                  (list (xy 0 1)(xy 1 2)(xy 2 3)(xy 3 0)
                        (xy 8 9)(xy 9 10)(xy 10 11)(xy 11 8)
                        (xy 0 4)(xy 1 5)(xy 2 6)(xy 3 7)
                        (xy 4 8)(xy 5 9)(xy 6 10)(xy 7 11))
                  (list (list (xy 0 1)(xy 3 1)(xy 2 1)(xy 1 1))
                        (list (xy 4 0)(xy 5 0)(xy 6 0)(xy 7 0))
                        (list (xy 0 0)(xy 9 0)(xy 13 0)(xy 4 1)(xy 12 1)(xy 8 1))
                        (list (xy 1 0)(xy 10 0)(xy 14 0)(xy 5 1)(xy 13 1)(xy 9 1))
                        (list (xy 2 0)(xy 11 0)(xy 15 0)(xy 6 1)(xy 14 1)(xy 10 1))
                        (list (xy 3 0)(xy 8 0)(xy 12 0)(xy 7 1)(xy 15 1)(xy 11 1)))))
;works
(send (morph (u0) (list (xyz 0 0 0)(xyz 2 0 0)(xyz 2 2 0)(xyz 0 2 0)
                        (xyz 1 0 2)(xyz 3 0 2)(xyz 3 2 2)(xyz 1 2 2)
                        (xyz 0 0 4)(xyz 2 0 4)(xyz 2 2 4)(xyz 0 2 4))
                  (list (xy 0 1)(xy 1 2)(xy 2 3)(xy 3 0)
                        (xy 4 5)(xy 5 6)(xy 6 7)(xy 7 4)
                        (xy 8 9)(xy 9 10)(xy 10 11)(xy 11 8)
                        (xy 0 4)(xy 1 5)(xy 2 6)(xy 3 7)
                        (xy 4 8)(xy 5 9)(xy 6 10)(xy 7 11))
                  (list (list (xy 0 1)(xy 3 1)(xy 2 1)(xy 1 1))
                        (list (xy 8 0)(xy 9 0)(xy 10 0)(xy 11 0))
                        (list (xy 0 0)(xy 13 0)(xy 4 1)(xy 12 1))
                        (list (xy 1 0)(xy 14 0)(xy 5 1)(xy 13 1))
                        (list (xy 2 0)(xy 15 0)(xy 6 1)(xy 14 1))
                        (list (xy 3 0)(xy 12 0)(xy 7 1)(xy 15 1))
                        (list (xy 4 0)(xy 17 0)(xy 8 1)(xy 16 1))
                        (list (xy 5 0)(xy 18 0)(xy 9 1)(xy 17 1))
                        (list (xy 6 0)(xy 19 0)(xy 10 1)(xy 18 1))
                        (list (xy 7 0)(xy 16 0)(xy 11 1)(xy 19 1)))))
;simple 4 sided morph
(send (morph (u0) (list (xyz 0 0 0)(xyz 2 0 0)(xyz 2 2 0)(xyz 0 2 0)
                        (xyz 0 0 4)(xyz 2 0 4)(xyz 2 2 4)(xyz 0 2 4))
                  (list (xy 0 1)(xy 1 2)(xy 2 3)(xy 3 0)
                        (xy 4 5)(xy 5 6)(xy 6 7)(xy 7 4)
                        (xy 0 4)(xy 1 5)(xy 2 6)(xy 3 7))
                  (list (list (xy 0 1)(xy 3 1)(xy 2 1)(xy 1 1))
                        (list (xy 4 0)(xy 5 0)(xy 6 0)(xy 7 0))
                        (list (xy 0 0)(xy 9 0)(xy 4 1)(xy 8 1))
                        (list (xy 1 0)(xy 10 0)(xy 5 1)(xy 9 1))
                        (list (xy 2 0)(xy 11 0)(xy 6 1)(xy 10 1))
                        (list (xy 3 0)(xy 8 0)(xy 7 1)(xy 11 1)))))
|#
(define default-morph-material (make-parameter "GENERIC - PREFABRICATED"))
(define (morph reference-point coords edges polygons #:material [material (default-morph-material)] #:level [level (current-level)])
  (let* ((sub-poly-sizes (get-sub-polys polygons))
         (msg-poly-sizes (intlistmsg* #:ilist sub-poly-sizes))
         (msg (morphmsg* #:refx (cx reference-point)
                         #:refy (cy reference-point)
                         #:refz (cz reference-point)
                         #:pts (prepare-points-to-send coords)
                         #:edges (prepare-points-to-send edges)
                         #:polygons (prepare-points-to-send (flatten polygons))
                         #:sizespolygons msg-poly-sizes
                         #:material material
                         #:level (storyinfo-index level))))
    (send/rcv-id "Morph" msg)
    #|
    (write-msg "Morph" msg)
    (send-points coords)
    (send-points edges)
    (send-points (flatten polygons))
    (let ((output (connection-out (bim-connection))))
      (write-sized serialize msg-poly-sizes output))
    (read-guid)
    |#
    ))

(define (box-2points point1 point2 #:bottom-level [bottom-level (current-level)])
  (let ((x1 (cx point1))
        (y1 (cy point1))
        (z1 (cz point1))
        (x2 (cx point2))
        (y2 (cy point2))
        (z2 (cz point2)))
    (morph
     (xyz 0 0 0)
     (list (xyz x1 y1 z1)
           (xyz x2 y1 z1)
           (xyz x2 y2 z1)
           (xyz x1 y2 z1)
           (xyz x1 y1 z2)
           (xyz x2 y1 z2)
           (xyz x2 y2 z2)
           (xyz x1 y2 z2))
     (list (xy 0 1)(xy 1 2)(xy 2 3)(xy 3 0)
           (xy 4 5)(xy 5 6)(xy 6 7)(xy 7 4)
           (xy 0 4)(xy 1 5)(xy 2 6)(xy 3 7))
     
     #;(list (list (xy 0 0)(xy 1 0) (xy 2 0)(xy 3 0))
             (list (xy 4 0)(xy 5 0) (xy 6 0)(xy 7 0))
             (list (xy 0 0)(xy 9 0) (xy 4 1)(xy 8 1))
             (list (xy 1 0)(xy 10 0)(xy 5 1)(xy 9 1))
             (list (xy 2 1)(xy 10 0)(xy 6 0)(xy 11 1))
             (list (xy 3 1)(xy 11 0) (xy 7 0)(xy 8 1)))
     
     (list (list (xy 0 1)(xy 3 1) (xy 2 1)(xy 1 1))
           (list (xy 4 0)(xy 5 0) (xy 6 0)(xy 7 0))
           (list (xy 0 0)(xy 9 0) (xy 4 1)(xy 8 1))
           (list (xy 1 0)(xy 10 0)(xy 5 1)(xy 9 1))
           (list (xy 2 0)(xy 11 0)(xy 6 1)(xy 10 1))
           (list (xy 3 0)(xy 8 0) (xy 7 1)(xy 11 1)))
     )))

(define (box point1 length width height #:bottom-level [bottom-level (current-level)])
  (box-2points point1 (+xyz point1 length width height) #:bottom-level bottom-level))

(define (cylinder p0 radius p1 #:level [level (current-level)])
  (send/rcv-id "Cylinder" (cylindermsg* #:p0x (cx p0)
                                        #:p0y (cy p0)
                                        #:p0z (cz p0)
                                        #:p1x (cx p1)
                                        #:p1y (cy p1)
                                        #:p1z (cz p1)
                                        #:radius radius
                                        #:level (storyinfo-index level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Polygons & Solids       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns points for a polygon
(define (polygon-points center sides radius [rotation 0])
  (map-division (lambda (angle)
                  (+pol center radius (+ rotation angle)))
                0 (* 2 pi) sides #f))

(define (apply-height points height)
  (map (lambda (p)
             (+z p vector))
       points))

(define (apply-vector points vector)
  (map (lambda (p)
             (+xyz p (cx vector) (cy vector) (cz vector)))
       points))
;(send (sweep (polygon-points (u0) 4 1) (list (z 0)(xz 1 1)(xz 2 2)(xz 1 3)(xz 0 2))))
(define (apply-r-vector points vector)
  (map (lambda (p)
         #;(p+v p vector)
         
         (println vector)
         (println (sph-psi vector))
         (p+v (rotate-p-y (rotate-p-z p (sph-phi vector))
                          (sph-psi vector))
              vector))
         #|
         (define pp p #;(p+v p vector))
         (define p-aux (xyz (- (* (cx pp) (cos (sph-phi vector)))
                               (* (cy pp) (sin (sph-phi vector))))
                            (+ (* (cx pp) (sin (sph-phi vector)))
                               (* (cy pp) (cos (sph-phi vector))))
                            (cz pp)))
         ;(p+v p-aux vector)
         (println (+xyz (xyz (cx p-aux)
                    (- (* (cy p-aux) (cos (- pi/2 (sph-psi vector))))
                       (* (cz p-aux) (sin (- pi/2 (sph-psi vector)))))
                    (+ (* (cy p-aux) (sin (- pi/2 (sph-psi vector))))
                       (* (cz p-aux) (cos (- pi/2 (sph-psi vector))))))
                       (cx vector)(cy vector)(cz vector)))
         (+xyz p-aux #;(xyz (cx p-aux)
                    (- (* (cy p-aux) (cos (- pi/2 (sph-psi vector))))
                       (* (cz p-aux) (sin (- pi/2 (sph-psi vector)))))
                    (+ (* (cy p-aux) (sin (- pi/2 (sph-psi vector))))
                       (* (cz p-aux) (cos (- pi/2 (sph-psi vector))))))
                       (cx vector)(cy vector)(cz vector)))
          |#
       points))


;(polygon-apply-vector (list (xy -1 -1)(xy 1 -1)(xy 1 1)(xy -1 1)) (z 1))
;Returns the solid points of a vector applied to all given points
(define (polygon-apply-vector points vector)
  (append points (map (lambda (p)
                        (if (number? vector)
                            (+z p vector)
                            (+xyz p (cx vector) (cy vector) (cz vector))))
                      points)))

;Returns edges for a polygon of n sides
(define (polygon-edges sides [start-number 0])
  (map-division (lambda (n)
                  (if (= n (- (+ start-number sides) 1))
                      (xy n start-number)
                      (xy n (+ n 1))))
                start-number (+ start-number sides) sides #f))

;Returns the polygon formed by edges - only required for internal use
(define (internal-polygon sides)
  (list (map-division (lambda (n)
                        (xy n 0))
                      0 sides sides #f)))

;(send (polygon-points (list (xy -1 -1)(xy 1 -1)(xy 1 1)(xy -1 1))))
;Creates a polygon given points. Points aren't closed.
(define (surface-polygon points [material (default-morph-material)])
  (let* ((sides (length points))
         (edges (polygon-edges sides))
         (polygon (internal-polygon sides)))
    (morph (u0) points edges polygon #:material material)))

;Returns edges for a solid formed by points
;Points are given in counter-clockwise order,
; the first half should be the base points
; the second half should be the top points
(define (solid-edges points)
  (let* ((size (length points))
         (sides (/ size 2))
         (base-edges (polygon-edges sides 0))
         (top-edges (polygon-edges sides sides))
         (side-edges (map (lambda (e1 e2)
                            (xy e1 e2))
                          (range 0 sides)
                          (range sides size))))
    (append base-edges top-edges side-edges)))

;Constructs the solid from edges - internal use
(define (internal-solid sides)
  (let ((base (map (lambda (n)
                     (xy n 1))
                   (cons 0 (reverse (range 1 sides)))))
        (top (map-division (lambda (n)
                             (xy n 0))
                           sides (* sides 2) sides #f))
        (sides-pol (map-division (lambda (n)
                               (list (xy n 0)
                                     (if (= n (- sides 1))
                                         (xy (+ n sides 1) 0)
                                         (xy (+ n (* sides 2) 1) 0))
                                     (xy (+ n sides) 1)
                                     (xy (+ n (* sides 2)) 1)))
                             0 sides sides #f)))
    (append (list base top) sides-pol)))

;Creates a solid, given its points.
;Counter-clockwise order
;First half of points: base
;Second half of points: top 
(define (solid solid-points [material (default-morph-material)] #:level [level (current-level)])
  (let* ((sides (/ (length solid-points) 2))
         (edges (solid-edges solid-points))
         (polygons (internal-solid sides)))
  (morph (u0) solid-points edges polygons #:material material #:level level)))

(define (test-solid-edges sides [start 0])
  (let* ((base-edges (polygon-edges sides start))
         (top-edges (polygon-edges sides (+ start sides)))
         (side-edges (map (lambda (e1 e2)
                            (xy e1 e2))
                          (range start (+ start sides))
                          (range (+ start sides) (+ start (* sides 2))))))
    (append base-edges top-edges side-edges)))

(define (test-internal-solid sides [start 0])
  (let ((base (map (lambda (n)
                     (xy (+ start n) 1))
                   (cons 0 (reverse (range 1 sides)))))
        (top (map-division (lambda (n)
                             (xy (+ start n) 0))
                           sides (* sides 2) sides #f))
        (sides-pol (map-division (lambda (n)
                                   (list (xy (+ start n) 0)
                                         (if (= n (- sides 1))
                                             (xy (+ (+ start n) sides 1) 0)
                                             (xy (+ (+ start n) (* sides 2) 1) 0))
                                         (xy (+ (+ start n) sides) 1)
                                         (xy (+ (+ start n) (* sides 2)) 1)))
                                 0 sides sides #f)))
    (append (list base top) sides-pol)))

(define (test-solid solid-points [n-polygons 2])
  (let* ((sides (/ (length solid-points) n-polygons))
         (edges #;(test-solid-edges sides 0)
                (sweep-solid-edges sides 2))
         (polygons (test-internal-solid sides 0)
                   #;(append (test-internal-solid sides 0)
                           (test-internal-solid sides (* sides 3)))))
    ;(println edges)
    ;(println "-----------------------")
    ;(println polygons)
    (morph (u0) solid-points edges polygons)))

(define (sweep-solid-edges sides n-faces)
  (flatten (append (for/list ([i (- n-faces 1)])
                             (polygon-edges sides (* i sides)))
                   (polygon-edges sides (* (- n-faces 1) sides))
                   (for/list ([i (- n-faces 1)])
                                     (map (lambda (e1 e2)
                                            (xy e1 e2))
                                          (range (* i sides) (+ (* i sides) sides))
                                          (range (+ (* i sides) sides) (+ (* i sides) (* sides 2)))))
                   )))
#;(list (list (xy 0 1)(xy 3 1)(xy 2 1)(xy 1 1))
                        (list (xy 8 0)(xy 9 0)(xy 10 0)(xy 11 0))
                        (list (xy 0 0)(xy 13 0)(xy 4 1)(xy 12 1))
                        (list (xy 1 0)(xy 14 0)(xy 5 1)(xy 13 1))
                        (list (xy 2 0)(xy 15 0)(xy 6 1)(xy 14 1))
                        (list (xy 3 0)(xy 12 0)(xy 7 1)(xy 15 1))
                        (list (xy 4 0)(xy 17 0)(xy 8 1)(xy 16 1))
                        (list (xy 5 0)(xy 18 0)(xy 9 1)(xy 17 1))
                        (list (xy 6 0)(xy 19 0)(xy 10 1)(xy 18 1))
                        (list (xy 7 0)(xy 16 0)(xy 11 1)(xy 19 1)))
(define (sweep-side-polygons sides n-faces [i 0])
  (if (= i (- n-faces 1))
      (list)
      (append (map-division (lambda (n)
                              (list (xy n 0)
                                    (if (= n (- (* (+ i 1) sides) 1))
                                        (xy (+ n (* sides (- n-faces 1)) 1) 0)
                                        (xy (+ n (* sides n-faces) 1) 0))
                                    (xy (+ n sides) 1)
                                    (xy (+ n (* sides n-faces)) 1)))
                            (* i sides) (+ (* i sides) sides) sides #f)
              (sweep-side-polygons sides n-faces (+ i 1)))))

(define (sweep-solid-polygons sides n-faces)
  (let ((base (map (lambda (n)
                     (xy n 1))
                   (cons 0 (reverse (range 1 sides)))))
        (top (map-division (lambda (n)
                             (xy n 0))
                           (* sides (- n-faces 1)) (* sides n-faces)  sides #f))
        (sides-pol (sweep-side-polygons sides n-faces)
                   #;(for/list ([i (- n-faces 1)])
                             (map-division (lambda (n)
                                   (list (xy n 0)
                                         (if (= n (- sides 1))
                                             (xy (+ n sides 1) 0)
                                             (xy (+ n (* sides 2) 1) 0))
                                         (xy (+ n sides) 1)
                                         (xy (+ n (* sides 2)) 1)))
                                 (* i sides) (+ (* i sides) sides) sides #f))))
    (append (list base top) sides-pol)))

(define (sweep-coords points path)
  (if (null? path)
      (list)
      (append (apply-vector points (car path)) (sweep-coords points (cdr path)))))

(define (sweep points path)
  (let ((sides (length points))
        (n-faces (length path)))
    (morph (u0) (sweep-coords points path) (sweep-solid-edges sides n-faces) (sweep-solid-polygons sides n-faces))))

;Does an extrusion given polygon-points and a vector for the extrusion
(define (extrusion polygon-points vector [material (default-morph-material)] #:level [level (current-level)])
  (solid (polygon-apply-vector polygon-points vector)
         material
         #:level level))

(define (pyramid-points p top sides radius [rotation 0])
  (if (number? top)
      (append (polygon-points p sides radius rotation) (list (+z p top)))
      (append (polygon-points p sides radius rotation) (list top))))

(define (pyramid-edges points)
  (let* ((sides (- (length points) 1))
         (base-edges (polygon-edges sides 0))
         (sides-edges (map-division (lambda (n)
                                      (xy n (- (length points) 1)))
                                    0 sides sides #f)))
    (append base-edges sides-edges)))

(define (internal-pyramid points)
  (let* ((sides (- (length points) 1))
         (base (map (lambda (n)
                      (xy n 1))
                    (cons 0 (reverse (range 1 sides)))))
         (sides-pol (map-division (lambda (n)
                                    (list (xy n 0)
                                          (if (= n (- sides 1))
                                              (xy (+ n 1) 0)
                                              (xy (+ n sides 1) 0))
                                          (xy (+ n sides) 1)))
                                  0 sides sides #f)))
    (append (list base) sides-pol)))

(define (pyramid pyramid-points [material (default-morph-material)] #:level [level (current-level)])
  (let ((edges (pyramid-edges pyramid-points))
        (polygons (internal-pyramid pyramid-points)))
  (morph (u0) pyramid-points edges polygons #:material material #:level level)))

(define (pyramid-by-center center top radius [n-points 100] #:material [material (default-morph-material)]#:level [level (current-level)])
  (pyramid (append (polygon-points center n-points radius)
                   (list top))
           material
           #:level level)) 

(define (morph-translate el x y z)
  (send/rcv-id "MorphTrans" (transformmsg* #:guid el
                                           #:op "t"
                                           #:x x
                                           #:y y
                                           #:z z
                                           #:angle 0
                                           #:scale 0)))

;axis: "x" "y" "z"
(define (morph-rotate el angle axis)
  (send/rcv-id "MorphTrans" (transformmsg* #:guid el
                                           #:op axis
                                           #:x 0
                                           #:y 0
                                           #:z 0
                                           #:angle angle
                                           #:scale 0)))

(define (morph-rotate-x el angle)
  (morph-rotate el angle "x"))
(define (morph-rotate-y el angle)
  (morph-rotate el angle "y"))
(define (morph-rotate-z el angle)
  (morph-rotate el angle "z"))

(define (morph-scale el scale)
  (send/rcv-id "MorphTrans" (transformmsg* #:guid el
                                           #:op "s"
                                           #:x 0
                                           #:y 0
                                           #:z 0
                                           #:angle 0
                                           #:scale scale)))

(define-syntax-rule (mf m i j) (real->double-flonum (matrix-ref m i j)))

(define (loc->matrix p)
  (let ((m (world-transformation p)))
    (list (mf m 0 0) (mf m 0 1) (mf m 0 2) (mf m 0 3)
          (mf m 1 0) (mf m 1 1) (mf m 1 2) (mf m 1 3)
          (mf m 2 0) (mf m 2 1) (mf m 2 2) (mf m 2 3))))

#|
(send (apply-matrix-to-morph (box (u0) 1 1 1) (list (cos pi/4) (- (sin pi/4)) 0 0
                                                           (sin pi/4) (cos pi/4)     0 0
                                                           0          0              1 0)))

(send (apply-matrix-to-morph (box (u0) 1 1 1) (list 1 0 0 10
                                                           0 1 0  0
                                                           0 0 1  0)))

(send (apply-matrix-to-morph (box (u0) 1 1 1) (list (cos pi/4) (- (sin pi/4)) 0 10
                                                           (sin pi/4) (cos pi/4)     0 0
                                                           0          0              1 0)))

(send (apply-matrix-to-morph (box (+xyz (u0) -0.5 -0.5 -0.5) 1 1 1) (list (cos pi/4) (- (sin pi/4)) 0 0
                                                           (sin pi/4) (cos pi/4)     0 0
                                                           0          0              1 0)))
|#

(define (apply-matrix-to-point p m)
  (xyz (+ (* (mf m 0 0) (cx p)) (* (mf m 0 1) (cy p)) (* (mf m 0 2) (cz p)) (mf m 0 3))
       (+ (* (mf m 1 0) (cx p)) (* (mf m 1 1) (cy p)) (* (mf m 1 2) (cz p)) (mf m 1 3))
       (+ (* (mf m 2 0) (cx p)) (* (mf m 2 1) (cy p)) (* (mf m 2 2) (cz p)) (mf m 2 3))))

(define (apply-rotation-matrix-to-point p m)
  (xyz (+ (* (mf m 0 0) (cx p)) (* (mf m 0 1) (cy p)) (* (mf m 0 2) (cz p)))
       (+ (* (mf m 1 0) (cx p)) (* (mf m 1 1) (cy p)) (* (mf m 1 2) (cz p)))
       (+ (* (mf m 2 0) (cx p)) (* (mf m 2 1) (cy p)) (* (mf m 2 2) (cz p)))))

(define (apply-translation-matrix-to-point p m)
  (xyz (+ (cx p) (mf m 0 3))
       (+ (cy p) (mf m 1 3))
       (+ (cz p) (mf m 2 3))))

(define (apply-matrix-to-morph el matrix)
  (send/rcv-id "ApplyMatrix" (applymatrix* #:guid el
                                           #:matrix matrix)))

(define (right-cuboid cb width height h/ct)
  (let-values ([(cb dz) (position-and-height cb h/ct)])
    (apply-matrix-to-morph 
     (box (+xy (u0 world-cs) (- (/ width 2.0)) (- (/ height 2.0))) width height dz)
     (loc->matrix cb))))


#|
;;Extrusion example
(send (extrusion (list (xyz -94695.808 -107031.17999999999 0.0)
       (xyz -94667.651 -107080.392 0.0)
       (xyz -94651.74400000001 -107071.325 0.0)
       (xyz -94658.07099999999 -107060.247 0.0)
       (xyz -94658.68300000001 -107059.165 0.0)
       (xyz -94662.55100000001 -107061.286 0.0)
       (xyz -94667.344 -107052.934 0.0)
       (xyz -94664.406 -107051.342 0.0)
       (xyz -94669.03 -107042.771 0.0)
       (xyz -94672.71400000001 -107044.78 0.0)
       (xyz -94678.067 -107035.45100000001 0.0)
       (xyz -94673.61100000001 -107033.038 0.0))
 11.781))

(send (extrusion (list (xyz -94.695808 -107.03117999999999 0.0)
       (xyz -94.667651 -107.080392 0.0)
       (xyz -94.65174400000001 -107.071325 0.0)
       (xyz -94.65807099999999 -107.060247 0.0)
       (xyz -94.65868300000001 -107.059165 0.0)
       (xyz -94.66255100000001 -107.061286 0.0)
       (xyz -94.667344 -107.052934 0.0)
       (xyz -94.664406 -107.051342 0.0)
       (xyz -94.66903 -107.042771 0.0)
       (xyz -94.67271400000001 -107.04478 0.0)
       (xyz -94.678067 -107.03545100000001 0.0)
       (xyz -94.67361100000001 -107.033038 0.0))
 11.781))
|#

