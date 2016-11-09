#lang racket
(provide (except-out (all-defined-out)
                     trim?))

(require "protobuf1/protobuf.rkt")
(require "protobuf1/encoding.rkt")
(require "messages.rkt")
(require "communication.rkt")
(require "geometry.rkt")
(require "../base/coord.rkt"
         "../base/connection.rkt")
(require srfi/26)

(define DEGRAD (/ pi 180.0))

(define crash-on-no-material? (make-parameter #t))
(define crash-on-no-name? (make-parameter #t))
(define trim? (make-parameter #f))
(define non-trim-layer (make-parameter "Non Trim Layer"))
(define trim-layer (make-parameter "Trim Layer"))
(define (default-layer)
  (if (trim?)
      (trim-layer)
      (non-trim-layer)))
(define default-top-link (make-parameter #t))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions to create objects;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-wall-alignment (make-parameter "Center"))
(define default-wall-type-of-material (make-parameter "Basic"))
(define default-wall-thickness (make-parameter 0.3))
(define default-wall-material  
  (make-parameter (cond [(eq? (default-wall-type-of-material) "Basic") "GENERIC - STRUCTURAL"]
                        [(eq? (default-wall-type-of-material) "Composite") "Generic Wall/Shell"])))
#|
Can be:
Normal |
Slanted - alpha angle needed \
DoubleSlanted - alpha and beta angle needed /\
|#
(define default-wall-profile (make-parameter "Normal"))


#|
Function used to create a wall
 
 TODO information
 returns: id of the created wall

Example of usage:
(send (wall (list (xy 0 0)(xy 10 0))))
(send (wall (list (xy 0 0)(xy 10 0)) #:type-of-profile "Slanted" #:alpha-angle (* 80 DEGRAD)))
(send (wall (list (xy 0 0)(xy 10 0)) #:type-of-profile "DoubleSlanted" #:alpha-angle (* 100 DEGRAD) #:beta-angle (* 80 DEGRAD)))
(send (wall (list (x 0)(x 5)) #:ref-material "Metal - Brass" #:opp-material "Metal - Brass" #:sid-material "Metal - Brass"))
Question:
Make the wall always double slanted whatever the angles?
|#
(define (walls guide 
              #:alignment [alignment (default-wall-alignment)]
              #:bottom-level [bottom-level (current-level)]
              #:top-level [top-level (upper-level bottom-level)]
              ;;ArchiCAD ONLY --------------------------------------------------------------
              #:top-linked? [top-linked? (default-top-link)]
              #:thickness [thickness (default-wall-thickness)]
              #:arcs [arcs (list)]
              
              #:type-of-material [type-of-material (default-wall-type-of-material)]
              #:material [material
                          #;(default-wall-material) 
                          (cond [(eq? type-of-material "Basic") "GENERIC - STRUCTURAL"]
                                [(eq? type-of-material "Composite") "Generic Wall/Shell"])]
              #:alpha-angle [alpha-angle (/ pi 2)]
              #:beta-angle [beta-angle (/ pi 2)]
              #:type-of-profile [type-of-profile (default-wall-profile)]
              #:height [height null]
              #:profile-name [profile-name ""]
              #:flipped? [flipped? #f]
              #:bottom-offset [bottom-offset 0]
              #:layer [layer (default-layer)]
              #:windows [windows (list)]
              #:window-order [window-order (list)]
              #:reference-offset [reference-offset 0]
              #:ref-material [ref-material ""]
              #:opp-material [opp-material ""]
              #:sid-material [sid-material ""])
  (let* (
         
         ;Don't like this if, was unable to do (unless (null? height) #:height height)
         (msg (if (null? height)
                  (wallmsg* #:pts (if (pointsmessage? guide)
                                      guide
                                      (prepare-points-to-send guide))
                            #:bottomindex (if (storyinfo? bottom-level)
                                              (storyinfo-index bottom-level)
                                              bottom-level)
                            #:upperindex (if (storyinfo? top-level)
                                             (storyinfo-index top-level)
                                             top-level)
                            #:thickness thickness
                            #:arcs (if (polyarcsmessage? arcs)
                                       arcs
                                       (prepare-arcs-to-send arcs))
                            #:material material
                            #:type type-of-material
                            #:referenceline alignment
                            #:alphaangle alpha-angle
                            #:betaangle beta-angle
                            #:typeprofile type-of-profile
                            #:profilename profile-name
                            #:flipped (not flipped?)
                            #:bottomoffset bottom-offset
                            #:layer layer
                            #:windows windows
                            #:windoworder window-order
                            #:refoffset reference-offset
                            #:refmat ref-material
                            #:oppmat opp-material
                            #:sidmat sid-material
                            #:toplinked top-linked?)
                  (wallmsg* #:pts (if (pointsmessage? guide)
                                      guide
                                      (prepare-points-to-send guide))
                            #:bottomindex (if (storyinfo? bottom-level)
                                              (storyinfo-index bottom-level)
                                              bottom-level)
                            #:upperindex (if (storyinfo? top-level)
                                             (storyinfo-index top-level)
                                             top-level)
                            #:thickness thickness
                            #:arcs (if (polyarcsmessage? arcs)
                                       arcs
                                       (prepare-arcs-to-send arcs))
                            #:material material
                            #:type type-of-material
                            #:referenceline alignment
                            #:alphaangle alpha-angle
                            #:betaangle beta-angle
                            #:typeprofile type-of-profile
                            #:height height
                            #:profilename profile-name
                            #:flipped (not flipped?)
                            #:bottomoffset bottom-offset
                            #:layer layer
                            #:windows windows
                            #:windoworder window-order
                            #:refoffset reference-offset
                            #:refmat ref-material
                            #:oppmat opp-material
                            #:sidmat sid-material
                            #:toplinked top-linked?))))
    (write-msg "NewWall" msg)
    ;(send-points guide)
    (let ((result (read-guids*)))
      (if (and (elementidlist-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error (string-append "The material does not exist - " material)))
          (elementidlist-guid result)
          #;(if (null? (cdr (elementidlist-guid result)))
              (car (elementidlist-guid result))
              (elementidlist-guid result))))))

(define (sub-polygons-from-list holes [counter 0])
  (if (null? holes)
      (list)
      (cons (+ counter (length (car holes)))
            (sub-polygons-from-list (cdr holes) (+ counter (length (car holes)))))))

(define (wall pt0 pt1 
              #:alignment [alignment (default-wall-alignment)]
              #:bottom-level [bottom-level (current-level)]
              #:top-level [top-level (upper-level bottom-level)]
              ;;ArchiCAD ONLY --------------------------------------------------------------
              #:thickness [thickness (default-wall-thickness)]
              #:arcs [arcs (list)]
              
              #:type-of-material [type-of-material (default-wall-type-of-material)]
              #:material [material
                          #;(default-wall-material) 
                          (cond [(eq? type-of-material "Basic") "GENERIC - STRUCTURAL"]
                                [(eq? type-of-material "Composite") "Generic Wall/Shell"])]
              #:alpha-angle [alpha-angle (/ pi 2)]
              #:beta-angle [beta-angle (/ pi 2)]
              #:type-of-profile [type-of-profile (default-wall-profile)]
              #:height [height null]
              #:profile-name [profile-name ""]
              #:flipped? [flipped? #f]
              #:bottom-offset [bottom-offset 0]
              #:layer [layer (default-layer)]
              #:windows [windows (list)]
              #:window-order [window-order (list)]
              #:reference-offset [reference-offset 0]
              #:ref-material [ref-material ""]
              #:opp-material [opp-material ""]
              #:sid-material [sid-material ""]
              #:top-linked? [top-linked? (default-top-link)])
  (car (walls (list pt0 pt1)
              #:alignment alignment
              #:bottom-level bottom-level
              #:top-level top-level
              #:thickness thickness
              #:arcs arcs
              
              #:type-of-material type-of-material
              #:material material
              #:alpha-angle alpha-angle
              #:beta-angle beta-angle
              #:type-of-profile type-of-profile
              #:height height
              #:profile-name profile-name
              #:flipped? flipped?
              #:bottom-offset bottom-offset
              #:layer layer
              #:windows windows
              #:window-order window-order
              #:reference-offset reference-offset
              #:ref-material ref-material
              #:opp-material opp-material
              #:sid-material sid-material
              #:top-linked? top-linked?)))

#|
Function used to create a door into an existing wall

 guid: id of wall, this value is returned by the functions that create walls
       (wall ...)
 objloc: object location in the wall
 zpos: z of the door

 returns: door id 

Example of usage:
(send (door wallId 1.0 0.0))
|#
(define default-door-type (make-parameter "Door 18"))

(define (door guid
              p
              #:type [type-of-door (default-door-type)]
              #:width [width -10000]
              #:height [height -10000]
              #:flip-x [flip-x #f]
              #:flip-y [flip-y #f]
              #:properties [properties (list)]
              #:layer [layer (default-layer)])
  (let ((splitted-list (split-params-list properties)))
        (send/rcv-id "Door" (doormessage* #:guid guid
                                          #:objloc (cx p)
                                          #:depthoffset (cy p)
                                          #:zpos (cz p)
                                          #:height height
                                          #:width width
                                          #:hole #f
                                          #:name type-of-door
                                          #:flipx flip-x
                                          #:flipy flip-y
                                          #:params (additionalparams* #:names (list-ref splitted-list 0)
                                                                      #:integers (list-ref splitted-list 1)
                                                                      #:doubles (list-ref splitted-list 2)
                                                                      #:strings (list-ref splitted-list 3)
                                                                      #:booleans (list-ref splitted-list 4)
                                                                      #:intarrays (list-ref splitted-list 5)
                                                                      #:doublearrays (list-ref splitted-list 6)
                                                                      #:stringarrays (list-ref splitted-list 7)
                                                                      #:boolarrays (list-ref splitted-list 8)
                                                                      #:paramtype (list-ref splitted-list 9)
                                                                      #:isarray (list-ref splitted-list 10))
                                          #:layer layer))))

;;TODO Review this function
(define (hole-in-wall guid
                      objloc
                      [width -10000] [bottom 0] [height -10000]
                      #:flip-x [flip-x #f]
                      #:flip-y [flip-y #f]
                      #:properties [properties (list)])
  (let ((splitted-list (split-params-list properties)))
        (send/rcv-id "Door" (doormessage* #:guid guid
                                          #:objloc objloc
                                          #:zpos bottom
                                          #:height height
                                          #:width width
                                          #:hole #t
                                          #:name ""
                                          #:flipx flip-x
                                          #:flipy flip-y
                                          #:params (additionalparams* #:names (list-ref splitted-list 0)
                                                                      #:integers (list-ref splitted-list 1)
                                                                      #:doubles (list-ref splitted-list 2)
                                                                      #:strings (list-ref splitted-list 3)
                                                                      #:booleans (list-ref splitted-list 4)
                                                                      #:intarrays (list-ref splitted-list 5)
                                                                      #:doublearrays (list-ref splitted-list 6)
                                                                      #:stringarrays (list-ref splitted-list 7)
                                                                      #:boolarrays (list-ref splitted-list 8)
                                                                      #:paramtype (list-ref splitted-list 9)
                                                                      #:isarray (list-ref splitted-list 10))))))

;;TODO Review this function
(define (hole-in-wall-test guid list-points [list-arcs (list)])
  (let ((msg (holemsg* #:guid guid )))
    (write-msg "HoleTest" msg)
    (send-points list-points)
    (send-arcs list-arcs)
    (read-guid)))

#|
Function used to create a window into a existing wall
 
 guid: id of wall, this value is returned by the functions that create walls
       (wall ...)
 objloc: object location in the wall
 zpos: z of the window

 returns: window id 

Example of usage:
(send (window wallId 1.0 1.0))
|#
(define (window guid
                p
                #:width [width -10000]
                #:height [height -10000]
                #:flip-x [flip-x #f]
                #:flip-y [flip-y #f]
                #:type-of-window [type-of-window "Window 18"]
                #:properties [properties (list)]
                #:layer [layer (default-layer)])
  (let ((splitted-list (split-params-list properties)))
    (send/rcv-id "Window" (windowmessage* #:guid guid
                                          #:objloc (cx p)
                                          #:depthoffset (cy p)
                                          #:zpos (cz p)
                                          #:name type-of-window
                                          #:width width
                                          #:height height
                                          #:flipx flip-x
                                          #:flipy flip-y
                                          #:params (additionalparams* #:names (list-ref splitted-list 0)
                                                                      #:integers (list-ref splitted-list 1)
                                                                      #:doubles (list-ref splitted-list 2)
                                                                      #:strings (list-ref splitted-list 3)
                                                                      #:booleans (list-ref splitted-list 4)
                                                                      #:intarrays (list-ref splitted-list 5)
                                                                      #:doublearrays (list-ref splitted-list 6)
                                                                      #:stringarrays (list-ref splitted-list 7)
                                                                      #:boolarrays (list-ref splitted-list 8)
                                                                      #:paramtype (list-ref splitted-list 9)
                                                                      #:isarray (list-ref splitted-list 10))
                                          #:layer layer))))
  
#|
Function to create a Curtain Wall
 
 TODO information
 main-or-distinct-panel: determines if a panel is a main panel or distinct panel.
                         They have different materials, controlled by #:panel-material #:secondary-panel-material
 
 returns: curtain-wall id

Example of usage: 
(send (delete-stories)(curtain-wall (list (xy 0 0)(xy 10 0))))

(send (curtain-wall (list (x 0)(x 10)) (list 2 3 5)(list 0.5 1 1.5) #:main-panel? (list #t #t #t #f #f #f #t #t #t))) 

(send (slab (list (x 0)(x 10)(xy 10 10)(xy 0 10)) #:parcs (list pi pi pi pi))
      (curtain-wall (list (x 0)(x 10)(xy 10 10)(xy 0 10)(xy 0 0)) (list 2 3 5)(list 0.5 1 1.5) #:panels-angle pi/3 #:main-panel? (list #t #t #t #f #f #f #t #t #t) #:arcs (list pi pi pi pi)))

(send (slab (list (x 0)(x 10)(xy 10 10)(xy 0 10)) #:parcs (list pi pi pi pi))
      (curtain-wall (list (x 0)(x 10)(xy 10 10)(xy 0 10)(xy 0 0)) (list 1)(list 1) #:main-panel? (list #f) #:arcs (list pi pi pi pi)))

|#
(define (curtain-wall guide
                      primary-segments
                      secondary-segments
                      #:panel-material [panel-material "Metal - Stainless Steel"]
                      #:secondary-panel-material [secondary-panel-material "Glass - Blue"]
                      #:panels-angle [panels-angle 0]
                      #:vertical-segments-material [vertical-segments-material "Metal - Stainless Steel"]
                      #:horizontal-segments-material [horizontal-segments-material "Metal - Stainless Steel"]
                      #:boundary-material [boundary-material "Metal - Stainless Steel"]
                      #:main-panel? [main-panel?
                                     (for/list ([i (* (length primary-segments)
                                                      (length secondary-segments))])
                                               #t)]
                      #:arcs [arcs (list)]
                      #:bottom-level [bottom-level (current-level)]
                      #:top-level [top-level (upper-level bottom-level)]
                      #:offset [offset 0]
                      #:layer [layer (default-layer)]
                      #:height [height null]
                      #:main-panel-thickness [main-panel-thickness 0.2]
                      #:secondary-panel-thickness [secondary-panel-thickness 0.2]
                      #:boundary-frame-width [boundary-frame-width 0.1]
                      #:boundary-frame-depth [boundary-frame-depth 0.3]
                      #:boundary-frame-depth-offset [boundary-frame-depth-offset 0.25]
                      #:mullion-frame-width [mullion-frame-width 0.08]
                      #:mullion-frame-depth [mullion-frame-depth 0.25]
                      #:mullion-frame-depth-offset [mullion-frame-depth-offset 0.2]
                      #:transom-frame-width [transom-frame-width 0.06]
                      #:transom-frame-depth [transom-frame-depth 0.1]
                      #:transom-frame-depth-offset [transom-frame-depth-offset 0.11]
                      ;Does NOT work because API does not support it
                      #:top-linked? [top-linked? (default-top-link)])
  (let ((c-wall-msg (if (null? height)
                        (curtainwallmsg* #:pts (prepare-points-to-send guide)
                                         #:arcs (prepare-arcs-to-send arcs)
                                         #:bottomindex (storyinfo-index bottom-level)
                                         #:upperindex (storyinfo-index top-level)
                                         #:primaries primary-segments
                                         #:secondaries secondary-segments
                                         #:mainpanels main-panel?
                                         #:panelmaterial secondary-panel-material
                                         #:secpanelmaterial panel-material 
                                         #:verticalframematerial vertical-segments-material
                                         #:horizontalframematerial horizontal-segments-material
                                         #:framematerial boundary-material
                                         #:panelsangle panels-angle
                                         #:offset offset
                                         #:layer layer
                                         #:mainpanelthickness main-panel-thickness
                                         #:secondarypanelthickness secondary-panel-thickness
                                         #:bframewidth boundary-frame-width
                                         #:bframedepth boundary-frame-depth
                                         #:bframeoffset boundary-frame-depth-offset
                                         #:mframewidth mullion-frame-width
                                         #:mframedepth mullion-frame-depth
                                         #:mframeoffset mullion-frame-depth-offset
                                         #:tframewidth transom-frame-width
                                         #:tframedepth transom-frame-depth
                                         #:tframeoffset transom-frame-depth-offset
                                         #:toplinked top-linked?)
                        (curtainwallmsg* #:pts (prepare-points-to-send guide)
                                         #:arcs (prepare-arcs-to-send arcs)
                                         #:bottomindex (storyinfo-index bottom-level)
                                         #:upperindex (storyinfo-index top-level)
                                         #:primaries primary-segments
                                         #:secondaries secondary-segments
                                         #:mainpanels main-panel?
                                         #:panelmaterial secondary-panel-material
                                         #:secpanelmaterial panel-material 
                                         #:verticalframematerial vertical-segments-material
                                         #:horizontalframematerial horizontal-segments-material
                                         #:framematerial boundary-material
                                         #:panelsangle panels-angle
                                         #:offset offset
                                         #:layer layer
                                         #:height height
                                         #:mainpanelthickness main-panel-thickness
                                         #:secondarypanelthickness secondary-panel-thickness
                                         #:bframewidth boundary-frame-width
                                         #:bframedepth boundary-frame-depth
                                         #:bframeoffset boundary-frame-depth-offset
                                         #:mframewidth mullion-frame-width
                                         #:mframedepth mullion-frame-depth
                                         #:mframeoffset mullion-frame-depth-offset
                                         #:tframewidth transom-frame-width
                                         #:tframedepth transom-frame-depth
                                         #:tframeoffset transom-frame-depth-offset
                                         #:toplinked top-linked?))))
    (send/rcv-id "CurtainWall" c-wall-msg)))
#|Functions do not work. Nothing happens. 
(define (internal-transform-curtain-wall cwall op x y z angle scale)
  (let ((msg (transformmsg* #:op op
                            #:guid cwall
                            #:x x
                            #:y y
                            #:z z
                            #:angle angle
                            #:scale scale)))
    (send/no-rcv "CWTransform" msg)))

(define (translate-curtain-wall cwall pt)
  (internal-transform-curtain-wall cwall "t" (cx pt)(cy pt)(cz pt) 0 0))

(define (rotate-curtain-wall cwall angle)
  (internal-transform-curtain-wall cwall "r" 0 0 0 angle 0))

(define (scale-curtain-wall cwall scale)
  (internal-transform-curtain-wall cwall "s" 0 0 0 0 scale))
|#
#|
Function to create a Slab
 TODO information
 returns: slab id
Example of usage: 
(send (slab cPoints))

|#

(define (sub-polys-position-specific-argument points [counter 0])
  (cond
    [(null? points) (list)]
    [(list? (car points)) (cons (+ counter (length (car points))) (sub-polys-position-specific-argument (cdr points) (+ counter (length (car points)))))]
    [else (sub-polys-position-specific-argument (cdr points) counter)]))

(define (sub-polys-position points [counter 0])
  (cond
    [(null? points) (list counter)]
    [(list? (car points)) (cons counter (sub-polys-position (cdr points) (+ counter (length (car points)))))]
    [else (sub-polys-position (cdr points) (+ counter 1))]))



(define (prepare-points points)
  (let ((points (close-guide points (car points))))
    (list (flatten points) (sub-polys-position points))))

(define default-slab-reference (make-parameter "Top"))
(define default-slab-type-of-material (make-parameter "Basic"))
#;(define default-slab-material  
    (make-parameter (cond [(eq? (default-slab-type-of-material) "Basic") "GENERIC - INTERNAL CLADDING"]
                          [(eq? (default-slab-type-of-material) "Composite") "Generic Slab/Roof"])))
(define (slab guide
              #:bottom-level [bottom-level (current-level)]
              ;;ArchiCAD ONLY --------------------------------------------------------------
              #:thickness [thickness 0.3]
              #:bottom [bottom 0]
              #:type-of-material [type-of-material (default-slab-type-of-material)]
              #:material [material (cond [(eq? type-of-material "Basic") "GENERIC - INTERNAL CLADDING"]
                                         [(eq? type-of-material "Composite") "Generic Slab/Roof"])]
              #:parcs [parcs (list)]
              #:layer [layer (default-layer)]
              #:reference [reference (default-slab-reference)]
              ;#:sub-polygons [sub-polygons (list (length guide))]
              )
  (let* ((slab-info (prepare-points guide))
         (slab-msg (slabmessage* #:level bottom
                                 #:material material
                                 #:thickness thickness
                                 #:type type-of-material
                                 #:bottomlevel (storyinfo-index bottom-level)
                                 #:subpolygons (cadr slab-info)
                                 #:pts (prepare-points-to-send (car slab-info))
                                 #:parcs (prepare-arcs-to-send parcs)
                                 #:layer layer
                                 #:reference reference)))
    (write-msg "NewSlab" slab-msg)  
    ;(send-points guide)
    ;(send-points (car slab-info))
    ;(read-guid)
    (read-material-guid)))

(define (read-material-guid)
  (let ((result (read-guid-aux)))
    (if (and (elementid-crashmaterial result) 
             (crash-on-no-material?))
        (begin 
          (disconnect)
          (error "The material does not exist"))
        (elementid-guid result))))


#|
Function to create a hole on a slab
 listpoints: list of points that define the hole
             IMPORTANT: the list must end on the point that it began 
                        so it is a closed slab
 listarcs: list of eventual angles that will be applied to the hole 
           can be empty
 returns: slab id
Example of usage: 
(send (hole-slab (slab slabPoints) hole-points))
|#

(define (hole id pts arcs type)
  (let ((msg (holemsg* #:guid id
                       #:pts (prepare-points-to-send (close-guide pts (car pts)))
                       #:arcs (prepare-arcs-to-send arcs)
                       #:type type)))
    (send/rcv-id "HoleSlab" msg)))

(define (hole-slab slab-id pts [arcs (list)])
  (hole slab-id pts arcs 0))

(define (hole-roof roof-id pts [arcs (list)])
  (hole roof-id pts arcs 1))


#|
Function to create walls from a Slab
 slab-id: id of the slab from where the walls will be created
 height: height of the walls that will be created
 material: material of the walls that will be created
 returns: a list with all the id's of the wall that were created
Example of usage: 
(send (walls-from-slab slabId 5.0))
|#

(define (internal-walls-from-slab slab-id height thickness material reference-line type layer)
  (let ((ele-id-msg (wallsfromslab* #:guid slab-id #:height height #:thickness thickness #:material material #:type type #:referenceline reference-line #:layer layer)))
    (write-msg "WallsSlab" ele-id-msg)
    ;(elementidlist-guid (read-guids))
    (let ((result (read-guids*)))
      (if (and (elementidlist-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error "The material does not exist"))
          (elementidlist-guid result)))))

(define walls-from-slab-material-default #;(make-parameter "GENERIC - STRUCTURAL")(make-parameter "Glass"))
(define walls-from-slab-reference-line-default (make-parameter "Center"))
(define (walls-from-slab slab-id
                         [height (default-level-to-level-height)]
                         #:thickness [thickness 0.3]
                         #:material [material (walls-from-slab-material-default)]
                         #:reference-line [reference-line (walls-from-slab-reference-line-default)]
                         #:layer [layer (default-layer)])
  (internal-walls-from-slab slab-id height thickness material reference-line "BasicStructure" layer))

#|
Function to create walls from a Slab, using composite materials
 slab-id: id of the slab from where the walls will be created
 height: height of the walls that will be created
 material: material of the walls that will be created
 returns: a list with all the id's of the wall that were created
Example of usage: 
(send (walls-from-slab slabId 5.0))
|#
(define walls-from-slab-composite-material-default (make-parameter "Generic Wall/Shell"))
(define walls-from-slab-composite-reference-line-default (make-parameter "Center"))
(define (walls-from-slab-composite slab-id
                                   [height (default-level-to-level-height)]
                                   #:thickness [thickness 0.3]
                                   #:material [material (walls-from-slab-composite-material-default)]
                                   #:reference-line [reference-line (walls-from-slab-composite-reference-line-default)]
                                   #:layer [layer (default-layer)])
  (internal-walls-from-slab slab-id height thickness material reference-line "CompositeStructure" layer))

#|HEIGHT NOT WORKING
Function to create curtain walls from a Slab
 slab-id: id of the slab from where the curtain walls will be created
 height: height of the curtain walls that will be created
 returns: a list with all the id's of the curtain wall that were created
Example of usage: 
(send (walls-from-slab slabId))
|#
(define (cwalls-from-slab slabId height)
  (let ((ele-id-msg (elementid* #:guid slabId
                                #:crashmaterial #f)))
    (write-msg "CWallsSlab" ele-id-msg)
    (send-double height)
    (read-guid)))

#|
Function to create a column
 orig-pos: origin of column
 circle-based?: circle column or not
 angle: rotation of the column on its on axis
 depth: size of y-axis
 width: size of x-axis
Example of usage: 
(send (column (xy 0 0)))
(send (column (xy 0 0) #:slant-angle (/ pi 4)))
(send (column (xy 0 0) #:slant-angle (/ pi 4) #:slant-direction (/ pi 2)))
|#
(define (column orig-pos
                #:bottom-level [bottom-level (current-level)]
                #:top-level [top-level (upper-level bottom-level)]
                ;;ArchiCAD ONLY --------------------------------------------------------------
                #:top-linked? [top-linked? (default-top-link)]
                #:circle-based? [circle-based? #f]
                #:angle [angle 0]
                #:depth [depth 0.15]
                #:width [width 0.15]
                #:slant-angle [slant-angle (/ pi 2)]
                #:slant-direction [slant-direction 0]
                #:height [height null]
                #:profile-name [profile-name ""]
                #:bottom-offset [bottom-offset 0]
                #:layer [layer (default-layer)])
  (let ((msg (if (null? height)
                 (columnmsg*  #:posx (cx orig-pos)
                              #:posy (cy orig-pos)
                              #:bottom (cz orig-pos)
                              #:circlebased circle-based?
                              #:angle angle
                              #:depth depth
                              #:width width
                              #:slantangle slant-angle
                              #:slantdirection slant-direction
                              #:bottomindex (storyinfo-index bottom-level)
                              #:upperindex (storyinfo-index top-level)
                              #:profilename profile-name
                              #:layer layer
                              #:toplinked top-linked?)
                 (columnmsg*  #:posx (cx orig-pos)
                              #:posy (cy orig-pos)
                              #:bottom (cz orig-pos)
                              #:height height
                              #:circlebased circle-based?
                              #:angle angle
                              #:depth depth
                              #:width width
                              #:slantangle slant-angle
                              #:slantdirection slant-direction
                              #:bottomindex (storyinfo-index bottom-level)
                              #:upperindex (storyinfo-index top-level)
                              #:profilename profile-name
                              #:layer layer
                              #:toplinked top-linked?))))
    (write-msg "NewColumn" msg)
    ;(read-guid)
    (read-material-guid)))
(define (column-two-points p1 p2
                #:bottom-level [bottom-level (current-level)]
                ;;ArchiCAD ONLY --------------------------------------------------------------
                #:circle-based? [circle-based? #f]
                #:angle [angle 0]
                #:depth [depth 0.15]
                #:width [width 0.15]
                #:profile-name [profile-name ""]
                #:layer [layer (default-layer)]
                #:top-linked? [top-linked? (default-top-link)])
  (let ((msg (columnmsg*  #:posx (cx p1)
                          #:posy (cy p1)
                          #:bottom (cz p1)
                          #:height (abs (- (cz p2)(cz p1)))
                          #:circlebased circle-based?
                          #:angle angle
                          #:depth depth
                          #:width width
                          #:slantangle (- pi/2 (sph-psi (p-p p2 p1)))
                          #:slantdirection  (- (sph-phi (p-p p2 p1)) pi/2) 
                          #:bottomindex (storyinfo-index bottom-level)
                          #:upperindex (storyinfo-index bottom-level)
                          #:profilename profile-name
                          #:layer layer
                          #:toplinked top-linked?)))
    (write-msg "NewColumn" msg)
    ;(read-guid)
    (read-material-guid)))

;(send (columns-from-slab (slab (list (xy -1 -1)(xy 1 -1)(xy 1 1)(xy -1 1)(xy -1 -1))) 5))
(define columns-from-slab-material-default (make-parameter "GENERIC - STRUCTURAL"))
(define (columns-from-slab slab
                           height
                           #:depth [depth 0.15]
                           #:width [width 0.15]
                           #:circle-based? [circle-based? #t]
                           #:material [material (columns-from-slab-material-default)]
                           #:layer [layer (default-layer)])
  (let ((msg (columnsfromslab*  #:guid slab
                                #:height height
                                #:circlebased circle-based?
                                #:depth depth
                                #:width width
                                #:material material
                                #:layer layer)))
    (write-msg "ColumnsSlab" msg)
    ;(read-guid)
    (read-material-guid)))
(define default-beam-profile (make-parameter ""))
(define (beam p0
              p1
              #:beam-height [beam-height 0.15]
              #:beam-width [beam-width 0.15]
              #:bottom-level [bottom-level (current-level)]
              #:material [material "GENERIC - STRUCTURAL"]
              #:profile [profile (default-beam-profile)]
              #:layer [layer (default-layer)])
  (let* ((new-p0 (loc-in-world p0))
         (new-p1 (loc-in-world p1))
         (msg (beammsg* #:x0 (cx new-p0)
                        #:y0 (cy new-p0)
                        #:x1 (cx new-p1)
                        #:y1 (cy new-p1)
                        #:beamheight beam-height
                        #:beamwidth beam-width
                        #:levelheight (cz new-p0)
                        #:bottomlevel (storyinfo-index bottom-level)
                        #:angle (- pi/2 (sph-psi (p-p p1 p0)))
                        #:material material
                        #:profilename profile
                        #:layer layer)))
    (write-msg "Beam" msg)
    ;(read-guid)
    (read-material-guid)))

(define (split-params-list property-lst)
  (let ((names (list))
        (int-values (list))
        (double-values (list))
        (string-values (list))
        (bool-values (list))
        (lst-int-values (list))
        (lst-double-values (list))
        (lst-string-values (list))
        (lst-bool-values (list))
        (param-types (list))
        (is-array? (list))
        (store? #t))
    (when (not (empty? property-lst))
        (begin (for ([name property-lst]
                     [value (cdr property-lst)])
                    (if store?
                        (begin
                          (set! names (append names (list name)))
                          (if (list? value)
                              (begin
                                (set! is-array? (append is-array? (list #t)))
                                (cond [(string? (car value))
                                       (begin
                                         (set! param-types (append param-types (list "s")))
                                         (set! lst-string-values (append lst-string-values (list (stringarray* #:lst value)))))]
                                      [(real? (car value))
                                       (begin
                                         (set! param-types (append param-types (list "d")))
                                         (set! lst-double-values (append lst-double-values (list (doublearray* #:lst value)))))]
                                      [(integer? (car value))
                                       (begin
                                         (set! param-types (append param-types (list "i")))
                                         (set! lst-int-values (append lst-int-values (list (intarray* #:lst value)))))]
                                      [(boolean? (car value))
                                       (begin
                                         (set! param-types (append param-types (list "b")))
                                         (set! lst-bool-values (append lst-bool-values (list (boolarray* #:lst value)))))]))
                              (begin
                                (set! is-array? (append is-array? (list #f)))
                                (cond [(string? value)
                                       (begin
                                         (set! param-types (append param-types (list "s")))
                                         (set! string-values (append string-values (list value))))]
                                      [(real? value)
                                       (begin
                                         (set! param-types (append param-types (list "d")))
                                         (set! double-values (append double-values (list value))))]
                                      [(integer? value)
                                       (begin
                                         (set! param-types (append param-types (list "i")))
                                         (set! int-values (append int-values (list value))))]
                                      [(boolean? value)
                                       (begin
                                         (set! param-types (append param-types (list "b")))
                                         (set! bool-values (append bool-values (list value))))])))
                          (set! store? #f))
                        (set! store? #t)))))
               (list names int-values double-values string-values bool-values lst-int-values lst-double-values lst-string-values lst-bool-values param-types is-array?)))


#;(define (split-params-list lsst)
  (let ((names (list))
        (int-values (list))
        (double-values (list))
        (string-values (list))
        (bool-values (list))
        (lst-int-values (list))
        (lst-double-values (list))
        (lst-string-values (list))
        (lst-bool-values (list))
        (param-types (list))
        (is-array? (list)))
    (for ([lst lsst])
         (let ((name (car lst))
               (value (cadr lst)))
           (set! names (append names (list name)))
           (if (list? value)
               (begin
                 (set! is-array? (append is-array? (list #t)))
                 (cond [(string? (car value))
                        (begin
                          (set! param-types (append param-types (list "s")))
                          (set! lst-string-values (append lst-string-values (list (stringarray* #:lst value)))))]
                       [(real? (car value))
                        (begin
                          (set! param-types (append param-types (list "d")))
                          (set! lst-double-values (append lst-double-values (list (doublearray* #:lst value)))))]
                       [(integer? (car value))
                        (begin
                          (set! param-types (append param-types (list "i")))
                          (set! lst-int-values (append lst-int-values (list (intarray* #:lst value)))))]
                       [(boolean? (car value))
                        (begin
                          (set! param-types (append param-types (list "b")))
                          (set! lst-bool-values (append lst-bool-values (list (boolarray* #:lst value)))))]))
               (begin
                 (set! is-array? (append is-array? (list #f)))
                 (cond [(string? value)
                        (begin
                          (set! param-types (append param-types (list "s")))
                          (set! string-values (append string-values (list value))))]
                       [(real? value)
                        (begin
                          (set! param-types (append param-types (list "d")))
                          (set! double-values (append double-values (list value))))]
                       [(integer? value)
                        (begin
                          (set! param-types (append param-types (list "i")))
                          (set! int-values (append int-values (list value))))]
                       [(boolean? value)
                        (begin
                          (set! param-types (append param-types (list "b")))
                          (set! bool-values (append bool-values (list value))))])))))
    (list names int-values double-values string-values bool-values lst-int-values lst-double-values lst-string-values lst-bool-values param-types is-array?)))




#|
Function to create a object
 index: index that indentifies what object will be used (needs better documentation)
 orig-pos: position of the object
Example of usage: 
(send (object 1324 (xy 0.0 0.0)))
(send (object "Dormer Pitched 18" (xy 0.0 0.0) #:additional-parameters (list (list "gs_roofang_deg" pi/2))))
|#
(define (object index/name
                orig-pos
                #:level [level (current-level)]
                #:use-xy-fix-size? [use-xy-fix-size? #f]
                #:x-ratio [x-ratio 1]
                #:y-ratio [y-ratio 1]
                #:use-obj-sect-attrs? [use-obj-sect-attrs? #t]
                #:angle [angle 0]
                #:height [height 0]
                #:properties [properties (list)]
                #:layer [layer (default-layer)])
  (let* ((splitted-list (split-params-list properties))
         (msg (if (string? index/name)
                  (objectmsg* #:index 0
                              #:posx (cx orig-pos)
                              #:posy (cy orig-pos)
                              #:usexyfixsize use-xy-fix-size?
                              #:useobjsectattrs use-obj-sect-attrs?
                              #:xratio x-ratio
                              #:yratio y-ratio
                              #:bottom height
                              #:angle angle
                              #:level (storyinfo-index level)
                              #:params (additionalparams* #:names (list-ref splitted-list 0)
                                                          #:integers (list-ref splitted-list 1)
                                                          #:doubles (list-ref splitted-list 2)
                                                          #:strings (list-ref splitted-list 3)
                                                          #:booleans (list-ref splitted-list 4)
                                                          #:intarrays (list-ref splitted-list 5)
                                                          #:doublearrays (list-ref splitted-list 6)
                                                          #:stringarrays (list-ref splitted-list 7)
                                                          #:boolarrays (list-ref splitted-list 8)
                                                          #:paramtype (list-ref splitted-list 9)
                                                          #:isarray (list-ref splitted-list 10))
                              #:layer layer
                              #:name index/name)
                  (objectmsg* #:index index/name
                              #:posx (cx orig-pos)
                              #:posy (cy orig-pos)
                              #:usexyfixsize use-xy-fix-size?
                              #:useobjsectattrs use-obj-sect-attrs?
                              #:xratio x-ratio
                              #:yratio y-ratio
                              #:bottom height
                              #:angle angle
                              #:level (storyinfo-index level)
                              #:params (additionalparams* #:names (list-ref splitted-list 0)
                                                          #:integers (list-ref splitted-list 1)
                                                          #:doubles (list-ref splitted-list 2)
                                                          #:strings (list-ref splitted-list 3)
                                                          #:booleans (list-ref splitted-list 4)
                                                          #:intarrays (list-ref splitted-list 5)
                                                          #:doublearrays (list-ref splitted-list 6)
                                                          #:stringarrays (list-ref splitted-list 7)
                                                          #:boolarrays (list-ref splitted-list 8)
                                                          #:paramtype (list-ref splitted-list 9)
                                                          #:isarray (list-ref splitted-list 10))
                              #:layer layer))))
    (write-msg "Object" msg)
    (read-guid)
    ))

#|
Function to create stairs
 index: index that indentifies what stairs will be used (needs better documentation)
 orig-pos: position of the stairs

(send (stairs "Stair L-Shape 18" (u0) #:additional-parameters (list (list "stairSlabThk" 2))))
(send (stairs "Stair Spiral 18" (u0) #:additional-parameters (list (list "angle" (/ (* 10 2pi) 360))
                                                                     (list "zzyzx" 0.3)
                                                                     (list "nRisers" 3)
                                                                     (list "iShowRailingOn" 4)
                                                                     (list "bShowRailAboveBreakLine" #f)
                                                                     (list "bShowRailOnFloorPlan" #f))))
|#
(define (stairs name 
                orig-pos 
                #:angle [angle 0] 
                #:x-ratio [x-ratio 1] 
                #:y-ratio [y-ratio 1]
                #:bottom-offset [bottom-offset 0] 
                #:bottom-level [bottom-level (current-level)]
                #:top-level [top-level (upper-level bottom-level)]
                #:use-xy-fix-size [use-xy-fix-size #f]
                #:properties [properties (list)]
                #:layer [layer (default-layer)]
                #:height [height 0]
                ;Does NOT work because API does not support it
                #:top-linked? [top-linked? (default-top-link)])
  (let* ((splitted-list (split-params-list properties))
         (msg (stairsmsg* #:name name
                          #:posx (cx orig-pos)
                          #:posy (cy orig-pos)
                          #:bottom bottom-offset
                          #:xratio x-ratio
                          #:yratio y-ratio
                          #:angle angle
                          #:bottomindex (storyinfo-index bottom-level)
                          #:upperindex (storyinfo-index top-level)
                          #:usexyfixsize use-xy-fix-size
                          #:params (additionalparams* #:names (list-ref splitted-list 0)
                                                      #:integers (list-ref splitted-list 1)
                                                      #:doubles (list-ref splitted-list 2)
                                                      #:strings (list-ref splitted-list 3)
                                                      #:booleans (list-ref splitted-list 4)
                                                      #:intarrays (list-ref splitted-list 5)
                                                      #:doublearrays (list-ref splitted-list 6)
                                                      #:stringarrays (list-ref splitted-list 7)
                                                      #:boolarrays (list-ref splitted-list 8)
                                                      #:paramtype (list-ref splitted-list 9)
                                                      #:isarray (list-ref splitted-list 10))
                          #:layer layer
                          #:height height
                          #:toplink top-linked?)))
    ;(list names int-values double-values string-values bool-values lst-int-values lst-double-values lst-string-values lst-bool-values param-types is-array?)
    (write-msg "Stairs" msg)
    ;(read-guid)
    (read-material-guid)))
#|
Function to create a library part
At the moment does not return anything. It would be more interesting than returning an idex, to be able to use the name given to the library part.
Example:
 (send (library-part "Test Library Part 1"
                            "PROJECT2 3, 270, 2 \r\n"
                            "MATERIAL mat \r\n BLOCK a, b, 1 \r\n ADD a * 0.5, b* 0.5, 1 \r\n CYLIND zzyzx - 3, MIN (a, b) * 0.5 \r\n ADDZ zzyzx - 3 \r\n CONE 2, MIN (a, b) * 0.5, 0.0, 90, 90 \r\n"
                            #:parameter-code "VALUES \"zzyzx\" RANGE [6,]"))
This example uses \r\n, newline for windows, it also works with \n...

To use the created library part, reference by name!
(send (object "Test Library Part 1" (u0)))
|#
(define (library-part name
                      2D-section
                      3D-section
                      #:master-code [master-code ""]
                      #:parameter-code [parameter-code ""]
                      #:type [type "Object"]
                      #:parent-id [parent-id "ModelElement"]
                      #:properties [properties (list)])
  (let* ((splitted-list (split-params-list properties))
         (msg (libpartmsg* #:name name
                           #:twocode 2D-section
                           #:threecode 3D-section
                           #:mastercode master-code
                           #:parametercode parameter-code
                           #:type type
                           #:parentid parent-id
                           #:names (list-ref splitted-list 0)
                           #:integers (list-ref splitted-list 1)
                           #:doubles (list-ref splitted-list 2)
                           #:strings (list-ref splitted-list 3)
                           #:booleans (list-ref splitted-list 4)
                           #:intarrays (list-ref splitted-list 5)
                           #:doublearrays (list-ref splitted-list 6)
                           #:stringarrays (list-ref splitted-list 7)
                           #:boolarrays (list-ref splitted-list 8)
                           #:paramtype (list-ref splitted-list 9)
                           #:isarray (list-ref splitted-list 10))))
    (write-msg "LibraryPart" msg)
    name
    ))
#|
Function to create a plane roof
 listpoints: list with the points that define the roof shape
 height: height of the roof
 listarcs: list with angles between two consecutive points
           ex: (list (* 90 DEGRAD) (* 45 DEGRAD)), this means
               between the first and second points of the roof there is an angle of 90o
               and between the second and third points one of 45o
Example of usage: 
(send (roof slabPoints 3))
|#
(define default-roof-alignment (make-parameter "Center"))
(define default-roof-type-of-material (make-parameter "Basic"))
#;(define default-roof-material  
    (make-parameter (cond [(eq? (default-roof-type-of-material) "Basic") "GENERIC - STRUCTURAL"]
                          [(eq? (default-roof-type-of-material) "Composite") "Generic Roof/Shell"])))
(define (roof guide
              #:bottom-level [bottom-level (current-level)]
              ;;ArchiCAD ONLY --------------------------------------------------------------
              #:thickness [thickness 0.3]
              #:height [height 0]
              #:type-of-material [type-of-material (default-roof-type-of-material)]
              #:material [material (cond [(eq? type-of-material "Basic") "GENERIC - STRUCTURAL"]
                                         [(eq? type-of-material "Composite") "Generic Roof/Shell"])]
              #:layer [layer (default-layer)])
  (let ((roof-info (prepare-points guide))
        (roof-msg (roofmsg* #:height height
                            #:material material
                            #:thickness thickness
                            #:type type-of-material
                            #:bottomlevel (storyinfo-index bottom-level)
                            #:layer layer)))
    (write-msg "NewRoof" roof-msg)  
    (send-points (car roof-info))
    ;(read-guid)
    (read-material-guid)))
#|
Function to create a poly roof
|#
(define (internal-poly-roof listpoints bottom-level height listarcs thickness levels-angle levels-height material type layer)
  (let* ((msg (roofmsg* #:height height
                        #:material material
                        #:thickness thickness
                        #:type type
                        #:bottomlevel (storyinfo-index bottom-level)
                        #:layer layer))
         (roof-levels-msg (rooflevelsmsg* #:angle levels-angle
                                          #:height levels-height))
         (sub-poly-list (get-sub-polys listpoints))
         (sub-poly-msg (intlistmsg* #:ilist sub-poly-list)))
    
    (write-msg "PolyRoof" msg)
    (send-points (flatten listpoints))
    (send-arcs listarcs)
    (let ((output (connection-out (bim-connection))))
      (write-sized serialize sub-poly-msg output)
      (write-sized serialize roof-levels-msg output))
    ;(read-guid)
    (read-material-guid)
    ))

(define poly-roof-material-default (make-parameter "GENERIC - STRUCTURAL"))
(define roof-material-default (make-parameter "GENERIC - STRUCTURAL"))
(define (poly-roof listpoints
                   #:bottom-level [bottom-level (current-level)]
                   #:height [height 0]
                   #:listarcs [listarcs (list)]
                   #:thickness [thickness 0.3]
                   #:levels-angle [levels-angle (list)]
                   #:levels-height [levels-height (list)]
                   #:material [material (roof-material-default)]
                   #:layer [layer (default-layer)])
  (internal-poly-roof listpoints bottom-level height listarcs thickness levels-angle levels-height material "BasicStructure") layer)

(define poly-roof-composite-material-default (make-parameter "Generic Roof/Shell"))
(define roof-composite-material-default (make-parameter "Generic Roof/Shell"))
(define (poly-roof-composite listpoints
                             #:bottom-level [bottom-level (current-level)]
                             #:height [height 0]
                             #:listarcs [listarcs (list)]
                             #:thickness [thickness 0.3]
                             #:levels-angle [levels-angle (list)]
                             #:levels-height [levels-height (list)]
                             #:material [material (roof-composite-material-default)]
                             #:layer [layer (default-layer)])
  (internal-poly-roof listpoints bottom-level height listarcs thickness levels-angle levels-height material "CompositeStructure") layer)

;(send (poly-roof (list (list (xy 10 10)(xy 10 -10)(xy -10 -10)(xy -10 10)(xy 10 10))(list (xy 5 0)(xy -5 0)(xy 5 0))) 0 (list 0 0 0 0 0 pi pi)))


(define default-mesh-material 
  (make-parameter "GENERIC - ENVIRONMENT"))
#|
(send (mesh (list (xyz 0 0 0)(xyz 5 0 5)(xyz 5 5 0) (xyz 0 5 0)(xyz 0 0 0))))
(send (mesh (list (xyz 0 0 0)(xyz 10 0 0)(xyz 10 10 0)(xyz 0 10 0)(xyz 0 0 0))))
(send (mesh (list (xyz 0 0 0)(xyz 5 0 0)(xyz 10 0 0)(xyz 10 5 0)(xyz 10 10 0)(xyz 5 10 0)(xyz 0 10 0)(xyz 0 5 0)(xyz 0 0 0))))
(send (mesh (list (xyz 0 0 0)(xyz 10 0 0)(xyz 10 10 0)(xyz 0 10 0)(xyz 0 0 0))
                     #:level-lines (list (xyz 2 2 2)(xyz 8 2 2)(xyz 8 8 2)(xyz 2 8 2))))
|#
(define (mesh guide
              #:bottom-level [bottom-level (current-level)]
              ;;ArchiCAD ONLY --------------------------------------------------------------
              #:bottom [bottom 0]
              #:material [material (default-mesh-material)]
              #:level-lines [level-lines (list)]
              #:override-material [override-material null]
              #:layer [layer (default-layer)])
  (let ((msg (if (null? override-material)
                      (meshmessage* #:level bottom
                                    #:material material
                                    #:bottomlevel (storyinfo-index bottom-level)
                                    #:layer layer)
                      (meshmessage* #:level bottom
                                    #:material material
                                    #:bottomlevel (storyinfo-index bottom-level)
                                    #:overridematerial override-material
                                    #:layer layer))))
    (write-msg "Mesh" msg)  
    (send-points guide)
    (send-points level-lines)
    ;(read-guid)
    (read-material-guid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions to manipulate objects;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| NOT WORKING
Function to add arcs to an Element

Example of usage: 
(send (add-arcs elementId cArcs))
|#
(define (add-arcs id listarcs)
  (let ((ele-id-msg (elementid* #:guid id
                                #:crashmaterial #f)))
    (write-msg "AddArcs" ele-id-msg)
    (send-arcs listarcs)
    (read-guid)
    ))

#|NOT WORKING
Function to translate an element
Receives a point that represents the translation and the object ID
Example of usage: 
(send (translate-element id (xyz 0 0 10))
|#
(define (translate-element ID point)
  (let ((t-msg (translatemsg* #:tx (cx point)
                              #:ty (cy point)
                              #:tz (cz point)
                              #:guid ID)))
    (write-msg "Translate" t-msg)
    ;;(read-guid)
    ))
#|
Function to rotate an element on the z-axis
 id: id of the element to rotate
     IMPORTANT: Working with slabs and columns at the moment.
 angle: angle of the rotation in radians
Example of usage: 
(send (rotate-element-z (slab slabPoints (list)) (* 45 DEGRAD)))
|#
(define (rotate-element-z ID angle [copy #f])
  (define eleList (if (list? ID) ID (list ID)))
  (let ((r-msg (rotatemsg* #:guid eleList
                           #:axis "z"
                           #:angle angle
                           #:copy copy)))
    (write-msg "RotateZ" r-msg)
    (define return-list (elementidlist-guid (read-guids*)))
    (if (equal? (length return-list) 1) (car return-list) return-list)
    ;(read-guid)
    ))

#|
Function to mirror an element on the x-axis
|#
(define (mirror-element-x ID [copy #t])
  (send/rcv-id "Mirror" (mirrormsg* #:guid ID
                                    #:axis "x"
                                    #:copy copy)))
#|
Function to mirror an element on the y-axis
|#
(define (mirror-element-y ID [copy #t])
  (send/rcv-id "Mirror" (mirrormsg* #:guid ID
                                    #:axis "y"
                                    #:copy copy)))

#|
Function to trim an element
Receives the ID of two elements to trim 
Example of usage: (trim-element idWall idSlab)
|#
(define (trim-elements ids [shell-ids (list)])
  (send/no-rcv "Trim" (trimmsg* #:guids ids
                                #:guids2 shell-ids)))

#|
Function to intersect a wall with an element
 ID1: id of the element that will suffer the changes (wall)
 ID2: id of the element that may suffer changes (depends on destructive)
 destructive: If #t, will destroy both elements, and the result of the operation will be the intersection
              If #f, the second element will remain intact. Useful for the construction of Absolute Towers
Example of usage: 
(send (intersect-wall (wall (xy -15 0) (xy 15 0) 3.0) (slab slabPoints (list))))

|#
(define (intersect-wall ID1 ID2 [destructive #f])
  (let ((i-msg (intersectmsg* #:guid1 ID1
                              #:guid2 ID2)))
    (if destructive (write-msg "DestructiveIntersectWall" i-msg)(write-msg "IntersectWall" i-msg))
    (read-guid)
    ))
#|
Function to create a profile.
At the moment profiles are only supported by walls.
The name of the profile is what is used to identify it, and the function returns that name.
Example:
(send (wall (list (x 0)(x 10)) #:profile-name (profile "curved" (list (xy 0 0)(xy 5 0)(xy 5 5)(xy 3 5)) #:arcs (list 0 0 0 pi/2))))
|#
(define (profile name
                 points
                 #:arcs [arcs (list)]
                 #:material [material "GENERIC - STRUCTURAL"])
  (send/no-rcv "Profile" (profilemsg* #:pts (prepare-points-to-send (append points (list (car points))))
                                      #:arcs (prepare-arcs-to-send arcs)
                                      #:material material
                                      #:name name)))

#|
(send (profile "test 1" (list (xy 0 0)(xy 5 0)(xy 5 5)(xy 0 5)))
        (add-hole-profile "test 1" (list (xy 2 2)(xy 3 2)(xy 3 3)(xy 2 3)))
        (wall (list (x 0)(x 5)) #:profile-name "test 1"))
|#
(define (add-hole-profile name
                          points
                          #:arcs [arcs (list)]
                          #:material [material "GENERIC - STRUCTURAL"])
  (send/no-rcv "AddToProfile" (profilemsg* #:pts (prepare-points-to-send (append points (list (car points))))
                                           #:arcs (prepare-arcs-to-send arcs)
                                           #:material material
                                           #:name name)))



#|
Function to delete list of elements
 elem-id: id of the element to be deleted, or list of ids to be deleted
Example of usage: 
(send (delete-elements elemID))                  
(send (delete-elements elemListID))

TODO: delete a door that was deleted before because the wall was deleted
|#
(define (delete-elements elem-id)
  (define eleList (if (list? elem-id) elem-id (list elem-id)))
  (let ((msg (elementidlist* #:guid eleList
                             #:crashmaterial #f)))
    (write-msg "Delete" msg)))

(define (render file-name)
  (send/no-rcv "Render" (rendermsg* #:file file-name)))

(define (group elements)
  (send/rcv-id "Group" (elementidlist* #:guid elements
                                       #:crashmaterial #f)))

(define (delete-all-elements)
  (write-msg-name "DeleteAll"))


;; CAMERA
(define (camera c-pos t-pos lens [sun-azimuth pi] [sun-altitude pi/4])
  (send/no-rcv "Camera" (cameramsg* #:cx (cx c-pos)
                                    #:cy (cy c-pos)
                                    #:cz (cz c-pos)
                                    #:tx (cx t-pos)
                                    #:ty (cy t-pos)
                                    #:tz (cz t-pos)
                                    #:lens lens
                                    #:sunazimuth sun-azimuth
                                    #:sunaltitude sun-altitude)))