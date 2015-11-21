#lang racket
(provide (except-out (all-defined-out)
                     ))

(require "protobuf1/protobuf.rkt")
(require "protobuf1/encoding.rkt")
(require "Messages.rkt")
(require "Communication.rkt")
(require "Geometry.rkt")
(require rosetta/revit)
(require srfi/26)

(define DEGRAD (/ pi 180.0))

(define crash-on-no-material? (make-parameter #t))
(define crash-on-no-name? (make-parameter #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions to create objects;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-wall-alignment (make-parameter "Center"))
(define default-wall-type-of-material (make-parameter "Basic"))
(define default-wall-thickness (make-parameter 0.3))
#;(define default-wall-material  
    (cond [(eq? (default-wall-type-of-material) "Basic") (make-parameter "GENERIC - STRUCTURAL")]
          [(eq? (default-wall-type-of-material) "Composite") (make-parameter "Generic Wall/Shell")]))
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
(send (create-wall (list (xy 0 0)(xy 10 0))))
(send (create-wall (list (xy 0 0)(xy 10 0)) #:type-of-profile "Slanted" #:alpha-angle (* 80 DEGRAD)))
(send (create-wall (list (xy 0 0)(xy 10 0)) #:type-of-profile "DoubleSlanted" #:alpha-angle (* 100 DEGRAD) #:beta-angle (* 80 DEGRAD)))
Question:
Make the wall always double slanted whatever the angles?
|#
(define (create-wall guide 
                     #:alignment [alignment (default-wall-alignment)]
                     #:bottom-level [bottom-level (current-level)]
                     #:top-level [top-level (upper-level #:level bottom-level)]
                     ;;ArchiCAD ONLY --------------------------------------------------------------
                     #:thickness [thickness (default-wall-thickness)]
                     #:angle [angle 0]
                     
                     #:type-of-material [type-of-material (default-wall-type-of-material)]
                     #:material [material 
                                 (cond [(eq? type-of-material "Basic") "GENERIC - STRUCTURAL"]
                                       [(eq? type-of-material "Composite") "Generic Wall/Shell"])]
                     #:alpha-angle [alpha-angle (/ pi 2)]
                     #:beta-angle [beta-angle (/ pi 2)]
                     #:type-of-profile [type-of-profile (default-wall-profile)]
                     #:height [height null])
  (let* ((p0 (x 0))
         (p1 (x 0))
         ;Don't like this if, was unable to do (unless (null? height) #:height height)
         (msg (if (null? height)
                  (wallmsg* #:p0x (cx p0)
                            #:p0y (cy p0)
                            #:p1x (cx p1)
                            #:p1y (cy p1)
                            #:bottomindex (storyinfo-index bottom-level)
                            #:upperindex (storyinfo-index top-level)
                            #:thickness thickness
                            #:angle angle
                            #:material material
                            #:type type-of-material
                            #:referenceline alignment
                            #:alphaangle alpha-angle
                            #:betaangle beta-angle
                            #:typeprofile type-of-profile)
                  (wallmsg* #:p0x (cx p0)
                            #:p0y (cy p0)
                            #:p1x (cx p1)
                            #:p1y (cy p1)
                            #:bottomindex (storyinfo-index bottom-level)
                            #:upperindex (storyinfo-index top-level)
                            #:thickness thickness
                            #:angle angle
                            #:material material
                            #:type type-of-material
                            #:referenceline alignment
                            #:alphaangle alpha-angle
                            #:betaangle beta-angle
                            #:typeprofile type-of-profile
                            #:height height))))
    (write-msg "NewWall" msg)
    (send-points guide)
    (let ((result (read-sized (cut deserialize (elementidlist*) <>)input)))
      (if (and (elementidlist-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error (string-append "The material does not exist - " material)))
          (if (null? (cdr (elementidlist-guid result)))
              (car (elementidlist-guid result))
              (elementidlist-guid result))))))

#|
Function used to create a door into an existing wall

 guid: id of wall, this value is returned by the functions that create walls
       (create-wall ...)
 objloc: object location in the wall
 zpos: z of the door

 returns: door id 

Example of usage:
(send (create-door wallId 1.0 0.0))
|#
(define (create-door guid objloc [width -10000] [bottom 0] [height -10000])
  (let ((door-to-create (doormessage* #:guid guid
                                      #:objloc objloc
                                      #:zpos bottom
                                      #:height height
                                      #:width width
                                      #:hole #f
                                      )))
    (write-msg "Door" door-to-create)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))

(define (create-hole-in-wall guid objloc [width -10000] [bottom 0] [height -10000])
  (let ((door-to-create (doormessage* #:guid guid
                                      #:objloc objloc
                                      #:zpos bottom
                                      #:height height
                                      #:width width
                                      #:hole #t
                                      )))
    (write-msg "Door" door-to-create)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))

(define (create-hole-in-wall-test guid list-points [list-arcs (list)])
  (let ((msg (holemsg* #:guid guid )))
    (write-msg "HoleTest" msg)
    (send-points list-points)
    (send-arcs list-arcs)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))

#|
Function used to create a window into a existing wall
 
 guid: id of wall, this value is returned by the functions that create walls
       (create-wall ...)
 objloc: object location in the wall
 zpos: z of the window

 returns: window id 

Example of usage:
(send (create-window wallId 1.0 1.0))
|#
(define (create-window guid objloc zpos)
  (let ((window-to-create (windowmessage* #:guid guid
                                          #:objloc objloc
                                          #:zpos zpos)))
    (write-msg "Window" window-to-create)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))

#|
Function to create a Curtain Wall
 
 TODO information
 
 returns: curtain-wall id

Example of usage: 
(send (delete-stories)(create-curtain-wall (list (xy 0 0)(xy 10 0))))
|#
(define (create-curtain-wall guide
                             #:listarcs [listarcs (list)]
                             #:bottom-level [bottom-level (current-level)]
                             #:top-level [top-level (upper-level #:level bottom-level)])
  (let ((c-wall-msg (curtainwallmsg* #:numpoints (length guide)
                                     #:numarcs (length listarcs)
                                     #:bottomindex (storyinfo-index bottom-level)
                                     #:upperindex (storyinfo-index top-level))))
    (write-msg "CurtainWall" c-wall-msg)
    (send-points guide)
    (send-arcs listarcs)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input)))
  )

#|
Function to create a Slab
 TODO information
 returns: slab id
Example of usage: 
(send (slab cPoints))

|#
(define default-slab-alignment (make-parameter "Center"))
(define default-slab-type-of-material (make-parameter "Composite"))
#;(define default-slab-material  
    (make-parameter (cond [(eq? (default-slab-type-of-material) "Basic") "GENERIC - INTERNAL CLADDING"]
                          [(eq? (default-slab-type-of-material) "Composite") "Generic Slab/Roof"])))
(define (create-slab guide
                     #:bottom-level [bottom-level (current-level)]
                     ;;ArchiCAD ONLY --------------------------------------------------------------
                     #:thickness [thickness 0.3]
                     #:bottom [bottom 0]
                     #:type-of-material [type-of-material (default-slab-type-of-material)]
                     #:material [material (cond [(eq? type-of-material "Basic") "GENERIC - INTERNAL CLADDING"]
                                                [(eq? type-of-material "Composite") "Generic Slab/Roof"])]
                     #:sub-polygons [sub-polygons (list (length guide))])
  (let ((slab-msg (slabmessage* #:level bottom
                                #:material material
                                #:thickness thickness
                                #:type type-of-material
                                #:bottomlevel (storyinfo-index bottom-level)
                                #:subpolygons sub-polygons)))
    (write-msg "NewSlab" slab-msg)  
    (send-points guide)
    ;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    (let ((result (read-sized (cut deserialize (elementid*) <>)input)))
      (if (and (elementid-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error "The material does not exist"))
          (elementid-guid result)))))

#|
Function to create a hole on a slab
 listpoints: list of points that define the hole
             IMPORTANT: the list must end on the point that it began 
                        so it is a closed slab
 listarcs: list of eventual angles that will be applied to the hole 
           can be empty
 returns: slab id
Example of usage: 
(send (create-hole-slab (create-slab slabPoints) hole-points))
|#
(define (create-hole-slab slab-id listpoints [listarcs (list)])
  (let ((slab-msg (holemsg* #:guid slab-id)))
    (write-msg "HoleSlab" slab-msg)  
    (send-points listpoints)
    (send-arcs listarcs)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))


#|
Function to create walls from a Slab
 slab-id: id of the slab from where the walls will be created
 height: height of the walls that will be created
 material: material of the walls that will be created
 returns: a list with all the id's of the wall that were created
Example of usage: 
(send (create-walls-from-slab slabId 5.0))
|#

(define (internal-create-walls-from-slab slab-id height thickness material reference-line type)
  (let ((ele-id-msg (wallsfromslab* #:guid slab-id #:height height #:thickness thickness #:material material #:type type #:referenceline reference-line)))
    (write-msg "WallsSlab" ele-id-msg)
    ;(elementidlist-guid (read-sized (cut deserialize (elementidlist*) <>)input))
    (let ((result (read-sized (cut deserialize (elementidlist*) <>)input)))
      (if (and (elementidlist-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error "The material does not exist"))
          (elementidlist-guid result)))))

(define create-walls-from-slab-material-default #;(make-parameter "GENERIC - STRUCTURAL")(make-parameter "Glass"))
(define create-walls-from-slab-reference-line-default (make-parameter "Center"))
(define (create-walls-from-slab slab-id [height (default-level-to-level-height)] #:thickness [thickness 0.3] #:material [material (create-walls-from-slab-material-default)] [reference-line (create-walls-from-slab-reference-line-default)])
  (internal-create-walls-from-slab slab-id height thickness material reference-line "BasicStructure"))

#|
Function to create walls from a Slab, using composite materials
 slab-id: id of the slab from where the walls will be created
 height: height of the walls that will be created
 material: material of the walls that will be created
 returns: a list with all the id's of the wall that were created
Example of usage: 
(send (create-walls-from-slab slabId 5.0))
|#
(define create-walls-from-slab-composite-material-default (make-parameter "Generic Wall/Shell"))
(define create-walls-from-slab-composite-reference-line-default (make-parameter "Center"))
(define (create-walls-from-slab-composite slab-id [height (default-level-to-level-height)] [thickness 0.3] [material (create-walls-from-slab-composite-material-default)] [reference-line (create-walls-from-slab-composite-reference-line-default)])
  (internal-create-walls-from-slab slab-id height thickness material reference-line "CompositeStructure"))

#|HEIGHT NOT WORKING
Function to create curtain walls from a Slab
 slab-id: id of the slab from where the curtain walls will be created
 height: height of the curtain walls that will be created
 returns: a list with all the id's of the curtain wall that were created
Example of usage: 
(send (create-walls-from-slab slabId))
|#
(define (create-cwalls-from-slab slabId height)
  (let ((ele-id-msg (elementid* #:guid slabId
                                #:crashmaterial #f)))
    (write-msg "CWallsSlab" ele-id-msg)
    (send-double height)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))

#|
Function to create a column
 orig-pos: origin of column
 circle-based?: circle column or not
 angle: angle of the column
 depth: size of y-axis
 width: size of x-axis
Example of usage: 
(send (create-column (xy 0 0)))
(send (create-column (xy 0 0) #:slant-angle (/ pi 4)))
(send (create-column (xy 0 0) #:slant-angle (/ pi 4) #:slant-direction (/ pi 2)))
|#
(define (create-column orig-pos
                       #:bottom-level [bottom-level (current-level)]
                       #:top-level [top-level (upper-level #:level bottom-level)]
                       ;;ArchiCAD ONLY --------------------------------------------------------------
                       #:circle-based? [circle-based? #t]
                       #:angle [angle 0]
                       #:depth [depth 0.15]
                       #:width [width 0.15]
                       #:slant-angle [slant-angle (/ pi 2)]
                       #:slant-direction [slant-direction 0])
  (let ((msg (columnmsg*  #:posx (cx orig-pos)
                          #:posy (cy orig-pos)
                          #:bottom 0
                          #:height 0
                          #:circlebased circle-based?
                          #:angle angle
                          #:depth depth
                          #:width width
                          #:slantangle slant-angle
                          #:slantdirection slant-direction
                          #:bottomindex (storyinfo-index bottom-level)
                          #:upperindex (storyinfo-index top-level))))
    (write-msg "NewColumn" msg)
    ;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    (let ((result (read-sized (cut deserialize (elementid*) <>)input)))
      (if (and (elementid-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error "The material does not exist"))
          (elementid-guid result)))))

;(send (create-columns-from-slab (create-slab (list (xy -1 -1)(xy 1 -1)(xy 1 1)(xy -1 1)(xy -1 -1))) 5))
(define create-columns-from-slab-material-default (make-parameter "GENERIC - STRUCTURAL"))
(define (create-columns-from-slab slab
                                  height
                                  #:depth [depth 0.15]
                                  #:width [width 0.15]
                                  #:circle-based? [circle-based? #t]
                                  #:material [material (create-columns-from-slab-material-default)] )
  (let ((msg (columnsfromslab*  #:guid slab
                                #:height height
                                #:circlebased circle-based?
                                #:depth depth
                                #:width width
                                #:material material)))
    (write-msg "ColumnsSlab" msg)
    ;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    (let ((result (read-sized (cut deserialize (elementidlist*) <>)input)))
      (if (and (elementidlist-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error "The material does not exist"))
          (elementidlist-guid result)))))

#|
Function to create a object
 index: index that indentifies what object will be used (needs better documentation)
 orig-pos: position of the object
Example of usage: 
(send (create-object 1324 (xy 0.0 0.0)))
|#
(define (create-object index
                       orig-pos
                       #:use-xy-fix-size? [use-xy-fix-size? #f]
                       #:x-ratio [x-ratio 1]
                       #:y-ratio [y-ratio 1]
                       #:use-obj-sect-attrs? [use-obj-sect-attrs? #t]
                       #:angle [angle 0]
                       #:height [height 0])
  (let ((msg (objectmsg* #:index index
                         #:posx (cx orig-pos)
                         #:posy (cy orig-pos)
                         #:usexyfixsize use-xy-fix-size?
                         #:useobjsectattrs use-obj-sect-attrs?
                         #:xratio x-ratio
                         #:yratio y-ratio
                         #:bottom height
                         #:angle angle)))
    (write-msg "Object" msg)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    ))

#|
Function to create stairs
 index: index that indentifies what stairs will be used (needs better documentation)
 orig-pos: position of the stairs
|#
(define (create-stairs #:name name 
                       #:orig-pos orig-pos 
                       #:angle [angle 0] 
                       #:x-ratio [x-ratio 1] 
                       #:y-ratio [y-ratio 1]
                       #:bottom-offset [bottom-offset 0] 
                       #:bottom-level [bottom-level (current-level)]
                       #:use-xy-fix-size [use-xy-fix-size #f])
  (let ((msg (stairsmsg* #:name name
                         #:posx (cx orig-pos)
                         #:posy (cy orig-pos)
                         #:bottom bottom-offset
                         #:xratio x-ratio
                         #:yratio y-ratio
                         #:angle angle
                         #:bottomindex (storyinfo-index bottom-level)
                         #:usexyfixsize use-xy-fix-size)))
    (write-msg "Stairs" msg)
    ;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    (let ((result (read-sized (cut deserialize (elementid*) <>)input)))
      (if (and (elementid-crashmaterial result)
               (crash-on-no-name?))
          (begin 
            (disconnect)
            (error "The name does not exist"))
          (elementid-guid result)))      
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
(send (create-roof slabPoints 3))
|#
(define default-roof-alignment (make-parameter "Center"))
(define default-roof-type-of-material (make-parameter "Composite"))
#;(define default-roof-material  
    (make-parameter (cond [(eq? (default-roof-type-of-material) "Basic") "GENERIC - STRUCTURAL"]
                          [(eq? (default-roof-type-of-material) "Composite") "Generic Roof/Shell"])))
(define (create-roof guide
                     #:bottom-level [bottom-level (current-level)]
                     ;;ArchiCAD ONLY --------------------------------------------------------------
                     #:thickness [thickness 0.3]
                     #:height [height 0]
                     #:type-of-material [type-of-material (default-roof-type-of-material)]
                     #:material [material (cond [(eq? type-of-material "Basic") "GENERIC - STRUCTURAL"]
                                                [(eq? type-of-material "Composite") "Generic Roof/Shell"])])
  (let ((roof-msg (roofmsg* #:height height
                            #:material material
                            #:thickness thickness
                            #:type type-of-material
                            #:bottomlevel (storyinfo-index bottom-level))))
    (write-msg "NewRoof" roof-msg)  
    (send-points guide)
    ;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    (let ((result (read-sized (cut deserialize (elementid*) <>)input)))
      (if (and (elementid-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error "The material does not exist"))
          (elementid-guid result)))))
#|
Function to create a poly roof
|#
(define (internal-create-poly-roof listpoints height listarcs thickness levels-angle levels-height material type)
  (let* ((msg (roofmsg* #:height height
                        #:material material
                        #:thickness thickness
                        #:type type))
         (roof-levels-msg (rooflevelsmsg* #:angle levels-angle
                                          #:height levels-height))
         (sub-poly-list (get-sub-polys listpoints))
         (sub-poly-msg (intlistmsg* #:ilist sub-poly-list)))
    
    (write-msg "PolyRoof" msg)
    (send-points (flatten listpoints))
    (send-arcs listarcs)
    (write-sized serialize sub-poly-msg output)
    (write-sized serialize roof-levels-msg output)
    ;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    (let ((result (read-sized (cut deserialize (elementid*) <>)input)))
      (if (and (elementid-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error "The material does not exist"))
          (elementid-guid result)))
    ))

(define create-poly-roof-material-default (make-parameter "GENERIC - STRUCTURAL"))
(define create-roof-material-default (make-parameter "GENERIC - STRUCTURAL"))
(define (create-poly-roof listpoints height [listarcs (list)] [thickness 0.3] [levels-angle (list)] [levels-height (list)] [material (create-roof-material-default)])
  (internal-create-poly-roof listpoints height listarcs thickness levels-angle levels-height material "BasicStructure"))

(define create-poly-roof-composite-material-default (make-parameter "Generic Roof/Shell"))
(define create-roof-composite-material-default (make-parameter "Generic Roof/Shell"))
(define (create-poly-roof-composite listpoints height [listarcs (list)] [thickness 0.3] [levels-angle (list)] [levels-height (list)] [material (create-roof-composite-material-default)])
  (internal-create-poly-roof listpoints height listarcs thickness levels-angle levels-height material "CompositeStructure"))

;(send (create-poly-roof (list (list (xy 10 10)(xy 10 -10)(xy -10 -10)(xy -10 10)(xy 10 10))(list (xy 5 0)(xy -5 0)(xy 5 0))) 0 (list 0 0 0 0 0 pi pi)))


(define default-mesh-material 
  (make-parameter "GENERIC - ENVIRONMENT"))
#|
(send (create-mesh (list (xyz 0 0 0)(xyz 5 0 5)(xyz 5 5 0) (xyz 0 5 0)(xyz 0 0 0))))
(send (create-mesh (list (xyz 0 0 0)(xyz 10 0 0)(xyz 10 10 0)(xyz 0 10 0)(xyz 0 0 0))))
(send (create-mesh (list (xyz 0 0 0)(xyz 5 0 0)(xyz 10 0 0)(xyz 10 5 0)(xyz 10 10 0)(xyz 5 10 0)(xyz 0 10 0)(xyz 0 5 0)(xyz 0 0 0))))
(send (create-mesh (list (xyz 0 0 0)(xyz 10 0 0)(xyz 10 10 0)(xyz 0 10 0)(xyz 0 0 0))
                     #:level-lines (list (xyz 2 2 2)(xyz 8 2 2)(xyz 8 8 2)(xyz 2 8 2))))
|#
(define (create-mesh guide
                     #:bottom-level [bottom-level (current-level)]
                     ;;ArchiCAD ONLY --------------------------------------------------------------
                     #:bottom [bottom 0]
                     #:material [material (default-mesh-material)]
                     #:level-lines [level-lines (list)])
  (let ((slab-msg (meshmessage* #:level bottom
                                #:material material
                                #:bottomlevel (storyinfo-index bottom-level))))
    (write-msg "Mesh" slab-msg)  
    (send-points guide)
    (send-points level-lines)
    ;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    (let ((result (read-sized (cut deserialize (elementid*) <>)input)))
      (if (and (elementid-crashmaterial result) 
               (crash-on-no-material?))
          (begin 
            (disconnect)
            (error "The material does not exist"))
          (elementid-guid result)))))

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
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
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
    ;;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    ))
#|
Function to rotate an element on the z-axis
 id: id of the element to rotate
     IMPORTANT: Working with slabs and columns at the moment.
 angle: angle of the rotation in radians
Example of usage: 
(send (rotate-element-z (create-slab slabPoints (list)) (* 45 DEGRAD)))
|#
(define (rotate-element-z ID angle [copy #f])
  (define eleList (if (list? ID) ID (list ID)))
  (let ((r-msg (rotatemsg* #:guid eleList
                           #:axis "z"
                           #:angle angle
                           #:copy copy)))
    (write-msg "RotateZ" r-msg)
    (define return-list (elementidlist-guid (read-sized (cut deserialize (elementidlist*) <>)input)))
    (if (equal? (length return-list) 1) (car return-list) return-list)
    ;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    ))

#|
Function to mirror an element on the x-axis
|#
(define (mirror-element-x ID [copy #t])
  (let ((msg (mirrormsg* #:guid ID
                         #:axis "x"
                         #:copy copy)))
    (write-msg "Mirror" msg)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))
#|
Function to mirror an element on the y-axis
|#
(define (mirror-element-y ID [copy #t])
  (let ((msg (mirrormsg* #:guid ID
                         #:axis "y"
                         #:copy copy)))
    (write-msg "Mirror" msg)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))

#|NOT WORKING
Function to trim an element
Receives the ID of two elements to trim 
Example of usage: (trim-element idWall idSlab)
|#
(define (trim-elements ID1 ID2)
  (let ((t-msg (trimmsg* #:guid1 ID1
                         #:guid2 ID2)))
    (write-msg "Trim" t-msg)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))))

#|
Function to intersect a wall with an element
 ID1: id of the element that will suffer the changes (wall)
 ID2: id of the element that may suffer changes (depends on destructive)
 destructive: If #t, will destroy both elements, and the result of the operation will be the intersection
              If #f, the second element will remain intact. Useful for the construction of Absolute Towers
Example of usage: 
(send (intersect-wall (create-wall (xy -15 0) (xy 15 0) 3.0) (create-slab slabPoints (list))))

|#
(define (intersect-wall ID1 ID2 [destructive #f])
  (let ((i-msg (intersectmsg* #:guid1 ID1
                              #:guid2 ID2)))
    (if destructive (write-msg "DestructiveIntersectWall" i-msg)(write-msg "IntersectWall" i-msg))
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    ))

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