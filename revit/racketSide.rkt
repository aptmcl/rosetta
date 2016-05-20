#lang racket

;;(require (except-in (planet aml/rosetta) box cylinder sphere surface-grid union))
(require "../base/coord.rkt")
(require "../base/connection.rkt")
(require "rosetta/protobuf1/protobuf.rkt")
(require "rosetta/protobuf1/encoding.rkt")
(require srfi/26)
(require "pbContracts.rkt")
(require racket/date)

(require racket/runtime-path)

(provide (all-defined-out))

(define-runtime-path addin "RosettaToRevit.addin")
(define-runtime-path dll "RosettaToRevit.dll")
(define-runtime-path google "Google.ProtocolBuffers.dll")
(define-runtime-path proto "protobuf-net.dll")

;;;;;;;Installation;;;;;;;;;;;;;;;;;;;;;

(define moved-addon-files? #f)

(define (move-addon-files)
  (define (safe-move from todir)
    (when (file-exists? from)
      (let-values ([(path suffix ignore) (split-path from)])
        (let ((to (build-path todir suffix)))
          (copy-file from to #t)
          (delete-file from)))))
  (unless moved-addon-files?
    (display "Checking plugin...")
    (if (directory-exists? "C:\\ProgramData\\Autodesk\\Revit\\Addins\\2015")
        (begin
          (safe-move addin "C:\\ProgramData\\Autodesk\\Revit\\Addins\\2015")
          (safe-move google "C:\\Autodesk")
          (safe-move proto "C:\\Autodesk")
          (safe-move dll "C:\\Autodesk")
          (set! moved-addon-files? #t)
          (displayln "done!"))
        (displayln "I could not find Revit 2015. Are you sure it is installed?"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define conn #f)

(define (bim-connection)
  (unless conn
    (error "Did you forget to initialize the connection?"))
  conn)

;;;;;;;Conversions;;;;;;;;;;;;;;;;;;;;;;

(define (mt measure)
  (* measure 3.28084))

(define (ft measure)
  (measure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide idstrc*) ;;HACK Martelada

(define current-level (make-parameter #f))

(define default-level-to-level-height (make-parameter (mt 3)))

(define default-beam-family (make-parameter (idstrc* #:id 0)))

(define default-wall-family (make-parameter (idstrc* #:id 0)))

(define default-slab-family (make-parameter (idstrc* #:id 0)))

(define default-roof-family (make-parameter (idstrc* #:id 0)))

(define default-column-family (make-parameter (idstrc* #:id 0)))

(define default-door-family (make-parameter (idstrc* #:id 0)))

(define server-addr "localhost")

(define (connect-to-revit)
  (move-addon-files)
  (let rec((n 10))
    (with-handlers ((exn:fail? (lambda (e)
                                 (displayln "Please, start the Revit->Rosetta plugin.")
                                 (sleep 10)
                                 (if (> n 0)
                                     (rec (- n 1))
                                     (raise e)))))
      (let-values([(in out) (tcp-connect server-addr 53800)])
        (file-stream-buffer-mode in 'none)
        (file-stream-buffer-mode out 'none)
        (set! conn (connection in out))
        ;;I'm not sure this is the correct place to do this.
        ;;I just know it must run before closing the socket
        #;   (plumber-add-flush! (current-plumber)
                                 (lambda (e)
                                   (plumber-flush-handle-remove! e)
                                   (disconnect-from-revit))))
      (when (project-document?)
        (set! current-level (make-parameter (get-level 0 "Level 1")))
        (delete-level "Level 2"))))
  (void))

(define (project-document?)
  (write-sized serialize (namestrc* #:name "isProject") (connection-out (bim-connection)))
  (boolstrc-answer (read-sized (cut deserialize (boolstrc*) <>) (connection-in (bim-connection)))))

;;Rosetta functions

(define-syntax-rule
  (send/no-rcv name body ...)
  (let ((output (connection-out (bim-connection))))
    (write-sized serialize (namestrc* #:name name) output)
    (write-sized serialize body output) ...))

(define-syntax-rule
  (send/rcv-id name body ...)
  (let ((input (connection-in (bim-connection))))
    (send/no-rcv name body ...)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define-syntax-rule
  (send/rcv-polyid name body ...)
  (let ((input (connection-in (bim-connection))))
    (send/no-rcv name body ...)
    (polyidstrc-ids (read-sized (cut deserialize (polyidstrc*) <>) input))))

(define (box p0 p1)
  (let ((h (- (cz p1) (cz p0)))
        (c (- (cy p1) (cy p0))))
    (send/rcv-id "box"
                 (boxstrc* #:p0coordx (cx p0)
                           #:p0coordy (cy p0)
                           #:p0coordz (cz p0)
                           #:p1coordx (cx p0)
                           #:p1coordy (+ (cy p0) c)
                           #:p1coordz (cz p0)
                           #:p2coordx (cx p1)
                           #:p2coordy (cy p1)
                           #:p2coordz (- (cz p1) h)
                           #:p3coordx (cx p1)
                           #:p3coordy (- (cy p1) c)
                           #:p3coordz (- (cz p1) h)
                           #:height h))))

(define (boxb p l c h)
  (send/rcv-id "box"
               (boxstrc* #:p0coordx (cx p)
                         #:p0coordy (cy p)
                         #:p0coordz (cz p)
                         #:p1coordx (cx p)
                         #:p1coordy (+ (cy p) c)
                         #:p1coordz (cz p)
                         #:p2coordx (+ (cx p) l)
                         #:p2coordy (+ (cy p) c)
                         #:p2coordz (cz p)
                         #:p3coordx (+ (cx p) l)
                         #:p3coordy (cy p)
                         #:p3coordz (cz p)
                         #:height h)))

(define (cylinder p r h)
  (send/rcv-id "cylinder"
               (cylinderstrc* #:p0coordx (cx p)
                              #:p0coordy (cy p)
                              #:p0coordz (cz p)
                              #:radius r
                              #:height h)))

(define (cylinderb p0 r p1)
  (send/rcv-id "cylinderb"
               (cylinderbstrc* #:p0coordx (cx p0)
                               #:p0coordy (cy p0)
                               #:p0coordz (cz p0)
                               #:radius r
                               #:p1coordx (cx p1)
                               #:p1coordy (cy p1)
                               #:p1coordz (cz p1))))

(define (cylinder-metric p0 r p1)
  (send/rcv-id "cylinderMetric"
               (cylinderbstrc* #:p0coordx (cx p0)
                               #:p0coordy (cy p0)
                               #:p0coordz (cz p0)
                               #:radius r
                               #:p1coordx (cx p1)
                               #:p1coordy (cy p1)
                               #:p1coordz (cz p1))))

(define (sphere p r)
  (send/rcv-polyid "sphere"
                   (spherestrc* #:p0coordx (- (cx p) r)
                                #:p0coordy (cy p)
                                #:p0coordz (cz p)
                                #:p1coordx (+ (cx p) r)
                                #:p1coordy (cy p)
                                #:p1coordz (cz p)
                                #:p2coordx (cx p)
                                #:p2coordy (+ (cy p) r)
                                #:p2coordz (cz p))))

(define (sphere-metric p r)
  (send/rcv-id "sphereMetric"
                   (spherestrc* #:p0coordx (- (cx p) r)
                                #:p0coordy (cy p)
                                #:p0coordz (cz p)
                                #:p1coordx (+ (cx p) r)
                                #:p1coordy (cy p)
                                #:p1coordz (cz p)
                                #:p2coordx (cx p)
                                #:p2coordy (+ (cy p) r)
                                #:p2coordz (cz p))))

;;;;;;;;;;;;;;;;;;;;BIM Function;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wall p0 p1 level)
  (send/rcv-id "wall"
               (wallstrc* #:p0coordx (cx p0)
                          #:p0coordy (cy p0)
                          #:p0coordz (cz p0)
                          #:p1coordx (cx p1)
                          #:p1coordy (cy p1)
                          #:p1coordz (cz p1)
                          #:level level)))

(define (wall-h p0 p1 h level)
  (send/rcv-id "wallH"
               (wallheightstrc* #:p0coordx (cx p0)
                                #:p0coordy (cy p0)
                                #:p0coordz (cz p0)
                                #:p1coordx (cx p1)
                                #:p1coordy (cy p1)
                                #:p1coordz (cz p1)
                                #:height h
                                #:level level)))

(define (wall-l p0 p1 levelb levelt)
  (send/rcv-id "wallL"
               (walllevelstrc* #:p0coordx (cx p0)
                               #:p0coordy (cy p0)
                               #:p0coordz (cz p0)
                               #:p1coordx (cx p1)
                               #:p1coordy (cy p1)
                               #:p1coordz (cz p1)
                               #:levelb levelb
                               #:levelt levelt)))

(define (curtain-wall p0 p1 ucoords vcoords base-level top-level)
  (let ((ucoordsdouble (convert-list ucoords))
        (vcoordsdouble (convert-list vcoords)))
    (send/rcv-id "curtainWall"
                 (curtainwallstrc* #:p0coordx (cx p0)
                                   #:p0coordy (cy p0)
                                   #:p0coordz (cz p0)
                                   #:p1coordx (cx p1)
                                   #:p1coordy (cy p1)
                                   #:p1coordz (cz p1)
                                   #:ulinecoord ucoordsdouble
                                   #:vlinecoord vcoordsdouble
                                   #:baselevel base-level
                                   #:toplevel top-level))))

(define (mass-wall p0 p1 p2 p3 height level)
  (send/rcv-id "massWall"
               (masswallstrc* #:bottomleftcornerx (cx p0)
                              #:bottomleftcornery (cy p0)
                              #:bottomleftcornerz (cz p0)
                              #:topleftcornerx (cx p1)
                              #:topleftcornery (cy p1)
                              #:topleftcornerz (cz p1)
                              #:bottomrightcornerx (cx p3)
                              #:bottomrightcornery (cy p3)
                              #:bottomrightcornerz (cz p3)
                              #:toprightcornerx (cx p2)
                              #:toprightcornery (cy p2)
                              #:toprightcornerz (cz p2)
                              #:height height
                              #:level-id level)))

(define (insert-door id loc #:family[family (default-door-family)])
  (send/rcv-id "insertDoor"
               (insertdoorstrc* #:hostid (idstrc-id id)
                                #:p0coordx (cx loc)
                                #:p0coordy (cy loc)
                                #:p0coordz (cz loc)
                                #:family family)))

(define (insert-door-relative id deltaX deltaY #:family[family (default-door-family)])
  (send/rcv-id "insertDoor1"
               (insertdoorbstrc* #:hostid (idstrc-id id)
                                 #:deltax deltaX
                                 #:deltay deltaY
                                 #:family family)))

(define (insert-window id deltaX deltaY)
  (send/rcv-id "insertWindow"
               (insertwindowstrc* #:hostid (idstrc-id id)
                                          #:deltax deltaX
                                          #:deltay deltaY)))

(define (delete-element elem)
  (send/no-rcv "deleteElement" elem))

(define (create-level #:height [height 0])
  (send/rcv-id "createLevel"
               (doublestrc* #:height height)))

(define (upper-level #:level [level (current-level)]
                     #:height [height (default-level-to-level-height)])
  (send/rcv-id "upperLevel"
               (upperlevelstrc* #:current level 
                                #:elevation height)))

(define (get-level height name)
  (send/rcv-id "getLevel"
               (levelstrc* #:h height #:name name)))

(define (delete-level name)
  (send/no-rcv "deleteLevel" (namestrc* #:name name)))

(define (create-round-floor center radius level)
  (send/rcv-id "createRoundFloor"
               (roundfloorstrc* #:radius radius
                                #:center-x (cx center)
                                #:center-y (cy center)
                                #:center-z (cz center)
                                #:level level)))

(define (create-floor p0 p1 level)
  (send/rcv-id "createFloor"
               (floorstrc* #:p0coordx (cx p0)
                           #:p0coordy (cy p0)
                           #:p0coordz (cz p0)
                           #:p1coordx (cx p1)
                           #:p1coordy (cy p1)
                           #:p1coordz (cz p1)
                           #:level level)))

(define (create-floor-opening p0 p1 floor)
  (send/no-rcv "createOpening"
               (flooropeningstrc* #:p0coordx (cx p0)
                                  #:p0coordy (cy p0)
                                  #:p0coordz (cz p0)
                                  #:p1coordx (cx p1)
                                  #:p1coordy (cy p1)
                                  #:p1coordz (cz p1)
                                  #:floorid (idstrc-id floor))))

(define (create-stairs-run blevel tlevel bp0 bp1 tp0 tp1)
  (send/no-rcv "stairsRun"
               (stairrunstrc* #:bottom-level blevel
                              #:top-level tlevel
                              #:bottomp0coordx (cx bp0)
                              #:bottomp0coordy (cy bp0)
                              #:bottomp0coordz (cz bp0)
                              #:bottomp1coordx (cx bp1)
                              #:bottomp1coordy (cy bp1)
                              #:bottomp1coordz (cz bp1)
                              #:topp0coordx (cx tp0)
                              #:topp0coordy (cy tp0)
                              #:topp0coordz (cz tp0)
                              #:topp1coordx (cx tp1)
                              #:topp1coordy (cy tp1)
                              #:topp1coordz (cz tp1))))

(define (intersect-wall-floor idw idf)
  (send/rcv-id "intersectWF" idw idf))

(define (disconnect-from-revit)
  (send/no-rcv "disconnect")
  (shutdown-connection (bim-connection))
  (void))

;;;;;;;;New 2.0 Operator ;;;;;;;;;;;;;;;


(define (create-wall guide #:bottom-level[bottom-level (current-level)] #:top-level[top-level (upper-level #:level bottom-level)] #:family[family (default-wall-family)])
  (let ((pts (convert-list guide)))
    (send/rcv-polyid "polyWall"
                     (polywallstrc* #:pts pts
                                    #:levelb bottom-level
                                    #:levelt top-level
                                    #:familyid family))))

(define (create-slab guide #:bottom-level [bottom-level (current-level)] #:family[family (default-slab-family)])
  (let ((pts (convert-list guide)))
    (send/rcv-id "createFloorFromPoints"
                 (polylinefloorstrc* #:floor bottom-level 
                                     #:points pts
                                     #:familyid family))))


(define (create-roof guide #:bottom-level[bottom-level (current-level)] #:family [family (default-roof-family)])
  (let ((pts (convert-list guide)))
    (send/rcv-id "createRoof"
                 (polylinefloorstrc* #:floor bottom-level 
                                     #:points pts
                                     #:familyid family))))

(define (create-walls-from-slab slab-id height #:bottom-level[bottom-level (current-level)])
  (send/rcv-polyid "wallsFromSlabs"
                   (wallsfromslabsstrc* #:slabid slab-id
                                        #:blevel bottom-level
                                        #:height height)))

(define (create-hole-slab slab-id points)
  (let ((pts (convert-list points)))
    (send/no-rcv "holeSlab"
                 (holeslabstrc* #:slabid slab-id
                                #:pts pts))))

(define (create-column center #:bottom-level [bottom-level (current-level)] #:top-level [top-level (upper-level #:level bottom-level)] #:width [width 0] #:family [family (default-column-family)])
  (send/rcv-id "createColumn"
               (columnstrc* #:p0coordx (cx center)
                            #:p0coordy (cy center)
                            #:p0coordz (cz center)
                            #:baselevel bottom-level
                            #:toplevel top-level
                            #:width width
                            #:familyid family)))

(define (intersect-wall idw idf)
  (send/rcv-id "intersectWF" idw idf))

(define (current-level-elevation)
  (send/no-rcv "levelElevation"
               (current-level))
  (doublestrc-height (read-sized (cut deserialize (doublestrc*) <>)
                                 (connection-in (bim-connection)))))

(define (create-railings slabid)
  (send/no-rcv "createRailings"
               (railingsstrc* #:slabid slabid)))

(define (get-wall-volume wallid)
  (send/no-rcv "getWallVolume" wallid)
  (doublevolumestrc-volume (read-sized (cut deserialize (doublevolumestrc*) <>)
                                       (connection-in (bim-connection)))))

(define (create-stairs blevel tlevel bp tp)
  (send/rcv-id "createStairs"
               (stairstrc* #:bottom-level blevel
                           #:top-level tlevel
                           #:bottomp0coordx (cx bp)
                           #:bottomp0coordy (cy bp)
                           #:bottomp0coordz (cz bp)
                           #:topp0coordx (cx tp)
                           #:topp0coordy (cy tp)
                           #:topp0coordz (cz tp))))

(define (levels-info)
  (send/no-rcv "getLevelsInfo")
  (polylevelstrc-levels (read-sized (cut deserialize (polylevelstrc*) <>)
                                    (connection-in (bim-connection)))))

(define (walls-info)
  (send/no-rcv "getWallsInfo")
  (polywallinfostrc-walls (read-sized (cut deserialize (polywallinfostrc*) <>)
                                      (connection-in (bim-connection)))))

(define (create-topo-surface points)
  (let ((pts (convert-list points)))
    (send/rcv-id "createTopoSurface"
                 (toposurfacestrc* #:pts pts))))

(define (create-building-pad points level)
  (let ((pts (convert-list points)))
    (send/rcv-id "createBuildingPad"
                 (buildingpadstrc* #:pts pts
                                   #:level-id level))))

(define (get-level-by-name name)
  (send/rcv-id "getLevelByName"
               (namestrc* #:name name)))

(define (highlight-element id)
  (send/no-rcv "highlightElement" id))

(define (get-selected-element)
  (send/rcv-polyid "getSelectedElement"))

(define (mass-sweep profile1 path profile2)
  (let ((prof1 (convert-list profile1))
        (prof2 (convert-list profile2))
        (pth (convert-list path)))
    (send/rcv-id "createMassSweep"
                 (masssweepstrc* #:profile1 prof1
                                 #:path pth
                                 #:profile2 prof2))))

(define (extrusion-mass points elevation)
  (let ((pts (convert-list points)))
    (send/rcv-id "createExtrusionMass"
                 (extrusionstrc* #:pts pts
                                 #:elevation elevation))))

(define (import-dwg file)
  (send/no-rcv "importDWG")
  (send/no-rcv file))

(define (move-element element vector)
  (send/no-rcv "moveElement"
               (movestrc* #:element element
                          #:vectorx (cx vector)
                          #:vectory (cy vector)
                          #:vectorz (cz vector))))

(define (rotate-element element angle p0 p1)
  (send/no-rcv "rotateElement"
               (rotatestrc* #:element element
                            #:angle angle
                            #:p0x (cx p0)
                            #:p0y (cy p0)
                            #:p0z (cz p0)
                            #:p1x (cx p1)
                            #:p1y (cy p1)
                            #:p1z (cz p1))))

(define (create-beam p0 p1 family)
  (send/rcv-id "createBeam"
               (beaminfostrc* #:p0coordx (cx p0)
                              #:p0coordy (cy p0)
                              #:p0coordz (cz p0)
                              #:p1coordx (cx p1)
                              #:p1coordy (cy p1)
                              #:p1coordz (cz p1)
                              #:family family)))

(define (load-family path)
  (send/rcv-id "loadFamily"
               (namestrc* #:name path)))

(define (family-element family #:flag-override[flag-override #f] #:parameter-names[parameter-names (list)] #:parameter-values[parameter-values (list)])
  (send/rcv-id "familyElement"
               (familyelementstrc* #:familyid family
                                   #:flag flag-override
                                   #:names parameter-names
                                   #:values parameter-values)))

;;;;;;;;Auxiliary Funtions;;;;;;;;;;;;;;

(define (convert-list lista)
  (let ((l (list)))
    (for/list ((pts lista))
      (set! l (append l (list (cx pts))))
      (set! l (append l (list (cy pts))))
      (set! l (append l (list (cz pts)))))
    l))

(define (convert-matrix mat)
  (let ((m (list)))
    (for/list ((ptss mat))
      (set! m (append m (list (linestrc* #:points (convert-list ptss))))))
    m))

;;;;;;;;;;Comment;;;;;;;;;;;;;;;;
