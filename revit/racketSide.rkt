#lang racket

;;(require (except-in (planet aml/rosetta) box cylinder sphere surface-grid union))
(require "../base/coord.rkt")
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
  (unless moved-addon-files?
    (display "Checking plugin...")
    (when (and (directory-exists? "C:\\ProgramData\\Autodesk\\Revit\\Addins\\2015")
               (file-exists? addin)
               (file-exists? google)
               (file-exists? proto)
               (file-exists? dll))
      (display "Installing plugin...")
      (rename-file-or-directory addin "C:\\ProgramData\\Autodesk\\Revit\\Addins\\2015\\RosettaToRevit.addin" #t)
      (rename-file-or-directory google "C:\\Autodesk\\Google.ProtocolBuffers.dll" #t)
      (rename-file-or-directory proto "C:\\Autodesk\\protobuf-net.dll" #t)
      (rename-file-or-directory dll "C:\\Autodesk\\RosettaToRevit.dll" #t))
    (displayln "done!")
    (setf moved-addon-files? #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define input #f)
(define output #f)

;;;;;;;Conversions;;;;;;;;;;;;;;;;;;;;;;

(define (mt measure)
  (* measure 3.28084))

(define (ft measure)
  (measure))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-level (make-parameter null))

(define default-level-to-level-height (make-parameter (mt 3)))

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
      (call-with-values(lambda () (tcp-connect server-addr 53800))
                       (lambda (a b)
                         (set! input a)
                         (set! output b)
                         (file-stream-buffer-mode input 'none)
                         (file-stream-buffer-mode output 'none)
                         ;;I'm not sure this is the correct place to do this.
                         ;;I just know it must run before closing the socket
                      #;   (plumber-add-flush! (current-plumber)
                                             (lambda (e)
                                               (plumber-flush-handle-remove! e)
                                               (disconnect-from-revit)))))
      (when (project-document?)
        (set! current-level (make-parameter (get-level 0 "Level 1")))
        (delete-level "Level 2"))))
  (void))

(define (project-document?)
  (write-sized serialize (namestrc* #:name "isProject") output)
  (boolstrc-answer (read-sized (cut deserialize (boolstrc*) <>) input)))

(define (close-ports)
  (close-input-port input)
  (close-output-port output))

;;Rosetta functions

(define-syntax-rule
  (send/no-rcv name body ...)
  (begin
    (write-sized serialize (namestrc* #:name name) output)
    (write-sized serialize body output) ...))

(define-syntax-rule
  (send/rcv-id name body ...)
  (begin
    (send/no-rcv name body ...)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define-syntax-rule
  (send/rcv-polyid name body ...)
  (begin
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

;;;;;;;;;;;;;;;;;;;;BIM Function;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (test-new) 
  (write-sized serialize (namestrc* #:name "test") output))

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

(define (curtain-wall p0 p1 p2 p3 level)
  (send/rcv-id "curtainWall"
               (curtainwallstrc* #:p0coordx (cx p0)
                                 #:p0coordy (cy p0)
                                 #:p0coordz (cz p0)
                                 #:p1coordx (cx p1)
                                 #:p1coordy (cy p1)
                                 #:p1coordz (cz p1)
                                 #:p2coordx (cx p2)
                                 #:p2coordy (cy p2)
                                 #:p2coordz (cz p2)
                                 #:p3coordx (cx p3)
                                 #:p3coordy (cy p3)
                                 #:p3coordz (cz p3)
                                 #:level level)))

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

(define (insert-door id loc)
  (send/rcv-id "insertDoor"
               (insertdoorstrc* #:hostid (idstrc-id id)
                                #:p0coordx (cx loc)
                                #:p0coordy (cy loc)
                                #:p0coordz (cz loc))))

(define (insert-door-relative id deltaX deltaY)
  (send/rcv-id "insertDoor1"
               (insertdoorbstrc* #:hostid (idstrc-id id)
                                 #:deltax deltaX
                                 #:deltay deltaY)))

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
                                  #:floorid (idstrc-id floor)) output))

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
  (close-ports)
  (void))

;;;;;;;;New 2.0 Operator ;;;;;;;;;;;;;;;


(define (create-wall guide #:bottom-level[bottom-level (current-level)] #:top-level[top-level (upper-level #:level bottom-level)])
  (let ((pts (convert-list guide)))
    (send/rcv-polyid "polyWall"
                     (polywallstrc* #:pts pts
                                    #:levelb bottom-level
                                    #:levelt top-level))))

(define (create-slab guide #:bottom-level [bottom-level (current-level)])
  (let ((pts (convert-list guide)))
    (send/rcv-id "createFloorFromPoints"
                 (polylinefloorstrc* #:floor bottom-level 
                                     #:points pts))))


(define (create-roof guide #:bottom-level[bottom-level (current-level)])
  (let ((pts (convert-list guide)))
    (send/rcv-id "createRoof"
                 (polylinefloorstrc* #:floor bottom-level 
                                     #:points pts))))

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

(define (create-column center #:bottom-level[bottom-level (current-level)] #:top-level[top-level (upper-level #:level bottom-level)] #:width [width 0])
  (send/rcv-id "createColumn"
               (columnstrc* #:p0coordx (cx center)
                            #:p0coordy (cy center)
                            #:p0coordz (cz center)
                            #:baselevel bottom-level
                            #:toplevel top-level
                            #:width width)))

(define (intersect-wall idw idf)
  (send/rcv-id "intersectWF" idw idf))

(define (current-level-elevation)
  (send/no-rcv "levelElevation"
               (current-level))
  (doublestrc-height (read-sized (cut deserialize (doublestrc*) <>) input)))

(define (create-railings slabid)
  (send/no-rcv "createRailings"
               (railingsstrc* #:slabid slabid) output))

(define (get-wall-volume wallid)
  (send/no-rcv "getWallVolume" wallid)
  (doublevolumestrc-volume (read-sized (cut deserialize (doublevolumestrc*) <>) input)))

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
  (polylevelstrc-levels (read-sized (cut deserialize (polylevelstrc*) <>) input)))

(define (walls-info)
  (send/no-rcv "getWallsInfo")
  (polywallinfostrc-walls (read-sized (cut deserialize (polywallinfostrc*) <>) input)))

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

(define (create-beam p0 p1 width height family wname hname)
  (send/rcv-id "createBeam"
               (beaminfostrc* #:p0coordx (cx p0)
                              #:p0coordy (cy p0)
                              #:p0coordz (cz p0)
                              #:p1coordx (cx p1)
                              #:p1coordy (cy p1)
                              #:p1coordz (cz p1)
                              #:width width
                              #:height height
                              #:family family
                              #:wname wname
                              #:hname hname)))

(define (load-family path)
  (send/rcv-id "loadFamily"
               (namestrc* #:name path)))

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

#;(define (surface-grid malha)
  (let ((m (convert-matrix malha)))
    (write-sized serialize (namestrc* #:name "surfaceGrid") output)
    (write-sized serialize (matrixstrc* #:lines m) output)))

#;(define (create-stairs-landing stairsId bottomleftp topleftp bottomrightp toprightp)
  (write-sized serialize (namestrc* #:name "stairsLanding") output)
  (write-sized serialize (landingstrc* #:bottomleftcornerx (cx bottomleftp)
                                       #:bottomleftcornery (cy bottomleftp)
                                       #:bottomleftcornerz (cz bottomleftp)
                                       #:topleftcornerx (cx topleftp)
                                       #:topleftcornery (cy topleftp)
                                       #:topleftcornerz (cz topleftp)
                                       #:bottomrightcornerx (cx bottomrightp)
                                       #:bottomrightcornery (cy bottomrightp)
                                       #:bottomrightcornerz (cz bottomrightp)
                                       #:toprightcornerx (cx toprightp)
                                       #:toprightcornery (cy toprightp)
                                       #:toprightcornerz (cz toprightp)
                                       #:stairsrunid stairsId)))

#;(define (finish-wall-face-interior wall)
  (write-sized serialize (namestrc* #:name "wallFinishFaceInterior") output)
  (write-sized serialize wall output))

#;(define (delete-all-elements)
  (write-sized serialize (namestrc* #:name "deleteAllElements") output))

#;(define (slab-wall p0 p1 p2 p3 level)
  (write-sized serialize (namestrc* #:name "slabWall") output)
  (write-sized serialize (slabwallstrc* #:bottomleftcornerx (cx p0)
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
                                        #:level-id level)))
