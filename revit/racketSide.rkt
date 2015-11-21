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

(define (move-addon-files)
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
  (displayln "done!"))

(move-addon-files)

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
  (let rec((n 10))
    (with-handlers ((exn:fail? (lambda (e)
                                 (displayln "Reinicie o Revit")
                                 (sleep 10)
                                 (if (> n 0)
                                     (rec (- n 1))
                                     (raise e)))))
      (call-with-values(lambda () (tcp-connect server-addr 53800))
                       (lambda (a b)
                         (set! input a)
                         (set! output b)
                         (file-stream-buffer-mode input 'none)
                         (file-stream-buffer-mode output 'none)))
      (set! current-level (make-parameter (get-level 0 "Level 1")))
      (delete-level "Level 2"))))

(define (connect-to-revit-family)
  (let rec((n 10))
    (with-handlers ((exn:fail? (lambda (e)
                                 (displayln "Reinicie o Revit")
                                 (sleep 10)
                                 (if (> n 0)
                                     (rec (- n 1))
                                     (raise e)))))
      (call-with-values(lambda () (tcp-connect server-addr 53800))
                       (lambda (a b)
                         (set! input a)
                         (set! output b)
                         (file-stream-buffer-mode input 'none)
                         (file-stream-buffer-mode output 'none))))))

(define (close-ports)
  (close-input-port input)
  (close-output-port output))

;;Rosetta functions

(define (box p0 p1)
  (let ((h (- (cz p1) (cz p0)))
        (c (- (cy p1) (cy p0))))
    (write-sized serialize (namestrc* #:name "box") output)
    (write-sized serialize (boxstrc* #:p0coordx (cx p0)
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
                                      #:height h) output)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define (boxb p l c h)
  (write-sized serialize (namestrc* #:name "box") output)
  (write-sized serialize (boxstrc* #:p0coordx (cx p)
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
                                    #:height h) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (cylinder p r h)
  (write-sized serialize (namestrc* #:name "cylinder") output)
  (write-sized serialize (cylinderstrc* #:p0coordx (cx p)
                                         #:p0coordy (cy p)
                                         #:p0coordz (cz p)
                                         #:radius r
                                         #:height h) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (cylinderb p0 r p1)
  (write-sized serialize (namestrc* #:name "cylinderb") output)
  (write-sized serialize (cylinderbstrc* #:p0coordx (cx p0)
                                          #:p0coordy (cy p0)
                                          #:p0coordz (cz p0)
                                          #:radius r
                                          #:p1coordx (cx p1)
                                          #:p1coordy (cy p1)
                                          #:p1coordz (cz p1)) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (sphere p r)
  (write-sized serialize (namestrc* #:name "sphere") output)
  (write-sized serialize (spherestrc* #:p0coordx (- (cx p) r)
                                       #:p0coordy (cy p)
                                       #:p0coordz (cz p)
                                       #:p1coordx (+ (cx p) r)
                                       #:p1coordy (cy p)
                                       #:p1coordz (cz p)
                                       #:p2coordx (cx p)
                                       #:p2coordy (+ (cy p) r)
                                       #:p2coordz (cz p)) output)
  (polyidstrc-ids (read-sized (cut deserialize (polyidstrc*) <>) input)))

(define (union id1 id2 . ids)
  (let ((l (list)))
    (set! l (append l (list id1)))
    (set! l (append l (list id2)))
    (set! l (append l ids))
    ))
    
    ;;(write-sized serialize (namestrc* #:name "union") output)
    ;;(write-sized serialize (polyidstrc* #:ids l) output)))

;;;;;;;;;;;;;;;;;;;;BIM Function;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (test-new) 
  (write-sized serialize (namestrc* #:name "test") output))

(define (wall p0 p1 level)
  (write-sized serialize (namestrc* #:name "wall") output)
  (write-sized serialize (wallstrc* #:p0coordx (cx p0)
                                    #:p0coordy (cy p0)
                                    #:p0coordz (cz p0)
                                    #:p1coordx (cx p1)
                                    #:p1coordy (cy p1)
                                    #:p1coordz (cz p1)
                                    #:level level) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (wall-h p0 p1 h level)
  (write-sized serialize (namestrc* #:name "wallH") output)
  (write-sized serialize (wallheightstrc* #:p0coordx (cx p0)
                                    #:p0coordy (cy p0)
                                    #:p0coordz (cz p0)
                                    #:p1coordx (cx p1)
                                    #:p1coordy (cy p1)
                                    #:p1coordz (cz p1)
                                    #:height h
                                    #:level level) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (wall-l p0 p1 levelb levelt)
  (write-sized serialize (namestrc* #:name "wallL") output)
  (write-sized serialize (walllevelstrc* #:p0coordx (cx p0)
                                         #:p0coordy (cy p0)
                                         #:p0coordz (cz p0)
                                         #:p1coordx (cx p1)
                                         #:p1coordy (cy p1)
                                         #:p1coordz (cz p1)
                                         #:levelb levelb
                                         #:levelt levelt) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (poly-wall pts levelb levelt)
  (let ((l (convert-list pts)))
    (write-sized serialize (namestrc* #:name "polyWall") output)
    (write-sized serialize (polywallstrc* #:pts l
                                          #:levelb levelb
                                          #:levelt levelt) output)
    (read-sized (cut deserialize (polyidstrc*) <>) input)))


(define (curtain-wall p0 p1 p2 p3 level)
  (write-sized serialize (namestrc* #:name "curtainWall") output)
  (write-sized serialize (curtainwallstrc* #:p0coordx (cx p0)
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
                                           #:level level) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (mass-wall p0 p1 p2 p3 height level)
  (write-sized serialize (namestrc* #:name "massWall") output)
  (write-sized serialize (masswallstrc* #:bottomleftcornerx (cx p0)
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
                                        #:level-id level) output)
  (read-sized (cut deserialize (idstrc*) <>) input))



(define (insert-door id loc)
  (write-sized serialize (namestrc* #:name "insertDoor") output)
  (write-sized serialize (insertdoorstrc* #:hostid (idstrc-id id)
                                          #:p0coordx (cx loc)
                                          #:p0coordy (cy loc)
                                          #:p0coordz (cz loc))output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (insert-door-relative id deltaX deltaY)
  (write-sized serialize (namestrc* #:name "insertDoor1") output)
  (write-sized serialize (insertdoorbstrc* #:hostid (idstrc-id id)
                                          #:deltax deltaX
                                          #:deltay deltaY) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (insert-window id deltaX deltaY)
  (write-sized serialize (namestrc* #:name "insertWindow") output)
  (write-sized serialize (insertwindowstrc* #:hostid (idstrc-id id)
                                          #:deltax deltaX
                                          #:deltay deltaY) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (delete-element elem)
  (write-sized serialize (namestrc* #:name "deleteElement") output)
  (write-sized serialize elem output))

(define (create-level height name)
  (write-sized serialize (namestrc* #:name "createLevel") output)
  (write-sized serialize (levelstrc* #:h height #:name name) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (upper-level #:level [level (current-level)]
                     #:height [height (default-level-to-level-height)])
  (write-sized serialize (namestrc* #:name "upperLevel") output)
  (write-sized serialize (upperlevelstrc* #:current level 
                                          #:elevation height) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (get-level height name)
  (write-sized serialize (namestrc* #:name "getLevel") output)
  (write-sized serialize (levelstrc* #:h height #:name name) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (delete-level name)
  (write-sized serialize (namestrc* #:name "deleteLevel") output)
  (write-sized serialize (namestrc* #:name name) output))

(define (create-round-floor center radius level)
  (write-sized serialize (namestrc* #:name "createRoundFloor") output)
  (write-sized serialize (roundfloorstrc* #:radius radius
                                          #:center-x (cx center)
                                          #:center-y (cy center)
                                          #:center-z (cz center)
                                          #:level level)output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (create-floor p0 p1 level)
  (write-sized serialize (namestrc* #:name "createFloor") output)
  (write-sized serialize (floorstrc* #:p0coordx (cx p0)
                                     #:p0coordy (cy p0)
                                     #:p0coordz (cz p0)
                                     #:p1coordx (cx p1)
                                     #:p1coordy (cy p1)
                                     #:p1coordz (cz p1)
                                     #:level level) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (column p0 blevel tlevel)
  (write-sized serialize (namestrc* #:name "createColumn") output)
  (write-sized serialize (columnstrc* #:p0coordx (cx p0)
                                      #:p0coordy (cy p0)
                                      #:p0coordz (cz p0)
                                      #:baselevel blevel
                                      #:toplevel tlevel) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (create-floor-opening p0 p1 floor)
  (write-sized serialize (namestrc* #:name "createOpening") output)
  (write-sized serialize (flooropeningstrc* #:p0coordx (cx p0)
                                            #:p0coordy (cy p0)
                                            #:p0coordz (cz p0)
                                            #:p1coordx (cx p1)
                                            #:p1coordy (cy p1)
                                            #:p1coordz (cz p1)
                                            #:floorid (idstrc-id floor)) output))


(define (floor-from-points lista level)
  (let ((l (convert-list lista)))
    (write-sized serialize (namestrc* #:name "createFloorFromPoints") output)
    (write-sized serialize (polylinefloorstrc* #:floor level 
                                               #:points l) output)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define (create-stairs-run blevel tlevel bp0 bp1 tp0 tp1)
  (write-sized serialize (namestrc* #:name "stairsRun") output)
  (write-sized serialize (stairrunstrc* #:bottom-level blevel
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
                                        #:topp1coordz (cz tp1)) output)
  (read-sized (cut deserialize (idstrc*) <>) input))



(define (intersect-wall-floor idw idf)
  (write-sized serialize (namestrc* #:name "intersectWF") output)
  (write-sized serialize idw output)
  (write-sized serialize idf output)
 (read-sized (cut deserialize (idstrc*) <>) input))

(define (disconnect-from-revit)
  (write-sized serialize (namestrc* #:name "disconnect") output)
  (close-ports))

;;;;;;;;New 2.0 Operator ;;;;;;;;;;;;;;;


(define (create-wall guide #:bottom-level[bottom-level (current-level)] #:top-level[top-level (upper-level #:level bottom-level)])
  (let ((pts (convert-list guide)))
    (write-sized serialize (namestrc* #:name "polyWall") output)
    (write-sized serialize (polywallstrc* #:pts pts
                                          #:levelb bottom-level
                                          #:levelt top-level) output)
    (polyidstrc-ids (read-sized (cut deserialize (polyidstrc*) <>) input))))

(define (create-slab guide #:bottom-level [bottom-level (current-level)])
  (let ((pts (convert-list guide)))
    (write-sized serialize (namestrc* #:name "createFloorFromPoints") output)
    (write-sized serialize (polylinefloorstrc* #:floor bottom-level 
                                               #:points pts) output)
    (read-sized (cut deserialize (idstrc*) <>) input)))


(define (create-roof guide #:bottom-level[bottom-level (current-level)])
 (let ((pts (convert-list guide)))
    (write-sized serialize (namestrc* #:name "createRoof") output)
    (write-sized serialize (polylinefloorstrc* #:floor bottom-level 
                                               #:points pts) output)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define (create-walls-from-slab slab-id height #:bottom-level[bottom-level (current-level)])
  (write-sized serialize (namestrc* #:name "wallsFromSlabs") output)
  (write-sized serialize (wallsfromslabsstrc* #:slabid slab-id
                                              #:blevel bottom-level
                                              #:height height) output)
  (polyidstrc-ids (read-sized (cut deserialize (polyidstrc*) <>) input)))

(define (create-hole-slab slab-id points)
  (let ((pts (convert-list points)))
    (write-sized serialize (namestrc* #:name "holeSlab") output)
    (write-sized serialize (holeslabstrc* #:slabid slab-id
                                          #:pts pts) output)))

(define (create-column center #:bottom-level[bottom-level (current-level)] #:top-level[top-level (upper-level #:level bottom-level)])
  (write-sized serialize (namestrc* #:name "createColumn") output)
  (write-sized serialize (columnstrc* #:p0coordx (cx center)
                                      #:p0coordy (cy center)
                                      #:p0coordz (cz center)
                                      #:baselevel bottom-level
                                      #:toplevel top-level) output)
  (read-sized (cut deserialize (idstrc*) <>) input))


(define (intersect-wall idw idf)
  (write-sized serialize (namestrc* #:name "intersectWF") output)
  (write-sized serialize idw output)
  (write-sized serialize idf output)
 (read-sized (cut deserialize (idstrc*) <>) input))


(define (current-level-elevation)
  (write-sized serialize (namestrc* #:name "levelElevation") output)
  (write-sized serialize (current-level) output)
  (doublestrc-height (read-sized (cut deserialize (doublestrc*) <>) input)))

(define (create-railings slabid)
  (write-sized serialize (namestrc* #:name "createRailings") output)
  (write-sized serialize (railingsstrc* #:slabid slabid) output))

(define (get-wall-volume wallid)
  (write-sized serialize (namestrc* #:name "getWallVolume") output)
  (write-sized serialize wallid output)
  (doublevolumestrc-volume (read-sized (cut deserialize (doublevolumestrc*) <>) input)))

(define (create-stairs blevel tlevel bp tp)
  (write-sized serialize (namestrc* #:name "createStairs") output)
  (write-sized serialize (stairstrc* #:bottom-level blevel
                                     #:top-level tlevel
                                     #:bottomp0coordx (cx bp)
                                     #:bottomp0coordy (cy bp)
                                     #:bottomp0coordz (cz bp)
                                     #:topp0coordx (cx tp)
                                     #:topp0coordy (cy tp)
                                     #:topp0coordz (cz tp)) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (levels-info)
  (write-sized serialize (namestrc* #:name "getLevelsInfo") output)
  (polylevelstrc-levels (read-sized (cut deserialize (polylevelstrc*) <>) input)))

(define (walls-info)
  (write-sized serialize (namestrc* #:name "getWallsInfo") output)
  (polywallinfostrc-walls (read-sized (cut deserialize (polywallinfostrc*) <>) input)))

(define (create-topo-surface points)
  (let ((pts (convert-list points)))
    (write-sized serialize (namestrc* #:name "createTopoSurface") output)
    (write-sized serialize (toposurfacestrc* #:pts pts) output)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define (create-building-pad points level)
  (let ((pts (convert-list points)))
    (write-sized serialize (namestrc* #:name "createBuildingPad") output)
    (write-sized serialize (buildingpadstrc* #:pts pts
                                             #:level-id level) output)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define (get-level-by-name name)
  (write-sized serialize (namestrc* #:name "getLevelByName") output)
  (write-sized serialize (namestrc* #:name name) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

(define (highlight-element id)
  (write-sized serialize (namestrc* #:name "highlightElement") output)
  (write-sized serialize id output))

(define (get-selected-element)
  (write-sized serialize (namestrc* #:name "getSelectedElement") output)
  (polyidstrc-ids (read-sized (cut deserialize (polyidstrc*) <>) input)))

(define (mass-sweep profile1 path profile2)
  (let ((prof1 (convert-list profile1))
        (prof2 (convert-list profile2))
        (pth (convert-list path)))
    (write-sized serialize (namestrc* #:name "createMassSweep") output)
    (write-sized serialize (masssweepstrc* #:profile1 prof1
                                          #:path pth
                                          #:profile2 prof2) output)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define (extrusion-mass points elevation)
  (let ((pts (convert-list points)))
    (write-sized serialize (namestrc* #:name "createExtrusionMass") output)
    (write-sized serialize (extrusionstrc* #:pts pts
                                           #:elevation elevation) output)
    (read-sized (cut deserialize (idstrc*) <>) input)))

(define (import-dwg file)
  (write-sized serialize (namestrc* #:name "importDWG") output)
  (write-sized serialize (namestrc* #:name file) output))

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
                                       #:stairsrunid stairsId) output)
  (read-sized (cut deserialize (idstrc*) <>) input))

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
                                        #:level-id level) output)
  (read-sized (cut deserialize (idstrc*) <>) input))