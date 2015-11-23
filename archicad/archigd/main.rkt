#lang racket
;(provide (except-out (all-defined-out)))

(require "Install.rkt")
(require "Communication.rkt")
(require "Geometry.rkt")
(require "BIMObjects.rkt")
(require "Messages.rkt")
(require "Inspector.rkt")
(require "protobuf1/protobuf.rkt")
(require "protobuf1/syntax.rkt")
(require "protobuf1/encoding.rkt")
(require rosetta/revit)
(require srfi/26)

(provide (all-defined-out)
         (all-from-out "Communication.rkt"
                       "Geometry.rkt"
                       "BIMObjects.rkt"
                       "Inspector.rkt"))

(define do-not-install? #f)
(define (do-not-install! bool)
  (set! do-not-install? bool))
(unless do-not-install?
  (move-addon))

(define (view-3d)
  (write-msg-name "3D"))

;(send (open-file "D:\\GitHubRep\\Tese\\Development\\Examples\\Models\\AT for eCADDe.pln"))
;(send (open-file "D:\\GitHubRep\\Tese\\Development\\Examples\\Models\\AT for eCADDe.ifc"))
;(send (open-file "C:\\Users\\Client\\Desktop\\SmallTower.ifc"))
(define (open-file path)
  (let ((msg (openmessage* #:path path
                           #:extension (last (string-split path ".")))))
    (write-msg "OpenFile" msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Function to create a Lemon
(define (create-lemon)
  (write-msg-name "Lemon"))

;;Function to create a Chapel no communication
(define (create-chapel-no-com)
  (write-msg-name "ChapelNoCom"))

;(provide (all-defined-out))

;;TEST FUNCTION
(define (test-function)
  (write-msg-name "Test")
  ;;(elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
  )
(define (test-function-msg msg)
  (write-msg "Test" msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Defines to help with Demos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tmat (list 1 0 0
                   0 0 0 
                   -1 0 0 
                   1 0 0))
(define lstpoints (list (xy 0.0 0.0) 
                        (xy 0.4 5.0) 
                        (xy 1.0 5.0)
                        (xy 1.0 6.0)
                        (xy 1.7 6.0)
                        (xy 1.7 7.0)
                        (xy 2.4 7.0)
                        (xy 2.4 7.4)
                        (xy 0.0 7.7)
                        (xy 0.0 8.0)
                        (xy 8.0 10.0)
                        (xy 12.0 0.0)
                        (xy 0.0 0.0)))
(define lstarcs (list (list 1 2 -0.143099565651258 )
                      (list 10 11 0.566476134070805)
                      (list 11 12 0.385936923743763)))
(define hpoints (list (xy -1.5 -0.3) 
                      (xy -1.5 3.1) 
                      (xy 1.5 3.1)
                      (xy 1.5 -0.3)
                      (xy -1.5 -0.3)))
(define harcs (list (list 2 3 (* -240 DEGRAD))))
(define hheight -5.2)
(define htmat (list 1 0 0 0 
                    0 1 0 0
                    0 0 1 10))

(define cPoints (list (xy 0.0 0.0)
                      (xy 0.0 10.0)
                      ;;(xy 10.0 10.0)
                      ;;(xy 10.0 0.0)
                      (xy 0.0 0.0)
                      ))

(define cArcs (list (* 90 DEGRAD)
                    (* 90 DEGRAD)
                    ))

(define (eight-spheres)
  (create-sphere 0.0 0.0 0.0001 0.0 0.0)
  (create-sphere 0.0 0.0 0.0001 0.0 1.0)
  (create-sphere 0.0 0.0 0.0001 0.0 -1.0)
  
  (create-sphere 0.0 0.0 0.0001 1.0 1.0)
  (create-sphere 0.0 0.0 0.0001 1.0 -1.0)
  
  (create-sphere 0.0 0.0 0.0001 -1.0 0.0)
  (create-sphere 0.0 0.0 0.0001 -1.0 1.0)
  (create-sphere 0.0 0.0 0.0001 -1.0 -1.0)
  )

(define slabPoints (list (xy 0.0 0.0)
                         (xy 10.0 0.0)
                         (xy 10.0 10.0)
                         (xy 0.0 10.0)
                         (xy 0.0 0.0)))

(define hole-points (list (xy 2.0 2.0)
                          (xy 8.0 2.0)
                          (xy 8.0 8.0)
                          (xy 2.0 8.0)
                          (xy 2.0 2.0)))

;;Park of trees:
;;(send (park 1324 (xy -10 -10) 10 10 8))
;;other objects id's: 1366 1362
(define (park object-index orig-pos rows columns distance-between-trees)
  (for ([c columns])
       (for ([r rows])
            (create-object object-index (xy (+ (cx orig-pos) (* r distance-between-trees)) (+ (cy orig-pos)(* c distance-between-trees)))))))

(define (see-object n until step)
  (create-object n (xy (+ (- until n) step) 0))
  (if (eq? n until)
      (list)
      (see-object (+ n 1) until (+ step 10))))

(define (create-default-object orig-pos)
  (let ((msg (objectmsg* #:index 0
                         #:posx (cx orig-pos)
                         #:posy (cy orig-pos))))
    (write-msg "DefaultObject" msg)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Examples:
;;
;; (send (write-msg-name "Test"))
;;
;; (send (create-wall (xy 0.0 0.0) (xy 3.0 0.0) 3.0) (create-wall (xy 1.0 1.0) (xy 3.0 1.0) 3.0))
;;

;using IDs
;; (send (define wallId (create-wall (xy 0.0 0.0) (xy 3.0 0.0) 3.0)) (define wallId2 (create-wall (xy 1.0 1.0) (xy 3.0 1.0) 3.0)))
;; (send (define sphereId (create-sphere 0.0 0.0 0.0001 0.0 0.0)) (define sphereId2 (create-sphere 0.0 0.0 0.0001 1.0 1.0)))

;Wall
;; (send (create-wall (xy 0.0 0.0) (xy 10.0 0.0) 10.0))
;; (send (create-wall(xy 0.0 0.0) (xy 0.0 10.0) 3.0 0.3 (* 90 DEGRAD)) (create-wall(xy 0.0 10.0) (xy 0.0 0.0) 3.0 0.3 (* 90 DEGRAD)))

;Doors
;; (send (define wallId (create-wall (xy 0.0 0.0) (xy 3.0 0.0) 3.0) )(create-door wallId 1.0 0.0) (define wallId2 (create-wall (xy 1.0 1.0) (xy 3.0 1.0) 3.0)) (create-door wallId2 1.0 1.0))
;;= (send (create-door (create-wall (xy 0.0 0.0) (xy 3.0 0.0) 3.0) 1.0 0.0) (create-story-above 10) (create-door (create-wall (xy 0.0 0.0) (xy 3.0 0.0) 3.0) 1.0 0.0))

;Window and Door
;; (send (define wallId (create-wall (xy 0.0 0.0) (xy 5.0 0.0) 3.0)) (create-window wallId 4.0 0.635) (create-door wallId 1.0 0.0))

;creating a chapel
;; (send (create-chapel))

;Shell
;; (send (create-complex-shell tmat lstpoints lstarcs 1 hpoints harcs hheight htmat 0.0 0.0))
;; (send (create-simple-shell lstpoints))
;; (send (create-shell lstpoints lstarcs))
;; (send (rotate-shell "x" 90 (create-shell lstpoints lstarcs)))
;; (send (let ((shell-id (create-shell lstpoints lstarcs)))(rotate-shell "x" 90 shell-id)(rotate-shell "y" 90 shell-id)))
;; (send (rotate-shell "y" 90 (rotate-shell "x" 90 (create-shell lstpoints lstarcs))))
;; (send (translate-shell (list 0 5 0) (create-shell lstpoints lstarcs)))
;; (send (translate-shell (list 11 11 0) (rotate-shell "x" 90 (create-shell lstpoints lstarcs))))

;Shell Hole
;; (send (create-hole-on-slab hpoints harcs hheight (translate-shell (list 11 11 0) (rotate-shell "x" 90 (create-shell lstpoints lstarcs)))))
;; (send (create-hole-on-slab hpoints harcs hheight (create-shell lstpoints lstarcs)))
;; (send (create-hole-on-slab hpoints harcs hheight (create-hole-on-slab hpoints harcs hheight (create-shell lstpoints lstarcs))))
;; (send (rotate-shell "y" 180 (create-hole-on-slab hpoints harcs hheight (create-shell lstpoints lstarcs))))
;; (send (create-hole-on-slab hpoints harcs hheight (rotate-shell "y" 180 (create-hole-on-slab hpoints harcs hheight (create-shell lstpoints lstarcs)))))

;Curtain Walls
;; (send (create-curtain-wall cPoints cArcs 5))
;;= (send (create-curtain-wall cPoints cArcs 5) (create-story-above 10) (create-curtain-wall cPoints cArcs 5))
;; (send (add-arcs (create-curtain-wall cPoints (list) 5) cArcs))
;; (send (add-arcs (add-arcs (create-curtain-wall cPoints (list) 5) cArcs) cArcs))

;Translate Curtain Wall
;; (send (translate-element (xyz 0 0 10) (create-curtain-wall cPoints cArcs 5)))

;Slab
;; (send (create-slab slabPoints (list) 0.0))
;; (send (rotate-element (create-slab slabPoints (list) 0.0) "z" (* 45 DEGRAD)))
;; (send (create-slab cPoints cArcs 0.0))

;Create walls or curtain walls on a regular slab
;; (send (create-walls-from-slab (create-slab slabPoints (list) 0.0) 5.0))
;;= (send (create-walls-from-slab (create-slab slabPoints (list) 0.0) 5.0) (create-story-above 5) (create-walls-from-slab (create-slab slabPoints (list)) 5.0))
;; (send (create-cwalls-from-slab (create-slab slabPoints (list) 0.0) 5.0))

;Create walls or curtain walls on an irregular slab
;; (send (create-walls-from-slab (create-slab slabPoints cArcs 0.0) 5.0))
;; (send (create-cwalls-from-slab (create-slab slabPoints cArcs 0.0) 5.0))

;Create Slab For Absolute Tower
;; (send (create-slab ATslab1 (list) 0.0))
;; (send (rotate-element (create-slab ATslab1 (list) 0.0) "z" (* 90 DEGRAD)))

;Trim Elements
;; (send (trim-elements (create-wall (xy -5.0 5.0) (xy 15.0 5.0) 3.0) (create-slab slabPoints (list)) ))

;PolyWall
;;(send (create-multi-wall (list (xy 0.0 0.0) (xy -10.0 0.0) (xy -10.0 10.0) (xy 0.0 10.0)) (list) 3.0 0.0 1.0))
;;;;create door into polywall
;;;; (send (define laux1 (create-multi-wall (list (xy 0.0 0.0) (xy -10.0 0.0) (xy -10.0 10.0) (xy 0.0 10.0)) (list) 3.0 0.0 1.0)))
;;;; followed by (send (create-door (car laux1) 1.0 0.0))

;True PolyWall - Problems with windows and orientation
;;(send (wallTest (list (xy 0.0 0.0) (xy -10.0 0.0) (xy -10.0 10.0) (xy 0.0 10.0)) (list) 3.0 0.0 1.0))
;;(send (wallTest (list (xy 0.0 0.0) (xy 10.0 0.0) (xy 10.0 10.0) (xy 0.0 10.0)) (list) 3.0 0.0 1.0))
;;(send (wallTest (list (xy 10.0 0.0) (xy 0.0 0.0) (xy 0.0 10.0) (xy 10.0 10.0)) (list) 3.0 0.0 1.0))

;Intersect Wall
;; (send (intersect-wall (create-wall (xy 5.0 0.0) (xy 15.0 0.0) 3.0) (create-slab slabPoints (list)) ))
;; (send (intersect-wall (create-wall (xy -5.0 5.0) (xy 15.0 5.0) 3.0) (create-slab slabPoints (list)) ))
;; (send (intersect-wall (create-wall (xy 40.0 0.0) (xy 40.0 50.0) 3.0) (create-slab slabPoints (list)) #t))

;Column
;; (send (create-column (xyz 0.0 0.0 0.0) 0.0 10.0 true 360 5.0 5.0))

;Story
;; (send (create-story-below 10.0) (create-slab slabPoints (list)) (create-story-below 10.0) (create-slab slabPoints (list)) (create-story-below 10.0) (create-slab slabPoints (list)))
;; (send (create-story-above 10.0) (create-story-above 10.0) (define current-story-information (check-story)))
;; (send (create-story-below 10.0) (create-story-below 10.0) (define current-story-information (check-story)))
;; (send (create-story 10) (create-story 20) (create-story 10))

;Delete
;; (send (delete-elements (create-wall (xy -5.0 5.0) (xy 15.0 5.0) 3.0)))
;;= (send (define wallIDAux (create-wall (xy 0.0 0.0) (xy 3.0 0.0) 3.0)) (create-door wallIDAux 1.0 0.0) (delete-elements wallIDAux))


;Mixed Tests
;; (send (create-wall (xy 0.0 0.0) (xy 3.0 0.0) 3.0) (create-curtain-wall cPoints cArcs 5) (create-story-above 10) (create-wall (xy 0.0 0.0) (xy 3.0 0.0) 3.0)(create-curtain-wall cPoints cArcs 5))



#|
(define (superellipse p a b n t)
  (+xy p (* a (expt (expt (cos t) 2) (/ 1 n)) (sgn (cos t)))
       (* b (expt (expt (sin t) 2) (/ 1 n)) (sgn (sin t)))))

(define (points-superellipse p a b n n-points)
  (map (lambda (t) (superellipse p a b n t))
       (division -pi pi n-points #f)))

(define p (xyz 0 0 0))
(define d 6.3)
(define l-corridor 2.2)
(define t 0.3)
(define r-small 15)
(define daux6 (- 6.3 l-corridor))
(define central-hole (list (+xy p (- (+ daux6 (/ t 2))) (- (+ daux6 (/ t 2))))
                           (+xy p (+ (+ daux6 (/ t 2))) (- (+ daux6 (/ t 2))))
                           (+xy p (+ (+ daux6 (/ t 2))) (+ (+ daux6 (/ t 2))))
                           (+xy p (- (+ daux6 (/ t 2))) (+ (+ daux6 (/ t 2))))
                           (+xy p (- (+ daux6 (/ t 2))) (- (+ daux6 (/ t 2))))))


(define (create-several-stories number)
  (for ([i number])
    (let* ((slab-points-aux (points-superellipse (xy 0 0) 26 21 1.75 50))
           (slab-points (append slab-points-aux (list (car slab-points-aux))))
           ;(slab-id (create-slab slab-points (list)))
           ;(slab-id (create-slab cPoints cArcs))
           (slab-id (create-slab slabPoints))
           )
      (create-walls-from-slab slab-id 10)
      ;(create-hole-slab slab-id central-hole)
      (create-hole-slab slab-id hole-points)
      ;(rotate-element-z slab-id (* i 10))
      
      (checked-create-story-above 10))))

(define (test-slab)
  (define slab-id (create-slab slabPoints))
  (create-walls-from-slab slab-id 10)
  (create-hole-slab slab-id hole-points)
  (checked-create-story-above 10)
  (set! slab-id (create-slab slabPoints))
  (create-walls-from-slab slab-id 10)
  (create-hole-slab slab-id hole-points)
  ;(delete-elements slab-id)
  )
|#

#| DIFFERENT WAY OF DOING create-slab FUNCTION - USING KEYWORDS
(define create-slab 
  (lambda (listpoints #:listarcs [listarcs (list)] #:bottomOffset [bottomOffset 0] #:material [material 60] #:thickness [thickness 0.0])
    (let ((slab-msg (slabmessage* #:level bottomOffset
                                  #:material material
                                  #:thickness thickness)))
    (write-msg "Slab" slab-msg)  
    (send-points listpoints)
    (send-arcs listarcs)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>)input)))))
|#



