#lang racket
(provide (except-out (all-defined-out)
                     ))

(require "protobuf1/protobuf.rkt")
(require "protobuf1/encoding.rkt")
(require "Messages.rkt")
(require "communication.rkt")
(require "objects.rkt")
(require "../base/utils.rkt"
         "../base/coord.rkt"
         "../base/shapes.rkt")
(require srfi/26)



(define (extract-list-of-points points-x points-y points-z)
  (if (null? points-x)
      (list)
      (cons (xyz (car points-x)(car points-y)(car points-z))
            (extract-list-of-points (cdr points-x)(cdr points-y)(cdr points-z)))))

(define (get-levels)
  (write-msg-name "GetLevels")
  (read-sized (cut deserialize (levelrepeated*) <>) input))

(define (get-walls)
  (write-msg-name "GetWalls")
  (read-sized (cut deserialize (wallrepeated*) <>)input))

(define (get-slabs)
  (write-msg-name "GetSlabs")
  (read-sized (cut deserialize (slabrepeated*) <>)input))

(define (get-columns)
  (write-msg-name "GetColumns")
  (read-sized (cut deserialize (columnrepeated*) <>)input))

(define (get-objects)
  (write-msg-name "GetObjects")
  (read-sized (cut deserialize (objectrepeated*) <>)input))

(define (get-roofs)
  (write-msg-name "GetRoofs")
  (read-sized (cut deserialize (roofrepeated*) <>)input))

(define (recreate-levels [level-list (get-levels)])
  (for ([n (length (levelrepeated-levels level-list))])
       (create-level #:height (storyinfo-level (list-ref (levelrepeated-levels level-list) n))))
  )

(define (recreate-walls [wall-list (get-walls)])
  (delete-elements (wallrepeated-guid wall-list))
  (for ([n (length (wallrepeated-guid wall-list))])
       ;(displayln (list-ref (wallrepeated-material wall-list) n))
       ;(displayln (list-ref (wallrepeated-type wall-list) n))
       (wall (list (xy (list-ref (wallrepeated-p0x wall-list) n)
                              (list-ref (wallrepeated-p0y wall-list) n))
                          (xy (list-ref (wallrepeated-p1x wall-list) n)
                              (list-ref (wallrepeated-p1y wall-list) n)))
                    #:alignment (list-ref (wallrepeated-referenceline wall-list) n)
                    #:bottom-level (list-ref (wallrepeated-bottomlevel wall-list) n)
                    #:thickness (list-ref (wallrepeated-thickness wall-list) n)
                    #:angle (list-ref (wallrepeated-angle wall-list) n)
                    #:top-level (list-ref (wallrepeated-toplevel wall-list) n)
                    #:type-of-material (list-ref (wallrepeated-type wall-list) n)
                    #:material (list-ref (wallrepeated-material wall-list) n)
                    #:alpha-angle (list-ref (wallrepeated-alphaangle wall-list) n)
                    #:beta-angle (list-ref (wallrepeated-betaangle wall-list) n)
                    #:type-of-profile (list-ref (wallrepeated-typeprofile wall-list) n))))

(define (recreate-slabs [slab-list (get-slabs)])
  (delete-elements (slabrepeated-guid slab-list))
  (for ([n (length (slabrepeated-guid slab-list))])
       (let ((points (list-ref (slabrepeated-points slab-list) n)))
         ;(displayln (list-ref (slabrepeated-type slab-list) n))
         ;(displayln (list-ref (slabrepeated-material slab-list) n))
         (slab (extract-list-of-points (pointsmessage-px points)(pointsmessage-py points)(pointsmessage-pz points))
                      #:bottom-level (list-ref (slabrepeated-bottomlevel slab-list) n)
                      #:thickness (list-ref (slabrepeated-thickness slab-list) n)
                      #:type-of-material (list-ref (slabrepeated-type slab-list) n)
                      #:material (list-ref (slabrepeated-material slab-list) n)
                      #:sub-polygons (intlistmsg-ilist (list-ref (slabrepeated-subpolygons slab-list) n))))))

(define (recreate-columns [column-list (get-columns)])
  (delete-elements (columnrepeated-guid column-list))
  (for ([n (length (columnrepeated-guid column-list))])
       (column (xy (list-ref (columnrepeated-px column-list) n)(list-ref (columnrepeated-py column-list) n))
                    #:bottom-level (list-ref (columnrepeated-bottomlevel column-list) n)
                    #:top-level (list-ref (columnrepeated-toplevel column-list) n)
                    #:circle-based? (list-ref (columnrepeated-circular column-list) n)
                    #:angle (list-ref (columnrepeated-angle column-list) n)
                    #:depth (list-ref (columnrepeated-depth column-list) n)
                    #:width (list-ref (columnrepeated-width column-list) n)
                    #:slant-angle (list-ref (columnrepeated-slantangle column-list) n)
                    #:slant-direction (list-ref (columnrepeated-slantdirection column-list) n))))

(define (recreate-objects [object-list (get-objects)])
  (delete-elements (objectrepeated-guid object-list))
  (for ([n (length (objectrepeated-guid object-list))])
       (if (list-ref (objectrepeated-stairs object-list) n)
       (stairs #:name (list-ref (objectrepeated-name object-list) n)
                      #:orig-pos (xy (list-ref (objectrepeated-px object-list) n)(list-ref (objectrepeated-py object-list) n))
                      #:angle (list-ref (objectrepeated-angle object-list) n)
                      #:x-ratio (list-ref (objectrepeated-xratio object-list) n)
                      #:y-ratio (list-ref (objectrepeated-yratio object-list) n)
                      #:bottom-offset (list-ref (objectrepeated-bottomoffset object-list) n)
                      #:bottom-level (list-ref (objectrepeated-bottomlevel object-list) n)
                      #:use-xy-fix-size (list-ref (objectrepeated-usexyfixsize object-list) n))
       ;(object index orig-pos)
       (list))))

(define (recreate-roofs [roof-list (get-roofs)])
  (delete-elements (roofrepeated-guid roof-list))
  (for ([n (length (roofrepeated-guid roof-list))])
       (let ((points (list-ref (roofrepeated-points roof-list) n)))
         ;(displayln (list-ref (roofrepeated-type roof-list) n))
         ;(displayln (list-ref (roofrepeated-material roof-list) n))
         (roof (extract-list-of-points (pointsmessage-px points)(pointsmessage-py points)(pointsmessage-pz points))
                      #:bottom-level (list-ref (roofrepeated-bottomlevel roof-list) n)
                      #:thickness (list-ref (roofrepeated-thickness roof-list) n)
                      #:type-of-material (list-ref (roofrepeated-type roof-list) n)
                      #:material (list-ref (roofrepeated-material roof-list) n)
                      ;Currently roofs don't have sub-polygons, i.e. holes. In the future it may be added
                      ;#:sub-polygons (intlistmsg-ilist (list-ref (roofrepeated-subpolygons roof-list) n))
                      #:height (list-ref (roofrepeated-height roof-list) n)))))
#|
Had to do change the function ClickAnElem provided by the API
It wasn't working correctly, the action of selecting an element was being
cancelled without anything occuring, I suspect that it cancelled due to
ArchiCAD not being selected.
|#
(define (select-element)
  (write-msg-name "SelectElement")
  (elementid-guid (read-sized (cut deserialize (elementid*) <>)input)))

(define (highlight-element elem-id)
  (let* ((eleList (if (list? elem-id)
                      elem-id
                      (list elem-id)))
         (msg (elementidlist* #:guid eleList
                              #:crashmaterial #f)))
    (write-msg "Highlight" msg)))