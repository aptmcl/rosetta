#lang racket
(require rosetta/revit)
;(delete-all-shapes)

(define (mm l)
   (/ l 1000))
(define building-length (mm 90600))
(define building-width (mm 33600))
(define building-height (mm 13500))
(define building-floors 4)
(define level-height (/ building-height (- building-floors 1)))
(define slab-height 500)
(define column-width (mm 600))
(define bar-thickness 200)

#;(define beam-family (load-beam-family
"C:\\ProgramData\\Autodesk\\RVT 2015\\Libraries\\Portugal\\Structural
Framing\\Wood\\M_Timber.rfa" '("b" "d")))
#;(define beam-family (load-beam-family "C:\\ProgramData\\Autodesk\\RVT
2015\\Libraries\\US Metric\\Structural Framing\\Wood\\M_Timber.rfa"
'("b" "d")))

#;(define column-family (load-column-family
"C:\\ProgramData\\Autodesk\\RVT
2015\\Libraries\\Portugal\\Columns\\M_Round Column.rfa"
'("Diameter")))
#;(define column-family (load-column-family
"C:\\ProgramData\\Autodesk\\RVT 2015\\Libraries\\US
Metric\\Columns\\M_Round Column.rfa" '("Diameter")))

;Levels
(define (level-list p)
  (for/list ((h (range 0 building-height level-height)))
    (level h)))


;Columns
(define (building-columns p n m)
  (parameterize ((default-column-family (column-family-element
column-family column-width)))
    (for/list ((x (range 0 (+ (- building-length column-width) (/ (-
building-length column-width) n)) (/ (- building-length column-width)
n))))
      (for/list ((y (range 0 (+ (- building-width column-width) (/ (-
building-width column-width) m)) (/ (- building-width column-width)
m))))
        (column (+xy p
                     (+ x (/ column-width 2))
                     (+ y (/ column-width 2))))))))


;Core Building
(define (interior-building p n m)
  (let ((ll (level-list p)))
    (map (lambda (level)
           (parameterize ((current-level level)
                          (default-level-to-level-height level-height))
             (slab (list p (+x p building-length) (+xy p
building-length building-width) (+y p building-width) p))
             (building-columns p m n)))
         ll)
    (roof (list p (+x p building-length) (+xy p building-length
building-width) (+y p building-width) p)
          (upper-level (last ll) level-height))))

(interior-building (u0) 11 6)

;Facade

(define (facade-beam p length height n m)
  (let ((element-length (/ length n))
        (element-height (/ height m))
        #;(loc p)
        (matrix (world-transformation p)))
    (for/list ((r (range 0 length (/ length n))))
      (for/list ((z (range 0 height (/ height m))))
        (facade-element-beam (+xz p r (+ z (/ element-height 2)))
                             element-length
                             element-height
                             (mm (+ 2000 (* 1000 (cos (+ (* z 2pi (/ 1
height)) (* r 4pi (/ 1 length))))))))))))

(define (facade-element-beam p width height dist)
  (let ((e (/ height 2)))
    (parameterize ((default-beam-family (beam-family-element beam-family e e)))
      (let* ((p0 p)
             (p1 (+xy p (/ width 2) (- dist))))
        (let ((v (unitize (p-p p1 p0))))
          (let* ((pa (+c p0 (v*r v (/ e -2))))
                 (pb (+c p1 (v*r v (/ e +2)))))
            (beam pa pb))))
      (let* ((p0 (+xz p width e))
             (p1 (+xyz p (/ width 2) (- dist) e)))
        (let ((v (unitize (p-p p1 p0))))
          (let* ((pa (+c p0 (v*r v (/ e -2))))
                 (pb (+c p1 (v*r v (/ e +2)))))
            (beam pa pb)))))))

(define (facades-beam p length width height n0 n1 m)
   (facade-beam p length height n0 m)
   (facade-beam (loc-from-o-phi (+x p length) pi/2) width height n1 m)
   (facade-beam (loc-from-o-phi (+xy p length width) pi) length height n0 m)
   (facade-beam (loc-from-o-phi (+y p width) 3pi/2) width height n1 m))

(facades-beam (xy 0 0) building-length building-width building-height 25 10 20)