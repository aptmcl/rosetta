#lang racket
(require rosetta/glfast)

(init 10)

;(extrude (list (xyz 0.0 4.0 0.0) (xyz 0.0 0.0 0.0)) 2.5)
;(extrude (list (xyz 0.0 4.0 0.0) (xyz 0.0 0.0 0.0) (xyz 1.0 -1.0 0.0) (xyz 4.0 4.0 0.0)) 2.5)

;(extrude (list (xyz 5.0 4.0 0.0) (xyz 5.0 0.0 0.0) (xyz 6.0 -1.0 0.0) (xyz 10.0 0.0 0.0)  (xyz 10.0 4.0 0.0)) 7.5)

(extrude (list (xyz 0.0 4.0 0.0) (xyz 0.0 0.0 0.0) (xyz 0.0 -1.0 1.0) (xyz 0.0 0.0 4.0)  (xyz 0.0 4.0 4.0)) 10.5 0.4 0.5 0.6)

(start)
