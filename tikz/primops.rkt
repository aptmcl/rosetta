#lang typed/racket/base/no-check
(require racket/math)
(require "../base/coord.rkt")

(provide point 
         circle 
         ellipse 
         arc 
         line 
         closed-line 
         spline 
         closed-spline
         hobby-spline 
         hobby-closed-spline
         rectangle
         text
         get-accumulated-tikz
         save-tikz
         display-tikz
         transform
         #;
         end-transform
         set-view)


(define tikz-port (open-output-string))

(define (tikz obj)
  (display obj tikz-port))

(define (tikzln obj)
  (displayln obj tikz-port))

(define (get-accumulated-tikz)
  (bytes->string/utf-8 (get-output-bytes tikz-port #t) #\?))

(define (save-tikz [pathname : Path-String])
  (with-output-to-file pathname
    (lambda ()
      (write-bytes (get-output-bytes tikz-port #t)))))

(define (display-tikz)
  (display (get-output-string tikz-port)))

(define (tikz-end)
  (tikzln ";"))

(define (tikz-e arg)
  (tikz arg)
  (tikz-end))

(define (tikz-draw [fill? #f])
  (tikz (if fill? "\\fill " "\\draw ")))

(define (tikz-number [x : Real])
  (if (integer? x)
      (tikz x)
      (if (< (abs x) 1e-4)
          (tikz 0)
          (tikz (/ (round (* x 10000.0)) 10000.0)))))
;       (string-trim 
;        (real->decimal-string x 6) "0" 
;        #:left? #f #:repeat? #t))))

(define (tikz-cm [x : Real])
  (tikz-number x)
  (tikz "cm"))
    
(define (tikz-coord [c : Loc])
  (tikz "(")
  (tikz-number (cx c))
  (tikz ",")
  (tikz-number (cy c))
  (unless (= (cz c) 0)
    (tikz ",")
    (tikz-number (cz c)))
  (tikz ")"))

(define (tikz-pgfpoint [c : Loc])
  (unless (= 0 (cz c))
    (error "Can't handle 3D coords"))
  (tikz "\\pgfpoint{")
  (tikz-cm (cx c))
  (tikz "}{")
  (tikz-cm (cy c))
  (tikz "}"))

(define (circle [c : Loc] [r : Real] [fill? : Boolean #f])
  (tikz-draw fill?)
  (tikz-coord c)
  (tikz "circle(")
  (tikz-cm r)
  (tikz-e ")"))

(define (point [c : Loc])
  (circle c 0.01))

(define (ellipse [c : Loc] [r0 : Real] [r1 : Real] [fi : Real] [fill? #f])
  (tikz-draw fill?)
  (tikz "[shift={")
  (tikz-coord c)
  (tikz "}]")
  (tikz "[rotate=")
  (tikz-number (radians->degrees fi))
  (tikz "]")
  (tikz "(0,0)")
  (tikz "ellipse(")
  (tikz-cm r0)
  (tikz " and ")
  (tikz-cm r1)
  (tikz-e ")"))

(define (arc [c : Loc] [r : Real] [ai : Real] [af : Real] [fill? #f])
  (tikz-draw fill?)
  (when fill?
    (tikz-coord c)
    (tikz "--"))
  (tikz-coord (+pol c r ai))
  (tikz "arc(")
  (tikz-number (radians->degrees ai))
  (tikz ":")
  (tikz-number
   (radians->degrees
    (if (> ai af)
      (+ af (* 2 pi))
      af)))
  (tikz ":")
  (tikz-cm r)
  (tikz ")")
  (when fill?
    (tikz-e "--cycle"))
  (tikz-end))

(define (line [pts : Locs])
  (tikz-draw)
  (tikz-coord (car pts))
  (for ([pt (in-list (cdr pts))])
    (tikz "--")
    (tikz-coord pt))
  (tikz-end))

(define (closed-line [pts : Locs] [fill? : Boolean #f])
  (tikz-draw fill?)
  (for ([pt (in-list pts)])
    (tikz-coord pt)
    (tikz "--"))
  (tikz-e "cycle"))

(define (spline [pts : Locs] [fill? : Boolean #f])
  (tikz-draw fill?)
  (tikz "plot [smooth,tension=1] coordinates {")
  (for ([pt (in-list pts)])
    (tikz-coord pt))
  (tikz-e "}"))

(define (closed-spline [pts : Locs] [fill? : Boolean #f])
  (tikz-draw fill?)
  (tikz "plot [smooth cycle,tension=1] coordinates {")
  (for ([pt (in-list pts)])
    (tikz-coord pt))
  (tikz-e "}"))

;;HACK we need to handle the starting and ending vectors
(define (hobby-spline [pts : Locs] [fill? : Boolean #f])
  (tikz-draw fill?)
  (tikz "[hobby]")
  (tikz "plot coordinates {")
  (for ([pt (in-list pts)])
    (tikz-coord pt))
  (tikz-e "}"))

;;HACK we need to handle the starting and ending vectors
(define (hobby-closed-spline [pts : Locs] [fill? : Boolean #f])
  (tikz-draw fill?)
  (tikz "[closed hobby]")
  (tikz "plot coordinates {")
  (for ([pt (in-list pts)])
    (tikz-coord pt))
  (tikz-e "}"))

(define (rectangle [p : Loc] [w : Real] [h : Real] [fill? : Boolean #f])
  (tikz-draw fill?)
  (tikz-coord p)
  (tikz "rectangle")
  (tikz-coord (+xy p w h))
  (tikz-end))

;;Assuming default Arial font for AutoCAD
#;
(define (text txt p0 p1 h rot x-scale incl horiz-just vert-just)
  (let ((scale-x (* 3.7 h (if x-scale x-scale 1)))
	(scale-y (* 3.7 h)))
    (tikz-draw)
    (tikz "[anchor=base west]")
    (tikz-coord p0)
    ;; (tikz "node[font=\\fontfamily{phv}\\selectfont,outer sep=0pt,inner sep=0pt,rotate=")
    (tikz "node[font=\\fontfamily{phv}\\selectfont,outer sep=0pt,inner sep=0pt")
    (unless (and (= horiz-just 0) (= vert-just 0))
      (tikz ",minimum width=")
      (tikz-cm (abs (/ (- (cx p1) (cx p0)) scale-x)))
      (tikz ",minimum height=")
      (tikz-cm (abs (/ (- (cy p1) (cy p0)) scale-y))))
    (unless (= rot 0)
      (tikz ",rotate=")
      (tikz-number (radians->degrees rot)))
    (unless (= incl 0)
      (tikz ",xslant=")
      (tikz-number (sin incl)))
    (tikz ",xscale=")
    (tikz-number scale-x)
    (tikz ",yscale=")
    (tikz-number scale-y)
    (tikz "]{")
    (tikz txt)
    (tikz-e "}")))

(define (text txt [p : Loc] [h : Real])
  (let ((scale-x (* 3.7 h))
	(scale-y (* 3.7 h)))
    (tikz-draw)
    (tikz "[anchor=base west]")
    (tikz-coord p)
    (tikz "node[font=\\fontfamily{phv}\\selectfont,outer sep=0pt,inner sep=0pt")
    (tikz ",xscale=")
    (tikz-number scale-x)
    (tikz ",yscale=")
    (tikz-number scale-y)
    (tikz "]{")
    (tikz txt)
    (tikz-e "}")))


;FINISH THIS
(define (transform [f : (-> Void)] [c : Loc])
  (tikz "\\begin{scope}")
  (tikz "[shift={")
  (tikz-coord c)
  (tikz "}]")
  #;#;(tikz "[rotate=")
  (tikz-number (radians->degrees fi))
  (tikz "]")
  (f)
  (tikz-e "\\end{scope}"))


(define (set-view [camera : Loc] [target : Loc] [lens : Real])
  (let ((v (p-p camera target))
        (contents (get-output-bytes tikz-port #t)))
    (tikz "\\tdplotsetmaincoords{")
    (tikz-number (radians->degrees (sph-psi v)))
    (tikz "}{")
    (tikz-number (+ (radians->degrees (sph-phi v)) 90))
    (tikzln "}")
    (tikzln "\\begin{tikzpicture}[tdplot_main_coords]")
    (write-bytes contents tikz-port)
    (tikzln "\\end{tikzpicture}")))
