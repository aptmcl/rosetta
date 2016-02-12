#lang typed/racket/base
(require (for-syntax racket/base))
(require "../base/connection.rkt"
         (except-in "../base/utils.rkt" random))
(require racket/runtime-path)

(provide start-sketchup
         stop-sketchup
         sketchup
         encode-xyz
         encode-list-f)

(define-runtime-path sketchup-template (build-path "template.skp"))
(define-runtime-path sketchup-init (build-path "rosetta.rb"))

#|
Sketchup specific operations
|#

(: start-sketchup (-> Connection))
(: stop-sketchup (Connection -> Void))

(define sketchup-port (+ 19405 (random 10 (make-pseudo-random-generator))))

(define (start-sketchup)
  (putenv "ROSETTAPORT" (number->string sketchup-port))
  (shell-execute 
   "open"
   "Sketchup.exe"
   (format "-RubyStartup \"~s\" -template \"~s\"" 
           (path->string sketchup-init)
           (path->string sketchup-template))
   (current-directory)
   'sw_shownormal)
  (establish-connection "Sketchup" sketchup-port))

(define (stop-sketchup sk)
  (shutdown-connection sk))

(define-cached (sketchup) : Connection
  (start-sketchup))


(define-type Location (List Real Real Real))
(define-type Locations (Listof Location))
(define-type Locationss (Listof (Listof Location)))

(define-type Strings (Listof String))

(: encode-xyz (Real Real Real Output-Port -> Void))
(define (encode-xyz x y z o)
  (write-string "[" o)
  (display (exact->inexact x) o)
  (write-string "," o)
  (display (exact->inexact y) o)
  (write-string "," o)
  (display (exact->inexact z) o)
  (write-string "]" o)
  (void))

(: encode-location (Location Output-Port -> Void))
(define (encode-location loc o)
  (encode-xyz (car loc) (cadr loc) (caddr loc) o))

(define (string->real [s : String]) : Real
  (let ((n (string->number s)))
    (if n
        (cast n Real)
        (error "Couldn't convert to number" n))))

(define (decode-real [i : Input-Port]) : Real
  (string->real (read-line-string i)))

(define (decode-location-from-string [input : String]) : Location
  (let ((res (regexp-match #rx"\\[(.+), (.+), (.+)\\]" input)))
    (if res
        (let ((xstr (cadr res))
              (ystr (caddr res))
              (zstr (cadddr res)))
          (if (and xstr ystr zstr)
              (list (string->real xstr)
                    (string->real ystr)
                    (string->real zstr))
              (error "Couldn't decode location" input)))
        (error "Couldn't decode location" input))))

(define (decode-location [i : Input-Port]) : Location
  (let ((input (read-line-string i)))
    (decode-location-from-string input)))
  
(define (decode-locations [i : Input-Port]) : Locations
  (let ([inputs (regexp-split #rx"\\|" (read-line-string i))])
    (map decode-location-from-string inputs)))

(: encode-locations (Locations Output-Port -> Void))
(define (encode-locations locs o)
  (write-string "[" o)
  (encode-location (car locs) o)
  (for ((loc (in-list (cdr locs))))
    (write-string "," o)
    (encode-location loc o))
  (write-string "]" o)
  (void))

(: encode-locationss (Locationss Output-Port -> Void))
(define (encode-locationss locss o)
  (write-string "[" o)
  (encode-locations (car locss) o)
  (for ((locs (in-list (cdr locss))))
    (write-string "," o)
    (encode-locations locs o))
  (write-string "]" o)
  (void))


(: encode-list-f (All (L) (-> (Listof L) (-> L Output-Port Void) Output-Port Void)))
(define (encode-list-f l f o)
  (write-string "[" o)
  (f (car l) o)
  (for ((loc (in-list (cdr l))))
    (write-string "," o)
    (f loc o))
  (write-string "]" o)
  (void))
  
(: decode-list-f (All (L) (-> Input-Port (-> String L) (Listof L))))
(define (decode-list-f i f)
  (let ((str (read-line-string i)))
    (if (string=? str "")
        (list)
        (let ([inputs (regexp-split #rx"\\|" str)])
          (map f inputs)))))

;;Not using this to remove dependency from xyz
#;#;#;
(: encode-pts ((Listof Loc) Output-Port -> Void))
(define (encode-pts pts o)
  (encode-list-f 
   pts
   (λ ([pt : Loc] [o : Output-Port])
     (%encode-xyz (cx pt) (cy pt) (cz pt)))))
(defencoder Locs encode-pts)

(: encode-strings ((Listof String) Output-Port -> Void))
(define (encode-strings ss o)
  (encode-list-f ss write o))

(: decode-strings (-> Input-Port (Listof String)))
(define (decode-strings i)
  (decode-list-f i decode-string))

(: decode-string (String -> String))
(define (decode-string str) str)

(define (read-line-void [i : Input-Port]) : Void
  (read-line-string i)
  (void))

(defencoder Real (lambda ([x : Real] [o : Output-Port]) (display (exact->inexact x) o)))
(defencoder String write)
(defencoder Strings encode-strings)
(defencoder Location encode-location)
(defencoder Locations encode-locations)
(defencoder Locationss encode-locationss)

(defdecoder Real decode-real)
(defdecoder String read-line-string)
(defdecoder Strings decode-strings)
(defdecoder Void read-line-void)
(defdecoder Location decode-location)
(defdecoder Locations decode-locations)


(define-syntax-rule
  (def (name param ...) res ...)
  (defcall (name (sketchup) param ...) res ...))

(def (addPoint x y z))
(def (addCircle x y z r nx ny nz))
(def (addCircleZ x y z r))
(def (addArc x y z xx xy xz nx ny nz r sa ea))
(def (addArcZ x y z r sa ea))
(def (addEllipse x y z xx xy xz yx yy yz ra rb))
(def (addSurfaceCircle x y z r nx ny nz))
(def (addSurfaceCircleZ x y z r))
(def (addSurfaceArc x y z xx xy xz nx ny nz r sa ea))
(def (addSurfaceArcZ x y z r sa ea))
(def (addLine [pts Locations]))
(def (addBezier [pts Locations]))
(def (addCubicBezier [pts Locations] precision))
(def (addPolygon [pts Locations]))
(def (addSurfacePolygon [pts Locations]))
(def (addCylinder x0 y0 z0 r x1 y1 z1))
(def (addCylinderZ x y z r h))
(def (addBox x0 y0 z0 x1 y1 z1))
(def (addBoxD x y z dx dy dz))
(def (addBoxTrans dx dy dz m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))
(def (addRegularPyramid n a x0 y0 z0 r x1 y1 z1))
(def (addCone x0 y0 z0 r x1 y1 z1))
(def (addConeZ x y z r h))
(def (addConeFrustum x0 y0 z0 r0 x1 y1 z1 r1))
(def (addConeFrustumZ x y z r0 h r1))
(def (addFrustum [pbs Locations] [pts Locations]))
(def (addPyramid [pbs Locations] [pt Location]))
(def (addSphere x y z r))
(def (addExtrusion [path String] nx ny nz))
(def (addExtrusionZ [path String] h))
(def (addSweep [path String] [profile String]))
(def (addText [text String] x y z sz))
(def (addTextTrans [text String] sz m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))
(def (addTextCentered [text String] x y z sz))
(def (addTextCenteredTrans [text String] sz m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33))
(def (addTorus x y z nx ny nz re ri))
(def (applyTransformation [s String] m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33) Void)

(def (addSurface [ids Strings]))
(def (joinCurves [ids Strings]))

(def (addUnion [sh0 String] [sh1 String]))
(def (addIntersection [sh0 String] [sh1 String]))
(def (addSubtraction [sh0 String] [sh1 String]))
(def (addSlice [sh String] x y z nx ny nz))
(def (mirror [sh String] x y z nx ny nz))
(def (copy [sh String]))

(define-syntax-rule
  (with-transformation (trans) body ...)
  (write-string (sketchup)))


(def (boundingBox [s String]) Locations)
(def (deleteShape [s String]) Void)
(def (deleteAllShapes) Void)
(def (allShapes) Strings)

(def (addSurfaceGrid [ptss Locationss]))

(def (start [s String]) Location)
(def (end [s String]) Location)
(def (vertices [s String]) Locations)

(def (camera) Location)
(def (target) Location)
(def (lens) Real)

(def (view ex ey ez tx ty tz lens) Void)
(def (viewTop) Void)
(def (renderView [path String] w h) Void)
(def (zoomExtents) Void)

(def (startGetPoint [msg String]) Void)
(def (getPoint) Location)


