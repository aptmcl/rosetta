#lang typed/racket/base/no-check
(require "../base/coord.rkt")
(require "../base/raw-connection.rkt")

(define id-counter -1)

(define (incr-id-counter)
  (set! id-counter (+ id-counter 1))
  id-counter)

(defdecoder void identity)

(defencoder bool (lambda ([b : Boolean] [o : Output-Port]) (write-byte (if b 1 0) o)))
(defdecoder bool (lambda ([i : Input-Port]) (= (read-byte i) 1)))

(defencoder string write-c-sharp-string)
(defdecoder string read-c-sharp-string)

(defencoder double write-double)
(defdecoder double read-double)

(defencoder Int32 write-int32)
(defdecoder Int32 read-int32)

(defencoder byte write-byte)
(defdecoder byte read-byte)

(defencoder XYZ
  (lambda ([p : Loc] [o : Output-Port])
    (let ((p (loc-in-world p)))
      (write-double (cx p) o)
      (write-double (cy p) o)
      (write-double (cz p) o))))

(defdecoder XYZ
  (lambda ([i : Input-Port])
    (xyz (read-double i)
         (read-double i)
         (read-double i))))

(defencoder Vector3d
  (lambda ([v : Vec] [o : Output-Port])
    (let ((v (vec-in-world v)))
      (write-double (cx v) o)
      (write-double (cy v) o)
      (write-double (cz v) o))))

(defdecoder Vector3d
  (lambda ([i : Input-Port])
    (vxyz (read-double i)
          (read-double i)
          (read-double i))))

(defencoder XYZArray
  (lambda ([pts : Locs] [o : Output-Port])
    (write-int32 (length pts) o)
    (for ([pt : Loc (in-list pts)])
      (let ((p (loc-in-world pt)))
        (write-double (cx p) o)
        (write-double (cy p) o)
        (write-double (cz p) o)))))

(defencoder stringArray
  (lambda ([strs : (Listof String)] [o : Output-Port])
    (write-int32 (length strs) o)
    (for ([str : String (in-list strs)])
      (write-c-sharp-string str o))))

(defencoder doubleArray
  (lambda ([ds : (Listof Real)] [o : Output-Port])
    (write-int32 (length ds) o)
    (for ([d : Real (in-list ds)])
      (write-double d o))))

(defencoder ElementId
  write-int32)

(defdecoder ElementId
  (lambda ([i : Input-Port])
    (let ((id (read-int32 i)))
      (if (= id -1)
          (error 'Revit "Error: ~A" (read-c-sharp-string i))
          id))
    ;;Don't read anything, just increment the counter
    #;(incr-id-counter)))

;Revit
(use-port-number! 11001)

(defop "public ElementId FindOrCreateLevelAtElevation(double elevation)")
(defop "public ElementId UpperLevel(ElementId currentLevelId, double addedElevation)")
(defop "public ElementId LoadFamily(string fileName)")
(defop "public ElementId FamilyElement(ElementId familyId, string[] namesList, double[] valuesList)")
(defop "public ElementId CreatePolygonalFloor(XYZ[] pts, ElementId levelId)")
(defop "public ElementId CreatePolygonalRoof(XYZ[] pts, ElementId levelId, ElementId famId)")
(defop "public ElementId CreateColumn(XYZ location, ElementId baseLevelId, ElementId topLevelId, ElementId famId)")
(defop "public ElementId CreateBeam(XYZ p0, XYZ p1, ElementId famId)")