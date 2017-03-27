#lang typed/racket/base/no-check
(require "../base/coord.rkt")
(require "../base/raw-connection.rkt")

(define id-counter -1)

(define (incr-id-counter)
  (set! id-counter (+ id-counter 1))
  id-counter)

(defdecoder void
  (lambda ([i : Input-Port])
    (let ((code (read-byte i)))
      (if (= code 0)
          #t
          (error 'Revit "Error: ~A" (read-c-sharp-string i))))))

(defencoder bool (lambda ([b : Boolean] [o : Output-Port]) (write-byte (if b 1 0) o)))
(defdecoder bool
  (lambda ([i : Input-Port])
    (let ((val (read-byte i)))
      (if (= val 127)
          (error 'Revit "Error: ~A" (read-c-sharp-string i))
          (= val 1)))))

(defencoder string write-c-sharp-string)
(defdecoder string read-c-sharp-string)

(defencoder double write-double)
(defdecoder double read-double)

(defencoder int write-int32)
(defdecoder int read-int32)

(defencoder byte write-byte)
(defdecoder byte read-byte)

(define to-feet 3.28084)
(define (write-length l o)
  (write-double (* l to-feet) o))
(define (read-length i)
  (/ (read-double i) to-feet))

(defencoder Length write-length)
(defdecoder Length read-length)

(defencoder XYZ
  (lambda ([p : Loc] [o : Output-Port])
    (let ((p (loc-in-world p)))
      (write-length (cx p) o)
      (write-length (cy p) o)
      (write-length (cz p) o))))

(defdecoder XYZ
  (lambda ([i : Input-Port])
    (xyz (read-length i)
         (read-length i)
         (read-length i))))

(defencoder Vector3d
  (lambda ([v : Vec] [o : Output-Port])
    (let ((v (vec-in-world v)))
      (write-length (cx v) o)
      (write-length (cy v) o)
      (write-length (cz v) o))))

(defdecoder Vector3d
  (lambda ([i : Input-Port])
    (vxyz (read-length i)
          (read-length i)
          (read-length i))))

(defencoder XYZArray
  (lambda ([pts : Locs] [o : Output-Port])
    (write-int32 (length pts) o)
    (for ([pt : Loc (in-list pts)])
      (let ((p (loc-in-world pt)))
        (write-length (cx p) o)
        (write-length (cy p) o)
        (write-length (cz p) o)))))

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

(defencoder LengthArray
  (lambda ([ls : (Listof Real)] [o : Output-Port])
    (write-int32 (length ls) o)
    (for ([l : Real (in-list ls)])
      (write-length l o))))


(define (verified-read-id [i : Input-Port])
  (let ((id (read-int32 i)))
      (if (= id -1)
          (error 'Revit "Error: ~A" (read-c-sharp-string i))
          id)))

(defencoder ElementId write-int32)
(defdecoder ElementId verified-read-id)

(defdecoder ElementIdArray 
  (lambda ([i : Input-Port])
    (let ((size (read-int32 i)))
      (if (= size -1)
          (error 'Revit "Error: ~A" (read-c-sharp-string i))
          (for/list ([n : Integer (in-range size)])
            (read-int32 i))))))

(defencoder Element write-int32)
(defdecoder Element verified-read-id)

(defencoder Level write-int32)
(defdecoder Level verified-read-id)

(defdecoder Level
  (lambda ([i : Input-Port])
    (let ((id (read-int32 i)))
      (if (= id -1)
          (error 'Revit "Error: ~A" (read-c-sharp-string i))
          id))
    ;;Don't read anything, just increment the counter
    #;(incr-id-counter)))


(defencoder Material write-int32)
(defdecoder Material verified-read-id)

;Revit
(use-port-number! 11001)

;;Note that doubles that represent lengths need to use the Length type so that we convert to
;;Revit's internal units (i.e., feet)
(defop "public ElementId FindOrCreateLevelAtElevation(Length elevation)")
(defop "public ElementId UpperLevel(ElementId currentLevelId, Length addedElevation)")
(defop "public Length GetLevelElevation(Level level)")
(defop "public ElementId LoadFamily(string fileName)")
(defop "public ElementId FamilyElement(ElementId familyId, string[] namesList, Length[] valuesList)")
(defop "public ElementId CreatePolygonalFloor(XYZ[] pts, ElementId levelId)")
(defop "public ElementId CreatePolygonalRoof(XYZ[] pts, ElementId levelId, ElementId famId)")
(defop "public ElementId CreatePathFloor(XYZ[] pts, double[] angles, ElementId levelId)")
(defop "public ElementId CreatePathRoof(XYZ[] pts, double[] angles, ElementId levelId, ElementId famId)")
(defop "public Element CreateColumn(XYZ location, ElementId baseLevelId, ElementId topLevelId, ElementId famId)")
(defop "public Element CreateColumnPoints(XYZ p0, XYZ p1, Level level0, Level level1, ElementId famId)")
(defop "public ElementId[] CreateLineWall(XYZ[] pts, ElementId baseLevelId, ElementId topLevelId, ElementId famId)")
(defop "public ElementId CreateSplineWall(XYZ[] pts, ElementId baseLevelId, ElementId topLevelId, ElementId famId, bool closed)")
(defop "public Element CreateLineRailing(XYZ[] pts, ElementId baseLevelId, ElementId familyId)") 
(defop "public Element CreatePolygonRailing(XYZ[] pts, ElementId baseLevelId, ElementId familyId)")
(defop "public ElementId CreateBeam(XYZ p0, XYZ p1, double rotationAngle, ElementId famId)")
(defop "public Element SurfaceGrid(XYZ[] linearizedMatrix, int n, int m)")
(defop "public void MoveElement(ElementId id, XYZ translation)")
(defop "public void RotateElement(ElementId id, double angle, XYZ axis0, XYZ axis1)")
(defop "public void CreatePolygonalOpening(XYZ[] pts, Element host)")
(defop "public void CreatePathOpening(XYZ[] pts, double[] angles, Element host)")
(defop "public Element InsertDoor(Length deltaFromStart, Length deltaFromGround, Element host, ElementId familyId)")
(defop "public Element InsertWindow(Length deltaFromStart, Length deltaFromGround, Element host, ElementId familyId)")
(defop "public Element InsertRailing(Element host, ElementId familyId)")

(defop "public void CreateFamily(string familyTemplatesPath, string familyTemplateName, string familyName)")
(defop "public void CreateFamilyExtrusionTest(XYZ[] pts, double height)")
(defop "public void InsertFamily(string familyName, XYZ p)")

(defop "public Material GetMaterial(string name)")
(defop "public void ChangeElementMaterial(Element element, Material material)")

(defop "public void HighlightElement(ElementId id)")
(defop "public ElementId[] GetSelectedElements()")
(defop "public bool IsProject()")
(defop "public void DeleteAllElements()")
(defop "public void SetView(XYZ camera, XYZ target, double focal_length)")
(defop "public void EnergyAnalysis()")
