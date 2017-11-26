#lang typed/racket/base/no-check
(require "../base/coord.rkt")
(require "../base/raw-connection.rkt")

(define id-counter -1)

(define (incr-id-counter)
  (set! id-counter (+ id-counter 1))
  id-counter)

(defdecoder void
  (lambda ([i : Input-Port])
    (let ((val (read-byte i)))
      (when (= val 127)
        (error 'AutoCAD "Error: ~A" (read-c-sharp-string i))))))

(defencoder String
  write-c-sharp-string)

(defencoder double
  write-double)

(defdecoder double
  read-double)

(defencoder Int32
  write-int32)

(defdecoder Int32
  read-int32)

(defencoder byte
  write-byte)

(defdecoder byte
  read-byte)

(defencoder int
  write-int32)

(defdecoder int
  read-int32)

(defencoder bool (lambda ([b : Boolean] [o : Output-Port]) (write-byte (if b 1 0) o)))
(defdecoder bool
  (lambda ([i : Input-Port])
    (let ((val (read-byte i)))
      (if (= val 127)
          (error 'AutoCAD "Error: ~A" (read-c-sharp-string i))
          (= val 1)))))

(defencoder string write-c-sharp-string)
(defdecoder string read-c-sharp-string)

(define (verified-read-id [i : Input-Port])
  (let ((id (read-int32 i)))
      (if (= id -1)
          (error 'AutoCAD "Error: ~A" (read-c-sharp-string i))
          id)))

(defencoder ObjectId write-int32)
(defdecoder ObjectId verified-read-id)

(defencoder Entity write-int32)
(defdecoder Entity verified-read-id)

(defencoder ObjectIdArray 
  (lambda ([os : (Listof Integer)] [o : Output-Port])
    (write-int32 (length os) o)
    (for ([obj : Integer (in-list os)])
      (write-int32 obj o))))
(defdecoder ObjectIdArray 
  (lambda ([i : Input-Port])
    (let ((size (read-int32 i)))
      (if (= size -1)
          (error 'AutoCAD "Error: ~A" (read-c-sharp-string i))
          (for/list ([n : Integer (in-range size)])
            (read-int32 i))))))

(defencoder doubleArray 
  (lambda ([vs : (Listof Float)] [o : Output-Port])
    (write-int32 (length vs) o)
    (for ([v :  (in-list os)])
      (write-double v o))))
(defdecoder doubleArray 
  (lambda ([i : Input-Port])
    (let ((size (read-int32 i)))
      (if (= size -1)
          (error 'AutoCAD "Error: ~A" (read-c-sharp-string i))
          (for/list ([n : Integer (in-range size)])
            (read-double i))))))

(defencoder Point3d
  (lambda ([p : Loc] [o : Output-Port])
    (let ((p (loc-in-world p)))
      (write-double (cx p) o)
      (write-double (cy p) o)
      (write-double (cz p) o))))

(defdecoder Point3d
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

(defencoder Point3dArray
  (lambda ([pts : Locs] [o : Output-Port])
    (write-int32 (length pts) o)
    (for ([pt : Loc (in-list pts)])
      (let ((p (loc-in-world pt)))
        (write-double (cx p) o)
        (write-double (cy p) o)
        (write-double (cz p) o)))))
(defdecoder Point3dArray
  (lambda ([i : Input-Port])
    (let ((size (read-int32 i)))
      (if (= size -1)
          (error 'AutoCAD "Error: ~A" (read-c-sharp-string i))
          (for/list ([n : Loc (in-range size)])
            ((decode Point3d) i))))))


(defdecoder Frame3d
  (lambda ([i : Input-Port])
    (u0 (cs-from-o-vx-vy-vz
         ((decode Point3d) i)
         ((decode Vector3d) i)
         ((decode Vector3d) i)
         ((decode Vector3d) i)))))

(defencoder Id write-int32)
(defdecoder Id
  (lambda ([i : Input-Port])
    (read-int32 i)
    ;;Don't read anything, just increment the counter
    #;(incr-id-counter)))

;AutoCAD
(use-port-number! 11000)

(defop "public int DeleteAll()")
(defop "public void SetView(Point3d position, Point3d target, double lens, bool perspective, string style)")
(defop "public void View(Point3d position, Point3d target, double lens)")
(defop "public void ViewTop()")
(defop "public Point3d ViewCamera()")
(defop "public Point3d ViewTarget()")
(defop "public double ViewLens()")
(defop "public byte Sync()")
(defop "public byte Disconnect()")
(defop "public void Delete(ObjectId id)")
(defop "public void DeleteMany(ObjectId[] ids)")
(defop "public ObjectId Copy(ObjectId id)")
(defop "public Entity Point(Point3d p)")
(defop "public Point3d PointPosition(Entity ent)")

(defop "public Entity PolyLine(Point3d[] pts)")
(defop "public Point3d[] LineVertices(ObjectId id)")

(defop "public Entity Spline(Point3d[] pts)")
(defop "public Entity InterpSpline(Point3d[] pts, Vector3d tan0, Vector3d tan1)")
(defop "public Entity ClosedPolyLine(Point3d[] pts)")
(defop "public Entity ClosedSpline(Point3d[] pts)")
(defop "public Entity InterpClosedSpline(Point3d[] pts)")
(defop "public Entity Circle(Point3d c, Vector3d n, double r)")
(defop "public Point3d CircleCenter(Entity ent)")
(defop "public Vector3d CircleNormal(Entity ent)")
(defop "public double CircleRadius(Entity ent)")
(defop "public Entity Ellipse(Point3d c, Vector3d n, Vector3d majorAxis, double radiusRatio)")
(defop "public Entity Arc(Point3d c, Vector3d n, double radius, double startAngle, double endAngle)")
(defop "public Entity Text(string str, Point3d corner, Vector3d vx, Vector3d vy, double height)")
(defop "public Entity SurfaceFromCurve(Entity curve)")
(defop "public Entity SurfaceCircle(Point3d c, Vector3d n, double r)")
(defop "public Entity SurfaceEllipse(Point3d c, Vector3d n, Vector3d majorAxis, double radiusRatio)")
(defop "public Entity SurfaceArc(Point3d c, Vector3d n, double radius, double startAngle, double endAngle)")
(defop "public Entity SurfaceClosedPolyLine(Point3d[] pts)")
(defop "public ObjectId[] SurfaceFromCurves(ObjectId[] ids)")
(defop "public Entity Sphere(Point3d c, double r)")
(defop "public Entity Torus(Point3d c, Vector3d vz, double majorRadius, double minorRadius)")
(defop "public Entity ConeFrustum(Point3d bottom, double base_radius, Point3d top, double top_radius)")
(defop "public Entity Cylinder(Point3d bottom, double radius, Point3d top)")
(defop "public Entity Cone(Point3d bottom, double radius, Point3d top)")
(defop "public Entity Box(Point3d corner, Vector3d vx, Vector3d vy, double dx, double dy, double dz)")
(defop "public Entity CenteredBox(Point3d corner, Vector3d vx, Vector3d vy, double dx, double dy, double dz)")
(defop "public ObjectId IrregularPyramidMesh(Point3d[] pts, Point3d apex)")
(defop "public ObjectId IrregularPyramid(Point3d[] pts, Point3d apex)")
(defop "public ObjectId IrregularPyramidFrustum(Point3d[] bpts, Point3d[] tpts)")
(defop "public Entity MeshFromGrid(int m, int n, Point3d[] pts, bool closedM, bool closedN)")
(defop "public Entity SurfaceFromGrid(int m, int n, Point3d[] pts, bool closedM, bool closedN, int level)")
(defop "public Entity SolidFromGrid(int m, int n, Point3d[] pts, bool closedM, bool closedN, int level, double thickness)")
(defop "public ObjectId Thicken(ObjectId obj, double thickness)")
(defop "public double[] CurveDomain(Entity ent)")
(defop "public double CurveLength(Entity ent)")
(defop "public Frame3d CurveFrameAt(Entity ent, double t)")
(defop "public Frame3d CurveFrameAtLength(Entity ent, double l)")
(defop "public Frame3d CurveClosestFrameTo(Entity ent, Point3d p)")
(defop "public ObjectId JoinCurves(ObjectId[] ids)")
(defop "public ObjectId NurbSurfaceFrom(ObjectId id)")
(defop "public double[] SurfaceDomain(Entity ent)")
(defop "public Frame3d SurfaceFrameAt(Entity ent, double u, double v)")
(defop "public ObjectId Extrude(ObjectId profileId, Vector3d dir)")
(defop "public ObjectId Sweep(ObjectId pathId, ObjectId profileId, double rotation, double scale)")
(defop "public ObjectId Loft(ObjectId[] profilesIds, ObjectId[] guidesIds, bool ruled, bool closed)")
(defop "public void Intersect(ObjectId objId0, ObjectId objId1)")
(defop "public void Subtract(ObjectId objId0, ObjectId objId1)")
(defop "public void Slice(ObjectId id, Point3d p, Vector3d n)")
(defop "public ObjectId Revolve(ObjectId profileId, Point3d p, Vector3d n, double startAngle, double amplitude)")
(defop "public void Move(ObjectId id, Vector3d v)")
(defop "public void Scale(ObjectId id, Point3d p, double s)")
(defop "public void Rotate(ObjectId id, Point3d p, Vector3d n, double a)")
(defop "public ObjectId Mirror(ObjectId id, Point3d p, Vector3d n, bool copy)")
(defop "public Point3d[] GetPoint(string prompt)")
(defop "public ObjectId[] GetAllShapes()")
(defop "public ObjectId[] GetAllShapesInLayer(ObjectId layerId)")
(defop "public Point3d[] BoundingBox(ObjectId[] ids)")
(defop "public void ZoomExtents()")
(defop "public ObjectId CreateLayer(string name)")
(defop "public void SetLayerColor(ObjectId id, byte r, byte g, byte b)")
(defop "public void SetShapeColor(ObjectId id, byte r, byte g, byte b)")
(defop "public ObjectId CurrentLayer()")
(defop "public void SetCurrentLayer(ObjectId id)")
(defop "public ObjectId ShapeLayer(ObjectId objId)")
(defop "public void SetShapeLayer(ObjectId objId, ObjectId layerId)")
(defop "public void SetSystemVariableInt(string name, int value)")
(defop "public int Render(int width, int height, string path)")
(defop "public int Command(string cmd)")
(defop "public void DisableUpdate()")
(defop "public void EnableUpdate()")

(defop "public byte ShapeCode(ObjectId id)")

