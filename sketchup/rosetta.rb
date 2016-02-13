#lang ruby

require 'sketchup.rb'

Sketchup.send_action("showRubyPanel:")

##################################################################################
# socket part
def connect
  port = ENV["ROSETTAPORT"].to_i
  SKSocket.connect("127.0.0.1", port)
  SKSocket.add_socket_listener {|e| socketListener(e)}
end

def socketListener(e)
  #if e == "Connection established"
  SKSocket.write("connected\n")
  #else
  begin
    # for debugging
    puts e
    SKSocket.write("%s\n" % (eval e))
  rescue Exception => err
    puts e
    puts err
    SKSocket.write("ERROR\n")
    SKSocket.write("%s\n" % err.message)
  #  end
  end
end

=begin
New approach based on Marshal::load

The idea is that we can call a method dynamically, for example:
Object.method(:addCircleZ)[1,2,3,4], which is equivalent to
addCircleZ(1,2,3,4)

The idea is to serialize [:addCircleZ,[1,2,3,4]] on the Racket side using Ruby Marshal format, and deserialize it on the Ruby side and then do
Object.send(code[0],*code[1])
or
Object.method(code[0]).call(*code[1])
depending on which one runs faster

The anticipated problem is that socket_listener seems to read (and process) one line at a time, which is not a good fit for a binary format 

def socketListener(e)
  if e == "Connection established"
    SKSocket.write("connected\n")
  else
    begin
      code = Marshal.load(e)
      SKSocket.write("%s\n" % Object.send(code[0],*code[1]))
    rescue Exception => err
      puts e
      puts err
      SKSocket.write("ERROR\n")
      SKSocket.write("%s\n" % err.message)
    end
  end
end
=end

def disconnect
  SKSocket.write("stop\n")
  SKSocket.disconnect()
end

def shutdown
  SKSocket.write("exit\n")
  SKSocket.disconnect()
end

###############################################################################
# database for storing the objects

=begin
Shapes = Hash.new

def returnShape(s)
  Shapes[s.object_id]=s
  s.object_id
end

def resolveShape(id)
  Shapes[id.to_i]
end

def deleteShape(id)
  begin
    resolveShape(id).erase!
  rescue
    # do nothing, most probably, the shape was erased manually
  end
  Shapes.delete(id.to_i)
end
=end

### Alternative
def returnShape(s)
  if (s.class==Sketchup::Group) or (s.class==Sketchup::ComponentInstance)
    s.guid
  else #if (s.class==Sketchup::Face)
    s.entityID
#  else
#    Shapes[s.object_id]=s
#    s.object_id
  end
end 

def resolveShape(id)
  if (id.length < 22)
    id = id.to_i
  end
  Sketchup.active_model.find_entity_by_id(id) or raise "Couldn't find shape with id %s" % id 
end

def deleteShape(id)
  begin
    resolveShape(id).erase!
  rescue
    # do nothing, most probably, the shape was erased manually
  end
  #Shapes.delete(id.to_i)
end

def deleteAllShapes()
  Sketchup.active_model.active_entities.clear!
  0
end

def allShapes()
  Sketchup.active_model.active_entities.map{|s| returnShape(s)}.join("|")
end

###########################################################################
# Definitions that are used with instancing
# box

def boxDefinition()
  definitions = Sketchup.active_model.definitions
  definition = definitions["Box"]
  if ! definition
    definition = definitions.add "Box"
    entities = definition.entities
    n=24
    base = entities.add_face([[0,0,0],[1,0,0],[1,1,0],[0,1,0]])
    base.pushpull -1
  end
  definition
end

def addBoxD(x,y,z,dx,dy,dz)
  entities = Sketchup.active_model.entities
  xform = Geom::Transformation.translation(Geom::Vector3d.new(x,y,z))*Geom::Transformation.scaling(dx,dy,dz)
  shape = entities.add_instance(boxDefinition(),xform)
  returnShape(shape)
end

def addBox(x0,y0,z0,x1,y1,z1)
  addBoxD(x0,y0,z0,x1-x0,y1-y0,z1-z0)
end

def addBoxTrans(dx,dy,dz,m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)
  entities = Sketchup.active_model.entities
  xform = Geom::Transformation.scaling(dx,dy,dz)
  shape = entities.add_instance(boxDefinition(),xform)
  returnShape(transform(shape,m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33))
end

# sphere

def sphereDefinition()
  definitions = Sketchup.active_model.definitions
  definition = definitions["Sphere"]
  if ! definition
    definition = definitions.add "Sphere"
    entities = definition.entities
    circle1 = entities.add_circle(ORIGIN,Z_AXIS,1.0,24)
    circle2 = entities.add_circle(ORIGIN,Y_AXIS,2.0,24)
    face1 = entities.add_face(circle1)
    face1.reverse!
    face1.followme circle2
    entities.erase_entities circle2
  end
  definition
end

def addSphere(x,y,z,r)
  Sketchup.active_model.start_operation("Sphere", true)
  xform = Geom::Transformation.translation(Geom::Vector3d.new(x,y,z))*Geom::Transformation.scaling(r)
  shape = Sketchup.active_model.entities.add_instance(sphereDefinition(),xform)
  Sketchup.active_model.commit_operation
  returnShape(shape)
end

# cone

def coneDefinition()
  definitions = Sketchup.active_model.definitions
  definition = definitions["Cone"]
  if ! definition
    definition = definitions.add "Cone"
    entities = definition.entities
    circleBase = entities.add_circle(ORIGIN,Z_AXIS,1.0,24)
    base = entities.add_face(circleBase)
    base.reverse!
    vertices = base.vertices
    face = entities.add_face(vertices[0],ORIGIN,Geom::Point3d.new(0,0,1))
    face.followme circleBase
    line = entities.add_line(vertices[0],vertices[12])
    line.find_faces
    line.erase!
  end
  definition
end

def addConeZ(x,y,z,r,h)
  Sketchup.active_model.start_operation("Cone", true)
  xform = Geom::Transformation.translation(Geom::Vector3d.new(x,y,z))*Geom::Transformation.scaling(r,r,h)
  shape = Sketchup.active_model.entities.add_instance(coneDefinition(),xform)
  Sketchup.active_model.commit_operation
  returnShape(shape)
end

def addCone(x0,y0,z0,r,x1,y1,z1)
  Sketchup.active_model.start_operation("Cone", true)
  p = Geom::Point3d.new(x0,y0,z0)
  v = Geom::Vector3d.new(x1-x0,y1-y0,z1-z0)
  h = v.length
  xform = Geom::Transformation.new(p,v)*Geom::Transformation.scaling(r,r,h)
  shape = Sketchup.active_model.entities.add_instance(coneDefinition(),xform)
  Sketchup.active_model.commit_operation
  returnShape(shape)
end

# cylinder

def cylinderDefinition()
  definitions = Sketchup.active_model.definitions
  definition = definitions["Cylinder"]
  if ! definition
    definition = definitions.add "Cylinder"
    entities = definition.entities
    circleBase = entities.add_circle(ORIGIN,Z_AXIS,1,24)
    base = entities.add_face(circleBase)
    base.pushpull(-1)
  end
  definition
end

def addCylinder(x0,y0,z0,r,x1,y1,z1)
  Sketchup.active_model.start_operation("Cylinder", true)
  p = Geom::Point3d.new(x0,y0,z0)
  v = Geom::Vector3d.new(x1-x0,y1-y0,z1-z0)
  h = v.length
  xform = Geom::Transformation.new(p,v)*Geom::Transformation.scaling(r,r,h)
  shape = Sketchup.active_model.entities.add_instance(cylinderDefinition(),xform)
  Sketchup.active_model.commit_operation
  returnShape(shape)
end

def addCylinderZ(x,y,z,r,h)
  Sketchup.active_model.start_operation("Cylinder", true)
  xform = Geom::Transformation.translation(Geom::Vector3d.new(x,y,z))*Geom::Transformation.scaling(r,r,h)
  shape = Sketchup.active_model.entities.add_instance(cylinderDefinition(),xform)
  Sketchup.active_model.commit_operation
  returnShape(shape)
end

############
# Sketchup has a bug/feature:
#   Any face created on the Z=0 plane will always have the normal pointing down

def addTriangle(ent,p0,p1,p2)
  normal = (p0-p1).cross(p2-p1)
  if (normal.length < 1e-3)
    nil
  else
    face = ent.add_face(p0,p1,p2)
    if face.normal.dot(normal) < 0 # (p0.z == 0) and (p1.z == 0) and (p2.z == 0)
      face.reverse!
    end
    face
  end
end

=begin
def addQuad(ent,p0,p1,p2,p3)
  normal = (p3-p0).cross(p1-p0)
  if (normal.length < 1e-3)
    addTriangle(ent,p1,p2,p3)
  else
    normal = (p0-p1).cross(p2-p1)
    if (normal.length < 1e-3)
      addTriangle(ent,p0,p2,p3)
    else
      normal = (p1-p2).cross(p3-p2)
      if (normal.length < 1e-3)
        addTriangle(ent,p0,p1,p3)
      else
        normal = (p2-p3).cross(p0-p3)
        if (normal.length < 1e-3)
          addTriangle(ent,p0,p1,p2)
        else
          face = ent.add_face(p0,p1,p2,p3)
          if face.normal.dot(normal) > 0
            face.reverse!
          end
        end
      end
    end
  end
end
=end

def addQuad(ent,p0,p1,p2,p3)
  normal = (p3-p0).cross(p1-p0)
  face = ent.add_face(p0,p1,p2,p3)
  if face.normal.dot(normal) > 0
    face.reverse!
  end
end


def addFace(ent,ps)
  p0 = Geom::Point3d.new(ps[0])
  p1 = Geom::Point3d.new(ps[1])
  p2 = Geom::Point3d.new(ps[2])
  expected_normal = (p0-p1).cross(p2-p1)
  if (expected_normal.length < 1e-3)
    nil
  else
    face = ent.add_face(ps)
    if face.normal.dot(expected_normal) > 0
      face.reverse!
    end
    face
  end
end

def addFace2(ent,ps)
  p0 = ps[0]
  p1 = ps[1]
  p2 = ps[2]
  expected_normal = (p0-p1).cross(p2-p1)
  if (expected_normal.length < 1e-3)
    nil
  else
    face = ent.add_face(ps)
    if face.normal.dot(expected_normal) > 0
      face.reverse!
    end
    face
  end
end

def isClosed!(ent)
  ent.set_attribute("rosetta","closed?",true)
end

def isClosed?(ent)
  ent.get_attribute("rosetta","closed?",false)
end

def isSmooth!(ent)
  ent.set_attribute("rosetta","smooth?",true)
end

def isSmooth?(ent)
  ent.get_attribute("rosetta","smooth?",false)
end

def setBoundary!(ent,curves)
  ent.set_attribute("rosetta","boundary",curves.map{|c|returnShape(c).to_s})
end

def getBoundary(ent)
  ent.get_attribute("rosetta","boundary").map{|id|resolveShape(id) }
end

def shapeBoundary(ent)
  getBoundary(ent) or [ent]
end

###############################################################################
# modeling primitives


# point
def addPoint(x,y,z)
  Sketchup.active_model.entities.add_cpoint([x,y,z])
end

# circle
def addCircle(x,y,z,r,nx,ny,nz,filled=false)
  n = 24
  entities = Sketchup.active_model.entities.add_group.entities
  normal = Geom::Vector3d.new(nx,ny,nz)
  edges = entities.add_circle([x,y,z],normal,r,n)
  if filled
    face = entities.add_face(edges)
    face.reverse! if face.normal.dot(normal) < 0
    isSmooth!(face)
    returnShape(face)
  else
    curve = edges[0].curve
    isClosed!(curve)
    isSmooth!(curve)
    returnShape(curve)
  end
end

def addCircleZ(x,y,z,r,filled=false)
  addCircle(x,y,z,r,0,0,1,filled)
end  

def addSurfaceCircle(x,y,z,r,nx,ny,nz)
  addCircle(x,y,z,r,nx,ny,nz,true)
end

def addSurfaceCircleZ(x,y,z,r)
  addCircleZ(x,y,z,r,true)
end

# ellipse

def addEllipse(x,y,z,xx,xy,xz,yx,yy,yz,ra,rb,filled=false)
  n = 24
  entities = Sketchup.active_model.entities.add_group.entities
  edges = entities.add_circle(ORIGIN,Z_AXIS,1,n)
  origin = Geom::Point3d.new(x,y,z)
  xaxis = Geom::Vector3d.new(xx,xy,xz)
  yaxis = Geom::Vector3d.new(yx,yy,yz)
  tr = Geom::Transformation.new(origin,xaxis,yaxis)*Geom::Transformation.scaling(ra,rb,1)
  entities.transform_entities(tr,edges)
  if filled
    face = entities.add_face(edges)
    normal = xaxis.cross(yaxis)
    face.reverse! if face.normal.dot(normal) < 0
    isSmooth!(face)
    returnShape(face)
  else
    curve = edges[0].curve
    isClosed!(curve)
    isSmooth!(curve)
    returnShape(curve)
  end
end

# arc
def addArc(x,y,z,xx,xy,xz,nx,ny,nz,r,sa,ea,filled=false)
  n = 24
  entities = Sketchup.active_model.entities.add_group.entities
  normal = Geom::Vector3d.new(nx,ny,nz)
  edges = entities.add_arc([x,y,z],[xx,xy,xz],normal,r,sa,ea,n)
  if filled
    edge = entities.add_line(edges[0].start,edges[-1].end)
    face = entities.add_face(edges << edge)
    face.reverse! if face.normal.dot(normal) < 0
    #entities.erase_entities(edges)
    returnShape(face)
  else
    curve = edges[0].curve
    if edges[0].start.position==edges[-1].end.position
      isClosed!(curve)
    end
    isSmooth!(curve)
    returnShape(curve)
  end
end

def addArcZ(x,y,z,r,sa,ea,filled=false)
  addArc(x,y,z,1,0,0,0,0,1,r,sa,ea,filled)
end

def addSurfaceArc(x,y,z,xx,xy,xz,nx,ny,nz,r,sa,ea)
  addArc(x,y,z,xx,xy,xz,nx,ny,nz,r,sa,ea,true)
end

def addSurfaceArcZ(x,y,z,r,sa,ea)
  addArcZ(x,y,z,r,sa,ea,true)
end

# line
def addLine(pts)
  entities = Sketchup.active_model.entities.add_group.entities
  edges = entities.add_curve(pts)
  returnShape(edges[0].curve)
end

# smooth line
def addSmoothLine(pts)
  entities = Sketchup.active_model.entities.add_group.entities
  edges = entities.add_curve(pts)
  curve = edges[0].curve
  isSmooth!(curve)
  returnShape(curve)
end

# polygonal surface
def addPolygon(pts)
  entities = Sketchup.active_model.entities.add_group.entities
  pts << pts.first
  edges = entities.add_curve(pts)
  curve = edges[0].curve
  isClosed!(curve)
  returnShape(curve)
end

# polygonal surface
def addSurfacePolygon(pts)
  entities = Sketchup.active_model.entities.add_group.entities
  face = addFace(entities,pts)
  returnShape(face)
end

# spline

def addBezier(pts)
  nPts = pts.length
  n = nPts * 2
  curvepts = []
  dt = 1.0/n
  for i in 0..n
    t = i * dt
    curvepts[i] = evaluateBezier(pts, t, nPts)
  end
  addSmoothLine(curvepts)
end

def evaluateBezier(pts, t, n)
  degree = n - 1
  t1 = 1.0 - t
  fact = 1.0
  n_choose_i = 1
  x = pts[0].x * t1
  y = pts[0].y * t1
  z = pts[0].z * t1
  for i in 1...degree
    fact = fact*t
    n_choose_i = n_choose_i*(degree-i+1)/i
    fn = fact * n_choose_i
    x = (x + fn*pts[i].x) * t1
    y = (y + fn*pts[i].y) * t1
    z = (z + fn*pts[i].z) * t1
  end
  x = x + fact*t*pts[degree].x
  y = y + fact*t*pts[degree].y
  z = z + fact*t*pts[degree].z
  Geom::Point3d.new(x, y, z)
end

def addCubicBezier(pts,nt)
  pts = addCubicBezierPoints(pts)
  curve = []
  nseg = pts.length-1
  aux_cpts = cubicBezierControlPoints(pts)
  aux_ptscpts = cubicBezierJoin(pts,aux_cpts)
  for i in (0..nseg-1)
    aux_p0 = aux_ptscpts[3*i]
    aux_p1 = aux_ptscpts[3*i+1]
    aux_p2 = aux_ptscpts[3*i+2]
    aux_p3 = aux_ptscpts[3*i+3]
    aux_abc = cubicBezierCoefAbc(aux_p0,aux_p1,aux_p2,aux_p3)
    aux_segment = cubicBezierSegment(aux_p0,aux_abc,nt)
    aux_segment.pop
    curve = curve+aux_segment
  end
  curve.push(aux_ptscpts[3*nseg])
  addSmoothLine(curve[nt..-(nt+1)])
end

def addCubicBezierPoints(pts)
  pt1 = pts[0]
  pt2 = pts[1]
  vec = pt2.vector_to pt1
  d = pt1.distance pt2
  ptbeg = pt1.offset vec, d
  pt1 = pts[-1]
  pt2 = pts[-2]
  vec = pt2.vector_to pt1
  vec = pts[-3].vector_to pt1 unless vec.valid?	
  d = pt1.distance pt2
  ptend = pt1.offset vec, d
  [ptbeg] + pts + [ptend]
end

# given a nt integer (number of segments to interpolate) interpolate nt points of a segment
def cubicBezierSegment(p0, abc, nt)
  segment = []
  for i in (0..nt)
    segment[i] = cubicBezierPoint(p0,abc,i/nt.to_f)
  end
  segment
end


# given a point, the abc coeficients and a 0<=t<=1, interpolate a point using the cubic formula
def cubicBezierPoint(p0, abc, t)
  [abc[0][0]*t*t*t+abc[1][0]*t*t+abc[2][0]*t+p0[0],abc[0][1]*t*t*t+abc[1][1]*t*t+abc[2][1]*t+p0[1],abc[0][2]*t*t*t+abc[1][2]*t*t+abc[2][2]*t+p0[2]]
end

def cubicBezierCoefAbc(p0, p1, p2, p3)
  aux_c = [3*(p1[0]-p0[0]),3*(p1[1]-p0[1]),3*(p1[2]-p0[2])]
  aux_b = [3*(p2[0]-p1[0])-aux_c[0],3*(p2[1]-p1[1])-aux_c[1],3*(p2[2]-p1[2])-aux_c[2]]
  aux_a = [p3[0]-p0[0]-aux_c[0]-aux_b[0],p3[1]-p0[1]-aux_c[1]-aux_b[1],p3[2]-p0[2]-aux_c[2]-aux_b[2]]
  [aux_a,aux_b,aux_c]
end

def cubicBezierControlPoints(pts)
  cpts = []
  aux_a = []
  aux_b = []
  np = pts.length-1
  cpts[0] = [(pts[1][0]-pts[0][0])/3,(pts[1][1]-pts[0][1])/3,(pts[1][2]-pts[0][2])/3]
  cpts[np] = [(pts[np][0]-pts[np-1][0])/3,(pts[np][1]-pts[np-1][1])/3,(pts[np][2]-pts[np-1][2])/3]                          
  aux_b[1] = -0.25
  aux_a[1] = [(pts[2][0]-pts[0][0]-cpts[0][0])/4,(pts[2][1]-pts[0][1]-cpts[0][1])/4,(pts[2][2]-pts[0][2]-cpts[0][2])/4]
  for i in (2..np-1)
    aux_b[i] = -1/(4+aux_b[i-1])
    aux_a[i] = [-(pts[i+1][0]-pts[i-1][0]-aux_a[i-1][0])*aux_b[i],-(pts[i+1][1]-pts[i-1][1]-aux_a[i-1][1])*aux_b[i],-(pts[i+1][2]-pts[i-1][2]-aux_a[i-1][2])*aux_b[i]]
  end
  for i in (1..np-1)
    cpts[np-i]=[aux_a[np-i][0]+aux_b[np-i]*cpts[np-i+1][0],aux_a[np-i][1]+aux_b[np-i]*cpts[np-i+1][1],aux_a[np-i][2]+aux_b[np-i]*cpts[np-i+1][2]]
  end
  cpts
end


# join two vectors, main points vector and cpoints vector
def cubicBezierJoin(pts,cpts)
  ptscpts = []
  np = pts.length-1
  for i in (0..np-1)
    ptscpts.push(pts[i])
    ptscpts.push([pts[i][0]+cpts[i][0],pts[i][1]+cpts[i][1],pts[i][2]+cpts[i][2]])
    ptscpts.push([pts[i+1][0]-cpts[i+1][0],pts[i+1][1]-cpts[i+1][1],pts[i+1][2]-cpts[i+1][2]])
  end
  ptscpts.push(pts[np])
  ptscpts
end

# joining curves
def appendVertices(curves)
  ([curves[0].vertices]+curves.drop(1).map{|c| c.vertices.drop(1)}).flatten
end

def joinCurves(ids)
  curves = ids.map!{|id| resolveShape(id)}
  vs = appendVertices(curves)
  entities = Sketchup.active_model.entities.add_group.entities
  edges = entities.add_curve(vs)
  curve = edges[0].curve
  setBoundary!(curve,curves)
#  curves.each{|c| c.model.entities.erase_entities(c.edges)}
  curves.each{|c| c.each_edge{|e| e.visible=false}}
  returnShape(curve)
end

# surface

def addSurface(ids)
  curves = ids.map!{|id| getBoundary(resolveShape(id))}.flatten 
  vs = appendVertices(curves)
  entities = Sketchup.active_model.entities.add_group.entities
  face = addFace2(entities,vs.map(&:position))
  setBoundary!(face,curves)
#  curves.each{|c| c.model.entities.erase_entities(c.edges)}
  curves.each{|c| c.each_edge{|e| e.visible=false}}
  returnShape(face)
end

# solids

def addConeFrustum(x0,y0,z0,r0,x1,y1,z1,r1)
  n=24
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  p0 = Geom::Point3d.new(x0,y0,z0)
  p1 = Geom::Point3d.new(x1,y1,z1)
  h = p0.distance(p1)
  t = Geom::Point3d.new(0,0,1)
  circleBase = entities.add_circle(ORIGIN,Z_AXIS.reverse,1,n)
  base = entities.add_face(circleBase)
  baseVertices = base.vertices
  circleTop = entities.add_circle(t,Z_AXIS,r1/r0,n)
  top = entities.add_face(circleTop)
  topVertices = top.vertices
  face = entities.add_face(Geom::Point3d.new(1,0,0),ORIGIN,t,Geom::Point3d.new(r1/r0,0,1))
  face.followme circleBase
  line = entities.add_line(baseVertices[0],baseVertices[12])
  line.find_faces
  line.erase!
  line = entities.add_line(topVertices[0],topVertices[12])
  line.find_faces
  line.erase!
  v = Geom::Vector3d.new(x1-x0,y1-y0,z1-z0)
  xform = Geom::Transformation.new(p0,v)*Geom::Transformation.scaling(r0,r0,h)
  group.transform!(xform)
  returnShape(group)
end

def addConeFrustumZ(x,y,z,r0,h,r1)
  n=24
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  p = Geom::Point3d.new(x,y,z)
  apex = Geom::Point3d.new(x,y,z+h)
  pr0 = Geom::Point3d.new(x+r0,y,z)
  pr1 = Geom::Point3d.new(x+r1,y,z+h)
  circleBase = entities.add_circle(p,Z_AXIS.reverse,r0,n)
  base = entities.add_face(circleBase)
  baseVertices = base.vertices
  circleTop = entities.add_circle(apex,Z_AXIS,r1,n)
  top = entities.add_face(circleTop)
  topVertices = top.vertices
  face = entities.add_face(pr0,p,apex,pr1)
  face.followme circleBase
  line = entities.add_line(baseVertices[0],baseVertices[12])
  line.find_faces
  line.erase!
  line = entities.add_line(topVertices[0],topVertices[12])
  line.find_faces
  line.erase!
  returnShape(group)
end

# general frustum (prismoid)
def addFrustum(pbs,pts)
  Sketchup.active_model.start_operation("Frustum", true)
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  n = pbs.length
  pbs = pbs.map! {|e| Geom::Point3d.new(e)}
  pts = pts.map! {|e| Geom::Point3d.new(e)}
  0.upto(n-1) do |i|
    addQuad(entities,pbs[i],pbs[(i+1)%n],pts[(i+1)%n],pts[i])
  end
  addFace2(entities,pbs.reverse!)
  addFace2(entities,pts)
  Sketchup.active_model.commit_operation
  returnShape(group)
end

# pyramid
def addPyramid(pbs,pt)
  Sketchup.active_model.start_operation("Pyramid", true)
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  n = pbs.length
  pbs = pbs.map! {|e| Geom::Point3d.new(e)}
  pt = Geom::Point3d.new(pt)
  expected_normal = pbs[0]-pt
  face = entities.add_face(pbs)
  if face.normal.dot(expected_normal) < 0
    face.reverse!
  end
  0.upto(n-1) do |i|
    entities.add_face(pbs[i],pt,pbs[(i+1)%n])
  end
  Sketchup.active_model.commit_operation
  returnShape(group)
end

=begin
def addPyramid(pbs,pt)
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  n = pbs.length
  pm = Geom::PolygonMesh.new(n+1,n)
  p0 = pm.add_point(pt)
  0.upto(n-1) do |i|
    p1 = pm.add_point(pbs[i])
    p2 = pm.add_point(pbs[(i+1)%n])
    pm.add_polygon(p0,p1,p2)
  end
  entities.fill_from_mesh(pm,true,Geom::PolygonMesh::NO_SMOOTH_OR_HIDE)
  returnShape(group)
end
=end

# text
# Note: alignment is only meaningful for multi-line text
def createText(str,sz,alignment)
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  entities.add_3d_text(str, alignment, "Arial", false, false, sz, 0.0, 0.0, true, 0.0)
  group
end

def addText(str,x,y,z,sz)
  group = createText(str,sz,TextAlignLeft)
  point = Geom::Point3d.new x,y,z
  trans = Geom::Transformation.new point
  group = group.move! trans
  returnShape(group)
end

def addTextCentered(str,x,y,z,sz)
  group = createText(str,sz,TextAlignCenter)
  point = Geom::Point3d.new(x,y,z)-(Geom::Point3d.new(0,0,0).vector_to(group.bounds.center))
  trans = Geom::Transformation.new point
  group = group.move! trans
  returnShape(group)
end

def addTextTrans(str,sz,m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)
  group = createText(str,sz,TextAlignLeft)
  returnShape(transform(group,m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33))
end

def addTextCenteredTrans(str,sz,m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)
  group = createText(str,sz,TextAlignCenter)
  xform = transformation(m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)*
    Geom::Transformation.translation(group.bounds.center.vector_to(Geom::Point3d.new(0,0,0)))
  group.transform! xform
  returnShape(group)
end

# torus
def addTorus(x,y,z,nx,ny,nz,re,ri)
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  circle1 = entities.add_circle(Geom::Point3d.new(re*1000,0,0),Y_AXIS,ri*1000,24)
  circle2 = entities.add_circle(ORIGIN,Z_AXIS,re*1000,24)
  face1 = entities.add_face(circle1)
  face1.followme circle2
  entities.erase_entities circle2
  tr = Geom::Transformation.new(Geom::Point3d.new(x,y,z),Geom::Vector3d.new(nx,ny,nz))*Geom::Transformation.scaling(0.001)
  group.transform!(tr)
  returnShape(group)
end

# regular pyramid
def addRegularPyramid(x0,y0,z0,r,x1,y1,z1)
end

# surface
def addSurfaceGrid(data)
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  n = data.length
  m = data[0].length
  pm = Geom::PolygonMesh.new(n*m,(n-1)*(m-1)*2)
  0.upto(n-2) do |i|
    0.upto(m-2) do |j|
      p0 = pm.add_point(Geom::Point3d.new(data[i][j]))
      p1 = pm.add_point(Geom::Point3d.new(data[i+1][j]))
      p2 = pm.add_point(Geom::Point3d.new(data[i][j+1]))
      p3 = pm.add_point(Geom::Point3d.new(data[i+1][j+1]))
      pm.add_polygon(p0,p1,p3)
      pm.add_polygon(p3,p2,p0)
    end
  end
  entities.fill_from_mesh(pm,true)
  returnShape(group)
end


# transformation
def transformation(m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)
  Geom::Transformation.new([m00,m10,m20,m30,
                            m01,m11,m21,m31,
                            m02,m12,m22,m32,
                            m03,m13,m23,m33])
end

def transform(shape,m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)
  shape.transform!(transformation(m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33))
  shape
end

def applyTransformation(id,m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)
  shape = resolveShape(id)
  transform(shape,m00,m01,m02,m03,m10,m11,m12,m13,m20,m21,m22,m23,m30,m31,m32,m33)
  ""
end

def faceCentroid(face)
   pts = face.vertices
   cx = cy = cz = 0
   n = pts.length
   for i in 0..n-2
      pt = pts[i].position
      cx += pt.x
      cy += pt.y
      cz += pt.z
   end
   Geom::Point3d.new(cx/n, cy/n, cz/n)
end

# extrude

def extrudeCurve(id,v)
  edges = resolveShape(id)
  p1 p2 are two points that define the vector
model = Sketchup.active_model

if Sketchup.version[0,1].to_i > 6
	model.start_operation((db("Extrude Edges by Vector")),true)
    ### 'false' is best to see results as UI/msgboxes...
else
	model.start_operation((db("Extrude Edges by Vector")))
end  


dx = p2.x - p1.x
dy = p2.y - p1.y
dz = p2.z - p1.z

model.selection.clear

cverts=[]
edges=[]
@ss.each{|e|
  cverts<< e.curve.vertices if e.curve and not cverts.include?(e.curve.vertices)
  edges<< e if not e.curve
}
cpoints=[]
cverts.each{|verts|
  pts=[]
  verts.each{|vert|pts<<vert.position}
  cpoints<< pts
}
if @group and @group.valid?
  group=@group
else
  group=model.active_entities.add_group()
  @group=group
end#if
ssa=[]
edges.each{|e|ssa<<group.entities.add_line(e.start.position,e.end.position)}
cpoints.each{|pts|ssa<<group.entities.add_curve(pts)}
ssa.flatten!

tr=Geom::Transformation.translation(p1.vector_to(p2))
points=[]
edges.each{|e|points<<[e.start.position,e.end.position]}
points_top=[]
points.each{|pints|
  pts=[]
  pints.each{|point|
    pt=point.clone
    pt.transform!(tr)
    pts << pt
  }
  points_top<<pts
}

clones=[]

points_top.each{|pts|clones<<group.entities.add_line(pts)}
cpoints_top=[]
cpoints.each{|points|
  pts=[]
  points.each{|point|
    pt=point.clone
    pt.transform!(tr)
    pts << pt
  }
  cpoints_top<<pts
}
cpoints_top.each{|pts|clones<<group.entities.add_curve(pts)}

clones.flatten!

clverts=[]
clones.each{|e|clverts<<[e.vertices[0].position,e.vertices[1].position]}

exfaces=[]
group.entities.each{|e|exfaces<<e if e.class==Sketchup::Face}

0.upto(ssa.length-1) do |i|
  edge=ssa[i]
 begin
  v1 = edge.vertices[0].position
  v2 = edge.vertices[1].position
  v1_top = Geom::Point3d.new(v1.x+dx, v1.y+dy, v1.z+dz)
  v2_top = Geom::Point3d.new(v2.x+dx, v2.y+dy, v2.z+dz)
  e1=group.entities.add_line(v1, v1_top)
  e2=group.entities.add_line(v2_top, v2)
  if edge.curve or (edge.smooth? or edge.soft?)
    mid=0
    e1.start.edges.each{|e|mid=mid+1 if e.curve}
    e2.start.edges.each{|e|mid=mid+1 if e.curve}
    if mid==4
      e1.smooth = true;e1.soft = true
      e2.smooth = true;e2.soft = true
    end#if
  end#if
  e1.find_faces
 rescue
  ###
 end#begin
  i=i+1 ### next edge
end # of upto
###
allfaces=[]
group.entities.each{|e|allfaces<<e if e.class==Sketchup::Face}
newfaces=allfaces-exfaces
newfaces[0].reverse! if newfaces[0].normal.z<0 ###
orient_connected_faces(newfaces[0],newfaces) if newfaces[0] and  newfaces[0].class==Sketchup::Face
### orient faces...
###
### reform curves
group.entities.to_a.each{|e|e.explode_curve if e.valid? and e.class==Sketchup::Edge and e.curve}
###
gpx=group.entities.add_group()
cvs=[]
cpoints.each{|pts|cvs=gpx.entities.add_curve(pts)}
cvs_top=[]
cpoints_top.each{|pts|cvs_top=gpx.entities.add_curve(pts)}
gpx.explode
###
### allow for 'merged' edges in selection...
clverts.each{|verts|
  group.entities.to_a.each{|e|
    if e.valid? and e.class==Sketchup::Edge
      if (e.vertices[0].position==verts[0] or e.vertices[0].position==verts[1]) and (e.vertices[1].position==verts[0] or e.vertices[1].position==verts[1])
        model.selection.add(e)### as it's a matching edge
      end#if
    end#if
  }
}

end#def


# sweep
def addSweep(idPath, idProfile)
  path = resolveShape(idPath)
  profile = resolveShape(idProfile)
 # group = Sketchup.active_model.entities.add_group(profile)
  edges = path.edges
  edge = edges[0]
  normal = edge.end.position-edge.start.position
  tr = Geom::Transformation.new(edge.start.position,normal)
  Sketchup.active_model.entities.transform_entities(tr,profile)
  if profile.class==Sketchup::Face
    if profile.normal.dot(normal) > 0
       profile.reverse!
    end
    profile.followme edges
  else
    profile.entities.to_a.each{|e|
      if e.class==Sketchup::Face
        if e.normal.dot(normal) > 0
          e.reverse!
        end
        e.followme edges
      end
    }
  end
  returnShape(path.parent)
end

# boolean operations
def addUnion(id0, id1)
  sh0 = resolveShape(id0)
  sh1 = resolveShape(id1)
  result = nil
#  if sh0.manifold? and sh1.manifold?
    result = sh0.union(sh1)
#  end
  if ! result
    result = Sketchup.active_model.entities.add_group(*sh0,*sh1)
  end    
  returnShape(result)
end

def addIntersection(id0, id1)
  sh0 = resolveShape(id0)
  sh1 = resolveShape(id1)
  returnShape(sh0.intersect(sh1))
end

def addSubtraction(id0, id1)
  sh0 = resolveShape(id0)
  sh1 = resolveShape(id1)
  result = sh1.subtract(sh0)
  if ! result
    result = sh0
  end
  returnShape(result)
end

def addSlice(id,x,y,z,nx,ny,nz)
  sh = resolveShape(id)
  p = Geom::Point3d.new(x,y,z)
  v = Geom::Vector3d.new(nx,ny,nz)
  Sketchup.active_model.start_operation("Cylinder", true)
  bb = sh.bounds
  r = 2*bb.diagonal
  xform = Geom::Transformation.new(p,v)*Geom::Transformation.scaling(r,r,r)
  cutter = Sketchup.active_model.entities.add_instance(cylinderDefinition(),xform)
  result = cutter.subtract(sh)
  if ! result
    result = sh
  end
  Sketchup.active_model.commit_operation
  returnShape(result)
end

def copy(id)
  sh = resolveShape(id)
  Sketchup.active_model.start_operation("Copy", true)
  sh.copy
  Sketchup.active_model.commit_operation
  returnShape(result)
end


###############################################################################
# modeling operations

# extrusion
def addExtrusion(idProfile,nx,ny,nz)
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  prof = resolveShape(idProfile)
  closedP = prof.class==Sketchup::Face or isClosed?(prof)
  profiles = shapeBoundary(prof)
  v = Geom::Vector3d.new(nx,ny,nz)
  n = profiles.map(&:edges).map(&:count).inject(0){|x,y| x + y}
  pm = Geom::PolygonMesh.new(3*n+1,2*n+2)
  botVerts = Array.new
  topVerts = Array.new
  for profile in profiles do
    botVerts.delete_at(-1)
    topVerts.delete_at(-1)
    verts = profile.vertices
    n = verts.length
    p0 = verts[0].position
    p3 = p0 + v
    p0 = pm.add_point(p0)
    p3 = pm.add_point(p3)
    botVerts << p0
    topVerts << p3
    if isSmooth?(profile)
      for i in 1..n-1
        p1 = verts[i].position
        p2 = p1 + v
        p1 = pm.add_point(p1)
        p2 = pm.add_point(p2)
        pm.add_polygon(p0,-p1,p2,-p3)
        botVerts << p1
        topVerts << p2
        p0 = p1
        p3 = p2
      end
    else
      for i in 1..n-1
        p1 = verts[i].position
        p2 = p1 + v
        p1 = pm.add_point(p1)
        p2 = pm.add_point(p2)
        pm.add_polygon(p0,p1,p2,p3)
        botVerts << p1
        topVerts << p2
        p0 = p1
        p3 = p2
      end
    end
  end
  if closedP
    pm.add_polygon(botVerts[-1],botVerts[0],topVerts[0],topVerts[-1])
  end
  entities.fill_from_mesh(pm,true,Geom::PolygonMesh::SOFTEN_BASED_ON_INDEX|Geom::PolygonMesh::SMOOTH_SOFT_EDGES)
  if prof.class==Sketchup::Face
    addFace(entities,botVerts.reverse!.map!{|i| pm.point_at(i)})
    addFace(entities,topVerts.map!{|i| pm.point_at(i)})
  end
  prof.model.entities.erase_entities(prof.edges) unless prof.deleted?
  returnShape(group)
end

def addExtrusionZ(idProfile,h)
  addExtrusion(idProfile,0,0,h)
end
=begin
# revolve

def revolveCurve(curve,p,v,segs,angle)
  group = Sketchup.active_model.entities.add_group
  entities = group.entities
  points=[]
  (segs+1).times do |i|
    pts=[]
    tr=Geom::Transformation.rotation(p,v,(i)*(angle)/segs)
    curve.vertices.each{|v|
      pt=v.position
      pt.transform!(tr)
      pts<<pt
    }
    points<<pts
  end
  vlines=[]
  points.each{|pts|
    pts.each{|pt|
      ptnext=pts[pts.index(pt)+1]
      vline=entities.add_line(pt,ptnext)if ptnext
      vlines<<vline if vline
    }
  }
  lines=[]
  points.each{|pts|
    pts.length.times do |i|
      pt=pts[i]
      ptnext=points[points.index(pts)+1][i]if points[points.index(pts)+1]
      ptup=points[points.index(pts)+1][i+1]if points[points.index(pts)+1]
      if ptnext
        line=entities.add_line(pt,ptnext)
        lines<<line
        if ptup
          lineup=entities.add_line(pt,ptup)### = the diagonal
        end#if
      end#if
    end#do
  }
  lines.each{|e| ### make faces
    if e and e.valid?
      e.find_faces
    end#if
  }
  ### tidy faceless edges...
  group.entities.to_a.each{|e|e.erase! if e.class==Sketchup::Edge and not e.faces[0]}
  ###
  if true
    edges=[]
    entities.each{|e|edges<<e if e.class==Sketchup::Edge}
    4*segs.times do |this|
      edges.each{|e|
        e.erase! if e.valid? and e.faces.length==2 and e.faces[0].normal.dot(e.faces[1].normal) > 0.999999999999
        e.erase! if e.valid? and not e.faces[0]
      }
    end#times
    ### tidy faceless edges...
    group.entities.to_a.each{|e|e.erase! if e.class==Sketchup::Edge and not e.faces[0]}
  end#if
  entities.each{|e|
    if e.class==Sketchup::Edge
      e.soft=true
      e.smooth=true
    end#if
  }
end

def addRevolve(idProfile,px,py,pz,nx,ny,nz,start,amplitude)
  Sketchup.active_model.start_operation("Revolve", true)
  profile = resolveShape(idProfile)
  p = Geom::Point3d.new(px,py,pz)
  v = Geom::Vector3d.new(nx,ny,nz)
  segs = 24
  angle=pi
  points=[]
  (segs+1).times do |i|###copy curve_edge-set points and and rotate them
    pts=[]
    tr=Geom::Transformation.rotation(p,v,(i)*(angle)/segs)
    if not @face
      @curve.curve.vertices.each{|v|
        pt=v.position
        pt.transform!(tr)
        pts<<pt
      }
    else
      @face.loops.each{|loop|
        first=nil
        loop.vertices.each{|v|
          pt=v.position
          pt.transform!(tr)
          first=pt if not first
          pts<<pt
        }
        pts<<first if first ###close loop
      }
    end#if
    points<<pts
  end#times
  group=entities.add_group()
  gents=group.entities
  ### points is array of arrays of all points
  if not @face
    len=@curve.curve.vertices.length
  else
    len=0;@face.loops.each{|loop|loop.vertices.each{|v|len+=1}}
  end#if
  vlines=[]
  num=1
  points.each{|pts|
    pts.each{|pt|
      ptnext=pts[pts.index(pt)+1]
      vline=gents.add_line(pt,ptnext)if ptnext
      vlines<<vline if vline
      @msg=((db("Extrude Edges by Lathe: Making Primary Lines "))+(num.to_s)+(db(" of "))+((segs*len).to_s))
      Sketchup::set_status_text(@msg)
      num+=1
    }
  }
  lines=[]
  num=1
  points.each{|pts|
    pts.length.times do |i|
      pt=pts[i]
      ptnext=points[points.index(pts)+1][i]if points[points.index(pts)+1]
      ptup=points[points.index(pts)+1][i+1]if points[points.index(pts)+1]
      if ptnext
        line=gents.add_line(pt,ptnext)
        lines<<line
        if ptup
          lineup=gents.add_line(pt,ptup)### = the diagonal
        end#if
        @msg=((db("Extrude Edges by Lathe: Making Secondary Lines "))+(num.to_s)+(db(" of "))+(((1+segs)*len).to_s))
        Sketchup::set_status_text(@msg)
      num+=1
      end#if
    end#do
  }
  GC.start ### ### ###
  num=1
  lines.each{|e| ### make faces
    if e and e.valid?
      @msg=((db("Extrude Edges by Lathe: Making Faces "))+(num.to_s)+(db(" of "))+((1+segs)*len).to_s)
      Sketchup::set_status_text(@msg)
      e.find_faces
      num+=1
    end#if
  }
  @msg=(db("Extrude Edges by Lathe: Tidying Faces"))
  ### check faces have 3 sides if not erase them...
  segs.times do |this|
    Sketchup::set_status_text(@msg)
    gents.each{|e|if @angle>=360
        e.erase! if e.class==Sketchup::Face and e.edges.length>3
        e.find_faces if e.class==Sketchup::Edge and e.faces.length==1
      end#if
    }
    @msg=@msg+"."
  end#times
  ### tidy faceless edges...
  group.entities.to_a.each{|e|e.erase! if e.class==Sketchup::Edge and not e.faces[0]}
  ###
  ### set @model attributes for next time used...
  @model.set_attribute("ExtrudeEdgesByLathe","angle",@angle)
  @model.set_attribute("ExtrudeEdgesByLathe","segs",@segs)
  ###
 @model.commit_operation
  ### remove coplanar edges ?
  @msg=((db("Extrude Edges by Lathe: Remove Coplanar Edges ?")))
  Sketchup::set_status_text(@msg)
  cop=UI.messagebox((db("Extrude Edges by Lathe: Remove Coplanar Edges ?"))+"\n\n",MB_YESNO,"")### 6=YES 7=NO
  if cop==6
    if Sketchup.version.split('.')[0].to_i >= 7
      @model.start_operation("ExtrudeEdgesByLathe", true)
    else
      @model.start_operation("ExtrudeEdgesByLathe")
    end
    edges=[]
    gents.each{|e|edges<<e if e.class==Sketchup::Edge}
    @msg=(db("Extrude Edges by Lathe: Removing Coplanar Edges"))
    4*segs.times do |this|
      edges.each{|e|
        e.erase! if e.valid? and e.faces.length==2 and e.faces[0].normal.dot(e.faces[1].normal) > 0.999999999999
        e.erase! if e.valid? and not e.faces[0]
      }
      Sketchup::set_status_text(@msg)
      @msg=@msg+"."
    end#times
    ### tidy faceless edges...
    group.entities.to_a.each{|e|e.erase! if e.class==Sketchup::Edge and not e.faces[0]}
    @model.commit_operation
  end#if
  ###
  if Sketchup.version.split('.')[0].to_i >= 7
    @model.start_operation("ExtrudeEdgesByLathe", true)
  else
    @model.start_operation("ExtrudeEdgesByLathe")
  end
  ### make gable-end faces
  if @face and @angle<360
    vlines.first.find_faces if vlines.first.valid?
    vlines.last.find_faces if vlines.last.valid?
  end#if
  ### orient faces...
  faces=[];gents.each{|e|faces<<e if e.class==Sketchup::Face}
  faces.reverse! if faces[0] and faces[0].normal.z<0
  while faces[0]
    face=faces[0]
    face.reverse! if face.normal.z<=0
    face.orient_connected_lathed_faces
    connected=[];face.all_connected.each{|e|connected.push(e)if e.kind_of?(Sketchup::Face)}
    faces=faces-connected-[face]
  end#while
  @model.commit_operation
  ###
  ### reverse faces ?
  @msg=(db("Extrude Edges by Lathe: Reverse Faces ?"))
  Sketchup::set_status_text(@msg)
  rev=UI.messagebox((db("Extrude Edges by Lathe: Reverse Faces ?"))+"\n\n",MB_YESNO,"")### 6=YES 7=NO
  if rev==6
    if Sketchup.version.split('.')[0].to_i >= 7
      @model.start_operation("ExtrudeEdgesByLathe", true)
    else
      @model.start_operation("ExtrudeEdgesByLathe")
    end
    faces=[];gents.each{|e|faces<<e if e.class==Sketchup::Face}
    num=1
    faces.each{|face|
      @msg=((db("Extrude Edges by Lathe: Reversing Faces "))+(num.to_s)+(db(" of "))+(faces.length.to_s))
      Sketchup::set_status_text(@msg)
      face.reverse!
      num+=1
    }
    @model.commit_operation
  end#if
  @model.active_view.invalidate ### ???
  if UI.messagebox((db("Extrude Edges by Lathe: Smooth Edges ?")),MB_YESNO,"")==6 ### 6=YES 7=NO
    if Sketchup.version.split('.')[0].to_i >= 7
      @model.start_operation("ExtrudeEdgesByLathe", true)
    else
      @model.start_operation("ExtrudeEdgesByLathe")
    end
    gents.each{|e|
      if e.class==Sketchup::Edge
        e.soft=true
        e.smooth=true
      end#if
    }
    gpx=entities.add_group(group)
    group.explode
    group=gpx
    @model.commit_operation
  end#if
  ###
  @msg=(db("Extrude Edges by Lathe: Explode Group ?"))
  Sketchup::set_status_text(@msg)
  if UI.messagebox((db("Extrude Edges by Lathe: Explode Group ?")),MB_YESNO,"")==6 ### 6=YES 7=NO
    if Sketchup.version.split('.')[0].to_i >= 7
      @model.start_operation("ExtrudeEdgesByLathe", true)
    else
      @model.start_operation("ExtrudeEdgesByLathe")
    end
    group.explode
    @model.commit_operation
  end#if
  ###
  self.deactivate(@model.active_view)
end

=end


# We are adding new methods to the existing class
class Geom::PolygonMesh

# Revolve edges defined by an array of points about an axis
# pts is an Array of points
# axis is an Array with a point and a vector
# numsegments is the number of segments in the rotaion direction
def add_revolved_points(pts, axis, numsegments)

    # Make sure that there are enough points
    numpts = pts.length
    if( numpts < 2 )
        raise ArgumentError, "At least two points required", caller
    end

    #TODO: Determine if the points are all in the same plane as the axis
    planar = false
    
    # Create a transformation that will revolve the points
    angle = Math::PI * 2
    da = angle / numsegments
    t = Geom::Transformation.rotation(axis[0], axis[1], da)
    
    # Add the points to the mesh
    index_array = []
    for pt in pts do
        if( pt.on_line?(axis) )
            index_array.push( [self.add_point(pt)] )
        else
            indices = []
            for i in 0...numsegments do
                indices.push( self.add_point(pt) )
                #puts "add #{pt} at #{indices.last}"
                pt.transform!(t)
            end
            index_array.push indices
        end
    end
    
    # Now create polygons using the point indices
    i1 = index_array[0]
    for i in 1...numpts do
        i2 = index_array[i]
        n1 = i1.length
        n2 = i2.length
        nest if( n1 < numsegments && n2 < numsegments )
        
        for j in 0...numsegments do
            jp1 = (j + 1) % numsegments
            if( n1 < numsegments )
                self.add_polygon i1[0], i2[jp1], i2[j]
                #puts "add_poly #{i1[0]}, #{i2[jp1]}, #{i2[j]}"
            elsif( n2 < numsegments )
                self.add_polygon i1[j], i1[jp1], i2[0]
                #puts "add_poly #{i1[j]}, #{i1[jp1]}, #{i2[0]}"
            else
                if( planar )
                    self.add_polygon i1[j], i1[jp1], i2[jp1], i2[j]
                else
                    # Try adding two triangles instead
                    self.add_polygon i1[j], i1[jp1], i2[jp1]
                    self.add_polygon i1[j], i2[jp1], i2[j]
                end
                #puts "add_poly #{i1[j]}, #{i1[jp1]}, #{i2[jp1]}, #{i2[j]}"
            end
        end
        
        i1 = i2
    end
    
end

# Extrude points along an axis with a rotation
def add_extruded_points(pts, center, dir, angle, numsegments)

    # Make sure that there are enough points
    numpts = pts.length
    if( numpts < 2 )
        raise ArgumentError, "At least two points required", caller
    end

    # compute the transformation
    vec = Geom::Vector3d.new dir
    distance = vec.length
    dz = distance / numsegments
    da = angle / numsegments
    vec.length = dz
    t = Geom::Transformation.translation vec
    r = Geom::Transformation.rotation center, dir, da
    tform = t * r
    
    # Add the points to the mesh
    index_array = []
    for i in 0...numsegments do
        indices = []
        for pt in pts do
            indices.push( self.add_point(pt) )
            pt.transform!(tform)
        end
        index_array.push indices
    end
    
    # Now create polygons using the point indices
    i1 = index_array[0]
    for i in 1...numsegments do
        i2 = index_array[i]
        
        for j in 0...numpts do
            k = (j+1) % numpts
            self.add_polygon -i1[j], i2[k], -i1[k]
            self.add_polygon i1[j], -i2[j], -i2[k]
        end
        
        i1 = i2
    end
end

end #Geom::PolygonMesh

def revolve_test(num)
    p0 = Geom::Point3d.new 0, 0, -50
    p1 = Geom::Point3d.new 100, 0, 0
    p2 = Geom::Point3d.new 50, 0, 50
    p3 = Geom::Point3d.new 0, 0, 75
    pts = [p1, p2,p3]
    axis = [Geom::Point3d.new(0, 0, 0), Geom::Vector3d.new(-10, -1, 50)]

    npts = pts.length
    numpts = npts * num
    numpoly = (npts-1)*num
    mesh = Geom::PolygonMesh.new numpts, numpoly
    
    mesh.add_revolved_points pts, axis, num
    
    Sketchup.active_model.entities.add_faces_from_mesh mesh, 0
end

def extrude_test(dist, angle, num)
    model = Sketchup.active_model
    face = model.selection.first
    if( not face.kind_of?(Sketchup::Face) )
        puts "You must select a Face"
        return
    end
    pts = face.outer_loop.vertices.collect {|v| v.position}
    numpts = num * pts.length
    numpoly = (numpts - pts.length) * 2
    mesh = Geom::PolygonMesh.new numpts, numpoly
    
    vec = Geom::Vector3d.new 0, 0, dist
    mesh.add_extruded_points pts, ORIGIN, vec, angle, num

    model.entities.add_faces_from_mesh mesh, 9
    
    true
end

def addCurveExtrusion(curve, center, v, angle, scale, n)
  pts = curve.vertices
  d = vec.length
   vec.length = dz
    t = Geom::Transformation.translation vec
    r = Geom::Transformation.rotation center, dir, da
    tform = t * r
    
    # Add the points to the mesh
    index_array = []
  for i in 0...n do
    indices = []
    for pt in pts do
      indices.push( self.add_point(pt) )
      pt.transform!(tform)
    end
    index_array.push indices
  end
    
    # Now create polygons using the point indices
    i1 = index_array[0]
    for i in 1...numsegments do
        i2 = index_array[i]
        
        for j in 0...numpts do
            k = (j+1) % numpts
            self.add_polygon -i1[j], i2[k], -i1[k]
            self.add_polygon i1[j], -i2[j], -i2[k]
        end
        
        i1 = i2
    end
    
end

def mirror(id,x,y,z,a,b,c)
  sh = resolveShape(id)
  Sketchup.active_model.start_operation("Mirror", true)
  d = -a*x-b*y-c*z
  trans = Geom::Transformation.new([-2*a*a+1,-2*b*a,   -2*c*a, 0,
									-2*a*b,  -2*b*b+1, -2*c*b, 0,
									-2*a*c,  -2*b*c, -2*c*c+1, 0,
									-2*a*d,  -2*b*d,   -2*c*d, 1])
  sh.transform!(trans)
  Sketchup.active_model.commit_operation
  returnShape(sh)
end

###############################################################################
# object operations

def boundingBox(id)
  bb = resolveShape(id).bounds
  "%s|%s" % [bb.corner(0).to_a,bb.corner(7).to_a]
end

###############################################################################
# view operations

def camera()
  Sketchup.active_model.active_view.camera.eye.to_a
end

def target()
  Sketchup.active_model.active_view.camera.target.to_a
end

def lens()
  Sketchup.active_model.active_view.camera.focal_length
end

def view(ex, ey, ez, tx, ty, tz, lens)
  Sketchup.active_model.active_view.camera = Sketchup::Camera.new [ex,ey,ez], [tx,ty,tz], [0,0,1]
  Sketchup.active_model.active_view.camera.focal_length = lens
end

def viewTop()
  Sketchup.send_action("viewTop:")
  ""
end

def zoomExtents()
  # Doesn't work? Sketchup.active_model.active_view.zoom_extents
  Sketchup.send_action("viewZoomExtents:")
  ""
end

def renderView(path, width, height)
  keys = {
   :filename => path,
   :width => width,
   :height => height,
   :antialias => true,
   :compression => 0.9,
   :transparent => true
  }
  Sketchup.active_model.active_view.write_image keys
  ""
end

class TrackMouseTool

  def initialize(msg)
    @msg = msg
    @done = false
  end

  def activate
    @ip = Sketchup::InputPoint.new
  end

  def onMouseMove(flags,x,y,view)
    @ip.pick(view,x,y)
  end

#  def onLButtonDown(flags,x,y,view)
#  end

  def onLButtonUp(flags,x,y,view)
    @ip.pick(view,x,y)
    @done = true
    Sketchup.send_action "selectSelectionTool:"
  end

  def getPoint()
    if @done
      @ip.position
    else
      nil
    end
  end
end

$TrackMouse = nil

def startGetPoint(msg)
  $TrackMouse = TrackMouseTool.new("Select Location")
  Sketchup.active_model.select_tool $TrackMouse
  ""
end

def getPoint()
  p = $TrackMouse.getPoint
  if p
    # Sketchup works in inches, we prefer mm
    p.to_a
  else
    "[12345, 54321, 98765]"
  end
end

=begin

class SelectTool
  def initialize
    @ip0 = Sketchup::InputPoint.new
    @ip1 = Sketchup::InputPoint.new
    @mod= Sketchup.active_model
    @ent = @mod.active_entities
    @sel = @mod.selection
    self.reset
    view = @mod.active_view.lock_inference
  end

  def reset
    @best = nil
    @edge = nil
    @ip1.clear
    @labels=[]
    @beg_pts=[]
    @end_pts=[]
    @drawn = false
  end

  def onMouseMove(flags, x, y, view)
    self.set_current_point(x, y, view)
    ph = view.pick_helper
    ph.do_pick(x, y)
    @edge = ph.picked_edge if ph.picked_edge != @edge
    @best = ph.best_picked if ph.best_picked != @best
    Sketchup::set_status_text("Select Object.", SB_PROMPT)
  end

  def set_current_point(x, y, view)
    if(!@ip0.pick(view, x, y, @ip1))
      return false
    end
    @ip1.copy! @ip0
    view.tooltip = @ip0.tooltip     
    view.refresh
  end

  def onLButtonDown(flags, x, y, view)
    if (@edge && @best.is_a?(Sketchup::Edge))
      @curve=@best.curve
      if @curve
        @sel.add @curve.edges
      end
    end
  end
		
  def onCancel(flag, view)
  end
		
  def onRButtonDown(flags, x, y, view)
  end
		
  def onKeyDown(key, repeat, flags, view)
  end
				
  def draw(view)
    @drawn = false
    if( @ip0.valid? && @ip0.display? ) then
      @ip0.draw(view)
      @drawn = true
    end
  end		
end # of class
=end

#-----------------------------------------------------------------------------
if(not file_loaded?("rosetta.rb"))
  file_loaded("rosetta.rb")
  add_separator_to_menu("Tools")
  UI.menu("Tools").add_item("Connect Rosetta") { connect }
  UI.menu("Tools").add_item("Disconnect Rosetta") { disconnect }
  # UI.menu("Tools").add_item("Shutdown Rosetta") { shutdown }

  # connect now
  #connect
end
