package aether

object Model {

  final case class Point2D(x: Double, y: Double)

  final case class Rectangle(p1: Point2D, p2: Point2D) {
    lazy val width: Double = (p1.x - p2.x).abs
    lazy val height: Double = (p1.y - p2.y).abs
    lazy val llCorner: Point2D = Point2D(p1.x min p2.x, p1.y min p2.y)
  }

  sealed trait Shape2D

  final case class Segment2D(p1: Point2D, p2: Point2D) extends Shape2D

  final case class Ellipse2D(c: Point2D, hr: Double, vr: Double, angle: Double) extends Shape2D

  final case class World2D(position: Rectangle, shapes: Set[Shape2D])

  final case class Direction3D(dx: Double, dy: Double, dz: Double)

  final case class Point3D(x: Double, y: Double, z: Double)

  trait StraightLine3D

  final case class Ray3D(p1: Point3D, d: Direction3D) extends StraightLine3D

  final case class Segment3D(p1: Point3D, p2: Point3D) extends StraightLine3D

  final case class Wall(p1: Point2D, p2: Point2D)

  final case class Cuboid(p1: Point3D, p7: Point3D)

  final case class Cylinder(center: StraightLine3D, radius: Double)

  final case class World3D(border: Cuboid, walls: Set[Wall], cylinders: Set[Cylinder])

  final case class XYPlane(z: Double)

  def intersection(wall: Wall, cylinder: Cylinder): Option[Point3D] = /* TODO Some(thing) */ None

  def intersection(cylinder1: Cylinder, cylinder2: Cylinder): Option[(Point3D, Point3D)] = /* TODO Some(thing) */ None

  def inject(world: World3D, cylinder: Cylinder): World3D = {
    val wallIntersection = world.walls.flatMap(intersection(_, cylinder))
    val cylinderIntersection = world.cylinders.flatMap(intersection(_, cylinder))

    require(wallIntersection.isEmpty && cylinderIntersection.isEmpty,
      s"$wallIntersection.isEmpty && $cylinderIntersection.isEmpty; otherwise is not implemented yet")

    world.copy(cylinders = world.cylinders + cylinder)
  }

  def intersection(wall: Wall, plane: XYPlane): Segment2D = {
    Segment2D(wall.p1, wall.p2)
  }

  def intersection(cylinder: Cylinder, plane: XYPlane): Option[Ellipse2D] = {
    val (intersect, p1, direction) = cylinder.center match {
      case Ray3D(p1, d) =>
        ((plane.z - p1.z) * d.dz >= 0,
          p1,
          d)
      case Segment3D(p1, p2) =>
        ((plane.z - p1.z) * (plane.z - p2.z) <= 0,
          p1,
          Direction3D(p2.x - p1.x, p2.y - p1.y, p2.z - p1.z))
    }
    if (!intersect) {
      None
    } else {
      val rz = (plane.z - p1.z) / direction.dz
      val x = rz * direction.dx + p1.x
      val y = rz * direction.dy + p1.y

      val t = {
        import direction._
        val dn = math.sqrt(Seq(dx, dy, dz).map(math.pow(_, 2)).sum)
        math.asin(dz.abs / dn)
      }

      Some(Ellipse2D(
        Point2D(x, y), cylinder.radius / math.cos(t), cylinder.radius, math.atan2(direction.dy, direction.dx)))
    }
  }

  def intersection(world: World3D, plane: XYPlane): World2D = {
    val position = {
      import world.border._
      Rectangle(Point2D(p1.x, p1.y), Point2D(p7.x, p7.y))
    }

    val shapes = {
      val segments = world.walls.map(intersection(_, plane))
      val ellipses = world.cylinders.flatMap(intersection(_, plane))
      segments.toSeq ++ ellipses.toSeq
    }

    World2D(position, shapes.toSet)
  }
}
