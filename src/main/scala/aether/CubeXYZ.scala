package aether

object CubeXYZ {

  final case class Direction(dx: Double, dy: Double, dz: Double)

  final case class Point(x: Double, y: Double, z: Double)

  trait StraightLineLike

  final case class Ray(p1: Point, d: Direction) extends StraightLineLike

  final case class Segment(p1: Point, p2: Point) extends StraightLineLike

  final case class Wall(p1: CubeXY.Point2D, p2: CubeXY.Point2D)

  final case class Cuboid(p1: Point, p7: Point)

  final case class Cylinder(center: StraightLineLike, radius: Double)

  final case class Hierarchy(priority: Int, origin: Option[Int], causes: Set[Int], effects: Set[Int])

  val DummyHierarchy = Hierarchy(-1, Option.empty, Set.empty, Set.empty)

  final case class World(border: Cuboid, walls: Set[Wall], cylinders: Map[Int, (Cylinder, Hierarchy)])

  final case class XYPlane(z: Double)

  def intersection(wall: Wall, cylinder: Cylinder): Option[Point] = /* TODO Some(thing) */ None

  def intersection(cylinder1: Cylinder, cylinder2: Cylinder): Option[(Point, Point)] = /* TODO Some(thing) */ None

  def inject(world: World, cylinder: Cylinder): World = {
    val wallIntersection = world.walls.flatMap(intersection(_, cylinder))
    val cylinderIntersection = world.cylinders
      .mapValues(p => intersection(p._1, cylinder).map(ps => (p._2, ps._1, ps._2)))
      .collect {
        case (k, Some(v)) => k -> v
      }

    require(wallIntersection.isEmpty && cylinderIntersection.isEmpty,
      s"$wallIntersection.isEmpty && $cylinderIntersection.isEmpty; otherwise is not implemented yet")

    val id = 1 + (world.cylinders.keySet + 0).max

    world.copy(cylinders = world.cylinders.updated(id, (cylinder, DummyHierarchy)))
  }

  def intersection(wall: Wall, plane: XYPlane): CubeXY.Segment2D = {
    CubeXY.Segment2D(wall.p1, wall.p2)
  }

  def intersection(cylinder: Cylinder, plane: XYPlane): Option[CubeXY.Ellipse2D] = {
    val (intersect, p1, direction) = cylinder.center match {
      case Ray(p1, d) =>
        ((plane.z - p1.z) * d.dz >= 0,
          p1,
          d)
      case Segment(p1, p2) =>
        ((plane.z - p1.z) * (plane.z - p2.z) <= 0,
          p1,
          Direction(p2.x - p1.x, p2.y - p1.y, p2.z - p1.z))
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

      Some(CubeXY.Ellipse2D(
        CubeXY.Point2D(x, y), cylinder.radius / math.cos(t), cylinder.radius, math.atan2(direction.dy, direction.dx)))
    }
  }

  def intersection(world: World, plane: XYPlane): CubeXY.World2D = {
    val position = {
      import world.border._
      CubeXY.Rectangle(CubeXY.Point2D(p1.x, p1.y), CubeXY.Point2D(p7.x, p7.y))
    }

    val shapes = {
      val segments = world.walls.map(intersection(_, plane))
      val ellipses = world.cylinders.values.flatMap(p => intersection(p._1, plane))
      segments.toSeq ++ ellipses.toSeq
    }

    CubeXY.World2D(position, shapes.toSet)
  }
}
