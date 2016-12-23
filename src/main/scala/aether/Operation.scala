package aether

import aether.Model._

object Operation {

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

  implicit final class RectangleOps(val rectangle: Rectangle) extends AnyVal {

    import rectangle._

    def width: Double = (p1.x - p2.x).abs

    def height: Double = (p1.y - p2.y).abs

    def llCorner: Point2D = Point2D(p1.x min p2.x, p1.y min p2.y)
  }

}
