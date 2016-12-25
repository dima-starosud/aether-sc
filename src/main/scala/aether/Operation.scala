package aether

import aether.Model._
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

object Operation {

  def intersection(wall: Wall, plane: XYPlane): Segment2D = {
    Segment2D(wall.p1, wall.p2)
  }

  def intersection(cylinder: Cylinder, plane: XYPlane): Option[Ellipse2D] = {
    val (intersect, p1, direction) = cylinder.center match {
      case Ray3D(p1, d) =>
        ((plane.z - p1.getZ) * d.dz >= 0,
          p1,
          d)
      case Segment3D(p1, p2) =>
        ((plane.z - p1.getZ) * (plane.z - p2.getZ) <= 0,
          p1,
          Direction3D(p2.getX - p1.getX, p2.getY - p1.getY, p2.getZ - p1.getZ))
    }
    if (!intersect) {
      None
    } else {
      val rz = (plane.z - p1.getZ) / direction.dz
      val x = rz * direction.dx + p1.getX
      val y = rz * direction.dy + p1.getY

      val t = {
        import direction._
        val dn = math.sqrt(Seq(dx, dy, dz).map(math.pow(_, 2)).sum)
        dz.abs / dn
      }

      Some(Ellipse2D(
        new Vector2D(x, y), cylinder.radius / t, cylinder.radius, math.atan2(direction.dy, direction.dx)))
    }
  }

  def intersection(world: World3D, plane: XYPlane): World2D = {
    val position = {
      import world.border._
      Rectangle(new Vector2D(p1.getX, p1.getY), new Vector2D(p7.getX, p7.getY))
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

    def width: Double = (p1.getX - p2.getX).abs

    def height: Double = (p1.getY - p2.getY).abs

    def llCorner: Vector2D = new Vector2D(p1.getX min p2.getX, p1.getY min p2.getY)
  }

}
