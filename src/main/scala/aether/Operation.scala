package aether

import aether.Model._
import org.apache.commons.math3.geometry.euclidean.threed._
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

object Operation {

  def intersection(wall: Wall, plane: XYPlane): Segment2D = {
    Segment2D(wall.p1, wall.p2)
  }

  def intersection(cylinder: Cylinder, xyPlane: XYPlane): Option[Ellipse2D] = {
    val line = cylinder.center.line
    val plane = xyPlane.plane
    val is = plane intersection line

    val intersects = cylinder.center.region.forall(_.getOffset(is) >= 0)

    if (!intersects) {
      None
    } else {
      val l = cylinder.radius / (plane.getNormal.normalize() dotProduct line.getDirection.normalize())

      val hm = new Vector2D(is.getX, is.getY)

      Some(Ellipse2D(
        hm, l, cylinder.radius, math.atan2(line.getDirection.getY, line.getDirection.getX)))
    }
  }

  def intersection(world: World3D, plane: XYPlane): World2D = {
    val position = {
      import world.border._
      Rectangle(new Vector2D(p1.getX, p1.getY), new Vector2D(p7.getX, p7.getY))
    }

    val shapes = {
      val segments = world.walls.map(intersection(_, plane))
      val ellipses = world.cylinders.values.flatten.flatMap(intersection(_, plane))
      segments.toSeq ++ ellipses.toSeq
    }

    World2D(position, shapes.toSet)
  }

  def horizontalInfiniteCylinder(z: Double): Cylinder = {
    val p1 = new Vector3D(Double.NaN, Double.NaN, z)
    Cylinder(Segment3D(p1, p1), Double.NaN)
  }

  implicit final class RectangleOps(val rectangle: Rectangle) extends AnyVal {

    import rectangle._

    def width: Double = (p1.getX - p2.getX).abs

    def height: Double = (p1.getY - p2.getY).abs

    def llCorner: Vector2D = new Vector2D(p1.getX min p2.getX, p1.getY min p2.getY)
  }

  implicit final class Vector3DOps(val vector: Vector3D) extends AnyVal {

    import vector._

    def toDirection: Direction3D = Direction3D(getX, getY, getZ)
  }

  implicit final class Direction3DOps(val direction: Direction3D) extends AnyVal {

    import direction._

    def toVector: Vector3D = new Vector3D(dx, dy, dz)
  }

  implicit final class XYPlaneOps(val self: XYPlane) extends AnyVal {
    def plane: Plane = {
      val planeNormal = new Vector3D(0, 0, self.z)
      new Plane(planeNormal, planeNormal, 0)
    }
  }

  implicit final class StraightLine3DOps(val self: StraightLine3D) extends AnyVal {

    import self._

    private def toEither: Either[Segment3D, Ray3D] = self match {
      case s: Segment3D => Left(s)
      case r: Ray3D => Right(r)
    }

    // TODO try kind of minP here
    def p1: Vector3D = toEither.fold(_.p1, _.p1)

    def minZ: Double = region.map(_.intersection(line).getZ).min

    def split(z: Double): Option[(StraightLine3D, StraightLine3D)] = {
      val plane = XYPlane(z).plane
      val p2 = plane intersection line
      val intersects = region.forall(_.getOffset(p2) >= 0)

      if (!intersects) {
        None
      } else {
        val line1 = Segment3D(p1, p2)
        val line2 = toEither.fold(_.copy(p1 = p2), _.copy(p1 = p2))
        Some((line1, line2))
      }
    }

    def amend(vector: Vector3D): StraightLine3D = {
      toEither.fold(
        s => {
          println(s"Shit: $s!!!")
          ???
        },
        r => r.copy(d = r.d add vector))
    }
  }

  implicit final class CylinderOps(val cylinder: Cylinder) extends AnyVal {
    def split(z: Double): Option[(Cylinder, Cylinder)] = {
      cylinder.center.split(z).map {
        case (c1, c2) => (cylinder.copy(center = c1), cylinder.copy(center = c2))
      }
    }
  }

  implicit val cylinderOrdering: Ordering[Cylinder] = Ordering.by(_.center.minZ)
}
