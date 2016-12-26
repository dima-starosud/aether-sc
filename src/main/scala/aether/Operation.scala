package aether

import aether.Model._
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

object Operation {

  def intersection(wall: Wall, plane: XYPlane): Segment2D = {
    Segment2D(wall.p1, wall.p2)
  }

  def intersection(cylinder: Cylinder, plane: XYPlane): Option[Ellipse2D] = {
    val p1 = cylinder.center.p1
    val direction = cylinder.center.d

    if (!cylinder.center.intersects(plane.z)) {
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

  implicit final class StraightLine3DOps(val line: StraightLine3D) extends AnyVal {
    private def toEither: Either[Segment3D, Ray3D] = line match {
      case s: Segment3D => Left(s)
      case r: Ray3D => Right(r)
    }

    def d: Direction3D = toEither.fold(s => (s.p2 subtract s.p1).toDirection, _.d)

    def p1: Vector3D = toEither.fold(_.p1, _.p1)

    private def p2: Option[Vector3D] = toEither.left.toOption.map(_.p2)

    def split(z: Double): Option[(StraightLine3D, StraightLine3D)] = {
      if (!intersects(z)) {
        None
      } else {
        val cz = (z - p1.getZ) / d.dz
        val p2 = new Vector3D(cz * d.dx + p1.getX, cz * d.dy + p1.getY, z)
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
        r => r.copy(d = (r.d.toVector add vector).toDirection))
    }

    def intersects(z: Double): Boolean = {
      p1.getZ <= z && p2.forall(_.getZ >= z)
    }
  }

  implicit final class CylinderOps(val cylinder: Cylinder) extends AnyVal {
    def split(z: Double): Option[(Cylinder, Cylinder)] = {
      cylinder.center.split(z).map {
        case (c1, c2) => (cylinder.copy(center = c1), cylinder.copy(center = c2))
      }
    }
  }

  implicit val cylinderOrdering: Ordering[Cylinder] = Ordering.by(_.center.p1.getZ)
}
