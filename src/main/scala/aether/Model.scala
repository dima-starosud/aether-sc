package aether

import org.apache.commons.math3.geometry.euclidean.threed.{Line, Plane, Vector3D}
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

import scala.collection.SortedSet

object Model {

  final case class Rectangle(p1: Vector2D, p2: Vector2D)

  sealed trait Shape2D

  final case class Segment2D(p1: Vector2D, p2: Vector2D) extends Shape2D

  final case class Ellipse2D(c: Vector2D, hr: Double, vr: Double, angle: Double) extends Shape2D

  final case class World2D(position: Rectangle, shapes: Set[Shape2D])

  sealed abstract class StraightLine3D(val line: Line, val region: Set[Plane])

  final case class Ray3D(p1: Vector3D, d: Vector3D)
    extends StraightLine3D(new Line(p1, p1 add d, 0), Set(new Plane(p1, d, 0)))

  final case class Segment3D(p1: Vector3D, p2: Vector3D)
    extends StraightLine3D(new Line(p1, p2, 0), Set(new Plane(p2 subtract p1, 0), new Plane(p1 subtract p2, 0)))

  final case class Wall(p1: Vector2D, p2: Vector2D)

  final case class Cuboid(p1: Vector3D, p7: Vector3D)

  final case class Cylinder(center: StraightLine3D, radius: Double)

  final case class World3D(border: Cuboid, walls: Set[Wall], cylinders: Map[Int, SortedSet[Cylinder]])

  final case class XYPlane(z: Double)

}
