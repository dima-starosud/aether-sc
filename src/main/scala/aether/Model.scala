package aether

import org.apache.commons.math3.geometry.euclidean.threed.Vector3D
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

object Model {

  final case class Rectangle(p1: Vector2D, p2: Vector2D)

  sealed trait Shape2D

  final case class Segment2D(p1: Vector2D, p2: Vector2D) extends Shape2D

  final case class Ellipse2D(c: Vector2D, hr: Double, vr: Double, angle: Double) extends Shape2D

  final case class World2D(position: Rectangle, shapes: Set[Shape2D])

  final case class Direction3D(dx: Double, dy: Double, dz: Double)

  trait StraightLine3D

  final case class Ray3D(p1: Vector3D, d: Direction3D) extends StraightLine3D

  final case class Segment3D(p1: Vector3D, p2: Vector3D) extends StraightLine3D

  final case class Wall(p1: Vector2D, p2: Vector2D)

  final case class Cuboid(p1: Vector3D, p7: Vector3D)

  final case class Cylinder(center: StraightLine3D, radius: Double)

  final case class World3D(border: Cuboid, walls: Set[Wall], cylinders: Set[Cylinder])

  final case class XYPlane(z: Double)

}
