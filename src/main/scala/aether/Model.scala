package aether

object Model {

  final case class Point2D(x: Double, y: Double)

  final case class Rectangle(p1: Point2D, p2: Point2D)

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

}
