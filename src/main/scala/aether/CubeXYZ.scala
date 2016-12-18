package aether

import ungeneric.Constrained

object CubeXYZ {

  final case class Direction(dx: Double, dy: Double, dz: Double)

  sealed trait Shape

  sealed trait D0 extends Shape

  final case class Point(x: Double, y: Double, z: Double) extends D0 {
    def +(d: Direction) = {
      import d._
      Point(x + dx, y + dy, z + dz)
    }
  }

  sealed trait D1 extends Shape {
    def p1: Point

    def p2: Point

    require(p1 != p2, s"$p1 != $p2")
  }

  final case class Segment(p1: Point, p2: Point) extends D1

  final case class Ray(p1: Point, d: Direction) extends D1 {
    override def p2: Point = p1 + d
  }

  final case class Line(p1: Point, p2: Point) extends D1

  sealed trait D2 extends Shape

  val Vertical = Constrained[D1 => ?, D1] { d1 =>
    import d1._
    require(p1.z == p2.z, s"$p1.z == $p2.z")
    d1
  }

  final case class Wall(center: Vertical.Value, radius: Double) extends D2

  sealed trait D3 extends Shape

  final case class Cuboid(p1: Point, p7: Point) extends D3

  final case class Cylinder(center: D1, radius: Double) extends D3

  final case class Hierarchy(priority: Int, origin: Option[Int], causes: Set[Int], effects: Set[Int])

  final case class World(border: Cuboid, walls: Map[Int, Wall], cylinders: Map[Int, (Cylinder, Hierarchy)])

  def intersection(head: Wall, cylinder: Cylinder): Option[Point] = ???

  def intersection(cylinder1: Cylinder, cylinder2: Cylinder): Option[(Point, Point)] = ???

  def inject(world: World, cylinder: Cylinder): World = {
    val p1 = intersection(world.walls.values.head, cylinder)
    val p2 = intersection(cylinder, cylinder)
    ???
  }

  def sliceZ(world: World, value: Double): CubeXY.World = {
    ???
  }

  def limitZ(world: World, min: Double, max: Double): World = {
    ???
  }

  def shiftZ(world: World, value: Double): World = {
    ???
  }
}
