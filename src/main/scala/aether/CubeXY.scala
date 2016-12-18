package aether

object CubeXY {

  sealed trait Shape

  final case class Point(x: Double, y: Double) extends Shape

  final case class Segment(p1: Point, p2: Point) extends Shape

  final case class Ellipse(p1: Point, p2: Point) extends Shape

  final case class World(shapes: Set[Shape])

}
