package aether

import java.awt.geom.{AffineTransform, Ellipse2D, Line2D}
import java.awt.{Dimension, Graphics2D, Shape => AwtShape}

object CubeXY {

  final case class Point(x: Double, y: Double)

  trait RectangleLike {
    def p1: Point

    def p2: Point

    lazy val width: Double = (p1.x - p2.x).abs

    lazy val height: Double = (p1.y - p2.y).abs

    lazy val llCorner: Point = Point(p1.x min p2.x, p1.y min p2.y)
  }

  final case class Rectangle(p1: Point, p2: Point) extends RectangleLike

  sealed trait Shape

  final case class Segment(p1: Point, p2: Point) extends Shape with RectangleLike

  final case class Ellipse(p1: Point, p2: Point) extends Shape with RectangleLike

  final case class World(position: Rectangle, shapes: Set[Shape])

  val asAwt: Shape => AwtShape = {
    case e@Ellipse(_, _) =>
      new Ellipse2D.Double(e.llCorner.x, e.llCorner.y, e.width, e.height)
    case Segment(p1, p2) =>
      new Line2D.Double(p1.x, p1.y, p2.x, p2.y)
  }

  def affineTransformFor(rectangle: Rectangle, dimension: Dimension): AffineTransform = {
    val rw = dimension.width.toDouble / rectangle.width
    val rh = dimension.height.toDouble / rectangle.height
    new AffineTransform(
      rw,
      0, 0,
      -rh,
      -rectangle.llCorner.x * rw,
      dimension.height.toDouble + rectangle.llCorner.y * rh)
  }

  def draw(world: World, dimension: Dimension, graphics: Graphics2D): Unit = {
    graphics.transform(affineTransformFor(world.position, dimension))
    world.shapes.foreach(graphics draw asAwt(_))
  }
}
