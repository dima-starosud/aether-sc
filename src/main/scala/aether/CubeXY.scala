package aether

import java.awt.geom.{AffineTransform, Ellipse2D => AwtEllipse2D, Line2D}
import java.awt.{Dimension, Graphics2D, Shape => AwtShape}

object CubeXY {

  final case class Point2D(x: Double, y: Double)

  final case class Rectangle(p1: Point2D, p2: Point2D) {
    lazy val width: Double = (p1.x - p2.x).abs
    lazy val height: Double = (p1.y - p2.y).abs
    lazy val llCorner: Point2D = Point2D(p1.x min p2.x, p1.y min p2.y)
  }

  sealed trait Shape2D

  final case class Segment2D(p1: Point2D, p2: Point2D) extends Shape2D

  final case class Ellipse2D(c: Point2D, hr: Double, vr: Double, angle: Double) extends Shape2D

  final case class World2D(position: Rectangle, shapes: Set[Shape2D])

  val asAwt: Shape2D => AwtShape = {
    case e: Ellipse2D =>
      AffineTransform.getRotateInstance(e.angle, e.c.x, e.c.y).createTransformedShape(
        new AwtEllipse2D.Double(e.c.x - e.hr, e.c.y - e.vr, 2 * e.hr, 2 * e.vr))
    case Segment2D(p1, p2) =>
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

  def draw(world: World2D, dimension: Dimension, graphics: Graphics2D): Unit = {
    graphics.transform(affineTransformFor(world.position, dimension))
    world.shapes.foreach(graphics draw asAwt(_))
  }
}
