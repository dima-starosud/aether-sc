package aether

import java.awt.geom.{Line2D, Ellipse2D, AffineTransform}
import java.awt.{Graphics2D, Dimension}

object CubeXY {

  final case class Point(x: Double, y: Double)

  final case class Rectangle(p1: Point, p2: Point)

  sealed trait Shape

  final case class Segment(p1: Point, p2: Point) extends Shape

  final case class Ellipse(p1: Point, p2: Point) extends Shape

  final case class World(position: Rectangle, shapes: Set[Shape])

  implicit final class AsAwtShape(val shape: Shape) extends AnyVal {

    import java.awt.{Shape => AwtShape}

    def asAwt: AwtShape = shape match {
      case Ellipse(p1, p2) =>
        val points =
          for {
            x <- Seq(p1.x, p2.x)
            y <- Seq(p1.y, p2.y)
          } yield Point(x, y)
        // points.minBy(???)
        new Ellipse2D.Double()
      case Segment(p1, p2) =>
        new Line2D.Double(p1.x, p1.y, p2.x, p2.y)
    }
  }

  def draw(/*world: World,*/ dimension: Dimension, graphics: Graphics2D): Unit = {
    graphics.transform(new AffineTransform(1, 0, 0, -1, 0, dimension.height))
    graphics.draw(new Ellipse2D.Double(20, 20, dimension.width - 40, dimension.height - 40))
    graphics.drawLine(20, 20, dimension.width - 20, dimension.height - 20)
  }
}
