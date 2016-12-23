package aether

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.geom.{AffineTransform, Ellipse2D => AwtEllipse2D, Line2D}
import java.awt.{BorderLayout, Dimension, Graphics, Graphics2D, Shape => AwtShape}
import java.io.Closeable
import javax.swing._

import aether.Model._
import aether.Operation._

object Draw {
  type Draw = (Dimension, Graphics2D) => Unit

  val Dim640x480 = new Dimension(640, 480)

  def animate(fps: Double = 25, size: Dimension = Dim640x480)(draw: Draw): Closeable = {
    val animator = new DrawingPanel(draw)
    animator.setPreferredSize(size)

    val frame = new JFrame("Animation")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.getContentPane.add(animator)
    frame.pack()
    frame.setVisible(true)

    val timer = new Timer((1000 / fps).toInt, _ => animator.repaint())
    timer.start()

    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        timer.stop()
      }
    })

    () => {
      timer.stop()
      frame.dispose()
    }
  }

  class DrawingPanel(draw: Draw) extends JPanel(new BorderLayout) {
    protected override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      draw(getSize, g.asInstanceOf[Graphics2D])
    }
  }

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
