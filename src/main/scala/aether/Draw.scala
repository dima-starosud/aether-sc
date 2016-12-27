package aether

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.geom.{AffineTransform, Line2D, Ellipse2D => AwtEllipse2D}
import java.awt.{BorderLayout, Dimension, Graphics, Graphics2D, Shape => AwtShape}
import javax.swing._

import aether.Model._
import aether.Operation._

object Draw {
  val Dim640x480 = new Dimension(640, 480)

  def animate(fps: Double = 25, size: Dimension = Dim640x480)(handler: SwingDrawingHandler): Unit = {
    val animator = new DrawingPanel(handler.draw)
    animator.setPreferredSize(size)

    val frame = new JFrame("Animation")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.getContentPane.add(animator)

    val am = frame.getRootPane.getActionMap
    val im = frame.getRootPane.getInputMap
    for {
      (stroke, action) <- handler.actions
    } {
      am.put(stroke, action)
      im.put(stroke, stroke)
    }

    frame.pack()
    frame.setVisible(true)

    val timer = new Timer((1000 / fps).toInt, _ => animator.repaint())
    timer.start()

    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        timer.stop()
      }
    })
  }

  class DrawingPanel(draw: (Dimension, Graphics2D) => Unit) extends JPanel(new BorderLayout) {
    protected override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      draw(getSize, g.asInstanceOf[Graphics2D])
    }
  }

  val asAwt: Shape2D => AwtShape = {
    case e: Ellipse2D =>
      AffineTransform.getRotateInstance(e.angle, e.c.getX, e.c.getY).createTransformedShape(
        new AwtEllipse2D.Double(e.c.getX - e.hr, e.c.getY - e.vr, 2 * e.hr, 2 * e.vr))
    case Segment2D(p1, p2) =>
      new Line2D.Double(p1.getX, p1.getY, p2.getX, p2.getY)
  }

  def affineTransformFor(rectangle: Rectangle, dimension: Dimension): AffineTransform = {
    val rw = dimension.width.toDouble / rectangle.width
    val rh = dimension.height.toDouble / rectangle.height
    new AffineTransform(
      rw,
      0, 0,
      -rh,
      -rectangle.llCorner.getX * rw,
      dimension.height.toDouble + rectangle.llCorner.getY * rh)
  }

  def draw2D(world: World2D, dimension: Dimension, graphics: Graphics2D): Unit = {
    graphics.transform(affineTransformFor(world.position, dimension))
    world.shapes.foreach(graphics draw asAwt(_))
  }
}
