package aether

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.geom.AffineTransform
import java.awt.{BorderLayout, Dimension, Graphics, Graphics2D}
import javax.swing._

import aether.Model._
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

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

  implicit final class RectangleOps(val rectangle: Rectangle) extends AnyVal {

    import rectangle._

    def width: Double = (point1.getX - point3.getX).abs

    def height: Double = (point1.getY - point3.getY).abs

    def llCorner: Vector2D = new Vector2D(point1.getX min point3.getX, point1.getY min point3.getY)
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
}
