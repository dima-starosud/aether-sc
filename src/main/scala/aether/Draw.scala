package aether

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, Graphics, Graphics2D}
import javax.swing._

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

}
