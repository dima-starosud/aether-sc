package draw

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, Graphics, Graphics2D}
import java.io.Closeable
import javax.swing._

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

}
