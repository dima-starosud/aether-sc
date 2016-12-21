package draw

import java.awt._
import java.awt.event.{WindowAdapter, WindowEvent}
import java.io.Closeable
import java.util.concurrent.atomic.AtomicReference
import javax.swing._

import scala.util.Random

object Draw {
  type Draw = (Dimension, Graphics2D) => Unit

  val Dim640x480 = new Dimension(640, 480)

  def animate(fps: Int = 25, size: Dimension = Dim640x480)(draw: Draw): Closeable = {
    val animator = new DrawingPanel(draw)
    animator.setPreferredSize(size)

    val frame = new JFrame("Animation")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.getContentPane.add(animator)
    frame.pack()
    frame.setVisible(true)

    val timer = new Timer(1000 / fps, _ => animator.repaint())
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

  // TODO remove this some time
  def main(args: Array[String]): Unit = {
    val closeable = new AtomicReference[Closeable]()
    SwingUtilities.invokeLater { () =>
      closeable.set(animate()((d, g) => g.drawLine(0, 0, Random.nextInt(d.width), Random.nextInt(d.height))))
    }
    Thread.sleep(10000)
    closeable.get().close()
  }
}
