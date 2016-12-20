package draw

import java.awt._
import java.awt.event.{WindowAdapter, WindowEvent}
import javax.swing._

import scala.util.Random

object Draw {
  def createAndShowGUI(): Unit = {
    val animator = new AnimationPanel
    animator.setPreferredSize(new Dimension(640, 480))

    val frame = new JFrame("Animation")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.getContentPane.add(animator)
    frame.pack()
    frame.setVisible(true)

    val timer = new Timer(100, _ => animator.repaint())
    timer.start()

    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        timer.stop()
      }
    })
  }

  def main(args: Array[String]): Unit = {
    SwingUtilities.invokeLater(() => createAndShowGUI())
  }
}

class AnimationPanel extends JPanel(new BorderLayout) {
  protected override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    g.asInstanceOf[Graphics2D].drawLine(0, 0, Random.nextInt(getWidth), Random.nextInt(getHeight))
  }
}
