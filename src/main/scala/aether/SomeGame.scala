package aether

import java.awt._
import java.awt.event._
import javax.swing._

object SomeGame {
  val Dim640x480 = new Dimension(640, 480)

  def main(args: Array[String]): Unit = {
    /**
      * TODO this is part of [[aether.Draw.animate]]
      */
    val game = new InterestingGame()
    val animator = new DrawingPanel(game.draw)
    animator.setPreferredSize(Dim640x480)

    val frame = new JFrame("Animation")
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.getContentPane.add(animator)
    frame.pack()
    frame.setVisible(true)

    val timer = new Timer(20, _ => animator.repaint())
    timer.start()

    frame.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = {
        timer.stop()
      }
    })
    frame.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = game.event(e)

      override def keyPressed(e: KeyEvent): Unit = game.event(e)

      override def keyReleased(e: KeyEvent): Unit = game.event(e)
    })
  }

  class DrawingPanel(draw: (Dimension, Graphics2D) => Unit) extends JPanel(new BorderLayout()) {
    protected override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      draw(getSize, g.asInstanceOf[Graphics2D])
    }
  }

}
