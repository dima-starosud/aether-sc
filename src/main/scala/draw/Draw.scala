package draw

import javax.swing._

object Draw {
  def createAndShowGUI(): Unit = {
    //Create and set up the window.
    val frame = new JFrame("HelloWorldSwing")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    //Add the ubiquitous "Hello World" label.
    val label = new JLabel("Hello World")
    frame.getContentPane.add(label)

    //Display the window.
    frame.pack()
    frame.setVisible(true)
  }

  def main(args: Array[String]): Unit = {
    SwingUtilities.invokeLater(new Runnable {
      override def run(): Unit = createAndShowGUI()
    })
  }
}
