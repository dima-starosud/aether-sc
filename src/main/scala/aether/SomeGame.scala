package aether

import javax.swing.SwingUtilities

object SomeGame {
  def main(args: Array[String]): Unit = {
    SwingUtilities.invokeLater(() => Draw.animate()(new InterestingGame()))
  }
}
