package aether

import java.awt.{Dimension, Graphics2D}
import javax.swing.{Action, KeyStroke}

trait SwingDrawingHandler {
  def draw(dimension: Dimension, graphics: Graphics2D): Unit

  def actions: Map[KeyStroke, Action]
}
