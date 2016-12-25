package aether

import java.awt.event.InputEvent
import java.awt.{Dimension, Graphics2D}

trait AwtGame {
  def draw(dimension: Dimension, graphics: Graphics2D): Unit

  def event(event: InputEvent): Unit
}
