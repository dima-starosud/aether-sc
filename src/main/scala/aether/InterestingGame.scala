package aether

import java.awt.event.{InputEvent, KeyEvent}
import java.awt.{Dimension, Graphics2D}

import aether.Model._
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

final class InterestingGame extends AwtGame {
  private val startMillis = System.currentTimeMillis()

  private val world = /* TODO this has to be managed separately */ World3D(
    Cuboid(new Vector3D(200, 200, 0), new Vector3D(600, 600, 1e9d)),
    Set.empty,
    Set(Cylinder(Ray3D(new Vector3D(400, 400, 0), Direction3D(0.01, 0, 1)), 50))
  )

  override def draw(dimension: Dimension, graphics: Graphics2D): Unit = {
    val w2d = Operation.intersection(synchronized(this.world), XYPlane(System.currentTimeMillis() - startMillis))
    Draw.draw(w2d, dimension, graphics)
  }

  override def event(event: InputEvent): Unit = synchronized {
    event match {
      case key: KeyEvent =>
        key.getID match {
          case KeyEvent.KEY_PRESSED =>
          case _ =>
        }
        key.getKeyCode match {
          case KeyEvent.VK_UP =>
          case KeyEvent.VK_DOWN =>
          case KeyEvent.VK_LEFT =>
          case KeyEvent.VK_RIGHT =>
          case _ =>
        }
      case _ =>
    }
  }
}
