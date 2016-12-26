package aether

import java.awt.event.{ActionEvent, KeyEvent}
import java.awt.{Dimension, Graphics2D}
import javax.swing.{AbstractAction, Action, KeyStroke}

import aether.Model._
import aether.Operation._
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

import scala.collection.SortedSet

class World3DAgent {
  private var world = World3D(
    Cuboid(new Vector3D(200, 200, 0), new Vector3D(600, 600, 1e9d)),
    Set.empty,
    Map(1 -> SortedSet(Cylinder(Ray3D(new Vector3D(400, 400, 0), Direction3D(0, 0, 1)), 50)))
  )

  def get(): World3D = synchronized(world)

  def event(cylinderId: Int, z: Double, event: Vector3D): Unit = synchronized {
    var cylinders = world.cylinders(cylinderId)
    cylinders = cylinders to horizontalInfiniteCylinder(z)
    val Some((c1, c2)) = cylinders.last.split(z)
    cylinders = cylinders.dropRight(1) + c1 + c2.copy(center = c2.center amend event)
    world = world.copy(cylinders = world.cylinders.updated(cylinderId, cylinders))
  }
}

final class InterestingGame extends SwingDrawingHandler {
  private val startMillis = System.nanoTime()

  private val worldAgent = new World3DAgent

  private def currentTimeMillis: Double = (System.nanoTime() - startMillis).toDouble / 1e6d

  override def draw(dimension: Dimension, graphics: Graphics2D): Unit = {
    val w2d = Operation.intersection(worldAgent.get(), XYPlane(currentTimeMillis))
    Draw.draw(w2d, dimension, graphics)
  }

  override lazy val actions: Map[KeyStroke, Action] = {
    val value = Map(
      KeyEvent.VK_UP -> new Vector3D(0, 0.05, 0.0),
      KeyEvent.VK_DOWN -> new Vector3D(0, -0.05, 0.0),
      KeyEvent.VK_LEFT -> new Vector3D(-0.05, 0, 0.0),
      KeyEvent.VK_RIGHT -> new Vector3D(0.05, 0, 0.0)
    )

    value.flatMap { case (key, vector) =>
      val pressedStroke = KeyStroke.getKeyStroke(key, 0, false)
      val releasedStroke = KeyStroke.getKeyStroke(key, 0, true)
      val pressedAction = new AbstractAction() {
        override def actionPerformed(e: ActionEvent): Unit = {
          setEnabled(false)
          worldAgent.event(1, currentTimeMillis, vector)
        }
      }
      val releasedAction = new AbstractAction() {
        override def actionPerformed(e: ActionEvent): Unit = {
          pressedAction.setEnabled(true)
          worldAgent.event(1, currentTimeMillis, vector.negate())
        }
      }

      Seq(pressedStroke -> pressedAction, releasedStroke -> releasedAction)
    }
  }
}
