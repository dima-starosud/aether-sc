package aether

import java.awt.event.{InputEvent, KeyEvent}
import java.awt.{Dimension, Graphics2D}

import aether.Model._
import aether.Operation._
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

import scala.collection.SortedSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

class World3DAgent {
  private var world = World3D(
    Cuboid(new Vector3D(200, 200, 0), new Vector3D(600, 600, 1e9d)),
    Set.empty,
    Map(1 -> SortedSet(Cylinder(Ray3D(new Vector3D(400, 400, 0), Direction3D(0, 0, 1)), 50)))
  )

  def get(): World3D = synchronized(world)

  def event(cylinderId: Int, z: Double, event: Vector3D) = synchronized {
    var cylinders = world.cylinders(cylinderId)
    cylinders = cylinders to horizontalInfiniteCylinder(z)
    val Some((c1, c2)) = cylinders.last.split(1)
    cylinders = cylinders.dropRight(1) + c1 + c2.copy(center = c2.center amend event)
    world = world.copy(cylinders = world.cylinders.updated(cylinderId, cylinders))
  }
}

final class InterestingGame extends AwtGame {
  private val startMillis = System.currentTimeMillis()

  private val worldAgent = new World3DAgent

  private def currentTime: Double = System.currentTimeMillis() - startMillis

  override def draw(dimension: Dimension, graphics: Graphics2D): Unit = {
    val w2d = Operation.intersection(worldAgent.get(), XYPlane(currentTime))
    Draw.draw(w2d, dimension, graphics)
  }

  private val action = Map(KeyEvent.KEY_PRESSED -> 1d, KeyEvent.KEY_RELEASED -> -1d)
  private val value = Map(
    KeyEvent.VK_UP -> new Vector3D(0, 0.1, 1),
    KeyEvent.VK_DOWN -> new Vector3D(0, -0.1, 1),
    KeyEvent.VK_LEFT -> new Vector3D(-0.1, 0, 1),
    KeyEvent.VK_RIGHT -> new Vector3D(0.1, 0, 1)
  )

  override def event(event: InputEvent): Unit = {
    for {
      e <- Try(event.asInstanceOf[KeyEvent]).toOption
      a <- action.get(e.getID)
      v <- value.get(e.getKeyCode)
    } {
      Future(println(Try(worldAgent.event(1, currentTime, v scalarMultiply a))))
    }
  }
}
