package aether

import java.awt.event.{ActionEvent, KeyEvent}
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Dimension, Graphics2D, Shape}
import javax.swing.{AbstractAction, Action, KeyStroke, SwingUtilities}

import aether.Model.{Cuboid, Rectangle}
import aether.PingPongAgent._
import org.apache.commons.math3.geometry.euclidean.threed.{Line, Plane, Vector3D}
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D

import scala.collection.SortedMap

object PingPong {
  def main(args: Array[String]): Unit = {
    SwingUtilities.invokeLater(() => Draw.animate()(new PingPongHandler()))
  }
}

object PingPongHandler {

}

final class PingPongHandler extends SwingDrawingHandler {
  private val StartMillis = System.currentTimeMillis()
  private val state = new PingPongAgent

  override def draw(dimension: Dimension, graphics: Graphics2D): Unit = {
    val s = state.at(System.currentTimeMillis() - StartMillis)
    graphics.setTransform(Draw.affineTransformFor(s.position, dimension))
    s.objects.foreach(graphics.fill)
  }

  override def actions: Map[KeyStroke, Action] = {
    val actions = Seq(
      (KeyEvent.VK_UP, false, RightRacket, RacketUp),
      (KeyEvent.VK_UP, true, RightRacket, RacketDown),
      (KeyEvent.VK_DOWN, false, RightRacket, RacketDown),
      (KeyEvent.VK_DOWN, true, RightRacket, RacketUp),
      (KeyEvent.VK_W, false, LeftRacket, RacketUp),
      (KeyEvent.VK_W, true, LeftRacket, RacketDown),
      (KeyEvent.VK_S, false, LeftRacket, RacketDown),
      (KeyEvent.VK_S, true, LeftRacket, RacketUp)
    )

    var result = Map.empty[KeyStroke, Action]
    var previous: Action = null
    for ((key, released, racket, racketAction) <- actions) {
      val enables = Some(previous).filter(_ => released)
      val stroke = KeyStroke.getKeyStroke(key, 0, released)
      val action = new AwtAction(enables, e => state.action(e.getWhen - StartMillis, racket, racketAction))
      result += (stroke -> action)
      previous = action
    }

    result
  }
}

object PingPongAgent {

  sealed trait Racket

  case object LeftRacket extends Racket

  case object RightRacket extends Racket

  sealed trait RacketAction

  case object RacketUp extends RacketAction

  case object RacketDown extends RacketAction

  final case class State2D(position: Rectangle, objects: Seq[Shape])

  // TODO simple stack-like structure should be enough for this
  type Staged[T] = SortedMap[Double, T]

  def xyPlane(z: Double): Plane = {
    val point = new Vector3D(0, 0, z)
    val normal = new Vector3D(0, 0, 1)
    new Plane(point, normal, 0)
  }

  def yzPlane(x: Double): Plane = {
    val point = new Vector3D(x, 0, 0)
    val normal = new Vector3D(1, 0, 0)
    new Plane(point, normal, 0)
  }

  def xzPlane(y: Double): Plane = {
    val point = new Vector3D(0, y, 0)
    val normal = new Vector3D(0, 1, 0)
    new Plane(point, normal, 0)
  }

  sealed trait AwtShape {
    def xy(z: Double): Shape
  }

  implicit final class StagedAwtShape[T <: AwtShape](val staged: Staged[T]) extends AwtShape {
    override def xy(z: Double): Shape = (staged to z).last._2.xy(z)
  }

  final case class Cylinder(point1: Vector3D, direction: Vector3D, radius: Double) extends AwtShape {
    val line = new Line(point1, point1 add direction, 0)
    val diameter: Double = 2 * radius

    override def xy(z: Double): Shape = {
      val center = xyPlane(z).intersection(line)
      new Ellipse2D.Double(center.getX - radius, center.getY - radius, diameter, diameter)
    }

    // def move(z: Double): Cylinder = ???

    // def impulse(direction: Vector3D): Cylinder = ???
  }

  final case class Parallelepiped(point1: Vector3D, point3: Vector3D, direction: Vector3D) extends AwtShape {
    val line1 = new Line(point1, point1 add direction, 0)
    val line3 = new Line(point3, point3 add direction, 0)

    override def xy(z: Double): Shape = {
      val parallelepiped = move(z)
      val (x1, y1, x3, y3) = (
        parallelepiped.point1.getX, parallelepiped.point1.getY,
        parallelepiped.point3.getX, parallelepiped.point3.getY)
      new Rectangle2D.Double(x1 min x3, y1 min y3, (x3 - x1).abs, (y3 - y1).abs)
    }

    def move(z: Double): Parallelepiped = {
      val plane = xyPlane(z)
      copy(point1 = plane intersection line1, point3 = plane intersection line3)
    }

    def impulse(impulse: Vector3D): Parallelepiped = copy(direction = direction add impulse)
  }

  final case class State3D(position: Cuboid, ball: Staged[Cylinder], rackets: Map[Racket, Staged[Parallelepiped]])

  implicit final class Vector3DOps(val vector: Vector3D) extends AnyVal {

    import vector._

    def xy: Vector2D = new Vector2D(getX, getY)
  }

  def intersection(state: State3D, z: Double): State2D = {
    import state._
    State2D(Rectangle(position.p1.xy, position.p7.xy),
      (ball +: rackets.values.toSeq).map(_ xy z)
    )
  }

  val Epsilon = 1e-9

  def touch(cuboid: Cuboid, parallelepiped: Parallelepiped, z: Double): Option[Double] = {
    val touchPoints =
      for {
        y <- Seq(cuboid.p1.getY, cuboid.p7.getY)
        plane = xzPlane(y)
        line <- Seq(parallelepiped.line1, parallelepiped.line3)
        intersection <- Option(plane intersection line)
        if intersection.getZ >= z
        if Seq(intersection subtract cuboid.p1, cuboid.p7 subtract intersection)
          .flatMap(_.toArray).forall(_ >= -Epsilon)
      } yield {
        intersection.getZ
      }
    if (touchPoints.isEmpty) None
    else Some(touchPoints.min - Epsilon)
  }

  val ZDirection = new Vector3D(0, 0, 1)
}

final class PingPongAgent {
  private val racketMoving: Map[RacketAction, Vector3D] = Map(
    RacketUp -> new Vector3D(0, 0.1, 0),
    RacketDown -> new Vector3D(0, -0.1, 0))

  private var state: State3D = State3D(
    Cuboid(new Vector3D(0, 0, 0), new Vector3D(1000, 600, 1e100)),
    SortedMap(0.0 -> Cylinder(new Vector3D(500, 300, 0), ZDirection, 25)),
    Map(
      LeftRacket -> SortedMap(0.0 ->
        Parallelepiped(new Vector3D(50, 250, 0), new Vector3D(70, 350, 0), ZDirection)),
      RightRacket -> SortedMap(0.0 ->
        Parallelepiped(new Vector3D(950, 250, 0), new Vector3D(930, 350, 0), ZDirection))
    )
  )

  def at(when: Double): State2D = synchronized {
    intersection(state, when)
  }

  def action(when: Double, racket: Racket, action: RacketAction): Unit = synchronized {
    var stages = state.rackets(racket).to(when)
    var stage = stages.last._2
    stage = stage.move(when).impulse(racketMoving(action))
    stages += (when -> stage)
    for (touchPoint <- touch(state.position, stage, when)) {
      stages += (touchPoint -> stage.move(touchPoint).copy(direction = ZDirection))
    }
    state = state.copy(rackets = state.rackets.updated(racket, stages))
  }
}

final class AwtAction(enables: Option[Action], callback: ActionEvent => Unit) extends AbstractAction {
  private[this] val changeEnabling: () => Unit =
    enables.fold(() => setEnabled(false))(a => () => a.setEnabled(true))

  override def actionPerformed(e: ActionEvent): Unit = {
    changeEnabling()
    callback(e)
  }
}
