package aether

import java.io.Closeable
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.swing.SwingUtilities

import aether.Draw._
import aether.Model._
import aether.Operation._

object Example {
  val world = World3D(
    Cuboid(Point3D(200, 200, 200), Point3D(600, 600, 600)),
    Set(
      Wall(Point2D(250, 250), Point2D(250, 550)),
      Wall(Point2D(250, 550), Point2D(550, 550)),
      Wall(Point2D(550, 550), Point2D(550, 250)),
      Wall(Point2D(550, 250), Point2D(250, 250))),
    Set(
      Cylinder(Ray3D(Point3D(250, 250, 200), Direction3D(1, 1, 1)), 50))
  )

  val z = new AtomicInteger(0)

  def pullWorld(): World2D = {
    intersection(world, XYPlane(world.border.p1.z + 5 * z.getAndIncrement()))
  }

  def main(args: Array[String]): Unit = {
    val closeable = new AtomicReference[Closeable]()
    SwingUtilities.invokeLater(() => closeable.set(Draw.animate()(draw(pullWorld(), _, _))))
    Thread.sleep(10000)
    closeable.get().close()
  }
}
