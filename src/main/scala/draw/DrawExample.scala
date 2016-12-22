package draw

import java.io.Closeable
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import javax.swing.SwingUtilities

import aether.CubeXY.{Point2D => Point2D, World2D => World2D, draw => draw2D}
import aether.CubeXYZ.{Point => Point3D, World => World3D, _}

object DrawExample {
  val world = World3D(
    Cuboid(Point3D(200, 200, 200), Point3D(600, 600, 600)),
    Set(
      Wall(Point2D(250, 250), Point2D(250, 550)),
      Wall(Point2D(250, 550), Point2D(550, 550)),
      Wall(Point2D(550, 550), Point2D(550, 250)),
      Wall(Point2D(550, 250), Point2D(250, 250))),
    Map(1 -> (Cylinder(Ray(Point3D(250, 250, 200), Direction(1, 1, 1)), 50) -> DummyHierarchy)))

  val z = new AtomicInteger(0)

  def pullWorld(): World2D = {
    intersection(world, XYPlane(world.border.p1.z + 5 * z.getAndIncrement()))
  }

  def main(args: Array[String]): Unit = {
    val closeable = new AtomicReference[Closeable]()
    SwingUtilities.invokeLater(() => closeable.set(Draw.animate()(draw2D(pullWorld(), _, _))))
    Thread.sleep(10000)
    closeable.get().close()
  }
}
