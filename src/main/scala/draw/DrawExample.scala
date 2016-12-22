package draw

import java.io.Closeable
import java.util.concurrent.atomic.AtomicReference
import javax.swing.SwingUtilities

import aether.CubeXY._

object DrawExample {
  val testWorldPosition = Rectangle(Point(20.0, 20.0), Point(60.0, 60.0))
  val testEllipses = (26 to 55).map(n => Ellipse(Point(25.0, 25.0), Point(n, n)))
  @volatile var testShapes = Stream.continually(testEllipses ++ testEllipses.reverse).flatten

  def pullWorld(): World = {
    val h #:: t = testShapes
    testShapes = t
    World(testWorldPosition, Set(h))
  }

  def main(args: Array[String]): Unit = {
    val closeable = new AtomicReference[Closeable]()
    SwingUtilities.invokeLater(() => closeable.set(Draw.animate(fps = 50)(draw(pullWorld(), _, _))))
    Thread.sleep(10000)
    closeable.get().close()
  }
}
