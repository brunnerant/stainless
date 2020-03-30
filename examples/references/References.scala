
import stainless.lang._

object Test {
  case class Point(x: BigInt, y: BigInt)

  def diff(x: Ref[Point], y: Ref[Point]): Point =
    Point(x.deref.x - y.deref.x, x.deref.y - y.deref.y)
}
