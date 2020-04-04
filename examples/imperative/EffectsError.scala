import stainless.lang._

case class Point(var x: BigInt, var y: BigInt)
case class Segment(var start: Point, var end: Point)

object Test {
  def shiftX(p: Point, dx: BigInt): Unit = {
    p.x += dx
  }

  def main() = {
    val p = Point(0, 0)
    val q = Point(1, 1)
    val s = Segment(p, q).start
  }
}
