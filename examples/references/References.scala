
import stainless.lang._

object Test {
  case class Point(var x: BigInt, var y: BigInt)

  // Accessing a shared reference is allowed
  def diff(x: Ref[Point], y: Ref[Point]): Point =
    Point(x.deref.x - y.deref.x, x.deref.y - y.deref.y)

  // Mutating a shared reference isn't allowed
  // def shiftX(p: Ref[Point], dx: BigInt): Unit = {
  //   p.deref.x += x
  // }

  // Instead, a mutable reference should be used
  def shiftX(p: RefMut[Point], dx: BigInt): Unit = {
    p.deref.x += dx
  }

  def shiftXByValue(p: Point, dx: BigInt): Point = {
    p.x += dx
    p
  }

  // The shiftX function is transformed into a function that returns the shifted point :
  // def shiftX(p: Point, dx: BigInt): Point = {
  //   Point(p.x + dx, p.y)
  // }
}
