
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
  } ensuring { _ =>
    p.deref.x == old(p).deref.x + dx
  }

  // It is also possible to mutate a parameter that was passed by value. Since we assume
  // that such parameters are never used once passed to the function, we don't need to return
  // their new state at the end of the function.
  def shiftXByValue(p: Point, dx: BigInt): Point = {
    p.x += dx
    p
  }

  // The shiftX function is transformed into a function that returns the shifted point :
  // def shiftX(p: Point, dx: BigInt): Point = {
  //   Point(p.x + dx, p.y)
  // }

  def weird(p1: Point, p2: Point, cond: Boolean): Unit = {
    (if (cond) {
      p2.y -= 1
      p1
    } else {
      p1.y -= 1
      p2
    }).x = 1
  }
}
