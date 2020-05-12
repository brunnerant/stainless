
import stainless.lang._

object Test {
  case class Point(var x: BigInt, var y: BigInt)

  // Accessing a shared reference is allowed
  def diff(x: Ref[Point], y: Ref[Point]): Point =
    Point(x.deref.x - y.deref.x, x.deref.y - y.deref.y)

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

  // We can also implement shiftByValue by calling shiftX with a mutable reference
  def shiftXByValueBis(p: Point, dx: BigInt): Point = {
    shiftX(p.refMut, dx)
    p
  } ensuring { res =>
    res == shiftXByValue(old(p), dx)
  }

  // This function is not useful, but it shows that the translation is somewhat
  // resistant to weird inputs
  def weird(p1: Point, p2: Point, cond: Boolean): Unit = {
    (if (cond) {
      p2.y -= 1
      p1
    } else {
      p1.y -= 1
      p2
    }).x = 1
  }

  def switch(n: BigInt, p1: RefMut[Point], p2: RefMut[Point]): RefMut[Point] =
    if (n % 2 == 0) p1 else p2

  // def switch(n: BigInt, p1: RefMut[Point], p2: RefMut[Point]): RefMut[Point] =
  //   if (n <= 0) p1 else switch(n - 1, p2, p1)

  def aliasing(n: BigInt): (Point, Point) = {
    val p1 = Point(0, 0)
    val p2 = Point(1, 1)
    val p3 = switch(n, p1.refMut, p2.refMut)
    p3.deref.x = 2
    (p1, p2)
  }
}
