// In this file, I try to see if having an ownership system would allow to solve
// more problems than the current way Stainless works.

import stainless.lang._

case class ADT(var a: Int, var b: Int)

object Test {
  def f(a1, a2) = {
    a1.x = 213
    a2.x = 321
  }

  def testAliasing(): ADT = {
    val adt = ADT(1, 2)
    val adt2 = shareReference(adt)
    adt.b += 1
    adt2.a = 3
    adt2
  } ensuring { res =>
      res == ADT(3, 3)
  }

  def shareReference(adt: ADT): ADT = {
    adt.a += 1
    adt
  }
}
