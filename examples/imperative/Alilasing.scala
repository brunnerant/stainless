import stainless.lang._

case class Container(var value: BigInt)

object Test {
  def transfer(a: Container, b: Container): Unit = {
    require(a.value >= 0 && b.value >= 0)

    if (a.value >= 2) {
      a.value -= 2
      b.value += 1
      transfer(a, b)
    }
  } ensuring { _ =>
    a.value == old(a).value % 2 && b.value == old(a).value / 2 + old(b).value
  }

  def test() = {
    val a = Container(10)
    val b = Container(10)
    transfer(a, a)
    b
  } ensuring { res =>
    res.value == 15
  }
}
