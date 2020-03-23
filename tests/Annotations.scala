import stainless.lang._
import stainless.annotation._

object Test {
  def max(x: BigInt @mut, y: BigInt): BigInt = {
    if (x > y) x else y
  } ensuring { res =>
    res >= x && res >= y && (res == x || res == y)
  }
}
