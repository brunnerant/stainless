import stainless.lang._
import stainless.collection._

case class Node(var value: BigInt, var next: Option[Node])

object Test {
  def nth(node: Option[Node], n: BigInt): Option[Node] = {
    node match {
      case None() => None()
      case Some(node) =>
        if (n == 0) Some(node)
        else nth(node.next, n - 1)
    }
  }

  def transfer(node1: Node, node2: Node): Unit = {
    require(node1.value >= 0 && node2.value >= 0)
    if (node1.value > 0) {
      node1.value -= 1
      node2.value += 1
      transfer(node1, node2)
    }
  } ensuring { res =>
    node1.value == 0 && node2.value == old(node1).value + old(node2).value
  }

  def test(): Unit = {
    val node1 = Node(1, Some(Node(2, None())))
    // val node2 = Node(3, Some(Node(4, None())))
    val Some(node2) = nth(Some(node1), 0)
    transfer(node1, node2)
    node2
  } ensuring { res =>
    res.value == 4
  }
}
