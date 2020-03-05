import stainless.lang._
import stainless.collection._

import stainless.lang.StaticChecks.Ensuring

case class LinkedList[T](var root: Option[Node[T]])
case class Node[T](value: T, var next: Option[Node[T]])

object Test {
  def max(x: BigInt, y: BigInt): BigInt = {
    if (x > y) x else y
  } ensuring { res =>
    x <= res && y <= res && (x == res || y == res)
  }

  def nodeLength[T](node: Option[Node[T]]): BigInt = {
    (node match {
      case Some(node) => 1 + nodeLength(node.next)
      case None() => 0
    }) : BigInt
  } ensuring { res: BigInt =>
    res >= 0
  }

  def length[T](list: LinkedList[T]): BigInt = {
    nodeLength(list.root)
  } ensuring { res =>
    res >= 0
  }

  // def pushLemma[T](list: Option[Node[T]], value: T): Boolean = {
  //   val extendedList = Some(Node(value, list))
  //   nodeLength(extendedList) == nodeLength(list) + 1
  // }.holds

  def push[T](list: LinkedList[T], value: T) = {
    list.root = Some(Node(value, list.root))
  } ensuring { res =>
    length(list) == length(old(list)) + 1
  }

  def pop[T](list: LinkedList[T]): Option[T] = {
    list.root match {
      case Some(node) =>
        list.root = node.next
        Some[T](node.value)
      case None() =>
        None[T]()
    }
  } ensuring { res =>
    (res.nonEmpty ==> (length(list) == length(old(list)) - 1)) &&
    (res.isEmpty ==> (length(list) == length(old(list))))
  }
}
