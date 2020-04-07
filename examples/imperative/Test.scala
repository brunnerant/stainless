
object Test {
    case class A(var a: Int, var b: Int)

    // def rec(a: A, n: Int, acc: Int): A = {
    //     if (n == 0) {
    //         a.b = a.a
    //         a.a = acc
    //         a
    //     } else {
    //         rec(a, n - 1, n * acc)
    //     }
    // }

    // def test(): B = {
    //     val a = A(42, 0)
    //     val b = rec(a, 5, 1)
    //     b
    // }

    // def rec(arr: Array[Int], n: Int, acc: Int): Array[Int] = {
    //     arr(n) = acc

    //     if (n == 0) arr
    //     else rec(arr, n - 1, n * acc)
    // }

    // def test(): Unit = {
    //     val a = Array.fill(5)(0)
    //     rec(a, 4, 1)(0) = 0
    // }

    // This is an example of a program that makes the AntiAliasing fail when it shouldn't
    // def test(cond: Boolean, x: A, y: A) {
    //     val z = if (cond) x else y
    //     z.a = z.b
    // }
}
