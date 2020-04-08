
object Test {
    case class A(var a: Int)

    // This is an example of a program that makes the AntiAliasing fail when it shouldn't
    // def test(cond: Boolean, x: A, y: A) {
    //     val z = if (cond) x else y
    //     z.a = 1
    // }

    // This is a second bug in stainless
    // def test(c1: Boolean, c2: Boolean) = {
    //     val a = A(1)
    //     val b = A(2)
    //     val c = A(3)
    //     val x = if (c1) if (c2) a else b else c
    //     x.a = 4
    // }
}
