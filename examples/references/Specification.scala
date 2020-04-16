
import stainless.lang._

object Test {
    case class A(var a: Int)

    // This function is recursive but it doesn't capture any of its arguments as
    // a mutable reference, so we know it cannot have effects when called.
    def fib1(n: Int): A = {
        def rec(n: Int, a: Int, b: Int): A = {
            if (n == 0) A(a)
            else rec(n - 1, b, a + b)
        }

        rec(n, 0, 1)
    }

    // This function is recursive and it captures a1 by mutable reference, so we know
    // it could have effects (and it actually has an effect).
    def fib2(n: Int, a1: RefMut[A]): RefMut[A] = {
        def rec(n: Int, a: Int, b: Int) = {
            if (n == 0) a1.deref.a += a
            else rec(n - 1, b, a + b)
        }

        rec(n, 0, 1)
        a1
    }

    // This function has effects but it is not recursive, so it can be inlined
    def select(cond: Boolean, a1: RefMut[A], a2: RefMut[A]): RefMut[A] = {
        if (cond) a1
        else a2
    }

    // This lists what should be transformable or not by the imperative phase of stainless.
    def examples(cond: Boolean) = {
        val a1 = A(1)
        val a2 = A(2)

        fib1(3).a = 1                   // This assignment can be performed, because fib1 has no effects,
                                        // looking at its signature. In fact, this assignment has no effect
                                        // from the point of view of this function.

        fib2(3, a1).a += 2              // This assignment cannot be performed, because fib2 has effects
                                        // and it is recursive, hence not inlinable

        select(cond, a1, a2).a = 3      // This can be performed thanks to inlining, even though the function
                                        // has effects

        // This is slightly contrived, but it should be possible to desugar it.
        // There are two parts to it: the outer assignment, and the mutation inside the target
        // of the assignment. Both parts should be considered for the translation to be correct
        {
            if (cond) {
                a1.a += 1
                a2
            } else {
                a2.a += 1
                a1
            }
        }.a = 42
    }

    // This example is a bit tricky to transform, and I haven't found a way of doing it yet.
    def test(a: Int, b: Int) = {
        def min(a: RefMut[Int], b: RefMut[Int]): RefMut[Int] =
            if (a.deref < b.deref) a else b

        val r: RefMut[Int] = min(a.refMut, b.refMut)
        r.deref += 1          // This assignment here should somehow know whether a or b should be mutated
        println(s"$a, $b")    // Because otherwise, this line will print the wrong data

        // In this example we could inline min to find out that information, but in a more
        // complex example where functions call other functions, it becomes hard to keep track of that.
    }

    // The following example shows how stateful functions could be transformed using
    // state monads. This would allow the translation to be modular and easily extensible
    // at function boundaries. However, I don't know to what extent lambdas are supported by
    // stainless, so I might need to ask.
    def test(a: RefMut[Int], b: RefMut[Int]) = {
        def min(a: RefMut[Int], b: RefMut[Int]): RefMut[Int] =
            if (a.deref < b.deref) a else b

        val r = min(a.refMut, b.refMut)
        r.deref += 1
    }

    def test(a: Int, b: Int): (Int, Int) = {
        def min(a: Int, b: Int): ((Int, Int) => Int, (Int, Int, Int) => (Int, Int)) =
            if (a < b) ((a, b) => a, (a, b, x) => (x, b))
            else ((a, b) => b, (a, b, x) => (a, x))

        val (get, set) = min(a, b)
        set(a, b, get(a, b) + 1)
    }
}
