
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
}
