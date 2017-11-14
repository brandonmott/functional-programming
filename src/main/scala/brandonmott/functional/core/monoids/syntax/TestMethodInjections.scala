package brandonmott.functional.core
package monoids
package syntax

import brandonmott.functional.core.monoids.{FoldLeft, Monoid}

/**
  * Demos the use of method injections.
  */
object TestMethodInjections {
  def main(args: Array[String]): Unit = {
    p("*" * 100)

    // Method Injection using implicit conversions example:
    p("Method Injection")

    // Enrich all types that has an instance for Monoid
    def plus[A: Monoid](a: A, b: A): A = implicitly[Monoid[A]].mappend(a, b)

    p(plus(3, 4))
    p(plus("a", "b"))

    //using Identity trait (normally in syntax object)
    implicit def toIdent[A](a: A): Identity[A] = new Identity[A] {
      val value = a
    }

    //inject plus to both Int and String with just one definition.
    p(3.plus(4))
    p("a".plus("b"))

    // Enrich my library. inject `|+|` to both Int and String with just one definition.
    implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
      val F = implicitly[Monoid[A]]
      val value = a
    }

    //inject |+| to both Int and String with just one definition.
    p(3 |+| 4)
    p("a" |+| "b")

    // Enrich my library. inject `summ` to both Int and String with just one definition.
    implicit def toMA[M[_], A](ma: M[A]): MA[M, A] = new MA[M, A] {
      val value: M[A] = ma
    }

    // Moving the sum function to MA and providing this implicit conversion for better syntax
    p(List(1, 2, 3, 4).summ)
    p(List("a", "b", "c").summ)

    p("*" * 100)
  }
}
