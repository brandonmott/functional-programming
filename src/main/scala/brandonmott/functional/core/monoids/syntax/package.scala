package brandonmott.functional.core
package monoids

/**
  * Method injections should be stored in an package called syntax.
  * Using the same technique, Scalaz provides Method injections for standard library types like Option and Boolean.
  */
package object syntax {
  //using Identity trait
  implicit def toIdent[A](a: A): Identity[A] = new Identity[A] {
    val value = a
  }

  // Enrich all types that has an instance for Monoid 
  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    val F = implicitly[Monoid[A]]
    val value = a
  }

  //using MA to summ a higher kinded type
  implicit def toMA[M[_], A](ma: M[A]): MA[M, A] = new MA[M, A] {
    val value: M[A] = ma
  }

  //Quick printer
  def p(a: Any) {
    println("###> " + a)
  }
}
