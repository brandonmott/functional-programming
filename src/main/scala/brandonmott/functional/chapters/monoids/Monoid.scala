package brandonmott.functional.chapters.monoids


trait Monoid[A] {
  def zero: A
  def op(a1: A, a2: A): A
}

object Monoid {
  object stringConcatination extends Monoid[String] {
    override def zero: String = ""
    override def op(a1: String, a2: String): String = a1 + a2  
  }
  
  object booleanOr extends Monoid[Boolean] {
    override def zero: Boolean = false
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  }

  object booleanAnd extends Monoid[Boolean] {
    override def zero: Boolean = true
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  }
  
  object intAddition extends Monoid[Int] {
    override def zero: Int = 0
    override def op(a1: Int, a2: Int): Int = a1 + a2 
  }
  
  object intMultiplication extends Monoid[Int] {
    override def zero: Int = 1
    override def op(a1: Int, a2: Int): Int = a1 * a2
  }
  
  object listConcatination extends Monoid[List[_]] {
    override def zero: List[_] = Nil 
    override def op(a1: List[_], a2: List[_]): List[_] = a1 ++ a2
  }
  

/* 
  Note: the order of the composition of the options matters.
  Every monoid has a duel where the op combines things in the opposite order. 
  Monoids like booleanOr and intAddition are evuivalent to their duels because
  their op is commutative as well as associative. Both forms are valid but not
  necessarily equivalent.
*/
  def optionCombineMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }
  /*
  A function having the same argument and return type is sometimes called an `endofunction`.
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = identity
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
  } 
}
