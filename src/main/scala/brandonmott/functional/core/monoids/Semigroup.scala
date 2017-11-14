package brandonmott.functional.core
package monoids

/**
  * A Semigroup for some given type A has a single operation, which takes two values of type A, and
  * returns a value of type A. This operation must be guaranteed to be associative.
  * 
  * The Semigroup law of `Associativity` requires an `ASSOCIATIVE BINARY OPERATION`:
  * This means that for any x, y, and z of the T type, the following is true: 
  * {{{ op(op(x, y), z) == op(x, op(y, z)) }}}
  */
trait Semigroup[T] {
  def combine(a: T, b: T): T 
}

object Semigroup {
  
  implicit val intPlus: Semigroup[Int] = new Semigroup[Int] {
    override def combine(a: Int, b: Int): Int = a + b 
  }
  
  implicit val stringConcat: Semigroup[String] = new Semigroup[String] {
    override def combine(a: String, b: String): String = a + b 
  }
  
  implicit def listSemigroup[T] = new Semigroup[List[T]] {
    override def combine(a: List[T], b: List[T]): List[T] = a ++ b 
  } 
}


// Here's an example of what Monoid looks like extending a Semigroup 
trait MonoidK[T] extends Semigroup[T] {
  val zero: T
}
object MonoidKOps {

  implicit object intAdditionMonoidK extends MonoidK[Int] {
    def combine(a: Int, b: Int): Int = a + b
    val zero = 0
  }

  object intMultMonoidK extends MonoidK[Int] {
    def combine(a: Int, b: Int): Int = a * b
    val zero = 1
  }

  implicit object stringConcatMonoidK extends MonoidK[String] {
    def combine(a: String, b: String): String = a + b
    val zero = ""
  }
  
  implicit def listMonoidK[T] = new MonoidK[List[T]] {
    def combine(a: List[T], b: List[T]): List[T] = a ++ b
    val zero = Nil
  }

  /**
    * Here's an extension of Option that returns the zero value of a monoid if None
    * @param inner - the option being pimped
    */
  implicit class OptionOps[T](inner: Option[T]) {
    def getOrElseZero(implicit monoid: MonoidK[T]): T = inner.getOrElse(monoid.zero)
  }
  
}

