package brandonmott.functional.core
package monoids

/** 
  * A Monoid is a `purely algebraic structure`, which means that it is defined only by its `algebra`. 
  * All Monoids must conform to the so called `Monoid laws`.
  * 
  * The Monoid laws:
  *   A Monoid must have a TYPE - T
  *   A Monoid must have one ASSOCIATIVE BINARY OPERATION - `mappend`
  *   A structure must have an IDENTITY ELEMENT — `mzero`
  * 
  * Monoid is a type along with an `ASSOCIATIVE BINARY OPERATION` over it, which also has an `IDENTITY ELEMENT`. */
trait Monoid[T] {
  
  /** Monoid's `ASSOCIATIVE BINARY OPERATION`:
    *   This means that for any x, y, and z of the T type, the following is true: 
    *     {{{ op(op(x, y), z) == op(x, op(y, z)) }}}
    */
  def mappend(l: T, r: T): T

  /** Monoid's `IDENTITY ELEMENT`:
    *   This element is characterized by the fact that the previous operation will always return the other element: 
    *     {{{ op(x, zero) == x and op(zero, x) == x` }}}
    */
  def mzero: T
}

/** 
  * Companion object for Monoid
  * 
  * The `Monoid laws` are extremely simple but they give us great power to write polymorphic functions 
  * based just on the fact that monoids always conform to the same rules.
  * 
  * Package up the each implementation of monoid in an object called Monoid. 
  * 
  * The reason for that is Scala’s implicit resolution rules: 
  * When it needs an implicit parameter of some type, it’ll look for anything in scope. 
  * It’ll include the companion object of the type that you’re looking for.
  */
object Monoid {

  /**
    * Let's look at INTEGER ADDITION Monoid:
    *   TYPE: Int
    *   ASSOCIATIVE BINARY OPERATION: add. 
    *     It is indeed ASSOCIATIVE because ((1 + 2) + 3) == (1 + (2 + 3)).
    *   IDENTITY ELEMENT: 0. It does nothing when added to another integer.
    */
  implicit object IntAdditionMonoid extends Monoid[Int] {
    def mappend(a: Int, b: Int): Int = a + b
    def mzero: Int = 0
  }

  /**
    * Let's look at INTEGER MULTIPLICATION:
    *   TYPE: Int
    *   ASSOCIATIVE BINARY OPERATION: multiply. 
    *     It is indeed ASSOCIATIVE because ((1 * 2) * 3) == (1 * (2 * 3)).
    *   IDENTITY ELEMENT: 1. any integer does nothing when multiplied by 1.
    */
  /*no implicit for Int to avoid ambiguous implicit values error with IntAdditionMonoid */ 
  object IntMultiplicationMonoid extends Monoid[Int] {
    override def mappend(l: Int, r: Int): Int = l * r
    val mzero: Int = 1
  }

  /**
    * Let's look at STRING CONCATENATION:
    *   TYPE: String
    *   ASSOCIATIVE BINARY OPERATION: concatenate. 
    *     It is ASSOCIATIVE because (("bra" + "nd") + "on") == ("bra" + ("nd" + "on")).
    *   IDENTITY ELEMENT: an empty string. an empty string doesn't change a string.
    */
  implicit object StringConcatenationMonoid extends Monoid[String] {
    override def mappend(l: String, r: String): String = l + r
    val mzero: String = ""
  }

  
}


