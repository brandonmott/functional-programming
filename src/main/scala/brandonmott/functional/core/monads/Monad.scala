package brandonmott.functional.core
package monads

trait Functor[F] {
  def map[A](f: F => A): Functor[A]
}

/** Monads are used as `Computation Builders`
  * 
  * Monads are functors so they must follow all the Functor laws.
  * <h4>Monad Rules:</h4>
  * ○ Identity law: Doing `map` over the `identity` function doesn't change the data: 
  *   {{{map(x)(i => i) == x}}} 
  *   Flat mapping over the `unit` function also keeps the data the same:
  *   {{{x.flatMap(i => unit(i)) == x}}} 
  *   The latter basically says that `flatMap` undoes `unit`. 
  *   Using the connection between `map`, `flatMap`, and `unit` we defined earlier, 
  *   we can derive one of these two rules from the other and vice versa. 
  *   The `unit` method can be thought of as the `zero element` in monoids.
  * <br><br>
  * ○ The unit law: From the definition of `unit`, we can also say this: 
  *   {{{unit(x).flatMap { y => f(y) } == f(x)}}} 
  *   From this, we will get {{{unit(x).map { y => f(x) } == unit(f(x))}}} 
  *   This gives us some interesting connections between all the methods.
  * <br><br>
  * ○ Composition: Multiple maps must be composed together. It should make no difference if we do 
  *   {{{x.map(i => y(i)).map(i => z(i))}}} or {{{x.map(i => z(y(i)))}}} 
  *   Moreover, multiple `flatMap` calls must also compose, making the following true: 
  *   {{{x.flatMap(i => y(i)).flatMap(i => z(i)) == x.flatMap(i => y(i).flatMap(j => z(j)))}}}
  * <br><br>
  * Monads, similarly to monoids, also have a `zero element` (identity). 
  *
  * <h4>Monad Laws Defined</h4>
  * To qualify as a monad, a type has to satisfy three laws:<br>
  *   1. Associativity:
  *     {{{m flatMap f flatMap g == m flatMap (x => f(x) flatMap g)}}}
  *   2. Left unit
  *     {{{unit(x) flatMap f == f(x)}}}
  *   3. Right unit
  *     {{{m flatMap unit == m}}}
  */
trait Monad[F] extends Functor[F] {
  /**
    * `unit` takes a value of the T type and turns it into a Monad of the T type.
    * In Scala, this can be expressed using a companion object with an `apply` method
    */
  def unit[A](value: A): Monad[A]
  
  def flatMap[A](f: F => Monad[A]): Monad[A]

  override def map[A](f: F => A): Monad[A] = 
    flatMap(i => unit(f(i)))
}


