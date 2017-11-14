package brandonmott.functional.core
package functors

/** `F[_]` uses the "Higher-kinded Type" feature in Scala. */

/**
  * A functor is a class that `has a map method` and `conforms to functor laws`
  *
  * The Functor laws:
  * Identity: Whenever the identity function is mapped over some data, it doesn't change it. 
  * In other words, map(x)(i => i) == x.
  *
  * Composition: Multiple maps must compose together. 
  * It should make no difference if we do this operation: 
  *       x.map(i => y(i)).map(i => z(i)) or x.map(i => z(y(i))). 
  *
  * The `map` Function: The map function preserves the structure of the data, for example, 
  * it does not add or remove elements, change their order, and so on. It just changes the representation.
  */
trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]
  
  
  // derived
  //Sometimes called imap
  def xmap[A,B](fa: F[A], f: A => B, g: B => A): F[B] = map(fa)(f)

  /**
    * Lift a function f to operate on Functors
    */
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  /**
    * Empty the `fa` of the values, preserving the structure
    */
  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  /**
    * Tuple the values in fa with the result of applying a function
    * with the value
    */
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))

}

object Functor {
  
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B):Option[B] = fa map f
  }
  
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }
  
}


