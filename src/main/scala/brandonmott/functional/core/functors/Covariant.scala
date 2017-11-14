package brandonmott.functional.core
package functors

/** All the following functors take an argument that is some arrangement of three type variables 
  * and then return a function with the type `F[A] => F[B]` */

/**
  * Covariant functor takes argument arrangement of type: `A => B`
  */
trait Covariant[F[_]] {
  def map[A, B](f: A => B): F[A] => F[B]

  //derived
  def xmap[A, B](f: A => B, g: B => A): F[A] => F[B] = map(f)
}

/**
  * Contravariant functor takes argument arrangement of type: `B => A`
  */
trait Contravariant[F[_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]
}

/**
  * Exponential functor takes argument arrangement of type: `(A => B, B => A)`
  */
trait Exponential[F[_]] {
  def xmap[A, B](f: (A => B, B => A)): F[A] => F[B]
}

/**
  * Applicative functor takes argument arrangement of type: `F[A => B]`
  */
trait Applicative[F[_]] {

  def apply[A, B](f: F[A => B]): F[A] => F[B]
}

/**
  * Monad functor takes argument arrangement of type: `A => F[B]`
  */
trait Monad[F[_]] {
  def flatMap[A, B](f: A => F[B]): F[A] => F[B]
}

/**
  * Comonad functor takes argument arrangement of type: `F[A] => B`
  */
trait Comonad[F[_]] {
  def coflatMap[A, B](f: F[A] => B): F[A] => F[B]
}
