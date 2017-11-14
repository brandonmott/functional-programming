package brandonmott.functional.chapters.errors

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Option => _, Some => _}

/** Option is an "algebraic data type" that has two cases: 
  *    1. it can be `defined` in which case it will be a `Some`, or 
  *    2. it can be `undefined`, in which case it will be a `None`
  * We can explicitly pattern match on an Option, we’ll almost always use the higher-order functions below.
  * See if you can recognize the patterns these functions encapsulate before you resort to pattern matching. */
sealed trait Option[+A] { // Option data type, `parameterized` on a type, `A`
  
  /** Note that place the functions inside the body of the Option trait (or an abstract class) so they can be called with 
    the syntax: {{{ obj.fn(arg1) or obj fn arg1 }}} instead of:   {{{fn(obj, arg1)}}}
    ! When Defining the functions use keyword `this` inside trait to refer to the Option instance  */

  /** `map` can be used to `transform` the result inside an Option, if it exists.
    * A common pattern is to transform an Option via calls to map, flatMap, and/or filter, and then use getOrElse to do error handling at the end */
  def map[B](f: A => B): Option[B] = this match {
    case n @ None => n
    case Some(v) => Some(f(v))
  }

  /** `getOrElse` returns the result inside the Some case of the Option, or if the Option is None, returns the given default value.
    *  The `default` value is a non-strict or lazy argument, as indicated by the `=> A` as the type of a. */
  def getOrElse [B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  /** `flatMap` is similar to `map`, except that the function we provide to transform the result can itself fail.
    *  The computation will abort as soon as the first failure is encountered */
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  // implement `flatMap` with explicit pattern matching.
  def flatMapViaMatch[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  /** `orElse` returns the first Option if it’s defined; otherwise, it returns the second Option.
    * `orElse` is similar to getOrElse, except that we return another Option if the first is undefined. 
    *   This is often useful when we need to chain together possibly failing computations, trying the second if the first hasn’t succeeded.
    *   The `ob` value is a non-strict or lazy argument, as indicated by the `=> A` as the type of a. */
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob
  //implement `orElse` with explicit pattern matching.
  def orElseViaMatch[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  /** `filter` returns the returns the Some case if the predicate `f` is true, else it returns None.
    * We can use filter to convert successes into failures if the successful values don’t match the given predicate.
    * Implement filter in terms of `flatMap` with the predicate <code> if(f(v)) Some(v) else None </code> inside. */
  def filter(f: A => Boolean): Option[A] = flatMap (x => if(f(x)) Some(x) else None)
  //implement `filter` with explicit pattern matching.
  def filterViaMatch(f: A => Boolean): Option[A] = this match{
    case Some(v) if f(v) => this
    case _ => None
    //Use the wildcard to return None when this is defined but the predicate `f(v)` yields false
  }
}

//a Option _data constructor_ representing a defined type A
case class Some[+A](get: A) extends Option[A]

//a Option _data constructor_ representing an undefined type
case object None extends Option[Nothing]

object Option {

  // The general rule of thumb is that we use exceptions only if no reasonable program would ever catch the exception
  // if for some callers the exception might be a recoverable error, we use Option (or  Either, discussed later) to give them flexibility.

  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + (( throw new Exception("fail!")) :Int)
      //A thrown Exception can be given any type; here we're annotating it with the type`Int`
    }
    catch {
      case e : Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /** As the implementation of `variance` demonstrates, with `flatMap` we can construct a computation with multiple stages, 
    * any of which may fail, and the computation will abort as soon as the first failure is encountered, 
    * since None.flatMap(f) will immediately return None, without running f. */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean( xs.map( x =>math.pow(x - m, 2))))

  /** we can `lift` ordinary functions to become functions that operate on `Option`.
    * the `map` function lets us operate on values of type `Option[A]` using a function of type `A => B`, returning `Option[B]`.
    * map turns a function `f` of type `A => B` into a function of type `Option[A] => Option[B]`.
    *
    * Functions can be transformed (via `lift`) to operate within the context of a single `Option` value, without rewriting it. */
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val absOtp = lift(math.abs)

  /** `Try` function is a general-purpose function we can use to convert from an exception-based API to an Option-oriented API.
    * This uses a `non-strict` or lazy argument, as indicated by the `=> A` as the type of a.
    * Note: This discards information about the error. */
  def Try[A](a: => A): Option[A] =
  try Some(a)
  catch{ case e: Exception => None}

  /** `map2` combines two Option values using a binary function. If either Option value is None, then the return value is None too.
    * `map2` function means that we never need to modify any existing functions of two arguments to make them “Option-aware.”
    * `map2` lifts a function of two arguments to operate in the context of Option after the fact. 
    * You can even define `map3`, `map4`, and `map5`.. and so on.  */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a flatMap{ a1 =>
    b map{ b1 =>
      f(a1, b1)
    }
  }

  /** `sequence` combines a list of Options into one Option containing a list of all the `Some` values in the original list.
    * It can be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
    * Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). 
    * 
    * This is an unfortunate consequence of Scala using subtyping to encode algebraic data types. */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  /** sequence combines a list of Options into one Option containing a list of all the `Some` values in the original list. 
    * If the original list contains None even once, the result of the function should be `None`; 
    *   otherwise the result is Some with a list of all the values. */
  //Here's an explicit recursive version:
  def sequenceViaMatch[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }
  /** `traverse` is a efficient way to map over a list using a function that might fail. 
    * Uses sequence mapping a list of Options into one Option containing a list of all the `Some` values in the original list. */
  def traverse [A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap( a1 => traverse(t)(f) map( b1 => a1 :: b1))
  }

  def sequenceViaTraverse[A, B](a: List[Option[A]]): Option[List[A]] = 
    traverse(a)(x => x)
  //Use identity function

  /** Since lifting functions is so common in Scala, Scala provides a syntactic construct 
    * called the for-comprehension that it expands automatically to a series of flatMap and map calls. */
  def map2ViaForComp[A, B, C](a: Option[A], b: Option[B])(f: (A, B) =>C): Option[C] =
    for{
      aa <- a
      bb <- b
    } yield f(aa,bb)

  def traverseViaForComp[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => for {
        aa <- f(h)
        bb <- traverseViaForComp(t)(f)
      } yield aa :: bb
    }
}


//  object OptionTest extends App{
//
//  } 