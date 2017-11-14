package brandonmott.functional.chapters.laziness

sealed trait Stream[+A] {
import Stream._
  /** `headOption` must "explicitly" `force` the h `thunk` via `h()`. In this example, we do NOT evaluate the tail of the Cons. */
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /** The natural recursive `toList` is a helper function that will "stack overflow" on large streams, since its not tail recursive. */
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toListRecursive
  }

  /** The tail-recursive toList Helper function for inspecting streams  
    * At each step we cons onto the front of the `list` list, which will result in the reverse of the stream. 
    * Then at the end we reverse the result to get the correct order again. */
  def toList: List[A] = {
    @annotation.tailrec
    def init(stream: Stream[A], list: List[A]):List[A] = stream match {
      case Cons(h,t) => init(t(), h() :: list)
      case _ => list
    }
    init(this, List()).reverse
  }

  /** In order to avoid the `reverse` at the end, we could write it using a mutable list buffer and an explicit recursive loop instead. 
    * Note that the mutable list buffer never escapes our `toList` method, so this function is still pure.  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  /** Create a new Stream[A] from taking the `n` first elements from `this`. Cases:
    *   If n > 1, recursively calling take on the invoked tail of a cons cell. 
    *   If n == 1, tail is not invoked
    *   If n == 0, we can avoid looking at the stream at all. */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /** `drop` creates a new Stream[A] from `this`, but ignores the `n` first elements. This can be achieved by recursively calling
    * `drop` on the invoked tail of a `Cons` cell. Note that the implementation is tail-recursive. */
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /** Similar to function take, but returning all starting elements of the stream that match the predicate `p` */
  def takeWhile(p: A => Boolean):Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  /** <b>An Important Note:</b> A major theme of sandbox.brandonmott.functional programming is "separation on concerns", separate descriptions of computations from actually running them.
    * <b>Examples:</b> First-class functions capture some computation in their bodies but only execute it once they receive their arguments.
    * Option captures the fact that an error occurred, where the decision of what to do about it became a separate concern.
    * `Laziness` lets us separate the `description` of an expression from the `evaluation` of that expression. */

  /** Remember the `tail` of the stream is a `lazy val`. So not only does the traversal terminate early, the tail of the stream is never evaluated at all!
    * If `p(head())` returns true, then exists terminates the traversal early and returns true as well.
    * If `p(head())` returns false, the right side of the || gets evaluated. */
  def existsExplicit(p: A => Boolean):Boolean = this match {
    case Cons(head,tail) => p(head()) || tail().existsExplicit(p) // Note that || is non-strict in its second argument.
    case _ => false
  }

  /** The combining function `f` takes its second argument `=> B` (as a by-name argument).
    * If `f` chooses not to evaluate its second parameter, this terminates the traversal early. */
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /** Argument `t` is the unevaluated recursive step that folds the tail of the stream. 
    * If `p(h)` returns true, `t` will never be evaluated and the computation terminates early.
    * Note: Since foldRight can terminate the traversal early, we can reuse it to implement exists. 
    * We can’t do that with a strict version of foldRight (meaning with List[A]) */
  def exists(p: A => Boolean): Boolean = foldRight(false)((h,t) => p(h) || t)

  //Laziness makes our code more reusable.

  /** checks that all elements in the Stream match a given predicate. This will terminate the traversal if encounters a non-matching value
    * Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a non-matching element is found. */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((h,t) => p(h) && t)

  /** `foldRight` will traverse the stream until the function `p` returns `empty[A]`
    *   If `p(h)` is true, `cons(h,t)` is applied to the match statement in `foldRight` 
    *     In foldRight's implementation, `cons(h,t)` matches the case that calls `foldRight` again.
    *   If `p(h)` is false, `empty[A]` is applied to the match statement in `foldRight`
    *     In foldRight's implementation, `empty[A]` matches the case that returns `z` the default (which is empty[A]). */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
  foldRight(empty[A])((h,t) =>
    if(p(h)) cons(h,t)
    else empty[A] )

  //Let None: Option[A] be the first argument to foldRight. Follow the types from there. 
  def headOptionViaFoldRight():Option[A] = foldRight(None: Option[A])((h,t) => Some(h) )
  
/** <b> INCREMENTAL function implementation </b>  `map`, `filter`, `append`, and `flatMap` implementations are `INCREMENTAL`. 
  * — they don’t fully generate their answers. It’s not until some other computation looks at the elements of the resulting Stream that 
  * the computation to generate that Stream actually takes place—and then it will do just enough work to generate the requested elements. 
  * Because of this `~`incremental nature`~`, we can call these functions one after another without fully instantiating the intermediate results.  */

  def map[B](f: => A => B): Stream[B] =
  foldRight(empty[B])((h,t) => cons(f(h), t) )

  def filter(p: => A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if(p(h)) cons(h,t)
      else t )

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h).append(t) )


  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
  zipWith(s2)((_,_))


  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /** `startsWith` checks if the `that` stream is equal to `this` stream. */
  def startsWith[A](that: Stream[A]): Boolean =
    zipAll(that)                        // transforms into `Stream[(Option[A],Option[B])]`. The Option indicates whether each stream has been exhausted.
      .takeWhile(!_._2.isEmpty)         // Checks if `that` stream has more values
      .forAll {case (a,b) => a == b}    // Checks all values for equality returns false when first case is false.

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case s => Some(s, s drop(1))
    } append Stream(empty)

  /** We can now implement `hasSubsequence` using functions we’ve written: */
  def hasSubsequence[A](s: Stream[A]): Boolean =
  tails exists (_ startsWith s)

  /** The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. 
    * It can be implemented using `foldRight` though. The implementation is just a `foldRight` that keeps the accumulated value and 
    * the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more 
    * state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished. */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
  foldRight((z, Stream(z)))((a, p0) => {
    // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
    lazy val p1 = p0
    val b2 = f(a, p1._1)
    (b2, cons(b2, p1._2))
  })._2

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

}
case object Empty extends Stream[Nothing]
//The Cons data constructor takes explicit thunks, instead of regular strict values
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

/* Memoizing streams and avoiding recomputation with smart constructors */
object Stream {
  
  //Cache the head and tail as lazy vals to avoid repeated evaluation.
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  
  def empty[A]: Stream[A] = Empty

  // Use _* to adapt a sequence, tell the compiler to pass each element of a sequence
  def apply[A](as: A*):Stream[A] = 
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /** Infinite streams and corecursion */
  
  /** Because they’re `incremental` the functions we’ve written also work for infinite streams.
    * is a stream that refers to itself; it generates an infinite series of 1‘s. */
  val ones: Stream[Int] = cons(1, ones)

  /** Generalize `ones` slightly to the function constant, which returns an infinite Stream of a given value. */
  def constant_a[A](a: A): Stream[A] = cons(a, constant(a))

  /** This is more efficient than `cons(a, constant(a))` since it's just one object referencing itself. */
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  /** creates an infinite stream that increments by 1. */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /** `fibs` that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
    * Uses a recursive helper function `init`
    * example: {{{fibs.take(10).toList = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)}}} */
  def fibs = {
    def init(n1: Int,n2: Int):Stream[Int] = {
      cons(n1,init(n2, n1+n2))
    }
    init(0,1)
  }

  /** The function `unfold` is a very general Stream-building function. 
    * `Option` is used to indicate when the Stream should be terminated, if at all.
    * `unfold` function is an example of what’s called a `corecursive` function. 
    * Whereas a `recursive` function "consumes data", a `corecursive` function "produces data". 
    * `unfold` function is productive as long as `f` terminates.
    * 
    * <b>`Corecursion` is also sometimes called `"guarded recursion"`, and `Productivity` is also sometimes called `"co-termination"`.</b>
    *
    * @param z - The Initial State `S`
    * @param f - Function takes the initial state `S` and produces either: 
    *               `Some` tuple with of the next value type `A` and the next state type `S` or 
    *               `None` which terminates with `empty`.  
    * @tparam A - the type of the stream
    * @tparam S - State
    * @return the result of type `Stream[A]` */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]) : Stream[A] =
  f(z) match {
    case Some( (a,s) ) => cons( a, unfold(s)(f) )
    case None => empty
  }

  /** The below two implementations use `fold` and `map` functions in the Option class to implement unfold, 
    * thereby doing away with the need to manually pattern match as in the above solution.  */
  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z).fold(empty[A])((p: (A,S)) => cons(p._1,unfold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A,S)) => cons(p._1,unfold(p._2)(f))).getOrElse(empty[A])
  
  /** `Corecursion` is also sometimes called guarded recursion, and productivity is also sometimes called cotermination. 
    * A `recursive` function "consumes data", a `corecursive` function "produces data". Recursive functions terminate by recursing on smaller inputs, 
    * Corecursive functions need not terminate so long as they remain "productive". (means that we can always evaluate more of the result in a finite amount of time.) 
    * Note: `Corecursion` is also sometimes called `guarded recursion`, and `productivity` is also sometimes called `cotermination`. */


  /** Using ``unfold`` */
  
  /** Scala provides shorter syntax when the first action of a function literal is to match on an expression. 
    * The function passed to `unfold` in `fibsViaUnfold` is equivalent to {{{ p => p match ( case (f0,f1) => ... ) }}}, 
    * but we avoid having to choose a name for `p`, only to pattern match on it. */
  def fibsViaUnFold: Stream[Int] = unfold((0,1)){ case (a,b) => Some(a,(b, a+b))}
  //`case (a,b) => Some(a,(b, a+b)` is equivalent to `p => p match { case (f0,f1) => Some(f0, (f1,  f0+f1) }`

  //the long version of the above function
  def fibsViaUnfoldLong: Stream[Int] = unfold((0,1)){x =>
    x match {
      case (a,b) => Some(a, (b, a+b))
    }
  }

  def fromViaUnFold(n: Int): Stream[Int] = unfold(n){x => Some((x, x + 1)) }

  def constantViaUnFold[A](a: A): Stream[A] = unfold(a){_ => Some((a,a)) }

  val onesViaUnFold: Stream[Int] = unfold(1){_ => Some((1,1))}


}