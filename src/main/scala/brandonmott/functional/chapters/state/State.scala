package brandonmott.functional.chapters.state

/** State is a Wrapper class for the state action data type. A general type for handling any type of state.
  * State is short for "computation that carries some state along, or state action, state transition, or even statement".
  * State replaces a type alias that would be:{{{ type State[S,+A] = S => (A,S) }}}
  * Includes general-purpose functions for capturing common patterns of stateful programs 
  * or working with state actions, and doesn't care about the type of the state. 
  * Defines methods on the State case class where possible, Otherwise they are in the State companion object. */
case class State[S, +A](run: S => (A, S)){
//  def unit: S => (A, S) = run

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b) ) )

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1) // f(a) returns State[S, B] which is a function S => (B, S)  
  })
  
}

object State {
  
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight( unit[S, List[A]](List()) )((f, acc) => f.map2(acc)(_ :: _))

  /** This implementation uses a loop internally and is the same recursion pattern as a left fold. It is quite common 
    * with left folds to build up a list in reverse order, then reverse it at the end. (We could also use a 
    * collection.mutable.ListBuffer internally.) */
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
  
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }
    
    State((s: S) => go(s, sas, List()))
  }

  /** We write the loop using a `foldLeft`. This is `tail recursive` like the previous solution, but it `reverses` 
    * the list _before_ folding it instead of after. You might think that this is slower than the `foldRight` solution 
    * since it walks over the list twice, but it's actually faster! The `foldRight` solution technically has to also 
    * walk the list twice, since it has to unravel the call stack, not being tail recursive. And the call stack will be 
    * as tall as the list is long. */
  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
  l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

/* By using for-comprehension to recover the `imperative style`, this code is much easier to read (and write). */

  /** `modify` is a combinator that can modify the state in arbitrary ways.
    * To facilitate this kind of imperative programming with for-comprehensions (or flatMaps), we really only need 
    * two primitive State combinators, for reading and writing the state. 
    * A combinator `get` for getting the current state, and a combinator `set` for setting a new state. */
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get        // Gets the current state and assigns it to `s`.
    _ <- set(f(s))  // Sets the new state to `f` applied to `s`.
  } yield ()
  
  /** The `get` action simply passes the incoming state along and returns it as the value */
  def get[S]: State[S,S] = State(s => (s,s))
  
  /** The `set` action is constructed with a new state s. The resulting action ignores the incoming state, 
    * replaces it with the new state, and returns () instead of a meaningful value: */
  def set[S](s: S): State[S,Unit] = State(_ => ((), s))
  
  /**
    * The State combinators that we wrote — `get`, `set`, `unit`, `map`, `map2`, and `flatMap` — 
    * are all the tools needed to implement any kind of state machine or stateful program in a purely sandbox.brandonmott.functional way. */
  
}


