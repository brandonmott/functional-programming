package brandonmott.functional.chapters.parallel

import java.util.concurrent._
import scala.language.implicitConversions


/** 
  * Par is a Purely brandonmott.functional.chapters library for creating parallel and asynchronous computations:
  *   - it doesn’t need to know how to actually implement the parallelism.
  *   - is a description of a parallel computation that gets interpreted at a later time by something like the get/run function.
  *   - is a first-class program that we can run.
  */
trait Par {
  /** 
    * Par is represented by a function that needs an ExecutorService 
    * By returning a Future you defer the decisions a future makes to the caller of `run` 
    */
  type Par[A] = ExecutorService => Future[A]
  
  /** 
    * `map2` combines the results of two parallel computations with a binary function. 
    */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] 
  
  /** 
    * `fork` takes an "unevaluated" Par and “marks” it for concurrent evaluation. 
    * The evaluation won’t actually occur until forced by `run` 
    */
  def fork[A](a: => Par[A]): Par[A] 

  /** 
    * `unit` promotes a constant value to a parallel computation 
    */
  def unit[A](a: A): Par[A]

  /** 
    * `lazyUnit` wraps its "unevaluated" argument in a Par and marks it for concurrent evaluation.
    * This is an example of a "derived combinator", as opposed to a "primitive combinator" like `unit` 
    */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
  /** 
    * `run` extracts a value from a Par by actually performing the computation. 
    * Renamed the `get` function to `run` because `Par` is a 'first-class program' that we can "run" 
    */
  def run[A](es: ExecutorService)(a: Par[A]): Future[A] =  a(es)
}

