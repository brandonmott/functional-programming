package brandonmott.functional.core
package monoids

/** 
  * Notes from "Monoids in real life":
  * The declarations of `foldLeft` and `foldRight` functions:
  * def foldLeft[B](z: B)(f: (B, A) => B): B
  * def foldRight[B](z: B)(f: (A, B) => B): B 
  *
  * Note: the `z` parameter in these two functions is called the `ZERO VALUE`. 
  *
  * If `A` and `B` are of the SAME type `A`:
  * def foldLeft[A](z: A)(f: (A, A) => A): A 
  * def foldRight[A](z: A)(f: (A, A) => A): A 
  *
  * Then both functions are EXACTLY the Monoid rules
  * foldLeft(m.mzero)(m.mappend) 
  * foldRight(m.mzero)(m.mappend)
  *
  * Note about `foldLeft` & `foldRight`: 
  * The final result is the SAME because Monoid's ASSOCIATIVE operation. 
  * The performance is different.
  *
  * The fact that a Monoid operation is `associative`
  * means that if we have to chain multiple operations, we could probably do it in parallel.
  *
  * For example, if we have the numbers 1, 2, 3, and 4 and wanted to find 4!, we can use what we used previously, 
  * which would end up being evaluated to the following:
  * mappend(mappend(mappend(1, 2), 3), 4) 
  *
  * The associativity, would allow us to do the following:
  * mappend(mappend(1, 2), mappend(3, 4)) 
  *
  * In machine learning, we might need to extract the features from some text. 
  * Then each feature will be weighted using a `coefficient` and 
  * a number equal to the number of times we've seen it (the `count`). 
  *
  * Let's try and get to a Monoid that can be used to `fold` a collection and give us what we need — 
  * the `count` of each feature. 
  *******************************************************************************************************/
package object monoids {
  //Quick printer
  def p(a: Any) {
    println("###> " + a)
  }
}
