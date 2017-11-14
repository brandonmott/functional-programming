package brandonmott.functional.core
package monoids

/**
  * MonoidOperations has GENERIC FUNCTIONS that preform folding operations on a sequence, using a Monoid and 
  * do different things depending on the Monoid operation. 
  */
object MonoidOperations {
    
  /** A generic fold operation that uses a Monoid. */
  def fold[T](list: List[T], m: Monoid[T]): T =
  list.foldLeft(m.mzero)(m.mappend)

  /** 
    * In `fold`, we made the A and B types to be the same in the foldLeft and foldRight functions. 
    * To add a possibility of `mapping` the type of the original list to a different type 
    * would give us the possibility of implementing even more complex operations. 
    */
  def foldMap[T,Y](list: List[T], m: Monoid[Y])(f: T => Y):Y = list.foldLeft(m.mzero){
    case (t, y) => m.mappend(t, f(y)) //m must be type Y 
  }

  /** 
    * Here the `nested operations` could be done independently and in parallel to IMPROVE EFFICIENCY. 
    * 
    * This is also called `balanced fold`. 
    *
    * It is worth mentioning that we've used an IndexedSeq here, 
    *   as it will guarantee that getting elements by index will be efficient. 
    *
    *   Also, this code is not parallel but we've switched the order of the operations as we mentioned previously. 
    *     In the case of integers, it might not make much of a difference
    *     But for other types such as strings, it will improve the performance because strings are immutable and 
    *     every concatenation will create a new string by allocating new space.
    *   
    *   So if we are simply going from the left-hand side to the right-hand side, 
    *   we will be allocating more and more space and throwing away the intermediate results all the time. 
    */
  def balancedFold[T, Y](list: IndexedSeq[T], m: Monoid[Y])(f: T => Y): Y =
  if (list.length == 0) {
    m.mzero
  } else if (list.length == 1) {
    f(list(0))
  } else {
    val (left, right) = list.splitAt(list.length / 2) //split list in half
    m.mappend(balancedFold(left, m)(f), balancedFold(right, m)(f))
  }

  /** 
    * Monoid also supports COMPOSITION; 
    *   If A and B are Monoids, then their `product` (A, B) is also a Monoid.
    * 
    * This would now allow us to simultaneously apply multiple operations using a Monoid. 
    * 
    * And we can `compose` even more and apply even more operations. 
    */
  def compose[T, Y](a: Monoid[T], b: Monoid[Y]): Monoid[(T, Y)] =
    new Monoid[(T, Y)] {
      val mzero: (T, Y) = (a.mzero, b.mzero)
      override def mappend(l: (T, Y), r: (T, Y)): (T, Y) = 
        (a.mappend(l._1, r._1), b.mappend(l._2, r._2))
    }
}
