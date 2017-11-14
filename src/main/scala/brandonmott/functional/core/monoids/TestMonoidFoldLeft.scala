package brandonmott.functional.core
package monoids

/**
  * To use Instances of Monoid: 
  *   either import into scope `explicitly` or define them `implicitly` in the companion object (preferred?)
  *
  * Here's how to import them into scope `explicitly`:
  * {{{
  * import Monoid.{IntAdditionMonoid,StringConcatenationMonoid}
  * import FoldLeft.FoldLeftList
  *   //Create an implicit reference to the object inside the Monoid companion object
  *   implicit val intAddition = IntAdditionMonoid
  *   implicit val stringConcat = StringConcatenationMonoid
  *   implicit val foldLeftList = FoldLeftList
  * }}}
  */
object TestMonoidFoldLeft {

  
  // def implicitly[T](t:T):T=t lets you pull out the implicit reference in that scope.
  implicit val intAddition = implicitly[Monoid[Int]]
  implicit val stringConcat = implicitly[Monoid[String]]

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def mappend(l: Int, r: Int): Int = l * r

    val mzero: Int = 1
  }

  /** `sum` function also generalized on List: 
    * 
    * So instead of taking a List[T], sum will take any `higher kinded type` of M[T]. (List,Option,Tree,etc.)
    * 
    * The `M[_]` indicates its a type constructor, that needs a type applied to it first. 
    * 
    * The Goal is to go from:
    * {{{def sum(xs: List[Int]): Int = xs.foldLeft(0) { _ + _ }}}}
    * to:
    * {{{def sum[M[_]: FoldLeft, T: Monoid](xs: M[T])}}}
    * 
    * After implementing Monoid
    *   `first try` - using monoid inside FoldLeftList.foldLeft method
    *   {{{def sum[T](xs: List[T], m: Monoid[T]): T = xs.foldLeft(m.mzero)(m.mappend)}}}
    *   
    *   `second try` - passing monoid implicitly 
    *   {{{def sum[T](xs: List[T])(implicit m: Monoid[T]): T = FoldLeftList.foldLeft(xs, m.mzero, m.mappend)}}}
    *   
    *   `third try` - using context bounds
    *   {{{
    *   def sum[T:Monoid](xs: List[T]): T = {
    *     val m = implicitly[Monoid[T]]
    *     FoldLeftList.foldLeft(xs, m.mzero, m.mappend) 
    *   }
    *   }}}
    * 
    * After implementing FoldLeft
    *   `forth try` - repeating the same steps for FoldLeft (first - third try above)
    *   {{{
    *     def sum[M[_], T](xs: M[T])(implicit m: Monoid[T], fl: FoldLeft[M]): T = fl.foldLeft(xs, m.mzero, m.mappend)
    *   }}}
    */
  def sum[M[_] : FoldLeft, T: Monoid](xs: M[T]): T = {
    val m = implicitly[Monoid[T]]
    val fl = implicitly[FoldLeft[M]]
    fl.foldLeft(xs, m.mzero, m.mappend)
  } //sum: [M[_], A](xs: M[A])(implicit evidence$1: FoldLeft[M], implicit evidence$2: Monoid[A])A

  //Quick printer
  def p(a: Any) {
    println("###> " + a)
  }

  def main(args: Array[String]): Unit = {
    println("*" * 100)

    p(sum(List(1, 2, 3, 4)))
    p(sum(List("a", "b", "c")))
    // explicitly passing all "implicit parameters" to replace the implicit IntAdditionMonoid.
    p(sum(List(1, 2, 3, 4))(implicitly[FoldLeft[List]], intMultiplication))
    
  }



}
