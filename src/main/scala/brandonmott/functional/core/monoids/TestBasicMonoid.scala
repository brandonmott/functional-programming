package brandonmott.functional.core
package monoids

object TestBasicMonoid extends App {
  //Quick printer
  def p(a: Any) {
    p("###> " + a)
  }  
  
  // Monoid DEFINITIONS 
  // defined as an "instance", instead of as an object inside the Monoid companion object.
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def mappend(l: Int, r: Int): Int = l + r
    val mzero: Int = 0
  }
  
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def mappend(l: Int, r: Int): Int = l * r
    val mzero: Int = 1
  }
  
  val stringConcatenation: Monoid[String] = new Monoid[String] {
    override def mappend(l: String, r: String): String = l + r
    val mzero: String = ""
  }
  
  val strings = List("This is\n", "a list of\n", "strings!")
  val numberList = List(1, 2, 3, 4, 5, 6)
  
  //Using String Concatenation
  p(s"Left folded:\n ${strings.foldLeft(stringConcatenation.mzero)(stringConcatenation.mappend)}")
  
  //Using String Concatenation
  p(s"Right folded:\n ${strings.foldRight(stringConcatenation.mzero)(stringConcatenation.mappend)}")
  
  //Using Integer Multiplication
  p(s"6! is: ${numberList.foldLeft(intMultiplication.mzero)(intMultiplication.mappend)}")

  /** In `fold`, we made the A and B types to be the same in the `foldLeft` and `foldRight` functions. */
  
  //Using the generic fold from MonoidOperations
  p(s"Left folded:\n ${MonoidOperations.fold(strings, stringConcatenation)}")
  p(s"Right folded:\n ${MonoidOperations.fold(strings, stringConcatenation)}")
  p(s"6! is: ${MonoidOperations.fold(numberList, intMultiplication)}")

  val numbers = Array(1, 2, 3, 4, 5, 6)
  p(s"4! is: ${MonoidOperations.balancedFold(numbers, intMultiplication)(identity)}")

  //COMPOSITION
  /** 
    * This Monoid of type (Int,Int) calculates the `sum` and the `factorial` of the numbers given to it. 
    */
  val sumAndProduct: Monoid[(Int, Int)] = MonoidOperations.compose(intAddition, intMultiplication)
  p(s"The sum and product is: ${MonoidOperations.balancedFold(numbers, sumAndProduct)(i => (i, i))}")

  /** 
    * We can also efficiently calculate the `mean` of all items in a list — 
    *   we just need to use the intAddition Monoid twice and 
    * map the `numbers` to (number, 1) in order to have the count together with the `sum`. 
    * */
  val intAddTwice = MonoidOperations.compose(intAddition, intAddition)
  val meanCount = MonoidOperations.balancedFold(numbers, intAddTwice)(i => (i, 1))
  p(s"The mean is: ${meanCount._1 / meanCount._2}") //result is rounded to 3, actual is 3.5 

}



