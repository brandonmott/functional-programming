package brandonmott.functional.chapters.structures

/** ADT means "algebraic data type" sometimes referred to as "abstract data type"
<b> An ADT is just a data type defined by 1 or more _data constructors_, each of which may contain 0 or more arguements. </b>

The data type is the `sum` or `union` of its _data constructors_,
and each data constructor is the `product` of its arguments, hence the name "algebraic data type".

"Algebraic data types" can be used to define other data structures  */

/* binary tree data structure */
sealed trait Tree[+A]                                               //Note: `Tree` data type, `parameterized` on a type, `A`
case class Leaf[A](value: A) extends Tree[A]                        //Note: a `Tree` data constructor representing the value
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] //Note: a `Tree` _data constructor_ representing the Branch to 2 more Tree data structures
//Nodes are leaves and branches

object Tree { //companion object
  //TODO continue on pg. 46
  
}

