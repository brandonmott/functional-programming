package brandonmott.functional.chapters.structures

import scala.{List => _}

/** An ADT is just a data type defined by 1 or more `data constructors`, each of which may contain 0 or more arguements.
  *
  * The data type is the `sum` or `union` of its `data constructors`, and each data constructor is the product of its arguments, 
  * hence the name "algebraic data type".
  *
 * @tparam A - `List` data type, `parameterized` on a type, `A`
  */
sealed trait List[+A]

/** [[Nil]] is a `List` data constructor representing the empty list */
case object Nil extends List[Nothing] 

/** [[Cons]] is a `List` data constructor, representing non-empty lists.
  * @param head The parameter `head` is the first element in the list 
  * @param tail The parameter `tail` is another `List`, which may be `Nil` or another `Cons`.
  * @tparam A   the `parameterized` type of the `List` data type
  */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/** `List` companion object. Contains functions for creating and working with lists. */
object List {
  
  /** `sum` is a function that uses pattern matching to add up a list of integers.
    *   In case Nil, the sum of the empty list is 0.
    *   In case Cons, the sum of a list starting with `x`, is `x` + the sum of the rest of the list. */
  def sum (ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  
  def product (ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  /** `apply` uses Variadic function syntax */
  def apply[A] (as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  

  /** Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. 
    * Taking the tail of an empty list is often a bug, and silently returning a value just means this bug will be discovered later, 
    * further from the place where it was introduced. It's generally good practice when pattern matching to use `_` for 
    * any variables you don't intend to use on the right hand side of a pattern. This makes it clear the value isn't relevant. */
  def tail[A](a:List[A]): List[A] = {
    a match {
      case Cons(x,y) => y
      case Nil => sys.error("List is Empty")
    }
  }

  /** If a function body consists solely of a match expression, we'll often put the match on the same line as the function signature, 
    * rather than introducing another level of nesting. */ 
  def setHead[A](a:List[A],el:A):List[A] = a match {
    case Nil => sys.error("List is empty")
    case Cons(_, tail) => Cons(el,tail)
  }

  /** Again, it's somewhat subjective whether to throw an exception when asked to drop more elements than the list contains. 
    * The usual default for `drop` is not to throw an exception. If you pay attention to how you use `drop`, it's often in cases 
    * where the length of the input list is unknown, and the number of elements to be dropped is being computed from something else. 
    * If `drop` threw an exception, we'd have to first compute or check the length and only drop up to that many elements.
    */
  def drop[A](a:List[A],x: Int):List[A] = {
    def go[A](l:List[A],n:Int):List[A] = {
      if(n == 0) l
      else l match {
        case Nil => sys.error("list is Empty")
        case Cons(h,t) => go(t, n - 1)
      }
    }
    go(a,x)
  }

  /** To illustrate using a <b> pattern guard </b>, to only match a `Cons` whose head satisfies our predicate, `f`.
    * The syntax is to add if condition between the pattern and the `=>`, the `condition` can use any of the variables from the pattern. 
    * Scala Can Infer type of `f` if we group dropWhile into 2 argument lists. We say that this version of dropWhile is `curried`.
    * Use: {{{ dropWhile(xs)(x => x < 4) }}}. Instead of: {{{ dropWhile(xs, (x: Int) => x < 4) }}} */
  def dropWhile[A](a:List[A])(f: A => Boolean):List[A] = a match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => a
  }

  /** Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive solution 
    * will use a `stack frame for each element` of the list, which can lead to `stack overflows` for large lists (can you see why?). */
  def init1[A](a: List[A]): List[A] = {
    a match {
      case Nil => sys.error("List is empty")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init1(t))
    }
  }

  /** With lists, it's common to use a temporary, mutable buffer internal to the function (with lazy lists or streams). 
    * So long as the buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
    * 
    * Another common convention is to accumulate the output list in `reverse` order, then `reverse` it at the end, 
    * which doesn't require even local mutation. */
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  /** One way to describe what `foldRight` does is that it replaces the constructors 
    *   `Nil` with `z` the default, and 
    *   `Cons` with `f` the function `(A,B) => B`. 
    * 
    * `foldRight` goes through the whole list once first, before it applies function `f`. Example:
    * {{{ foldRight( as=Cons(1, Cons(2, Nil)) , z=0)( (a,b) => a + b ) }}}
    * Steps:
    *   1. `f` replaces `Cons` & `z` replaces `Nil`    //> f(1, f(2, 0))  
    *   2. solve for `f`: (2, 0) => 2 + 0 == 2         //> f(1, 2) 
    *   3. solve for `f`: (1, 2) => 1 + 2 == 3         //> 3 */
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  /** Note: `foldRight` does not support early termination.
    * Before we ever call our function, `f`, we evaluate its argument, which in the case of `foldRight`
    * means traversing the list all the way to the end. We need `non-strict` evaluation to support early termination. */
    
    
  /** `foldLeft` is `curried`, for better type inference, so you can write: {{{ list.foldLeft(0.0)(_ + _) }}}
    * `foldLeft` also uses tailrec annotation for `tail-call optimization`.
    */
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }
  
  def length[A](as: List[A]):Int = foldRight(as,0)((_,acc)=> acc + 1)
    
  def sum1(as: List[Int]) = foldLeft(as,0)(_ + _)
  
  def product1(as:List[Double]) = foldLeft(as,1.0)(_ * _)
  
  def reverse[A](as: List[A]) = foldLeft(as, List[A]())((acc,h) => Cons(h,acc))
  
  /** The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflow, 
    * The other implementations build up a chain of functions which, results in the operations being performed with the correct associativity. 
    * We are calling `foldRight` with the `B` type being instantiated to `B => B`, then calling the built up function with the `z` argument. 
    * <b>Note: these implementations are more of theoretical interest</b> - they aren't stack-safe and won't work for large lists.  */
   def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
     foldLeft(reverse(l), z)((b,a) => f(a,b))
 
   def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
     foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
 
   def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
     foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  
  
//  def foldRight1[A,B](as: List[A], bs: B)(f: (A,B) => B) = {
//    @annotation.tailrec
//    def go[A,B](l: List[A], b: B) = 
//      l match {
//        case Nil => b
//        case Cons(h,t) => go(t, f(h,b))(f)
//      }
//    go(reverse(as),bs)
//  }

  def appendViaFoldLeft[A](as: List[A], bs: List[A]) = 
    foldLeft(as, bs)((b,a) => Cons(a,b))
  
  /** `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation performed by `foldRight`. */
  def appendViaFoldRight[A](as: List[A], bs: List[A]) = 
    foldRight(as,bs)(Cons(_,_))

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  }

  def concat[A](as: List[List[A]]): List[A] = 
    foldRight(as, Nil:List[A])(append)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  def mkString(ds: List[Double]):List[String] = {
    foldRight(ds,Nil:List[String])((a,b) => Cons(a.toString,b))
  }

  //not inferred because arguments are combined
  def map1[A,B](as: List[A], f: A => B): List[B] = 
    foldRight(as, Nil:List[B])((a,b) => Cons(f(a),b) )

  //type inferred because its curried (takes 2 seperate arguments) map2(list)(...)
  def map2[A,B](as: List[A])( f: A => B): List[B] = 
    foldRightViaFoldLeft(as, Nil:List[B])((a,b) => Cons(f(a),b) )

  //type inferred again because its curried (takes 2 seperate arguments) map2(list)(...)
  def map3[A,B](as: List[A])( f: A => B): List[B] = 
    foldLeft(reverse(as), Nil:List[B])((b,a) => Cons(f(a),b) )

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => as
      case Cons(h,t) => if(f(h)) Cons(h,filter(t)(f)) else filter(t)(f)
    }
  }

  def flatMap1[A,B](as: List[A])(f: A => List[B]): List[B] = 
    foldRight(as, Nil:List[B]){(a,b) => append(f(a), b) }

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = 
    foldRight(as, Nil:List[B]){(a,b) => foldRight(f(a),b)(Cons(_,_))}
  
  /** This could also be implemented directly using `foldRight`. */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    concat(map2(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)(a => if(f(a)) List(a) else Nil)
  
  /** To match on multiple values, we can put the values into a pair and match on the pair, as shown next, 
    * and the same syntax extends to matching on N values (Pair and tuple objects). You can also (less conveniently, but more efficiently) 
    * nest pattern matches: on the right hand side of the `=>`, simply begin another `match` expression. 
    * The inner `match` will have access to all the variables introduced in the outer `match`. */
  def addPairs(as: List[Int], ax: List[Int]): List[Int] = (as,ax) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h,t)) => Cons(h1+h,addPairs(t,t1))
  }

  /** This function is usually called `zipWith`. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list. */
  def zipWith[A,B,C](as:List[A],ax:List[B])(f:(A,B) => C):List[C]= (as,ax) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
  }
  /** The discussion about stack usage from the explanation of `map` also applies to `addPairs` & `zipWith`. */
  
  /** There's nothing particularly bad about this implementation, except that it's somewhat <b> "monolithic" <b> and easy to get wrong.
    * Where possible, we prefer to assemble functions like this combinator functions. It makes the code more "correct" and "easier to read" and "understand". 
    * Notice that in this implementation we need special purpose logic to break out of our loops early. Composing functions like this, 
    * without giving up the efficiency of having the resulting functions work in one pass over the data.  */
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}

//end of object List

/**
 * Created by brandonmott1 on 1/13/16.
 */
object exercise1 extends App {
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //this case is structurally equivalent to the target
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(s"The value of x = $x")
  val list1 = List(1,2,3,4,5,6,7) //Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,Nil)))))))
  println(s"list1 = ${list1}")
  println(s"List.tail(list1) = ${List.tail(list1)}")
  println(s"List.setHead(list1,0) = ${List.setHead(list1,0)}")
  println(s"List.drop(list1,4) = ${List.drop(list1,4)}")
  println(s"List.length(list1) = ${List.length(list1)}")//map1 not curried, there for not inferred
  println(s"List.map1(list1, x => x + 1) = ${List.map1(list1, (x: Int) => x + 1)}")
  println(s"List.map2(list1, x => x + 1) = ${List.map2(list1)( x => x + 1)}")
  println(s"List.map3(list1, x => x + 1) = ${List.map3(list1)( x => x + 1)}")
  println(s"List.filter(list1, x => x < 4) = ${List.filter(list1)( x => x < 4)}")
  println(s"List.flatMap1(List(1,2,3))(i => Cons(i,i)) = ${List.flatMap1(List(1, 2, 3))(i => List(i, i))}")
  println(s"List.flatMap2(List(1,2,3))(i => Cons(i,i)) = ${List.flatMap2(List(1, 2, 3))(i => List(i, i))}")
  println(s"List.flatMap(List(1,2,3))(i => Cons(i,i)) = ${List.flatMap(List(1, 2, 3))(i => List(i, i))}")
  println(s"List.filterViaFlatMap(list1)(i => i < 4) = ${List.filterViaFlatMap(list1)(i => i < 4)}")
}

/** These are defined methods on class List[A], 
  * rather than as stand alone functions defined in the object List */
trait OtherListFunctions{

  /** Returns a list consisting of the first n elements of `this` */
  def take[A](n: Int): List[A]

  /** Returns a list consisting of the longest valid prefix of `this` whose elements all pass the predicate `f` */
  def takeWhile[A](f: A => Boolean): List[A]
  
  /** Returns true if and only if all elements of `this` pass predicate `f` */
  def forall[A](f: A => Boolean): Boolean

  /** Returns true if any element of `this` passes the predicate `d` */
  def exists[A](f: A => Boolean): Boolean

  /** `scanLeft` and `scanRight`: like `foldLeft` and `foldRight` but they return the list of partial results, 
    * rather than just the final accumulated value. */
}
