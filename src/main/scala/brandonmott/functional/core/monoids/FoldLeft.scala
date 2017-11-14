package brandonmott.functional.core
package monoids

/**
  * FoldLeft has a single operation `foldLeft`
  */
trait FoldLeft[F[_]] {
  def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
}


object FoldLeft {

  implicit object FoldLeftList extends FoldLeft[List] {
    def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B): B = xs.foldLeft(b)(f)
  }

}
