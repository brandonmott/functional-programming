package brandonmott.functional.core
package monoids
package syntax

import brandonmott.functional.core.monoids.{FoldLeft, Monoid}

/**
  * Enrich my library. inject `summ` to both Int and String just one definition.
  * Moving the `sum` function to MA and providing an implicit conversion for better syntax
  */
trait MA[M[_], A] {
  val value: M[A]

  def summ(implicit m: Monoid[A], fl: FoldLeft[M]): A = fl.foldLeft(value, m.mzero, m.mappend)
}